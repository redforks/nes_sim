use js_sys::Float32Array;
use nes_core::nes::apu::AudioDriver;
use wasm_bindgen::prelude::*;

const CHUNK_SIZE: usize = 1024;

#[wasm_bindgen(inline_js = r#"
const PROCESSOR_NAME = "nes-audio-processor";

function createProcessorSource() {
  return `
const PROCESSOR_NAME = "${PROCESSOR_NAME}";

class NesAudioProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.queue = [];
    this.offset = 0;
    this.bufferedSamples = 0;
    this.port.onmessage = (event) => {
      const data = event.data;
      if (!data || data.type !== "chunk" || !(data.samples instanceof Float32Array)) {
        return;
      }

      this.queue.push(data.samples);
      this.bufferedSamples += data.samples.length;
    };
  }

  process(_inputs, outputs) {
    const output = outputs[0];
    if (!output || output.length === 0) {
      return true;
    }

    const channel = output[0];
    let written = 0;

    while (written < channel.length) {
      if (this.queue.length === 0) {
        channel.fill(0, written);
        break;
      }

      const current = this.queue[0];
      const remaining = current.length - this.offset;
      const count = Math.min(channel.length - written, remaining);
      channel.set(current.subarray(this.offset, this.offset + count), written);
      written += count;
      this.offset += count;

      if (this.offset >= current.length) {
        this.queue.shift();
        this.offset = 0;
        this.bufferedSamples -= current.length;
      }
    }

    for (let i = 1; i < output.length; i += 1) {
      output[i].set(channel);
    }

    return true;
  }
}

registerProcessor(PROCESSOR_NAME, NesAudioProcessor);
`;
}

export class AudioWorkletDriverHandle {
  constructor() {
    const AudioContextCtor = globalThis.AudioContext || globalThis.webkitAudioContext;
    if (!AudioContextCtor) {
      throw new Error("Web Audio API is not available in this browser");
    }

    this.context = new AudioContextCtor({ latencyHint: "interactive" });
    this.sampleRate = this.context.sampleRate;
    this.node = null;
    this.pendingChunks = [];
    this.ready = this.installWorklet();
    this.ready.catch((error) => console.error("failed to initialize NES audio worklet", error));
    this.resumeNow();
  }

  async installWorklet() {
    if (!this.context.audioWorklet) {
      throw new Error("AudioWorklet is not supported in this browser");
    }

    const blob = new Blob([createProcessorSource()], { type: "application/javascript" });
    const url = URL.createObjectURL(blob);

    try {
      await this.context.audioWorklet.addModule(url);
    } finally {
      URL.revokeObjectURL(url);
    }

    this.node = new AudioWorkletNode(this.context, PROCESSOR_NAME, {
      numberOfInputs: 0,
      numberOfOutputs: 1,
      outputChannelCount: [1],
    });
    this.node.connect(this.context.destination);

    for (const chunk of this.pendingChunks) {
      this.postChunk(chunk);
    }
    this.pendingChunks.length = 0;
  }

  pushChunk(chunk) {
    this.resumeNow();
    if (this.node) {
      this.postChunk(chunk);
      return;
    }

    this.pendingChunks.push(chunk);
  }

  postChunk(chunk) {
    this.node.port.postMessage({ type: "chunk", samples: chunk }, [chunk.buffer]);
  }

  flush() {}

  resumeNow() {
    if (this.context.state !== "running") {
      void this.context.resume().catch(() => {});
    }
  }
}
"#)]
extern "C" {
    type AudioWorkletDriverHandle;

    #[wasm_bindgen(catch, constructor)]
    fn new() -> Result<AudioWorkletDriverHandle, JsValue>;

    #[wasm_bindgen(method, getter, js_name = sampleRate)]
    fn sample_rate(this: &AudioWorkletDriverHandle) -> f64;

    #[wasm_bindgen(method, js_name = pushChunk)]
    fn push_chunk(this: &AudioWorkletDriverHandle, chunk: &Float32Array);

    #[wasm_bindgen(method)]
    fn flush(this: &AudioWorkletDriverHandle);
}

pub struct WebAudioDriver {
    handle: AudioWorkletDriverHandle,
    buffer: Vec<f32>,
    sample_rate: u32,
}

impl WebAudioDriver {
    pub fn new() -> Result<Self, JsValue> {
        let handle = AudioWorkletDriverHandle::new()?;
        let sample_rate = handle.sample_rate().round().max(1.0) as u32;
        Ok(Self {
            handle,
            buffer: Vec::with_capacity(CHUNK_SIZE),
            sample_rate,
        })
    }

    fn send_buffer(&mut self) {
        if self.buffer.is_empty() {
            return;
        }

        let chunk = Float32Array::from(self.buffer.as_slice());
        self.handle.push_chunk(&chunk);
        self.buffer.clear();
    }
}

impl AudioDriver for WebAudioDriver {
    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn push_sample(&mut self, sample: f32) {
        self.buffer.push(sample);
        if self.buffer.len() >= CHUNK_SIZE {
            self.send_buffer();
        }
    }

    fn flush(&mut self) {
        self.send_buffer();
        self.handle.flush();
    }
}
