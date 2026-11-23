use js_sys::Float32Array;
use nes_core::audio_chunk::ChunkedAudioPush;
use wasm_bindgen::prelude::*;

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
    this.maxBufferedSamples = Math.max(128, Math.ceil(sampleRate * 0.12));
    this.port.onmessage = (event) => {
      const data = event.data;
      if (!data || data.type !== "chunk" || !(data.samples instanceof Float32Array)) {
        return;
      }

      this.queue.push(data.samples);
      this.bufferedSamples += data.samples.length;
      this.trimBufferedAudio();
    };
  }

  trimBufferedAudio() {
    while (this.bufferedSamples > this.maxBufferedSamples && this.queue.length > 0) {
      const current = this.queue[0];
      const remaining = current.length - this.offset;
      const overflow = this.bufferedSamples - this.maxBufferedSamples;

      if (overflow >= remaining) {
        this.queue.shift();
        this.offset = 0;
        this.bufferedSamples -= remaining;
      } else {
        this.offset += overflow;
        this.bufferedSamples -= overflow;
      }
    }
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
    this.pendingSamples = 0;
    this.maxBufferedSamples = Math.max(128, Math.ceil(this.sampleRate * 0.12));
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
    this.pendingSamples = 0;
  }

  pushChunk(chunk) {
    this.resumeNow();
    if (this.node) {
      this.postChunk(chunk);
      return;
    }

    this.pendingChunks.push(chunk);
    this.pendingSamples += chunk.length;
    this.trimPendingChunks();
  }

  postChunk(chunk) {
    this.node.port.postMessage({ type: "chunk", samples: chunk }, [chunk.buffer]);
  }

  trimPendingChunks() {
    while (this.pendingSamples > this.maxBufferedSamples && this.pendingChunks.length > 0) {
      const dropped = this.pendingChunks.shift();
      this.pendingSamples -= dropped.length;
    }
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
    chunker: ChunkedAudioPush,
    sample_rate: u32,
}

impl WebAudioDriver {
    pub fn new() -> Result<Self, JsValue> {
        let handle = AudioWorkletDriverHandle::new()?;
        let sample_rate = handle.sample_rate().round().max(1.0) as u32;
        Ok(Self {
            handle,
            chunker: ChunkedAudioPush::new(),
            sample_rate,
        })
    }
}

impl WebAudioDriver {
    /// Push through the shared chunker: browser audio matches the desktop
    /// cadence (`AUDIO_CHUNK_SAMPLES`), one worklet post per chunk.
    pub fn push_samples(&mut self, samples: &[f32]) {
        let Self {
            chunker, handle, ..
        } = self;
        chunker.push(samples, |chunk| {
            handle.push_chunk(&Float32Array::from(chunk));
        });
    }

    pub fn flush_frame(&mut self) {
        let Self {
            chunker, handle, ..
        } = self;
        chunker.flush(|chunk| {
            handle.push_chunk(&Float32Array::from(chunk));
        });
        handle.flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nes_core::audio_chunk::AUDIO_CHUNK_SAMPLES;

    /// Parity pin: the web sink chunks at the desktop cadence through the
    /// shared routine. One frame-sized push emits one chunk (the old
    /// 256-sample cadence would have emitted four).
    #[test]
    fn web_chunks_at_the_shared_desktop_cadence() {
        assert_eq!(AUDIO_CHUNK_SAMPLES, 1024);
        let frame: Vec<f32> = (0..AUDIO_CHUNK_SAMPLES).map(|i| i as f32).collect();
        let mut chunker = ChunkedAudioPush::new();
        let mut emitted = Vec::new();
        chunker.push(&frame, |chunk| emitted.push(chunk.to_vec()));
        chunker.flush(|chunk| emitted.push(chunk.to_vec()));
        assert_eq!(emitted.len(), 1);
        assert_eq!(emitted[0], frame);
    }
}
