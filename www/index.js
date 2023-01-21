import {new_machine, init, draw_chr} from "nes_web"

async function main() {
    let resp = await fetch("01-basics.nes");
    let rom = await resp.arrayBuffer();
    // draw_chr(new Uint8Array(rom), 'canvas');
    let machine = new_machine('canvas', new Uint8Array(rom));
    let time = performance.now();
    let tick = () => {
        let now = performance.now();
        let delta = now - time;
        time = now;
        machine.process_frame(delta);
        console.log("process frame using: " + (performance.now() - time) + "ms");
    };
    let ticks = 0
    requestAnimationFrame(function loop() {
        tick();
        if (ticks++ < 60) {
            requestAnimationFrame(loop);
        }
    })
}

init();
main().then(() => console.log("done")).catch(e => console.error(e));
