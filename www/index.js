import {new_machine, init, draw_chr} from "../nes_web/pkg/nes_web.js"

async function main() {
    let resp = await fetch("contra.nes");
    let rom = await resp.arrayBuffer();
    // draw_chr(new Uint8Array(rom), 'canvas');
    let machine = new_machine('canvas', new Uint8Array(rom));
    let tick = () => {
        machine.process_frame();
    };
    let ticks = 0
    requestAnimationFrame(function loop() {
        tick();
        requestAnimationFrame(loop);
    })
}

init();
main().then(() => console.log("done")).catch(e => console.error(e));
