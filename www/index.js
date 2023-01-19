import {new_machine, init, draw_chr} from "nes_web"

async function main() {
    let resp = await fetch("01-basics.nes");
    let rom = await resp.arrayBuffer();
    draw_chr(new Uint8Array(rom), 'canvas');
    // let machine = new_machine(new Uint8Array(rom));
    // let time = 0;
    // let tick = () => {
    //     let now = performance.now();
    //     let delta = now - time;
    //     time = now;
    //     machine.tick_for_milliseconds(delta);
    // };
    // requestAnimationFrame(function loop() {
    //     tick();
    //     requestAnimationFrame(loop);
    // })
}

init();
main().then(() => console.log("done")).catch(e => console.error(e));
