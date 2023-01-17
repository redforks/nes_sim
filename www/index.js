import {new_machine, init} from "nes_web";

async function main() {
    let resp = await fetch("01-basics.nes");
    let rom = await resp.arrayBuffer();
    let machine = new_machine(new Uint8Array(rom));
    // for (;;) {
    //     machine.tick()
    //
    // }
}

init();
main().then(() => console.log("done")).catch(e => console.error(e));
