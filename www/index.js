import {new_machine, init} from "../nes_web/pkg/nes_web.js"

async function main() {
    init();
    const playButton = document.getElementById("play-button");
    if (!playButton) {
        throw new Error("Play button not found");
    }

    let resp = await fetch("contra.nes");
    let rom = await resp.arrayBuffer();
    // draw_chr(new Uint8Array(rom), 'canvas');
    let started = false;

    playButton.addEventListener("click", async () => {
        if (started) {
            return;
        }

        started = true;
        playButton.disabled = true;
        playButton.style.display = "none";

        try {
            const machine = new_machine('canvas', new Uint8Array(rom));

            let tick = () => {
                machine.process_frame();
            };

            requestAnimationFrame(function loop() {
                tick();
                requestAnimationFrame(loop);
            });
        } catch (e) {
            started = false;
            playButton.style.display = "inline-block";
            playButton.disabled = false;
            throw e;
        }
    });
}

main().catch(e => console.error(e));
