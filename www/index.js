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
    let machine = null;

    window.addEventListener("keydown", (event) => {
        if (!machine) {
            return;
        }

        if (event.code === "F2") {
            event.preventDefault();
            machine.reset();
            return;
        }

        const handledKeys = ["ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "KeyZ", "KeyX", "Space", "Enter"];
        if (handledKeys.includes(event.code)) {
            event.preventDefault();
            machine.key_down(event.code);
        }
    });

    window.addEventListener("keyup", (event) => {
        if (!machine) {
            return;
        }

        const handledKeys = ["ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "KeyZ", "KeyX", "Space", "Enter"];
        if (handledKeys.includes(event.code)) {
            event.preventDefault();
            machine.key_up(event.code);
        }
    });

    playButton.addEventListener("click", async () => {
        if (started) {
            return;
        }

        started = true;
        playButton.disabled = true;
        playButton.style.display = "none";

        try {
            machine = new_machine('canvas', new Uint8Array(rom));

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
