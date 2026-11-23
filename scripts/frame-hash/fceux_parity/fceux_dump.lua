-- dump emulated screen (XBackBuf, palette applied) after each emu.frameadvance() as GD truecolor file
local out = os.getenv("DUMP_DIR")
local n = tonumber(os.getenv("NFRAMES") or "600")
for i = 0, n - 1 do
  emu.frameadvance()
  local f = io.open(string.format("%s/f%05d.gd", out, i), "wb")
  f:write(gui.gdscreenshot(true))
  f:close()
end
emu.exit()
