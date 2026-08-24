-- gorf_speech_browser.lua
-- Gorf Program 2 native speech browser for MAME 0.289+
--
-- Boots Gorf normally, then takes over foreground execution while leaving the
-- original interrupt-driven sound and SC-01 speech service active. The browser
-- uses Gorf's resident drawchar/CHRTBL path; no MAME overlay is used.
--
-- Controls:
--   LEFT / RIGHT  toggle FRAGMENTS / PHRASES
--   UP / DOWN     move selection
--   FIRE          play selected entry
--   1P START      exit MAME
--   2P START      play all / stop
--
-- Gorf has no WoW-style numeric phrase table. The PHRASES pane enumerates the
-- actual Program-2 attract, coin, generic, insult, start, promotion and flagship
-- speech paths. Foreign speech addresses are resolved through the active X11's
-- 36-entry Program-2 translation table.
--
-- Speech is played directly from the loaded ROM. See ghelp() for console tools.

local VERSION = "1.0.10-20260814-1508"
local BUILD_FILE = "gorf_speech_browser.lua"

-- Detailed trace is controlled at runtime with gtrace().
local DEBUG_TRACE = false

local C = {
  CPU_TAG = ":maincpu",

  -- Astrocade inputs / SC-01.
  COINPORT = 0x10,
  P2PORT = 0x11,
  P1PORT_READY = 0x12,
  SETTINGS = 0x13,
  LANGUAGE_MASK = 0x08,       -- bit 3: 1 resident English, 0 foreign X11

  -- GORFOS Blocks 0101-0102: resident speech path and ring state.
  SPEAKLINK = 0x10B8,
  SPEAK = 0x10D7,
  ONHOLD = 0xD111,
  TOPTALK = 0xD112,
  BOTTOMTALK = 0xD120,
  TALKHERE = 0xD122,
  PHONE_COUNT = 0xD124,
  TALKIN = 0xD125,
  TALKOUT = 0xD127,
  SKILLFACTOR = 0xD037,

  -- Foreign X11 contract.
  X11_ENTRY = 0xC000,
  X11_MESSAGES = 0xC003,
  X11_FIRST = 0xC000,
  X11_LAST = 0xCFFF,
  TRANSLATION_COUNT = 36,

  -- Browser work RAM used after foreground takeover.
  IDLE_LOOP = 0xD400,
  DRAW_CODE = 0xD420,
  DRAW_DATA = 0xD520,
  CALL_STACK = 0xD7E0,

  TAKEOVER_DELAY_SEC = 2.0,
  INPUT_INITIAL_REPEAT = 15,
  INPUT_REPEAT_RATE = 4,
  UI_ROWS = 7,
  UI_WIDTH = 27,            -- native CSPELL centering limit without 16-bit wrap
  TRACE_STALL_SEC = 1.5,
  WAV_POSTROLL_SEC = 0.15,

  -- Gorf native drawchar colors with Magic EXPAND enabled.
  -- GORFOS GNAME uses $0408 for ordinary posted text; use the same $08
  -- Magic mode for browser repainting rather than SPELL's XOR-oriented $28.
  ATTR_BLUE = 0x0808,
  ATTR_YELLOW = 0x0408,
  ATTR_RED = 0x0C08,
}

-- Z80 idle loop: EI / HALT / JP $D401. Interrupts remain active.
local IDLE_LOOP_BYTES = { 0xFB, 0x76, 0xC3, 0x01, 0xD4 }

local SC01_NAMES = {
  [0x00]="EH3", [0x01]="EH2", [0x02]="EH1", [0x03]="PA0",
  [0x04]="DT",  [0x05]="A2",  [0x06]="A1",  [0x07]="ZH",
  [0x08]="AH2", [0x09]="I3",  [0x0A]="I2",  [0x0B]="I1",
  [0x0C]="M",   [0x0D]="N",   [0x0E]="B",   [0x0F]="V",
  [0x10]="CH",  [0x11]="SH",  [0x12]="Z",   [0x13]="AW1",
  [0x14]="NG",  [0x15]="AH1", [0x16]="OO1", [0x17]="OO",
  [0x18]="L",   [0x19]="K",   [0x1A]="J",   [0x1B]="H",
  [0x1C]="G",   [0x1D]="F",   [0x1E]="D",   [0x1F]="S",
  [0x20]="A",   [0x21]="AY",  [0x22]="Y1",  [0x23]="UH3",
  [0x24]="AH",  [0x25]="P",   [0x26]="O",   [0x27]="I",
  [0x28]="U",   [0x29]="Y",   [0x2A]="T",   [0x2B]="R",
  [0x2C]="E",   [0x2D]="W",   [0x2E]="AE",  [0x2F]="AE1",
  [0x30]="AW2", [0x31]="UH2", [0x32]="UH1", [0x33]="UH",
  [0x34]="O2",  [0x35]="O1",  [0x36]="IU",  [0x37]="U1",
  [0x38]="THV", [0x39]="TH",  [0x3A]="ER",  [0x3B]="EH",
  [0x3C]="E1",  [0x3D]="AW",  [0x3E]="PA1", [0x3F]="STOP",
}

-- The 36 Program-2 resident speech keys used by every current Program-2 X11.
local FRAGMENTS = {
  { key=0x115D, label="SPK_INSERT",      text="Insert coin" },
  { key=0x116D, label="SPK_GORF",        text="I am the Gorfian Empire" },
  { key=0x1185, label="SPK_SPACE",       text="Space" },
  { key=0x118C, label="SPK_CONQUER",     text="Gorfians conquer another galaxy" },
  { key=0x11A8, label="SPK_TRY",         text="Try again; I devour your coins" },
  { key=0x11C7, label="SPK_LONG",        text="Long live Gorf" },
  { key=0x11D7, label="SPK_ROBOTS",      text="Gorfian robots, attack! Attack!" },
  { key=0x11F6, label="SPK_BADMOVE",     text="Bad move" },
  { key=0x1200, label="SPK_HAHA",        text="Ha ha ha ha" },
  { key=0x120A, label="SPK_ESCAPE",      text="You cannot escape the Gorfian robots" },
  { key=0x122E, label="SPK_GOTYOU",      text="Got you" },
  { key=0x1237, label="SPK_NICE",        text="Nice shot" },
  { key=0x1243, label="SPK_TOOBAD",      text="Too bad" },
  { key=0x124B, label="SPK_PRIS",        text="Gorfians take no prisoners" },
  { key=0x1264, label="SPK_CADET",       text="Cadet" },
  { key=0x126B, label="SPK_CAPT",        text="Captain" },
  { key=0x1274, label="SPK_COLONEL",     text="Colonel" },
  { key=0x127B, label="SPK_GENERAL",     text="General" },
  { key=0x1284, label="SPK_WARRIOR",     text="Warrior" },
  { key=0x128D, label="SPK_AVENGER",     text="Avenger" },
  { key=0x1298, label="SPK_PROMOTE",     text="You have been promoted to" },
  { key=0x12B2, label="SPK_SOME",        text="Some galactic defender you are" },
  { key=0x12CE, label="SPK_BITE",        text="Bite the dust" },
  { key=0x12DB, label="SPK_HAIL",        text="All hail the supreme Gorfian Empire" },
  { key=0x12FC, label="SPK_ENEMY",       text="Another enemy ship destroyed" },
  { key=0x1317, label="SPK_BETCHA",      text="Your end draws near" },
  { key=0xA985, label="FLAGSHIP_INTRO",  text="Next time will be harder, but for now" },
  { key=0xA9A8, label="GORFIAN_CHRONICLES", text="In the Gorfian chronicles" },
  { key=0xA9C1, label="FLAGSHIP_HIT",    text="For hitting my flagship" },
  { key=0xB3BE, label="SPK_PUSH",        text="Push a player button" },
  { key=0xB3D4, label="SPK_DOOM",        text="You will meet a Gorfian doom" },
  { key=0xB3EF, label="SPK_SURVIVAL",    text="Survival is impossible" },
  { key=0xB405, label="SPK_ROBOWARRIOR", text="Robot warriors, seek and destroy the" },
  { key=0xB426, label="SPK_GORFIAN",     text="My Gorfian robots are unbeatable" },
  { key=0xB449, label="SPK_IAM",         text="I am a Gorfian consciousness" },
  { key=0xB465, label="SPK_PREPARE",     text="Prepare yourself for annihilation" },
}

local BY_KEY = {}
for i, f in ipairs(FRAGMENTS) do
  f.id = i - 1
  BY_KEY[f.key] = f
end

local RANK_KEYS = { 0x1264, 0x126B, 0x1274, 0x127B, 0x1284, 0x128D }
local RANK = "__RANK__"

local PHRASES = {}
local function add_phrase(path, components, text)
  PHRASES[#PHRASES + 1] = {
    id = #PHRASES,
    kind = "phrase",
    path = path,
    components = components,
    description = text,
  }
end

add_phrase("phrases[0]", {0x115D}, "Insert coin")
add_phrase("phrases[1]", {0x116D}, "I am the Gorfian Empire")
add_phrase("phrases[2]", {0x11C7}, "Long live Gorf")
add_phrase("phrases[3]", {0x115D}, "Insert coin")
add_phrase("SPKCOIN[0]", {0xB3BE}, "Push a player button")
add_phrase("SPKCOIN[1]", {0x11C7}, "Long live Gorf")

local generic_endings = {
  {0x118C, "Gorfians conquer another galaxy"},
  {0x11A8, "Try again; I devour your coins"},
  {0x120A, "You cannot escape the Gorfian robots"},
  {0x116D, "I am the Gorfian Empire"},
  {0x12DB, "All hail the supreme Gorfian Empire"},
}
for n, e in ipairs(generic_endings) do
  add_phrase(string.format("_SPKGENERIC[%d]", n - 1),
    {0x1243, 0x1185, RANK, e[1]},
    "Too bad Space [rank]. " .. e[2])
end
for n, e in ipairs(generic_endings) do
  add_phrase(string.format("_SPKGENERIC[%d]", n + 4),
    {0x12CE, 0x1185, RANK, e[1]},
    "Bite the dust Space [rank]. " .. e[2])
end

add_phrase("_SPKINSULT[0]", {0x1200}, "Ha ha ha ha")
add_phrase("_SPKINSULT[1]", {0x12FC}, "Another enemy ship destroyed")
add_phrase("_SPKINSULT[2]", {0x1317,0x1185,RANK}, "Your end draws near Space [rank]")
add_phrase("_SPKINSULT[3]", {0x11F6,0x1185,RANK}, "Bad move Space [rank]")
add_phrase("_SPKINSULT[4]", {0x122E,0x1185,RANK}, "Got you Space [rank]")
add_phrase("_SPKINSULT[5]", {0x12B2,0x1185,RANK}, "Some galactic defender you are Space [rank]")

add_phrase("SPEAKSTART[0]", {0x116D}, "I am the Gorfian Empire")
add_phrase("SPEAKSTART[1]", {0x11D7}, "Gorfian robots, attack! Attack!")
add_phrase("SPEAKSTART[2]", {0xB3D4,0x1185,RANK}, "You will meet a Gorfian doom Space [rank]")
add_phrase("SPEAKSTART[3]", {0xB3EF,0x1185,RANK}, "Survival is impossible Space [rank]")
add_phrase("SPEAKSTART[4]", {0x120A}, "You cannot escape the Gorfian robots")
add_phrase("SPEAKSTART[5]", {0xB405,0x1185,RANK}, "Robot warriors, seek and destroy the Space [rank]")
add_phrase("SPEAKSTART[6]", {0xB426}, "My Gorfian robots are unbeatable")
add_phrase("SPEAKSTART[7]", {0xB449}, "I am a Gorfian consciousness")
add_phrase("SPEAKSTART[8]", {0xB465,0x1185,RANK}, "Prepare yourself for annihilation Space [rank]")
add_phrase("SPEAKSTART[9]", {0x124B}, "Gorfians take no prisoners")
add_phrase("Promotion", {0x1298,0x1185,RANK}, "You have been promoted to Space [rank]")
add_phrase("Flagship sequence", {0xA985,0xA9A8,0xA9C1},
  "Next time will be harder, but for now; in the Gorfian chronicles; for hitting my flagship")

local S = {
  enabled = true,
  takeover = false,
  frame_subscription = nil,
  stop_subscription = nil,
  shortcuts = {},
  catalog = { phrase = {}, fragment = {} },
  pane = "fragment",
  selection = { phrase = 0, fragment = 0 },
  window_first = { phrase = 1, fragment = 1 },
  last_language_key = nil,
  pending = nil,
  status = "WAITING FOR GORF INITIALIZATION",
  last_controls = 0,
  last_2p_start = false,
  hold_dir = 0,
  hold_frames = 0,
  trace = nil,
  wav_enabled = false,
  wav_active = false,
  wav_filename = nil,
  wav_stop_at = nil,
  wav_batch_item = false,
  batch = nil,
  ui_dirty = false,
  draw_count = 0,
  drawchar = nil,
  x11_cache = nil,
}

local machine = manager.machine
local cpu = machine.devices[C.CPU_TAG]
if not cpu then error("[GORF SPEECH] main CPU not found at " .. C.CPU_TAG) end
local program = cpu.spaces and cpu.spaces["program"] or nil
local io = cpu.spaces and cpu.spaces["io"] or nil
if not program then error("[GORF SPEECH] main CPU program space is unavailable") end
if not io then error("[GORF SPEECH] main CPU I/O space is unavailable") end

local function printf(fmt, ...)
  print(string.format(fmt, ...))
end

local function hex2(v) return string.format("$%02X", v & 0xFF) end
local function hex4(v) return string.format("$%04X", v & 0xFFFF) end

local start_play_all
local stop_play_all
local batch_finish_item
local foreground_idle
local read_2p_start

local function read16(addr)
  local lo = program:read_u8(addr)
  local hi = program:read_u8((addr + 1) & 0xFFFF)
  return lo | (hi << 8)
end

local function write16(addr, value)
  program:write_u8(addr, value & 0xFF)
  program:write_u8((addr + 1) & 0xFFFF, (value >> 8) & 0xFF)
end

local function machine_seconds()
  local ok, value = pcall(function() return machine.time:as_double() end)
  if ok then return value end
  return 0
end

local function sc01_ready()
  local ok, raw = pcall(function() return io:read_u8(C.P1PORT_READY) end)
  return ok and ((raw & 0x80) ~= 0) or false
end

local function ring_next(p)
  p = (p + 2) & 0xFFFF
  if p > C.BOTTOMTALK then p = C.TOPTALK end
  return p
end

local function speech_snapshot()
  local p1 = io:read_u8(C.P1PORT_READY)
  local pc = cpu.state["PC"] and (cpu.state["PC"].value & 0xFFFF) or 0
  return {
    onhold = program:read_u8(C.ONHOLD),
    count = program:read_u8(C.PHONE_COUNT),
    pointer = read16(C.TALKHERE),
    talkin = read16(C.TALKIN),
    talkout = read16(C.TALKOUT),
    p1 = p1,
    ready = (p1 & 0x80) ~= 0,
    pc = pc,
  }
end

local function speech_queue_idle()
  -- GORFOS PHONE treats the speech ring as empty when TALKIN == TALKOUT and
  -- PHONE# is zero. SC-01 READY does not gate SPEAK queue insertion.
  return program:read_u8(C.ONHOLD) == 0
     and program:read_u8(C.PHONE_COUNT) == 0
     and read16(C.TALKIN) == read16(C.TALKOUT)
end

local function speech_complete()
  -- A zero PHONE# means the final byte has been sent, not necessarily that the
  -- SC-01 has finished sounding it. PHONE waits for NEWPHONE/READY before it
  -- can issue STOP, so use READY for item completion and WAV boundaries.
  return speech_queue_idle() and sc01_ready()
end

local function reset_speech_state()
  -- GORFOS Block 0123 "CLEAR THE THROAT": clear PHONE#/ONHOLD and reset
  -- both ring pointers to TOPTALK. Queue contents and TALKHERE are irrelevant
  -- once TALKIN == TALKOUT, so leave them untouched.
  program:write_u8(C.PHONE_COUNT, 0)
  program:write_u8(C.ONHOLD, 0)
  write16(C.TALKIN, C.TOPTALK)
  write16(C.TALKOUT, C.TOPTALK)
end

local function enqueue_speech_addresses(addresses)
  if not speech_queue_idle() then return false, "speech queue busy" end

  -- GORFOS speaklink after its TALKOK gate: write DE at TALKIN, advance by
  -- two bytes, wrap after BOTTOMTALK, then publish TALKIN. Lua executes while
  -- emulation is paused, so publish only after all pointers are staged.
  local slot = read16(C.TALKIN)
  local out = read16(C.TALKOUT)
  if slot < C.TOPTALK or slot > C.BOTTOMTALK or (slot & 1) ~= 0 then
    return false, string.format("invalid TALKIN %s", hex4(slot))
  end

  local staged = {}
  for _, address in ipairs(addresses) do
    if address ~= 0 then
      local next_slot = ring_next(slot)
      if next_slot == out then return false, "speech queue full" end
      staged[#staged + 1] = { slot=slot, address=address }
      slot = next_slot
    end
  end

  if #staged == 0 then return false, "all components are suppressed" end

  for _, item in ipairs(staged) do
    write16(item.slot, item.address)
  end
  write16(C.TALKIN, slot)
  return true
end

local function dip_info()
  local ok, raw = pcall(function() return io:read_u8(C.SETTINGS) end)
  if not ok then return {raw=nil, foreign=false, readable=false} end
  return {raw=raw, foreign=(raw & C.LANGUAGE_MASK) == 0, readable=true}
end

local function read_x11_message(index)
  local p = C.X11_MESSAGES
  for _ = 0, index - 1 do
    local n = program:read_u8(p)
    p = p + 1 + n
    if p > C.X11_LAST then return "" end
  end
  local n = program:read_u8(p)
  local chars = {}
  for i = 1, math.min(n, 40) do
    local b = program:read_u8(p + i)
    chars[#chars + 1] = (b >= 0x20 and b <= 0x7E) and string.char(b) or "."
  end
  return table.concat(chars)
end

local function x11_variant()
  local m0 = read_x11_message(0)
  local m1 = read_x11_message(1)
  if m0:find("QU[", 1, true) or m1:find("RIN QUJ", 1, true) then return "Klingon" end
  if m1:find("FIN DU JEU", 1, true) then return "French" end
  if m1:find("SPIEL BEENDET", 1, true) then return "German" end
  return "Foreign"
end

local function locate_x11_tables()
  if S.x11_cache then return S.x11_cache end
  if program:read_u8(C.X11_ENTRY) ~= 0xC3 then return nil end

  local key_bytes = {}
  for _, f in ipairs(FRAGMENTS) do
    key_bytes[#key_bytes + 1] = f.key & 0xFF
    key_bytes[#key_bytes + 1] = (f.key >> 8) & 0xFF
  end

  local bytes = #key_bytes
  for base = C.X11_FIRST, C.X11_LAST - bytes + 1 do
    local match = true
    for i = 1, bytes do
      if program:read_u8(base + i - 1) ~= key_bytes[i] then
        match = false
        break
      end
    end
    if match then
      local targets = base - bytes
      if targets >= C.X11_FIRST then
        local valid = true
        for i = 0, C.TRANSLATION_COUNT - 1 do
          local p = read16(targets + i * 2)
          if p ~= 0 and not (p >= C.X11_FIRST and p <= C.X11_LAST) and not BY_KEY[p] then
            valid = false
            break
          end
        end
        if valid then
          S.x11_cache = { key_table=base, target_table=targets }
          return S.x11_cache
        end
      end
    end
  end
  return nil
end

local function active_language()
  local dip = dip_info()
  if not dip.readable or not dip.foreign then
    return { key="english", name="English", valid=true, foreign=false, dip=dip }
  end

  local tables = locate_x11_tables()
  if not tables then
    return { key="foreign-invalid", name="Foreign X11 invalid", valid=false, foreign=true, dip=dip }
  end
  local variant = x11_variant()
  return {
    key=variant:lower(),
    name=variant,
    valid=true,
    foreign=true,
    dip=dip,
    target_table=tables.target_table,
    key_table=tables.key_table,
  }
end

local function translated_address(lang, key)
  if not lang.foreign then return key end
  local f = BY_KEY[key]
  if not f then return 0 end
  return read16(lang.target_table + f.id * 2)
end

local function current_rank_key()
  local rank = program:read_u8(C.SKILLFACTOR)
  if rank > 5 then rank = 5 end
  return RANK_KEYS[rank + 1], rank
end

local function expand_components(components)
  local result = {}
  for _, v in ipairs(components) do
    if v == RANK then
      result[#result + 1] = current_rank_key()
    else
      result[#result + 1] = v
    end
  end
  return result
end

local function rank_text(text)
  local key = current_rank_key()
  local f = BY_KEY[key]
  return tostring(text or ""):gsub("%[rank%]", f and f.text or "rank")
end

local function build_catalog(force)
  local lang = active_language()
  if not lang.valid then
    S.catalog.fragment = {}
    S.catalog.phrase = {}
    S.last_language_key = lang.key
    return lang, false
  end

  local lang_key = lang.key
  if lang.foreign then
    lang_key = lang_key .. ":" .. hex4(lang.target_table)
  end
  if not force and S.last_language_key == lang_key and #S.catalog.fragment > 0 then
    return lang, true
  end

  local fragments = {}
  for _, f in ipairs(FRAGMENTS) do
    local addr = translated_address(lang, f.key)
    fragments[#fragments + 1] = {
      kind="fragment", id=f.id, key=f.key, address=addr,
      label=f.label, description=f.text, playable=addr ~= 0
    }
  end

  table.sort(fragments, function(a, b)
    if a.playable ~= b.playable then return a.playable end
    if a.address ~= b.address then return a.address < b.address end
    return a.key < b.key
  end)

  local phrases = {}
  for _, p in ipairs(PHRASES) do
    local comps = expand_components(p.components)
    local first = 0
    local playable = false
    for _, key in ipairs(comps) do
      local addr = translated_address(lang, key)
      if addr ~= 0 then
        playable = true
        if first == 0 then first = addr end
      end
    end
    phrases[#phrases + 1] = {
      kind="phrase", id=p.id, path=p.path, components=p.components,
      address=first, description=rank_text(p.description), playable=playable
    }
  end

  S.catalog.fragment = fragments
  S.catalog.phrase = phrases
  S.last_language_key = lang_key

  for _, kind in ipairs({"phrase","fragment"}) do
    local n = #S.catalog[kind]
    if n == 0 then
      S.selection[kind] = 0
      S.window_first[kind] = 1
    else
      if S.selection[kind] < 0 then S.selection[kind] = 0 end
      if S.selection[kind] > n then S.selection[kind] = n end
      local max_first = math.max(1, n - C.UI_ROWS + 1)
      local first = S.window_first[kind] or 1
      if first < 1 then first = 1 end
      if first > max_first then first = max_first end
      S.window_first[kind] = first
    end
  end
  return lang, true
end

local function find_drawchar()
  -- Release-ROM signature for Gorf's native drawchar entry. Absolute CALL/JR
  -- operands are intentionally ignored.
  local fixed = {
    [0]=0xC5, [1]=0xE5, [2]=0xD5, [3]=0xD6, [4]=0x20,
    [7]=0xD6, [8]=0x0F, [9]=0xFE, [10]=0x0B,
    [13]=0xD6, [14]=0x07, [15]=0x6F, [16]=0x26, [17]=0x00,
    [18]=0x29, [19]=0x29, [20]=0x5D, [21]=0x54, [22]=0x29, [23]=0x19,
  }
  for base = 0x0000, 0x3FFF - 52 do
    local ok = true
    for off, byte in pairs(fixed) do
      if program:read_u8(base + off) ~= byte then ok = false; break end
    end
    if ok then
      -- Tail: POP DE, POP HL, LD A,H, ADD A,07, LD H,A, POP BC, RET.
      local tail = {0xD1,0xE1,0x7C,0xC6,0x07,0x67,0xC1,0xC9}
      local tail_ok = false
      for t = 40, 48 do
        local m = true
        for i = 1, #tail do
          if program:read_u8(base + t + i - 1) ~= tail[i] then m=false; break end
        end
        if m then tail_ok = true; break end
      end
      if tail_ok then return base end
    end
  end
  return nil
end

local function validate_program()
  local insert_sig = {
    0x0F,0x3E,0x27,0x0D,0x1F,0x3A,0x2A,0x3E,
    0x19,0x35,0x23,0x09,0x21,0x0D,0x0D,0x3E
  }
  for i = 1, #insert_sig do
    if program:read_u8(0x115D + i - 1) ~= insert_sig[i] then
      return false, string.format("SPK_INSERT signature differs at %s", hex4(0x115D + i - 1))
    end
  end

  local speaklink_sig = {
    0xDB,0x10,0xE6,0x80,0xC8,0xF3,0x2A,0x25,0xD1,0x73,0x23,0x72,0x23
  }
  for i = 1, #speaklink_sig do
    if program:read_u8(C.SPEAKLINK + i - 1) ~= speaklink_sig[i] then
      return false, string.format("GORFOS speaklink signature differs at %s",
        hex4(C.SPEAKLINK + i - 1))
    end
  end

  local speak_sig = { 0xDB,0x13,0xE6,0x08,0xC2,0xB8,0x10,0xC3,0x00,0xC0 }
  for i = 1, #speak_sig do
    if program:read_u8(C.SPEAK + i - 1) ~= speak_sig[i] then
      return false, string.format("GORFOS speak signature differs at %s",
        hex4(C.SPEAK + i - 1))
    end
  end

  local dc = find_drawchar()
  if not dc then return false, "Gorf drawchar signature not found in resident ROM" end
  S.drawchar = dc
  return true, string.format("Program-2 GORFOS speech path and drawchar %s match", hex4(dc))
end

local function install_idle_loop()
  for i = 1, #IDLE_LOOP_BYTES do
    program:write_u8(C.IDLE_LOOP + i - 1, IDLE_LOOP_BYTES[i])
  end
  if cpu.state["HALT"] then cpu.state["HALT"].value = 0 end
  if cpu.state["IFF1"] then cpu.state["IFF1"].value = 1 end
  if cpu.state["IFF2"] then cpu.state["IFF2"].value = 1 end
  if cpu.state["SP"] then cpu.state["SP"].value = C.CALL_STACK end
  cpu.state["PC"].value = C.IDLE_LOOP
end

local function clear_video_ram()
  for addr = 0x4000, 0x7FFF do program:write_u8(addr, 0) end
end

local function trace_state(label, st)
  st = st or speech_snapshot()
  if DEBUG_TRACE then
    printf("[GORF SPEECH DEBUG] %s hold=%02X count=%02X ptr=%04X TALKIN=%04X TALKOUT=%04X READY=%d P1=%02X PC=%04X",
      label, st.onhold, st.count, st.pointer, st.talkin, st.talkout,
      st.ready and 1 or 0, st.p1, st.pc)
  end
  return st
end

local function fragment_stream(address)
  if address == 0 then return 0, "", "" end
  local count = program:read_u8(address)
  local names, raws = {}, {}
  for i = 0, count - 1 do
    local raw = program:read_u8(address + 1 + i)
    local phone = raw & 0x3F
    names[#names + 1] = SC01_NAMES[phone] or string.format("P%02X", phone)
    raws[#raws + 1] = string.format("%02X", raw)
  end
  return count, table.concat(names, " "), table.concat(raws, " ")
end

local function phrase_components_for_entry(entry)
  return expand_components(entry.components or {})
end

local function trace_request(entry, kind)
  local pre = trace_state("PRE")
  print("")

  if kind == "fragment" then
    printf("[GORF SPEECH] PLAY FRAGMENT key=%s address=%s text=\"%s\"",
      hex4(entry.key), hex4(entry.address), tostring(entry.description or ""))
    local count, phones, raws = fragment_stream(entry.address)
    printf("[GORF SPEECH] PHONEMES %s", phones)
    if DEBUG_TRACE then
      printf("[GORF SPEECH DEBUG] FRAGMENT length=%s payload=%s-%s",
        hex2(count), hex4(entry.address + 1), hex4(entry.address + count))
      printf("[GORF SPEECH DEBUG] RAW      %s", raws)
    end
  else
    printf("[GORF SPEECH] PLAY PHRASE id=%s path=\"%s\" text=\"%s\"",
      hex2(entry.id), tostring(entry.path or ""), tostring(entry.description or ""))

    local lang = active_language()
    local comps = phrase_components_for_entry(entry)
    for n, key in ipairs(comps) do
      local f = BY_KEY[key]
      local addr = translated_address(lang, key)
      printf("[GORF SPEECH]   FRAGMENT %d/%d key=%s address=%s text=\"%s\"%s",
        n, #comps, hex4(key), hex4(addr), f and f.text or "unknown",
        addr == 0 and " [SUPPRESSED]" or "")
      if addr ~= 0 then
        local count, phones, raws = fragment_stream(addr)
        printf("[GORF SPEECH]     PHONEMES %s", phones)
        if DEBUG_TRACE then
          printf("[GORF SPEECH DEBUG]     length=%s payload=%s-%s",
            hex2(count), hex4(addr + 1), hex4(addr + count))
          printf("[GORF SPEECH DEBUG]     RAW      %s", raws)
        end
      else
        print("[GORF SPEECH]     PHONEMES <suppressed by active X11>")
      end
    end
  end

  S.trace = {
    kind=kind,
    id=entry.id,
    key=entry.key,
    address=entry.address,
    path=entry.path,
    description=entry.description,
    start_time=machine_seconds(),
    last_progress_time=machine_seconds(),
    last_pointer=pre.pointer,
    last_count=pre.count,
    last_talkout=pre.talkout,
    seq=0,
    stall_reported=false,
    seen_activity=false,
    last_phone=nil,
    last_transition=nil,
  }
end

local function trace_started()
  if not S.trace then return end
  local st = trace_state("START")
  S.trace.last_pointer = st.pointer
  S.trace.last_count = st.count
  S.trace.last_talkout = st.talkout
  S.trace.last_progress_time = machine_seconds()
  if st.count ~= 0 or st.talkin ~= st.talkout then
    S.trace.seen_activity = true
  end
end

local function trace_progress()
  local t = S.trace
  if not t then return end

  local st = speech_snapshot()
  local now = machine_seconds()
  local busy = st.count ~= 0 or st.talkin ~= st.talkout
  if busy then t.seen_activity = true end

  local progressed = st.pointer ~= t.last_pointer
                  or st.count ~= t.last_count
                  or st.talkout ~= t.last_talkout

  if st.pointer ~= t.last_pointer and st.pointer ~= 0 then
    local addr = (st.pointer - 1) & 0xFFFF
    local raw = program:read_u8(addr)
    local phone = raw & 0x3F
    local name = SC01_NAMES[phone] or string.format("P%02X", phone)
    local transition = (t.last_phone or "START") .. "->" .. name
    t.seq = t.seq + 1
    t.last_phone = name
    t.last_transition = transition
    if DEBUG_TRACE then
      printf("[GORF SPEECH DEBUG] #%02d ROM=%04X raw=%02X phone=%02X %-4s transition=%s count=%02X ptr=%04X TALKOUT=%04X READY=%d",
        t.seq, addr, raw, phone, name, transition,
        st.count, st.pointer, st.talkout, st.ready and 1 or 0)
    end
  elseif progressed and DEBUG_TRACE then
    printf("[GORF SPEECH DEBUG] PROGRESS count=%02X ptr=%04X TALKOUT=%04X READY=%d",
      st.count, st.pointer, st.talkout, st.ready and 1 or 0)
  end

  if progressed then
    t.last_progress_time = now
    t.stall_reported = false
    t.last_pointer = st.pointer
    t.last_count = st.count
    t.last_talkout = st.talkout
  end

  -- GORFOS PHONE advances TALKOUT when it consumes the final primitive byte.
  -- Use the native ring state to close this browser item so play-all can
  -- queue the next record. The next record may wait in the ring until SC-01
  -- READY; that is normal Gorf behavior.
  if t.seen_activity and speech_queue_idle() then
    trace_state("END", st)
    if t.kind == "fragment" then
      printf("[GORF SPEECH] END FRAGMENT key=%s address=%s elapsed=%.3fs",
        hex4(t.key), hex4(t.address), now - t.start_time)
    else
      printf("[GORF SPEECH] END PHRASE id=%s path=\"%s\" elapsed=%.3fs",
        hex2(t.id), tostring(t.path or ""), now - t.start_time)
    end
    local batch_item = S.batch and S.batch.current_index ~= nil
    S.trace = nil
    if S.wav_active then
      S.wav_stop_at = now + C.WAV_POSTROLL_SEC
    elseif batch_item then
      batch_finish_item()
    end
    return
  end

  if busy and (now - t.last_progress_time) >= C.TRACE_STALL_SEC and not t.stall_reported then
    t.stall_reported = true
    trace_state("STALL", st)
    printf("[GORF SPEECH] STALL %.2fs: %s last_source=%s last_phone=%s transition=%s count=%02X TALKIN=%04X TALKOUT=%04X READY=%d",
      now - t.last_progress_time, t.kind:upper(),
      hex4((st.pointer - 1) & 0xFFFF), tostring(t.last_phone or "none"),
      tostring(t.last_transition or "none"), st.count, st.talkin, st.talkout,
      st.ready and 1 or 0)
  end
end

local function wav_language_slug()
  local lang = active_language()
  local s = tostring(lang.key or "unknown"):lower()
  s = s:gsub("[^a-z0-9]+", "_"):gsub("^_+", ""):gsub("_+$", "")
  return s ~= "" and s or "unknown"
end

local function wav_filename(entry, kind)
  if kind == "fragment" then
    return string.format("gorf_%s_fragment_%04X.wav",
      wav_language_slug(), entry.address & 0xFFFF)
  end
  return string.format("gorf_%s_phrase_%02X.wav",
    wav_language_slug(), entry.id & 0xFF)
end

local function stop_owned_wav(reason)
  if not S.wav_active then return end
  pcall(function() machine.sound:stop_recording() end)
  printf("[GORF SPEECH] WAV %s: %s", reason or "saved", tostring(S.wav_filename or ""))
  S.wav_active = false
  S.wav_filename = nil
  S.wav_stop_at = nil
  S.wav_batch_item = false
end

local function start_entry_wav(entry, kind, batch_item)
  if S.wav_active then return false, "browser WAV recorder is still active" end

  local already = false
  pcall(function() already = machine.sound.recording == true end)
  if already then return false, "MAME sound recorder is already active" end

  local name = wav_filename(entry, kind)
  local ok, started = pcall(function() return machine.sound:start_recording(name) end)
  if not ok or not started then return false, "MAME could not start WAV " .. name end

  S.wav_active = true
  S.wav_filename = name
  S.wav_stop_at = nil
  S.wav_batch_item = batch_item == true
  return true
end

local function service_wav_capture()
  if not S.wav_active or not S.wav_stop_at then return end
  if machine_seconds() < S.wav_stop_at then return end

  local was_batch = S.wav_batch_item
  local name = S.wav_filename
  pcall(function() machine.sound:stop_recording() end)
  S.wav_active = false
  S.wav_filename = nil
  S.wav_stop_at = nil
  S.wav_batch_item = false
  printf("[GORF SPEECH] WAV saved: %s", tostring(name or ""))

  if was_batch then batch_finish_item() end
end

local function set_wav_capture(value)
  if value == nil then
    S.wav_enabled = not S.wav_enabled
  elseif type(value) == "boolean" then
    S.wav_enabled = value
  elseif type(value) == "number" then
    S.wav_enabled = value ~= 0
  elseif type(value) == "string" then
    local v = value:lower()
    if v == "on" or v == "true" or v == "1" then S.wav_enabled = true
    elseif v == "off" or v == "false" or v == "0" then S.wav_enabled = false
    else
      print("[GORF SPEECH] usage: gwav() | gwav(true) | gwav(false)")
      return S.wav_enabled
    end
  else
    print("[GORF SPEECH] usage: gwav() | gwav(true) | gwav(false)")
    return S.wav_enabled
  end
  printf("[GORF SPEECH] WAV capture for Fire plays: %s", S.wav_enabled and "ON" or "OFF")
  return S.wav_enabled
end

local function set_debug_trace(value)
  if value == nil then
    DEBUG_TRACE = not DEBUG_TRACE
  elseif type(value) == "boolean" then
    DEBUG_TRACE = value
  elseif type(value) == "number" then
    DEBUG_TRACE = value ~= 0
  elseif type(value) == "string" then
    local v = value:lower()
    if v == "on" or v == "true" or v == "1" then DEBUG_TRACE = true
    elseif v == "off" or v == "false" or v == "0" then DEBUG_TRACE = false
    else
      print("[GORF SPEECH] usage: gtrace() | gtrace(true) | gtrace(false)")
      return DEBUG_TRACE
    end
  else
    print("[GORF SPEECH] usage: gtrace() | gtrace(true) | gtrace(false)")
    return DEBUG_TRACE
  end
  printf("[GORF SPEECH] detailed trace: %s", DEBUG_TRACE and "ON" or "OFF")
  return DEBUG_TRACE
end

local function print_console_commands()
  print("")
  print("[GORF SPEECH] console commands:")
  print("[GORF SPEECH]   gwav()         toggle WAV capture for Fire plays")
  print("[GORF SPEECH]   gwav(true)     WAV capture ON")
  print("[GORF SPEECH]   gwav(false)    WAV capture OFF")
  print("[GORF SPEECH]   gall()         play all entries in current view")
  print("[GORF SPEECH]   gstop()        stop play-all after current item")
  print("[GORF SPEECH]   gtrace()       toggle detailed speech trace")
  print("[GORF SPEECH]   gtrace(true)   detailed trace ON")
  print("[GORF SPEECH]   gtrace(false)  detailed trace OFF")
  print("[GORF SPEECH]   gexit()        exit MAME")
  print("[GORF SPEECH]   ghelp()        show this command list")
  print("")
  printf("[GORF SPEECH]   current WAV capture: %s", S.wav_enabled and "ON" or "OFF")
  printf("[GORF SPEECH]   current trace: %s", DEBUG_TRACE and "ON" or "OFF")
end

local function install_console_shortcut(name, handler)
  local previous = rawget(_G, name)
  S.shortcuts[name] = { handler=handler, previous=previous, restore=previous ~= nil }
  rawset(_G, name, handler)
end

local function install_console_shortcuts()
  install_console_shortcut("gwav", function(value) return set_wav_capture(value) end)
  install_console_shortcut("gall", function() return start_play_all() end)
  install_console_shortcut("gstop", function() return stop_play_all() end)
  install_console_shortcut("gtrace", function(value) return set_debug_trace(value) end)
  install_console_shortcut("gexit", function() machine:exit() end)
  install_console_shortcut("ghelp", function() print_console_commands() end)
end

local function restore_console_shortcuts()
  for name, shortcut in pairs(S.shortcuts) do
    if rawget(_G, name) == shortcut.handler then
      if shortcut.restore then rawset(_G, name, shortcut.previous)
      else rawset(_G, name, nil) end
    end
  end
  S.shortcuts = {}
end

local function takeover(reason)
  if S.takeover then return true end

  local ok, why = validate_program()
  if not ok then
    S.status = "PROGRAM VALIDATION FAILED"
    printf("[GORF SPEECH] takeover refused: %s", why)
    return false
  end

  local lang, cat_ok = build_catalog(true)
  if not cat_ok then
    S.status = "FOREIGN DIP SELECTED X11 INVALID"
    printf("[GORF SPEECH] takeover refused: %s", lang.name)
    return false
  end

  reset_speech_state()
  clear_video_ram()
  install_idle_loop()

  S.takeover = true
  S.selection.phrase = 0
  S.selection.fragment = 0
  S.window_first.phrase = 1
  S.window_first.fragment = 1
  S.trace = nil
  S.batch = nil
  S.wav_active = false
  S.wav_filename = nil
  S.wav_stop_at = nil
  S.wav_batch_item = false
  S.last_controls = 0
  S.last_2p_start = read_2p_start()
  S.hold_dir = 0
  S.hold_frames = 0
  S.pending = nil
  S.status = "READY"
  S.ui_dirty = true

  printf("[GORF SPEECH] browser takeover active (%s); language=%s", reason or "manual", lang.name)
  printf("[GORF SPEECH] catalog: %d phrases, %d fragments; TALKIN=%s drawchar=%s",
    #S.catalog.phrase, #S.catalog.fragment, hex4(C.TALKIN), hex4(S.drawchar))
  if lang.foreign then
    printf("[GORF SPEECH] X11 tables: targets=%s keys=%s",
      hex4(lang.target_table), hex4(lang.key_table))
  end
  print("[GORF SPEECH] controls: UP/DOWN select; LEFT/RIGHT type; FIRE play; 1P exit; 2P play all/stop")
  return true
end

local function current_list(kind)
  kind = kind or S.pane
  return S.catalog[kind], kind
end

local function selected_entry(kind)
  local list, actual = current_list(kind)
  local index = S.selection[actual]
  return index, list[index], actual
end

local function keep_selection_visible(kind, index)
  local list = S.catalog[kind]
  if not list or #list == 0 or not index or index < 1 then return end
  local first = S.window_first[kind] or 1
  if index < first then first = index
  elseif index > first + C.UI_ROWS - 1 then first = index - C.UI_ROWS + 1 end
  local max_first = math.max(1, #list - C.UI_ROWS + 1)
  if first < 1 then first = 1 end
  if first > max_first then first = max_first end
  S.window_first[kind] = first
end

local function request_entry(index, kind, source)
  local list = S.catalog[kind]
  local e = list and list[index] or nil
  if not e then return false, "index out of range" end
  if e.playable == false then return false, "entry is suppressed" end
  S.selection[kind] = index
  keep_selection_visible(kind, index)
  S.pending = {index=index, kind=kind, entry=e, source=source or "manual"}
  S.status = string.format("QUEUED %s %d", kind:upper(), index)
  S.ui_dirty = true
  return true
end

local function start_entry(entry, kind)
  local lang = active_language()
  local addresses = {}

  if kind == "fragment" then
    local address = translated_address(lang, entry.key)
    if address ~= 0 then addresses[#addresses + 1] = address end
  else
    for _, key in ipairs(phrase_components_for_entry(entry)) do
      local address = translated_address(lang, key)
      if address ~= 0 then addresses[#addresses + 1] = address end
    end
  end

  return enqueue_speech_addresses(addresses)
end

batch_finish_item = function()
  local b = S.batch
  if not b or not b.active or not b.current_index then return end
  b.completed = b.completed + 1
  b.current_index = nil
  if b.stop_requested then
    printf("[GORF SPEECH] play-all stopped: %d/%d completed", b.completed, b.total)
    S.batch = nil
    S.ui_dirty = true
  elseif b.completed >= b.total then
    printf("[GORF SPEECH] play-all complete: %d %s entries", b.completed, b.kind)
    S.batch = nil
    S.ui_dirty = true
  end
end

local function service_batch()
  local b = S.batch
  if not b or not b.active then return end

  if b.stop_requested and not b.current_index then
    printf("[GORF SPEECH] play-all stopped: %d/%d completed", b.completed, b.total)
    S.batch = nil
    S.ui_dirty = true
    return
  end
  if b.current_index or S.pending or S.trace or S.wav_active then return end
  if not speech_queue_idle() then return end

  while b.next_index <= #S.catalog[b.kind] do
    local index = b.next_index
    b.next_index = b.next_index + 1
    local e = S.catalog[b.kind][index]
    if e and e.playable ~= false then
      b.current_index = index
      local ok, err = request_entry(index, b.kind, "batch")
      if not ok then
        printf("[GORF SPEECH] play-all error: %s", tostring(err))
        S.batch = nil
        S.ui_dirty = true
      end
      return
    end
  end

  printf("[GORF SPEECH] play-all complete: %d %s entries", b.completed, b.kind)
  S.batch = nil
  S.ui_dirty = true
end

start_play_all = function()
  if not S.takeover then
    print("[GORF SPEECH] gall(): browser has not taken over yet")
    return false
  end
  if S.batch then
    print("[GORF SPEECH] gall(): play-all is already running")
    return false
  end
  if S.pending or S.trace or S.wav_active or not speech_queue_idle() then
    print("[GORF SPEECH] gall(): wait for current speech/WAV to finish")
    return false
  end

  local kind = S.pane
  local total = 0
  for _, e in ipairs(S.catalog[kind]) do
    if e.playable ~= false then total = total + 1 end
  end
  if total == 0 then
    printf("[GORF SPEECH] gall(): no playable %s entries", kind)
    return false
  end

  S.batch = {
    active=true, kind=kind, next_index=1, current_index=nil,
    completed=0, total=total, stop_requested=false,
  }
  S.ui_dirty = true
  printf("[GORF SPEECH] play-all: %d %s entries; WAV capture %s",
    total, kind, S.wav_enabled and "ON" or "OFF")
  return true
end

stop_play_all = function()
  if not S.batch then
    print("[GORF SPEECH] gstop(): no play-all run is active")
    return false
  end

  local b = S.batch
  b.stop_requested = true

  if S.pending and S.pending.source == "batch" and not S.trace and not S.wav_active then
    S.pending = nil
    b.current_index = nil
    printf("[GORF SPEECH] play-all stopped: %d/%d completed", b.completed, b.total)
    S.batch = nil
    S.ui_dirty = true
  elseif b.current_index then
    print("[GORF SPEECH] play-all will stop after the current item")
  else
    printf("[GORF SPEECH] play-all stopped: %d/%d completed", b.completed, b.total)
    S.batch = nil
    S.ui_dirty = true
  end
  return true
end

foreground_idle = function()
  if not S.takeover or not cpu.state["PC"] then return false end
  local pc = cpu.state["PC"].value & 0xFFFF
  return pc >= C.IDLE_LOOP and pc <= (C.IDLE_LOOP + #IDLE_LOOP_BYTES - 1)
end

local function service_pending()
  if not S.pending or not S.takeover or not speech_queue_idle() then return end
  if S.wav_active then return end

  local p = S.pending
  local batch_item = p.source == "batch"

  if batch_item and S.batch and S.batch.stop_requested then
    S.pending = nil
    S.batch.current_index = nil
    printf("[GORF SPEECH] play-all stopped: %d/%d completed", S.batch.completed, S.batch.total)
    S.batch = nil
    S.ui_dirty = true
    return
  end

  if S.wav_enabled then
    local wav_ok, wav_err = start_entry_wav(p.entry, p.kind, batch_item)
    if not wav_ok then
      printf("[GORF SPEECH] WAV ERROR: %s", tostring(wav_err))
      if batch_item then
        print("[GORF SPEECH] play-all aborted because WAV capture is ON but recording could not start")
        S.pending = nil
        S.batch = nil
        return
      end
    end
  end

  trace_request(p.entry, p.kind)
  local ok, err = start_entry(p.entry, p.kind)
  if ok then
    trace_started()
    S.status = string.format("PLAYING %s %d", p.kind:upper(), p.index)
    S.pending = nil
  elseif err ~= "speech busy" then
    S.status = "ERROR: " .. tostring(err)
    printf("[GORF SPEECH] START ERROR: %s", tostring(err))
    S.trace = nil
    S.pending = nil
    if S.wav_active then stop_owned_wav("discarded") end
    if batch_item then S.batch = nil end
  end
end

local function move_selection(delta)
  local kind = S.pane
  local list = S.catalog[kind]
  if #list == 0 then return end
  local current = S.selection[kind] or 0
  local n
  if current == 0 then n = (delta < 0) and #list or 1
  else
    n = current + delta
    if n < 1 then n = #list
    elseif n > #list then n = 1 end
  end
  S.selection[kind] = n
  keep_selection_visible(kind, n)
  S.status = "READY"
  S.ui_dirty = true
end

local function select_pane(kind)
  if kind ~= "phrase" and kind ~= "fragment" then return end
  if S.pane == kind then return end
  S.pane = kind
  S.status = "READY"
  S.ui_dirty = true
end

local function read_controls()
  -- GORFOS GJ: upright Player 1 is port $12. Only the lower joystick
  -- nibble is inverted. SWFIRE is bit $10 and is active-low.
  local raw = io:read_u8(C.P1PORT_READY)
  local joy = (raw ~ 0x0F) & 0x0F
  local fire = ((raw & 0x10) == 0) and 0x10 or 0
  return joy | fire
end

local function read_1p_start()
  -- Gorf COINPORT bit 4: 1-player Start, active low.
  return ((~io:read_u8(C.COINPORT)) & 0x10) ~= 0
end

read_2p_start = function()
  -- Gorf COINPORT bit 5: 2-player Start, active low.
  return ((~io:read_u8(C.COINPORT)) & 0x20) ~= 0
end

local function process_inputs()
  if not S.takeover then return end

  local c = read_controls()
  local start2 = read_2p_start()
  local start2_pressed = start2 and not S.last_2p_start

  if S.batch then
    if start2_pressed then stop_play_all() end
    if read_1p_start() then machine:exit() end
    S.last_controls = c
    S.last_2p_start = start2
    return
  end

  if start2_pressed then start_play_all() end

  local pressed = c & (~S.last_controls) & 0x3F
  if (pressed & 0x0C) ~= 0 then
    select_pane(S.pane == "fragment" and "phrase" or "fragment")
  end

  local dir = 0
  if (c & 0x01) ~= 0 and (c & 0x02) == 0 then dir = -1
  elseif (c & 0x02) ~= 0 and (c & 0x01) == 0 then dir = 1 end

  if dir ~= 0 then
    if dir ~= S.hold_dir then
      S.hold_dir = dir
      S.hold_frames = 0
      move_selection(dir)
    else
      S.hold_frames = S.hold_frames + 1
      if S.hold_frames >= C.INPUT_INITIAL_REPEAT
          and ((S.hold_frames - C.INPUT_INITIAL_REPEAT) % C.INPUT_REPEAT_RATE) == 0 then
        move_selection(dir)
      end
    end
  else
    S.hold_dir = 0
    S.hold_frames = 0
  end

  if (pressed & 0x10) ~= 0 then
    local index, e, kind = selected_entry()
    if e then
      local ok, err = request_entry(index, kind)
      if not ok then S.status = "ERROR: " .. tostring(err) end
    end
  end

  if read_1p_start() then
    if S.trace then trace_state("EXIT") end
    machine:exit()
  end

  S.last_controls = c
  S.last_2p_start = start2
end

local function transliterate_for_gorf(text)
  local s = tostring(text or "")
  local repl = {
    ["Ä"]="AE", ["Ö"]="OE", ["Ü"]="UE", ["ẞ"]="SS",
    ["ä"]="AE", ["ö"]="OE", ["ü"]="UE", ["ß"]="SS",
    ["é"]="E", ["è"]="E", ["ê"]="E", ["à"]="A", ["ç"]="C",
    ["É"]="E", ["È"]="E", ["Ê"]="E", ["À"]="A", ["Ç"]="C",
  }
  for from,to in pairs(repl) do s = s:gsub(from,to) end
  s = s:upper()

  local out = {}
  for i = 1, #s do
    local b = s:byte(i)
    if (b >= 0x30 and b <= 0x39) or (b >= 0x41 and b <= 0x5A) or b == 0x20 then
      out[#out + 1] = string.char(b)
    else
      out[#out + 1] = " "
    end
  end
  return table.concat(out)
end

local function fixed_native_text(text, width)
  local s = transliterate_for_gorf(text)
  if #s > width then s = s:sub(1, width) end
  if #s < width then s = s .. string.rep(" ", width - #s) end
  return s
end

local function native_center(text)
  local s = transliterate_for_gorf(text)
  if #s > C.UI_WIDTH then s = s:sub(1, C.UI_WIDTH) end
  local col = math.max(0, (C.UI_WIDTH - #s) // 2)
  return string.rep(" ", col) .. s .. string.rep(" ", C.UI_WIDTH - col - #s)
end

local function native_menu_lines()
  local lang = active_language()
  local list = S.catalog[S.pane]
  local selected = S.selection[S.pane]
  local first = S.window_first[S.pane] or 1
  local max_first = math.max(1, #list - C.UI_ROWS + 1)
  if first < 1 then first = 1 end
  if first > max_first then first = max_first end
  S.window_first[S.pane] = first

  local lines = {}
  local pane_name = S.pane == "phrase" and "PHRASES" or "FRAGMENTS"
  lines[#lines + 1] = { row=0, text=native_center(lang.name .. " " .. pane_name), attr=C.ATTR_BLUE }

  local vmaj, vmin, vpatch = VERSION:match("^(%d+)%.(%d+)%.(%d+)")
  local short_version = vmaj and ("V" .. vmaj .. vmin .. vpatch) or "VER"
  lines[#lines + 1] = {
    row=1,
    text=string.rep(" ", math.max(0, C.UI_WIDTH - #short_version)) .. short_version,
    attr=C.ATTR_BLUE
  }

  for row = 0, C.UI_ROWS - 1 do
    local idx = first + row
    local e = list[idx]
    local line
    if e then
      if S.pane == "fragment" then
        local addr = e.address ~= 0 and string.format("%04X", e.address) or "0000"
        local desc = e.description
        if e.address == 0 then desc = "SUPPRESSED " .. desc end
        line = " " .. addr .. " " .. fixed_native_text(desc, C.UI_WIDTH - 6)
      else
        line = " " .. string.format("%02X", e.id) .. " " ..
               fixed_native_text(e.description, C.UI_WIDTH - 4)
      end
    else
      line = string.rep(" ", C.UI_WIDTH)
    end
    lines[#lines + 1] = {
      row=2 + row,
      text=fixed_native_text(line, C.UI_WIDTH),
      attr=(idx == selected) and C.ATTR_YELLOW or C.ATTR_RED
    }
  end

  lines[#lines + 1] = { row=10, text=native_center("UP DOWN SELECT FIRE PLAY"), attr=C.ATTR_YELLOW }
  lines[#lines + 1] = { row=11, text=native_center("LEFT RIGHT CHANGE TYPE"), attr=C.ATTR_YELLOW }
  lines[#lines + 1] = {
    row=12,
    text=native_center(S.batch and "1P EXIT 2P STOP" or "1P EXIT 2P PLAY ALL"),
    attr=C.ATTR_YELLOW
  }
  return lines
end

local function screen_line_x(row)
  -- Gorf RELABS takes its integer X coordinate from D.  Native text screens
  -- commonly use X=$3C,$32,$28,$1E (10-pixel spacing); the browser uses
  -- six-pixel spacing to fit its compact 13-row layout in the 80-pixel axis.
  return ((76 - row * 6) & 0xFF) << 8
end

local function centered_text_y(text)
  -- Native CSPELL centers at $6000 and subtracts $0380 per character.
  -- At 28 characters the start would go negative and wrap to $FE00, so
  -- browser lines are deliberately limited to 27 characters.
  return 0x6000 - (#text * 0x0380)
end

local function write_native_draw_program(lines)
  if not S.drawchar then return false, "native drawchar address unavailable" end

  local data = C.DRAW_DATA
  local strings = {}
  for _, line in ipairs(lines) do
    if #line.text > C.UI_WIDTH then
      return false, string.format("native UI line exceeds %d-character Gorf limit", C.UI_WIDTH)
    end
    local addr = data
    for i = 1, #line.text do
      program:write_u8(data, line.text:byte(i))
      data = data + 1
    end
    program:write_u8(data, 0)
    data = data + 1
    strings[#strings + 1] = addr
  end

  local code = {}
  local function emit(v) code[#code + 1] = v & 0xFF end
  local function emit16(v) emit(v); emit(v >> 8) end

  -- DI / PUSH IX / PUSH IY.
  emit(0xF3); emit(0xDD); emit(0xE5); emit(0xFD); emit(0xE5)

  local call_sites = {}
  for i, line in ipairs(lines) do
    emit(0x01); emit16(line.attr)                 -- LD BC,exp/mag
    emit(0x11); emit16(screen_line_x(line.row))  -- LD DE,X
    emit(0x21); emit16(centered_text_y(line.text)) -- LD HL,Y, centered
    emit(0xDD); emit(0x21); emit16(strings[i])   -- LD IX,string
    emit(0xCD); call_sites[#call_sites + 1] = #code + 1
    emit16(0)                                    -- CALL draw_string (patched below)
  end

  -- POP IY / POP IX / EI / JP HALT.
  emit(0xFD); emit(0xE1); emit(0xDD); emit(0xE1)
  emit(0xFB)
  emit(0xC3); emit16(C.IDLE_LOOP + 1)

  local draw_string = C.DRAW_CODE + #code

  -- draw_string:
  -- Preserve IX across drawchar; native Gorf callers protect IX/IY around it.
  emit(0xDD); emit(0x7E); emit(0x00)    -- LD A,(IX+0)
  emit(0xB7)                            -- OR A
  emit(0xC8)                            -- RET Z
  emit(0xDD); emit(0x23)                -- INC IX
  emit(0xDD); emit(0xE5)                -- PUSH IX
  emit(0xCD); emit16(S.drawchar)        -- CALL drawchar
  emit(0xDD); emit(0xE1)                -- POP IX
  emit(0x18); emit(0xF0)                -- JR draw_string (-16)

  for _, pos in ipairs(call_sites) do
    code[pos] = draw_string & 0xFF
    code[pos + 1] = (draw_string >> 8) & 0xFF
  end

  if C.DRAW_CODE + #code >= C.DRAW_DATA then
    return false, "native UI code exceeds reserved RAM"
  end
  if data >= C.CALL_STACK - 0x40 or data > 0xD7FF then
    return false, "native UI strings exceed Gorf work RAM"
  end

  for i, b in ipairs(code) do program:write_u8(C.DRAW_CODE + i - 1, b) end
  return true
end

local function render_ui_native()
  if not S.takeover or not S.ui_dirty or not foreground_idle() then return end
  local ok, err = write_native_draw_program(native_menu_lines())
  if not ok then
    S.status = "ERROR: " .. err
    printf("[GORF SPEECH] %s", err)
    S.ui_dirty = false
    return
  end
  if cpu.state["SP"] then cpu.state["SP"].value = C.CALL_STACK end
  if cpu.state["HALT"] then cpu.state["HALT"].value = 0 end
  cpu.state["PC"].value = C.DRAW_CODE
  S.ui_dirty = false
  S.draw_count = S.draw_count + 1
end

local function on_frame()
  if not S.enabled then return end

  local lang = active_language()
  local key = lang.key
  if lang.foreign and lang.target_table then key = key .. ":" .. hex4(lang.target_table) end
  if key ~= S.last_language_key then
    S.x11_cache = nil
    build_catalog(true)
    if S.takeover then S.status = "LANGUAGE CHANGED " .. lang.name:upper() end
    S.pending = nil
      S.ui_dirty = true
  end

  if not S.takeover then
    if machine_seconds() >= C.TAKEOVER_DELAY_SEC and speech_complete() then
      takeover("auto")
    end
    return
  end

  process_inputs()
  trace_progress()
  service_wav_capture()
  service_batch()
  service_pending()
  render_ui_native()
end

print("============================================================")
printf("[GORF SPEECH] GORF SPEECH BROWSER %s", VERSION)
printf("[GORF SPEECH] takeover RAM: $%04X; native UI code: $%04X; ROM patching: NONE",
  C.IDLE_LOOP, C.DRAW_CODE)
print("[GORF SPEECH] display: native Gorf drawchar + resident CHRTBL; MAME overlay: NONE")
install_console_shortcuts()
print_console_commands()
print("============================================================")

build_catalog(true)

if emu.add_machine_frame_notifier then
  S.frame_subscription = emu.add_machine_frame_notifier(on_frame)
else
  emu.register_frame_done(on_frame, "gorf_speech_browser")
end

if emu.add_machine_stop_notifier then
  S.stop_subscription = emu.add_machine_stop_notifier(function()
    S.enabled = false
    if S.wav_active then stop_owned_wav("closed") end
    restore_console_shortcuts()
  end)
end

print(string.format(
  "[GORF SPEECH] %s loaded from %s; Gorf boots normally, then browser takeover begins after %.1fs when the GORFOS speech path is complete",
  VERSION, BUILD_FILE, C.TAKEOVER_DELAY_SEC))
