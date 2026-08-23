-- gorf_sound_browser.lua
-- Gorf Program 2 native ROM sound-event browser for MAME 0.289+
--
-- Gorf boots normally, then the browser takes over the foreground with a small
-- Z80 loop in work RAM and a native Gorf drawchar UI.  Sound playback does not
-- use captured register streams, JSON libraries, WebAudio, or synthesized
-- approximations.  Every catalog entry invokes a Program-2 TERSE word, an exact
-- game sound routine, or the same native music-engine submission used by game
-- code. Composite TERSE events retain their original two-processor sequencing.
--
-- While the browser owns the foreground, its HALT loop calls Gorf's native
-- busaround routine once after every interrupt.  This is required because the
-- Gorf music interpreter is foreground-serviced; interrupts alone do not
-- advance an active ROM score.
--
-- Lua does not write sound registers, capture sound streams, or synthesize audio.
-- It injects only the foreground/browser launcher code, reads the native Gorf
-- music-processor work arrays for status, and optionally asks MAME to record WAV.
--
-- Controls:
--   UP / DOWN     move selection through the complete catalog
--   FIRE          play/stop the selected sound event
--   1P START      exit MAME
--   2P START      play all / stop current event and cancel play-all
--
-- Console:
--   gswav() / gswav(true|false)  toggle/set per-score WAV capture
--   gsall()                       play the complete catalog
--   gsstop()                      stop current score/play-all
--   gsselect(n)                   select catalog item n
--   gsplay(n)                     play catalog item n
--   gslist()                      list the complete ROM event catalog
--   gsinfo()                      show selected score details
--   gsaudit()                     dump selected score and music-engine state
--   gsstate()                     dump native Gorf music-processor state
--   gstrace() / gstrace(true|false) toggle/set score-transition logging
--   gsdiag()                      dump fixed Gorf music-engine anchors
--   gsexit()                      exit MAME
--   gshelp()                      show commands

local VERSION = "0.5.5-20260823-1050"
local BUILD_FILE = "gorf_sound_browser.lua"

local C = {
  CPU_TAG = ":maincpu",

  -- Gorf input ports (reads) share port numbers with primary sound writes.
  COINPORT = 0x10,
  P1PORT = 0x12,

  -- Gorf Program-2 low-level music engine.
  -- These are fixed release-ROM entry points recovered from GORFOS/TERSE
  -- and confirmed by direct game callers in the Program-2 disassembly.
  MUSICFLAG = 0xD0AF,
  PRIMARY_MUSIC = 0xD0B1,
  SECONDARY_MUSIC = 0xD0E1,
  ENDMUS = 0x0B85,
  EMUSIC = 0x0B86,
  WRITE = 0x062E,
  RELABS = 0xD080,
  BUSAROUND = 0x0F7E,
  BMUSIC = 0x0FAC,
  PMUSIC = 0x0FC2,
  MMUSIC = 0x0FDB,
  MPMUSIC = 0x0FF0,
  DSPATCH = 0x005A,

  -- Browser work RAM. Matches the proven speech-browser layout.
  IDLE_LOOP = 0xD400,
  DRAW_CODE = 0xD420,
  PLAY_CODE = 0xD700,
  DRAW_DATA = 0xD560,
  TERSE_THREAD = 0xD6C0,
  TERSE_EXIT = 0xD6D0,
  TERSE_RETURN_STACK = 0xD6F0,
  CALL_STACK = 0xD7E0,

  -- Exclusive generated-code/data limits. Exact-fit programs are valid.
  DRAW_CODE_LIMIT = 0xD560,
  DRAW_DATA_LIMIT = 0xD6C0,
  PLAY_CODE_LIMIT = 0xD7A0,

  -- emusic clears this inclusive work-array range. quityet subsequently lets
  -- musinterp commit the post-QUIET HL value back to MUSPC, so this reset
  -- footprint is the stable finite-score completion state.
  CLEAR_FIRST = 0x05,
  CLEAR_LAST = 0x2F,

  TAKEOVER_DELAY_SEC = 12.0,
  INPUT_INITIAL_REPEAT = 15,
  INPUT_REPEAT_RATE = 4,
  UI_ROWS = 7,
  UI_WIDTH = 27,
  WAV_POSTROLL_SEC = 0.15,
  BATCH_MIN_AUDITION_SEC = 2.0,

  -- Native completion/loop detection. Finite QUIET scores complete when
  -- emusic's work-array reset footprint is present. Three ROM streams settle
  -- at documented stationary continuous endpoints. A repeated non-consecutive
  -- MUSPC marks a score control-flow loop. Play-all gives every nonterminating
  -- event a minimum audible interval before stopping it through emusic.
  LOOP_MIN_TRANSITIONS = 2,

  ATTR_BLUE = 0x0808,
  ATTR_YELLOW = 0x0408,
  ATTR_RED = 0x0C08,
}

local IDLE_LOOP_BYTES = {
  0xFB,                         -- $D400 EI
  0x76,                         -- $D401 HALT; resumes after each interrupt
  0xCD, 0x7E, 0x0F,             -- $D402 CALL $0F7E (busaround)
  0xC3, 0x01, 0xD4,             -- $D405 JP $D401
}

-- Complete Program-2 sound-event catalog.  "terse" routes dispatch the original
-- compiled colon word through DSPATCH. "routine" routes call a self-contained
-- native game launcher. "score" is reserved for gameplay paths whose containing
-- routine requires live mission state; it reproduces that call site's exact
-- processor and bmusic/pmusic submission without entering unsafe mission logic.
-- Components document every distinct ROM score used by an event and drive the
-- two-processor completion monitor.
local ROM_CATALOG = {
  { id="CNSD", name="COIN INSERT", chip="secondary",
    route={kind="terse", word=0x136D}, components={{chip="secondary",score=0x1354,trigger="b2music"}},
    source="GORFOS 0109 CNSD" },
  { id="GOYAK_FX", name="ATTRACT JOYSTICK FX", chip="primary",
    route={kind="score", score=0x136A, chip="primary", trigger="pmusic"},
    components={{chip="primary",score=0x136A,trigger="pmusic"}}, source="goyak synchronized attract effect" },
  { id="PLAYER_SHOT", name="PLAYER SHOT", chip="primary",
    route={kind="routine", routine=0x2AEF}, components={{chip="primary",score=0x2669,trigger="bmusic"}},
    source="player fire native launcher" },
  { id="PLAYER_EXPLOSION", name="PLAYER SHIP EXPLOSION", chip="both",
    route={kind="terse", word=0x26F8},
    components={{chip="primary",score=0x268C,trigger="pmusic"},{chip="secondary",score=0x268C,trigger="p2music"}},
    source="GORFOS 0186 1G" },
  { id="PZIP", name="PZIP", chip="primary",
    route={kind="terse", word=0x270C}, components={{chip="primary",score=0x26B3,trigger="bmusic"}},
    source="GORFOS 0187 PZ" },
  { id="ZPIP", name="ZPIP", chip="primary",
    route={kind="terse", word=0x2715}, components={{chip="primary",score=0x26CF,trigger="bmusic"}},
    source="GORFOS 0187 ZP" },
  { id="TAKEOFF", name="TAKEOFF", chip="both",
    route={kind="terse", word=0x2792},
    components={{chip="secondary",score=0x2758,trigger="p2music"},{chip="primary",score=0x271E,trigger="pmusic"}},
    source="GORFOS 0188 TO" },
  { id="DIVE", name="DIVE", chip="secondary",
    route={kind="routine", routine=0x27D1}, components={{chip="secondary",score=0x27A1,trigger="bmusic"}},
    source="GORFOS 0189 PLAYKBS" },
  { id="INVADER_THUMP", name="INVADER THUMP", chip="secondary",
    route={kind="terse", word=0x812E},
    components={{chip="secondary",score=0x8115,trigger="p2music",
      endpoint={kind="continuous",pc=0x812E}}},
    source="INVADERS 0113 TH" },
  { id="LARGE_INVADER", name="LARGE INVADER", chip="secondary",
    route={kind="terse", word=0x8154},
    components={{chip="secondary",score=0x8139,trigger="p2music",
      endpoint={kind="continuous",pc=0x8153}}},
    source="INVADERS 0114 IA" },
  { id="LASER_SHOT", name="LASER SHOT", chip="secondary",
    route={kind="score", score=0x8BA0, chip="secondary", trigger="pmusic"},
    components={{chip="secondary",score=0x8BA0,trigger="pmusic"}}, source="Attack Fighter FCHECK sound branch" },
  { id="GALAXIAN_ATTACK", name="GALAXIAN ATTACK", chip="secondary",
    route={kind="routine", routine=0x97BE},
    components={{chip="secondary",score=0x9786,trigger="bmusic",
      endpoint={kind="continuous",pc=0x97BE}}},
    source="Galaxians GA native launcher" },
  { id="SHIP_SPIRAL", name="SHIP SPIRAL", chip="secondary",
    route={kind="terse", word=0x9F5A}, components={{chip="secondary",score=0x9F45,trigger="b2music"}},
    source="SPACE WARP 0110 SP" },
  { id="FIREBLAST", name="FIREBLAST", chip="secondary",
    route={kind="terse", word=0x9F7C}, components={{chip="secondary",score=0x9F65,trigger="b2music"}},
    source="SPACE WARP 0110 FBL" },
  { id="STAR_SPIRAL", name="STAR SPIRAL", chip="both",
    route={kind="terse", word=0x9FF1},
    components={{chip="primary",score=0x9F87,trigger="pmusic"},{chip="secondary",score=0x9FBC,trigger="p2music"}},
    source="SPACE WARP 0111 ST" },
  { id="BACKGROUND_SHIP", name="BACKGROUND SHIP", chip="secondary",
    route={kind="terse", word=0xAA1F}, components={{chip="secondary",score=0xA9D7,trigger="b2music"}},
    source="FLAG SHIP 0111 BSF" },
  { id="SHIP_EXPLOSION", name="SHIP EXPLOSION", chip="both",
    route={kind="terse", word=0xAA6C},
    components={{chip="secondary",score=0xAA61,trigger="p2music"},{chip="primary",score=0xAA28,trigger="pmusic"}},
    source="FLAG SHIP 0112 SE" },
  { id="FIREBALL", name="FIREBALL", chip="secondary",
    route={kind="terse", word=0xAA9F}, components={{chip="secondary",score=0xAA7B,trigger="b2music"}},
    source="FLAG SHIP 0113 FBS" },
  { id="SHIP_SHOTOFF", name="SHIP SHOTOFF", chip="primary",
    route={kind="terse", word=0xAAD3}, components={{chip="primary",score=0xAAAA,trigger="bmusic"}},
    source="FLAG SHIP 0113 SO" },
  { id="BLACK_HOLE", name="BLACK HOLE EMERGENCE", chip="both",
    route={kind="terse", word=0xAB80},
    components={{chip="primary",score=0xAADE,trigger="pmusic"},{chip="secondary",score=0xAB2F,trigger="p2music"}},
    source="FLAG SHIP 0114 BH" },
}


local machine = manager.machine
local cpu = machine.devices[C.CPU_TAG]
if not cpu then error("[GORF SOUND] main CPU not found at " .. C.CPU_TAG) end
local program = cpu.spaces and cpu.spaces["program"] or nil
local io_space = cpu.spaces and cpu.spaces["io"] or nil
if not program then error("[GORF SOUND] main CPU program space is unavailable") end
if not io_space then error("[GORF SOUND] main CPU I/O space is unavailable") end

local S = {
  enabled = true,
  takeover = false,
  takeover_attempted = false,
  frame_subscription = nil,
  stop_subscription = nil,
  shortcuts = {},

  catalog = {},
  source_label = "GORF PROGRAM 2 ROM",
  selection = 1,
  window_first = 1,

  status = "WAITING FOR GORF INITIALIZATION",
  last_controls = 0,
  last_2p_start = false,
  hold_dir = 0,
  hold_frames = 0,

  playback = nil,
  batch = nil,

  wav_enabled = false,
  wav_active = false,
  wav_filename = nil,
  wav_stop_at = nil,
  trace_enabled = true,

  ui_dirty = false,
  draw_count = 0,
  drawchar = nil,
  bmusic = C.BMUSIC,
  pmusic = C.PMUSIC,

}

local function printf(fmt, ...)
  print(string.format(fmt, ...))
end

local function hex2(v) return string.format("$%02X", v & 0xFF) end
local function hex4(v) return string.format("$%04X", v & 0xFFFF) end

local function machine_seconds()
  local ok, value = pcall(function() return machine.time:as_double() end)
  if ok then return value end
  return 0
end

local function read16(addr)
  local lo = program:read_u8(addr)
  local hi = program:read_u8((addr + 1) & 0xFFFF)
  return lo | (hi << 8)
end

local function chip_music_array(chip_name)
  return chip_name == "secondary" and C.SECONDARY_MUSIC or C.PRIMARY_MUSIC
end

local function chip_marker(chip_name)
  return chip_name == "secondary" and "S" or "P"
end

local function region_fits(start_address, byte_count, limit_exclusive)
  return byte_count >= 0 and start_address + byte_count <= limit_exclusive
end

-- ---------------------------------------------------------------------------
-- Native Gorf music-processor state
-- ---------------------------------------------------------------------------

local function native_processor_state(chip_name)
  local array = chip_name == "secondary" and C.SECONDARY_MUSIC or C.PRIMARY_MUSIC
  local cleared = true
  local first_nonzero_offset = nil
  local first_nonzero_value = nil
  for offset = C.CLEAR_FIRST, C.CLEAR_LAST do
    local value = program:read_u8(array + offset)
    if value ~= 0 then
      cleared = false
      first_nonzero_offset = offset
      first_nonzero_value = value
      break
    end
  end
  return {
    array = array,
    muspc = read16(array + 0x00),
    startpc = read16(array + 0x02),
    soundbox = program:read_u8(array + 0x04),
    multiple = program:read_u8(array + 0x07),
    priority = program:read_u8(array + 0x08),
    notetimer = program:read_u8(array + 0x2E),
    mst = program:read_u8(array + 0x2F),
    cleared = cleared,
    first_nonzero_offset = first_nonzero_offset,
    first_nonzero_value = first_nonzero_value,
  }
end

local SCORE_OPCODE_NAMES = {
  [0x00]="RANDOMNOTES", [0x01]="DURATION", [0x02]="CONTJUMP", [0x03]="YIELD",
  [0x04]="QUIET", [0x05]="RAMBLE", [0x06]="RAMP", [0x07]="MUSICIN",
  [0x08]="RAMBLEON", [0x09]="RAMBLEOFF", [0x0A]="COUNTLIMITS", [0x0B]="MOVESTEP",
  [0x0C]="MOVELOW", [0x0D]="MOVEHIGH", [0x0E]="MOVETB", [0x0F]="MOVENOISE",
  [0x10]="MASTER", [0x11]="TONEA", [0x12]="TONEB", [0x13]="TONEC",
  [0x14]="VIBS", [0x15]="MCVOLS", [0x16]="ABVOLS", [0x17]="NOISE",
  [0x18]="MOVESOUND", [0x19]="PANLIMIT", [0x1A]="MOVEVOLS", [0x1B]="THUMPER",
}

local function route_text(entry)
  local r = entry.route
  if r.kind == "terse" then return "TERSE " .. hex4(r.word) end
  if r.kind == "routine" then return "NATIVE ROUTINE " .. hex4(r.routine) end
  return string.format("NATIVE SCORE %s %s %s", hex4(r.score), r.chip:upper(), r.trigger:upper())
end

local function component_text(component)
  local text = string.format("%s:%s/%s", component.chip:upper(), hex4(component.score), component.trigger:upper())
  if component.endpoint then
    text = text .. string.format(" endpoint=%s@%s", component.endpoint.kind:upper(), hex4(component.endpoint.pc))
  end
  return text
end

local function log_processor_state(prefix, chip_name)
  local s = native_processor_state(chip_name)
  printf("[GORF SOUND] %s %-9s MUSPC=%s STARTPC=%s PRIORITY=%02X MULTIPLE=%02X MST=%02X TIMER=%02X SOUNDBOX=%02X CLEARED=%s",
    prefix, chip_name:upper(), hex4(s.muspc), hex4(s.startpc), s.priority, s.multiple,
    s.mst, s.notetimer, s.soundbox, s.cleared and "YES" or "NO")
end

local function pre_reset_state_text(primary, secondary)
  return string.format("P MUSPC=%s PRIORITY=%02X MST=%02X | S MUSPC=%s PRIORITY=%02X MST=%02X",
    hex4(primary.muspc), primary.priority, primary.mst,
    hex4(secondary.muspc), secondary.priority, secondary.mst)
end

local function log_selection(_reason)
  local e = S.catalog[S.selection]
  if not e then return end
  local chip = e.chip == "both" and "B" or chip_marker(e.chip)
  printf("[GORF SOUND] SELECT %02d %s %s", S.selection, chip, e.name)
end

local function set_catalog(entries, label)
  S.catalog = entries
  S.source_label = label or "GORF PROGRAM 2 ROM"
  S.source_path = nil
  S.selection = 1
  S.window_first = 1
  S.ui_dirty = true
end

local function install_rom_catalog()
  local entries = {}
  for _, src in ipairs(ROM_CATALOG) do
    local e = {}
    for k, v in pairs(src) do e[k] = v end
    entries[#entries + 1] = e
  end
  set_catalog(entries, "GORF PROGRAM 2 ROM")
end

-- ---------------------------------------------------------------------------
-- Gorf validation and native browser display
-- ---------------------------------------------------------------------------

local function find_drawchar()
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

local function rom_bytes(address, count)
  local out = {}
  for i = 0, count - 1 do
    out[#out + 1] = string.format("%02X", program:read_u8(address + i))
  end
  return table.concat(out, " ")
end

local function validate_catalog()
  if #ROM_CATALOG ~= 20 then return false, "catalog event count is not 20" end
  local ids, scores, endpoint_count = {}, {}, 0
  local route_counts = {terse=0, routine=0, score=0}
  local composite_count = 0
  local expected_endpoints = {
    INVADER_THUMP={kind="continuous", pc=0x812E},
    LARGE_INVADER={kind="continuous", pc=0x8153},
    GALAXIAN_ATTACK={kind="continuous", pc=0x97BE},
  }
  local seen_endpoints = {}
  for index, entry in ipairs(ROM_CATALOG) do
    if ids[entry.id] then return false, "duplicate catalog id " .. entry.id end
    ids[entry.id] = true
    if not entry.route or not entry.components or #entry.components == 0 then
      return false, string.format("catalog item %d is incomplete", index)
    end
    local r = entry.route
    route_counts[r.kind] = (route_counts[r.kind] or 0) + 1
    if entry.chip == "both" then composite_count = composite_count + 1 end
    if r.kind == "terse" then
      if program:read_u8(r.word) ~= 0xCF then
        return false, string.format("TERSE word signature differs at %s", hex4(r.word))
      end
    elseif r.kind == "routine" then
      if program:read_u8(r.routine) ~= 0x21 then
        return false, string.format("native launcher signature differs at %s", hex4(r.routine))
      end
    elseif r.kind ~= "score" then
      return false, "unknown route kind " .. tostring(r.kind)
    end
    local component_chips = {}
    for _, component in ipairs(entry.components) do
      if component.chip ~= "primary" and component.chip ~= "secondary" then
        return false, string.format("invalid processor in catalog item %d", index)
      end
      if component_chips[component.chip] then
        return false, string.format("duplicate %s component in catalog item %d", component.chip, index)
      end
      component_chips[component.chip] = true
      local opcode = program:read_u8(component.score)
      if opcode > 0x1B then
        return false, string.format("score signature differs at %s", hex4(component.score))
      end
      scores[component.score] = true
      if component.endpoint then
        local endpoint = component.endpoint
        if endpoint.kind ~= "continuous" then
          return false, string.format("invalid endpoint policy in catalog item %d", index)
        end
        if type(endpoint.pc) ~= "number" or endpoint.pc < 0 or endpoint.pc > 0xFFFF then
          return false, string.format("invalid endpoint PC in catalog item %d", index)
        end
        local expected = expected_endpoints[entry.id]
        if not expected or endpoint.kind ~= expected.kind or endpoint.pc ~= expected.pc then
          return false, string.format("stationary endpoint differs for %s", entry.id)
        end
        seen_endpoints[entry.id] = true
        endpoint_count = endpoint_count + 1
      end
    end
  end
  local score_count = 0
  for _ in pairs(scores) do score_count = score_count + 1 end
  if score_count ~= 24 then return false, string.format("catalog has %d distinct scores, expected 24", score_count) end
  if endpoint_count ~= 3 then
    return false, string.format("catalog has %d stationary endpoints, expected 3", endpoint_count)
  end
  for id in pairs(expected_endpoints) do
    if not seen_endpoints[id] then return false, "missing stationary endpoint for " .. id end
  end
  if route_counts.terse ~= 15 or route_counts.routine ~= 3 or route_counts.score ~= 2 then
    return false, string.format("route counts are %d TERSE / %d native / %d exact",
      route_counts.terse, route_counts.routine, route_counts.score)
  end
  if composite_count ~= 5 then
    return false, string.format("catalog has %d composite events, expected 5", composite_count)
  end
  return true, string.format("20 events / %d distinct scores / %d stationary endpoints", score_count, endpoint_count)
end

local function validate_work_ram_layout()
  if not region_fits(C.IDLE_LOOP, #IDLE_LOOP_BYTES, C.DRAW_CODE) then
    return false, "idle loop overlaps native UI code"
  end
  if C.DRAW_CODE_LIMIT ~= C.DRAW_DATA or C.DRAW_DATA_LIMIT ~= C.TERSE_THREAD then
    return false, "generated UI limits do not match the reserved RAM layout"
  end
  if C.TERSE_THREAD + 4 > C.TERSE_EXIT then
    return false, "TERSE thread overlaps its return target"
  end
  if C.TERSE_EXIT + 4 > C.TERSE_RETURN_STACK then
    return false, "TERSE return target overlaps its return stack"
  end
  if C.TERSE_RETURN_STACK + 0x10 > C.PLAY_CODE then
    return false, "TERSE return stack overlaps native launcher code"
  end
  if C.PLAY_CODE_LIMIT ~= C.CALL_STACK - 0x40 then
    return false, "native launcher call-stack guard is not 64 bytes"
  end
  if C.CLEAR_FIRST ~= 0x05 or C.CLEAR_LAST ~= 0x2F then
    return false, "native cleared-state footprint differs from emusic"
  end
  return true
end

local function validate_program()
  local ram_ok, ram_why = validate_work_ram_layout()
  if not ram_ok then return false, ram_why end

  -- English Program-2 SPK_INSERT signature. This first build is intentionally
  -- scoped to the resident English Gorf requested for testing.
  local insert_sig = {
    0x0F,0x3E,0x27,0x0D,0x1F,0x3A,0x2A,0x3E,
    0x19,0x35,0x23,0x09,0x21,0x0D,0x0D,0x3E
  }
  for i = 1, #insert_sig do
    if program:read_u8(0x115D + i - 1) ~= insert_sig[i] then
      return false, string.format("English Program-2 signature differs at %s", hex4(0x115D + i - 1))
    end
  end

  S.drawchar = find_drawchar()
  if not S.drawchar then return false, "Gorf drawchar signature not found" end

  -- Do not byte-scan pmusic. The first build did that and rejected the
  -- stock English ROM despite the release engine using the fixed Program-2
  -- low-level entries below. English SPK_INSERT plus drawchar remain the
  -- compatibility gate; the music addresses are architecture constants.
  if program:read_u8(C.ENDMUS) ~= 0x03 then
    return false, string.format("ENDMUS byte differs at %s", hex4(C.ENDMUS))
  end
  local write_sig = {0x78,0xD3,0x19,0x79,0xD3,0x0C}
  for i = 1, #write_sig do
    if program:read_u8(C.WRITE + i - 1) ~= write_sig[i] then
      return false, string.format("native pattern writer differs at %s", hex4(C.WRITE + i - 1))
    end
  end
  if program:read_u8(C.RELABS) ~= 0xC3 then
    return false, string.format("RELABS vector is not initialized at %s", hex4(C.RELABS))
  end
  local score1354 = {0x14,0x86,0x10,0x10,0x06,0x10}
  for i = 1, #score1354 do
    if program:read_u8(0x1354 + i - 1) ~= score1354[i] then
      return false, string.format("score $1354 signature differs at %s", hex4(0x1354 + i - 1))
    end
  end
  local amfx = {0x02,0x56,0x13}
  for i = 1, #amfx do
    if program:read_u8(0x136A + i - 1) ~= amfx[i] then
      return false, string.format("AM_FX signature differs at %s", hex4(0x136A + i - 1))
    end
  end

  S.bmusic = C.BMUSIC
  S.pmusic = C.PMUSIC

  local catalog_ok, catalog_why = validate_catalog()
  if not catalog_ok then return false, catalog_why end

  return true, string.format("drawchar=%s busaround=%s bmusic=%s pmusic=%s; RAM layout verified; %s",
    hex4(S.drawchar), hex4(C.BUSAROUND), hex4(S.bmusic), hex4(S.pmusic), catalog_why)
end

local function install_idle_loop()
  for i = 1, #IDLE_LOOP_BYTES do program:write_u8(C.IDLE_LOOP + i - 1, IDLE_LOOP_BYTES[i]) end
  if cpu.state["HALT"] then cpu.state["HALT"].value = 0 end
  if cpu.state["IFF1"] then cpu.state["IFF1"].value = 1 end
  if cpu.state["IFF2"] then cpu.state["IFF2"].value = 1 end
  if cpu.state["SP"] then cpu.state["SP"].value = C.CALL_STACK end
  cpu.state["PC"].value = C.IDLE_LOOP
end

local function clear_video_ram()
  for addr = 0x4000, 0x7FFF do program:write_u8(addr, 0) end
end

local function foreground_idle()
  if not S.takeover or not cpu.state["PC"] then return false end
  local pc = cpu.state["PC"].value & 0xFFFF
  return pc >= C.IDLE_LOOP and pc <= (C.IDLE_LOOP + #IDLE_LOOP_BYTES - 1)
end

local function transliterate_for_gorf(text)
  local s = tostring(text or "")
  s = s:upper()
  local out = {}
  for i = 1, #s do
    local b = s:byte(i)
    if (b >= 0x30 and b <= 0x39) or (b >= 0x41 and b <= 0x5A)
        or b == 0x20 or b == 0x2D or b == 0x5E then
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

local function screen_line_x(row)
  return ((76 - row * 6) & 0xFF) << 8
end

local function centered_text_y(text)
  return 0x6000 - (#text * 0x0380)
end

local function text_y_at_column(column)
  return centered_text_y(string.rep(" ", C.UI_WIDTH)) + (column * 0x0700)
end

local function selected_entry()
  return S.selection, S.catalog[S.selection], S.catalog
end

local function keep_selection_visible(index)
  local list = S.catalog
  if #list == 0 then
    S.selection = 0
    S.window_first = 1
    return
  end
  if index < 1 then index = 1 end
  if index > #list then index = #list end
  S.selection = index
  local first = S.window_first or 1
  if index < first then first = index
  elseif index > first + C.UI_ROWS - 1 then first = index - C.UI_ROWS + 1 end
  local max_first = math.max(1, #list - C.UI_ROWS + 1)
  if first < 1 then first = 1 end
  if first > max_first then first = max_first end
  S.window_first = first
end

local function status_line()
  local wav = S.wav_enabled and " WAV" or ""
  if S.batch then
    local index = S.batch.current_index or math.min(S.batch.next_index or 1, S.batch.total)
    local entry = S.catalog[index]
    if S.playback and entry then return string.format("PLAY ALL %02d %s%s", index, entry.name, wav) end
    return string.format("PLAY ALL READY %02d%s", index, wav)
  end
  if S.playback then
    if S.status and S.status:match("^PLAY ") then return S.status .. wav end
    local state = (S.status == "LOOPING" or S.status == "CONTINUOUS") and S.status or "PLAYING"
    return string.format("%s %02d %s%s", state, S.playback.index, S.playback.entry.name, wav)
  end
  if S.status and S.status ~= "READY" then return S.status .. wav end
  return "READY" .. wav
end

local function native_menu_lines()
  local list = S.catalog
  local selected = S.selection or 0
  local first = S.window_first or 1
  local max_first = math.max(1, #list - C.UI_ROWS + 1)
  if first < 1 then first = 1 end
  if first > max_first then first = max_first end
  S.window_first = first

  local lines = {}
  local vmaj, vmin, vpatch = VERSION:match("^(%d+)%.(%d+)%.(%d+)")
  local short_version = vmaj and ("V" .. vmaj .. vmin .. vpatch) or "VER"
  lines[#lines + 1] = {
    row=0,
    column=0,
    text="GORF SOUND BROWSER",
    attr=C.ATTR_BLUE
  }
  lines[#lines + 1] = {
    row=0,
    column=C.UI_WIDTH - #short_version,
    text=short_version,
    attr=C.ATTR_YELLOW
  }

  if #list == 0 then
    lines[#lines + 1] = { row=2, text=native_center("NO SOUNDS"), attr=C.ATTR_RED }
    for row = 1, C.UI_ROWS - 1 do
      lines[#lines + 1] = { row=2 + row, text=string.rep(" ", C.UI_WIDTH), attr=C.ATTR_RED }
    end
  else
    for row = 0, C.UI_ROWS - 1 do
      local idx = first + row
      local e = list[idx]
      local line = string.rep(" ", C.UI_WIDTH)
      if e then
        local chip = e.chip == "secondary" and "S" or (e.chip == "primary" and "P" or "B")
        line = string.format(" %02d %s %s", idx, chip, fixed_native_text(e.name, C.UI_WIDTH - 6))
      end
      lines[#lines + 1] = {
        row=2 + row,
        text=fixed_native_text(line, C.UI_WIDTH),
        attr=(idx == selected) and C.ATTR_YELLOW or C.ATTR_RED
      }
    end
  end

  lines[#lines + 1] = { row=9, text=native_center(status_line()), attr=C.ATTR_BLUE }
  lines[#lines + 1] = { row=11, text=native_center("UP DOWN SELECT - FIRE PLAY"), attr=C.ATTR_YELLOW }
  lines[#lines + 1] = {
    row=12,
    text=native_center(S.batch and "1P EXIT - 2P STOP" or "1P EXIT - 2P PLAY ALL"),
    attr=C.ATTR_YELLOW
  }
  return lines
end

local function write_native_draw_program(lines)
  if not S.drawchar then return false, "native drawchar unavailable" end

  local data = C.DRAW_DATA
  local strings = {}
  for _, line in ipairs(lines) do
    if #line.text > C.UI_WIDTH then return false, "native UI line too long" end
    strings[#strings + 1] = data
    data = data + #line.text + 1
  end

  -- Gorf's resident character table has no hyphen. This six-column 1bpp
  -- pattern supplies a centered dash through the native Pattern Board writer.
  local dash_glyph = data
  local dash_bytes = {0x00,0x00, 0x01,0x80, 0x01,0x80, 0x01,0x80, 0x01,0x80, 0x00,0x00}
  data = data + #dash_bytes
  if data > C.DRAW_DATA_LIMIT then return false, "native UI strings exceed reserved draw RAM" end

  local code = {}
  local function emit(v) code[#code + 1] = v & 0xFF end
  local function emit16(v) emit(v); emit(v >> 8) end
  local labels, relative_patches, absolute_patches = {}, {}, {}
  local function mark(name) labels[name] = #code end
  local function emit_jr(opcode, target)
    emit(opcode)
    local operand = #code + 1
    emit(0)
    relative_patches[#relative_patches + 1] = {operand=operand, target=target}
  end
  local function emit_call_address(address)
    emit(0xCD); emit16(address)
  end
  local function emit_call_label(target)
    emit(0xCD)
    local operand = #code + 1
    emit(0); emit(0)
    absolute_patches[#absolute_patches + 1] = {operand=operand, target=target}
  end

  emit(0xF3); emit(0xDD); emit(0xE5); emit(0xFD); emit(0xE5) -- DI/PUSH IX/PUSH IY

  for i, line in ipairs(lines) do
    emit(0x01); emit16(line.attr)
    emit(0x11); emit16(screen_line_x(line.row))
    emit(0x21); emit16(line.column and text_y_at_column(line.column) or centered_text_y(line.text))
    emit(0xDD); emit(0x21); emit16(strings[i])
    emit_call_label("draw_string")
  end

  emit(0xFD); emit(0xE1); emit(0xDD); emit(0xE1); emit(0xFB)
  emit(0xC3); emit16(C.IDLE_LOOP + 1)

  mark("draw_string")
  mark("draw_string_loop")
  emit(0xDD); emit(0x7E); emit(0x00)
  emit(0xB7); emit(0xC8)
  emit(0xDD); emit(0x23)

  emit(0xFE); emit(0x2D)                       -- CP '-' (injected dash glyph)
  emit_jr(0x28, "draw_dash")                  -- JR Z,draw_dash

  mark("draw_standard")
  emit(0xDD); emit(0xE5)
  emit_call_address(S.drawchar)
  emit(0xDD); emit(0xE1)
  emit_jr(0x18, "draw_string_loop")

  mark("draw_dash")
  emit(0xDD); emit(0xE5)
  emit_call_label("draw_dash_glyph")
  emit(0xDD); emit(0xE1)
  emit_jr(0x18, "draw_string_loop")

  -- draw_dash_glyph mirrors the tail of Gorf's drawchar routine, substituting
  -- the injected six-column bitmap while retaining RELABS and write.
  mark("draw_dash_glyph")
  emit(0xC5); emit(0xE5); emit(0xD5)           -- PUSH BC/HL/DE
  emit(0xFD); emit(0x21); emit16(dash_glyph)   -- LD IY,dash_glyph
  emit(0xD1); emit(0xE1); emit(0xE5); emit(0xD5)
  emit_call_address(C.RELABS)
  emit(0x11); emit16(0x0602)                   -- LD DE,$0602
  emit_call_address(C.WRITE)
  emit(0xD1); emit(0xE1)
  emit(0x7C); emit(0xC6); emit(0x07); emit(0x67)
  emit(0xC1); emit(0xC9)                       -- POP BC / RET

  for _, patch in ipairs(relative_patches) do
    local target = labels[patch.target]
    if target == nil then return false, "unresolved native UI branch " .. patch.target end
    local displacement = target - patch.operand
    if displacement < -128 or displacement > 127 then
      return false, "native UI branch exceeds JR range"
    end
    code[patch.operand] = displacement & 0xFF
  end
  for _, patch in ipairs(absolute_patches) do
    local target = labels[patch.target]
    if target == nil then return false, "unresolved native UI call " .. patch.target end
    local address = C.DRAW_CODE + target
    code[patch.operand] = address & 0xFF
    code[patch.operand + 1] = (address >> 8) & 0xFF
  end

  if not region_fits(C.DRAW_CODE, #code, C.DRAW_CODE_LIMIT) then
    return false, "native UI code exceeds reserved RAM"
  end

  local data_cursor = C.DRAW_DATA
  for _, line in ipairs(lines) do
    for i = 1, #line.text do
      program:write_u8(data_cursor, line.text:byte(i))
      data_cursor = data_cursor + 1
    end
    program:write_u8(data_cursor, 0)
    data_cursor = data_cursor + 1
  end
  for _, byte in ipairs(dash_bytes) do
    program:write_u8(data_cursor, byte)
    data_cursor = data_cursor + 1
  end
  for i, b in ipairs(code) do program:write_u8(C.DRAW_CODE + i - 1, b) end
  return true
end

local function render_ui_native()
  if not S.takeover or not S.ui_dirty or not foreground_idle() then return end
  local ok, err = write_native_draw_program(native_menu_lines())
  if not ok then
    S.status = "ERROR: " .. err
    printf("[GORF SOUND] %s", err)
    S.ui_dirty = false
    return
  end
  if cpu.state["SP"] then cpu.state["SP"].value = C.CALL_STACK end
  if cpu.state["HALT"] then cpu.state["HALT"].value = 0 end
  cpu.state["PC"].value = C.DRAW_CODE
  S.ui_dirty = false
  S.draw_count = S.draw_count + 1
end

-- ---------------------------------------------------------------------------
-- Sound playback and WAV capture
-- ---------------------------------------------------------------------------

local function safe_slug(text)
  local s = tostring(text or "sound"):lower()
  s = s:gsub("[^a-z0-9]+", "_"):gsub("^_+", ""):gsub("_+$", "")
  if s == "" then s = "sound" end
  return s
end

local function wav_filename(entry)
  return string.format("gorf_sound_%s.wav", safe_slug(entry.id or entry.name))
end

local function stop_owned_wav(reason)
  if not S.wav_active then return end
  pcall(function() machine.sound:stop_recording() end)
  printf("[GORF SOUND] WAV %s: %s", reason or "saved", tostring(S.wav_filename or ""))
  S.wav_active = false
  S.wav_filename = nil
  S.wav_stop_at = nil
end

local function start_entry_wav(entry)
  if S.wav_active then return false, "browser WAV recorder is still active" end
  local already = false
  pcall(function() already = machine.sound.recording == true end)
  if already then return false, "MAME sound recorder is already active" end
  local name = wav_filename(entry)
  local ok, started = pcall(function() return machine.sound:start_recording(name) end)
  if not ok or not started then return false, "MAME could not start WAV " .. name end
  S.wav_active = true
  S.wav_filename = name
  S.wav_stop_at = nil
  printf("[GORF SOUND] WAV recording: %s", name)
  return true
end

local function service_wav_capture()
  if S.wav_active and S.wav_stop_at and machine_seconds() >= S.wav_stop_at then
    stop_owned_wav("saved")
    S.ui_dirty = true
  end
end

local run_native_stop

local function stop_sound(reason)
  if S.playback and S.playback.mode == "native" then
    local ok, err = run_native_stop()
    if not ok then
      printf("[GORF SOUND] native stop failed: %s", tostring(err))
      S.status = "STOP ERROR"
      S.ui_dirty = true
      return false
    end
  end
  if S.playback then
    printf("[GORF SOUND] STOP %02d %s%s", S.playback.index, tostring(S.playback.entry.name),
      reason and (" (" .. reason .. ")") or "")
  end
  S.playback = nil
  if S.wav_active and not S.wav_stop_at then S.wav_stop_at = machine_seconds() + C.WAV_POSTROLL_SEC end
  S.status = "READY"
  S.ui_dirty = true
  return true
end

local function emit_native_emusic(code, array, soundbox)
  local function emit(v) code[#code + 1] = v & 0xFF end
  local function emit16(v) emit(v); emit(v >> 8) end

  -- Exact setup used by Gorf's _EMUSIC/_E2MUSIC wrappers.  emusic itself
  -- performs the matching EXX before returning.
  emit(0xD9)                                  -- EXX
  emit(0x11); emit16(array)                   -- LD DE,music array
  emit(0x21); emit16(0x002F)                  -- LD HL,$002F (MST)
  emit(0x19)                                  -- ADD HL,DE
  emit(0x36); emit(0x01)                      -- LD (HL),1
  emit(0x21); emit16(0x0004)                  -- LD HL,$0004 (SOUNDBOX)
  emit(0x19)                                  -- ADD HL,DE
  emit(0x36); emit(soundbox)                  -- primary $18 / secondary $58
  emit(0xCD); emit16(C.EMUSIC)                -- CALL emusic
end

local function write_native_play_launcher(entry)
  local route = entry.route
  local code = {}
  local terse_word = nil
  local function emit(v) code[#code + 1] = v & 0xFF end
  local function emit16(v) emit(v); emit(v >> 8) end

  emit(0xF3)                                  -- DI

  -- Stop both native processors through Gorf's emusic routine before submitting
  -- the selected event through its documented game path.
  emit_native_emusic(code, C.PRIMARY_MUSIC, 0x18)
  emit_native_emusic(code, C.SECONDARY_MUSIC, 0x58)

  emit(0x3E); emit(0x01)                     -- LD A,1
  emit(0x32); emit16(C.MUSICFLAG)             -- LD (MUSICFLAG),A

  if route.kind == "terse" then
    -- A tiny TERSE thread dispatches the original colon word.  Its RETURN lands
    -- on a RAM CODE word that re-enables interrupts and rejoins the HALT loop.
    terse_word = route.word
    emit(0xDD); emit(0x21); emit16(C.TERSE_RETURN_STACK) -- LD IX,TERSE RSP
    emit(0x31); emit16(C.CALL_STACK)                    -- LD SP,TERSE PSP
    emit(0xFD); emit(0x21); emit16(C.DSPATCH)           -- LD IY,DSPATCH
    emit(0x01); emit16(C.TERSE_THREAD)                  -- LD BC,event thread
    emit(0xC3); emit16(C.DSPATCH)                       -- JP DSPATCH
  elseif route.kind == "routine" then
    emit(0xCD); emit16(route.routine)          -- CALL exact native game launcher
    emit(0xFB)
    emit(0xC3); emit16(C.IDLE_LOOP + 1)
  else
    local array = chip_music_array(route.chip)
    local trigger = route.trigger == "bmusic" and C.BMUSIC or C.PMUSIC
    emit(0x21); emit16(route.score)            -- LD HL,score
    emit(0xFD); emit(0x21); emit16(array)      -- LD IY,game music processor
    emit(0xCD); emit16(trigger)                -- CALL exact game engine entry
    emit(0xFB)
    emit(0xC3); emit16(C.IDLE_LOOP + 1)
  end

  if not region_fits(C.PLAY_CODE, #code, C.PLAY_CODE_LIMIT) then
    return false, "native launcher exceeds reserved RAM"
  end
  if terse_word then
    program:write_u8(C.TERSE_THREAD + 0, terse_word & 0xFF)
    program:write_u8(C.TERSE_THREAD + 1, terse_word >> 8)
    program:write_u8(C.TERSE_THREAD + 2, C.TERSE_EXIT & 0xFF)
    program:write_u8(C.TERSE_THREAD + 3, C.TERSE_EXIT >> 8)
    program:write_u8(C.TERSE_EXIT + 0, 0xFB)  -- EI
    program:write_u8(C.TERSE_EXIT + 1, 0xC3)  -- JP idle HALT/service loop
    program:write_u8(C.TERSE_EXIT + 2, (C.IDLE_LOOP + 1) & 0xFF)
    program:write_u8(C.TERSE_EXIT + 3, (C.IDLE_LOOP + 1) >> 8)
  end
  for i, b in ipairs(code) do program:write_u8(C.PLAY_CODE + i - 1, b) end
  return true
end

local function write_native_stop_launcher()
  local code = {}
  local function emit(v) code[#code + 1] = v & 0xFF end
  local function emit16(v) emit(v); emit(v >> 8) end

  emit(0xF3)
  emit(0xDD); emit(0xE5)
  emit(0xFD); emit(0xE5)
  emit_native_emusic(code, C.PRIMARY_MUSIC, 0x18)
  emit_native_emusic(code, C.SECONDARY_MUSIC, 0x58)
  emit(0xAF)                                  -- XOR A
  emit(0x32); emit16(C.MUSICFLAG)             -- LD (MUSICFLAG),0
  emit(0xFD); emit(0xE1)
  emit(0xDD); emit(0xE1)
  emit(0xFB)
  emit(0xC3); emit16(C.IDLE_LOOP + 1)

  if not region_fits(C.PLAY_CODE, #code, C.PLAY_CODE_LIMIT) then
    return false, "native stop launcher exceeds reserved RAM"
  end
  for i, b in ipairs(code) do program:write_u8(C.PLAY_CODE + i - 1, b) end
  return true
end

run_native_stop = function()
  local ok, err = write_native_stop_launcher()
  if not ok then return false, err end
  if cpu.state["SP"] then cpu.state["SP"].value = C.CALL_STACK end
  if cpu.state["HALT"] then cpu.state["HALT"].value = 0 end
  cpu.state["PC"].value = C.PLAY_CODE
  return true
end

local function start_native(entry, index, source)
  local ok, err = write_native_play_launcher(entry)
  if not ok then return false, err end

  if cpu.state["SP"] then cpu.state["SP"].value = C.CALL_STACK end
  if cpu.state["HALT"] then cpu.state["HALT"].value = 0 end
  cpu.state["PC"].value = C.PLAY_CODE

  local tracks = {}
  for _, component in ipairs(entry.components) do
    if not tracks[component.chip] then
      tracks[component.chip] = {
        chip=component.chip, music_array=chip_music_array(component.chip), expected_score=component.score,
        armed=false, seen_running=false,
        last_pc=nil, pc_seen={}, transitions=0, ended=false, loop_detected=false, loop_pc=nil,
        endpoint=component.endpoint,
      }
    end
  end
  S.playback = {
    mode="native",
    entry=entry,
    index=index,
    source=source or "manual",
    start_time=machine_seconds(),
    tracks=tracks,
    steps=0,
    loop_detected=false,
    cleared_end=false,
    batch_stop_at=nil,
    batch_end_reason=nil,
    launch_complete=false,
  }
  return true
end

local function start_entry(entry, index, source)
  if not S.takeover then return false, "browser has not taken over yet" end
  if not entry then return false, "no selected sound" end
  if S.playback then stop_sound("restart") end
  if S.wav_active then return false, "wait for current WAV to finish" end

  if S.wav_enabled then
    local ok, err = start_entry_wav(entry)
    if not ok then return false, err end
  end

  local primary_before = native_processor_state("primary")
  local secondary_before = native_processor_state("secondary")
  local ok, err = start_native(entry, index, source)

  if not ok then
    if S.wav_active then stop_owned_wav("discarded") end
    return false, err
  end

  print("")
  printf("[GORF SOUND] PLAY %02d %s request=%s", index, entry.name, source or "manual")
  printf("[GORF SOUND]   ROUTE %s", route_text(entry))
  printf("[GORF SOUND]   SOURCE %s", tostring(entry.source))
  printf("[GORF SOUND]   PRE-RESET %s", pre_reset_state_text(primary_before, secondary_before))
  for i, component in ipairs(entry.components) do
    printf("[GORF SOUND]   SUBMIT %d %s", i, component_text(component))
  end
  S.status = string.format("PLAY %02d STEP %02d", index, 0)
  S.ui_dirty = true
  return true
end

local function finish_playback(reason)
  local p = S.playback
  if not p then return end
  local elapsed = machine_seconds() - p.start_time
  local steps = p.steps or 0
  local ok, err = run_native_stop()
  if not ok then
    printf("[GORF SOUND] native stop failed at end of %s: %s", tostring(p.entry.name), tostring(err))
    S.status = "STOP ERROR"
    S.ui_dirty = true
    return
  end
  printf("[GORF SOUND] END %02d %s steps=%d elapsed=%.3fs%s",
    p.index, tostring(p.entry.name), steps, elapsed, reason and (" " .. reason) or "")
  S.playback = nil
  if S.wav_active then S.wav_stop_at = machine_seconds() + C.WAV_POSTROLL_SEC end
  S.status = string.format("DONE %02d STEPS %02d", p.index, math.min(steps, 99))
  S.ui_dirty = true
end

local function schedule_batch_finish(p, reason)
  if p.source ~= "batch" then return false end
  if not p.batch_stop_at then
    p.batch_stop_at = p.start_time + C.BATCH_MIN_AUDITION_SEC
    p.batch_end_reason = reason
    local remaining = math.max(0, p.batch_stop_at - machine_seconds())
    printf("[GORF SOUND] BATCH AUDITION %02d %s minimum=%.3fs remaining=%.3fs",
      p.index, p.entry.name, C.BATCH_MIN_AUDITION_SEC, remaining)
  end
  if machine_seconds() >= p.batch_stop_at then
    finish_playback(p.batch_end_reason or reason)
    return true
  end
  return false
end

local function service_playback()
  local p = S.playback
  if not p then return end
  if not p.launch_complete then
    if not foreground_idle() then return end
    p.launch_complete = true
  end
  if p.batch_stop_at and machine_seconds() >= p.batch_stop_at then
    finish_playback(p.batch_end_reason or "NATIVE NONTERMINATING")
    return
  end

  local all_ended = true
  for _, chip_name in ipairs({"primary", "secondary"}) do
    local track = p.tracks[chip_name]
    if track then
      local state = native_processor_state(chip_name)
      local pc = state.muspc
      if not track.armed and state.startpc == track.expected_score then
        track.armed = true
        track.seen_running = true
        if S.trace_enabled then
          printf("[GORF SOUND] START %02d %s STARTPC=%s MUSPC=%s CLEARED=%s",
            p.index, chip_marker(chip_name), hex4(state.startpc), hex4(pc),
            state.cleared and "YES" or "NO")
        end
      end

      local transitioned = false
      if track.armed and pc ~= track.last_pc then
        transitioned = true
        track.transitions = track.transitions + 1
        p.steps = p.steps + 1
        local opcode = program:read_u8(pc)
        local endpoint_reached = track.endpoint and pc == track.endpoint.pc and state.mst == 0
        local opcode_text = string.format("%02X", opcode)
        local operation = state.cleared and "POST-QUIET" or (SCORE_OPCODE_NAMES[opcode] or "INVALID")
        if endpoint_reached then
          opcode_text = "--"
          operation = track.endpoint.kind:upper()
        end
        if S.trace_enabled then
          printf("[GORF SOUND] STEP %02d/%02d %s MUSPC=%s OPCODE=%-2s %-12s PRIORITY=%02X MULTIPLE=%02X MST=%02X TIMER=%02X CLEARED=%s",
            p.index, p.steps, chip_marker(chip_name), hex4(pc), opcode_text, operation,
            state.priority, state.multiple, state.mst, state.notetimer, state.cleared and "YES" or "NO")
        end
        track.last_pc = pc
        if not track.loop_detected then
          S.status = string.format("PLAY %02d STEP %02d", p.index, math.min(p.steps, 99))
          S.ui_dirty = true
        end
      end

      if track.armed and track.seen_running and state.cleared and not track.ended then
        track.ended = true
        p.cleared_end = true
        printf("[GORF SOUND] PROCESSOR CLEARED END %02d %s MUSPC=%s STARTPC=%s reset=+$%02X..+$%02X transitions=%d",
          p.index, chip_marker(chip_name), hex4(pc), hex4(state.startpc),
          C.CLEAR_FIRST, C.CLEAR_LAST, track.transitions)
      elseif track.armed and track.seen_running and pc == C.ENDMUS and not track.ended then
        track.ended = true
        printf("[GORF SOUND] PROCESSOR ENDMUS %02d %s transitions=%d",
          p.index, chip_marker(chip_name), track.transitions)
      end

      local endpoint = track.endpoint
      if track.armed and endpoint and track.seen_running and not track.ended
          and pc == endpoint.pc and state.mst == 0 then
        if endpoint.kind == "continuous" and not track.loop_detected then
          track.loop_detected = true
          track.loop_pc = pc
          p.loop_detected = true
          printf("[GORF SOUND] PROCESSOR CONTINUOUS %02d %s %s MUSPC=%s transitions=%d",
            p.index, chip_marker(chip_name), p.entry.name, hex4(pc), track.transitions)
          if p.source == "batch" then
            if schedule_batch_finish(p, "NATIVE CONTINUOUS") then return end
          else
            S.status = "CONTINUOUS"
            S.ui_dirty = true
          end
        end
      end

      if transitioned and track.armed and track.seen_running and not track.ended
          and not track.loop_detected and not state.cleared and pc ~= C.ENDMUS then
        local first_transition = track.pc_seen[pc]
        if first_transition and (track.transitions - first_transition) >= C.LOOP_MIN_TRANSITIONS then
          track.loop_detected = true
          track.loop_pc = pc
          p.loop_detected = true
          printf("[GORF SOUND] LOOP %02d %s %s MUSPC=%s transitions=%d",
            p.index, chip_marker(chip_name), p.entry.name, hex4(pc), track.transitions)
          if p.source == "batch" then
            if schedule_batch_finish(p, "NATIVE LOOP") then return end
          else
            S.status = "LOOPING"
            S.ui_dirty = true
          end
        else
          track.pc_seen[pc] = track.transitions
        end
      end
      if not track.armed or not track.ended then all_ended = false end
    end
  end

  -- Composite events complete only after every declared processor reaches an
  -- authoritative native completion state.
  if all_ended then
    local reason = p.cleared_end and "NATIVE CLEARED END" or "NATIVE ENDMUS"
    finish_playback(reason)
  end
end

local function set_wav_capture(value)
  if value == nil then value = not S.wav_enabled end
  S.wav_enabled = value == true
  printf("[GORF SOUND] WAV capture %s", S.wav_enabled and "ON" or "OFF")
  S.ui_dirty = true
  return S.wav_enabled
end

-- ---------------------------------------------------------------------------
-- Selection, play-all and controls
-- ---------------------------------------------------------------------------

local function move_selection(delta)
  local list = S.catalog
  if #list == 0 then return end
  local current = S.selection or 1
  if current < 1 then current = 1 end
  local n = current + delta
  if n < 1 then n = #list elseif n > #list then n = 1 end
  keep_selection_visible(n)
  if not S.playback and not S.batch then S.status = "READY" end
  S.ui_dirty = true
end

local function start_play_all()
  if not S.takeover then print("[GORF SOUND] gsall(): browser has not taken over yet"); return false end
  if S.batch then print("[GORF SOUND] gsall(): play-all already active"); return false end
  if S.playback or S.wav_active then print("[GORF SOUND] gsall(): wait for current sound/WAV"); return false end
  local list = S.catalog
  if #list == 0 then print("[GORF SOUND] gsall(): catalog is empty"); return false end
  S.batch = { next_index=1, current_index=nil, completed=0, total=#list }
  printf("[GORF SOUND] play-all: %d sounds; WAV %s", #list, S.wav_enabled and "ON" or "OFF")
  S.ui_dirty = true
  return true
end

local function stop_play_all()
  if not S.batch then
    if S.playback then stop_sound("user stop"); return true end
    print("[GORF SOUND] gsstop(): nothing is playing")
    return false
  end
  local completed, total = S.batch.completed, S.batch.total
  if S.playback and not stop_sound("play-all stop") then return false end
  if S.wav_active then stop_owned_wav("stopped") end
  S.batch = nil
  S.status = "READY"
  S.ui_dirty = true
  printf("[GORF SOUND] play-all stopped immediately: %d/%d completed", completed, total)
  return true
end

local function service_batch()
  local b = S.batch
  if not b then return end

  if b.current_index and not S.playback and not S.wav_active then
    b.completed = b.completed + 1
    b.current_index = nil
  end
  if b.current_index or S.playback or S.wav_active then return end

  local list = S.catalog
  if b.next_index > #list then
    printf("[GORF SOUND] play-all complete: %d sounds", b.completed)
    S.batch = nil
    S.ui_dirty = true
    return
  end

  local index = b.next_index
  b.next_index = b.next_index + 1
  b.current_index = index
  S.selection = index
  keep_selection_visible(index)
  local ok, err = start_entry(list[index], index, "batch")
  if not ok then
    printf("[GORF SOUND] play-all error at %d: %s", index, tostring(err))
    S.batch = nil
    S.ui_dirty = true
  end
end

local function read_controls()
  local raw = io_space:read_u8(C.P1PORT)
  local joy = (raw ~ 0x0F) & 0x0F
  local fire = ((raw & 0x10) == 0) and 0x10 or 0
  return joy | fire
end

local function read_1p_start()
  return ((~io_space:read_u8(C.COINPORT)) & 0x10) ~= 0
end

local function read_2p_start()
  return ((~io_space:read_u8(C.COINPORT)) & 0x20) ~= 0
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
    local index, e = selected_entry()
    if e then
      if S.playback and S.playback.index == index and S.playback.entry == e then
        stop_sound("fire stop")
      else
        local ok, err = start_entry(e, index, "manual")
        if not ok then printf("[GORF SOUND] PLAY ERROR: %s", tostring(err)); S.status = "PLAY ERROR" end
      end
    end
  end

  if read_1p_start() then machine:exit() end
  S.last_controls = c
  S.last_2p_start = start2
end

-- ---------------------------------------------------------------------------
-- Console commands
-- ---------------------------------------------------------------------------

local function console_list()
  local list = S.catalog
  printf("[GORF SOUND] complete catalog: %d events / 24 distinct scores; source=%s", #list, S.source_label)
  for i, e in ipairs(list) do
    printf("[GORF SOUND] %02d %-9s %-22s %s",
      i, e.chip:upper(), e.name, route_text(e))
    local parts = {}
    for _, component in ipairs(e.components) do parts[#parts + 1] = component_text(component) end
    printf("[GORF SOUND]    scores=%s source=%s", table.concat(parts, " "), tostring(e.source))
  end
  return #list
end

local function console_info()
  local index, e = selected_entry()
  if not e then print("[GORF SOUND] no selected sound"); return nil end
  printf("[GORF SOUND] selected %02d id=%s name=%s", index, tostring(e.id), tostring(e.name))
  printf("[GORF SOUND] processors=%s route=%s", e.chip:upper(), route_text(e))
  for i, component in ipairs(e.components) do
    printf("[GORF SOUND] component %d %s", i, component_text(component))
  end
  if e.source then printf("[GORF SOUND] source=%s", e.source) end
  return e
end

local function console_select(index)
  index = math.floor(tonumber(index) or 0)
  if index < 1 or index > #S.catalog then
    printf("[GORF SOUND] gsselect(): index must be 1..%d", #S.catalog)
    return false
  end
  keep_selection_visible(index)
  if not S.playback and not S.batch then S.status = "READY" end
  S.ui_dirty = true
  log_selection("console")
  return true
end

local function console_play(index)
  index = math.floor(tonumber(index) or 0)
  local list = S.catalog
  if index < 1 or index > #list then
    printf("[GORF SOUND] gsplay(): index must be 1..%d", #list)
    return false
  end
  keep_selection_visible(index)
  local ok, err = start_entry(list[index], index, "console")
  if not ok then printf("[GORF SOUND] gsplay(): %s", tostring(err)) end
  return ok
end

local function console_audit()
  local index, e = selected_entry()
  if not e then print("[GORF SOUND] gsaudit(): no selected score"); return false end
  printf("[GORF SOUND] AUDIT %02d %s %s", index, e.chip:upper(), e.name)
  printf("[GORF SOUND] route=%s source=%s", route_text(e), tostring(e.source or "--"))
  if e.route.kind == "terse" then
    printf("[GORF SOUND] TERSE word %s: %s", hex4(e.route.word), rom_bytes(e.route.word, 24))
  elseif e.route.kind == "routine" then
    printf("[GORF SOUND] native routine %s: %s", hex4(e.route.routine), rom_bytes(e.route.routine, 16))
  end
  for i, component in ipairs(e.components) do
    printf("[GORF SOUND] component %d %s bytes: %s", i, component_text(component), rom_bytes(component.score, 48))
  end
  log_processor_state("AUDIT", "primary")
  log_processor_state("AUDIT", "secondary")
  printf("[GORF SOUND] MUSICFLAG=%02X dspatch=%s busaround=%s emusic=%s bmusic=%s pmusic=%s",
    program:read_u8(C.MUSICFLAG), hex4(C.DSPATCH), hex4(C.BUSAROUND), hex4(C.EMUSIC), hex4(C.BMUSIC), hex4(C.PMUSIC))
  return true
end

local function console_state()
  for _, chip_name in ipairs({"primary", "secondary"}) do
    local s = native_processor_state(chip_name)
    printf("[GORF SOUND] %-9s array=%s MUSPC=%s STARTPC=%s SOUNDBOX=%02X MULTIPLE=%02X PRIORITY=%02X NOTETIMER=%02X MST=%02X CLEARED=%s",
      chip_name:upper(), hex4(s.array), hex4(s.muspc), hex4(s.startpc), s.soundbox,
      s.multiple, s.priority, s.notetimer, s.mst, s.cleared and "YES" or "NO")
    if not s.cleared then
      printf("[GORF SOUND]   first active work byte +$%02X=%02X",
        s.first_nonzero_offset, s.first_nonzero_value)
    end
  end
  printf("[GORF SOUND] MUSICFLAG=%02X", program:read_u8(C.MUSICFLAG))
  return true
end

local function console_diag()
  printf("[GORF SOUND] diagnostic: drawchar=%s dspatch=%s busaround=%s emusic=%s bmusic=%s pmusic=%s",
    S.drawchar and hex4(S.drawchar) or "--", hex4(C.DSPATCH), hex4(C.BUSAROUND), hex4(C.EMUSIC), hex4(C.BMUSIC), hex4(C.PMUSIC))
  printf("[GORF SOUND] ROM %s busaround: %s", hex4(C.BUSAROUND), rom_bytes(C.BUSAROUND, 26))
  printf("[GORF SOUND] ROM %s emusic: %s", hex4(C.EMUSIC), rom_bytes(C.EMUSIC, 36))
  printf("[GORF SOUND] ROM %s bmusic: %s", hex4(C.BMUSIC), rom_bytes(C.BMUSIC, 22))
  printf("[GORF SOUND] ROM %s pmusic: %s", hex4(C.PMUSIC), rom_bytes(C.PMUSIC, 26))
  printf("[GORF SOUND] MUSICFLAG=%02X P1+08=%02X P2+08=%02X",
    program:read_u8(C.MUSICFLAG),
    program:read_u8(C.PRIMARY_MUSIC + 0x08),
    program:read_u8(C.SECONDARY_MUSIC + 0x08))
  return true
end

local function print_console_commands()
  print("[GORF SOUND] console commands:")
  print("  gswav() / gswav(true|false)   WAV capture toggle/set")
  print("  gsall()                        play complete catalog")
  print("  gsstop()                       stop current sound/play-all")
  print("  gsselect(n)                    select catalog item n")
  print("  gsplay(n)                      play catalog item n")
  print("  gslist()                       list complete catalog")
  print("  gsinfo()                       selected ROM score details")
  print("  gsaudit()                      dump selected score/engine state")
  print("  gsstate()                      dump native Gorf processor state")
  print("  gstrace() / gstrace(true|false) score-transition log toggle/set")
  print("  gsdiag()                       dump Gorf music-engine anchors")
  print("  gsexit()                       exit MAME")
  print("  gshelp()                       show this list")
end

local function install_console_shortcut(name, handler)
  local previous = rawget(_G, name)
  S.shortcuts[name] = { handler=handler, previous=previous, restore=previous ~= nil }
  rawset(_G, name, handler)
end

local function install_console_shortcuts()
  install_console_shortcut("gswav", function(value) return set_wav_capture(value) end)
  install_console_shortcut("gsall", function() return start_play_all() end)
  install_console_shortcut("gsstop", function() return stop_play_all() end)
  install_console_shortcut("gsselect", function(index) return console_select(index) end)
  install_console_shortcut("gsplay", function(index) return console_play(index) end)
  install_console_shortcut("gslist", function() return console_list() end)
  install_console_shortcut("gsinfo", function() return console_info() end)
  install_console_shortcut("gsaudit", function() return console_audit() end)
  install_console_shortcut("gsstate", function() return console_state() end)
  install_console_shortcut("gstrace", function(value)
    if value == nil then value = not S.trace_enabled end
    S.trace_enabled = value == true
    printf("[GORF SOUND] score-transition trace %s", S.trace_enabled and "ON" or "OFF")
    return S.trace_enabled
  end)
  install_console_shortcut("gsdiag", function() return console_diag() end)
  install_console_shortcut("gsexit", function() machine:exit() end)
  install_console_shortcut("gshelp", function() print_console_commands() end)
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

-- ---------------------------------------------------------------------------
-- Takeover and frame service
-- ---------------------------------------------------------------------------

local function takeover(reason)
  if S.takeover then return true end
  local ok, why = validate_program()
  if not ok then
    S.status = "PROGRAM VALIDATION FAILED"
    printf("[GORF SOUND] takeover refused: %s", why)
    return false
  end

  clear_video_ram()
  install_idle_loop()

  S.takeover = true
  local stop_ok, stop_err = run_native_stop()
  if not stop_ok then
    S.status = "NATIVE STOP FAILED"
    printf("[GORF SOUND] takeover refused: native stop failed: %s", tostring(stop_err))
    S.takeover = false
    return false
  end
  S.last_controls = 0
  S.last_2p_start = read_2p_start()
  S.hold_dir = 0
  S.hold_frames = 0
  S.playback = nil
  S.batch = nil
  S.status = "READY"
  S.ui_dirty = true

  printf("[GORF SOUND] browser takeover active (%s); %s", reason or "manual", why)
  printf("[GORF SOUND] catalog: %d sound events / 24 distinct scores; source=%s", #S.catalog, S.source_label)
  print("[GORF SOUND] controls: UP/DOWN select/scroll; FIRE play/stop current; 1P exit; 2P play all/stop")
  log_selection("takeover")
  return true
end

local function on_frame()
  if not S.enabled then return end

  if not S.takeover then
    if not S.takeover_attempted and machine_seconds() >= C.TAKEOVER_DELAY_SEC then
      S.takeover_attempted = true
      if takeover("auto") then render_ui_native() end
    end
    return
  end

  process_inputs()
  service_playback()
  service_wav_capture()
  service_batch()
  render_ui_native()
end

print("============================================================")
printf("[GORF SOUND] GORF SOUND BROWSER %s", VERSION)
printf("[GORF SOUND] takeover RAM: %s; UI code: %s; native launcher: %s; ROM patching: NONE",
  hex4(C.IDLE_LOOP), hex4(C.DRAW_CODE), hex4(C.PLAY_CODE))
printf("[GORF SOUND] Gorf Program-2 engine: dspatch=%s bmusic=%s pmusic=%s",
  hex4(C.DSPATCH), hex4(C.BMUSIC), hex4(C.PMUSIC))
printf("[GORF SOUND] native engine: busaround=%s emusic=%s bmusic=%s pmusic=%s", hex4(C.BUSAROUND), hex4(C.EMUSIC), hex4(C.BMUSIC), hex4(C.PMUSIC))
print("[GORF SOUND] playback: original TERSE event words and native game launchers through the ROM music interpreter")
print("[GORF SOUND] Lua sound writes/capture/replay: NONE; completion monitor: emusic reset footprint plus native loop/endpoints")
install_console_shortcuts()
print_console_commands()
print("============================================================")

install_rom_catalog()
printf("[GORF SOUND] complete ROM catalog: %d gameplay events / 24 distinct score streams", #S.catalog)

if emu.add_machine_frame_notifier then
  S.frame_subscription = emu.add_machine_frame_notifier(on_frame)
else
  emu.register_frame_done(on_frame, "gorf_sound_browser")
end

if emu.add_machine_stop_notifier then
  S.stop_subscription = emu.add_machine_stop_notifier(function()
    S.enabled = false
    if S.wav_active then stop_owned_wav("closed") end
    restore_console_shortcuts()
  end)
end

printf("[GORF SOUND] %s loaded from %s; Gorf boots normally, ROM browser takeover begins after %.1fs",
  VERSION, BUILD_FILE, C.TAKEOVER_DELAY_SEC)
