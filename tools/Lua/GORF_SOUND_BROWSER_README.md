# Gorf Sound Browser

Playback follows one canonical game route per event:

| Route class | Entries | Tool action |
|---|---:|---|
| Compiled TERSE event word | 15 | Dispatch the original ROM word through Gorf's `DSPATCH` |
| Self-contained native launcher | 3 | Call the release-ROM gameplay launcher |
| Exact engine submission | 2 | Submit the original score to the original processor and engine entry without entering a stateful game routine |

The event names, addresses, processor assignments, score roots, and route selection are defined in `SOUND_MAP.md` and are not duplicated here.

## Requirements

- MAME 0.289 or later with Lua enabled
- English `gorf` Program 2 ROM set
- `gorf_sound_browser.lua`

Run the tool as an autoboot script:

```text
mame gorf -autoboot_script gorf_sound_browser.lua
```

Gorf boots normally. The tool **waits 12 seconds**, validates the resident ROM, then replaces the foreground game loop with the browser controller.

## Native takeover model

Lua acts as the supervisor. It validates ROM signatures, generates menu and launcher code in work RAM, reads controls, updates the catalog window, records diagnostics, and transfers the Z80 program counter to the generated routines.

The Z80 remains responsible for display and sound execution:

- Gorf's resident character renderer draws the screen.
- Gorf's `DSPATCH` executes compiled TERSE events.
- Native gameplay launchers submit their own scores.
- `emusic`, `bmusic`, and `pmusic` control the original work arrays.
- Interrupt-time `muscpus` advances synthesis.
- The injected foreground loop calls `busaround` to advance score bytecode.

The tool does not patch ROM, write Astrocade sound registers from Lua, synthesize replacement effects, or replay captured register streams.

## Injected work-RAM layout

| Address | Use |
|---:|---|
| `$D400-$D407` | Resident interrupt/service loop |
| `$D420-$D51F` | Generated native menu renderer and string loop |
| `$D520-$D6BF` | Generated menu text |
| `$D6C0-$D6C3` | Two-cell TERSE thread |
| `$D6D0-$D6D3` | TERSE return target |
| `$D6F0` | TERSE return-stack base |
| `$D700-$D79F` | Generated play or stop launcher |
| `$D7E0` | Native call stack and TERSE parameter-stack pointer |

The generator rejects a menu whose code reaches `$D520`, text that reaches `$D6C0`, or a launcher that reaches `$D7A0`. These bounds keep the display program, text, TERSE state, launcher, and stack regions separate.

## Injected foreground loop

The resident loop at `$D400` is eight bytes:

```asm
$D400  EI
$D401  HALT
$D402  CALL $0F7E       ; Gorf busaround
$D405  JP   $D401
```

`HALT` resumes on the game interrupt. The interrupt handler runs the native timed music processors. The foreground call to `busaround` then executes pending score opcodes for both work arrays. Omitting either half stops normal score progression.

## Native stop and reset sequence

Every audition begins by stopping both processors through ROM `emusic`. The generated setup for each work array matches Gorf's `EMUSIC` and `E2MUSIC` wrappers:

```asm
        EXX
        LD   DE,music_array       ; $D0B1 or $D0E1
        LD   HL,$002F             ; MST field
        ADD  HL,DE
        LD   (HL),$01
        LD   HL,$0004             ; SOUNDBOX field
        ADD  HL,DE
        LD   (HL),soundbox        ; $18 or $58
        CALL $0B86                ; emusic
```

The play launcher runs that sequence for `$D0B1/$18` and `$D0E1/$58`, then writes `MUSICFLAG=1`. The stop launcher runs both sequences, writes `MUSICFLAG=0`, restores `IX` and `IY`, enables interrupts, and returns to `$D401`.

## TERSE launch path

For a TERSE-routed event, Lua writes this thread and return target:

```asm
$D6C0  DW event_word
$D6C2  DW $D6D0

$D6D0  EI
$D6D1  JP $D401
```

The generated launcher initializes the TERSE virtual machine and enters the ROM dispatcher:

```asm
        LD   IX,$D6F0       ; TERSE return stack
        LD   SP,$D7E0       ; TERSE parameter/call stack
        LD   IY,$005A       ; DSPATCH
        LD   BC,$D6C0       ; TERSE instruction pointer
        JP   $005A
```

The selected ROM word executes without translation. Its native `RETURN` resumes dispatch at the second thread cell, which transfers control to the injected service loop.

## Native-routine and exact-submission paths

Self-contained gameplay launchers use this generated tail:

```asm
        CALL native_game_launcher
        EI
        JP   $D401
```

The native launcher loads its score and processor array, tail-calls the original music start routine, and returns through the browser's `CALL` instruction.

The two stateful call sites use this generated form:

```asm
        LD   HL,score
        LD   IY,music_array
        CALL native_engine_entry
        EI
        JP   $D401
```

`SOUND_MAP.md` records the original routines, the excluded state dependencies, and the exact values supplied by the browser.

## Native menu renderer

The tool scans Program-2 ROM for the resident `drawchar` routine by instruction signature. It does not assume the address until the signature is found.

For each redraw, Lua generates a Z80 display routine at `$D420` and null-terminated strings at `$D520`. The routine calls the resident character renderer once per character and returns to `$D401`. Video RAM is cleared only when takeover begins.

The screen uses the native Gorf character set and palette attributes:

- Blue: title and status
- Yellow: version, instructions, and selected row
- Red: unselected catalog rows

Each row carries a `P`, `S`, or `B` processor marker. The marker is a compact display property; the detailed processor and score mapping remains in `SOUND_MAP.md`.

## Controls

| Input | Action |
|---|---|
| Up / Down | Move selection through the full catalog; hold for repeat |
| Fire | Play the selected event; press again to stop it |
| 2P Start | Start play-all; press again to request stop after the current event |
| 1P Start | Exit MAME |

Directional repeat begins after 15 frames and repeats every four frames. Left and Right have no browser function.

## Playback state and completion

The catalog declares every processor expected to run for an event. The monitor creates one track per declared processor and reads the native work array each frame.

A finite processor track completes only after:

1. `MUSPC` leaves `ENDMUS` at `$0B85`; and
2. `MUSPC` later returns to `$0B85` through native `emusic`.

This two-step rule prevents the initial idle state from being mistaken for immediate completion. An event using both processors completes only after both tracks complete.

For each track, the tool records the last `MUSPC`, transition count, visited addresses, completion state, and detected loop address. A repeated nonconsecutive `MUSPC`, separated by at least two transition counts, marks a native control-flow loop. Manual playback leaves that loop active until Fire or `gsstop()`. Play-all stops a detected loop through the native stop launcher and advances.

Two ROM streams have stationary endpoints that cannot be inferred from the general return-to-`ENDMUS` or repeated-PC rules. Their component declarations carry exact endpoint policies:

| Event | Processor | Endpoint | Classification | Browser action |
|---|---|---:|---|---|
| Invader Thump | Secondary | `$812F` | Terminal held state | Mark the processor complete; play-all stops it through the native stop launcher and advances |
| Galaxian Attack | Secondary | `$97BE` | Continuous final ramble | Mark the processor continuous; manual playback remains active, while play-all stops it through the native stop launcher and advances |

The endpoint check requires the declared `MUSPC`, a processor that has left `ENDMUS`, and `MST=0`. No wall-clock timeout participates in completion. Timed duration, ramp, note, and ramble stages therefore run to the state established by the ROM score.

## Console commands

| Command | Action |
|---|---|
| `gslist()` | List all catalog entries, canonical routes, score components, processors, and source labels |
| `gsselect(n)` | Select item `n` without playing it |
| `gsplay(n)` | Select and play item `n` |
| `gsinfo()` | Print the selected event, route, components, and source label |
| `gsaudit()` | Dump selected ROM bytes and both processor states |
| `gsstate()` | Dump `MUSPC`, `STARTPC`, `SOUNDBOX`, `MULTIPLE`, `PRIORITY`, `NOTETIMER`, `MST`, and `MUSICFLAG` |
| `gsdiag()` | Dump the fixed dispatcher and music-engine anchors with ROM bytes |
| `gstrace()` / `gstrace(true\|false)` | Toggle or set score-transition logging; enabled by default |
| `gsall()` | Start play-all |
| `gsstop()` | Stop the current event or request play-all stop |
| `gswav()` / `gswav(true\|false)` | Toggle or set per-event WAV recording |
| `gsexit()` | Exit MAME |
| `gshelp()` | Print the command list |

## Logging

Selection logging reports the item number, event name, canonical route, source label, processor set, and every declared score component.

Playback logging reports:

- request source: manual, console, or play-all;
- both processor states before launch;
- every score submission declared for the event;
- each observed `MUSPC` transition;
- current opcode and decoded opcode name;
- `PRIORITY`, `MULTIPLE`, `MST`, and `NOTETIMER`;
- per-processor native completion;
- detected loops, continuous endpoints, terminal stationary endpoints, and their addresses;
- elapsed time and stop reason.

The trace reads game state only. It does not capture, replay, or alter sound-register writes.

## WAV recording

When enabled, playback calls MAME's sound recorder and writes one file per event:

```text
gorf_sound_<event_id>.wav
```

The tool refuses to start when another MAME recording is active. Native stop runs before capture closes, followed by 0.15 seconds of post-roll. WAV recording is observational and does not participate in sound execution.

## ROM validation

Takeover is refused unless all of the following checks pass:

- English Program-2 `SPK_INSERT` signature at `$115D`;
- resident `drawchar` instruction signature;
- `ENDMUS` byte `$03` at `$0B85`;
- opening bytes of score `$1354`;
- `COINSOUND2` bytes `02 56 13` at `$136A`;
- exactly 20 unique catalog IDs;
- valid route and component declarations for every entry;
- exactly 15 TERSE routes, three native-launcher routes, two exact-submission routes, and five composite events;
- `$CF` entry signature for every selected TERSE word;
- `$21` entry signature for every selected native launcher;
- valid `$00-$1B` opening opcode at every declared score root;
- exactly 24 distinct declared score roots;
- exact stationary endpoint declarations for Invader Thump `$812F` (`terminal`) and Galaxian Attack `$97BE` (`continuous`).

These checks bind the tool to the intended Program-2 image and reject incomplete or mismatched catalogs before foreground takeover.

## Verification status

The delivered source passed:

- Lua syntax validation with `texluac -p`;
- static catalog validation for 20 IDs, 15 TERSE routes, three native routes, two exact submissions, and 24 score roots;
- stationary-endpoint regression checks for Invader Thump `$812F` and Galaxian Attack `$97BE`;
- work-RAM region checks for generated code, strings, TERSE state, and stack separation;
- ZMAC assembly of the annotated Program-2 disassembly without errors;
- byte identity against all eight documented Program-2 ROM SHA-1 values.

Runtime test procedure: exercise individual playback, Fire stop, all five two-processor events, logging, WAV capture, both stationary endpoint classifications, and one complete `gsall()` pass through all 20 events.
