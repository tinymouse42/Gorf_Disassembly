# Gorf Sound Browser

The complete sound-engine analysis, event catalog, score programs, route provenance, and lifecycle classification are maintained in [`SOUND_MAP.md`](SOUND_MAP.md). 

This README covers the Lua Sound Browser tool: startup, native takeover, injected Z80, controls, monitoring, logging, and validation.

The tool does not patch ROM, write Astrocade sound registers from Lua, synthesize replacement effects, or replay captured register streams.

| Route class | Entries | Tool action |
|---|---:|---|
| Compiled TERSE event word | 15 | Dispatch the original ROM word through Gorf's `DSPATCH` |
| Self-contained native launcher | 3 | Call the release-ROM gameplay launcher |
| Exact engine submission | 2 | Submit the original score to the original processor and engine entry without entering a stateful game routine |

Run the tool as an autoboot script:

```text
mame gorf -autoboot_script gorf_sound_browser.lua
```

Gorf boots normally. The tool waits 12 seconds, validates the resident ROM, then replaces the foreground game loop with the browser controller.

## Native takeover model

Lua acts as the supervisor. It validates ROM signatures, generates menu and launcher code in work RAM, reads controls, updates the catalog window, records diagnostics, and transfers the Z80 program counter to the generated routines.

The Z80 remains responsible for display and sound execution:

- Gorf's resident character renderer draws the screen.
- Gorf's `DSPATCH` executes compiled TERSE events.
- Native gameplay launchers submit their own scores.
- `emusic`, `bmusic`, and `pmusic` control the original work arrays.
- Interrupt-time `muscpus` advances synthesis.
- The injected foreground loop calls `busaround` to advance score bytecode.

## Injected work-RAM layout

| Address | Use |
|---:|---|
| `$D400-$D407` | Resident interrupt/service loop |
| `$D420-$D55F` | Generated native menu renderer, string loop, and dash-glyph path |
| `$D560-$D6BF` | Generated menu text and dash bitmap |
| `$D6C0-$D6C3` | Two-cell TERSE thread |
| `$D6D0-$D6D3` | TERSE return target |
| `$D6F0` | TERSE return-stack base |
| `$D700-$D79F` | Generated play or stop launcher |
| `$D7E0` | Native call stack and TERSE parameter-stack pointer |

The generator treats `$D560`, `$D6C0`, and `$D7A0` as exclusive limits. A generated object may end at `$D55F`, `$D6BF`, or `$D79F`; it may not write the following byte. Menu strings, the dash bitmap, and launcher bodies are sized before any of their bytes are written. Static layout validation also checks the idle loop, TERSE thread, return target, return-stack base, launcher region, and 64-byte call-stack guard before takeover.

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

For each redraw, Lua generates a Z80 display routine at `$D420` and null-terminated strings at `$D560`. The title begins at the left edge, while the version is drawn independently in yellow at the right edge. The status row contains only current execution state; it does not display the visible window range or catalog size.

Gorf's resident character table has no hyphen. A six-column dash bitmap in work RAM is drawn through Gorf's `RELABS` vector and native `write` routine. The Up and Down controls use native text, and the ROM character table remains unchanged.

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
| 2P Start | Start play-all; press again to stop the current event and cancel play-all immediately |
| 1P Start | Exit MAME |

The screen presents these controls as `UP DOWN SELECT - FIRE PLAY` and `1P EXIT - 2P PLAY ALL`. Directional repeat begins after 15 frames and repeats every four frames. Left and Right have no browser function. Cursor movement redraws the native menu without printing selection traffic to the Lua console; explicit console selections, takeover, and playback remain logged.

## Playback state and completion

The catalog declares every processor and score root expected to run for an event. The monitor creates one track per declared processor. Monitoring remains disarmed until the injected launcher returns to the foreground service loop and the processor's `STARTPC` matches the declared score root. This prevents the prior event's work-array state from appearing as the first transition of a new event.

Normal finite completion is the exact reset footprint produced by `emusic`: every work byte from `+$05` through `+$2F` is zero. This range includes `MULTIPLE`, `PRIORITY`, all motion state, `NOTETIMER`, and `MST`. `STARTPC` and `SOUNDBOX` are outside the cleared range and remain available for identity and diagnostics.

`MUSPC==$0B85` is not required for normal completion. On the final `QUIET`, Gorf's `quityet` handler calls `emusic`, but `musinterp.endprocess` then commits the score pointer already advanced past the `$04` opcode. The stable postcondition is a cleared processor with `MUSPC` at the byte after `QUIET`. `SOUND_MAP.md` records that post-PC for all 17 finite score roots.

An event using both processors completes only after both declared work arrays reach an authoritative completion state. Clearing one array cannot end a composite event while the other remains active.

For each track, the tool records the last `MUSPC`, transition count, visited addresses, completion state, and detected loop address. A repeated nonconsecutive `MUSPC`, separated by at least two transition counts, marks a native control-flow loop. Manual playback leaves nonterminating scores active until Fire or `gsstop()`.

Three ROM streams settle at stationary continuous endpoints that cannot be inferred from the general return-to-`ENDMUS` or repeated-PC rules. Their component declarations carry exact endpoint policies:

| Event | Processor | Endpoint | Native state |
|---|---|---:|---|
| Invader Thump | Secondary | `$812E` | Unlimited master-oscillator ramp remains active after the final `YIELD` |
| Large Invader | Secondary | `$8153` | Unlimited ramble remains active; the adjacent `QUIET` is not executed |
| Galaxian Attack | Secondary | `$97BE` | Unlimited final ramble remains active after the final `YIELD` |

The endpoint check requires the declared `MUSPC`, a matching `STARTPC`, a completed native launcher, and `MST=0`. No wall-clock timeout participates in finite-score completion. Timed duration, ramp, note, and ramble stages therefore run to the state established by the ROM score.

Play-all applies a separate browser policy to nonterminating scores. A detected loop or stationary continuous endpoint receives at least 2.0 seconds from submission before the tool calls native `emusic` and advances. If native progression takes longer than two seconds to establish the loop, play-all stops when that loop is detected. Manual playback has no duration limit. Pressing 2P Start or calling `gsstop()` stops the current processor through the native stop launcher and cancels play-all immediately.

The UI status begins at `PLAY nn STEP 00` and advances once for every observed processor score-PC transition. Natural finite completion leaves `DONE nn STEPS ss` on screen. Completed playback is removed immediately, so the first Fire press after `DONE` starts the selected event again; it is not consumed as a stop request.

## Console commands

| Command | Action |
|---|---|
| `gslist()` | List all catalog entries, canonical routes, score components, processors, and source labels |
| `gsselect(n)` | Select item `n` without playing it |
| `gsplay(n)` | Select and play item `n` |
| `gsinfo()` | Print the selected event, route, components, and source label |
| `gsaudit()` | Dump selected ROM bytes and both processor states |
| `gsstate()` | Dump `MUSPC`, `STARTPC`, `SOUNDBOX`, `MULTIPLE`, `PRIORITY`, `NOTETIMER`, `MST`, cleared-state status, the first active work byte, and `MUSICFLAG` |
| `gsdiag()` | Dump the fixed dispatcher and music-engine anchors with ROM bytes |
| `gstrace()` / `gstrace(true\|false)` | Toggle or set score-transition logging; enabled by default |
| `gsall()` | Start play-all |
| `gsstop()` | Stop the current event or cancel play-all immediately |
| `gswav()` / `gswav(true\|false)` | Toggle or set per-event WAV recording |
| `gsexit()` | Exit MAME |
| `gshelp()` | Print the command list |

## Logging

Explicit selection logging is one line containing the item number, processor marker, and event name. Directional navigation is silent.

Each playback block begins after a blank console line. The `PLAY` line contains the two-digit item number without a `/20` suffix. The following indented lines report:

- request source: manual, console, or play-all;
- canonical route and source label;
- a compact `PRE-RESET` snapshot of both processors' `MUSPC`, `PRIORITY`, and `MST` immediately before the generated launcher resets them through `emusic`;
- every score submission declared for the event;
- the point at which each expected `STARTPC` is accepted and monitoring becomes armed;
- each observed `MUSPC` transition and aggregate event step number;
- current opcode and decoded opcode name;
- `PRIORITY`, `MULTIPLE`, `MST`, `NOTETIMER`, and cleared-state classification;
- per-processor `ENDMUS`, reset-footprint, loop, or stationary-continuous classification;
- detected loops, continuous endpoints, their addresses, and the play-all audition interval;
- elapsed time and stop reason.

`PRE-RESET` records residue or an active score that the launcher is about to replace. The complete work-array dump remains available through `gsstate()` and `gsaudit()`. The trace reads game state only. It does not capture, replay, or alter sound-register writes.

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
- native Pattern Board writer signature at `$062E` and initialized `RELABS` vector at `$D080`;
- `ENDMUS` byte `$03` at `$0B85`;
- opening bytes of score `$1354`;
- exactly 20 unique catalog IDs;
- valid route and component declarations for every entry;
- exactly 15 TERSE routes, three native-launcher routes, two exact-submission routes, and five composite events;
- `$CF` entry signature for every selected TERSE word;
- `$21` entry signature for every selected native launcher;
- valid `$00-$1B` opening opcode at every declared score root;
- exactly 24 distinct declared score roots;
- exact stationary continuous endpoint declarations for Invader Thump `$812E`, Large Invader `$8153`, and Galaxian Attack `$97BE`;
- generated work-RAM ordering, exclusive region limits, the 64-byte call-stack guard, and the `emusic` cleared range `+$05..+$2F`.

These checks bind the tool to the intended Program-2 image and reject incomplete or mismatched catalogs before foreground takeover.

## Verification status

The delivered source passed:

- Lua syntax validation with `texluac -p`;
- static catalog validation for 20 IDs, 15 TERSE routes, three native routes, two exact submissions, and 24 score roots;
- stationary-continuous regression checks for Invader Thump `$812E`, Large Invader `$8153`, and Galaxian Attack `$97BE`;
- two-second play-all audition checks for items 09 and 10, advancement through item 11, and immediate play-all cancellation;
- simulated reset-footprint completion, immediate first-Fire replay, two-processor completion, and play-all advancement;
- pre-write and observed-write RAM-bound checks for generated code, strings, the dash bitmap, TERSE state, and stack separation;
- launch-arming checks that reject stale pre-reset `MUSPC` state;
- ZMAC assembly of the annotated Program-2 disassembly without errors;
- byte identity against all eight documented Program-2 ROM SHA-1 values.

Runtime test procedure: exercise individual playback, Fire stop, all five two-processor events, logging, WAV capture, all three stationary continuous classifications, immediate 2P play-all cancellation, and one complete `gsall()` pass through all 20 events.
