<!-- SOUND_MAP.md -->
# Gorf Program 2 Astrocade Sound Map

Audit date: 2026-08-22.

This document is the reverse-engineered reference for non-speech audio in the English Gorf Program 2 release ROM. It covers the two Astrocade sound blocks, Gorf's resident music processors, the score-bytecode interpreter, all 20 identified sound events, and all 24 score roots submitted by those events.

SC-01 speech is a separate subsystem documented in `SPEECH_MAP.md`. Gorf's non-speech audio consists of ROM score programs interpreted into Astrocade register writes. The scores are not PCM samples and are not Z80 instructions.

## System architecture

Gorf operates two instances of the Astrocade eight-register sound block. One 48-byte work array drives each block.

| Processor | Physical registers | Block port | Work array |
|---|---|---:|---:|
| Primary / processor 1 | `$10-$17` | `$18` | `$D0B1-$D0E0` |
| Secondary / processor 2 | `$50-$57` | `$58` | `$D0E1-$D110` |

Score opcodes address logical registers `$10-$17`. `portout` converts the logical register number through the selected work array's `SOUNDBOX` field, so the same score interpreter drives either physical block.

`MUSICFLAG` at `$D0AF` enables the complete music service. A zero value prevents both interrupt-time synthesis and foreground score interpretation.

### Astrocade sound registers

| Logical | Primary | Secondary | Function |
|---:|---:|---:|---|
| `$10` | `$10` | `$50` | Master oscillator divider shared by tones A, B, and C |
| `$11` | `$11` | `$51` | Tone A divider |
| `$12` | `$12` | `$52` | Tone B divider |
| `$13` | `$13` | `$53` | Tone C divider |
| `$14` | `$14` | `$54` | Vibrato speed and depth |
| `$15` | `$15` | `$55` | Tone C volume, master modulation, and noise-enable control |
| `$16` | `$16` | `$56` | Packed Tone A and Tone B volumes |
| `$17` | `$17` | `$57` | Noise level and modulation control |

For an unmodulated tone, the nominal divider relationship is:

```text
frequency = chip clock / (2 * (master + 1) * (tone divider + 1))
```

The block ports `$18` and `$58` transfer the corresponding eight-register bank as one descending block.

## Native music engine

| Address | Symbol | Function |
|---:|---|---|
| `$0B85` | `ENDMUS` | Idle `MUSPC` sentinel; the byte at this address is `$03` |
| `$0B86` | `emusic` | Stop one processor, clear its work state, and write zero to all eight hardware registers |
| `$0F7E` | `busaround` | Run pending score-bytecode work for both processors |
| `$0FAC` | `bmusic` | Non-replacing score start |
| `$0FC2` | `pmusic` | Replacing priority score start |
| `$0FDB` | `mmusic` | Non-replacing start with caller-supplied `MULTIPLE` |
| `$0FF0` | `mpmusic` | Replacing priority start with caller-supplied `MULTIPLE` |

### Start controls

`bmusic` and `mmusic` test `PRIORITY` at work-array offset `+$08`. An active priority score blocks the new start. A successful start sets `MST`, clears `NOTETIMER`, installs `MULTIPLE`, and writes the score address to both `MUSPC` and `STARTPC`.

`pmusic` and `mpmusic` call `emusic` for the selected processor, install the new score, set `PRIORITY`, and request immediate interpretation through `MST`.

`emusic` is the native stop operation. It writes the address of `ENDMUS` to `MUSPC`, clears work-array offsets `+$05` through `+$2F`, reads `SOUNDBOX`, and writes zero to all eight registers of that physical sound block.

### TERSE music interface

| Primary word | Address | Secondary word | Address | Native operation |
|---|---:|---|---:|---|
| `EMUSIC` | `$1009` | `E2MUSIC` | `$1058` | Initialize selector and call `emusic` |
| `BMUSIC` | `$101E` | `B2MUSIC` | `$106D` | `bmusic` |
| `PMUSIC` | `$102C` | `P2MUSIC` | `$107B` | `pmusic` |
| `MMUSIC` | `$103A` | `M2MUSIC` | `$1089` | `mmusic` |
| `MPMUSIC` | `$1049` | `MP2MUSIC` | `$1098` | `mpmusic` |

The primary wrappers select `$D0B1`; the secondary wrappers select `$D0E1`. `EMUSIC` sets `SOUNDBOX=$18`, and `E2MUSIC` sets `SOUNDBOX=$58`. The TERSE word `SHUTUP` executes both stop operations.

## Service schedule

Starting a score only initializes its work array. Two native service paths advance it:

1. Interrupt-time `muscpus` calls `muscpu` for both work arrays. `muscpu` advances timers, ramps, volume motion, master-oscillator motion, and noise motion, then writes changed values to the selected sound block.
2. Foreground `busaround` calls `musinterp` for both work arrays. `musinterp` runs only when the processor's `MST` field is nonzero.

`musinterp` loads `MUSPC`, fetches opcodes through `OPADDRESSES`, and executes zero-returning opcodes in one pass. A nonzero-returning opcode commits the updated `MUSPC`, clears `MST`, and returns to timed synthesis. A timer or limit later raises `MST` to resume bytecode interpretation.

## Music-processor work array

Both processors use the same 48-byte layout. Offset `+$06` is cleared and otherwise unused by the identified Program-2 engine paths.

| Offset | Source field | Function |
|---:|---|---|
| `+$00-$01` | `MUSPC` | Current score address |
| `+$02-$03` | `STARTPC` | Root used by `QUIET` repeat control |
| `+$04` | `SOUNDBOX` | `$18` primary or `$58` secondary |
| `+$05` | `MOVALUE` | Current master-oscillator value |
| `+$06` | — | Cleared work byte; no identified Program-2 use |
| `+$07` | `MULTIPLE` | Repeat count |
| `+$08` | `PRIORITY` | Replacement protection tested by `bmusic` and `mmusic` |
| `+$09` | `RAMPFLAG` | Current ramble/ramp direction state |
| `+$0A` | `RAMBLEFLAG` | Enables master-oscillator ramble processing |
| `+$0B` | `HIGHLIM` | Ramble high limit |
| `+$0C` | `LOWLIM` | Ramble low limit |
| `+$0D` | `STEP` | Ramble step |
| `+$0E` | `RAMBLETIMER` | Ramble countdown |
| `+$0F` | `TIMEBASE` | Ramble timer reload |
| `+$10` | `LIMCOUNTER` | Ramble-limit transition count |
| `+$11` | `STOPTB` | Time-base movement stop value |
| `+$12` | `TBSTEP` | Time-base movement step |
| `+$13` | `TBTB` | Time-base movement reload |
| `+$14` | `TBTIMER` | Time-base movement countdown |
| `+$15` | `NOSTOP` | Noise movement stop value |
| `+$16` | `NOSTEP` | Noise movement step |
| `+$17` | `NOTIMER` | Noise movement countdown |
| `+$18` | `NOTIMEBASE` | Noise movement reload |
| `+$19` | `NOVALUE` | Current noise value |
| `+$1A` | `STOPSTEPS` | Step-movement stop value |
| `+$1B` | `BIGOFASTEP` | Step-movement magnitude |
| `+$1C` | `STEPTIMEBASE` | Step-movement reload |
| `+$1D` | `STEPTIMER` | Step-movement countdown |
| `+$1E` | `STOPLOWLIM` | Low-limit movement stop value |
| `+$1F` | `LOWSTEP` | Low-limit movement step |
| `+$20` | `LOW#` | Low-limit movement counter |
| `+$21` | `LOWCOUNTER` | Low-limit movement countdown |
| `+$22` | `STOPHIGHLIM` | High-limit movement stop value |
| `+$23` | `HIGHSTEP` | High-limit movement step |
| `+$24` | `HIGH#` | High-limit movement counter |
| `+$25` | `HIGHCOUNTER` | High-limit movement countdown |
| `+$26` | `VOLHIGHLIM` | Volume-motion high limit |
| `+$27` | `VOLOWLIM` | Volume-motion low limit |
| `+$28` | `VOLSTEP` | Volume-motion step |
| `+$29` | `VOLTIMEBASE` | Volume-motion reload |
| `+$2A` | `VOLTIMER` | Volume-motion countdown |
| `+$2B` | `MCTRACKER` | Current Tone C/modulation value |
| `+$2C` | `SYNCMO` | Master-oscillator thumper synchronization flag |
| `+$2D` | `STARTMC` | Thumper starting Tone C/modulation value |
| `+$2E` | `NOTETIMER` | Duration countdown |
| `+$2F` | `MST` | Score-state-transition request |

## Score bytecode

`OPADDRESSES` contains 28 handler pointers for opcodes `$00-$1B`.

| Opcode | Source operation | Operands | Interpreter effect |
|---:|---|---:|---|
| `$00` | `RANDOMNOTES` | 3 | Generate a bounded random register value and write it through `portout` |
| `$01` | `DURATION` / `loadtimer` | 1 | Load `NOTETIMER`; yield until the timer expires |
| `$02` | `CONTJUMP` | 2 | Replace the score PC with an absolute little-endian address |
| `$03` | `QUITJUMP` | 0 | Yield: commit the next score PC and return to timed synthesis |
| `$04` | `QUIET` / `quityet` | 0 | Decrement `MULTIPLE`; restart at `STARTPC` or call `emusic` |
| `$05` | `RAMBLE` | 4 | Configure downward master-oscillator ramble: high, low, step, timebase |
| `$06` | `RAMP` | 4 | Configure upward master-oscillator ramble with the same operands |
| `$07` | `MUSIC` | 0 | Disabled in the Program-2 release implementation; handler is `RET` |
| `$08` | `RAMBLE-ON` | 0 | Enable ramble processing |
| `$09` | `RAMBLE-OFF` | 0 | Disable ramble processing |
| `$0A` | `LIMITRAMBLE` | 1 | Load `LIMCOUNTER`; completion stops ramble and raises `MST` |
| `$0B` | `STEPMOVIN` | 3 | Configure stepped movement |
| `$0C` | `LOWMOVIN` | 3 | Configure low-limit movement |
| `$0D` | `HIGHMOVIN` | 3 | Configure high-limit movement |
| `$0E` | `TBMOVIN` | 3 | Configure time-base movement |
| `$0F` | `NOMOVIN` | 4 | Configure noise movement and write its initial value |
| `$10` | `MASTER` / `mastart` | 1 | Write the master oscillator |
| `$11` | Tone A | 1 | Write logical register `$11` |
| `$12` | Tone B | 1 | Write logical register `$12` |
| `$13` | Tone C | 1 | Write logical register `$13` |
| `$14` | `VIBS` | 1 | Write vibrato register `$14` |
| `$15` | `MCVOLS` | 1 | Write Tone C/modulation control through `halfvols` |
| `$16` | `ABVOLS` | 1 | Write packed Tone A/B volume through `halfvols` |
| `$17` | Noise | 1 | Write noise register `$17` and save `NOVALUE` |
| `$18` | `SOUNDMOVIN` | 0 | Disabled in Program 2; handler is `RET` |
| `$19` | `PANLIMITCOUNTIN` | 0 | Disabled in Program 2; handler is `RET` |
| `$1A` | `VOLMOVIN` | 4 | Configure volume motion: high, low, step, timebase |
| `$1B` | `MOHITTIN` | 1 | Enable master-oscillator thumper synchronization |

Opcode `$03` is not the stop operation. It yields after a programmed stage. Opcode `$04` performs the repeat-or-stop decision and calls `emusic` when `MULTIPLE` reaches zero. `ENDMUS` is the address installed by `emusic`; the interpreter does not execute it while the processor is idle.

### Demo-mode volume

`ABVOLS` and `MCVOLS` pass their packed volume values through `halfvols`. When `DEMOMODE` is nonzero, Gorf rotates and masks the volume nibbles before writing the hardware. This is the game's native attract-mode attenuation.

## Complete sound-event catalog

The catalog contains 20 event-level sounds and 24 distinct score starting addresses. Fifteen single-processor events contribute 15 roots. Player Ship Explosion submits one shared root to both processors. Four other composite events contribute eight distinct roots. The total is `15 + 1 + 8 = 24`.

`P` denotes primary processor 1, `S` secondary processor 2, and `B` both processors.

| # | Proc. | Event | Game launch path | Score submission |
|---:|:---:|---|---|---|
| 1 | S | Coin Insert | TERSE `CNSD` `$136D`: `COINSOUND1 B2MUSIC` | S `$1354` non-replacing |
| 2 | P | Attract Joystick FX | Sound call at `$139E` inside `goyak` `$1376` | P `$136A` replacing priority |
| 3 | P | Player Shot | Native launcher `$2AEF` | P `$2669` non-replacing |
| 4 | B | Player Ship Explosion | TERSE `1G` `$26F8` | P `$268C` priority, `8 MS`, S `$268C` priority |
| 5 | P | PZIP | TERSE `PZ` `$270C` | P `$26B3` non-replacing |
| 6 | P | ZPIP | TERSE `ZP` `$2715` | P `$26CF` non-replacing |
| 7 | B | Takeoff | TERSE `TO` `$2792` | S `$2758` priority, then P `$271E` priority |
| 8 | S | Dive | Native `playkbs` `$27D1` | S `$27A1` non-replacing |
| 9 | S | Invader Thump | TERSE `TH` `$812E`: `E2MUSIC`, then priority start | S `$8115` priority |
| 10 | S | Large Invader | TERSE `IA` `$8154` | S `$8139` priority |
| 11 | S | Laser Shot | `FCHECK` sound branch `$9059` | S `$8BA0` replacing priority |
| 12 | S | Galaxian Attack | Native launcher `$97BE` | S `$9786` non-replacing |
| 13 | S | Ship Spiral | TERSE `SP` `$9F5A`: `E2MUSIC`, then non-replacing start | S `$9F45` non-replacing |
| 14 | S | Fireblast | TERSE `FBL` `$9F7C`: `E2MUSIC`, then non-replacing start | S `$9F65` non-replacing |
| 15 | B | Star Spiral | TERSE `ST` `$9FF1` | P `$9F87` priority, then S `$9FBC` priority |
| 16 | S | Background Ship | TERSE `BSF` `$AA1F` | S `$A9D7` non-replacing |
| 17 | B | Ship Explosion | TERSE `SE` `$AA6C` | S `$AA61` priority, then P `$AA28` priority |
| 18 | S | Fireball | TERSE `FBS` `$AA9F`: `E2MUSIC`, then non-replacing start | S `$AA7B` non-replacing |
| 19 | P | Ship Shotoff | TERSE `SO` `$AAD3`: `EMUSIC`, then non-replacing start | P `$AAAA` non-replacing |
| 20 | B | Black Hole Emergence | TERSE `BH` `$AB80` | P `$AADE` priority, then S `$AB2F` priority |

### Additional equivalent launch paths

- TERSE `1D` at `$2683` submits Player Shot score `$2669` through primary `BMUSIC`. Live player-fire code uses the self-contained native launcher at `$2AEF`.
- TERSE `GA` at `$9D33` submits Galaxian Attack score `$9786` through secondary `B2MUSIC`. Live Galaxian attack code uses the self-contained native launcher at `$97BE`.
- A larger TERSE mission word beginning at `$8936` submits `IASCORE` `$8139` through `B2MUSIC` at cell `$8945`. It reuses the Large Invader root and does not define another score stream.

The 20-event inventory assigns one canonical route to each audible event. Equivalent wrappers remain part of the address map because they complete the recovered call graph without creating additional event identities.

## Score-root inventory and lifecycle

| Root | Symbol | Event use | Linear ROM body | Native lifecycle |
|---:|---|---|---|---|
| `$1354` | `COINSOUND1` | Coin Insert, S | `$1354-$1369` | `QUIET` -> `emusic` |
| `$136A` | `COINSOUND2` | Attract Joystick FX, P | `$136A-$136C` | Jump to `$1356`, then `QUIET` |
| `$2669` | `PLAYER_SHOT_SCORE` | Player Shot, P | `$2669-$2682` | `QUIET` |
| `$268C` | `PLAYER_EXPLOSION_SCORE` | Player Ship Explosion, P and S | `$268C-$26B2` | `QUIET` |
| `$26B3` | `PZSCORE` | PZIP, P | `$26B3-$26CE` | `QUIET` |
| `$26CF` | `ZPSCORE` | ZPIP, P | `$26CF-$26F7` | `QUIET` |
| `$271E` | `TO1SCORE` | Takeoff, P | `$271E-$2757` | `QUIET` |
| `$2758` | `TO2SCORE` | Takeoff, S | `$2758-$2791` | `QUIET` |
| `$27A1` | `KBSCORE` | Dive, S | `$27A1-$27D0` | Loops at `$27C0` |
| `$8115` | `THUMPSCORE` | Invader Thump, S | `$8115-$812D` | Final `YIELD`; output remains latched until replaced or stopped |
| `$8139` | `IASCORE` | Large Invader, S | `$8139-$8153` | `QUIET` |
| `$8BA0` | `LZSCORE` | Laser Shot, S | `$8BA0-$8BC1` | Jumps to `KBSCORE`, then loops at `$27C0` |
| `$9786` | `GASCORE` | Galaxian Attack, S | `$9786-$97BD` | Final ramble remains active; `MUSPC` stays at `$97BE` |
| `$9F45` | `SPSCORE` | Ship Spiral, S | `$9F45-$9F59` | `QUIET` |
| `$9F65` | `FBLSCORE` | Fireblast, S | `$9F65-$9F7B` | `QUIET` |
| `$9F87` | `ST1SCORE` | Star Spiral, P | `$9F87-$9FBB` | `QUIET` |
| `$9FBC` | `ST2SCORE` | Star Spiral, S | `$9FBC-$9FF0` | `QUIET` |
| `$A9D7` | `BSFSCORE` | Background Ship, S | `$A9D7-$AA1E` | Tone sequence loops at `$A9E4` |
| `$AA28` | `SE1SCORE` | Ship Explosion, P | `$AA28-$AA60` | `QUIET` |
| `$AA61` | `SE2SCORE` | Ship Explosion, S | `$AA61-$AA6B` | Jumps into `SE1SCORE` at `$AA2E`, then `QUIET` |
| `$AA7B` | `FBSCORE` | Fireball, S | `$AA7B-$AA9E` | Jumps to `BSFSCORE`, then loops at `$A9E4` |
| `$AAAA` | `SOSCORE` | Ship Shotoff, P | `$AAAA-$AAD2` | `QUIET` |
| `$AADE` | `BH1SCORE` | Black Hole Emergence, P | `$AADE-$AB2E` | `QUIET` |
| `$AB2F` | `BH2SCORE` | Black Hole Emergence, S | `$AB2F-$AB7F` | `QUIET` |

The 24-root count uses submitted starting addresses. Four roots intentionally share another score's body through `CONTJUMP`: `$136A -> $1356`, `$8BA0 -> $27A1`, `$AA61 -> $AA2E`, and `$AA7B -> $A9D7`.

### Stationary native endpoints

Most finite streams reach `QUIET`, which calls `emusic` and installs `ENDMUS`. Native control-flow loops revisit a score address. Two streams follow neither pattern and therefore require exact lifecycle descriptions:

| Root | Final programmed operation | Settled `MUSPC` | Native state |
|---:|---|---:|---|
| `$8115` `THUMPSCORE` | `YIELD` at `$812D`; the completed ramp later requests one more interpretation pass | `$812F` | `$812E` is the adjacent TERSE word's `$CF` byte and is rejected as a score opcode; the resulting register state remains latched |
| `$9786` `GASCORE` | `YIELD` at `$97BD` after an unlimited final ramble | `$97BE` | The score PC remains stationary while interrupt-time `muscpu` continues the configured ramble |

These addresses describe resident engine behavior. They are not synthetic duration cutoffs.

## Complete score programs

The following listings preserve every opcode and operand byte in each submitted linear root. Operation names correspond to the opcode table above. `YIELD` is opcode `$03`; `QUIET` is opcode `$04`.

### Core GORFOS scores

#### `$1354` `COINSOUND1` — Coin Insert

```text
14 86 10 10 06 10 3C 01 03 13 5E 12 96 11 7E 16
FF 15 0F 01 40 04
```

`VIBS($86); MASTER($10); RAMP($10,$3C,$01,$03); TONE_C($5E); TONE_B($96); TONE_A($7E); ABVOLS($FF); MCVOLS($0F); DURATION($40); QUIET`

#### `$136A` `COINSOUND2` — Attract Joystick FX

```text
02 56 13
```

`CONTJUMP($1356)`. The target is `COINSOUND1` after its opening `VIBS($86)`, so this route shares the body while omitting the vibrato write.

#### `$2669` `PLAYER_SHOT_SCORE`

```text
13 15 12 87 11 A0 0F 02 FE 01 50 10 4A 05 08 6A
04 01 0A 03 16 34 15 15 03 04
```

`TONE_C($15); TONE_B($87); TONE_A($A0); NOMOVIN($02,$FE,$01,$50); MASTER($4A); RAMBLE($08,$6A,$04,$01); LIMITRAMBLE($03); ABVOLS($34); MCVOLS($15); YIELD; QUIET`

#### `$268C` `PLAYER_EXPLOSION_SCORE`

```text
13 75 12 66 11 53 15 1F 16 FF 0F 1F 01 05 04 10
03 05 02 03 FF 2A 0A 02 03 1A 0F 00 FF 08 15 1F
16 FF 0A 03 08 03 04
```

`TONE_C($75); TONE_B($66); TONE_A($53); MCVOLS($1F); ABVOLS($FF); NOMOVIN($1F,$01,$05,$04); MASTER($03); RAMBLE($02,$03,$FF,$2A); LIMITRAMBLE($02); YIELD; VOLMOVIN($0F,$00,$FF,$08); MCVOLS($1F); ABVOLS($FF); LIMITRAMBLE($03); RAMBLE-ON; YIELD; QUIET`

#### `$26B3` `PZSCORE` — PZIP

```text
13 2C 12 22 11 1F 10 10 05 10 A0 04 01 0A 01 0F
70 04 01 10 16 66 15 26 14 4F 03 04
```

`TONE_C($2C); TONE_B($22); TONE_A($1F); MASTER($10); RAMBLE($10,$A0,$04,$01); LIMITRAMBLE($01); NOMOVIN($70,$04,$01,$10); ABVOLS($66); MCVOLS($26); VIBS($4F); YIELD; QUIET`

#### `$26CF` `ZPSCORE` — ZPIP

```text
13 2C 12 22 11 1F 0F 30 04 01 00 10 60 05 30 60
FC 01 0A 01 16 66 15 16 03 10 2B 05 05 2F FC 01
0A 01 0F 06 FC 01 1E 03 04
```

`TONE_C($2C); TONE_B($22); TONE_A($1F); NOMOVIN($30,$04,$01,$00); MASTER($60); RAMBLE($30,$60,$FC,$01); LIMITRAMBLE($01); ABVOLS($66); MCVOLS($16); YIELD; MASTER($2B); RAMBLE($05,$2F,$FC,$01); LIMITRAMBLE($01); NOMOVIN($06,$FC,$01,$1E); YIELD; QUIET`

#### `$271E` `TO1SCORE` — Takeoff primary

```text
13 10 12 09 11 07 10 02 06 02 90 0A 03 0A 01 0F
73 03 01 03 15 1B 16 BB 03 10 90 13 11 12 44 11
3E 05 74 90 FF 04 0A 01 03 10 74 05 02 74 FF 01
0F 03 FF 01 6B 08 0A 01 03 04
```

`TONE_C($10); TONE_B($09); TONE_A($07); MASTER($02); RAMP($02,$90,$0A,$03); LIMITRAMBLE($01); NOMOVIN($73,$03,$01,$03); MCVOLS($1B); ABVOLS($BB); YIELD; MASTER($90); TONE_C($11); TONE_B($44); TONE_A($3E); RAMBLE($74,$90,$FF,$04); LIMITRAMBLE($01); YIELD; MASTER($74); RAMBLE($02,$74,$FF,$01); NOMOVIN($03,$FF,$01,$6B); RAMBLE-ON; LIMITRAMBLE($01); YIELD; QUIET`

#### `$2758` `TO2SCORE` — Takeoff secondary

```text
13 14 12 05 11 03 10 02 06 02 90 0A 03 0A 01 0F
73 03 01 03 15 1B 16 BB 03 10 02 13 13 12 31 11
54 05 02 22 01 01 0A 01 03 10 22 05 22 70 01 01
0F 03 FE 01 6B 08 0A 01 03 04
```

`TONE_C($14); TONE_B($05); TONE_A($03); MASTER($02); RAMP($02,$90,$0A,$03); LIMITRAMBLE($01); NOMOVIN($73,$03,$01,$03); MCVOLS($1B); ABVOLS($BB); YIELD; MASTER($02); TONE_C($13); TONE_B($31); TONE_A($54); RAMBLE($02,$22,$01,$01); LIMITRAMBLE($01); YIELD; MASTER($22); RAMBLE($22,$70,$01,$01); NOMOVIN($03,$FE,$01,$6B); RAMBLE-ON; LIMITRAMBLE($01); YIELD; QUIET`

#### `$27A1` `KBSCORE` — Dive

```text
13 14 12 C8 11 FD 16 77 15 07 14 48 10 02 05 02
50 01 01 0A 01 03 16 22 15 03 05 30 50 FF 01 0F
3A 02 01 00 01 1D 0F 00 FE 01 3A 01 1D 02 C0 27
```

`TONE_C($14); TONE_B($C8); TONE_A($FD); ABVOLS($77); MCVOLS($07); VIBS($48); MASTER($02); RAMBLE($02,$50,$01,$01); LIMITRAMBLE($01); YIELD; ABVOLS($22); MCVOLS($03); RAMBLE($30,$50,$FF,$01); NOMOVIN($3A,$02,$01,$00); DURATION($1D); NOMOVIN($00,$FE,$01,$3A); DURATION($1D); CONTJUMP($27C0)`

The jump repeats the alternating noise-movement and duration sequence beginning at `$27C0`.

### Mission 1: Astro Battles / Invaders

#### `$8115` `THUMPSCORE` — Invader Thump

```text
1A 0F 00 FF 03 15 0F 16 FF 1B 0F 13 A8 12 B2 11
BD 10 80 06 80 8C 04 24 03
```

`VOLMOVIN($0F,$00,$FF,$03); MCVOLS($0F); ABVOLS($FF); MOHITTIN($0F); TONE_C($A8); TONE_B($B2); TONE_A($BD); MASTER($80); RAMP($80,$8C,$04,$24); YIELD`

The stream has no `QUIET`. `YIELD` first leaves `MUSPC=$812E`. When the final timed ramp completes, `MST` rises once more; `$812E` is the `$CF` byte that opens the adjacent TERSE word, so the score interpreter rejects it and settles at `$812F`. The resulting hardware state remains until another start or `emusic` replaces it.

#### `$8139` `IASCORE` — Large Invader

```text
10 24 05 20 30 02 01 0C 12 FF 10 0D 02 FF 10 13
3E 12 4A 11 5E 16 88 15 08 03 04
```

`MASTER($24); RAMBLE($20,$30,$02,$01); LOWMOVIN($12,$FF,$10); HIGHMOVIN($02,$FF,$10); TONE_C($3E); TONE_B($4A); TONE_A($5E); ABVOLS($88); MCVOLS($08); YIELD; QUIET`

### Mission 3: Attack Fighter

#### `$8BA0` `LZSCORE` — Laser Shot

```text
10 28 13 0D 12 29 11 3E 16 BB 15 1B 0F 20 01 01
00 01 20 0F 00 FF 01 20 05 08 28 FF 01 01 20 02
A1 27
```

`MASTER($28); TONE_C($0D); TONE_B($29); TONE_A($3E); ABVOLS($BB); MCVOLS($1B); NOMOVIN($20,$01,$01,$00); DURATION($20); NOMOVIN($00,$FF,$01,$20); RAMBLE($08,$28,$FF,$01); DURATION($20); CONTJUMP($27A1)`

The target is `KBSCORE`; Laser Shot therefore enters the Dive loop after its own opening section.

### Mission 2: Galaxians

#### `$9786` `GASCORE` — Galaxian Attack

```text
13 3E 12 25 11 20 10 10 05 08 20 FF 03 0A 01 17
18 14 00 16 99 15 29 03 14 42 08 0A 01 03 05 20
30 01 03 14 44 0A 01 03 05 1C 40 01 03 14 4A 0A
02 03 05 18 1C FF 04 03
```

`TONE_C($3E); TONE_B($25); TONE_A($20); MASTER($10); RAMBLE($08,$20,$FF,$03); LIMITRAMBLE($01); NOISE($18); VIBS($00); ABVOLS($99); MCVOLS($29); YIELD; VIBS($42); RAMBLE-ON; LIMITRAMBLE($01); YIELD; RAMBLE($20,$30,$01,$03); VIBS($44); LIMITRAMBLE($01); YIELD; RAMBLE($1C,$40,$01,$03); VIBS($4A); LIMITRAMBLE($02); YIELD; RAMBLE($18,$1C,$FF,$04); YIELD`

The last ramble has no limit operation and continues under `muscpu`. `MUSPC` remains at `$97BE`; the sound ends only when game code replaces or stops processor 2.

### Mission 4: Space Warp

#### `$9F45` `SPSCORE` — Ship Spiral

```text
10 57 05 0C 57 FB 01 0A 01 16 88 15 19 13 20 12
25 11 2E 03 04
```

`MASTER($57); RAMBLE($0C,$57,$FB,$01); LIMITRAMBLE($01); ABVOLS($88); MCVOLS($19); TONE_C($20); TONE_B($25); TONE_A($2E); YIELD; QUIET`

#### `$9F65` `FBLSCORE` — Fireblast

```text
13 3E 12 46 11 96 10 40 05 20 A0 FE 01 0A 02 17
20 16 88 15 19 03 04
```

`TONE_C($3E); TONE_B($46); TONE_A($96); MASTER($40); RAMBLE($20,$A0,$FE,$01); LIMITRAMBLE($02); NOISE($20); ABVOLS($88); MCVOLS($19); YIELD; QUIET`

#### `$9F87` `ST1SCORE` — Star Spiral primary

```text
13 20 12 31 11 4A 17 0C 10 40 05 10 40 FF 02 0A
01 16 14 15 14 03 0E 01 FF 01 08 0A 02 03 0D 02
F2 01 0B 02 01 01 17 00 08 0A 02 03 0B 04 02 01
0A 06 08 03 04
```

`TONE_C($20); TONE_B($31); TONE_A($4A); NOISE($0C); MASTER($40); RAMBLE($10,$40,$FF,$02); LIMITRAMBLE($01); ABVOLS($14); MCVOLS($14); YIELD; TBMOVIN($01,$FF,$01); RAMBLE-ON; LIMITRAMBLE($02); YIELD; HIGHMOVIN($02,$F2,$01); STEPMOVIN($02,$01,$01); NOISE($00); RAMBLE-ON; LIMITRAMBLE($02); YIELD; STEPMOVIN($04,$02,$01); LIMITRAMBLE($06); RAMBLE-ON; YIELD; QUIET`

#### `$9FBC` `ST2SCORE` — Star Spiral secondary

```text
13 37 12 54 11 7E 17 0C 10 40 05 10 40 FF 02 0A
01 16 77 15 17 03 0E 01 FF 01 08 0A 02 03 0D 02
F2 01 0B 02 01 01 17 00 08 0A 02 03 0B 04 02 01
0A 06 08 03 04
```

`TONE_C($37); TONE_B($54); TONE_A($7E); NOISE($0C); MASTER($40); RAMBLE($10,$40,$FF,$02); LIMITRAMBLE($01); ABVOLS($77); MCVOLS($17); YIELD; TBMOVIN($01,$FF,$01); RAMBLE-ON; LIMITRAMBLE($02); YIELD; HIGHMOVIN($02,$F2,$01); STEPMOVIN($02,$01,$01); NOISE($00); RAMBLE-ON; LIMITRAMBLE($02); YIELD; STEPMOVIN($04,$02,$01); LIMITRAMBLE($06); RAMBLE-ON; YIELD; QUIET`

### Mission 5: Flag Ship

#### `$A9D7` `BSFSCORE` — Background Ship

```text
10 23 05 10 52 02 01 16 88 15 08 13 FF 13 FD 12
FE 11 FF 01 04 13 FA 12 FC 11 FE 01 04 13 F7 12
FA 11 FD 01 04 13 F4 12 F8 11 FC 01 04 13 F1 12
F6 11 FB 01 04 13 EE 12 F4 11 FA 01 04 13 EB 12
F2 11 F9 01 04 02 E4 A9
```

`MASTER($23); RAMBLE($10,$52,$02,$01); ABVOLS($88); MCVOLS($08); TONE_C($FF); [TONE_C/TONE_B/TONE_A and DURATION($04) descending sequence]; CONTJUMP($A9E4)`

The jump repeats the tone sequence from `$A9E4`; the opening master, ramble, and volume setup runs once.

#### `$AA28` `SE1SCORE` — Ship Explosion primary

```text
13 25 12 29 11 2E 16 66 15 06 10 80 05 02 80 FF
01 0A 01 03 17 10 05 02 0F 01 06 0A 05 16 FF 15
3F 03 1A 0F 00 FF 16 15 1F 16 FF 05 0F B0 01 02
0F 00 00 00 B7 0A 01 03 04
```

`TONE_C($25); TONE_B($29); TONE_A($2E); ABVOLS($66); MCVOLS($06); MASTER($80); RAMBLE($02,$80,$FF,$01); LIMITRAMBLE($01); YIELD; NOISE($10); RAMBLE($02,$0F,$01,$06); LIMITRAMBLE($05); ABVOLS($FF); MCVOLS($3F); YIELD; VOLMOVIN($0F,$00,$FF,$16); MCVOLS($1F); ABVOLS($FF); RAMBLE($0F,$B0,$01,$02); NOMOVIN($00,$00,$00,$B7); LIMITRAMBLE($01); YIELD; QUIET`

#### `$AA61` `SE2SCORE` — Ship Explosion secondary

```text
01 06 13 1B 12 1F 11 22 02 2E AA
```

`DURATION($06); TONE_C($1B); TONE_B($1F); TONE_A($22); CONTJUMP($AA2E)`

The target is the volume/master/ramble body inside `SE1SCORE`. The secondary event therefore begins after a six-tick delay with different tone dividers, then shares the primary explosion tail.

#### `$AA7B` `FBSCORE` — Fireball

```text
10 10 05 10 30 01 02 0A 01 13 29 12 25 11 27 16
A9 15 1C 03 0F 6C 03 02 00 05 30 8F 03 02 0A 01
03 02 D7 A9
```

`MASTER($10); RAMBLE($10,$30,$01,$02); LIMITRAMBLE($01); TONE_C($29); TONE_B($25); TONE_A($27); ABVOLS($A9); MCVOLS($1C); YIELD; NOMOVIN($6C,$03,$02,$00); RAMBLE($30,$8F,$03,$02); LIMITRAMBLE($01); YIELD; CONTJUMP($A9D7)`

The target is `BSFSCORE`, which settles into its `$A9E4` tone loop.

#### `$AAAA` `SOSCORE` — Ship Shotoff

```text
10 10 05 06 10 FE 02 0F 00 00 00 05 16 AA 15 1A
13 20 12 FE 11 FF 01 04 13 1C 12 FC 11 FE 01 04
13 1A 12 FA 11 FD 01 04 04
```

`MASTER($10); RAMBLE($06,$10,$FE,$02); NOMOVIN($00,$00,$00,$05); ABVOLS($AA); MCVOLS($1A); three tone-divider groups separated by DURATION($04); QUIET`

#### `$AADE` `BH1SCORE` — Black Hole primary

```text
13 62 12 40 11 11 10 10 05 10 C0 04 01 0A 01 0F
B0 04 01 00 16 44 15 15 03 10 C0 06 02 C0 F8 01
0F 00 F8 01 B0 0A 01 03 10 80 06 02 80 F8 01 0F
00 F8 01 70 0A 01 03 10 80 06 02 80 F8 01 0F 00
F8 01 70 0A 01 03 10 02 05 02 FF 01 03 0A 01 03
04
```

`TONE_C($62); TONE_B($40); TONE_A($11); MASTER($10); RAMBLE($10,$C0,$04,$01); LIMITRAMBLE($01); NOMOVIN($B0,$04,$01,$00); ABVOLS($44); MCVOLS($15); YIELD; three master/ramp/noise/limit stages; final MASTER($02), RAMBLE($02,$FF,$01,$03), LIMITRAMBLE($01), YIELD; QUIET`

#### `$AB2F` `BH2SCORE` — Black Hole secondary

```text
13 50 12 30 11 13 10 10 05 10 C0 04 01 0A 01 0F
B0 04 01 00 16 77 15 18 03 10 C0 06 02 C0 F8 01
0F 00 F8 01 B0 0A 01 03 10 80 06 02 80 F8 01 0F
00 F8 01 70 0A 01 03 10 80 06 02 80 F8 01 0F 00
F8 01 70 0A 01 03 10 02 05 02 FF 01 03 0A 01 03
04
```

`BH2SCORE` has the same stage structure as `BH1SCORE` with different opening tone dividers and volume values: `TONE_C($50); TONE_B($30); TONE_A($13); ABVOLS($77); MCVOLS($18)`. It terminates through `QUIET`.

## Source authority and verification

Evidence priority:

1. Program-2 release ROM bytes and addresses in `Gorf_Disassembly.asm`.
2. Original GORFOS and mission-source definitions that match the release bytes.
3. Direct Program-2 TERSE cells and native Z80 call sites for score, processor, and start primitive.
4. Resident interpreter behavior at `OPADDRESSES`, `muscpu`, `musinterp`, `bmusic`, `pmusic`, and `emusic`.
5. Runtime observation on the English Program-2 ROM under MAME 0.289.

Static verification establishes 20 event identities, 24 distinct submitted score roots, 15 canonical TERSE event words, three canonical self-contained native launchers, two stateful embedded call sites, five two-processor composite events, and two stationary native endpoints. The annotated ASM assembles to the eight documented Program-2 ROM images without byte changes.

Primary source scans:

- [GORFOS original source listing](https://ftpmirror.your.org/pub/misc/bitsavers/pdf/nuttingAssoc/gorf/GORFOS.txt)
- [Astro Battles / Invaders](https://ftpmirror.your.org/pub/misc/bitsavers/pdf/nuttingAssoc/gorf/gorf_inv.pdf)
- [Attack Fighter](https://ftpmirror.your.org/pub/misc/bitsavers/pdf/nuttingAssoc/gorf/gorf_atf.pdf)
- [Galaxians](https://ftpmirror.your.org/pub/misc/bitsavers/pdf/nuttingAssoc/gorf/gorf_galax_cpy2.pdf)
- [Gorf master source scans, including Space Warp and Flag Ship](https://ftpmirror.your.org/pub/misc/bitsavers/pdf/nuttingAssoc/gorf/Gorf_wo_speech.pdf)
