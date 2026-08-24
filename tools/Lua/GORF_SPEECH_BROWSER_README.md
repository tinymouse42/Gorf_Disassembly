# Gorf Speech Browser

`gorf_speech_browser.lua` is a MAME Lua utility for browsing, testing, tracing, and capturing Gorf Program 2 speech to WAV.

The complete speech-engine analysis, catalog, are maintained in [`SPEECH_MAP.md`](../../docs/SPEECH_MAP.md). 


Gorf boots normally. After the resident speech path is complete, the browser takes over foreground execution from work RAM while leaving the game's interrupt-driven sound and SC-01 service active. The browser renders through Gorf's native `drawchar`/`CHRTBL` path and Astrocade Magic RAM; no MAME overlay is used.

The browser supports resident English speech and the active Program 2 X11 language ROM. Current German, French, and Klingon X11 builds use the same 36 resident speech keys and are resolved through their loaded translation tables.

## Run

From the repository root:

```sh
mame -console -window -autoboot_script tools/Lua/gorf_speech_browser.lua gorf

mame -console -window -autoboot_script tools/Lua/gorf_speech_browser.lua -rompath roms/ gorf
```

Use `-rompath roms/` when testing locally built ROM archives. For a Program 2 X11 build, run the MAME-compatible archive/driver produced by that language build and select the Foreign language DIP setting.

The configured takeover delay is 2 seconds, but takeover also waits for the resident speech path to report complete, so the browser may appear later than the minimum delay.

## Game Screen Controls

- **Up / Down** — move through the current list; selection wraps at the ends
- **Left / Right** — switch between fragments and phrases
- **Fire** — play the selected entry
- **1P Start** — exit MAME
- **2P Start** — play all entries in the current view; press again to stop after the current item

The browser starts in the fragment view with no selection. Down selects the first entry; Up selects the last. The selected row is highlighted in yellow.

Fragment rows show the active ROM address and description and are ordered by physical ROM address. In a foreign X11 build, the displayed address is the translated X11 speech-record address.

## Speech Model

Gorf Program 2 uses 36 resident speech-record keys. The fragment view exposes those 36 records directly.

The phrase view contains 34 Program 2 speech paths covering attract prompts, coin/start prompts, generic taunts, insults, game-start lines, promotion speech, and the flagship sequence. Compound paths are expanded into their component speech records. Rank-dependent paths use the current game rank.

Resident speech uses the pointer ring at `$D112-$D121`:

- `$D112` — `TOPTALK`
- `$D120` — `BOTTOMTALK`
- `$D122` — `TALKHERE`
- `$D124` — `PHONE#`
- `$D125` — `TALKIN`
- `$D127` — `TALKOUT`

For diagnostic playback, the browser resolves the selected resident/X11 record address and queues it through the documented `TALKIN` ring behavior. Gorf's native interrupt-driven `PHONE` path remains responsible for feeding the SC-01.

For foreign speech, the browser locates the active Program 2 X11 translation tables and maps the same 36 resident keys to language-ROM speech records. A zero target is treated as a suppressed fragment.

## Lua Console Commands

```text
gwav()         Toggle WAV capture for played entries
gwav(true)     Enable WAV capture
gwav(false)    Disable WAV capture

gall()         Play all entries in the current view
gstop()        Stop play-all after the current item

gtrace()       Toggle detailed speech tracing
gtrace(true)   Enable detailed tracing
gtrace(false)  Disable detailed tracing

gexit()        Exit MAME
ghelp()        Show the command list
```

Console logging reports the selected key/path, active ROM address, decoded SC-01 phonemes, and completion status.

Fragment example:

```text
[GORF SPEECH] PLAY FRAGMENT key=$11A8 address=$11A8 text="Try again; I devour your coins"
[GORF SPEECH] PHONEMES T R AH2 I1 Y PA0 UH1 G EH1 I3 N PA1 PA1 AH1 I1 Y1 D Y V AH1 U1 ER K O1 UH3 I3 E1 N S PA1
[GORF SPEECH] END FRAGMENT key=$11A8 address=$11A8 elapsed=3.730s
```

Phrase logging expands the selected path into its component records and reports each resident key, active address, description, and phoneme stream in playback order.

`key` is the Program 2 resident speech-record address. `address` is the active speech-record address: the same value for resident English, or the translated record address in a foreign X11 ROM.

The `PHONEMES` line uses SC-01 mnemonics decoded from the stored record bytes. Names such as `AH1`, `I3`, `O1`, and `THV` are SC-01 speech sounds; `PA0` and `PA1` are pause phonemes. Detailed tracing additionally reports raw bytes, speech-ring state, per-phoneme progress, READY state, and detected stalls.

## WAV Capture

When WAV capture is enabled, each played fragment or phrase is recorded separately. The console reports the completed filename with `WAV saved:`.

Example filenames:

```text
gorf_english_fragment_11A8.wav
gorf_english_phrase_06.wav
```

`gall()` follows the current `gwav()` setting. With WAV capture disabled it only plays the list; with WAV capture enabled it records each entry separately.

## Native Foreground and UI Injection

The browser does not patch ROM. After takeover it uses Gorf work RAM for a small foreground loop, generated UI code, native-text data, and a private call stack:

```text
$D400-$D404  foreground HALT loop
$D420-$D4FC  generated native UI code
$D520-$D66F  current UI strings
$D7E0        browser call stack
```

The foreground loop leaves interrupts enabled and spends its time in `HALT`, allowing the original interrupt-driven sound and SC-01 service to continue. UI redraws call Gorf's resident `drawchar` routine at `$093E`, preserve IX/IY around the native renderer, and use Magic EXPAND in non-XOR replace mode.

## Notes

- Speech data is read from the loaded ROM; the browser does not rewrite speech records.
- Resident English and Program 2 X11 speech share the same 36 resident keys.
- The browser screen is entirely native Gorf rendering; no MAME overlay is used.
- Browser UI colors use the game's blue, yellow, and red palette entries.
- The injected UI program is regenerated when the screen changes, so string contents and per-row color attributes in a debugger snapshot reflect the exact browser state at capture time.
