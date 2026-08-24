; Gorf Speech Browser - injected Z80 foreground/UI program
; --------------------------------------------------------
; Browser: gorf_speech_browser.lua 1.0.10-20260814-1508
; Runtime target: Gorf Program 2 / MAME 0.289
;
; This listing is the Z80 program injected into Gorf work RAM by the Lua
; browser.  It was reconstructed from the Lua code generator and verified
; byte-for-byte against a live MAME debugger dump for the captured UI state.
;
; The UI program at $D420 is regenerated when the browser screen changes.
; Its structure is fixed; row color attributes and string contents reflect
; the current selection/view at the time of generation.
;
; Browser RAM layout:
;   $D400-$D404  foreground HALT loop
;   $D420-$D4FC  generated native UI code
;   $D520-$D66F  current UI strings
;   $D7E0        browser call stack
;
; $D405-$D41F and $D4FD-$D51F are intentionally omitted.  The browser does
; not write or execute those addresses.
;
; Native renderer:
;   $093E        Gorf drawchar
;

; -----------------------------------------------------------------------------
; $D400-$D404 - Foreground HALT loop
; Leaves interrupts enabled so Gorf's native sound and SC-01 interrupt service
; continue while the browser owns foreground execution.
; -----------------------------------------------------------------------------

        ORG     $D400

        EI                      ; enable interrupts
idle:   HALT                    ; wait for native interrupt service
        JP      idle            ; return to HALT after each interrupt


; -----------------------------------------------------------------------------
; $D420-$D4FC - Generated native UI program
; BC = drawchar EXPAND/Magic attributes
; DE = X coordinate
; HL = centered Y coordinate
; IX = NUL-terminated string in $D520-$D66F
; -----------------------------------------------------------------------------

        ORG     $D420

        DI
        PUSH    IX
        PUSH    IY

; Heading - blue
        LD      BC,$0808
        LD      DE,$4C00
        LD      HL,$0180
        LD      IX,$D520
        CALL    draw_string

; Version - blue
        LD      BC,$0808
        LD      DE,$4600
        LD      HL,$0180
        LD      IX,$D53C
        CALL    draw_string

; Catalog row 0
        LD      BC,$0C08        ; red, Magic EXPAND/replace
        LD      DE,$4000
        LD      HL,$0180
        LD      IX,$D558
        CALL    draw_string

; Catalog row 1
        LD      BC,$0C08
        LD      DE,$3A00
        LD      HL,$0180
        LD      IX,$D574
        CALL    draw_string

; Catalog row 2
        LD      BC,$0C08
        LD      DE,$3400
        LD      HL,$0180
        LD      IX,$D590
        CALL    draw_string

; Catalog row 3
        LD      BC,$0C08
        LD      DE,$2E00
        LD      HL,$0180
        LD      IX,$D5AC
        CALL    draw_string

; Catalog row 4
        LD      BC,$0C08
        LD      DE,$2800
        LD      HL,$0180
        LD      IX,$D5C8
        CALL    draw_string

; Catalog row 5
        LD      BC,$0C08
        LD      DE,$2200
        LD      HL,$0180
        LD      IX,$D5E4
        CALL    draw_string

; Catalog row 6
        LD      BC,$0C08
        LD      DE,$1C00
        LD      HL,$0180
        LD      IX,$D600
        CALL    draw_string

; Footer row 1 - yellow
        LD      BC,$0408
        LD      DE,$1000
        LD      HL,$0180
        LD      IX,$D61C
        CALL    draw_string

; Footer row 2 - yellow
        LD      BC,$0408
        LD      DE,$0A00
        LD      HL,$0180
        LD      IX,$D638
        CALL    draw_string

; Footer row 3 - yellow
        LD      BC,$0408
        LD      DE,$0400
        LD      HL,$0180
        LD      IX,$D654
        CALL    draw_string

        POP     IY
        POP     IX
        EI
        JP      idle

; Draw a NUL-terminated string through Gorf's native character renderer.
draw_string:
        LD      A,(IX+0)
        OR      A
        RET     Z
        INC     IX
        PUSH    IX              ; drawchar may use index state
        CALL    $093E           ; Gorf native drawchar
        POP     IX
        JR      draw_string
