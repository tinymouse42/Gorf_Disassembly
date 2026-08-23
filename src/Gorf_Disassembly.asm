;******************************************************************************************
; Gorf Disassembly version 2 ROMS
; David E. Turner (Commander Dave)
; daveturner0x2a@gmail.com
;******************************************************************************************
;
;   CODING CONVENTIONS: (PRELIMINARY - 07/08/2026)
;
;       ;********************************
;       ; My Comments or explanations
;       ;********************************
;
;       ;#########################################################################
;       ; { MODULE - BLOCK #### }
;       ; TERSE CODE from original source
;       ;#########################################################################
;
;           TERSE Block Headers:
;           Prefix all block numbers with their module name to prevent duplicates
;           in this unified Z80 master file. [i.e.] { INVADERS - BLOCK 0150 }
;
;           MODULE PREFIX    SOURCE FILENAME(S)      DESCRIPTION
;           -------------    ------------------      -----------
;           GORFOS           gorf_os.pdf / .txt      Core Operating System & VGS engine
;           INVADERS         gorf_inv.pdf            Mission 1 (Space Invaders)
;           GALAXIANS        gorf_galax_cpy1/2.pdf   Mission 2 (Galaxians)
;           ATTACK FIGHTER   gorf_atf.pdf / .txt     Mission 3 (Laser Attack / Lasar LZ)
;           SPACE WARP       (Found in Master Index) Mission 4 (Space Warp)
;           FLAG SHIP        (Found in Master Index) Mission 5 (Flag Ship / Mother Ship)
;           GAME OVER        gorf_go.pdf             Title Screen & Master Control
;
;       ZMAC (this assembler) does not distinguish between upper/lower case labels.
;
;       High-Level TERSE Colon Definitions (:) and standard TERSE CODE WORDS:
;               Start with an underscore and are UPPERCASE. Unusable symbols become
;               lowercase. [i.e.] _DROP is fine but B@ becomes _Bat. B+ is _Bplus.
;
;       Low-Level TERSE CODE WORDS:
;               If lowercase (like 'snap') and shares a name with an UPPERCASE word,
;               append '_code'. [i.e.] snap_code.
;
;       TERSE SUBR (subroutines):
;               All lowercase by convention.
;
;       Variables:
;               All UPPERCASE by convention.
;
;       Internal Local Labels:
;               Lowercase by convention to visually distinguish from TERSE words.
;               [i.e.] max1, equal1, plus_bang1.
;
;******************************************************************************************
;   GORF v2 ROM CHECKSUMS
;   ---------------------
;
;   ROM File    CRC32       SHA1
;   -----------------------------------------------------------------------------------------
;   gorf-a.bin  5b348321    76e2e3ad1a66755f1a369167fdb157690fd44a52
;   gorf-b.bin  62d6de77    2601faf12d0ab4972c5535ffd722b03ecd8c097c
;   gorf-c.bin  1d3bc9c9    0b363a71d7585a4828e08668ebb2999c55e02721
;   gorf-d.bin  70046e56    392214cc6ed4155bfe022d36f0f86c2594a5ab57
;   gorf-e.bin  2d456eb5    720fb8b48e20c1fc281d8804259016c3c5364a07
;   gorf-f.bin  f7e4e155    9c9d6d3bfee6556dc7a01de81d6148dd02f04fc9
;   gorf-g.bin  4e2bd9b9    9edccceea5af015275582553ed238c40c73d8f4f
;   gorf-h.bin  fe7b863d    5aa8d824814ee1c30eaf0044da78d3aa8220dcaa
;
;******************************************************************************************
;
;   Sound events and score streams.
;   High-level event symbols point to compiled TERSE colon words.
;   Score symbols point to bytecode consumed by the native music interpreter. 
;   Several events submit two scores, one to each Astrocade music processor.
;
PLAYER_SHOT_SOUND         EQU     $2683   ; GORFOS 0186: 1D, primary BMUSIC
PLAYER_EXPLOSION_SCORE    EQU     $268C   ; GORFOS 0186: 1GSCORE
PLAYER_EXPLOSION_SOUND    EQU     $26F8   ; GORFOS 0186: 1G, primary then secondary
PZSCORE                   EQU     $26B3   ; GORFOS 0187: PZIP score
PZIP_SOUND                EQU     $270C   ; GORFOS 0187: PZ
ZPSCORE                   EQU     $26CF   ; GORFOS 0187: ZPIP score
ZPIP_SOUND                EQU     $2715   ; GORFOS 0187: ZP
TO1SCORE                  EQU     $271E   ; GORFOS 0188: takeoff primary score
TO2SCORE                  EQU     $2758   ; GORFOS 0188: takeoff secondary score
TAKEOFF_SOUND             EQU     $2792   ; GORFOS 0188: TO composite event

;   Mission 1: Astro Battles / Invaders.
THUMPSCORE                EQU     $8115   ; INVADERS 0113: background thump score
INVADER_THUMP_SOUND       EQU     $812E   ; INVADERS 0113: TH
IASCORE                   EQU     $8139   ; INVADERS 0114: large-invader score
LARGE_INVADER_SOUND       EQU     $8154   ; INVADERS 0114: IA

;   Mission 4: Space Warp.
SPSCORE                   EQU     $9F45   ; SPACE WARP 0110: ship-spiral score
SHIP_SPIRAL_SOUND         EQU     $9F5A   ; SPACE WARP 0110: SP
FBLSCORE                  EQU     $9F65   ; SPACE WARP 0110: fireblast score
FIREBLAST_SOUND           EQU     $9F7C   ; SPACE WARP 0110: FBL
ST1SCORE                  EQU     $9F87   ; SPACE WARP 0111: star-spiral primary score
ST2SCORE                  EQU     $9FBC   ; SPACE WARP 0111: star-spiral secondary score
STAR_SPIRAL_SOUND         EQU     $9FF1   ; SPACE WARP 0111: ST composite event

;   Mission 5: Flag Ship.
BSFSCORE                  EQU     $A9D7   ; FLAG SHIP 0111: background-ship score
BACKGROUND_SHIP_SOUND     EQU     $AA1F   ; FLAG SHIP 0111: BSF
SE1SCORE                  EQU     $AA28   ; FLAG SHIP 0112: ship-explosion primary score
SE2SCORE                  EQU     $AA61   ; FLAG SHIP 0112: ship-explosion secondary score
SHIP_EXPLOSION_SOUND      EQU     $AA6C   ; FLAG SHIP 0112: SE composite event
FBSCORE                   EQU     $AA7B   ; FLAG SHIP 0113: fireball score
FIREBALL_SOUND            EQU     $AA9F   ; FLAG SHIP 0113: FBS
SOSCORE                   EQU     $AAAA   ; FLAG SHIP 0113: ship-shotoff score
SHIP_SHOTOFF_SOUND        EQU     $AAD3   ; FLAG SHIP 0113: SO
BH1SCORE                  EQU     $AADE   ; FLAG SHIP 0114: black-hole primary score
BH2SCORE                  EQU     $AB2F   ; FLAG SHIP 0114: black-hole secondary score
BLACK_HOLE_SOUND          EQU     $AB80   ; FLAG SHIP 0114: BH composite event
;
;******************************************************************************************
;   COLD START of the game. This section just jumps over the RST $08
;   which hold _ENTER for the Terse engine.
;******************************************************************************************

            ORG     $0000               ; Genesis 1:1-5

COLDSTRT:   nop
            nop
            di
            jp      WARMSTRT
            nop
            nop

;******************************************************************************************
; ----> ENTER   Enters Terse mode via RST $08 ($CF).
;               Pushes the current processor state onto the IX (return/loop) stack,
;               saves the return address, and sets up execution.
;******************************************************************************************

_ENTER      EQU     $CF                 ; $CF is hex for RST $08.
                                        ; _ENTER makes code look better

            dec     ix
            ld      (ix+$00),b
            dec     ix
            ld      (ix+$00),c
            pop     bc
            DW      _DSPATCH

;******************************************************************************************
;   Hardware Reset code
;
;       Everything seems to get set to zero which in some case does not make sense
;       but I suppose that every port has to have something written to it or it is
;       in an unstable state. It would be much like setting variables to zero in code.
;
;******************************************************************************************

WARMSTRT:   di                          ; Warmstart the machine.
                                        ; Actual initialization begins here. COLDSTRT ($0000)
                                        ; jumps to this label to safely bypass the TERSE
                                        ; _ENTER vector located at $0008.

            im      0

            xor     a                   ; a=0

            out     ($00),a             ; Reset color Pallette ($00-$07)
            out     ($01),a
            out     ($02),a
            out     ($03),a
            out     ($04),a
            out     ($05),a
            out     ($06),a
            out     ($07),a

            out     ($09),a             ; No pallette split, background color 0
            out     ($0A),a             ; Vertical blank line register
            out     ($0B),a             ; Color block transfer
            out     ($0C),a             ; Magic RAM control
            out     ($0D),a             ; Interrupt vector
            out     ($0E),a             ; Interrupt Enable and Mode/Vertical Line Feedback
            out     ($0F),a             ; Interrupt Line/Horizontal Address Feedback

            inc     a                   ; a=1
            out     ($08),a             ; Set High resolution

            ; The following ports doesn't
            ; exist in Gorf hardware.

            ld      a,$F0
            out     ($F8),a
            ld      a,$00
            out     ($F9),a
            ld      a,$0F
            out     ($FA),a
            out     ($FB),a
            ld      a,$01
            out     ($FF),a

;******************************************************************************************
;   TERSE REGISTER USE:
;
;       BC = Instruction Pointer    (IP)
;       SP = Parmeter Stack Pointer (PSP)
;       IX = Return Stack Pointer   (RSP)
;       IY = Dispatcher             (DSPATCH)
;
;******************************************************************************************
;   Initalize the TERSE stack pointers
;******************************************************************************************

            ld      ix,RSP              ; Terse Return Stack Pointer
            ld      sp,PSP              ; Terse Parameter Stack Pointer
            ld      bc,ISP              ; Terse Instruction Pointer
            ld      iy,DSPATCH          ; Dispatcher

;******************************************************************************************
; ---->  DSPATCH    Fetches, decodes, and executes the
;                   next Terse instruction. Updates (BC) to
;                   point to the subsequent instruction.
;                   (Note: This is the engine's internal equivalent of the standard
;                   TERSE verb 'NEXT').
;
;   The _DSPATCH label is a special case. Unlike most Terse words, it directly holds the Z80
;   instruction JP (IY).  This disguise ensures that Terse instructions visually ends with a
;   Terse-like word, maintaining the language's aesthetic.
;
;******************************************************************************************

_DSPATCH    EQU     $E9FD               ; Replacement for JP (IY) to make
                                        ; TERSE instructions look better

DSPATCH:    ld      a,(bc)              ; Actual DSPATCH keyword code
            inc     bc
            ld      l,a
            ld      a,(bc)
            inc     bc
            ld      h,a
            jp      (hl)

;******************************************************************************************
; ----> RETURN      Exits the current Terse word, returns to dispatcher.
;                   (Note: This is the internal runtime execution routine for the ';' verb).
;******************************************************************************************

_RETURN:    ld      c,(ix+$00)
            inc     ix
            ld      b,(ix+$00)
            inc     ix
            DW      _DSPATCH

;******************************************************************************************
; ----> LIT         Compiled primitive to push next 16-bit word literal onto stack.
;******************************************************************************************

_LIT:       ld      a,(bc)
            inc     bc
            ld      l,a
            ld      a,(bc)
            inc     bc
            ld      h,a
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> LITbyte     Gets the byte at the next address (literal value) and pushes it
;                   to the parameter stack. (Custom engine optimization - Not in TERSE Vocabulary).
;******************************************************************************************

_LITbyte:   ld      a,(bc)
            inc     bc
            ld      l,a
            ld      h,$00
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> DLIT        Primitive compiled before 32-bit double literals. (Gets contents of
;                   the next four addresses and pushes them to the parameter stack).
;******************************************************************************************
_DLIT:      ld      a,(bc)
            inc     bc
            ld      l,a
            ld      a,(bc)
            inc     bc
            ld      h,a
            push    hl
            jp      _LIT

;******************************************************************************************
; ----> _BARRAY     Runtime execution behavior for BARRAY: Takes an index 'i' from the stack,
;                   retrieves the base address of the byte array from the instruction stream,
;                   and pushes the memory address of the i-th byte (base_address + i) to the stack.
;******************************************************************************************

_BARRAY:    pop     hl
calc_prep1: ld      a,(bc)
            inc     bc
            ld      e,a
            ld      a,(bc)
            inc     bc
            ld      d,a
            add     hl,de
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> _ARRAY      Takes an index 'i' from the stack, doubles it (since words are 2 bytes),
;                   and redirects execution to _BARRAY to calculate and push the memory
;                   address of the i-th word (base_address + (i * 2)) to the stack.
;******************************************************************************************

_ARRAY:     pop     hl
            add     hl,hl
            jp      calc_prep1

;******************************************************************************************
; ----> 0       Puts a 0 on the stack. (0 is a constant)
;******************************************************************************************

_0:         ld      hl,$0000
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> 1       Puts a 1 on the stack. (1 is a constant)
;******************************************************************************************

_1:         ld      hl,$0001
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> DUP     Duplicates the top value on the stack.
;******************************************************************************************

_DUP:       pop     hl
            push    hl
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> 2DUP    Duplicates the top two values on the stack.
;******************************************************************************************

_2DUP:      pop     hl
            pop     de
            push    de
            push    hl
            push    de
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> DROP    Removes the top value from the stack.
;******************************************************************************************

_DROP:      pop     hl
            DW      _DSPATCH

;******************************************************************************************
; ----> R>      Pops a 16-bit value from the return stack (pointed to by the ix register)
;               and pushes it onto the user stack. See >R.
;******************************************************************************************

_Rgt:       ld      l,(ix+$00)
            inc     ix
            ld      h,(ix+$00)
            inc     ix
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> >R      Push the value from the user stack and push it onto
;               the return stack. See R>.
;******************************************************************************************

_gtR:       pop     hl
            dec     ix
            ld      (ix+$00),h
            dec     ix
            ld      (ix+$00),l
            DW      _DSPATCH

;******************************************************************************************
; ----> SWAP    Exchanges the top two values on the stack.
;******************************************************************************************

_SWAP:      pop     hl
            pop     de
            push    hl
            push    de
            DW      _DSPATCH

;******************************************************************************************
; ----> 2SWAP   Swaps two pairs of values on the stack (e.g., double-precision numbers).
;******************************************************************************************

_2SWAP:     pop     hl
            pop     de
            exx
            pop     hl
            pop     de
            exx
            push    de
            push    hl
            exx
            push    de
            push    hl
            exx
            DW      _DSPATCH

;******************************************************************************************
; ----> SP@     Returns the memory address of the top of the user data stack.
;******************************************************************************************

_SPat:      ld      hl,$0000
            add     hl,sp
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> @       Returns the 16-bit word found at the specified memory address.
;******************************************************************************************

_at:        pop     hl
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            push    de
            DW      _DSPATCH

;******************************************************************************************
; ----> B@      Returns the 8-bit byte found at the specified memory address.
;               (The Z80 code pads the high byte with $00 before pushing).
;*****************************************************************************************

_Bat:       pop     hl
            ld      e,(hl)
            ld      d,$00
            push    de
            DW      _DSPATCH

;******************************************************************************************
; ----> !       Stores a 16-bit integer at the specified memory address.
;******************************************************************************************
_bang:      pop     hl
            pop     de
            ld      (hl),e
            inc     hl
            ld      (hl),d
            DW      _DSPATCH

;******************************************************************************************
; ----> B!      Store the least significant 8 bits of m at byte-address p. m p ---
;******************************************************************************************
_Bbang:     pop     hl
            pop     de
            ld      (hl),e
            DW      _DSPATCH

;******************************************************************************************
; ----> ZERO     Sets the 16-bit word at the specified memory location to 0.
;******************************************************************************************
_ZERO:      pop     hl
            ld      (hl),$00
            inc     hl
            ld      (hl),$00
            DW      _DSPATCH

;******************************************************************************************
; ----> +       Performs 16-bit integer addition of the top two stack values.
;******************************************************************************************
_plus:      pop     de
            pop     hl
            add     hl,de
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> -       Performs 16-bit integer subtraction (subtracts the top stack
;               value from the second stack value).
;******************************************************************************************
_minussign: pop     de
            pop     hl
minussign1: xor     a
            sbc     hl,de
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> MINUS   Negate a 16-bit number by taking its two's complement (0 - m).
;******************************************************************************************
_MINUS:     pop     de
minus_prep: ld      hl,$0000
            jp      minussign1

;******************************************************************************************
; ----> COM     One's complement (bitwise NOT). Calculated mathematically as 0 - (m + 1).
;******************************************************************************************
_COM:       pop     de
            inc     de
            jp      minus_prep

;******************************************************************************************
; ----> 1-    Decrement value on stack. q=m-1   m --- q
;******************************************************************************************
_1minus:    pop     hl
            dec     hl
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> 1+    Increment value on stack. q=m+1   q=m+1
;******************************************************************************************
_1plus:     pop     hl
            inc     hl
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> 2+    Value on stack +2. q=m+2   m --- q
;******************************************************************************************
_2plus:     pop     hl
            inc     hl
            inc     hl
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> 2-    Value on stack -2. q=m-2   m --- q
;******************************************************************************************
_2minus:    pop     hl
            dec     hl
            dec     hl
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> 2*    Value on stack x 2. q=m*2 (Shift left)   m --- q
;******************************************************************************************
_2splat:    pop     hl
            add     hl,hl
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> 2/    Value on stack /2  q=m/2 (Shift Right)   m --- q
;******************************************************************************************
_2slash:    pop     hl
            sra     h
            rr      l
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> ABS     Leave the absolute value of a number.
;******************************************************************************************
_ABS:       pop     hl
            bit     7,h
            jp      p,abs1
            ex      de,hl
            ld      hl,$0000
            xor     a
            sbc     hl,de
abs1:       push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> B@+7    (Not a standard TERSE word in the glossary)
;
;               In many retro games, inline text strings use bit 7
;               (the highest bit) as a hidden flag—often to mark the very last
;               character of a string so the engine knows when to stop reading
;               memory. This routine is a "fetch character and step" operator. It
;               pulls the character, advances the pointer to the next address,
;               and strips off that hidden high-bit flag so the character can be
;               printed normally.
;******************************************************************************************
_Bat_inc7:  pop     hl
            ld      a,(hl)
            inc     hl
            push    hl
            ld      l,a
            res     7,l
            ld      h,$00
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> 1-B!    Subtract 1 from the contents of the byte at location p.   p ---
;******************************************************************************************
_1minusBbang: pop   hl
            dec     (hl)
            DW      _DSPATCH

;******************************************************************************************
; ----> 1+B!    Add 1 to the contents of the byte at location p.   p ---
;******************************************************************************************
_1plusBbang: pop    hl
            inc     (hl)
            DW      _DSPATCH

;******************************************************************************************
; ----> =        If m = n then push a 1 otherwise push a 0. (m n --- f True if m=n)
;******************************************************************************************
_equal:     pop     de
equal2:     pop     hl
            xor     a
            sbc     hl,de
            ld      hl,$0000
            jp      nz,equal1
            inc     hl
equal1:     push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> <>      If m <> n then push a 1, otherwise push a 0. (m n --- f True if m <> n)
;               (THIS IS NOT IN THE TERSE GLOSSARY)
;******************************************************************************************
_not_equal: pop     de
not_equal2: pop     hl
            xor     a
            sbc     hl,de
            ld      hl,$0000
            jp      z,not_equal1
            inc     hl
not_equal1: push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> U<          True if unsigned m < n. (Evaluated mathematically by subtracting
;                   n from m. If a borrow occurs, m is less than n, so it pushes 1.
;                   Otherwise it pushes 0).
;******************************************************************************************
_Uless:     pop     de
            pop     hl
gt2:        xor     a
            sbc     hl,de
            ld      hl,$0000
            jp      nc,gt1
            inc     hl
gt1:        push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> U>=         True if unsigned m >= n. (Evaluated mathematically by subtracting
;                   n from m. If no borrow occurs, m is greater than or equal to n,
;                   so it pushes 1. Otherwise it pushes 0).
;******************************************************************************************
_Ugt_equal: pop     de
            pop     hl
lt2:        xor     a
            sbc     hl,de
            ld      hl,$0000
            jp      c,lt1
            inc     hl
lt1:        push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> <           True if signed m < n. (Evaluated mathematically by subtracting
;                   n from m and checking the sign and overflow flags. Pushes 1 if
;                   true, 0 if false).
;******************************************************************************************
_less:      pop     de
gt_equal3:  pop     hl
gt_equal2:  xor     a
            sbc     hl,de
            ld      de,$0000
            push    af
            pop     hl
            ld      a,l
            and     $84
            jp      pe,gt_equal1
            inc     de
gt_equal1:  push    de
            DW      _DSPATCH

;******************************************************************************************
; ----> >=          True if signed m >= n. (Evaluated mathematically by subtracting
;                   n from m and checking the sign and overflow flags. Pushes 1 if
;                   true, 0 if false).
;******************************************************************************************
_gt_equal:  pop     de
lt_equal3:  pop     hl
lt_equal2:  xor     a
            sbc     hl,de
            ld      de,$0000
            push    af
            pop     hl
            ld      a,l
            and     $84
            jp      po,lt_equal1
            inc     de
lt_equal1:  push    de
            DW      _DSPATCH

;******************************************************************************************
; ----> U>          True if unsigned m > n. (Evaluated by popping the values in
;                   reverse order (n, then m) and jumping to the unsigned < routine).
;******************************************************************************************
_Ugt:       pop     hl
            pop     de
            jp      gt2

;******************************************************************************************
; ----> U<=         True if unsigned m <= n. (Evaluated by popping the values in
;                   reverse order (n, then m) and jumping to the unsigned >= routine).
;******************************************************************************************
_Uless_equal:   pop    hl
                pop     de
                jp      lt2

;******************************************************************************************
; ----> >           True if signed m > n. (Evaluated by popping the values in
;                   reverse order (n, then m) and jumping to the signed < routine).
;******************************************************************************************
_gt:        pop     hl
L01DB:      pop     de
            jp      gt_equal2

;******************************************************************************************
; ----> <=          True if signed m <= n. (Evaluated by popping the values in
;                   reverse order (n, then m) and jumping to the signed >= routine).
;******************************************************************************************
_less_equal:    pop hl
L01E0:          pop     de
                jp      lt_equal2

;******************************************************************************************
; ----> 0=      True if m is zero.      m --- f
;******************************************************************************************
_zeroequal: ld  de,$0000
            jp      equal2

;******************************************************************************************
; ----> _0notequal  True if m is not equal to zero.   m --- f
;******************************************************************************************
_0notequal: ld      de,$0000
            jp      not_equal2

;******************************************************************************************
; ----> 0<      True if m is negative.
;******************************************************************************************
_0less:     ld      de,$0000
            jp      gt_equal3

;******************************************************************************************
; ----> 0>=     True if m is positive or equal to 0.
;******************************************************************************************
_0gtequal:  ld      de,$0000
            jp      lt_equal3

;******************************************************************************************
; ----> 0>      True if m is positive and non-zero.
;******************************************************************************************

_0gt:       ld      hl,$0000
            jp      L01DB

;******************************************************************************************
; ----> 0<=     True if m is negative or equal to 0.
;******************************************************************************************

_0lessequal:    ld  hl,$0000
                jp      L01E0

;******************************************************************************************
; ----> NOT     Logical NOT. Equivalent to 0=
;******************************************************************************************
_NOT:       jp      _zeroequal

;******************************************************************************************
; ----> -DUP    Duplicate the top value on the stack if it is not zero.   m --- m m
;******************************************************************************************
_minusDUP:  pop     hl
            ld      a,l
            or      h
            jp      z,minusDUP1         ; Replaced $0212
            push    hl
minusDUP1:  push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> AND     Bitwise logical AND.   m n --- q
;******************************************************************************************
_AND:       pop     de
            pop     hl
            ld      a,l
            and     e
            ld      l,a
            ld      a,h
            and     d
            ld      h,a
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> OR
;******************************************************************************************
_OR         EQU     $
            pop     de
            pop     hl
            ld      a,l
            or      e
            ld      l,a
            ld      a,h
            or      d
            ld      h,a
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> XOR
;
;******************************************************************************************
_XOR        EQU     $
            pop     de
            pop     hl
            ld      a,l
            xor     e
            ld      l,a
            ld      a,h
            xor     d
            ld      h,a
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> BMOVE       Move the n bytes starting at byte-address p into the n
;                    byte-cells starting at byte-address q. The contents of p is
;                    moved first. p q n ---
;******************************************************************************************
_BMOVE:     exx
            pop     bc
            pop     de
            pop     hl
            ld      a,b
            or      c
            jp      z,bmove1
            ldir
bmove1:     exx
            DW      _DSPATCH

;******************************************************************************************
; ----> DO
;******************************************************************************************
_DO         EQU     $
            ld      de,$FFFA
            add     ix,de
            pop     hl
            pop     de
            ld      (ix+$00),l
            ld      (ix+$01),h
            ld      (ix+$02),e
            ld      (ix+$03),d
            ld      (ix+$04),c
            ld      (ix+$05),b
            DW      _DSPATCH

;******************************************************************************************
; ----> LOOP    Increment the DO-loop index by one, terminating the loop if
;               the new index is equal to or greater than the limit.
;******************************************************************************************
_LOOP:      ld        l,(ix+$00)
            ld        h,(ix+$01)
            inc       hl
            ld        (ix+$00),l
            ld        (ix+$01),h
            ld        e,(ix+$02)
            ld        d,(ix+$03)
            xor       a
            sbc       hl,de
            push      af
            pop       hl
            ld        a,l
            and       $84
            jp        po,loop_continue    ; Was $0285
            ld        de,$0006
            add       ix,de
            jp        loop_end            ; Was $028B

loop_continue:
            ld        c,(ix+$04)
            ld        b,(ix+$05)

loop_end:   DW        _DSPATCH

;******************************************************************************************
; ----> I       Returns the index of the innermost DO-loop.   --- m
;******************************************************************************************
_I:         ld      l,(ix+$00)
            ld      h,(ix+$01)
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> J       Returns the index of the next outer DO-loop.   --- m
;******************************************************************************************
_J:         ld      l,(ix+$06)
            ld      h,(ix+$07)
            push    hl
            DW      _DSPATCH
;******************************************************************************************
; ----> K       Returns the index of the second outer DO-loop.   --- m
;******************************************************************************************
_K:         ld      l,(ix+$0c)
            ld      h,(ix+$0d)
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> I+      Adds m to the index of the innermost DO-loop.   m --- q
;******************************************************************************************
_Iplus:     ld      l,(ix+$00)
            ld      h,(ix+$01)
            pop     de
            add     hl,de
            push    hl
            DW      _DSPATCH
;******************************************************************************************
; ----> J+      Adds m to the index of the next outer DO-loop.
;******************************************************************************************
_Jplus:     ld      l,(ix+$06)
            ld      h,(ix+$07)
            pop     de
            add     hl,de
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> K+      Adds m to the index of the second outer DO-loop.
;******************************************************************************************
_Kplus:     ld      l,(ix+$0c)
            ld      h,(ix+$0d)
            pop     de
            add     hl,de
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> OVER    Push the second stack value to the top.   m n --- m n m
;******************************************************************************************
_OVER:      pop     hl
            pop     de
            push    de
            push    hl
            push    de
            DW      _DSPATCH

;******************************************************************************************
; ----> +!      Add integer m to value at address p.   m p ---
;******************************************************************************************
_plusbang:  pop     hl
            pop     de
plus_bang1: ld      a,(hl)
            add     a,e
            ld      (hl),a
            inc     hl
            ld      a,(hl)
            adc     a,d
            ld      (hl),a
            DW      _DSPATCH

;******************************************************************************************
; ----> 1+!     Add 1 to the contents of location p.   p ---
;******************************************************************************************
_1plusbang: ld      de, $0001
            pop     hl
            jp      plus_bang1          ; Replaced $02D2

;******************************************************************************************
; ----> 1-!     Subtract 1 from the contents of location P.   p ---
;******************************************************************************************
_1minusbang:    ld  de, $FFFF
            pop     hl
            jp      plus_bang1          ; Replaced $02D2

;******************************************************************************************
; ----> SWAB    Exchange the high and low-order bytes of a value.   m --- q
;******************************************************************************************
_SWAB:      pop     hl
            ld      e,l
            ld      l,h
            ld      h,e
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> OUTP    Outputs byte-value m to output port n.
;               Note: The 16-bit port address (n) is split in two. The lower
;               half tells the system which hardware port to talk to. The upper
;               half is broadcasted on the upper address wires at the exact
;               same time. Hardware can use this upper half as a "sub-port"
;               (like an apartment number) for extra instructions.
;******************************************************************************************
_OUTP:      exx
            pop     bc
            pop     hl
            out     (c),l
            exx
            DW      _DSPATCH

;******************************************************************************************
; ----> INP     Inputs from port m returning value n.
;               Just like OUTP, when the Z80 reads from the port in
;               register C, it simultaneously broadcasts the value of register
;               B on the upper address wires. Hardware can use this B value as
;               a "sub-port" to know exactly which data it should send back.
;******************************************************************************************
_INP        EQU     $
            exx
            pop     bc
            in      l,(c)
            ld      h,$00
            push    hl
            exx
            DW      _DSPATCH

;******************************************************************************************
; ----> ROT
;******************************************************************************************
_ROT        EQU     $
            pop     de
            pop     hl
            ex      (sp),hl
            push    de
            push    hl
            DW      _DSPATCH

;******************************************************************************************
; ----> PICK    Return the nth value on the stack.   n --- q
;******************************************************************************************
_PICK:      pop     hl
            dec     hl
            add     hl,hl
            add     hl,sp
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            push    de
            DW      _DSPATCH

;******************************************************************************************
; ----> 2DROP   Drop the top two values from the stack.
;******************************************************************************************
_2DROP:     pop     hl
            pop     hl
            DW      _DSPATCH

;******************************************************************************************
; ----> *       16-bit signed multiply.
;******************************************************************************************
_star:      exx
            pop     de
            pop     bc
            ld      hl,$0000
star1:      srl     b               ; Was $031D
            rr      c
            jp      nc,star2        ; Was $0325
            add     hl,de
star2:      ld      a,b             ; Was $0325
            or      c
            jp      z,star_end      ; Was $0331
            sla     e
            rl      d
            jp      star1           ; Was $031D
star_end:   push    hl              ; Was $0331
            exx
            DW      _DSPATCH

;******************************************************************************************
; ----> /MOD    16-bit integer divide, m/n. The quotient is left on top of
;               the stack, the remainder beneath. The remainder has the sign
;               of the quotient, q.
;******************************************************************************************
_slashMOD:  exx
            pop     bc
            pop     hl
            ld      a,b
            xor     h
            push    af
            xor     h
            bit     7,a
            jp      z,div1          ; Was $0348
            ld      a,b
            cpl
            ld      b,a
            ld      a,c
            cpl
            ld      c,a
            inc     bc
div1:       bit     7,h             ; Was $0348
            jp      z,div2          ; Was $0354
            ld      a,h
            cpl
            ld      h,a
            ld      a,l
            cpl
            ld      l,a
            inc     hl
div2:       ld      a,$01           ; Was $0354
            ld      de,$0000
div3:       bit     7,b             ; Was $0359
            jp      nz,div_loop     ; Was $0366
            sla     c
            rl      b
            inc     a
            jp      div3            ; Was $0359

div_loop:   or      a               ; Was $0366
            sbc     hl,bc
            jp      nc,div4         ; Was $036E
            add     hl,bc
            dec     de
div4:       inc     de              ; Was $036E
            dec     a
            jp      z,div_end       ; Was $037E
            sla     e
            rl      d
            srl     b
            rr      c
            jp      div_loop        ; Was $0366

div_end:    pop     af              ; Was $037E
            bit     7,a
            jp      z,div_exit      ; Was $0392
            ld      a,h
            cpl
            ld      h,a
            ld      a,l
            cpl
            ld      l,a
            inc     hl
            ld      a,d
            cpl
            ld      d,a
            ld      a,e
            cpl
            ld      e,a
            inc     de
div_exit:   push    hl              ; Was $0392
            push    de
            exx
            DW      _DSPATCH

;******************************************************************************************
; ----> +LOOP   Add m to the loop index. Exit from the loop is made when the
;               resultant index reaches or passes the limit.
;******************************************************************************************
_plusLOOP:  pop     hl
            ld      a,h
            ld      e,(ix+$00)
            ld      d,(ix+$01)
            add     hl,de
            ld      (ix+$00),l
            ld      (ix+$01),h
            ld      e,(ix+$02)
            ld      d,(ix+$03)
            or      a
            sbc     hl,de
            push    af
            pop     hl
            bit     7,a
            ld      a,l
            jp      z,ploop_check   ; Was $03CD
            and     $84
            jp      pe,ploop_cont1  ; Was $03C4
            ld      de,$0006
            add     ix,de
            jp      ploop_exit1     ; Was $03CA

ploop_cont1: ld     c,(ix+$04)      ; Was $03C4
            ld      b,(ix+$05)
ploop_exit1: jp     ploop_end       ; Was $03CA

ploop_check: and    $84             ; Was $03CD
            jp      po,ploop_cont2  ; Was $03DA
            ld      de,$0006
            add     ix,de
            jp      ploop_end       ; Was $03E0

ploop_cont2: ld     c,(ix+$04)      ; Was $03DA
            ld      b,(ix+$05)
ploop_end:  DW      _DSPATCH        ; Was $03E0

;******************************************************************************************
; ----> BRANCH      Unconditional branch.
;******************************************************************************************
_BRANCH:    ld      a,(bc)
            ld      e,a
            inc     bc
            ld      a,(bc)
            ld      b,a
            ld      c,e
            DW      _DSPATCH

;******************************************************************************************
; ----> 0BRANCH Conditional branch-if-false
;******************************************************************************************
_0BRANCH:   pop     hl
            ld      a,h
            or      l
            jp      z,branch_true
            inc     bc
            inc     bc
            DW      _DSPATCH

;******************************************************************************************
; Target for Conditional Branch-If-False (_0BRANCH)
;******************************************************************************************
branch_true:    jp  _BRANCH

;******************************************************************************************
; ----> LEAVE   Force termination of a DO-loop.   ---
;******************************************************************************************
_LEAVE:     ld      a,(ix+$00)
            ld      (ix+$02),a
            ld      a,(ix+$01)
            ld      (ix+$03),a
            DW      _DSPATCH

;******************************************************************************************
; ----> A"      Makes a string similarly to ." but does not type it. Instead
;               A" returns the string address q.
;******************************************************************************************
_Aquote:    push    bc
            ld      a,(bc)
            inc     bc
            ld      l,a
            ld      h,$00
            add     hl,bc
            ld      b,h
            ld      c,l
            DW      _DSPATCH

;=========================================================================================
;====> End of the Core Foundational dictionary (i.e., Shifts from raw Z80 to TERSE words)
;=========================================================================================


;******************************************************************************************
; ----> MAX     Leave the greater of the two numbers.
;******************************************************************************************

_MAX:       DB      _ENTER
            DW      _2DUP
            DW      _less               ; Replaced $01A8
            DW      _0BRANCH            ; Conditional branch-if-false
            DW      max1                ; Replaced $041B
            DW      _SWAP
max1:       DW      _DROP               ; Was $041B
            DW      _RETURN

;******************************************************************************************
; ----> MOVE    Move n 16-bit words from p to q.   p q n ---
;******************************************************************************************
_MOVE:      DB      _ENTER
            DW      _2splat
            DW      _BMOVE
            DW      _RETURN

;******************************************************************************************
; ----> MIN     Leave the lesser of the two numbers.   m n --- p
;******************************************************************************************
_MIN:       DB      _ENTER
            DW      _2DUP
            DW      _gt                 ; Replaced $01DA
            DW      _0BRANCH            ; Conditional branch-if-false
            DW      min1                ; Replaced $0431
            DW      _SWAP
min1:       DW      _DROP               ; Was $0431
            DW      _RETURN

;******************************************************************************************
; ----> NAND    Logical AND followed by COMPLEMENT.   m n --- q
;******************************************************************************************
_NAND:      DB      _ENTER
            DW      _AND                ; Replaced $0215
            DW      _COM
            DW      _RETURN

;******************************************************************************************
; ----> NOR     Logical OR followed by COMPLEMENT.   m n --- q
;******************************************************************************************
_NOR:       DB      _ENTER
            DW      _OR
            DW      _COM
            DW      _RETURN

;******************************************************************************************
; ----> MOD     Leave the remainder of m/n.   m n --- r
;******************************************************************************************
_MOD:       DB      _ENTER
            DW      _slashMOD           ; Replaced $0335
            DW      _DROP
            DW      _RETURN

;******************************************************************************************
; ----> /       Divide m by n, leaving quotient q.   m n --- q
;******************************************************************************************
_slash:     DB      _ENTER
            DW      _slashMOD           ; Replaced $0335
            DW      _SWAP
            DW      _DROP
            DW      _RETURN

;******************************************************************************************
; ----> OF      Case statement branch. Compares the top two items on the stack.
;               If they match, both are dropped and execution continues.
;               If they don't match, the top item is dropped, the second item
;               is kept on the stack, and execution branches to the next case.
;******************************************************************************************
_OF:        pop     hl
            pop     de
            or      a
            sbc     hl,de
            jp      nz,of_false
            inc     bc
            inc     bc
            DW      _DSPATCH

of_false:   ld      a,(bc)
            ld      l,a
            inc     bc
            ld      a,(bc)
            ld      b,a
            ld      c,l
            push    de
            DW      _DSPATCH

;******************************************************************************************
; ----> CASES   Positional execution jump table. Takes an index from the stack.
;               The table of execution addresses follows the call inline.
;               The first inline 16-bit value is the address of the end of the table.
;               If the index is within bounds, it executes the corresponding routine.
;               If out of bounds, it falls through to the end of the table.
;******************************************************************************************
_CASES:     ld      a,(bc)
            ld      e,a
            inc     bc
            ld      a,(bc)
            ld      d,a
            inc     bc
            pop     hl
            sla     l
            rl      h
            add     hl,bc
            ld      b,d
            ld      c,e
            ex      de,hl
            or      a
            sbc     hl,de
            jp      c,case_end
            jp      z,case_end
            ld      a,(de)
            ld      l,a
            inc     de
            ld      a,(de)
            ld      h,a
            jp      (hl)
case_end:   DW      _DSPATCH

;******************************************************************************************
; ----> SCAN    Searches a memory block for a specific byte.
;               Takes the address, length, and target byte from the stack.
;               If found, leaves the address of the byte and a true flag (1).
;               If not found, leaves a false flag (0).
;******************************************************************************************
_SCAN:      ld      d,b
            ld      e,c
            pop     hl
            ld      a,l
            pop     bc
            pop     hl
            cpir
            jp      nz,scan_false
            dec     hl
            push    hl
            ld      hl,$0001
            jp      scan_end
scan_false: ld      hl,$0000
scan_end:   push    hl
            ld      b,d
            ld      c,e
            DW      _DSPATCH

;##########################################################################################
;    { BLOCK 0030 }
;    0F BA= HISCR2 : HS2 0 HISCR2 ;
;##########################################################################################
;******************************************************************************************
; ----> HS2     Leaves the address of the 0th byte of the HISCR2 array on the stack.
;******************************************************************************************
_HS2:       DB      _ENTER
            DW      _0
            DW      _BARRAY             ; Was $0088
            DW      HISCR2              ; HISCR2 RAM vector ($D010)
            DW      _RETURN

;##########################################################################################
;    { BLOCK 0030 }
;    0F BA= HISCR4 : HS4 0 HISCR4 ;
;##########################################################################################

;******************************************************************************************
; ----> HS4     Leaves the address of the 0th byte of the HISCR4 array on the stack.
;******************************************************************************************
_HS4:       DB      _ENTER
            DW      _0
            DW      _BARRAY             ; Was $0088
            DW      HISCR4              ; HISCR4 RAM vector ($D023)
            DW      _RETURN

;##########################################################################################
;    { BLOCK 0031 }
;    4 BARRAY vqhead : NILVQ 0 0 vqhead ! 0 2 vqhead ! ;
;##########################################################################################
;******************************************************************************************
; ----> NILVQ   Clears the four bytes of the vqhead array ($D08C to $D08F) by storing
;               a 16-bit 0 at base + 0 and another 16-bit 0 at base + 2.
;******************************************************************************************
_NILVQ:     DB      _ENTER
            DW      _0
            DW      _0
            DW      _BARRAY             ; Was $0088
            DW      $D08C               ; vqhead RAM vector
            DW      _bang
            DW      _0
            DW      _LITbyte
            DB      $02
            DW      _BARRAY             ; Was $0088
            DW      $D08C               ; vqhead RAM vector
            DW      _bang
            DW      _RETURN

;##########################################################################################
;    { BLOCK 0031 }
;    ( RAM ORIGIN AND SPECIAL VGS VERBS )
;    CODE DI DI, NEXT ( disable interrupts )
;##########################################################################################
;******************************************************************************************
; ----> DI      Disable interrupts.
;******************************************************************************************
_DI         EQU     $
            di
            DW      _DSPATCH

;##########################################################################################
;    { BLOCK 0031 }
;    CODE EI EI, NEXT ( enable interrupts )
;##########################################################################################
;******************************************************************************************
; ----> EI      Enable interrupts.
;******************************************************************************************
_EI:        ei
            DW      _DSPATCH

;##########################################################################################
;    { BLOCK 0031 }
;    CODE XDI DI, A XRA, INMOD OUT, NEXT
;##########################################################################################
;******************************************************************************************
; ----> XDI     Disable interrupts and clear INMOD port ($0E).
;******************************************************************************************
_XDI:       di
            xor     a
            out     ($0E),a             ;INMOD
            DW      _DSPATCH

;##########################################################################################
;    { BLOCK 0031 }
;    : MS 0 DO 4 0 DO LOOP LOOP ;
;##########################################################################################
;******************************************************************************************
; ----> MS      Delay loop. Expects a delay value (n) on the stack. The outer
;               DO-loop runs n times, and an inner DO-loop runs 4 times per
;               outer iteration.   n ---
;******************************************************************************************
_MS:        DB      _ENTER
            DW      _0
            DW      _DO
            DW      _LITbyte
            DB      $04
            DW      _0
            DW      _DO
            DW      _LOOP
            DW      _LOOP
            DW      _RETURN

;##########################################################################################
;    { BLOCK 0034 }
;       ( VGS                 random, ranger )
;
;       2 A= RND#      ( you must seed RND# !!!!!!!!! )
;
;       SUBR random    ( 32 bit random # generator )
;            ( out- randomly selected # in DEHL )
;
;    B PUSH, 0 RND# LBCD, 1321 H LXI, B DAD, H PUSH,
;    2776 H LXI, B DADC, 1 RND# LDED, D DAD, XTHL,
;    B DAD, XTHL, D DADC, XTHL, B DAD, XTHL, D DADC, XTHL,
;    E D MOV, B E MOV, C B MOV, 0 C MVI, B DAD, 0 RND# SHLD,
;    XTHL, D DADC, 1 RND# SHLD, D POP, B POP, RET,
;
;##########################################################################################

;******************************************************************************************
; ----> random      32-bit random number generator.
;                   Implements a custom Linear Congruential Generator.
;
; Out:   DEHL ---   32-bit randomly selected number
;******************************************************************************************
random:     push    bc                  ; Save BC (TERSE Instruction Pointer)
            ld      bc,(RND_SEED)       ; Load lower 16 bits of RND# (RND#0)
            ld      hl,$1321            ; Load lower 16 bits of constant
            add     hl,bc               ; Add to RND#0
            push    hl                  ; Save intermediate lower result on stack
            ld      hl,$2776            ; Load upper 16 bits of constant
            adc     hl,bc               ; Add RND#0 + carry to upper constant
            ld      de,($D0AD)          ; Load upper 16 bits of RND# (RND#1)
            add     hl,de               ; Add RND#1 to intermediate upper result
            ex      (sp),hl             ; Swap: HL = lower result, Stack = upper result
            add     hl,bc               ; Add RND#0 to lower result
            ex      (sp),hl             ; Swap back
            adc     hl,de               ; Add RND#1 + carry to upper result
            ex      (sp),hl             ; Swap: HL = lower result, Stack = upper result
            add     hl,bc               ; Add RND#0 to lower result
            ex      (sp),hl             ; Swap back
            adc     hl,de               ; Add RND#1 + carry to upper result
            ex      (sp),hl             ; Swap: HL = lower result, Stack = upper result
            ld      d,e                 ; \
            ld      e,b                 ; |
            ld      b,c                 ; | Shift intermediate lower result values
            ld      c,$00               ; /
            add     hl,bc               ; Add shifted value to lower result
            ld      (RND_SEED),hl       ; Save new lower 16 bits to RND_SEED (RND#0)
            ex      (sp),hl             ; Retrieve upper result from stack
            adc     hl,de               ; Add shifted value + carry to upper result
            ld      ($D0AD),hl          ; Save new upper 16 bits to $D0AD (RND#1)
            pop     de                  ; Pop the lower 16 bits into DE
            pop     bc                  ; Restore BC (TERSE Instruction Pointer)
            ret                         ; Return with random number in DEHL

;##########################################################################################
;    SUBR ranger ( pass # in HL, range in DE, )
;
;    B PUSH, EXX, 0 H LXI, H D MOV, L E MOV, EXX, D PUSH, B POP, XCHG,
;    0 H LXI, BEGIN, B SRLR, C RARR, CY, IF, D DAD, EXX, D DADC,
;    EXX, THEN, B A MOV, C ORA, <>, IF, E SLAR, D RALR, EXX, E RALR,
;    D RALR, EXX, { SWAP } JMP, THEN, EXX, B POP, RET, .ABS
;##########################################################################################

;******************************************************************************************
; ----> ranger      Scales a random number by a given range.
;                   Effectively computes (HL * DE) >> 16 using a shift-and-add
;                   multiplier algorithm.
;
;                   NOTE: This routine pulls a brilliant optimization trick! It
;                   accumulates the upper 16 bits of the result in the alternate
;                   HL' register. Instead of moving HL' to HL at the end, it
;                   simply leaves the CPU swapped in the alternate register set
;                   permanently! It balances the TERSE Instruction Pointer by
;                   pushing the normal BC at the start and popping it into the
;                   alternate BC' at the end.
;
; In:    HL   ---   Random number
;        DE   ---   Range
; Out:   HL   ---   Scaled random number (0 to Range-1)
;******************************************************************************************
ranger:     push    bc                  ; Save normal BC (TERSE Instruction Pointer)
            exx                         ; Switch to alternate register set
            ld      hl,$0000            ; Clear HL' (Upper 16 bits of accumulator)
            ld      d,h                 ; Clear D'
            ld      e,l                 ; Clear E'
            exx                         ; Switch back to normal register set
            push    de                  ; \
            pop     bc                  ; / Move range from DE to BC
            ex      de,hl               ; Move random number from HL to DE
            ld      hl,$0000            ; Clear HL (Lower 16 bits of accumulator)
ranger_loop:srl     b                   ; \
            rr      c                   ; / Shift range (BC) right by 1
            jr      nc,ranger_skip      ; If carry is clear, skip the addition
            add     hl,de               ; Add random number to lower accumulator
            exx                         ; Switch to alternate register set
            adc     hl,de               ; Add random number + carry to upper accumulator
            exx                         ; Switch back to normal register set
ranger_skip:ld      a,b                 ; \
            or      c                   ; | Check if range (BC) is 0
            jr      z,ranger_end        ; / If 0, multiplication is complete
            sla     e                   ; \
            rl      d                   ; |
            exx                         ; | Shift random number (DE and DE') left by 1
            rl      e                   ; |
            rl      d                   ; |
            exx                         ; /
            jp      ranger_loop         ; Loop until range is 0
ranger_end: exx                         ; Switch to alternate set (HL' is the result)
            pop     bc                  ; Pop original IP into alternate BC'
            ret                         ; Return (CPU remains in alternate set!)

;########################################################################################
;    { BLOCK 0035 }
;    ( VGS                 random, RND, RANGER )
;
;    SUBR rnd ( pass range in DE, returns # in HL )
;     D PUSH, random CALL, D POP, ranger CALL, RET,
;########################################################################################
;******************************************************************************************
; ----> rnd     Subroutine: Generates a random number within a given range.
;
; In:    DE   ---   Range
; Out:   HL   ---   Random number (0 to Range-1)
;******************************************************************************************
rnd:        push    de                  ; SUBROUTINE
            call    random
            pop     de
            call    ranger
            ret

;########################################################################################
;    CODE RANDOM ( out= # on stack ) random CALL, H PUSH, NEXT
;########################################################################################
;******************************************************************************************
; ----> RANDOM  Pushes a 16-bit random number onto the parameter stack.
;******************************************************************************************
_RANDOM:    call    random
            push    hl
            DW      _DSPATCH

;########################################################################################
;    CODE RND ( pass range on stack )
;     random CALL, D POP, ranger CALL, H PUSH, NEXT
;########################################################################################
;******************************************************************************************
; ----> RND     Pops a range from the stack and pushes a random number
;               scaled to that range.
;******************************************************************************************
_RND:       call    random
            pop     de
            call    ranger
            push    hl
            DW      _DSPATCH

;########################################################################################
;    CODE UNEQRND .REL
;    ( RANDOM NUMBER ROUTINE DESIGNED NOT TO REPEAT SAME # TWICE )
;    ( TRACKBYTE RANGE UNEQRND --- NEWNUM )
;    RANDOM CALL, D POP, D PUSH, ranger CALL, L A MOV,
;    D POP, H POP, M CMP, 0=, IF, A INR, E CMP, 0=, IF, A XRA,
;    THEN, THEN, A E MOV, 0 D MVI, E M MOV, D PUSH, .ABS NEXT
;########################################################################################
;******************************************************************************************
; ----> UNEQRND Generates a random number within a range that is guaranteed
;               not to repeat the previous number (stored at TRACKBYTE).
;               If the new number equals the old, it increments it. If the
;               increment hits the range limit, it wraps to 0.
;
; In:   (p = TRACKBYTE addr, n = RANGE, m = NEWNUM)
;******************************************************************************************
_UNEQRND:   call    random
            pop     de
            push    de
            call    ranger
            ld      a,l
            pop     de
            pop     hl
            cp      (hl)
            jr      nz,uneqrnd_store    ; IF it doesn't match the old number, skip ahead
            inc     a                   ; It matched! Increment to make it different
            cp      e                   ; Did it hit the range limit?
            jr      nz,uneqrnd_store    ; No, skip ahead
            xor     a                   ; Yes, wrap around to 0
uneqrnd_store:
            ld      e,a                 ; THEN, THEN... move new number to DE
            ld      d,$00
            ld      (hl),e              ; Store new number in TRACKBYTE
            push    de                  ; Push new number to stack
            DW      _DSPATCH

;########################################################################################
; { BLOCK 0036 }
;  ( VGS                          COLOR AND FLOOD )
;  ( UPDATE COLOR MAP -- QUICKLY )
;
;  CODE COLOR EXX, H POP, 800 B LXI,
;  BEGIN, M A MOV, A OUTP, H INX, C INR, LOOP,
;  EXX, NEXT
;########################################################################################
;******************************************************************************************
; ----> COLOR   Quickly updates the 8 hardware color palette registers ($00-$07).
;               Expects the base address of an 8-byte color array.
;******************************************************************************************
_COLOR:     exx
            pop     hl
            ld      bc,$0800            ; B = 8 (Loop count), C = $00 (Start port)
color1:     ld      a,(hl)
            out     (c),a               ; Output color to port
            inc     hl                  ; Next color byte
            inc     c                   ; Next port
            djnz    color1
            exx
            DW      _DSPATCH

;########################################################################################
;  CODE FLOOD   ( set all color ports to the same value )
;    ( in- byte color value )
;    ( out- screen color ports set to same value )
;  EXX, H POP, L A MOV, 800 B LXI, BEGIN, A OUTP, C INR, LOOP,
;  EXX, NEXT
;########################################################################################
;******************************************************************************************
; ----> FLOOD   Sets all hardware color ports to the same specified byte value.
;               The loop executes 8 times to cover the 8 color registers.
;
; In:    Color value (byte)
; Out:   Screen color ports set to the same value
;******************************************************************************************
_FLOOD:     exx
            pop     hl
            ld      a,l
            ld      bc,$0800
FLOOD1:     out     (c),a
            inc     c
            djnz    FLOOD1
            exx
            DW      _DSPATCH

;########################################################################################
;  : FILL   ( fill screen whith constant data )
;    ( in- constant , starting address , # of bytes to fill )
;    ( out- does sequential fill whith constant specified )
;
;    ROT ROT 2DUP ! SWAP DROP DUP 1+ ROT 1- BMOVE ;
;########################################################################################
;******************************************************************************************
; ----> FILL    Fills a sequential block of memory (usually the screen) with
;               a constant data value using the BMOVE operator.
;
; In:    Constant data value
;        Starting memory address
;        Number of bytes to fill
; Out:   Memory filled sequentially with the specified constant
;******************************************************************************************
_FILL:      DB      _ENTER
            DW      _ROT
            DW      _ROT
            DW      _2DUP
            DW      _bang
            DW      _SWAP
            DW      _DROP
            DW      _DUP
            DW      _1plus
            DW      _ROT
            DW      _1minus
            DW      _BMOVE
            DW      _RETURN

;##########################################################################################
;    { BLOCK 0038 }
;    ( RELABS ) F= NULRET F= norrel
;
;    SUBR ffnorrel <ASSEMBLE
;    7 C BIT, 0<>, IF, 1 Y A LDX, H ADD, A DCR, A H MOV,
;    THEN, 6 C BIT, 0<>, IF, 0 Y A LDX, D ADD, A DCR,
;    A D MOV, THEN, ( FALL INTO NORREL )
;##########################################################################################
;******************************************************************************************
; ----> ffnorrel    Adjusts X and Y coordinates based on pattern dimensions if
;                   the hardware Flip (bit 7) or Flop (bit 6) flags are set in C.
;                   Falls directly into norrel to calculate the final address.
;
; In:    BC   ---   exp/mag
;        DE   ---   x coordinate
;        HL   ---   y coordinate
;        IY   ---   Pattern address (IY+0 = X size, IY+1 = Y size)
; Out:   Same as norrel
;******************************************************************************************
ffnorrel:   bit     7,c
            jp      z,ffnorrel1
            ld      a,(iy+$01)          ; Load Y size
            add     a,h                 ; Add to Y coordinate
            dec     a
            ld      h,a                 ; Save adjusted Y coordinate
ffnorrel1:  bit     6,c
            jp      z,norrel
            ld      a,(iy+$00)          ; Load X size
            add     a,d                 ; Add to X coordinate
            dec     a
            ld      d,a                 ; Save adjusted X coordinate (Falls into norrel)

;##########################################################################################
;    LABEL norrel ( relative X Y to magic address conversion )
;       ( in- BC=exp/mag DE=x HL=y )
;       ( out- BC=exp/mag+shift HL=scradr )
;
;       H A MOV, 0 H MVI, A L MOV,
;       H DAD, H DAD, H DAD,
;       H DAD, D PUSH, L E MOV, H D MOV, H DAD, H DAD, ( *64 )
;       D DAD, ( *80 ) XCHG, H POP, ( x )
;       L A MOV,  ( SAVE BIT CNT ) H L MOV, 0 H MVI, D DAD, ( x+y )
;       RLC, RLC, HEX 3 ANI,
;
;       MRFLOP C BIT, 0<>, IF, NEG, 0=, IF, H DCX, THEN, THEN,
;       3 ANI, A E MOV, C A MOV, FC ANI, E ORA, A C MOV,
;    LABEL NULRET RET, ASSEMBLE> -->
;##########################################################################################
;******************************************************************************************
; ----> norrel      Relative X/Y to magic address conversion. Converts 2D
;                   screen coordinates into a linear memory address for the
;                   screen buffer (scradr) and shifts the exp/mag register.
;                   HL becomes (Y * 80) + X.
;
; In:    BC   ---   exp/mag
;        DE   ---   x coordinate
;        HL   ---   y coordinate
; Out:   BC   ---   exp/mag + shift
;        HL   ---   scradr (Screen RAM Address)
;******************************************************************************************
norrel:     ld      a,h
            ld      h,$00
            ld      l,a                 ; Move Y coordinate from H to L
            add     hl,hl               ; \
            add     hl,hl               ; |
            add     hl,hl               ; | HL = Y * 16
            add     hl,hl               ; /
            push    de                  ; Save original DE (X coordinate)
            ld      e,l                 ; \
            ld      d,h                 ; / DE = Y * 16
            add     hl,hl               ; \
            add     hl,hl               ; / HL = Y * 64
            add     hl,de               ; HL = (Y * 64) + (Y * 16) = Y * 80
            ex      de,hl               ; DE = Y * 80
            pop     hl                  ; HL = Original DE (X coordinate)
            ld      a,l                 ; Save Bit Count (Original E) into A
            ld      l,h                 ; \
            ld      h,$00               ; / L = Original D (Integer X)
            add     hl,de               ; HL = (Y * 80) + X
            rlca                        ; \
            rlca                        ; / Shift Bit Count
            and     $03                 ; Mask bottom 2 bits
            bit     6,c                 ; Check MRFLOP bit
            jp      z,norrel1
            neg
            jp      nz,norrel1
            dec     hl                  ; Adjust address if Flop is set and remainder is 0
norrel1:    and     $03
            ld      e,a
            ld      a,c
            and     $FC                 ; Mask out old shift bits
            or      e                   ; OR in new shift bits
            ld      c,a                 ; Save back to C
nulret:     ret                         ; LABEL NULRET

;##########################################################################################
;    { BLOCK 0039 }
;    ( VGS                   RELABS )
;
;    CODE RELABS ( relative to absolute conversion )
;       ( in- exp/mag , X , Y )
;       ( out- exp/mag+shift , scradr )
;
;       EXX, ( save BC )
;       H POP, ( Y ) D POP, ( X ) B POP, ( exp/mag )
;       relabs CALL, B PUSH, ( exp/mag+shf )
;       H PUSH, ( scradr ) EXX, NEXT
;##########################################################################################
;******************************************************************************************
; ----> RELABS  Relative to absolute conversion. Pops coordinates and
;               magic/expand properties from the parameter stack, then
;               calls the relabs RAM vector to calculate the screen address.
;               The RAM vector allows the game to swap between normal
;               and upside-down (cocktail) rendering dynamically.
;
; In:    exp/mag
;        X coordinate
;        Y coordinate
; Out:   exp/mag + shift
;        scradr (Screen RAM Address)
;******************************************************************************************
_RELABS:    exx
            pop     hl                  ; ( Y )
            pop     de                  ; ( X )
            pop     bc                  ; ( exp/mag )
            call    RELABS              ; Calls RAM vector (allows Cocktail mode swapping!)
            push    bc                  ; ( exp/mag+shf )
            push    hl                  ; ( scradr )
            exx
            DW      _DSPATCH

;##########################################################################################
;    ( UPSIDE DOWN RELABS ROUTINES FOR COCKTAIL MODE USE )
;
;    SUBR cockrel
;       norrel CALL, XCHG, 3FBF H LXI, A ANA,
;       D DSBC, C A MOV, 0C0 XRI, A C MOV, RET,
;##########################################################################################
;******************************************************************************************
; ----> cockrel Upside-down relative to absolute conversion. Used when
;               Player 2 is active in Cocktail mode. It calculates the
;               standard address via norrel, then inverts it by subtracting
;               it from the end of the screen buffer ($3FBF) and flipping
;               the pattern shift bits.
;
; In:    BC = exp/mag
;        DE = X coordinate
;        HL = Y coordinate
; Out:   BC = exp/mag + shift (inverted)
;        HL = scradr (inverted)
;******************************************************************************************
cockrel:    call    norrel
            ex      de,hl
            ld      hl,$3FBF
            and     a
            sbc     hl,de
            ld      a,c
            xor     $C0
            ld      c,a
            ret

;##########################################################################################
;    SUBR cockff
;
;       ffnorrel CALL, XCHG, 3FBF H LXI, A ANA,
;       D DSBC, C A MOV, 0C0 XRI, A C MOV, RET,
;##########################################################################################
;******************************************************************************************
; ----> cockff  Upside-down conversion with flip/flop support. Same as
;               cockrel, but calls ffnorrel first to adjust for hardware
;               flips before calculating the inverted screen address.
;
; In:    BC = exp/mag
;        DE = X coordinate
;        HL = Y coordinate
; Out:   BC = exp/mag + shift (inverted)
;        HL = scradr (inverted)
;******************************************************************************************
cockff:     call    ffnorrel
            ex      de,hl
            ld      hl,$3FBF
            and     a
            sbc     hl,de
            ld      a,c
            xor     $C0
            ld      c,a
            ret

;##########################################################################################
;    { BLOCK 0040 }
;    ( JOYSTICK READ ROUTINE )
;    HEX
;    SUBR GJ .REL
;    COCKTAIL LDA, A ANA, 0=, IF, 12 IN, ELSE, 11 IN, THEN,
;    0F XRI, RET, .ABS
;##########################################################################################
;******************************************************************************************
; ----> gj      Subroutine: Reads the joystick input port depending on the COCKTAIL
;               mode flag. Inverts the lower 4 bits so that active joystick switches
;               (which are usually active-low) are represented as 1s instead of 0s.
;******************************************************************************************
gj:         ld      a,(COCKTAIL)        ; Load the Cocktail mode flag
            and     a                   ; Test if it is 0
            jr      nz,gj1              ; If COCKTAIL != 0, jump to read port $11
            in      a,($12)             ; IF 0: Read Player 1 joystick port ($12)
            jr      gj2                 ; Skip the ELSE block
gj1:        in      a,($11)             ; ELSE: Read Player 2 / Cocktail port ($11)
gj2:        xor     $0F                 ; Invert the lower 4 bits (active-low to active-high)
            ret

;##########################################################################################
;    { BLOCK 0041 }
;    ( VGS                   WRITE )
;
;    SUBR write
;       ( pattern board write )
;       B A MOV, XPAND OUT, C A MOV, MAGIC OUT, 24 B MVI,
;       MRFLIP C BIT, 0<>, IF, PBFLIP B SET, THEN,
;       MRFLOP C BIT, 0<>, IF, PBFLOP B RES, THEN,
;       H PUSH, Y PUSHX, H POP,
;       MREXP  C BIT, 0<>, IF, PBEXP B SET,
;       ELSE, PBFLUSH B SET, E INR,
;       THEN,   B A MOV, PBSTAT OUT, ( B=status C=magic )
;       L A MOV, PBLINADRL OUT,
;       H A MOV, PBLINADRH OUT,
;       H POP,
;       L A MOV, PBAREADRL OUT,
;       H A MOV, PBAREADRH OUT,
;    -->
;
;    { BLOCK 0042 }
;    ( VGS                   write con't. )
;       E H MOV, ( X size )
;       MREXP C BIT, 0<>, IF, H RLCR, ( *2 ) THEN,
;       H DCR, ( H=X size zero relative )
;       MRFLIP C BIT, 0<>, IF,
;          MRFLOP C BIT, 0<>, IF, DECIMAL -80 A MVI, H ADD,
;                           ELSE, DECIMAL -80 A MVI, H SUB, THEN,
;       ELSE,
;          MRFLOP C BIT, 0<>, IF, DECIMAL  80 A MVI, H ADD,
;                           ELSE, DECIMAL  80 A MVI, H SUB, THEN,
;       THEN, ( A=Xmod ) PBXMOD OUT,
;       HEX H A MOV, PBXWIDE OUT,
;       D A MOV, ( Y size ) A DCR, ( 0 rel ) PBYHIGH OUT,
;       RET,
;    -->
;##########################################################################################

;******************************************************************************************
; ----> write   Pattern transfer routine. Programs the custom Pattern Board
;               hardware to rapidly draw a sprite from ROM/RAM to the screen buffer.
;
; In:    B  --- Expand mode byte (colors to expand 1bpp pattern to)
;        C  --- Magic Register Byte (Properties: Flip, Flop, Expand, XOR, OR)
;        D  --- Height of pattern (Y size)
;        E  --- Width of pattern (X size)
;        HL --- Destination address on screen (scradr)
;        IY --- Source address of Pattern (patadr)
;******************************************************************************************
write:      ld      a,b
            out     (XPAND),a           ; Set expand mode byte (colors)
            ld      a,c
            out     (MAGIC),a           ; Set Magic RAM control properties
            ld      b,$24               ; Default PB status: PBFLOP (5) and PBCONS (2) set
            bit     MRFLIP,c            ; Check Magic Register FLIP bit
            jp      z,write1            ; IF MRFLIP is clear, skip to write1
            set     PBFLIP,b            ; IF MRFLIP is set, apply FLIP to Pattern Board Status
write1:     bit     MRFLOP,c            ; Check Magic Register FLOP bit
            jp      z,write2            ; IF MRFLOP is clear, skip to write2
            res     PBFLOP,b            ; IF MRFLOP is set, REMOVE FLOP from Pattern Board Status
write2:     push    hl                  ; Save Destination Address
            push    iy                  ; \
            pop     hl                  ; / Move Source Address from IY into HL
            bit     MREXP,c             ; Check Magic Register EXPAND bit
            jp      z,write3            ; IF MREXP is clear, jump to write3
            set     PBEXP,b             ; IF MREXP is set, apply EXPAND to Pattern Board Status
            jp      write4              ; Skip the ELSE block
write3:     set     PBFLUSH,b           ; ELSE: apply FLUSH to Pattern Board Status
            inc     e                   ; Increment pattern width
write4:     ld      a,b                 ;
            out     (PBSTAT),a          ; Output final Status to Pattern Board
            ld      a,l                 ; \
            out     (PBLINADRL),a       ; | Output Source Address LSB (PBLINADRL)
            ld      a,h                 ; |
            out     (PBLINADRH),a       ; / Output Source Address MSB (PBLINADRH)
            pop     hl                  ; Retrieve Destination Address
            ld      a,l                 ; \
            out     (PBAREADRL),a       ; | Output Destination Address LSB (PBAREADRL)
            ld      a,h                 ; |
            out     (PBAREADRH),a       ; / Output Destination Address MSB (PBAREADRH)
            ld      h,e                 ; Move X size into H
            bit     MREXP,c             ; Check Expand bit again
            jp      z,write5            ; IF MREXP is clear, skip to write5
            rlc     h                   ; If expanding, double the X size (* 2)
write5:     dec     h                   ; Make X size zero-relative
            bit     MRFLIP,c            ; Check FLIP
            jp      z,write6            ; IF FLIP is clear, jump to write6...
            bit     MRFLOP,c            ;    Check FLOP
            jp      z,write7            ;    IF FLOP is clear, jump to write7...
            ld      a,$B0               ;       Xmod = -80 (Gorf screen width)
            add     a,h                 ;       Xmod += X size
            jp      write8              ;       Skip to Xmod output
write7:     ld      a,$B0               ;    ELSE (FLIP set, FLOP clear)
            sub     h                   ;       Xmod -= X size
write8:     jp      write9              ;       Skip to Xmod output
write6:     bit     MRFLOP,c            ; ELSE (FLIP is clear), Check FLOP...
            jp      z,write10           ;    IF FLOP is clear, jump to write10...
            ld      a,$50               ;    ELSE (FLIP clear, FLOP set)
            add     a,h                 ;       Xmod = 80 + X size
            jp      write9              ;       Skip to Xmod output
write10:    ld      a,$50               ;    ELSE (FLIP clear, FLOP clear)
            sub     h                   ;       Xmod = 80 - X size
write9:     out     (PBXMOD),a          ; Output calculated Xmod (line skip value)
            ld      a,h                 ; \
            out     (PBXWIDE),a         ; / Output X Width (zero relative)
            ld      a,d                 ; \
            dec     a                   ; | Output Y Height (zero relative)
            out     (PBYHIGH),a         ; /
            ret

;##########################################################################################
;    { BLOCK 0044 }
;    ( VGS                   writep )
;
;    SUBR writep ( does write with pattern size header on pattern )
;       ( in- BC=exp/mag+shift DE=y/x size HL=scradr IY=patadr )
;       ( WRTSYS 0<> for pattern board 0= for software write )
;       ( out- C=mag+shift ; pattern on screen )
;       0 Y E LDX, ( X size ) Y INXX,
;       0 Y D LDX, ( Y size ) Y INXX, write JMP,
;##########################################################################################
;******************************************************************************************
; ----> writep  Subroutine: Draws a sprite using the pattern board, reading the X and Y
;               size header directly from the pattern data (the first two bytes) before
;               jumping to the standard write routine.
;
; In:    BC   ---   exp/mag + shift
;        HL   ---   scradr (Destination screen RAM address)
;        IY   ---   patadr (Source pattern address, header is 2 bytes: X size, Y size)
; Out:   C    ---   mag + shift
;        Pattern drawn on screen
;******************************************************************************************
writep:     ld      e,(iy+$00)          ; Load X size from pattern header
            inc     iy                  ; Advance pattern address
            ld      d,(iy+$00)          ; Load Y size from pattern header
            inc     iy                  ; Advance pattern address past header
            jp      write               ; Jump to pattern transfer routine

;##########################################################################################
;    { BLOCK 0045 }
;    ( VGS                   WRITEP )
;
;    CODE WRITEP ( write with pattern size header on pattern )
;       ( in- x , y , patadr , ex/mag )
;       ( WRTSYS 0<> for pattern board 0= for software write )
;       ( out- pattern on screen )
;       Y PUSHX, H POP, EXX, B POP, Y POPX, H POP, D POP,
;       relabs CALL,
;       writep CALL, EXX, H PUSH, Y POPX, NEXT
;##########################################################################################
;******************************************************************************************
; ----> WRITEP  Pops the required parameters from the stack, calculates the absolute
;               screen address via relabs, and calls the writep subroutine to
;               draw a sprite that has a 2-byte size header.
;
; In:    x coordinate
;        y coordinate
;        patadr (Pattern Address)
;        ex/mag (Expand / Magic flags)
; Out:   Pattern drawn on screen
;******************************************************************************************
_WRITEP:    push    iy
            pop     hl
            exx
            pop     bc                  ; ( ex/mag )
            pop     iy                  ; ( patadr )
            pop     hl                  ; ( y coordinate )
            pop     de                  ; ( x coordinate )
            call    RELABS              ; Convert relative X/Y to absolute scradr
            call    writep              ; Draw the pattern
            exx
            push    hl
            pop     iy
            DW      _DSPATCH

;##########################################################################################
;    CODE FFWRITEP Y PUSHX, H POP, EXX, B POP, Y POPX, H POP, D POP,
;    ffrelabs CALL, writep CALL, EXX, H PUSH, Y POPX, NEXT
;##########################################################################################
;******************************************************************************************
; ----> FFWRITEP Same as WRITEP, but calls ffrelabs instead of relabs to
;                adjust the coordinates for hardware Flip/Flop before drawing.
;
; In:    x coordinate
;        y coordinate
;        patadr (Pattern Address)
;        ex/mag (Expand / Magic flags)
; Out:   Pattern drawn on screen
;******************************************************************************************
_FFWRITEP:  push    iy
            pop     hl
            exx
            pop     bc                  ; ( ex/mag )
            pop     iy                  ; ( patadr )
            pop     hl                  ; ( y coordinate )
            pop     de                  ; ( x coordinate )
            call    FFRELABS            ; Convert relative X/Y to absolute scradr with Flip/Flop
            call    writep              ; Draw the pattern
            exx
            push    hl
            pop     iy
            DW      _DSPATCH

;##########################################################################################
;    { BLOCK 0056 }
;    ( 16 BIT INTEGER DIVIDE ROUTINE: M N UN/ Q R ) DECIMAL
;    FORWARD .ZERO FORWARD IDV50 FORWARD IDV60
;    FORWARD IDV10 FORWARD IDV20 FORWARD IDV30 FORWARD IDV40
;
;    SUBR unsdiv <ASSEMBLE
;       L C MOV, H B MOV, D A MOV, 0 H LXI,
;       E ORA, .ZERO JRZ, B A MOV, 16 B MVI,
;    LABEL IDV10 C RALR, RAL, H DADC, D DSBC,
;    LABEL IDV20 CMC, IDV50 JRNC,
;    LABEL IDV30 IDV10 DJNZ, IDV60 JMPR,
;    LABEL IDV40 C RALR, RAL, H DADC, A ANA, D DADC,
;    IDV30 JRC, IDV20 JRZ,
;    LABEL IDV50 IDV40 DJNZ, D DAD, A ANA, ( MAKE IT POS )
;    LABEL IDV60 C RALR, RAL, A D MOV, C E MOV,
;    LABEL .ZERO RET, ASSEMBLE>
;##########################################################################################

;******************************************************************************************
; ----> unsdiv1 Subroutine: 16-bit unsigned integer division.
;
; In:    HL = Dividend (M)
;        DE = Divisor (N)
; Out:   DE = Quotient (Q)
;        HL = Remainder (R)
;******************************************************************************************
unsdiv1:    ld      c,l
            ld      b,h
            ld      a,d
            ld      hl,$0000
            or      e
            jr      z,ZERO              ; IF Divisor is 0, jump to end
            ld      a,b
            ld      b,$10               ; Loop 16 times (for 16 bits)
IDV10:      rl      c
            rla
            adc     hl,hl
            sbc     hl,de
IDV20:      ccf
            jr      nc,IDV50
IDV30:      djnz    IDV10
            jr      IDV60
IDV40:      rl      c
            rla
            adc     hl,hl
            and     a                   ; Clear carry flag
            adc     hl,de
            jr      c,IDV30
            jr      z,IDV20
IDV50:      djnz    IDV40
            add     hl,de
            and     a                   ; ( MAKE IT POS )
IDV60:      rl      c
            rla
            ld      d,a
            ld      e,c
ZERO:       ret

;##########################################################################################
;    SUBR UNSDIV
;       H PUSH, D DSBC, CY, IF, 0 D LXI, H POP, ELSE,
;       H POP, unsdiv CALL, THEN, RET,
;
;    CODE UN/
;       EXX, D POP, H POP,
;       UNSDIV CALL, H PUSH, D PUSH, EXX, NEXT DECIMAL -->
;##########################################################################################

;******************************************************************************************
; ----> unsdiv2     Subroutine: Unsigned division wrapper with math protection.
;                   If the divisor is larger than the dividend, it skips the
;                   division entirely and simply returns a quotient of 0.
;******************************************************************************************
unsdiv2:        push    hl
                sbc     hl,de
                jp      nc,unsdiv_call      ; IF it fits, jump to divide
                ld      de,$0000            ; ELSE set Quotient to 0
                pop     hl
                jp      unsdiv_end          ; And skip to the end
unsdiv_call:    pop     hl
                call    unsdiv1             ; Call the main division loop
unsdiv_end:     ret

;******************************************************************************************
; ----> UN/     TERSE Dictionary Word: Pops Divisor and Dividend from the stack,
;               calls the UNSDIV routine, and pushes the Remainder and Quotient.
;
; In:    Dividend (M)
;        Divisor (N)
; Out:   Remainder (R)
;        Quotient (Q)
;******************************************************************************************
_UNSDIV:    exx
            pop     de                  ; D POP (Divisor)
            pop     hl                  ; H POP (Dividend)
            call    unsdiv2             ; UNSDIV CALL
            push    hl                  ; H PUSH (Remainder)
            push    de                  ; D PUSH (Quotient)
            exx
            DW      _DSPATCH

;##########################################################################################
;    { BLOCK 0047 }
;    ( SNAP COMMAND )
;    HEX CODE snap EXX,
;    25 A MVI, PBSTAT OUT,
;    H POP,
;    D POP, E A MOV, PBAREADRL OUT, D A MOV, 40 ORI, PBAREADRH OUT,
;    D POP, B POP,
;    B INX, B INX, B INX, B SRLR, C RARR, B SRLR, C RARR,
;    C A MOV, PBXWIDE OUT, A INR, A M MOV, H INX, E M MOV, H INX,
;    L A MOV, PBLINADRL OUT, H A MOV, PBLINADRH OUT,
;    50 A MVI, C SUB, PBXMOD OUT,
;    E A MOV, A DCR, PBYHIGH OUT, ( DO IT TO IT )
;    EXX, NEXT
;##########################################################################################

;******************************************************************************************
; ----> snap    Low-level Pattern Board Move / Snapshot machine code.
;******************************************************************************************
snap_code:  exx
            ld      a,$25
            out     (PBSTAT),a          ; PBSTAT ($7A) - Set PBFLOP, PBCONS, PBDIR
            pop     hl
            pop     de
            ld      a,e
            out     (PBAREADRL),a       ; PBAREADRL ($7B)
            ld      a,d
            or      $40
            out     (PBAREADRH),a       ; PBAREADRH ($7C)
            pop     de
            pop     bc
            inc     bc
            inc     bc
            inc     bc
            srl     b                   ; \
            rr      c                   ; | Shift BC right 2 times (Divide by 4)
            srl     b                   ; |
            rr      c                   ; /
            ld      a,c
            out     (PBXWIDE),a         ; PBXWIDE ($7D)
            inc     a
            ld      (hl),a
            inc     hl
            ld      (hl),e
            inc     hl
            ld      a,l
            out     (PBLINADRL),a       ; PBLINADRL ($78)
            ld      a,h
            out     (PBLINADRH),a       ; PBLINADRH ($79)
            ld      a,$50
            sub     c
            out     (PBXMOD),a          ; PBXMOD ($7B)
            ld      a,e
            dec     a
            out     (PBYHIGH),a         ; PBYHIGH ($7E)
            exx
            DW      _DSPATCH

;##########################################################################################
;    { BLOCK 0047 } ...CONTINUED
;    : SNAP 0 ROT ROT RELABS SWAP DROP SWAP snap ;
;    HEX DATA CKSM1 5A B,
;    DECIMAL -->
;##########################################################################################

;******************************************************************************************
; ----> SNAP    High-level TERSE dictionary word. Prepares the stack parameters
;               and calculates the absolute screen address before calling the
;               low-level machine code 'snap' routine.
;******************************************************************************************
_SNAP:      DB      _ENTER              ; Enter TERSE execution
            DW      _0                  ; Push 0
            DW      _ROT                ; \
            DW      _ROT                ; / Bring X, Y to top
            DW      _RELABS             ; Convert X,Y to absolute screen address
            DW      _SWAP               ; \
            DW      _DROP               ; | Rearrange stack and drop unneeded shift value
            DW      _SWAP               ; /
            DW      snap_code           ; Call low-level machine code snap
            DW      _RETURN             ; Return to caller

;******************************************************************************************
; Checksum byte for ROM Integrity
;******************************************************************************************
CKSM1:      DB      $5A                 ; Hardcoded ROM Checksum 1


;##########################################################################################
;    { BLOCK 0058 }
;    ( 8 X 10 CHARACTER SET - ROTATED )
;    HEX
;    DATA CHRTBL
;    0000 , 0000 , 0000 , 0000 , 0000 , 0000 , ( SPACE )
;    E01F , F03F , 3030 , 3030 , F03F , E01F , ( 0 )
;    0000 , 2030 , E03F , F03F , 0030 , 0000 , ( 1 )
;    603E , 703F , 3033 , 3033 , F033 , E031 , ( 2 )
;    6018 , 7038 , 3033 , 3033 , F03F , E01E , ( 3 )
;    F003 , F003 , 0003 , 0003 , F03F , F03F , ( 4 )
;    F01B , F03B , 3033 , 3033 , 303F , 001E , ( 5 )
;    E01F , F03F , 3033 , 3033 , 303F , 001E , ( 6 )
;    3000 , 3038 , 303E , B00F , F003 , F000 , ( 7 )
;    E01E , F03F , 3033 , 3033 , F03F , E01E , ( 8 )
;    E001 , F003 , 3033 , 3033 , F03F , E01F , ( 9 )
;    -->
;##########################################################################################
;    { BLOCK 0059 }
;    ( MORE CHARS )
;    C03F , E03F , 700C , 700C , E03F , C03F , ( A )
;    F03F , F03F , 3033 , 3033 , F03F , E01E , ( B )
;    E01F , F03F , 3030 , 3030 , 7038 , 6018 , ( C )
;    F03F , F03F , 3030 , 3030 , F03F , E01F , ( D )
;    F03F , F03F , 3033 , 3033 , 3033 , 3030 , ( E )
;    F03F , F03F , 3003 , 3003 , 3003 , 3000 , ( F )
;    E01F , F03F , 3030 , 3036 , 303E , 201E , ( G )
;    F03F , F03F , 0003 , 0003 , F03F , F03F , ( H )
;    0000 , 3030 , F03F , F03F , 3030 , 0000 , ( I )
;    001C , 003C , 0030 , 0030 , F03F , F01F , ( J )
;    F03F , F03F , 8003 , C00F , F03C , 7038 , ( K )
;    F03F , F03F , 0030 , 0030 , 0030 , 0030 , ( L )
;    F03F , 6000 , 8001 , 8001 , 6000 , F03F , ( M )
;    F03F , F03F , C001 , 8003 , F03F , F03F , ( N )
;    E01F , F03F , 3030 , 3030 , F03F , E01F , ( O )
;    F03F , F03F , 3003 , 3003 , F003 , E001 , ( P )
;    -->
;##########################################################################################
;    { BLOCK 0060 }
;    ( CHARS )
;    E01F , F03F , 3020 , 3028 , F01F , E02F , ( Q )
;    F03F , F03F , 3003 , 3007 , F03F , E03D , ( R )
;    E019 , F03B , 3033 , 3033 , 703F , 601E , ( S )
;    3000 , 3000 , F03F , F03F , 3000 , 3000 , ( T )
;    F01F , F03F , 0030 , 0030 , F03F , F01F , ( U )
;    F003 , F00F , 003E , 003E , F00F , F003 , ( V )
;    F03F , 000C , 8007 , 8007 , 000C , F03F , ( W )
;    7038 , F03C , C00F , C00F , F03C , 7038 , ( X )
;    7000 , F000 , C03F , C03F , F000 , 7000 , ( Y )
;    303C , 303E , 3037 , B033 , F031 , F030 , ( Z )
;    0000 , 6018 , 6018 , 0000 , 0000 , 0000 , ( : )
;    E01F , 7038 , B037 , B037 , 703B , E01F , ( COPYRITE )
;    -->
;##########################################################################################

;******************************************************************************************
; GORF STANDARD CHARACTER SET (CHRTBL)
;
; Because the Gorf arcade cabinet uses a vertically oriented monitor, the font
; graphics are stored lying on their side.
;
; Specifically, each 12-byte character is drawn as 6 consecutive vertical columns
; (2 bytes per column), scanning from the left side of the letter to the right.
; To visualize the characters in the assembly, simply tilt your head 90 degrees
; to the left and read the binary bitmap.
;
; Example: The letter 'A'
;
;               $3F, $C0    --XXXXXXXX------   (Left Leg)
;               $3F, $E0    --XXXXXXXXX-----   (Inner Left)
;               $0C, $70    ----XX---XXX----   (Crossbar)
;               $0C, $70    ----XX---XXX----   (Crossbar)
;               $3F, $E0    --XXXXXXXXX-----   (Inner Right)
;               $3F, $C0    --XXXXXXXX------   (Right Leg)
;
; Furthermore, these 1-bit-per-pixel (1bpp) font graphics are automatically translated
; into full-color sprites using the Pattern Board's "Magic Expand" hardware. When drawn
; to the screen, the custom hardware converts every individual bit into a 2-bit pixel.
; The resulting colors are determined by the values currently programmed into the
; XPAND hardware port ($19).
;******************************************************************************************
CHRTBL      EQU     $   ; Beginning of character table

            DB      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; Space
            DB      $1F, $E0, $3F, $F0, $30, $30, $30, $30, $3F, $F0, $1F, $E0    ; 0
            DB      $00, $00, $30, $20, $3F, $E0, $3F, $F0, $30, $00, $00, $00    ; 1
            DB      $3E, $60, $3F, $70, $33, $30, $33, $30, $33, $F0, $31, $E0    ; 2
            DB      $18, $60, $38, $70, $33, $30, $33, $30, $3F, $F0, $1E, $E0    ; 3
            DB      $03, $F0, $03, $F0, $03, $00, $03, $00, $3F, $F0, $3F, $F0    ; 4
            DB      $1B, $F0, $3B, $F0, $33, $30, $33, $30, $3F, $30, $1E, $00    ; 5
            DB      $1F, $E0, $3F, $F0, $33, $30, $33, $30, $3F, $30, $1E, $00    ; 6
            DB      $00, $30, $38, $30, $3E, $30, $0F, $B0, $03, $F0, $00, $F0    ; 7
            DB      $1E, $E0, $3F, $F0, $33, $30, $33, $30, $3F, $F0, $1E, $E0    ; 8
            DB      $01, $E0, $03, $F0, $33, $30, $33, $30, $3F, $F0, $1F, $E0    ; 9
            DB      $3F, $C0, $3F, $E0, $0C, $70, $0C, $70, $3F, $E0, $3F, $C0    ; A
            DB      $3F, $F0, $3F, $F0, $33, $30, $33, $30, $3F, $F0, $1E, $E0    ; B
            DB      $1F, $E0, $3F, $F0, $30, $30, $30, $30, $38, $70, $18, $60    ; C
            DB      $3F, $F0, $3F, $F0, $30, $30, $30, $30, $3F, $F0, $1F, $E0    ; D
            DB      $3F, $F0, $3F, $F0, $33, $30, $33, $30, $33, $30, $30, $30    ; E
            DB      $3F, $F0, $3F, $F0, $03, $30, $03, $30, $03, $30, $00, $30    ; F
            DB      $1F, $E0, $3F, $F0, $30, $30, $36, $30, $3E, $30, $1E, $20    ; G
            DB      $3F, $F0, $3F, $F0, $03, $00, $03, $00, $3F, $F0, $3F, $F0    ; H
            DB      $00, $00, $30, $30, $3F, $F0, $3F, $F0, $30, $30, $00, $00    ; I
            DB      $1C, $00, $3C, $00, $30, $00, $30, $00, $3F, $F0, $1F, $F0    ; J
            DB      $3F, $F0, $3F, $F0, $03, $80, $0F, $C0, $3C, $F0, $38, $70    ; K
            DB      $3F, $F0, $3F, $F0, $30, $00, $30, $00, $30, $00, $30, $00    ; L
            DB      $3F, $F0, $00, $60, $01, $80, $01, $80, $00, $60, $3F, $F0    ; M
            DB      $3F, $F0, $3F, $F0, $01, $C0, $03, $80, $3F, $F0, $3F, $F0    ; N
            DB      $1F, $E0, $3F, $F0, $30, $30, $30, $30, $3F, $F0, $1F, $E0    ; O
            DB      $3F, $F0, $3F, $F0, $03, $30, $03, $30, $03, $F0, $01, $E0    ; P
            DB      $1F, $E0, $3F, $F0, $20, $30, $28, $30, $1F, $F0, $2F, $E0    ; Q
            DB      $3F, $F0, $3F, $F0, $03, $30, $07, $30, $3F, $F0, $3D, $E0    ; R
            DB      $19, $E0, $3B, $F0, $33, $30, $33, $30, $3F, $70, $1E, $60    ; S
            DB      $00, $30, $00, $30, $3F, $F0, $3F, $F0, $00, $30, $00, $30    ; T
            DB      $1F, $F0, $3F, $F0, $30, $00, $30, $00, $3F, $F0, $1F, $F0    ; U
            DB      $03, $F0, $0F, $F0, $3E, $00, $3E, $00, $0F, $F0, $03, $F0    ; V
            DB      $3F, $F0, $0C, $00, $07, $80, $07, $80, $0C, $00, $3F, $F0    ; W
            DB      $38, $70, $3C, $F0, $0F, $C0, $0F, $C0, $3C, $F0, $38, $70    ; X
            DB      $00, $70, $00, $F0, $3F, $C0, $3F, $C0, $00, $F0, $00, $70    ; Y
            DB      $3C, $30, $3E, $30, $37, $30, $33, $B0, $31, $F0, $30, $F0    ; Z
            DB      $00, $00, $18, $60, $18, $60, $00, $00, $00, $00, $00, $00    ; :
            DB      $1F, $E0, $38, $70, $37, $B0, $37, $B0, $3B, $70, $1F, $E0    ; Copyright Symbol

;##########################################################################################
;   { BLOCK 0051 }
;   ( NEW CHARACTER DRAW ROUTINE )
;   ( IN HL=Y DE=X BC=EXPAND/MAGIC A= CHAR TO DISPLAY )
;   HEX
;   SUBR drawchar .REL B PUSH, H PUSH, D PUSH, 20 SUI, 0<>, IF,
;   0F SUI, 0B CPI, CY~, IF, 7 SUI, THEN, THEN,
;   A L MOV, 0 H MVI, H DAD, H DAD, L E MOV, H D MOV,
;   H DAD, D DAD, CHRTBL D LXI, D DAD, H PUSH, Y POPX,
;   D POP, H POP, H PUSH, D PUSH, relabs CALL,
;   602 D LXI, write CALL, D POP, H POP, H A MOV, 7 ADI,
;   A H MOV, B POP, RET, .ABS
;##########################################################################################

;******************************************************************************************
; ----> drawchar    Draws a single text character to the screen using the Pattern Board.
;                   Automatically translates standard ASCII hex values into their
;                   corresponding index in the custom CHRTBL.
;
; IN:    A  --- ASCII Character to display
;        BC --- EXPAND/MAGIC properties
;        DE --- X Coordinate
;        HL --- Y Coordinate
; OUT:   HL --- Updated Y Coordinate (advanced by 7 pixels for the next character)
;******************************************************************************************
drawchar:   push    bc                  ; Save EXPAND/MAGIC
            push    hl                  ; Save Y Coordinate
            push    de                  ; Save X Coordinate

            sub     $20                 ; Subtract $20 (ASCII Space)
            jr      z,drawchar1         ; IF 0 (Space), skip to draw
            sub     $0F                 ; Subtract $0F to reach Digits ('0' is now index 1)
            cp      $0B                 ; Compare with 11
            jr      c,drawchar1         ; IF < 11, it is a digit, skip to draw
            sub     $07                 ; ELSE subtract $07 to reach Letters ('A' is now index 11)

drawchar1:  ld      l,a                 ; \
            ld      h,$00               ; / HL = Character Index
            add     hl,hl               ; \
            add     hl,hl               ; / HL = Index * 4
            ld      e,l                 ; \
            ld      d,h                 ; / DE = Index * 4
            add     hl,hl               ; HL = Index * 8
            add     hl,de               ; HL = (Index * 8) + (Index * 4) = Index * 12
            ld      de,CHRTBL           ;
            add     hl,de               ; HL = Address of character graphics in CHRTBL

            push    hl                  ; \
            pop     iy                  ; / Move CHRTBL source address into IY for 'write'

            pop     de                  ; \   Pop X into DE
            pop     hl                  ;  |  Pop Y into HL
            push    hl                  ;  |  Push Y back
            push    de                  ; /   Push X back (Peeks at X and Y for relabs)

            call    RELABS              ; Call RAM vector to calculate absolute screen address

            ld      de,$0602            ; D (Height) = 6 lines, E (Width) = 2 bytes
            call    write               ; Call pattern transfer routine

            pop     de                  ; Restore X Coordinate
            pop     hl                  ; Restore Y Coordinate
            ld      a,h                 ; \
            add     a,$07               ;  | Advance Y Coordinate by 7 pixels for the next character
            ld      h,a                 ; /
            pop     bc                  ; Restore EXPAND/MAGIC
            ret

;******************************************************************************************
; ----> cpost   TERSE INTERFACE - X Y COLOR/MAGIC CHAR cpost --- NEW X Y
;******************************************************************************************
_CPOST:     exx
            pop     bc
            ld      a,c
            pop     bc
            pop     hl
            pop     de
            push    ix
            push    iy
            call    drawchar
            pop     iy
            pop     ix
            push    de
            push    hl
            push    bc
            exx
            DW      _DSPATCH

;##########################################################################################
; { BLOCK 0052 }
; ( VGS                        SPOST )
;
; : SPOST ( post an ascii-string on the screen ; see options )
;   ( in= x , y , opt+ex/mag , addr , count )
;        ( i.e. 0 0 28 A" STRING" COUNT SPOST )
;         ( WRTSYS 0<> for pattern board 0= for software write )
;         ( cannot be used in immediat mode )
;   ( out- character on screen )
;   OVER + SWAP DO I B@ cpost LOOP 2DROP DROP ;
; -->
;##########################################################################################

;******************************************************************************************
; ----> SPOST   TERSE Word. Posts an ascii-string on the screen.
;******************************************************************************************
_SPOST:     DB      _ENTER
            DW      _OVER               ; OVER
            DW      _plus               ; +
            DW      _SWAP               ; SWAP
            DW      _DO                 ; DO
            DW      _I                  ; I
            DW      _Bat                ; B@
            DW      _CPOST              ; cpost
            DW      _LOOP               ; LOOP
            DW      _2DROP              ; 2DROP
            DW      _DROP               ; DROP
            DW      _RETURN             ; ;

;##########################################################################################
; { BLOCK 0053 }
; ( INDEX MESSAGE LIST )
; HEX
; DATA MSGLINK 0 ,
;##########################################################################################

MSGLINK:    DW      $3CD9               ; Pointer to the active message list (ASCMSGT)

;##########################################################################################
; F= iml
; SUBR indexmsg .REL SETTINGS IN, 8 ANI, 0<>, IF, MSGLINK LHLD,
; ELSE, 0C003 H LXI,
; THEN,
; LABEL iml C A MOV, A ANA, RZ,
; M E MOV, 0 D MVI, D DAD, H INX, C DCR, iml JMPR, .ABS
;##########################################################################################

;******************************************************************************************
; ----> indexmsg    Subroutine: Locates a message string from a string table based on
;                   an index. It checks the dipswitches (SETTINGS) to see which
;                   language/message list to use.
;
; IN:    C = Message Index
; OUT:  HL = Address of the string to print
;******************************************************************************************
indexmsg:   in      a,($13)             ; Read SETTINGS dipswitches
            and     $08                 ; Check bit 3 (Language/Message set)
            jr      z,iml_alt           ; IF clear, use alternate messages at $C003
            ld      hl,(MSGLINK)        ; ELSE use primary messages pointed to by MSGLINK
            jr      iml                 ; Skip to index loop

iml_alt:    ld      hl,$C003            ; Alternate message table base address

iml:        ld      a,c                 ; Move index counter into A
            and     a                   ; Check if index is 0
            ret     z                   ; IF 0, return (HL points to correct string)

iml_loop:   ld      e,(hl)              ; \
            ld      d,$00               ; | Read the length of the current string
            add     hl,de               ; / Add length to HL to skip over the string data
            inc     hl                  ; Skip the length byte itself
            dec     c                   ; Decrement index counter
            jr      iml                 ; Jump back to check if index reached 0

;##########################################################################################
; { BLOCK 0053 } ...CONTINUED
; CODE INXMSG EXX, B POP, indexmsg CALL, H PUSH, EXX, NEXT
;##########################################################################################

;******************************************************************************************
; ----> INXMSG  TERSE Dictionary Word: Pops the requested message index from the
;               TERSE stack, calls indexmsg to find its address, and pushes
;               the resulting string address back onto the stack.
;******************************************************************************************
_INXMSG:    exx
            pop     bc                  ; Pop message index into B
            call    indexmsg            ; Call low-level string locator
            push    hl                  ; Push resulting string address
            exx
            DW      _DSPATCH

;##########################################################################################
;       : ICSPOST INXMSG COUNT SPOST ;
;##########################################################################################
;******************************************************************************************
; ----> ICSPOST High-level TERSE word. Takes a message index, converts it to an
;               address, counts the string length, and posts it to the screen.
;******************************************************************************************
_ICSPOST:   DB      _ENTER              ; Enter TERSE execution
            DW      _INXMSG             ; Get string address from index
            DW      _Bat_inc7           ; COUNT (Gorf strings use the high-bit flag, so it compiles as B@+7)
            DW      _SPOST              ; Post string to screen
            DW      _RETURN

;##########################################################################################
;       : GNAME >R 100 5000 408 R> ICSPOST ;
;       2 +BLOCK CONTINUED
;##########################################################################################
;******************************************************************************************
; ----> GNAME   High-level TERSE word. Sets up the screen coordinates ($100, $5000)
;               and the Exp/Mag attributes ($408) for a string, then calls ICSPOST.
;******************************************************************************************
_GNAME:     DB      _ENTER              ; Enter TERSE execution
            DW      _gtR                ; >R (Save string index to Return stack)
            DW      _LIT            ; \
            DW      $0100               ; / Push X coordinate ($100)
            DW      _LIT            ; \
            DW      $5000               ; / Push Y coordinate ($5000)
            DW      _LIT            ; \
            DW      $0408               ; / Push Exp/Mag attributes ($408)
            DW      _Rgt                ; R> (Retrieve string index from Return stack)
            DW      _ICSPOST            ; Call ICSPOST to draw the string
            DW      _RETURN

;##########################################################################################
;  { BLOCK 0055 }
;  ( SPELL A STRING CENTERED )
;  HEX F= SPLP
;  CODE CSPELL <ASSEMBLE
;  H POP, M E MOV, H INX, E A MOV, EXX, 6000 H LXI, B A MOV,
;  0FC80 D LXI,
;  BEGIN, D DAD, LOOP, B POP, D POP, X PUSHX, Y PUSHX, EXX,
;  LABEL SPLP M A MOV, H INX, EXX, drawchar CALL, EXX,
;  E DCR, SPLP JRNZ, Y POPX, X POPX, NEXT ASSEMBLE>
;##########################################################################################
;******************************************************************************************
; ----> CSPELL  Low-Level Machine Code: Spell a String Centered.
;               Pops the string address, calculates the centering offset based on
;               string length, and loops through the string calling drawchar.
;******************************************************************************************
_CSPELL:    pop     hl                  ; Pop string address into HL
            ld      e,(hl)              ; Read string length into E
            inc     hl                  ; Advance to first character
            ld      a,e                 ; Copy length to A
            exx                         ; Swap to alternate registers
            ld      hl,$6000            ; Base centering coordinate
            ld      b,a                 ; B = string length
            ld      de,$FC80            ; Offset to subtract per character (-$0380)
cspell_lp1: add     hl,de               ; \
            djnz    cspell_lp1          ; / Loop to calculate starting X coordinate
            pop     bc                  ; Pop Expand/Magic Attributes
            pop     de                  ; Pop Y Coordinate
            push    ix                  ; \
            push    iy                  ; / Save TERSE stack pointers
            exx                         ; Swap back to primary registers
SPLP:       ld      a,(hl)              ; Read next character
            inc     hl                  ; Advance string pointer
            exx                         ; Swap to alternate registers (X, Y, Attr ready)
            call    drawchar            ; Draw the character
            exx                         ; Swap back to primary registers
            dec     e                   ; Decrement character count
            jr      nz,SPLP             ; If more characters, loop back
            pop     iy                  ; \
            pop     ix                  ; / Restore TERSE stack pointers
            DW      _DSPATCH

;##########################################################################################
;  { BLOCK 0055 } ...CONTINUED
;  : XUP 40 * ;
;  : SPELL INXMSG 428 SWAP CSPELL ;
;  DECIMAL -->
;##########################################################################################

;******************************************************************************************
; ----> XUP     High-Level TERSE word. Multiplies the top of the stack by $40.
;******************************************************************************************
_XUP:       DB      _ENTER              ; Enter TERSE execution
            DW      _LITbyte            ; \
            DB      $40                 ; / Push literal byte $40
            DW      _star               ; Multiply (*)
            DW      _RETURN

;******************************************************************************************
; ----> SPELL   High-Level TERSE word. Takes a message index and Y coordinate.
;               Locates the string, adds the default Exp/Mag attributes ($0428),
;               and calls CSPELL to center and draw it.
;******************************************************************************************
_SPELL:     DB      _ENTER              ; Enter TERSE execution
            DW      _INXMSG             ; Convert string index to address
            DW      _LIT            ; \
            DW      $0428               ; / Push Exp/Mag attributes ($0428)
            DW      _SWAP               ; Swap string address and attributes
            DW      _CSPELL             ; Draw the centered string
            DW      _RETURN

;##########################################################################################
; { BLOCK 0056 }
; ( DISPLAY 6 DIGIT BCD NUMBER -- X Y OPT NUMADDR DISPBCD6 )
; HEX SUBR digit 0F ANI, 0=, IF, D ORA, 0<>, IF, 0F0 A MVI, THEN,
; ELSE, 0 D MVI, THEN, 30 ADI, EXX, drawchar CALL, EXX, RET,
; HEX
;##########################################################################################
;******************************************************************************************
; ----> digit   Subroutine: Converts a BCD nibble to an ASCII character and draws it.
;               Uses a brilliant trick for leading zeroes: it sets A to $F0, so when
;               $30 is added to it, it wraps around to $20 (the ASCII code for Space)!
;
; IN:    A = BCD Digit to draw
;        D = Leading Zero Flag (0 = No longer leading, <>0 = Still leading)
;******************************************************************************************
digit:      and     $0F
            jp      nz,digit1           ; IF digit <> 0, skip leading zero check
            or      d                   ; Check D (Leading zero flag)
            jp      z,digit2            ; IF D = 0 (Not a leading zero), skip to addition
            ld      a,$F0               ; ELSE it IS a leading zero. Load $F0. ($F0+$30 = $20 Space)
digit2:     jp      digit3              ; Skip the ELSE block
digit1:     ld      d,$00               ; ELSE (digit <> 0): Clear the leading zero flag
digit3:     add     a,$30               ; Add $30 to convert to ASCII
            exx                         ; Swap to alternate registers (X, Y, Attr ready)
            call    drawchar            ; Draw the character
            exx                         ; Swap back
            ret

;##########################################################################################
; { BLOCK 0056 } ...CONTINUED
; F= DGTL
; CODE DISPBCD6 <ASSEMBLE H POP, M A MOV, H INX, M ORA,
; H INX, M ORA, A D MOV, 3 E MVI,
; EXX, B POP, H POP, D POP, X PUSHX, Y PUSHX, EXX,
; LABEL DGTL M A MOV, RRC, RRC, RRC, RRC, digit CALL,
; M A MOV, digit CALL, H DCX, E DCR, DGTL JRNZ,
; Y POPX, X POPX, NEXT ASSEMBLE>
; CC? IFTRUE TIMOR IFEND
; DECIMAL -->
;##########################################################################################
;******************************************************************************************
; ----> DISPBCD6    TERSE Word: Displays a 6-digit BCD number.
;                   It first ORs all 3 bytes together into D to initialize
;                   the leading zero flag.
;******************************************************************************************
_DISPBCD6:  pop     hl                  ; Pop number address into HL
            ld      a,(hl)              ; \
            inc     hl                  ; |
            or      (hl)                ; | OR all 3 bytes together
            inc     hl                  ; |
            or      (hl)                ; /
            ld      d,a                 ; Move result into D (Leading zero flag)
            ld      e,$03               ; E = 3 bytes to process (6 digits)
            exx
            pop     bc                  ; Pop Exp/Mag Attributes
            pop     hl                  ; Pop Y Coordinate
            pop     de                  ; Pop X Coordinate
            push    ix                  ; \
            push    iy                  ; / Save TERSE stack pointers
            exx
DGTL:       ld      a,(hl)              ; Read BCD byte
            rrca                        ; \
            rrca                        ; | Shift right 4 times to get upper nibble
            rrca                        ; |
            rrca                        ; /
            call    digit               ; Draw upper digit
            ld      a,(hl)              ; Read BCD byte again
            call    digit               ; Draw lower digit (digit subroutine masks out upper nibble)
            dec     hl                  ; Move to next byte
            dec     e                   ; Decrement byte counter
            jr      nz,DGTL             ; Loop if more bytes remain
            pop     iy                  ; \
            pop     ix                  ; / Restore TERSE stack pointers
            DW      _DSPATCH

;##########################################################################################
;  { BLOCK 0057 }
;  ( TWO DIGIT BCD DISPLAY ROUTINE AND BUMPER )
;  CODE DISPBCD2 H POP, EXX, B POP, H POP, D POP,
;  X PUSHX, Y PUSHX, EXX,
;  1 D MVI, L A MOV, RRC, RRC, RRC, RRC, digit CALL,
;  0 D MVI, L A MOV, digit CALL,
;  Y POPX, X POPX, NEXT
;
;  { : <DI, } { [ } ASM { ] } LDAI, PSW PUSH, DI, p { ; }
;  { : EI>, } { [ } ASM { ] } PSW POP, V, IF, EI, THEN, p { ; }
;  DECIMAL -->
;##########################################################################################
;******************************************************************************************
; ----> DISPBCD2    TERSE Word: Displays a 2-digit BCD number.
;******************************************************************************************
_DISPBCD2:  pop     hl                  ; Pop BCD value directly into HL
            exx
            pop     bc                  ; Pop Exp/Mag Attributes
            pop     hl                  ; Pop Y Coordinate
            pop     de                  ; Pop X Coordinate
            push    ix                  ; \
            push    iy                  ; / Save TERSE stack pointers
            exx
            ld      d,$01               ; D = 1 (Force leading zero flag ON so it can blank)
            ld      a,l                 ; Get BCD byte
            rrca                        ; \
            rrca                        ; | Shift right 4 times to get upper nibble
            rrca                        ; |
            rrca                        ; /
            call    digit               ; Draw upper digit
            ld      d,$00               ; D = 0 (Clear leading zero flag so lower digit always draws)
            ld      a,l                 ; Get BCD byte again
            call    digit               ; Draw lower digit
            pop     iy                  ; \
            pop     ix                  ; / Restore TERSE stack pointers
            DW      _DSPATCH

;##########################################################################################
; { BLOCK 0058 }
; ( WRITE PROTECT PRIMITIVES ) HEX
; ( *** SEND EVERYTHING THRU THESE, THEY DO DOUBLE WRITING *** )
;
; SUBR 2wp! ( HL=addr, DE=value )
;  A5 A MVI, 5B OUT,
;  E M MOV, H INX, 5B OUT, D M MOV, RET,
;##########################################################################################
;******************************************************************************************
; ----> 2wp!    Subroutine: Writes a 16-bit word (DE) to hardware Write-Protected RAM (HL).
;               Unlocks the NVRAM by writing $A5 to port $5B before each byte write.
;******************************************************************************************
two_wp_bang:  ld      a,$A5
            out     ($5B),a             ; Unlock NVRAM
            ld      (hl),e              ; Write LSB
            inc     hl
            out     ($5B),a             ; Unlock NVRAM
            ld      (hl),d              ; Write MSB
            ret

;##########################################################################################
; { BLOCK 0058 } ...CONTINUED
; SUBR 2wpb! ( HL = addr, E = value )
;  A5 A MVI, 5B OUT, E M MOV, RET,
;##########################################################################################
;******************************************************************************************
; ----> 2wpb!   Subroutine: Writes an 8-bit byte (E) to hardware Write-Protected RAM (HL).
;******************************************************************************************
two_wpb_bang: ld      a,$A5
            out     ($5B),a             ; Unlock NVRAM
            ld      (hl),e              ; Write byte
            ret

;##########################################################################################
; { BLOCK 0058 } ...CONTINUED
; ( * WP WORD! and WP BYTE! * )
; SUBR wp! ( HL = addr DE = value ) <DI, H PUSH, 2wp! CALL,
;  H POP, EI>, RET,
;##########################################################################################
;******************************************************************************************
; ----> wp!     Subroutine: Interrupt-safe 16-bit write to protected NVRAM.
;******************************************************************************************
wp_bang:    ld      a,i                 ; \
            push    af                  ; | <DI, (Save interrupt state and disable)
            di                          ; /
            push    hl
            call    two_wp_bang          ; Call 16-bit unlock/write
            pop     hl
            pop     af                  ; \
            jp      po,wp_ei_skip       ; | EI>, (Restore previous interrupt state)
            ei                          ; |
wp_ei_skip: ret                         ; /

;##########################################################################################
; { BLOCK 0058 } ...CONTINUED
; SUBR wpb! ( HL = addr, E = value )
;  <DI, H PUSH, 2wpb! CALL, H POP, EI>,
;  RET,
;##########################################################################################
;******************************************************************************************
; ----> wpb!    Subroutine: Interrupt-safe 8-bit write to protected NVRAM.
;******************************************************************************************
wpb_bang:   ld      a,i                 ; \
            push    af                  ; | <DI, (Save interrupt state and disable)
            di                          ; /
            push    hl
            call    two_wpb_bang         ; Call 8-bit unlock/write
            pop     hl
            pop     af                  ; \
            jp      po,wpb_ei_skip      ; | EI>, (Restore previous interrupt state)
            ei                          ; |
wpb_ei_skip:ret                         ; /

;##########################################################################################
; { BLOCK 0058 } ...CONTINUED
; CODE BCDBUMP H POP, M A MOV, 1 ADI, DAA, A E MOV,
; wpb! CALL, NEXT
; -->
;##########################################################################################
;******************************************************************************************
; ----> BCDBUMP TERSE Word: Increments a BCD byte directly in protected NVRAM.
;******************************************************************************************
_BCDBUMP:   pop     hl                  ; Pop memory address
            ld      a,(hl)              ; Read current BCD byte
            add     a,$01               ; Add 1
            daa                         ; Decimal Adjust Accumulator (Keep it BCD)
            ld      e,a                 ; Move new value to E
            call    wpb_bang            ; Safely write byte back to NVRAM
            DW      _DSPATCH

;##########################################################################################
; { BLOCK 0059 } ( POINT WRITE ROUTINE STUFF )
; HEX
; ( SUBROUTINE TO DRAW A POINT )
; SUBR UPPOINT DI, H A MOV, C0 CPI, RNC, D A MOV, 50 CPI, RNC,
; C A MOV, RRC, RRC, A B MOV, 20 C MVI, ( FUDGE B )
; relabs CALL, C A MOV, MAGIC OUT, B M MOV, ( WRITE IT )
; EI, RET,
;##########################################################################################
;******************************************************************************************
; ----> UPPOINT Subroutine: Draws a single pixel (point) to the screen buffer.
;               Uses the Pattern Board's MAGIC register.
;
; IN:    HL = Y Coordinate
;        DE = X Coordinate
;        C  = Color / Magic Register Properties
;******************************************************************************************
UPPOINT:    di                  ; Disable interrupts
            ld      a,h         ; \
            cp      $C0         ;  | Bounds check Y coordinate (Must be < $C0)
            ret     nc          ; /  Return if out of bounds
            ld      a,d         ; \
            cp      $50         ;  | Bounds check X coordinate (Must be < $50)
            ret     nc          ; /  Return if out of bounds
            ld      a,c         ; Get Color / Magic properties
            rrca                ; \
            rrca                ; / Shift right 2 times
            ld      b,a         ; Save shifted value in B (FUDGE B)
            ld      c,$20       ; Set C to $20 for relabs calculation
            call    RELABS      ; Convert relative X/Y to absolute screen address
            ld      a,c         ; Get Magic properties back
            out     ($0C),a     ; MAGIC OUT (Hardware port $0C)
            ld      (hl),b      ; Write the pixel to the calculated screen address
            ei                  ; Re-enable interrupts
            ret

;##########################################################################################
; { BLOCK 0059 } ...CONTINUED
; CODE POINT EXX, B POP, H POP, D POP, UPPOINT CALL, EXX, NEXT
; DECIMAL -->
;##########################################################################################
;******************************************************************************************
; ----> POINT   TERSE Word: Pops X, Y, and Color from the stack and calls UPPOINT
;               to draw a single pixel.
;******************************************************************************************
_POINT:     exx
            pop     bc          ; Pop Color / Magic
            pop     hl          ; Pop Y Coordinate
            pop     de          ; Pop X Coordinate
            call    UPPOINT     ; Call low-level point drawing routine
            exx
            DW      _DSPATCH

;##########################################################################################
;   { BLOCKS 0060 TO 0073 }
;   ( MUSIC PROCESSOR VARIABLES, EQUATES, AND COMPILER MACROS )
;
;   NOTE: These 14 blocks reserve RAM space for the audio engine and define
;         compile-time macros used for music track generation.
;         They generate NO executable Z80 machine code in the physical ROM!
;##########################################################################################
;##########################################################################################
;   { BLOCK 0074 }
;   BTABLE MOTABLE 23 B, 22 B, 20 B, 1E B, 1C B, 1A B, 18 B, 17 B,
;   16 B, 15 B, 14 B, 13 B, 12 B, 11 B, 0D B, 0B B, -->
;##########################################################################################
MOTABLE:    DB      $23, $22, $20, $1E, $1C, $1A, $18, $17
            DB      $16, $15, $14, $13, $12, $11, $0D, $0B

;##########################################################################################
;   { BLOCK 0075 }
;   BTABLE sin-table
;   ( 00-10 ) ~ 255 255 255 255 254 253 252 251 250 248 247 ^
;   ( 11-21 ) ~ 245 243 241 239 237 234 231 229 226 223 220 ^
;   ( 22-32 ) ~ 216 213 209 206 202 198 194 190 185 181 177 ^
;   ( 33-43 ) ~ 172 167 162 157 152 147 142 137 132 126 121 ^
;   ( 44-54 ) ~ 115 109 104  98  92  86  80  74  68  62  56 ^
;   ( 55-63 ) ~  50  44  38  31  25  19  13   6   0         ^
;##########################################################################################
sin_table:  DB      $FF, $FF, $FF, $FF, $FE, $FD, $FC, $FB, $FA, $F8, $F7
            DB      $F5, $F3, $F1, $EF, $ED, $EA, $E7, $E5, $E2, $DF, $DC
            DB      $D8, $D5, $D1, $CE, $CA, $C6, $C2, $BE, $B9, $B5, $B1
            DB      $AC, $A7, $A2, $9D, $98, $93, $8E, $89, $84, $7E, $79
            DB      $73, $6D, $68, $62, $5C, $56, $50, $4A, $44, $3E, $38
            DB      $32, $2C, $26, $1F, $19, $13, $0D, $06, $00

;##########################################################################################
; { BLOCK 0075 } ...CONTINUED
; SUBR sin ( pass <, 0<= < <= 63, in E ) 0 D MVI,
; 0 sin-table H LXI, D DAD, M A MOV, RET,
;##########################################################################################
;******************************************************************************************
; ----> sin     Subroutine: Sine wave table lookup.
; IN:   E = Index (0 to 63)
; OUT:  A = Sine value from table
;******************************************************************************************
sin:        ld      d,$00               ; Clear D to make DE a clean 16-bit offset
            ld      hl,sin_table        ; Base address of sine table
            add     hl,de               ; Add offset to base address
            ld      a,(hl)              ; Read sine value into A
            ret

;##########################################################################################
; { BLOCK 0070 }
; ( HELPING SUBR's for MUSCPU *** NOTICE *** ) HEX
; ( The MUSPC rides in HL for the coarse of the MUSCPU )
; ( EACH MUSCPU LOADS ITS STARTING RAM IN IY )
; SUBR PCJUMP ( reload MUSPC )
;  M E MOV, H INX, M D MOV, XCHG, ( leave in HL )
;  L MUSPC Y STX, H MUSPC 1+ Y STX, ( store )   RET,
;##########################################################################################
pcjump:    ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ex      de,hl
            ld      (iy+$00),l
            ld      (iy+$01),h
            ret

;##########################################################################################
; SUBR portout ( pass value in A, port in C )
;  A E MOV, 17 A MVI, C CMP, ( all ports are 10-17 )
;  0>=, IF, ( check for bad values )
;  8 SUI, ( bottom ) C CMP, 0<, IF, ( oked )
;  18 A MVI, C SUB, SOUNDBOX Y SUBX, NEG, A C MOV, E OUTP,
;  THEN, THEN, A XRA, RET,
;##########################################################################################
portout:    ld      e,a
            ld      a,$17
            cp      c
            jp      m,portout_done
            sub     $08
            cp      c
            jp      p,portout_done
            ld      a,$18
            sub     c
            sub     (iy+$04)
            neg
            ld      c,a
            out     (c),e
portout_done:
            xor     a
            ret

;##########################################################################################
; SUBR babs ( byte absolute value )
;  7 A BIT, 0<>, IF, NEG, 7 A RES, THEN, RET,
;##########################################################################################
babs:       bit     7,a
            jp      z,babs_done
            neg
            res     7,a
babs_done:  ret

;##########################################################################################
; CODE BABS H POP, L A MOV, babs CALL, A L MOV, H PUSH, NEXT
;
; The source contains this CODE definition, but the corresponding release-ROM
; implementation is not present at this location.  Do not define _BABS with
; EQU $ here: the next emitted routine is halfvols, and that alias would create
; a false TERSE dictionary symbol.
;##########################################################################################

;##########################################################################################
; { BLOCK 0071 }
; ( HELPING SUBR's cont. ) HEX
; SUBR HALFVOLS ( pass A ) EXX, A B MOV, DEMOMODE LDA,
; A ANA, B A MOV, 0<>, IF, RRC, RRC, 33 ANI, THEN,
; EXX, RET,
;##########################################################################################
halfvols:  exx
            ld      b,a
            ld      a,(DEMOMODE)
            and     a
            ld      a,b
            jp      z,halfvols_done
            rrca
            rrca
            and     $33
halfvols_done:
            exx
            ret

;##########################################################################################
; { BLOCK 0071 } CONTINUTED
; SUBR LIMITCOUNT ( detect Music-State-transition if completed )
;  LIMCOUNTER Y A LDX, A ORA, 0<>, IF,
;  A DCR, A LIMCOUNTER Y STX, 0=, IF, ( done )
;  A RAMBLEFLAG Y STX, ( stop ramble ) 1 MST Y MVIX,
;  THEN, THEN, RET,
;##########################################################################################
limitcount:
            ld  a,(iy+$10)
            or      a
            jp      z,limitcount_done
            dec     a
            ld      (iy+$10),a
            jp      nz,limitcount_done
            ld      (iy+$0a),a
            ld      (iy+$2f),$01
limitcount_done:
            ret

;##########################################################################################
; { BLOCK 0072 }
; ( MUSIC PROCESSOR- emusic )
; DATA ENDMUS ASM PLAY
;
; SUBR emusic ( ** each EMUSIC passes vector addr in DE' )
;  MUSPC H LXI, D DAD, ENDMUS B LXI, C M MOV, H INX, B M MOV,
;  A XRA, 5 H LXI, D DAD, ( skip MUSPC,STARTPC,SOUNDBOX )
;  ENDMUSRAM BEGMUSRAM - 5 - B MVI,
;  BEGIN, A M MOV, H INX, B DCR, 0=, END,
;  SOUNDBOX H LXI, D DAD, M C MOV, 8 B MVI,
;  BEGIN, C DCR, A OUTP, B DCR, 0=, END,
;  EXX, RET,
; -->
;
; ENDMUS is the one-byte music opcode/data item at $0B85.  The old raw
; disassembly rendered its $03 byte as INC BC, which made emusic appear one
; byte too early.  Existing callers target $0B86, confirming the boundary.
;##########################################################################################
ENDMUS:     DB      $03

emusic:     ld      hl,$0000
            add     hl,de
            ld      bc,ENDMUS
            ld      (hl),c
            inc     hl
            ld      (hl),b
            xor     a
            ld      hl,$0005
            add     hl,de
            ld      b,$2B
            ld      (hl),a
            inc     hl
            dec     b
            jp      nz,$0B97
            ld      hl,$0004
            add     hl,de
            ld      c,(hl)
            ld      b,$08
            dec     c
            out     (c),a
            dec     b
            jp      nz,$0BA4
            exx
            ret

;##########################################################################################
; { BLOCK 0073 }
; ( OPCODE SUBR's, 0-4, ALL OPCODES LEAVE HL OK )
;
;  SUBR RANDOMNOTES H PUSH, ( save PC from RND )
;  0 D MVI, M E MOV, D PUSH, H INX, M E MOV, D PUSH, H INX,
;  M E MOV, random CALL, D POP, ( disp. ) D DAD, ( returns in HL )
;  B POP, L A MOV, portout CALL, H POP, 3 D LXI, D DAD, ( MUSPC )
;  A XRA, RET,
;##########################################################################################
randomnotes:
            push    hl
            ld      d,$00
            ld      e,(hl)
            push    de
            inc     hl
            ld      e,(hl)
            push    de
            inc     hl
            ld      e,(hl)
            call    random
            pop     de
            add     hl,de
            pop     bc
            ld      a,l
            call    portout
            pop     hl
            ld      de,$0003
            add     hl,de
            xor     a
            ret

;##########################################################################################
; SUBR LOADTIMER M A MOV, A NOTETIMER Y STX, H INX, ( A XRA,
;  A COMPDURATION Y STX, ) 1 ORI, RET,
; SUBR CONTJUMP M E MOV, H INX, M D MOV, XCHG, A XRA, RET,
; SUBR QUITJUMP ( H DCX, 3 in A ) RET,
;##########################################################################################

loadtimer: ld      a,(hl)
            ld      (iy+$2e),a
            inc     hl
            or      $01
            ret

contjump:  ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ex      de,hl
            xor     a
            ret

quitjump:  ret

;##########################################################################################
; SUBR QUITYET? ( QUIET ) MULTIPLE Y DCRX,
;  0<>, IF, STARTPC Y L LDX, STARTPC 1+ Y H LDX, A XRA,
;  ELSE, Y PUSHX, EXX, D POP, emusic CALL, 1 ORI, THEN, RET,
; -->
;##########################################################################################
quityet:   dec     (iy+$07)
            jp      z,$0BE7
            ld      l,(iy+$02)
            ld      h,(iy+$03)
            xor     a
            jp      $0BF0
            push    iy
            exx
            pop     de
            call    emusic
            or      $01
            ret

;##########################################################################################
; { BLOCK 0074 }
; ( OPCODE SUBR's,  5-6, HL= MUSPC )
; FORWARD RAMBLESTORES
;
; SUBR RAMBLIN' A XRA, ( turn off ramp flag )
;  LABEL RAMBLESTORES A RAMPFLAG Y STX,
;  M A MOV, H INX, A HIGHLIM Y STX,
;  M A MOV, H INX, A LOWLIM Y STX,
;  M A MOV, H INX, A STEP Y STX,
;  M A MOV, H INX, A RAMBLETIMER Y STX,
;  A TIMEBASE Y STX, 1 A MVI, A RAMBLEFLAG Y STX, A DCR, RET,
;##########################################################################################
ramblin:   xor     a
ramble_stores:
            ld      (iy+$09),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$0b),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$0c),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$0d),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$0e),a
            ld      (iy+$0f),a
            ld      a,$01
            ld      (iy+$0a),a
            dec     a
            ret

;##########################################################################################
; SUBR RAMPIN' 1 A MVI, RAMBLESTORES JMP,
; -->
;##########################################################################################
rampin:    ld      a,$01
            jp      ramble_stores

;##########################################################################################
; { BLOCK 0075 }
; ( OPCODE SUBR's, 8-0B, 10H )
;
; SUBR MASTART ( MASTER, 10H ) SOUNDBOX Y A LDX, 8 SUI, A C MOV,
;  M A MOV, H INX, A MOVALUE Y STX,
;  A OUTP, A XRA, RET
;##########################################################################################
mastart:    ld      a,(iy+$04)
            sub     $08
            ld      c,a
            ld      a,(hl)
            inc     hl
            ld      (iy+$05),a
            out     (c),a
            xor     a
            ret

;##########################################################################################
; SUBR RAMBLE-ON 1 A MVI, A RAMBLEFLAG Y STX, A XRA, RET,
;##########################################################################################
ramble_on:  ld      a,$01
            ld      (iy+$0a),a
            xor     a
            ret

;##########################################################################################
; SUBR RAMBLE-OFF A XRA, A RAMBLEFLAG Y STX, RET,
;##########################################################################################
ramble_off: xor     a
            ld      (iy+$0a),a
            ret

;##########################################################################################
; SUBR LIMITRAMBLE ( set up LIMCOUNTER )
;  1 A MVI, A RAMBLEFLAG Y STX, M A MOV, H INX,
;  A LIMCOUNTER Y STX, A XRA, RET,
;##########################################################################################
limitramble:
            ld      a,$01
            ld      (iy+$0a),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$10),a
            xor     a
            ret

;##########################################################################################
; SUBR STEPMOVIN' M A MOV, H INX, A STOPSTEPS Y STX,
;  M A MOV, H INX, A BIGOFASTEP Y STX, M A MOV, H INX,
;  A STEPTIMEBASE Y STX, A STEPTIMER Y STX, A XRA, RET,
;##########################################################################################
stepmovin:  ld      a,(hl)
            inc     hl
            ld      (iy+$1a),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$1b),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$1c),a
            ld      (iy+$1d),a
            xor     a
            ret

;##########################################################################################
; SUBR ABVOLIN' ( 16H ) RRC, A C MOV, M A MOV, 0F ANI,
;  HALFVOLS CALL, A E MOV, M A MOV, 0F0 ANI, HALFVOLS CALL,
;  0F0 ANI, E ORA, H INX, portout JMP,
; -->
;##########################################################################################
abvolin:    rrca
            ld      c,a
            ld      a,(hl)
            and     $0F
            call    halfvols
            ld      e,a
            ld      a,(hl)
            and     $F0
            call    halfvols
            and     $F0
            or      e
            inc     hl
            jp      portout

;##########################################################################################
; { BLOCK 0076 }
; ( OPCODES 0C-0F )
;
; The source comments in the earlier disassembly were displaced by one routine.
; Block 0079's OPADDRESSES table fixes the entry points unambiguously:
;   $0C69 LOWMOVIN', $0C7D HIGHMOVIN', $0C91 TBMOVIN', $0CA5 NOMOVIN'.
;##########################################################################################

;##########################################################################################
; SUBR LOWMOVIN' M A MOV, H INX, A STOPLOWLIM Y STX,
;  M A MOV, H INX, A LOWSTEP Y STX, M A MOV, H INX, A LOW# Y STX,
;  A LOWCOUNTER Y STX, A XRA, RET,
;##########################################################################################
lowmovin:   ld      a,(hl)
            inc     hl
            ld      (iy+$1e),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$1f),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$20),a
            ld      (iy+$21),a
            xor     a
            ret

;##########################################################################################
; SUBR HIGHMOVIN' M A MOV, H INX, A STOPHIGHLIM Y STX,
;  M A MOV, H INX, A HIGHSTEP Y STX, M A MOV, H INX,
;  A HIGH# Y STX, A HIGHCOUNTER Y STX, A XRA, RET,
;##########################################################################################
highmovin:  ld      a,(hl)
            inc     hl
            ld      (iy+$22),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$23),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$24),a
            ld      (iy+$25),a
            xor     a
            ret

;##########################################################################################
; SUBR TBMOVIN' M A MOV, H INX, A STOPTB Y STX, M A MOV, H INX,
;  A TBSTEP Y STX, M A MOV, H INX, A TBTB Y STX, A TBTIMER Y STX,
;  A XRA, RET,
;##########################################################################################
tbmovin:    ld      a,(hl)
            inc     hl
            ld      (iy+$11),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$12),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$13),a
            ld      (iy+$14),a
            xor     a
            ret

;##########################################################################################
; SUBR NOMOVIN' M A MOV, H INX, A NOSTOP Y STX, M A MOV, H INX,
;  A NOSTEP Y STX, M A MOV, H INX, A NOTIMER Y STX,
;  A NOTIMEBASE Y STX, SOUNDBOX Y C LDX, C DCR,
;  M A MOV, H INX, A NOVALUE Y STX, A OUTP, A XRA, RET,
; -->
;##########################################################################################
nomovin:    ld      a,(hl)
            inc     hl
            ld      (iy+$15),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$16),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$17),a
            ld      (iy+$18),a
            ld      c,(iy+$04)
            dec     c
            ld      a,(hl)
            inc     hl
            ld      (iy+$19),a
            out     (c),a
            xor     a
            ret

;##########################################################################################
;       { BLOCK 0077 }
;       ( OPCODES 11-16 I/O PORT OUTPUTS and PAN COUNTING, 1AH ) HEX
;
;
;##########################################################################################
; SUBR OPPORT ( 11H-14H, 17H )
; RRC, A C MOV, M A MOV, H INX, portout JMP,
;##########################################################################################
opport:     rrca
            ld      c,a
            ld      a,(hl)
            inc     hl
            jp      portout

;##########################################################################################
;       SUBR MCMOVIN' ( 15H ) RRC, A C MOV, M A MOV, 0F0 ANI, A E MOV,
;       M A MOV, 0F ANI,
;       HALFVOLS CALL, E ORA, H INX, A MCTRACKER Y STX, portout JMP,
;##########################################################################################
mcmovin:    rrca
            ld      c,a
            ld      a,(hl)
            and     $F0
            ld      e,a
            ld      a,(hl)
            and     $0F
            call    halfvols
            or      e
            inc     hl
            ld      (iy+$2b),a
            jp      portout

;##########################################################################################
;       SUBR NOISEPORT ( 17H )
;        RRC, A C MOV, M A MOV, H INX, A NOVALUE Y STX, portout JMP,
;##########################################################################################
noiseport:  rrca
            ld      c,a
            ld      a,(hl)
            inc     hl
            ld      (iy+$19),a
            jp      portout

;##########################################################################################
;       SUBR SOUNDMOVIN' ( 18H ) RET, ( M E MOV, H INX, E LEFTPAN Y STX,
;        H PUSH, PANOUTS CALL, H POP,
;        M A MOV, H INX, A PANSTEP Y STX,
;        M A MOV, H INX, A PANTIMEBASE Y STX,
;        A PANTIMER Y STX, FF PANCOUNTER Y MVIX, A XRA, RET, )
;##########################################################################################
soundmovin: ret                         ; Stereo pan opcode is disabled in Program 2

;##########################################################################################
;       SUBR PANLIMITCOUNTIN' ( 19 ) RET, -->
;       ( M A MOV, H INX, A PANCOUNTER Y STX,
;        PANTIMEBASE Y A LDX, A PANTIMER Y STX, A XRA, RET, )
;##########################################################################################
panlimitcountin:    ret                 ; Stereo pan-limit opcode is disabled in Program 2

;##########################################################################################
;       { BLOCK 0078 }
;       ( STEREO OPCODE 1A, THUMPER 1B, MUSIC GENERATOR 07H ) HEX
;##########################################################################################

;##########################################################################################
;       SUBR VOLMOVIN' ( 1AH )
;        M A MOV, HALFVOLS CALL, H INX, A VOLHIGHLIM Y STX,
;        M A MOV, HALFVOLS CALL, H INX, A VOLOWLIM Y STX,
;        M A MOV, H INX, A VOLSTEP Y STX,
;        M A MOV, H INX, A VOLTIMEBASE Y STX,
;        1 VOLTIMER Y MVIX, A XRA, RET,
;##########################################################################################
volmovin:   ld      a,(hl)
            call    halfvols
            inc     hl
            ld      (iy+$26),a
            ld      a,(hl)
            call    halfvols
            inc     hl
            ld      (iy+$27),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$28),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$29),a
            ld      (iy+$2a),$01
            xor     a
            ret

;##########################################################################################
;       SUBR MOHITTIN' ( 1B )
;        1 SYNCMO Y MVIX, ( turn on THUMPER-sync ) 3 A MVI, THUMPCOUNTER
;        STA, M A MOV, H INX, A STARTMC Y STX, A XRA, RET,
;##########################################################################################
mohittin:   ld      (iy+$2c),$01
            ld      a,$03
            ld      ($D0B0),a
            ld      a,(hl)
            inc     hl
            ld      (iy+$2d),a
            xor     a
            ret

;##########################################################################################

;       SUBR MUSICIN' ( 07 ) RET, -->
;        M A MOV, H INX, A COMPDURATION Y STX, 1 NOTETIMER Y MVIX,
;        M A MOV, H INX, A COMPSTEP Y STX, COMPTB COMPTIMER Y MVIX,
;        4 ATRACKER Y MVIX, 9 BTRACKER Y MVIX, 0E CTRACKER Y MVIX,
;        8 MOTRACKER Y MVIX, A XRA, RET, ( zero )
;       -->
;##########################################################################################
musicin:    ret


;##########################################################################################
;
;       { BLOCK 0079 }
;       ( OPCODE ADDRESS TABLE and FORWARDS ) HEX
;              1B C= #-OF-OPCODES
;       F= process F= endprocess F= MUSEND
;       TABLE OPADDRESSES
;             RANDOMNOTES , LOADTIMER , CONTJUMP , QUITJUMP ,
;             QUITYET? , RAMBLIN' , RAMPIN' , MUSICIN' ,
;             RAMBLE-ON , RAMBLE-OFF , LIMITRAMBLE , STEPMOVIN' ,
;             LOWMOVIN' , HIGHMOVIN' , TBMOVIN' , NOMOVIN' ,
;             MASTART , 3 0 << OPPORT , >>
;             OPPORT , MCMOVIN' , ABVOLIN' , NOISEPORT ,
;             SOUNDMOVIN' , PANLIMITCOUNTIN' , VOLMOVIN' , MOHITTIN' ,
;       -->
;##########################################################################################

                ; No code here. Informational block only.

;##########################################################################################
;
;       { BLOCK 0080 }
;       ( COMPMUSIC's +-disp., 15MOD, NOTABLE and THUMPLOCATION ) HEX
;       -->
;
;       ( BTABLE THUMPLOCATION ( where to locate sound in stereo image )
;        ( 3F B, 2A B, 15 B, 00 B, )
;       BTABLE NOTABLE ASM ( 3 octave range )
;        #G0 B, #A0 B, #B0 B, #C1 B, #D1 B, #E1 B, #FS1 B,
;        #G1 B, #A1 B, #B1 B, #C2 B, #D2 B, #E2 B, #FS2 B,
;        #G2 B, #A2 B, #B2 B, #C3 B, #D3 B, #E3 B, #FS3 B,
;
;
;
;        SUBR +-disp. ( change A to a + or - 3-bit # )
;        0 A BIT, 0<>, IF, ( neg ) F8 ORI, ELSE, 7 ANI, THEN, RET,
;
;
;
;       SUBR 15MOD ( base 21decimal ) 15 CPI, 0>=, IF, 7 SUI,
;        ( only adjust note down 1 octave ) THEN, A ORA, 0<, IF,
;        ( adjust up 1 octave ) 7 ADI, THEN, RET,
;
;
;        SUBR UP-AN-OUT ( pass index in A ) EXX,
;        0 NOTABLE D LXI, 0 H MVI, A L MOV,
;        D DAD, C INR, M A MOV, A OUTP, ( outp note ) EXX, RET,
;
;##########################################################################################

                ; RE NOTE: Block 0080 declares +-disp., 15MOD, and UP-AN-OUT,
                ; but no byte-exact implementation has been identified in the
                ; Program 2 release image. Keep the source declarations as
                ; provenance; do not attach these names to unrelated ROM bytes.

;##########################################################################################

;       { BLOCK 0081 }
;       ( STEREO STUFF, LIMITCOUNTING ) -->
;       ( PANLIMITS- achieving limits of volumes per channel )
;
;       SUBR PANLIMIT ( A&E= LEFTVOL, D=tb, H=counter, L=stepvol )
;         H INR, 0<>, IF, ( wasn't FF ) H DCR, H DCR, ( counted )
;          0=, IF, ( counted down )
;           1 MST Y MVIX, ( detect state transition ) 0 D MVI,
;          THEN, ELSE, H DCR, ( back to FF )
;         THEN, A XRA, L SUB, ( change step sign ) A L MOV,
;        RET, ( zero or non-zero )
;       -->
;
;##########################################################################################
                ; RE NOTE: Block 0081 declares PANLIMIT, but no byte-exact
                ; implementation has been identified in the Program 2 release
                ; image. The name remains intentionally unattached rather than
                ; inferred from nearby native code.




;******************************************************************************************
; OPADDRESSES is the music interpreter's opcode vector table from Block 0079.
; The 56 bytes here were previously disassembled as plausible-looking Z80 instructions,
; but they are 28 little-endian routine pointers for opcodes $00 through $1B.
;******************************************************************************************
OPADDRESSES:
            DW      randomnotes         ; $00
            DW      loadtimer           ; $01
            DW      contjump            ; $02
            DW      quitjump            ; $03
            DW      quityet             ; $04
            DW      ramblin             ; $05
            DW      rampin              ; $06
            DW      musicin             ; $07
            DW      ramble_on           ; $08
            DW      ramble_off          ; $09
            DW      limitramble         ; $0A
            DW      stepmovin           ; $0B
            DW      lowmovin            ; $0C
            DW      highmovin           ; $0D
            DW      tbmovin             ; $0E
            DW      nomovin             ; $0F
            DW      mastart             ; $10
            DW      opport              ; $11
            DW      opport              ; $12
            DW      opport              ; $13
            DW      opport              ; $14
            DW      mcmovin             ; $15
            DW      abvolin             ; $16
            DW      noiseport           ; $17
            DW      soundmovin          ; $18
            DW      panlimitcountin     ; $19
            DW      volmovin            ; $1A
            DW      mohittin            ; $1B

;******************************************************************************************
; ----> muscpu  Interrupt-time music processor update.
;
;               IY selects one of the two 48-byte processor work areas:
;                   $D0B1 = processor 1 / Astrocade sound ports $10-$17
;                   $D0E1 = processor 2 / Astrocade sound ports $50-$57
;
;               MST (IY+$2F) separates score interpretation from timed synthesis updates.
;               While MST is clear, this routine advances timers, ramps, modulation, and
;               noise motion and writes the resulting register values to the selected sound
;               block. It raises MST when the score interpreter must consume another opcode.
;******************************************************************************************
muscpu:     xor     a
            cp      (iy+$2f)
            jp      z,$0D5C
            ret

;*******************************************************************

            cp      (iy+$2e)
            jp      z,$0D6C
            dec     (iy+$2e)
            jp      nz,$0D6C
            ld      (iy+$2f),$01
            xor     a
            cp      (iy+$0a)
            jp      z,$0E5F
            dec     (iy+$0e)
            jp      nz,$0E5F
            ld      l,(iy+$0d)
            ld      a,(iy+$04)
            sub     $08
            ld      c,a
            ld      a,(iy+$05)
            add     a,l
            ld      (iy+$05),a
            out     (c),a
            ld      d,a
            xor     a
            cp      (iy+$2c)
            jp      z,$0DB2
            exx
            ld      a,(iy+$2d)
            ld      (iy+$2b),a
            ld      a,(iy+$0f)
            and     $F0
            rrca
            rrca
            rrca
            rrca
            inc     a
            ld      (iy+$29),a
            ld      hl,$D0B0
            dec     (hl)
            ld      c,(hl)
            jp      nz,$0DB1
            ld      (hl),$04
            exx
            ld      a,(iy+$0c)
            dec     a
            cp      d
            jp      nc,$0E06
            ld      a,(iy+$24)
            or      a
            jp      z,$0DDE
            dec     (iy+$25)
            jp      nz,$0DDE
            ld      c,a
            ld      a,(iy+$0b)
            add     a,(iy+$23)
            cp      (iy+$22)
            jp      nz,$0DD8
            ld      (iy+$24),$00
            ld      (iy+$0b),a
            ld      (iy+$25),c
            call    limitcount
            ld      a,(iy+$09)
            cp      $03
            jp      nz,$0DF1
            ld      a,$01
            ld      (iy+$09),a
            jp      $0E06
            or      a
            jp      z,$0E02
            inc     (iy+$09)
            ld      a,(iy+$0b)
            sub     l
            ld      (iy+$05),a
            jp      $0E06
            sub     l
            ld      (iy+$0d),a
            ld      a,(iy+$0b)
            cp      d
            jp      c,$0E59
            ld      a,(iy+$20)
            or      a
            jp      z,$0E31
            dec     (iy+$21)
            jp      nz,$0E31
            ld      c,a
            ld      a,(iy+$0c)
            add     a,(iy+$1f)
            cp      (iy+$1e)
            jp      nz,$0E2B
            ld      (iy+$20),$00
            ld      (iy+$0c),a
            ld      (iy+$21),c
            call    limitcount
            ld      a,(iy+$09)
            cp      $02
            jp      nz,$0E42
            dec     (iy+$09)
            jp      $0E59
            or      a
            jp      z,$0E55
            ld      a,$03
            ld      (iy+$09),a
            ld      a,(iy+$0c)
            sub     l
            ld      (iy+$05),a
            jp      $0E59
            sub     l
            ld      (iy+$0d),a
            ld      a,(iy+$0f)
            ld      (iy+$0e),a
            ld      a,(iy+$2a)
            or      a
            jp      z,$0EB0
            dec     a
            jp      nz,$0EAD
            ld      d,(iy+$2b)
            ld      a,d
            and     $F0
            ld      e,a
            ld      a,d
            sub     e
            ld      l,(iy+$28)
            add     a,l
            ld      d,a
            ld      h,(iy+$26)
            cp      h
            jp      m,$0E86
            ld      d,h
            xor     a
            sub     l
            ld      l,a
            jp      $0E92
            dec     a
            ld      h,(iy+$27)
            cp      h
            jp      p,$0E92
            ld      d,h
            xor     a
            sub     l
            ld      l,a
            ld      (iy+$28),l
            ld      a,d
            rrca
            rrca
            rrca
            rrca
            add     a,d
            ld      c,(iy+$04)
            dec     c
            dec     c
            out     (c),a
            dec     c
            ld      a,d
            or      e
            ld      (iy+$2b),a
            out     (c),a
            ld      a,(iy+$29)
            ld      (iy+$2a),a
            ld      a,(iy+$1d)
            or      a
            jp      z,$0EDD
            dec     (iy+$1d)
            jp      nz,$0EDD
            ld      a,(iy+$0d)
            ld      e,a
            call    babs
            add     a,(iy+$1b)
            cp      (iy+$1a)
            jp      z,$0ED3
            ld      a,(iy+$1c)
            ld      (iy+$1d),a
            bit     7,e
            jp      z,$0EDA
            neg
            ld      (iy+$0d),a
            ld      a,(iy+$12)
            or      a
            jp      z,$0EFF
            dec     (iy+$14)
            jp      nz,$0EFF
            add     a,(iy+$0f)
            ld      (iy+$0f),a
            sub     (iy+$11)
            jp      nz,$0EF9
            ld      (iy+$12),a
            ld      a,(iy+$13)
            ld      (iy+$14),a
            ld      a,(iy+$17)
            or      a
            jp      z,$0F27
            dec     (iy+$17)
            jp      nz,$0F27
            ld      c,(iy+$04)
            dec     c
            ld      a,(iy+$19)
            add     a,(iy+$16)
            ld      (iy+$19),a
            out     (c),a
            cp      (iy+$15)
            jp      z,$0F27
            ld      a,(iy+$18)
            ld      (iy+$17),a
            ret

;******************************************************************************************
; ----> musinterp  Score-bytecode interpreter for the processor selected by IY.
;
;                  A non-zero MST requests score processing. MUSPC is loaded into HL and
;                  opcodes $00-$1B dispatch through OPADDRESSES. Handlers returning zero
;                  continue immediately with the next opcode; a non-zero return commits
;                  MUSPC, clears MST, and returns control to the timed music processor.
;******************************************************************************************
musinterp:  ld      a,(iy+$2f)
            or      a
            jp      nz,process
            ret

process:    ld      l,(iy+$00)
            ld      h,(iy+$01)
process_next_opcode:
            ld      a,(hl)
            inc     hl
            cp      $1C                 ; Valid score opcodes are $00-$1B
            jp      nc,process_bad_opcode
            exx
            ld      hl,endprocess
            push    hl
            ld      hl,OPADDRESSES
            rlca
            ld      e,a
            ld      d,$00
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            push    de
            exx
            ret

;*******************************************************************

            jp      endprocess
process_bad_opcode:
            or      $01                 ; Invalid opcode terminates this interpreter pass
endprocess: or      a
            jp      z,process_next_opcode
            ld      (iy+$00),l          ; Commit MUSPC returned by the opcode handler
            ld      (iy+$01),h
musend:     ld      (iy+$2f),$00        ; MST=0: timed synthesis updates may run
            ret

;******************************************************************************************
; ----> muscpus  Run timed synthesis for both Astrocade sound blocks.
;                MUSICFLAG gates the complete music/sound sequencer.
;******************************************************************************************
muscpus:    ld      a,(MUSICFLAG)
            or      a
            jp      z,muscpus_done
            push    iy
            ld      iy,$D0B1            ; Processor 1 -> ports $10-$17
            call    muscpu
            ld      iy,$D0E1            ; Processor 2 -> ports $50-$57
            call    muscpu
            pop     iy
muscpus_done:
            ret

;******************************************************************************************
; ----> busaround  Advance pending score-bytecode work for both music processors.
;                  Each musinterp call runs only when that processor's MST is non-zero.
;******************************************************************************************
busaround:  ld      a,(MUSICFLAG)
            or      a
            jp      z,busaround_done
            push    iy
            ld      iy,$D0B1            ; Processor 1 score interpreter
            call    musinterp
            ld      iy,$D0E1            ; Processor 2 score interpreter
            call    musinterp
            pop     iy
busaround_done:
            ret

;******************************************************************************************
; ----> BMS     TERSE CODE word for background music score processing.
;******************************************************************************************
_BMS:       push    bc
            call    busaround
            pop     bc
            DW      _DSPATCH

;******************************************************************************************
; ----> loadpc  Install a new score address as both MUSPC and STARTPC.
;******************************************************************************************
loadpc:     ld      (iy+$00),l
            ld      (iy+$02),l
            ld      (iy+$01),h
            ld      (iy+$03),h
            ret

;*******************************************************************
; Music score start controls.
;
; bmusic / mmusic start only when PRIORITY (IY+$08) is clear. pmusic / mpmusic
; clear the selected processor first, then install the new score as priority. The m*
; forms take MULTIPLE in E; the other forms set MULTIPLE=1. Successful starts finish
; through loadpc, keeping MUSPC and STARTPC synchronized.

bmusic:     ld      a,(iy+$08)          ; PRIORITY
            or      a
            jp      nz,bmusic_done      ; Do not replace a priority score
            ld      (iy+$2f),$01        ; MST: request score interpretation
            ld      (iy+$2e),a          ; NOTETIMER = 0
            inc     a
            ld      (iy+$07),a          ; MULTIPLE = 1
            jp      loadpc
bmusic_done:
            ret

pmusic:     ld      (iy+$2f),$01        ; Hold timed updates during reinitialization
            push    iy
            exx
            pop     de
            call    emusic
            ld      a,$01
            ld      (iy+$2f),a          ; MST = 1: interpret the new score
            ld      (iy+$07),a          ; MULTIPLE = 1
            ld      (iy+$08),a          ; PRIORITY = 1
            jp      loadpc

mmusic:     ld      a,(iy+$08)          ; PRIORITY
            or      a
            jp      nz,mmusic_done      ; Do not replace a priority score
            ld      (iy+$2f),$01        ; MST: request score interpretation
            ld      (iy+$2e),a          ; NOTETIMER = 0
            ld      (iy+$07),e          ; MULTIPLE supplied by caller
            jp      loadpc
mmusic_done:
            ret

mpmusic:    ld      (iy+$2f),$01        ; Hold timed updates during reinitialization
            push    iy
            exx
            pop     de
            call    emusic
            ld      a,$01
            ld      (iy+$2f),a
            ld      (iy+$08),a
            ld      (iy+$07),e
            jp      loadpc

;******************************************************************************************

_EMUSIC:    exx                         ; TERSE CODE: initialize music processor 1
            ld      de,$D0B1            ; Base of MUSIC-BARRAY-1
            ld      hl,$002F            ; MST field
            add     hl,de               ; $D0E0
            ld      (hl),$01            ; Hold score processing during initialization
            ld      hl,$0004            ; SOUNDBOX field
            add     hl,de               ; $D0B5
            ld      (hl),$18            ; Processor 1 maps to ports $10-$17
            call    emusic
            DW      _DSPATCH

;******************************************************************************************

_BMUSIC:    pop     hl
            push    iy
            ld      iy,$D0B1
            call    bmusic
            pop     iy
            DW      _DSPATCH

;******************************************************************************************

_PMUSIC:    pop     hl
            push    iy
            ld      iy,$D0B1
            call    pmusic
            pop     iy
            DW      _DSPATCH

;******************************************************************************************

_MMUSIC:    pop     hl
            pop     de
            push    iy
            ld      iy,$D0B1
            call    mmusic
            pop     iy
            DW      _DSPATCH

;******************************************************************************************

_MPMUSIC:   pop     hl
            pop     de
            push    iy
            ld      iy,$D0B1
            call    mpmusic
            pop     iy
            DW      _DSPATCH

;##########################################################################################
;  { BLOCK 0097 }
;   ( MUSIC PROCESSOR- E2MUSIC, B2MUSIC, ... )
;   CODE E2MUSIC EXX, 0 MUSIC-BARRAY-2 D LXI, ( pass to emusic )
;    MST H LXI, D DAD, 1 M MVI, ( make non-zero )
;    SOUNDBOX H LXI, D DAD, CHIP2 M MVI,
;    emusic CALL, ( musicoverun flag is zeroed last ) NEXT
;   ( *** ALWAYS CALL E2MUSIC AS AN INIT IN PROGRAM *** )
;##########################################################################################
_E2MUSIC:   exx                         ; TERSE CODE: initialize music processor 2
            ld      de,$D0E1            ; Base of MUSIC-BARRAY-2
            ld      hl,$002F            ; MST field
            add     hl,de               ; $D110
            ld      (hl),$01            ; Hold score processing during initialization
            ld      hl,$0004            ; SOUNDBOX field
            add     hl,de               ; $D0E5
            ld      (hl),$58            ; Processor 2 maps to ports $50-$57
            call    emusic
            DW      _DSPATCH

;##########################################################################################
;   CODE B2MUSIC H POP, Y PUSHX,
;    0 MUSIC-BARRAY-2 Y LXIX, bmusic CALL, Y POPX, NEXT
;##########################################################################################
_B2MUSIC:   pop     hl
            push    iy
            ld      iy,$D0E1
            call    bmusic
            pop     iy
            DW      _DSPATCH

;##########################################################################################
;   CODE P2MUSIC H POP, Y PUSHX,
;    0 MUSIC-BARRAY-2 Y LXIX, pmusic CALL, Y POPX, NEXT
;##########################################################################################
_P2MUSIC:   pop     hl
            push    iy
            ld      iy,$D0E1
            call    pmusic
            pop     iy
            DW      _DSPATCH

;##########################################################################################
;   CODE M2MUSIC H POP, D POP, Y PUSHX,
;    0 MUSIC-BARRAY-2 Y LXIX, mmusic CALL, Y POPX, NEXT
;##########################################################################################
_M2MUSIC:   pop     hl
            pop     de
            push    iy
            ld      iy,$D0E1
            call    mmusic
            pop     iy
            DW      _DSPATCH

;##########################################################################################
;   CODE MP2MUSIC H POP, D POP, Y PUSHX,
;    0 MUSIC-BARRAY-2 Y LXIX, mpmusic CALL, Y POPX, NEXT
;##########################################################################################
_MP2MUSIC:  pop     hl
            pop     de
            push    iy
            ld      iy,$D0E1
            call    mpmusic
            pop     iy
            DW      _DSPATCH

;##########################################################################################
;   : SHUTUP EMUSIC E2MUSIC ; -->
;##########################################################################################
SHUTUP:     DB      _ENTER
            DW      _EMUSIC
            DW      _E2MUSIC
            DW      _RETURN

;******************************************************************************************
; GORF VOTRAX SPEECH ARCHITECTURE
;
; A TALK PRIM is the unit queued by SPEAK. Each primitive is stored as:
;
;       DB  phoneme_count
;       DB  phoneme_0, phoneme_1, ... phoneme_(count-1)
;
; The low six bits select the SC-01 phoneme. Source modifiers HI ($80) and UP ($40)
; are ORed into the phoneme byte to carry the inflection control used by the game.
;
; The resident English path queues primitive addresses in an eight-entry circular
; pointer queue spanning TOPTALK through BOTTOMTALK. TALKIN is the producer pointer;
; TALKOUT is the consumer pointer. PHONE services that queue from the periodic interrupt,
; sending one phoneme whenever NEWPHONE reports the SC-01 ready.
;
; Program 2 exposes 36 resident speech keys:
;       26 low-ROM primitives     $115D-$1317
;        3 Flag Ship primitives  $A985-$A9C1
;        7 late-ROM primitives   $B3BE-$B465
;
; Phrase composition occurs above the primitive layer. Several game phrases are complete
; TALK PRIM records, while others are assembled by queuing multiple primitives. The rank
; construction is the clearest example:
;
;       SPK_SPACE -> one of SPK_CADET/SPK_CAPT/SPK_COLONEL/
;                    SPK_GENERAL/SPK_WARRIOR/SPK_AVENGER
;
; SETTINGS bit 3 selects the resident English path. When it is clear, SPEAK transfers the
; primitive request to the language ROM entry at FOREIGN_SPEECH_ENTRY ($C000).
;
; Three additional Program-2 TALK PRIM records live in the upper ROM and form the Flag Ship
; completion announcement:
;
;       SPK_FLAGSHIP_INTRO        $A985  "Next time will be harder, but for now"
;       SPK_GORFIAN_CHRONICLES    $A9A8  "In the Gorfian chronicles"
;       SPK_FLAGSHIP_HIT          $A9C1  "For hitting my flagship"
;
; They are speech keys in the same resident-address namespace used by the language ROM.
;******************************************************************************************

;##########################################################################################
; { BLOCK 0098 }
;  ( SCOT'S VOTRAX TALKING CHIP SOFTWARE for JAY )
;  ( IMPORTANT EQUATES ) HEX
;  8 C= #TALKSTK ( size of stack for primitive talk statements )
;  3F C= STOP ( phoneme to stop the chip's talking )
;  17 C= PHONEOUT ( port to stuff phoneme into )
;  12 C= NEWPHONE ( port to read for phoneme request )
;
;  CODE BONE H POP, 1 M MVI, NEXT
;  CODE BZERO H POP, 0 M MVI, NEXT
;  -->
;##########################################################################################
_BONE:      pop     hl
            ld      (hl),$01
            DW      _DSPATCH

_BZERO:     pop     hl
            ld      (hl),$00
            DW      _DSPATCH


;******************************************************************************************
;
;  { BLOCK 0099 }
;   ( VOTRAX RAM and VOCABULARY STUFF )
;   0 BV= ONHOLD ( timer to time out before talking )
;   0 V= TOPTALK ( top stack entry )
;   VPTR @ #TALKSTK 2- -1 MAX 2 * + VPTR !
;   0 V= BOTTOMTALK ( bottom speech queue entry )
;   0 V= TALKHERE ( holds phoneme address to talk from )
;   0 BV= PHONE# ( # of phonemes in primitive statement )
;   0 V= TALKIN ( holds stack address to stuff next primitive into
;  )0 V= TALKOUT ( holds stack address to talk from )
;   0 BV= MSYRND 0 BV= HTYRND ( ANTIDOUBLETALK NUMBER TRACKERS )
;   CC? IFTRUE VOCABULARY TALKING TERSE DEFINITIONS
;    { : TALK } UNMAP TALKING { ; }
;    UNMAP HERE there @ DP ! ( put behind the screen )
;   TALKING DEFINITIONS OTHERWISE { : TALK } { ; } IFEND
;   -->
;
;******************************************************************************************

                ; Source-level RAM and vocabulary declarations.
                ; The release image emits no executable bytes for this block.

;******************************************************************************************
;
; { BLOCK 0100 }
; ( TALKING PHONEME EQUATES ) HEX
; 00 C= EH3 01 C= EH2 02 C= EH1 03 C= PA0 04 C= DT
; 05 C= pA2 06 C= pA1 07 C= ZH 08 C= AH2
; 09 C= I3 0A C= I2 0B C= I1
; 0C C= M 0D C= N 0E C= pB 0F C= V 10 C= CH 11 C= SH 12 C= Z
; 13 C= AW1 14 C= NG 15 C= AH1 16 C= OO1 17 C= OO
; 18 C= L 19 C= K0 1A C= J0 1B C= H
; 1C C= G 1D C= pF 1E C= pD 1F C= S
; 20 C= pA 21 C= AY 22 C= Y1 23 C= UH3 24 C= AH 25 C= P
; 26 C= O 27 C= I0 28 C= U 29 C= Y 2A C= T 2B C= R 2C C= pE
; 2D C= W 2E C= pAE 2F C= pAE1 30 C= AW2 31 C= UH2 32 C= UH1
; 33 C= UH 34 C= O2 35 C= O1 36 C= IU 37 C= U1
; 38 C= THV 39 C= TH 3A C= ER 3B C= EH 3C C= pE1 3D C= AW
; 3E C= PA1 ( 3F C= STOP )
; { : HI } 80 OR { ; } { : UP } 40 OR { ; }
; -->
;
;******************************************************************************************

                ; Source-level SC-01 phoneme constants and inflection helpers.
                ; The release image emits no executable bytes for this block.

;******************************************************************************************
; { BLOCK 0101 }
; ( BTABLE HELPING VERBS )
; { : PRIM } ( DATA nnnn TALK PRIM sets up a primitive )
;  HERE ~ ( mark stack ) 0 { ; }
; { : ENDPRIM } ( stuff primitive's size ) ^ ( clear stack off )
;  DUP ( here ) HERE SWAP - 1 - SWAP BU! { ; }
; CC? IFTRUE HERE there ! DP ! TERSE DEFINITIONS IFEND
;******************************************************************************************

                ; Source compiler helpers for constructing TALK PRIM records.
                ; PRIM marks the start of a primitive; ENDPRIM writes the phoneme count.
                ; These definitions emit no executable bytes at this location.

;##########################################################################################
; SUBR speaklink .REL 10 IN, 80 ANI, RZ,
; DI, TALKIN LHLD, E M MOV, H INX, D M MOV, H INX,
; XCHG, BOTTOMTALK H LXI, A ANA, D DSBC, <, IF, TOPTALK D LXI,
; THEN, TALKIN SDED, EI, RET,
;##########################################################################################
;******************************************************************************************
; ----> speaklink   Queue one resident English TALK PRIM.
;
; In:    DE = address of a TALK PRIM
;
; Port $10 bit 7 is the source-defined gate for the resident speech request. When the
; gate is clear, the request is discarded. When set, the primitive address is written to
; the slot selected by TALKIN, TALKIN advances by one word, and the producer pointer wraps
; from BOTTOMTALK back to TOPTALK.
;
; The queue stores primitive pointers, not phoneme bytes. Interrupts are disabled only
; while the producer pointer and queue entry are updated.
;******************************************************************************************
speaklink:  in      a,($10)
            and     $80
            ret     z

            di
            ld      hl,(TALKIN)
            ld      (hl),e              ; Queue primitive address, low byte
            inc     hl
            ld      (hl),d              ; Queue primitive address, high byte
            inc     hl                  ; HL = next producer slot
            ex      de,hl               ; DE = next producer slot

            ld      hl,BOTTOMTALK
            and     a
            sbc     hl,de
            jr      nc,speaklink_store
            ld      de,TOPTALK          ; Wrap after final queue slot

speaklink_store:
            ld      (TALKIN),de
            ei
            ret

;##########################################################################################
; HEX SUBR speak SETTINGS IN, 8 ANI, speaklink JNZ, 0C000 JMP,
; .ABS
;##########################################################################################
;******************************************************************************************
; ----> speak       Route one TALK PRIM request to the selected language implementation.
;
; In:    DE = address of the resident English TALK PRIM
;
; SETTINGS bit 3 set:
;       Queue the resident Program-2 English primitive through speaklink.
;
; SETTINGS bit 3 clear:
;       Transfer control to the language ROM at FOREIGN_SPEECH_ENTRY ($C000). The foreign
;       module receives the same logical speech request and provides its own language data.
;******************************************************************************************
speak:      in      a,(SETTINGS)
            and     $08
            jp      nz,speaklink
            jp      FOREIGN_SPEECH_ENTRY

;########################################################################################
; CODE SPEAK D POP, speak CALL, NEXT
; DECIMAL -->
;########################################################################################
;******************************************************************************************
; ----> SPEAK       TERSE interface to speak.
;                   Pops a TALK PRIM address and submits it to the active language path.
;******************************************************************************************
_SPEAK:     pop     de
            call    speak
            DW      _DSPATCH

;##########################################################################################
; { BLOCK 0102 }
; ( SUBR TO EMBED IN INTERRUPTS ) HEX
; ASM SUBR PHONE ONHOLD LDA, A ORA, 0=, IF,
;   NEWPHONE IN, 7 A BIT, <>, IF, ( chip's ready for more )
;    PHONE# LDA, A ORA, 0=, IF, ( primitive's done )
;     TALKIN LHLD, TALKOUT LDED, D DSBC, <>, IF, ( more to say )
;  0D15 B LXI, A INP,
;     XCHG, M E MOV, H INX, M D MOV, ( next prim's addr )
;     XCHG, M A MOV, ( PHONE# ) H INX, TALKHERE SHLD,
;  ELSE, 3F00 PHONEOUT + B LXI, A INP, 0C15 B LXI, A INP, RET,
;  THEN,   ELSE, TALKHERE LHLD, THEN, A DCR, 0=, IF,
;   EXX, TALKOUT LHLD, 2 D LXI, D DAD, XCHG, BOTTOMTALK H LXI,
;   A ORA, D DSBC, <, IF, TOPTALK D LXI, THEN, TALKOUT SDED, EXX,
;   THEN, PHONE# STA, M B MOV, ( next phoneme ) H INX,
;  PHONEOUT C MVI, A INP, TALKHERE SHLD, THEN, ELSE,
;  A DCR, ONHOLD STA, THEN, RET, -->
;##########################################################################################
;******************************************************************************************
; ----> phone       Interrupt-time Votrax queue service.
;
; The periodic interrupt calls PHONE after the music/sound service. PHONE performs at most
; one SC-01 phoneme transfer per call.
;
; ONHOLD:
;       Non-zero values delay speech. The counter is decremented and no queue work occurs.
;
; NEWPHONE ($12), bit 7:
;       SC-01 ready indication. If the chip is not ready, PHONE returns immediately.
;
; PHONECOUNT:
;       Number of phonemes remaining in the active primitive after the phoneme about to be
;       sent. A zero count causes PHONE to fetch the next primitive pointer from TALKOUT.
;
; TALKOUT:
;       Advances by one word when the final phoneme of a primitive is loaded, wrapping from
;       BOTTOMTALK to TOPTALK.
;
; Queue empty:
;       When TALKIN == TALKOUT and no primitive is active, PHONE issues STOP ($3F) on the
;       speech I/O cycle and returns.
;******************************************************************************************
phone:      ld      a,(ONHOLD)
            or      a
            jp      nz,phone_hold

            in      a,($12)             ; NEWPHONE: SC-01 ready status
            bit     7,a
            jp      z,phone_return_gate

            ld      a,(PHONECOUNT)
            or      a
            jp      nz,phone_continue_primitive

            ld      hl,(TALKIN)
            ld      de,(TALKOUT)
            sbc     hl,de               ; Carry is already clear from PHONECOUNT OR A above
            jp      z,phone_queue_empty

            ld      bc,$0D15            ; Source-defined auxiliary I/O cycle before a primitive
            in      a,(c)

            ex      de,hl               ; HL = TALKOUT queue slot
            ld      e,(hl)
            inc     hl
            ld      d,(hl)              ; DE = queued TALK PRIM address
            ex      de,hl               ; HL = TALK PRIM
            ld      a,(hl)              ; Primitive phoneme count
            inc     hl
            ld      (TALKHERE),hl        ; First phoneme address
            jp      phone_count_ready

phone_queue_empty:
            ld      bc,$3F17            ; STOP phoneme ($3F) on PHONEOUT ($17)
            in      a,(c)
            ld      bc,$0C15            ; Source-defined auxiliary I/O cycle after STOP
            in      a,(c)
            ret

phone_count_ready:
            jp      phone_decrement_count

phone_continue_primitive:
            ld      hl,(TALKHERE)

phone_decrement_count:
            dec     a
            jp      nz,phone_send

            exx
            ld      hl,(TALKOUT)
            ld      de,$0002
            add     hl,de               ; Advance consumer to next queue slot
            ex      de,hl
            ld      hl,BOTTOMTALK
            or      a
            sbc     hl,de
            jp      nc,phone_store_talkout
            ld      de,TOPTALK

phone_store_talkout:
            ld      (TALKOUT),de
            exx

phone_send:
            ld      (PHONECOUNT),a
            ld      b,(hl)              ; B = encoded SC-01 phoneme
            inc     hl
            ld      c,$17               ; PHONEOUT
            in      a,(c)               ; I/O cycle presents phoneme in upper address byte
            ld      (TALKHERE),hl

phone_return_gate:
            jp      phone_return        ; Preserve the ROM control-flow join at $1155

phone_hold:
            dec     a
            ld      (ONHOLD),a

phone_return:
            ret

;******************************************************************************************
; RESIDENT ENGLISH TALK PRIMITIVES
;
; Each record begins with an 8-bit phoneme count followed by that many encoded SC-01
; phoneme bytes. The low six bits select the phoneme; bit 7 (HI) and bit 6 (UP) carry
; the source-defined inflection modifiers.
;
; The DB records below are the released Program-2 speech data. The source listings provide
; the phoneme mnemonics and primitive names; the emitted bytes define the exact playback.
;
;                   SC-01 PHONEME TABLE
;
;       00 EH3    01 EH2    02 EH1    03 PA0    04 DT
;       05 pA2    06 pA1    07 ZH     08 AH2    09 I3
;       0A I2     0B I1     0C M      0D N      0E pB
;       0F V      10 CH     11 SH     12 Z      13 AW1
;       14 NG     15 AH1    16 OO1    17 OO     18 L
;       19 K0     1A J0     1B H      1C G      1D pF
;       1E pD     1F S      20 pA     21 AY     22 Y1
;       23 UH3    24 AH     25 P      26 O      27 I0
;       28 U      29 Y      2A T      2B R      2C pE
;       2D W      2E pAE    2F pAE1   30 AW2    31 UH2
;       32 UH1    33 UH     34 O2     35 O1     36 IU
;       37 U1     38 THV    39 TH     3A ER     3B EH
;       3C pE1    3D AW     3E PA1    3F STOP
;
;       HI = $80
;       UP = $40
;******************************************************************************************
;##########################################################################################
;       DATA 'INSERT TALK PRIM
;       PA1 I0 N S ER T PA1 K0 O1 UH3 I3 AY N N PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Insert coin."
; Role:   Complete attract-mode utterance; GOYTBL entries 0 and 3.
;******************************************************************************************

SPK_INSERT:
            DB      $0F                 ; Length of phrase
            DB      $3E, $27, $0D, $1F, $3A, $2A, $3E    ; INSERT
            DB      $19, $35, $23, $09, $21, $0D, $0D, $3E    ; COIN

;##########################################################################################
;       DATA 'GORF TALK PRIM PA1
;       AH1 I1 UP Y UP pAE M UP THV UH1 UP G DT O1 R UP pF UP Y pA1 N
;       EH1 UP M UP P AH1 I1 R PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "I am the Gorfian Empire."
; Role:   Complete utterance; used by attract, mission-start, and last-ship follow-up speech.
;******************************************************************************************

SPK_GORF:
            DB      $17                 ; Length of phrase
            DB      $3E, $15, $4B, $69, $2E, $4C, $38, $72
            DB      $1C, $04, $35, $6B, $5D, $29, $06, $0D
            DB      $42, $4C, $25, $15, $0B, $2B, $3E

;##########################################################################################
;       DATA 'SPACE TALK PRIM
;       S P pA1 I3 UP Y UP S UP ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Space"
; Role:   Rank-prefix fragment. GETRANK queues this before the selected rank primitive.
;******************************************************************************************

 SPK_SPACE:
            DB      $06
            DB      $1F, $25, $06, $49, $69, $5F

;##########################################################################################
;       DATA 'CONQUER TALK PRIM
;       G DT O1 UP R UP pF UP Y UP pA1 N S K0 AH1 N K0 UP ER UP
;       AH2 N UP UH UP TH UP ER G UP pAE1 L UH1 K0 UP S Y PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Gorfians conquer another galaxy."
; Role:   Complete game-over follow-up; LBYTBL.
;******************************************************************************************

SPK_CONQUER:
            DB      $1B
            DB      $1C, $04, $75, $6B, $5D, $69, $06, $0D
            DB      $1F, $19, $15, $0D, $59, $7A, $08, $4D
            DB      $73, $79, $3A, $5C, $2F, $18, $32, $59
            DB      $1F, $29, $3E

;##########################################################################################
;       DATA 'TRY TALK PRIM
;       T R UP AH2 UP I1 UP Y UP PA0 UH1 G UP EH1 UP I3 N PA1 PA1 AH1
;       I1 UP Y1 UP pD Y V AH1 UP U1 ER K0 UP O1 UP UH3 I3 pE1 N S PA1
;##########################################################################################
;******************************************************************************************
; Speech: "Try again; I devour your coins."
; Role:   Complete game-over follow-up; LBYTBL.
;******************************************************************************************

SPK_TRY:
            DB      $1E
            DB      $2A, $6B, $48, $4B, $69, $03, $32, $5C
            DB      $42, $09, $0D, $3E, $3E, $15, $4B, $62
            DB      $1E, $29, $0F, $55, $37, $3A, $59, $75
            DB      $23, $09, $3C, $0D, $1F, $3E

;##########################################################################################
;       DATA 'LONG TALK PRIM
;       PA1 L AW UP NG UP L I1 UP V UP G UP DT UP O1 UP O1 UP R R pF pF
;##########################################################################################
;******************************************************************************************
; Speech: "Long live Gorf!"
; Role:   Complete attract/ready-to-play utterance; GOYTBL and SPKCOIN.
;******************************************************************************************

SPK_LONG:
            DB      $0F
            DB      $3E, $18, $7D, $54, $18, $4B, $4F, $5C
            DB      $44, $75, $75, $2B, $2B, $1D, $1D

;##########################################################################################
;       DATA 'ROBOTS TALK PRIM
;       G UP DT UP O1 UP R UP pF UP pE1 UP EH2 N
;       R UP O1 UP U1 UP pB AH1 UH3 T S PA1 PA1
;       UH1 T pAE EH3 UP K0 UP PA0 UH1 T pAE EH3 UP K0 UP PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Gorfian robots... attack! attack!"
; Role:   Complete mission-start utterance; SPEAKSTART.
;******************************************************************************************

SPK_ROBOTS:
            DB      $1E
            DB      $5C, $44, $75, $6B, $5D, $7C, $01, $0D
            DB      $6B, $75, $77, $0E, $15, $23, $2A, $1F
            DB      $3E, $3E, $32, $2A, $2E, $40, $59, $03
            DB      $32, $2A, $2E, $40, $59, $3E

;##########################################################################################
;       DATA 'BADMOVE TALK PRIM
;       pB pAE EH1 UP pD UP M UP U UP U1 V PA1
;       ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Bad move"
; Role:   Loss-of-ship prefix; HITYAK appends SPACE + current rank.
;******************************************************************************************

SPK_BADMOVE:
            DB      $09
            DB      $0E, $2E, $42, $5E, $4C, $68, $37, $0F, $3E

;##########################################################################################
;       DATA 'HA TALK PRIM
;       H UP AH1 UP H AH1 UP H AH1 UP H AH1 PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Ha ha ha ha!"
; Role:   Complete loss-of-ship taunt; HITYAK.
;******************************************************************************************

SPK_HAHA:
            DB      $09
            DB      $5B, $55, $1B, $55, $1B, $55, $1B, $15, $3E

;##########################################################################################
;       DATA 'ESCAPE TALK PRIM
;       Y1 I1 U1 UP U1 UP K0 pAE1 UP N UP UH1 T PA0 EH1 S K0 pA2 UP
;       pE1 UP P UP THV UH1 G DT O1 UP R UP pF UP Y UP pA1 N
;       PA0 R O1 UP U1 UP pB AH1 T S PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "You cannot escape the Gorfian robots."
; Role:   Complete utterance; mission-start and last-ship follow-up.
;******************************************************************************************

SPK_ESCAPE:
            DB      $23
            DB      $22, $0B, $77, $77, $19, $6F, $4D, $32
            DB      $2A, $03, $02, $1F, $19, $45, $7C, $65
            DB      $38, $32, $1C, $04, $75, $6B, $5D, $69
            DB      $06, $0D, $03, $2B, $75, $77, $0E, $15
            DB      $2A, $1F, $3E

;##########################################################################################
;       DATA 'GOTYOU TALK PRIM
;       G AH1 EH3 UP T UP Y1 UP I3 U1 PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Got you"
; Role:   Loss-of-ship prefix; HITYAK appends SPACE + current rank.
;******************************************************************************************

SPK_GOTYOU:
            DB      $08
            DB      $1C, $15, $40, $6A, $62, $09, $37, $3E

;##########################################################################################
;       DATA 'NICE TALK PRIM
;       N UH3 AH2 UP Y UP S UP PA0 UP
;       SH UP AH1 UP UH3 T PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Nice shot"
; Role:   Standalone resident speech primitive. Program 2 contains no direct symbolic
;         reference to this key in the decoded control flow.
;******************************************************************************************

SPK_NICE:
            DB      $0B
            DB      $0D, $23, $48, $69, $5F, $43, $51, $55, $23, $2A, $3E

;##########################################################################################
;       DATA 'TOOBAD TALK PRIM
;       T U UP pB UP pAE UP EH3 pD PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Too bad"
; Role:   Last-ship prefix; LBYAK appends SPACE + current rank.
;******************************************************************************************

SPK_TOOBAD:
            DB      $07
            DB      $2A, $68, $4E, $6E, $00, $1E, $3E

;##########################################################################################
;       DATA 'PRIS TALK PRIM
;       G DT O1 UP R UP pF Y pA1 N S T pA UP K0 UP N O UP
;       P R UP I1 UP S UP I3 UP N EH3 R S PA1 ENDPRIM -->
;##########################################################################################
;******************************************************************************************
; Speech: "Gorfians take no prisoners!"
; Role:   Complete mission-start utterance; SPEAKSTART.
;******************************************************************************************

SPK_PRIS:
            DB      $18
            DB      $1C, $04, $75, $6B, $1D, $29, $06, $0D
            DB      $1F, $2A, $60, $59, $0D, $66, $25, $6B
            DB      $4B, $5F, $49, $0D, $00, $2B, $1F, $3E

;##########################################################################################
;       DATA 'CADET TALK PRIM
;       K0 UH pD EH2 UP T UP PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Cadet"
; Role:   Rank suffix selected by GETRANK for SKILLFACTOR 0.
;******************************************************************************************

SPK_CADET:
            DB      $06
            DB      $19, $33, $1E, $41, $6A, $3E

;##########################################################################################
;       DATA 'CAPT TALK PRIM
;       K0 pAE1 UP EH3 UP P UP T I3 N PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Captain"
; Role:   Rank suffix selected by GETRANK for SKILLFACTOR 1.
;******************************************************************************************

SPK_CAPT:
            DB      $08
            DB      $19, $6F, $40, $65, $2A, $09, $0D, $3E

;##########################################################################################
;       DATA 'COLONEL TALK PRIM
;       K0 ER UP N UP AH2 L PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Colonel"
; Role:   Rank suffix selected by GETRANK for SKILLFACTOR 2.
;******************************************************************************************

SPK_COLONEL:
            DB      $06
            DB      $19, $7A, $4D, $08, $18, $3E

;##########################################################################################
;       DATA 'GENERAL TALK PRIM
;       pD J0 EH2 UP N UP ER UH3 L PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "General"
; Role:   Rank suffix selected by GETRANK for SKILLFACTOR 3.
;******************************************************************************************

SPK_GENERAL:
            DB      $08
            DB      $1E, $1A, $41, $4D, $3A, $23, $18, $3E

;##########################################################################################
;       DATA 'WARRIOR TALK PRIM
;       W O UP R UP AY UP Y1 UP EH3 R PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Warrior"
; Role:   Rank suffix selected by GETRANK for SKILLFACTOR 4.
;******************************************************************************************

SPK_WARRIOR:
            DB      $08
            DB      $2D, $66, $6B, $61, $62, $00, $2B, $3E

;##########################################################################################
;       DATA 'AVENGER TALK PRIM
;       UH1 V EH1 UP EH3 UP N UP N pD J0 ER PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Avenger"
; Role:   Rank suffix selected by GETRANK for SKILLFACTOR 5 and above.
;******************************************************************************************

SPK_AVENGER:
            DB      $0A
            DB      $32, $0F, $42, $40, $4D, $0D, $1E, $1A, $3A, $3E

;##########################################################################################
;       DATA 'PROMOTE TALK PRIM
;       Y1 IU U1 UP U1 UP H pAE1 EH3 UP V UP pB EH3 EH1 UP N UP
;       P R UH2 M O UP T UP EH3 pD PA0 T IU U1 PA0 ENDPRIM -->
;##########################################################################################
;******************************************************************************************
; Speech: "You have been promoted to"
; Role:   Promotion prefix. The Program-2 phrase is:
;             SPK_PROMOTE + SPK_SPACE + current rank
;         producing "You have been promoted to Space <rank>."
;         No decoded resident control-flow path directly references this primitive.
;******************************************************************************************

SPK_PROMOTE:
            DB      $19
            DB      $22, $36, $77, $77, $1B, $2F, $40, $4F
            DB      $0E, $00, $42, $4D
            DB      $25, $2B, $31, $0C
            DB      $66, $6A, $00, $1E, $03, $2A, $36, $37
            DB      $03

;##########################################################################################
;       DATA 'SOME TALK PRIM
;       S UH M G UH1 L pAE K0 T I1 K0 PA0 pD pE1 pF UP EH1 UP N pD ER
;       PA0 Y IU UP U1 UP U1 UP UH R PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Some galactic defender you are"
; Role:   Loss-of-ship prefix; HITYAK appends SPACE + current rank.
;******************************************************************************************

SPK_SOME:
            DB      $1B
            DB      $1F, $33, $0C, $1C, $32, $18
            DB      $2E, $19, $2A, $0B, $19, $03, $1E, $3C
            DB      $5D, $42, $0D, $1E, $3A, $03, $29, $76
            DB      $77, $77, $33, $2B, $3E

;##########################################################################################
;       DATA 'BITE TALK PRIM
;       pB AH2 I3 Y1 UP UP T UP THV UH UP pD UP UH UP S T PA1
;       ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Bite the dust"
; Role:   Last-ship prefix; LBYAK appends SPACE + current rank.
;******************************************************************************************

SPK_BITE:
            DB      $0C
            DB      $0E, $08, $09, $62, $6A, $38, $73, $5E
            DB      $73, $1F, $2A, $3E

;##########################################################################################
;       DATA 'HAIL TALK PRIM
;       AW L H pA1 UP AY UP L THV UH S U1 P UP R UP pE1 UP M PA1
;       G DT O1 UP R UP F Y pA1 N PA1 EH UP M UP P AH2 I3 Y R PA1
;       ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "All hail the supreme Gorfian Empire!"
; Role:   Complete game-over follow-up; LBYTBL.
;******************************************************************************************

SPK_HAIL:
            DB      $20
            DB      $3D, $18, $1B, $46, $61, $18, $38, $33
            DB      $1F, $37, $65, $6B, $7C, $0C, $3E, $1C
            DB      $04, $75, $6B, $0F, $29, $06, $0D, $3E
            DB      $7B, $4C, $25, $08, $09, $29, $2B, $3E

;##########################################################################################
;       DATA 'ENEMY TALK PRIM
;       AH1 N UH THV R UP R UP PA0 EH1 N EH1 M Y
;       SH I0 P PA0 pD UP pE1 UP S UP T R O1 I1 Y1 pD PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Another enemy ship destroyed!"
; Role:   Complete loss-of-ship taunt; HITYAK.
;******************************************************************************************

SPK_ENEMY:
            DB      $1A
            DB      $15, $0D, $33, $38, $6B, $6B, $03, $02
            DB      $0D, $02, $0C, $29, $11, $27, $25, $03
            DB      $5E, $7C, $5F, $2A, $2B, $35, $0B, $22
            DB      $1E, $3E

;##########################################################################################
; DATA 'BETCHA TALK PRIM
;  Y O UP R UP EH N pD PA0 pD R AW1 S N I0 UP R ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Your end draws near"
; Role:   Loss-of-ship prefix; HITYAK appends SPACE + current rank.
;******************************************************************************************

SPK_BETCHA:
            DB      $0E
            DB      $29, $66, $6B, $3B, $0D, $1E, $03
            DB      $1E, $2B, $13, $1F, $0D, $67, $2B

;#########################################################################
; { GORFOS - BLOCK 0109 }
; ( GETRANK, COINSOUND, GOYAK, AND COINYAK )
; TABLE RKTBL 'CADET , 'CAPT , 'COLONEL , 'GENERAL , 'WARRIOR ,
; 'AVENGER , : GETRANK SKILLFACTOR B@ 5 MIN
; 'SPACE SPEAK RKTBL @ ;
;#########################################################################

RKTBL:      DW      SPK_CADET, SPK_CAPT, SPK_COLONEL, SPK_GENERAL, SPK_WARRIOR, SPK_AVENGER

;******************************************************************************************
; ----> GETRANK    Queue the word "Space" and return the primitive for the current rank.
;
; SKILLFACTOR is clamped to 5, so every value at or above Space Avenger resolves to the
; Avenger primitive. GETRANK deliberately does not speak the rank primitive itself: callers
; receive that address on the TERSE stack and decide when to queue it.
;
; Phrase produced after the caller speaks the returned primitive:
;       "Space Cadet" / "Space Captain" / "Space Colonel" /
;       "Space General" / "Space Warrior" / "Space Avenger"
;******************************************************************************************
_GETRANK:   DB      _ENTER
            DW      _LIT
            DW      SKILLFACTOR
            DW      _Bat
            DW      _LITbyte
            DB      $05
            DW      _MIN
            DW      _LIT
            DW      SPK_SPACE
            DW      _SPEAK
            DW      _ARRAY
            DW      RKTBL
            DW      _at
            DW      _RETURN

;******************************************************************************************
; GORFOS BLOCK 0109 - ATTRACT SPEECH AND COIN SOUND
;******************************************************************************************

; Attract-mode speech selection table used by goyak. The low two bits selected from the
; Z80 refresh register index one of these four primitive speech records.
GOYTBL:     DW      SPK_INSERT          ; "Insert coin."
            DW      SPK_GORF            ; "I am the Gorfian Empire."
            DW      SPK_LONG            ; "Long live Gorf!"
            DW      SPK_INSERT          ; "Insert coin."

; Coin sound score consumed by the native music opcode interpreter.
;
; Source score: VIBS $86; MASTER $10; RAMP $03,$01,$3C,$10; TONES G1,E1,C2;
;               ABVOLS $FF; MCVOLS $0F; DURATION $40; QUIET.
COINSOUND1: DB      $14,$86             ; VIBS $86
coinsound1_continue:
            DB      $10,$10             ; MASTER $10
            DB      $06,$10,$3C,$01,$03 ; RAMP
            DB      $13,$5E,$12,$96,$11,$7E ; TONES G1, E1, C2
            DB      $16,$FF             ; ABVOLS $FF
            DB      $15,$0F             ; MCVOLS $0F
            DB      $01,$40             ; DURATION $40
            DB      $04                 ; QUIET

; COINSOUND2 is a CONTJUMP stream. Its target is coinsound1_continue, so it enters
; COINSOUND1 immediately after the opening VIBS command. goyak uses this entry while
; synchronizing the attract joystick effect with delayed speech.
COINSOUND2: DB      $02,$56,$13

;******************************************************************************************
; ----> CNSD    Start COINSOUND1 on music processor 2 through B2MUSIC.
;               B2MUSIC is non-preemptive: an active priority score is preserved.
;******************************************************************************************
_CNSD:      DB      _ENTER
            DW      _LIT
            DW      COINSOUND1
            DW      _B2MUSIC
            DW      _RETURN

;******************************************************************************************
; ----> goyak   Attract-mode joystick speech driver.
;
;               creditcheck calls goyak while DEMOMODE is active. Joystick movement starts
;               one GOYTBL phrase and sets GOYFLAG to prevent immediate retriggering. When
;               the speech queue is idle, goyak loads ONHOLD=$3B and priority-starts
;               COINSOUND2 on processor 1 before queuing the selected speech primitive.
;******************************************************************************************
goyak:      ld      a,(GOYFLAG)
            and     a
            ret     nz                  ; One attract phrase is already active
            call    gj                  ; Read active player's joystick
            and     $0F
            ret     z                   ; No movement
            ld      a,$01
            ld      (MUSICFLAG),a       ; Enable both music processors

            ld      a,(PHONECOUNT)      ; Active primitive still has phonemes pending
            and     a
            jp      nz,goyak_select_phrase
            ld      hl,(TALKIN)         ; Producer pointer
            ld      de,(TALKOUT)        ; Consumer pointer
            sbc     hl,de
            jp      nz,goyak_select_phrase

            ld      a,$3B
            ld      (ONHOLD),a           ; Delay speech until the synchronized effect is underway
            ld      hl,COINSOUND2
            ld      iy,$D0B1            ; Music processor 1
            call    pmusic              ; Priority-start synchronized coin effect

goyak_select_phrase:
            ld      a,r
            and     $03
            rlca                        ; Word-table offset: (R & 3) * 2
            ld      e,a
            ld      d,$00
            ld      hl,GOYTBL
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      d,(hl)              ; DE = selected TALK primitive
            ld      a,$01
            ld      (GOYFLAG),a
            jp      speak

;******************************************************************************************
; GORFOS BLOCK 0110 - PLAYER TAUNT SPEECH
;******************************************************************************************

; Follow-up phrase table used by LBYAK after the player's last fire base is destroyed.
LBYTBL:     DW      SPK_CONQUER         ; "Gorfians conquer another galaxy."
            DW      SPK_TRY             ; "Try again; I devour your coins."
            DW      SPK_ESCAPE          ; "You cannot escape the Gorfian robots."
            DW      SPK_GORF            ; "I am the Gorfian Empire."
            DW      SPK_HAIL            ; "All hail the supreme Gorfian Empire!"

;******************************************************************************************
; ----> LBYAK   Last-ship / last-fire-base taunt sequence.
;
;               PLAYERHITCHECK enters KILLLAST when FBCOUNTER reaches zero. KILLLAST calls
;               LBYAK during the final fire-base destruction sequence.
;
;               LBYAK queues one of:
;                   "Too bad, Space <rank>."
;                   "Bite the dust, Space <rank>."
;
;               It then queues one random game-over follow-up from LBYTBL.
;******************************************************************************************
_LBYAK:     DB      _ENTER
            DW      _LITbyte
            DB      $02
            DW      _RND
            DW      _0BRANCH
            DW      lbyak_bite
            DW      _LIT
            DW      SPK_TOOBAD
            DW      _BRANCH
            DW      lbyak_continue
lbyak_bite: DW      _LIT
            DW      SPK_BITE
lbyak_continue:
            DW      _SPEAK
            DW      _GETRANK
            DW      _SPEAK
            DW      _LITbyte
            DB      $05
            DW      _RND
            DW      _ARRAY
            DW      LBYTBL
            DW      _at
            DW      _SPEAK
            DW      _RETURN

; Non-repeating loss-of-ship taunt table. HTYRND stores the previous table index.
HITYTBL:    DW      SPK_HAHA            ; "Ha ha ha ha!"                         (complete)
            DW      SPK_ENEMY           ; "Another enemy ship destroyed!"        (complete)
            DW      SPK_BETCHA          ; "Your end draws near, Space <rank>."
            DW      SPK_BADMOVE         ; "Bad move, Space <rank>."
            DW      SPK_GOTYOU          ; "Got you, Space <rank>."
            DW      SPK_SOME            ; "Some galactic defender you are, Space <rank>."

;******************************************************************************************
; ----> HITYAK  Surviving-fire-base hit taunt.
;
;               PLAYERHITCHECK calls HITYAK during its eight-step post-hit sequence while
;               at least one fire base remains. UNEQRND prevents the immediately preceding
;               HITYTBL selection from repeating.
;
;               Entries 0-1 are complete phrases. Entries 2-5 append SPACE + current rank.
;******************************************************************************************
_HITYAK:    DB      _ENTER
            DW      _LIT
            DW      HTYRND              ; Previous HITYTBL selection
            DW      _LITbyte
            DB      $06
            DW      _UNEQRND
            DW      _DUP
            DW      _1
            DW      _gt
            DW      _0BRANCH
            DW      hityak_no_rank
            DW      _ARRAY
            DW      HITYTBL
            DW      _at
            DW      _SPEAK
            DW      _GETRANK
            DW      _BRANCH
            DW      hityak_final_speak
hityak_no_rank:
            DW      _ARRAY
            DW      HITYTBL
            DW      _at
hityak_final_speak:
            DW      _SPEAK
            DW      _RETURN

;******************************************************************************************
; { BLOCK 0111 }
; ( CLEAR BLOCK COMMAND - USES THE PATTERN BOARD )
; HEX ( THIS VERSION CLEARS A 1K BLOCK )
; 0 BV= PIXVAL
;
; SUBR clear1k 20 A MVI, PBSTAT OUT, PIXVAL H LXI,
; 0 M MVI, L A MOV, PBLINADRL OUT, H A MOV, PBLINADRH OUT,
; E A MOV, PBAREADRL OUT, D A MOV, PBAREADRH OUT,
; 1 A MVI, PBXMOD OUT,
; 7F A MVI, PBXWIDE OUT, 7 A MVI, PBYHIGH OUT, RET,
; F= CLLL
;******************************************************************************************

clear1k:    ld      a,$20
            out     (PBSTAT),a
            ld      hl,$D12B
            ld      (hl),$00
            ld      a,l
            out     ($78),a
            ld      a,h
            out     ($79),a
            ld      a,e
            out     ($7B),a
            ld      a,d
            out     ($7C),a
            ld      a,$01
            out     ($7B),a
            ld      a,$7F
            out     ($7D),a
            ld      a,$07
            out     ($7E),a
            ret

;******************************************************************************************
; { BLOCK 0111 }  ...Continued
; CODE CL 4000 D LXI, <ASSEMBLE
; LABEL CLLL clear1k CALL, D A MOV, 4 ADI, A D MOV, CLLL JP,
; NEXT ASSEMBLE>
; DECIMAL -->
;******************************************************************************************

_CL:        ld      de,$4000
CLLL:       call    clear1k
            ld      a,d
            add     a,$04
            ld      d,a
            jp      p,CLLL
            DW      _DSPATCH

;******************************************************************************************
; Terse routine just returns - why ?
;******************************************************************************************

DONULL:     DB      _ENTER
            DW      _RETURN

;******************************************************************************************
; GORF_WO_SPEECH {Block 105 }
; XY 100 * SWAP 40 * SWAP ;
;******************************************************************************************

_XY:        DB      _ENTER
            DW      _LIT
            DW      $0100
            DW      _star

            DW      _SWAP

            DW      _LITbyte
            DB      $40
            DW      _star
            DW      _SWAP

            DW      _RETURN

            DW      $E9E1
            DW      _DSPATCH
;******************************************************************************************
; {Block 105 } ... Continued
; INIT GRAPHICS 1 8 OUTP 204 10 OUTP 43 9 OUTP ;
;******************************************************************************************

W_1476:
            DB      _ENTER
            DW      DONULL		; do nothing ?
            DW      _1                  ; OUT $01 to port $08 (Hi-Res)
            DW      _LITbyte
            DB      $08
            DW      _OUTP
            DW      _LITbyte
            DB      $CC
            DW      _LITbyte
            DB      $0A                 ; OUT $CC to port $0A (Vert Blank)
            DW      _OUTP
            DW      _LITbyte
            DB      $2B
            DW      _LITbyte
            DB      $09
            DW      _OUTP               ; OUT $2B to port $09 (pallette split)
            DW      _RETURN
;******************************************************************************************

            ld      b,(ix+$1c)
            ld      c,(ix+$1b)
            ld      d,(ix+$0e)
            ld      e,(ix+$0d)
            ld      h,(ix+$1e)
            ld      l,(ix+$1d)
            push    hl
            ex      (sp),iy
            ld      h,(ix+$14)
            ld      l,(ix+$13)
            call    FFRELABS
            ld      (ix+$1a),h
            ld      (ix+$19),l
            call    writep
            ld      (ix+$2c),c
            pop     iy
            ret

            ld      b,(ix+$1c)
            ld      c,(ix+$2c)
            ld      h,(ix+$1e)
            ld      l,(ix+$1d)
            push    hl
            ex      (sp),iy
            ld      h,(ix+$1a)
            ld      l,(ix+$19)
            call    writep
            pop     iy
            ret

            rst     $08
            halt
            nop
            jr      $147D
            nop
            ld      b,h
            ld      (bc),a
            halt
            nop
            ld      b,b
            adc     a,l
            ld      (bc),a
            rla
            inc     bc
            adc     a,b
            nop
            ld      b,b
            out     ($76),a
            nop
            ld      b,b
            adc     a,l
            ld      (bc),a
            sbc     a,(hl)
            nop
            ld      de,$1701
            inc     bc
            adc     a,b
            nop
            ld      b,b
            out     ($2A),a
            ld      bc,$00F7
            ld      e,a
            ld      (bc),a
            sbc     a,b
            nop
            halt
            nop
            jr      $14A6
            nop
            ld      de,$7601
            nop
            ld      b,b
            rla
            inc     bc
            adc     a,b
            nop
            ld      b,b
            out     ($2A),a
            ld      bc,$00F7
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      b,b
            out     ($6D),a
            nop
            ld      b,b
            exx
            rst     $30
            nop
            ld      h,c
            nop


            ld      hl,($D940)
            ld      a,l
            or      h
            jp      z,$153B
            inc     hl
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            dec     hl
            dec     hl
            ld      ($D940),de
            jp      $153C
            jp      (hl)
            ret

            di
            call    $1526
            push    hl
            ei
            DW      _DSPATCH

;******************************************************************************************

            ld      de,($D940)
            ld      ($D940),hl
            inc     hl
            ld      (hl),e
            inc     hl
            ld      (hl),d
            ret

;******************************************************************************************

            ld      e,(iy+$00)
            ld      d,(iy+$01)
            ld      a,e
            or      d
            jp      z,$157C
            ld      c,(iy+$02)
            ld      b,(iy+$03)
            push    hl
            inc     hl
            ld      (hl),e
            inc     hl
            ld      (hl),d
            inc     hl
            ld      (hl),c
            inc     hl
            ld      (hl),b
            ex      de,hl
            pop     de
            inc     hl
            inc     hl
            inc     hl
            ld      (hl),e
            inc     hl
            ld      (hl),d
            ld      a,e
            inc     bc
            ld      (bc),a
            ld      a,d
            inc     bc
            ld      (bc),a
            jp      $158C
            ld      e,l
            ld      d,h
            inc     hl
            ld      (hl),e
            inc     hl
            ld      (hl),d
            inc     hl
            ld      (hl),e
            inc     hl
            ld      (hl),d
            ld      (iy+$02),e
            ld      (iy+$03),d
            ld      (iy+$00),e
            ld      (iy+$01),d
            ret

;******************************************************************************************
; ----> L1593       Fixed busy-wait delay followed by a cold restart.
;******************************************************************************************
L1593:      ld      e,$06
L1595:      dec     l
            jp      nz,L1595
            dec     h
            jp      nz,L1595
            dec     e
            jp      nz,L1595
            jp      COLDSTRT            ; Reset system

;******************************************************************************************

            ld      e,(iy+$00)
            ld      d,(iy+$01)
            ld      l,(iy+$02)
            ld      h,(iy+$03)
            xor     a
            sbc     hl,de
            jp      nz,$15C5
            ld      (iy+$00),a
            ld      (iy+$01),a
            ld      (iy+$02),a
            ld      (iy+$03),a
            jp      $160B
            ld      e,(ix+$01)
            ld      d,(ix+$02)
            ld      c,(ix+$03)
            ld      b,(ix+$04)
            ld      l,c
            ld      h,b
            inc     hl
            ld      (hl),e
            inc     hl
            ld      (hl),d
            ld      hl,$0003
            add     hl,de
            ld      (hl),c
            inc     hl
            ld      (hl),b
            push    ix
            pop     hl
            ex      de,hl
            push    hl
            ld      l,(iy+$00)
            ld      h,(iy+$01)
            and     a
            sbc     hl,de
            pop     hl
            jp      nz,$15F9
            ld      (iy+$00),l
            ld      (iy+$01),h
            jp      $160B
            ld      l,(iy+$02)
            ld      h,(iy+$03)
            and     a
            sbc     hl,de
            jp      nz,$160B
            ld      (iy+$02),c
            ld      (iy+$03),b
            ret
            exx
            pop     hl
            ex      (sp),iy
            push    ix
            push    hl
            pop     ix
            call    $15A4
            pop     ix
            pop     iy
            exx
            DW      _DSPATCH
;******************************************************************************************
            ld      l,(iy+$00)
            ld      h,(iy+$01)
            ld      a,h
            or      l
            ret     z
            inc     hl
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            dec     hl
            dec     hl
            ld      (iy+$02),l
            ld      (iy+$03),h
            ld      (iy+$00),e
            ld      (iy+$01),d
            ret
            ld      l,(iy+$00)
            ld      h,(iy+$01)
            ld      a,h
            or      l
            ret     z
            ld      e,l
            ld      d,h
            push    de
            ld      de,$0007
            add     hl,de
            ld      a,(hl)
            add     a,c
            ld      (hl),a
            and     a
            sbc     hl,de
            pop     de
            inc     hl
            ld      a,(hl)
            inc     hl
            ld      h,(hl)
            ld      l,a
            xor     e
            ld      b,a
            ld      a,h
            xor     d
            or      b
            jp      nz,$1646
            ret
;******************************************************************************************
; write byte in E to protected memory
;******************************************************************************************
_PWB:       pop     hl
            pop     de
            call    wpb_bang            ; Write byte to protected memory
            DW      _DSPATCH
;******************************************************************************************
; write byte 01 to protected memory
;******************************************************************************************
_P1:        pop     hl
            ld      e,$01
            call    wpb_bang            ; Write byte to protected memory
            DW      _DSPATCH
;******************************************************************************************
; write byte 00 to protected memory
;******************************************************************************************
_P0:        pop     hl
            ld      e,$00
            call    wpb_bang            ; Write byte to protected memory
            DW      _DSPATCH
;******************************************************************************************
; decrement byte in protected memory
;******************************************************************************************
_PDEC:
            pop     hl
            ld      e,(hl)
            dec     e
            call    wpb_bang            ; Write byte to protected memory
            DW      _DSPATCH
;******************************************************************************************
; add to byte in protected memory
;******************************************************************************************
_PADD:      pop     hl
            ld      a,(hl)
            pop     de
            add     a,e
            ld      e,a
            call    wpb_bang            ; Write byte to protected memory
            DW      _DSPATCH
;******************************************************************************************
; increment byte in protected memory
;******************************************************************************************
_PINC:      pop     hl
            ld      e,(hl)
            inc     e
            call    wpb_bang            ; Write byte to protected memory
            DW      _DSPATCH
;******************************************************************************************
; sub from byte in protected memory
;******************************************************************************************
_PSUB:      pop     hl
            ld      a,(hl)
            pop     de
            sub     e
            ld      e,a
            call    wpb_bang            ; Write byte to protected memory
            DW      _DSPATCH
;******************************************************************************************
            and     $0F
            ld      e,a
            rlca
            rlca
            rlca
            rlca
            or      e
            ld      e,a
            cpl
            ld      d,a
            jp      $0A8D
            ld      a,(COMBO1)
            inc     a
            ld      hl,COMBO1
            call    $169B
            ld      a,r
            ld      hl,COMBO2
            call    $169B
            ret


;******************************************************************************************
; SUBR CCHECK H A MOV, L ADD, A INR, RNZ, L A MOV, RLC, RLC,
; RLC, RLC, L XRA, RET,
; F= WPZAP F= WPNOZ
;******************************************************************************************

ccheck:     ld      a,h
            add     a,l
            inc     a
            ret     nz
            ld      a,l
            rlca
            rlca
            rlca
            rlca
            xor     l
            ret

;******************************************************************************************
;
; CODE WPCLEAR <ASSEMBLE EXX, ( save BC )
;  LDAR, 0 RND# STA, ( init RND for convenience )
; COMBO1 LHLD, CCHECK CALL, WPZAP JRNZ, COMBO2 LHLD, CCHECK CALL,
; WPNOZ JRZ,

; LABEL WPZAP WPRAMSTART H LXI, 0 E MVI, 80 B MVI,
; BEGIN, wpb! CALL, H INX, LOOP,
; -->
;
;******************************************************************************************
_WPCLEAR    EQU     $

            exx
            ld      a,r                 ; Get a random number seed
            ld      (RND_SEED),a

            ld      hl,(COMBO1)
            call    ccheck
            jr      nz,WPZAP
            ld      hl,(COMBO2)
            call    ccheck
            jr      z,WPNOZ

WPZAP:      ld      hl,WPRAMSTART       ; Zero out first 128 ($80) bytes of static RAM
            ld      e,$00
            ld      b,$80
WPZAP1:     call    wpb_bang
            inc     hl
            djnz    WPZAP1

            xor     a
            ld      (PHONECOUNT),a      ; No active primitive
            ld      (ONHOLD),a          ; No speech holdoff
            ld      hl,TOPTALK
            ld      (TALKIN),hl         ; Empty queue: producer == consumer == TOPTALK
            ld      (TALKOUT),hl
            ld      bc,$0C15
            in      a,(c)
            ld      hl,DEMOMODE
            ld      e,$01
            call    wpb_bang            ; Write byte to protected memory
            call    $16A9
WPNOZ:      ld      hl,$D00B
            ld      c,$10
            in      e,(c)
            call    wpb_bang            ; Write byte to protected memory
            ld      hl,CRASHCTR
            ld      a,(hl)
            cp      $03
            jp      c,$172D
            ld      e,$00
            call    wpb_bang            ; Write byte to protected memory
            ld      e,$01
            ld      hl,DEMOMODE
            call    wpb_bang            ; Write byte to protected memory
            jp      L1593               ; Fixed delay, then cold restart
            exx
            DW      _DSPATCH

;******************************************************************************************
            nop
            dec     b
            nop
            inc     bc
            add     a,b
            nop
            nop
            ld      bc,$B7D9
            rla
            ld      e,a
            ld      d,$00
            ld      hl,$1730
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ld      hl,($D002)
            add     hl,de
            ex      de,hl
            ld      a,d
            cp      $10
            jr      nc,$1755
            ld      hl,$D002
            call    $0A8D
            exx
            ret
;******************************************************************************************
            exx
            ld      hl,$D009
            ld      e,$01
            call    wpb_bang            ; Write byte to protected memory
            exx
            ret
;******************************************************************************************
            in      a,($13)
            and     $08
            jp      z,$CC00
            in      a,($10)
            bit     3,a
            jp      nz,$1781
            ld      e,$01
            ld      hl,DEMOMODE
            call    wpb_bang            ; Write byte to protected memory
            ld      hl,$D00A
            call    wpb_bang            ; Write byte to protected memory
            jp      L1593
;******************************************************************************************
            ld      b,a
            in      a,($10)
            cp      b
            jp      nz,$1781
            ld      hl,$D00B
            ld      bc,$FFFF
            xor     (hl)
            bit     0,a
            jp      z,$17BE
            ex      af,af'
            ld      a,(hl)
            bit     0,a
            jp      z,$17A5
            res     0,a
            ld      e,a
            call    wpb_bang            ; Write byte to protected memory
            ex      af,af'
            jp      $17BE
;******************************************************************************************
            set     0,a
            ld      e,a
            call    wpb_bang            ; Write byte to protected memory
            call    $1757
            exx
            ld      a,($D008)
            ld      b,a
            ld      c,$13
            in      a,(c)
            and     $01
            or      $02
            exx
            ld      c,a
            ex      af,af'
            bit     1,a
            jp      z,$17F6
            ex      af,af'
            ld      a,(hl)
            bit     1,a
            jp      z,$17D4
            res     1,a
            ld      e,a
            call    wpb_bang            ; Write byte to protected memory
            ex      af,af'
            jp      $17F6
;******************************************************************************************
            set     1,a
            ld      e,a
            call    wpb_bang            ; Write byte to protected memory
            call    $1757
            exx
            in      a,($13)
            ld      b,a
            and     $06
            rra
            ld      c,a
            ld      a,b
            and     $01
            or      $02
            cp      c
            jp      z,$17F4
            ld      a,c
            exx
            ld      b,a
            jp      $17F6
;******************************************************************************************
            exx
            ld      c,a
            ld      a,c
            cp      $FF
            jp      z,$1807
            call    $1738
            ld      hl,$D005
            ld      e,(hl)
            inc     e
            call    wpb_bang            ; Write byte to protected memory
            ld      a,b
            cp      $FF
            jp      z,$1818
            call    $1738
            ld      hl,$D007
            ld      e,(hl)
            inc     e
            call    wpb_bang            ; Write byte to protected memory
            ld      c,$15
            xor     a
            ld      b,a
            ld      hl,$D004
            cp      (hl)
            jp      z,$1836
            ld      e,(hl)
            dec     e
            call    wpb_bang            ; Write byte to protected memory
            ld      hl,$D004
            ld      a,$0A
            cp      (hl)
            jp      nc,$1833
            set     0,b
            jp      $1848
            inc     hl
            cp      (hl)
            jp      z,$1848
            ld      e,(hl)
            dec     e
            call    wpb_bang            ; Write byte to protected memory
            ld      hl,$D004
            ld      e,$14
            call    wpb_bang            ; Write byte to protected memory
            xor     a
            ld      hl,$D006
            cp      (hl)
            jp      z,$1863
            ld      e,(hl)
            dec     e
            call    wpb_bang            ; Write byte to protected memory
            ld      hl,$D006
            ld      a,$0A
            cp      (hl)
            jp      nc,$1860
            set     1,b
            jp      $1875
;******************************************************************************************
            inc     hl
            cp      (hl)
            jp      z,$1875
            ld      e,(hl)
            dec     e
            call    wpb_bang            ; Write byte to protected memory
            ld      hl,$D006
            ld      e,$14
            call    wpb_bang            ; Write byte to protected memory
            ld      e,b
            ld      a,b
            and     $01
            ld      b,a
            in      a,(c)
            ld      a,e
            rrca
            and     $01
            or      $02
            ld      b,a
            in      a,(c)
            ld      hl,$D008
            call    wpb_bang            ; Write byte to protected memory
            ret
;******************************************************************************************
            daa
            push    de
            ld      e,a
            call    wpb_bang            ; Write byte to protected memory
            ld      a,e
            pop     de
            ret
            exx
            pop     hl
            pop     de
            ld      a,(hl)
            add     a,e
            call    $188C
            inc     hl
            ld      a,(hl)
            adc     a,d
            call    $188C
            inc     hl
            ld      a,(hl)
            adc     a,$00
            call    $188C
            exx
            DW      _DSPATCH

;******************************************************************************************
; ZEROSCORE - ( ZERO QUAD BYTES OF MEMORY )
; Description & Context: High-level TERSE word. Clears 3 bytes of memory at the
;                        address provided on the stack. Specifically used to zero
;                        out the 3-byte BCD scores for the players. Previously
;                        labeled as _P0Q.
;******************************************************************************************
_ZEROSCORE:  exx
            pop     hl
            ld      b, $03
            ld      e, $00
zeroscore0: call    wpb_bang            ; Write byte to protected memory
            inc     hl
            djnz    zeroscore0
            exx
            DW      _DSPATCH

;******************************************************************************************
            or      e
            rst     $38
            rst     $38
            rst     $38
            add     a,$18
            and     c
            add     hl,de
            adc     a,b
            add     hl,de
            push    af
            in      a,($0E)
            ld      ($D09B),a
            ld      a,$08
            ld      ($D09D),a
            out     ($0E),a
            pop     af
            ei
            ret
            xor     a
            ld      ($D09D),a
            ld      a,$18
            out     ($0E),a
            ld      a,($D09D)
            and     a
            jp      z,$18DE
            ld      a,($D09B)
            ret
            di
            ld      a,($D091)
            add     a,c
            ld      c,a
            sub     $70
            cp      $34
            jr      c,$18FC
            ld      a,c
            out     ($0F),a
            ld      a,$C4
            out     ($0D),a
            pop     iy
            pop     ix
            pop     hl
            pop     de
            pop     bc
            pop     af
            exx
            ex      af,af'
            pop     hl
            pop     de
            pop     bc
            ld      a,$01
            ld      ($D095),a
            ld      a,($D093)
            ld      ($D099),a
            pop     af
            ei
            ret
            xor     a
            ld      ($D095),a
            ei
            call    $18D6
            ld      c,a
            ld      a,($D099)
            and     a
            jp      z,$18E9
            ld      iy,$D08C
            ld      l,(iy+$00)
            ld      h,(iy+$01)
            ld      a,h
            or      l
            jp      z,$18E9
            push    hl
            pop     ix
            ld      a,(ix+$07)
            and     a
            jp      z,$18E9
            bit     3,(ix+$00)
            jp      nz,$1978
            ld      a,(COCKTAIL)
            and     a
            jp      z,$1969
            ld      l,(ix+$1d)
            ld      h,(ix+$1e)
            ld      a,l
            or      h
            ld      a,$CB
            jp      z,$1963
            sub     (ix+$14)
            inc     hl
            sub     (hl)
            jp      $1966
            sub     (ix+$14)
            jp      $196C
            ld      a,(ix+$14)
            sub     c
            jp      p,$1972
            cpl
            inc     a
            cp      (ix+$08)
            jp      c,$18E9
            di
            call    $161F
            ei
            ld      hl,$191C
            push    hl
            ld      l,(ix+$05)
            ld      h,(ix+$06)
            jp      (hl)
            push    af
            push    bc
            push    de
            push    hl
            exx
            ex      af,af'
            push    af
            push    bc
            push    de
            push    hl
            push    ix
            push    iy
            ld      a,$80
            out     ($0F),a
            ld      a,$C2
            out     ($0D),a
            jp      $1917
;
            push    af
            push    bc
            push    de
            push    hl
            exx
            ex      af,af'
            push    af
            push    bc
            push    de
            push    hl
            push    ix
            push    iy
            call    $0F64
            call    phone               ; Interrupt-time SC-01 queue service; at most one phoneme per call
            ld      hl,$D090
            ld      a,(hl)
            and     a
            jp      z,$19C1
            dec     (hl)
            call    $1762
            call    $16A9
            ld      a,($D097)
            and     a
            jp      nz,$1A35
            ld      hl,($D09F)
            ld      a,h
            or      l
            jp      z,$19D7
            dec     hl
            ld      ($D09F),hl
            ld      hl,($D0A1)
            ld      a,h
            or      l
            jp      z,$19E3
            dec     hl
            ld      ($D0A1),hl
            ld      hl,($D0A3)
            ld      a,h
            or      l
            jp      z,$19EF
            dec     hl
            ld      ($D0A3),hl
            ld      hl,($D0A5)
            ld      a,h
            or      l
            jp      z,$19FB
            dec     hl
            ld      ($D0A5),hl
            ld      hl,$D0A7
            ld      a,(hl)
            and     a
            jp      z,$1A04
            dec     (hl)
            ld      hl,($D0A9)
            ld      a,h
            or      l
            jp      z,$1A10
            dec     hl
            jp      $1A17
;
            ld      hl,$D08A
            inc     (hl)
            ld      hl,$0400
            ld      ($D0A9),hl
            ld      a,($D099)
            and     a
            jp      z,$1A25
            dec     a
            ld      ($D099),a
            ld      c,$01
            ld      iy,$D08C
            call    $163B
            ld      a,($D095)
            and     a
            jp      nz,$1917
            pop     iy
            pop     ix
            pop     hl
            pop     de
            pop     bc
            pop     af
            exx
            ex      af,af'
            pop     hl
            pop     de
            pop     bc
            pop     af
            ei
            ret
            di
            ld      a,$18
            ld      i,a
            ld      a,$C2
            out     ($0D),a
            ld      a,$01
            ld      ($D095),a
            ld      a,$08
            out     ($0E),a
            ld      a,$80
            out     ($0F),a
            im      2
            ei
            DW      _DSPATCH
;******************************************************************************************
            ld      a,$B4
            ld      ($D090),a
            ret
_BARK:      call    $1A60            ; Source-proven dictionary entry used by SHOW4P
            DW      _DSPATCH
;******************************************************************************************
            bit     7,(ix+$00)
            jp      nz,$1A8C
            di
            ld      iy,$D08C
            call    $15A4
            bit     4,(ix+$00)
            ld      (ix+$00),$00
            jp      nz,$1A8B
            push    ix
            pop     hl
            call    $1545
            ei
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$1d),e
            ld      (ix+$1e),d
            ret
            ld      a,(hl)
            inc     hl
            ld      (ix+$1b),a
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$05),e
            ld      (ix+$06),d
            ret
            ld      a,(hl)
            inc     hl
            bit     0,(ix+$00)
            jp      z,$1AB4
            ld      a,$01
            ld      (ix+$0a),a
            ld      (ix+$21),l
            ld      (ix+$22),h
            pop     hl
            ret
            ld      c,(hl)
            inc     hl
            ld      b,(hl)
            inc     hl
            ex      de,hl
            ld      l,(ix+$23)
            ld      h,(ix+$24)
            ld      (hl),e
            inc     hl
            ld      (hl),d
            inc     hl
            ld      (ix+$23),l
            ld      (ix+$24),h
            ld      l,c
            ld      h,b
            ret
            ld      l,(ix+$23)
            ld      h,(ix+$24)
            dec     hl
            ld      d,(hl)
            dec     hl
            ld      e,(hl)
            ld      (ix+$23),l
            ld      (ix+$24),h
            ex      de,hl
            ret
            res     7,(ix+$00)
            pop     hl
            ret
            ld      a,(hl)
            inc     hl
            ld      (ix+$1c),a
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$27),e
            ld      (ix+$28),d
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$0d),e
            ld      (ix+$0e),d
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$13),e
            ld      (ix+$14),d
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ex      de,hl
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$0f),e
            ld      (ix+$10),d
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$15),e
            ld      (ix+$16),d
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$11),e
            ld      (ix+$12),d
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$17),e
            ld      (ix+$18),d
            ret
            ld      a,(hl)
            inc     hl
            ld      e,(ix+$23)
            ld      d,(ix+$24)
            ld      (de),a
            inc     de
            ld      (ix+$23),e
            ld      (ix+$24),d
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      c,(ix+$23)
            ld      b,(ix+$24)
            dec     bc
            ld      a,(bc)
            dec     a
            jp      z,$1B6B
            ld      (bc),a
            ex      de,hl
            jp      $1B71
            ld      (ix+$23),c
            ld      (ix+$24),b
            ret
            ld      c,(hl)
            inc     hl
            ld      b,(hl)
            inc     hl
            ld      a,(ix+$00)
            xor     c
            and     b
            xor     c
            ld      (ix+$00),a
            ret
            ld      a,(hl)
            inc     hl
            ex      de,hl
            rrca
            rrca
            ld      b,a
            and     $C0
            ld      c,a
            ld      a,b
            bit     5,a
            jp      z,$1B94
            or      $C0
            jp      $1B96
            and     $3F
            ld      b,a
            ld      l,(ix+$0d)
            ld      h,(ix+$0e)
            add     hl,bc
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      a,(de)
            inc     de
            ld      b,a
            ld      c,$00
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            add     hl,bc
            ld      (ix+$13),l
            ld      (ix+$14),h
            ex      de,hl
            ret
            ld      c,(hl)
            inc     hl
            ld      b,$00
            ex      de,hl
            ld      l,(ix+$25)
            ld      h,(ix+$26)
            add     hl,bc
            ld      c,(hl)
            inc     hl
            ld      b,(hl)
            ld      (ix+$1d),c
            ld      (ix+$1e),b
            ex      de,hl
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            push    de
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$25),e
            ld      (ix+$26),d
            ret
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      (ix+$2a),e
            ld      (ix+$2b),d
            ret
            ld      a,(hl)
            inc     hl
            ld      (ix+$08),a
            ret
            ld      c,(hl)
            inc     hl
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      a,r
            and     c
            ret     nz
            ex      de,hl
            ret
            adc     a,l
            ld      a,(de)
            sbc     a,b
            ld      a,(de)
            sbc     a,(hl)
            ld      a,(de)
            xor     c
            ld      a,(de)
            cp      a
            ld      a,(de)
            ld      d,$1B
            dec     de
            dec     de
            jr      nc,$1C28
            rst     $10
            ld      a,(de)
            jp      (hl)
            ld      a,(de)
            push    af
            ld      a,(de)
            nop
            dec     de
            dec     bc
            dec     de
            add     a,b
            dec     de
            ld      b,l
            dec     de
            ld      d,(hl)
            dec     de
            ld      (hl),d
            dec     de
            cp      b
            dec     de
            rst     $08
            dec     de
            push    de
            dec     de
            ret     po
            dec     de
            ex      de,hl
            dec     de
            pop     af
            dec     de
            rst     $28
            ld      a,(de)
            ld      a,(ix+$0a)
            and     a
            ret     nz
            ld      l,(ix+$21)
            ld      h,(ix+$22)
            ld      de,$1C38
            push    de
            ld      c,(hl)
            inc     hl
            ld      b,$00
            ex      de,hl
            ld      hl,$1BFD
            add     hl,bc
            ld      c,(hl)
            inc     hl
            ld      b,(hl)
            ex      de,hl
            push    bc
            ret
            call    $1C2D
            bit     7,(ix+$00)
            jr      nz,$1C66
            bit     2,(ix+$00)
            jr      z,$1C66
            ld      l,(ix+$2a)
            ld      h,(ix+$2b)
            ld      (ix+$1d),l
            ld      (ix+$1e),h
            ret
            di
            ld      c,(ix+$07)
            ld      b,(ix+$0a)
            ld      a,b
            sub     c
            jp      m,$1C7D
            ld      (ix+$0a),a
            ld      (ix+$07),$00
            jp      $1C87
            ld      a,c
            sub     b
            ld      (ix+$07),a
            ld      (ix+$0a),$00
            ld      c,b
            bit     1,(ix+$00)
            jr      nz,$1CB3
            ld      b,$00
            ld      l,(ix+$0b)
            ld      h,(ix+$0c)
            and     a
            sbc     hl,bc
            jr      z,$1C9D
            jp      p,$1CAD
            ld      a,l
            add     a,c
            ld      c,a
            xor     a
            ld      h,a
            ld      l,a
            ld      (ix+$0a),a
            ld      (ix+$07),a
            res     7,(ix+$00)
            ld      (ix+$0b),l
            ld      (ix+$0c),h
            ei
            ret
            ld      a,c
            and     a
            ret     z
            ld      l,(ix+$0d)
            ld      h,(ix+$0e)
            ld      e,(ix+$0f)
            ld      d,(ix+$10)
            ld      b,c
            add     hl,de
            djnz    $1CC5
            ld      a,h
            cp      $50
            jr      nc,$1CF3
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      e,(ix+$15)
            ld      d,(ix+$16)
            ld      b,c
            add     hl,de
            djnz    $1CE0
            ld      a,h
            cp      $BA
            jr      nc,$1CF3
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      (ix+$08),$28
            ret
            res     7,(ix+$00)
            set     6,(ix+$00)
            ret
            rst     $08
            ld      b,l
            ld      a,(de)
            ld      h,c
            nop
            rst     $08
            call    $7604
            inc     d
            jp      c,$B514
            inc     b
            ld      l,l
            nop
            sub     a
            ret     nc
            inc     bc
            ld      bc,_LIT
            sbc     a,c
            ret     nc
            inc     bc
            ld      bc,_LIT
            sbc     a,l
            ret     nc
            inc     bc
            ld      bc,$0076
            ld      hl,_LIT
            sub     c
            ret     nc
            rst     $30
            nop
            halt
            nop
            ld      (bc),a
            ld      l,l
            nop
            sub     e
            ret     nc
            rst     $30
            nop
            call    m,$611C
            nop
            in      a,($08)
            call    $1492
            in      a,($08)
            and     a
            jr      z,$1D47
            ld      l,(ix+$27)
            ld      h,(ix+$28)
            ld      a,h
            or      l
            jr      z,$1D47
            jp      (hl)
            ret
            call    $1C67
            call    $1CB5
            bit     5,(ix+$00)
            jp      nz,$1D5B
            call    $14BF
            jp      $1D5F
            res     5,(ix+$00)
            call    $1C4B
            bit     6,(ix+$00)
            jp      nz,$1D6F
            call    $1492
            jp      $1D77
            res     6,(ix+$00)
            set     5,(ix+$00)
            jp      $1A6B
            call    $1C67
            call    $1CB5
            bit     5,(ix+$00)
            jp      nz,$1D8D
            call    $14BF
            jp      $1D91
            res     5,(ix+$00)
            call    $1C4B
            bit     6,(ix+$00)
            jp      nz,$1DA1
            call    $1D32
            jp      $1DA9
            res     6,(ix+$00)
            set     5,(ix+$00)
            jp      $1A6B
            bit     0,(ix+$00)
            jp      z,$1DB8
            set     6,(ix+$00)
            ret
            ld      a,c
            and     a
            ret     z
            push    af
            ld      l,(ix+$0d)
            ld      h,(ix+$0e)
            ld      e,(ix+$0f)
            ld      d,(ix+$10)
            ld      c,(ix+$11)
            ld      b,(ix+$12)
            ex      de,hl
            add     hl,bc
            ex      de,hl
            add     hl,de
            dec     a
            jr      nz,$1DCE
            ld      (ix+$0f),e
            ld      (ix+$10),d
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      a,h
            cp      $50
            jp      c,$1DEF
            set     0,(ix+$00)
            set     6,(ix+$00)
            pop     af
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      e,(ix+$15)
            ld      d,(ix+$16)
            ld      c,(ix+$17)
            ld      b,(ix+$18)
            ex      de,hl
            add     hl,bc
            ex      de,hl
            add     hl,de
            dec     a
            jr      nz,$1E02
            ld      (ix+$15),e
            ld      (ix+$16),d
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      a,h
            cp      $B6
            ret     c
            set     0,(ix+$00)
            set     6,(ix+$00)
            ret
            call    $1C67
            call    $1DAC
            bit     5,(ix+$00)
            jp      nz,$1E35
            call    $14BF
            jp      $1E39
            res     5,(ix+$00)
            call    $1C4B
            bit     6,(ix+$00)
            jp      nz,$1E49
            call    $1492
            jp      $1E51
            set     5,(ix+$00)
            res     6,(ix+$00)
            jp      $1A6B
            ld      a,c
            and     a
            ret     z
            call    $061F
            and     $0C
            jp      z,$1E9D
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      e,(ix+$15)
            ld      d,(ix+$16)
            ld      b,c
            call    $061F
            and     $08
            jp      nz,$1E88
            and     a
            sbc     hl,de
            ld      a,h
            cp      (ix+$17)
            jp      nc,$1E83
            ld      h,(ix+$17)
            ld      l,$00
            djnz    $1E74
            jp      $1E97
            add     hl,de
            ld      a,h
            cp      (ix+$18)
            jp      c,$1E95
            ld      h,(ix+$18)
            ld      l,$00
            djnz    $1E88
            ld      (ix+$13),l
            ld      (ix+$14),h
            call    $061F
            and     $03
            ret     z
            ld      l,(ix+$0d)
            ld      h,(ix+$0e)
            ld      e,(ix+$0f)
            ld      d,(ix+$10)
            ld      b,c
            call    $061F
            and     $02
            jp      z,$1ECC
            and     a
            sbc     hl,de
            ld      a,h
            cp      (ix+$11)
            jp      nc,$1EC7
            ld      h,(ix+$11)
            ld      l,$00
            djnz    $1EB8
            jp      $1EDB
            add     hl,de
            ld      a,h
            cp      (ix+$12)
            jp      c,$1ED9
            ld      h,(ix+$12)
            ld      l,$00
            djnz    $1ECC
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ret
            call    $1C67
            call    $1E54
            ld      (ix+$08),$28
            bit     5,(ix+$00)
            jp      nz,$1EF9
            call    $14BF
            jp      $1EFD
            res     5,(ix+$00)
            call    $1C4B
            bit     6,(ix+$00)
            jp      nz,$1F0D
            call    $1D32
            jp      $1F15
            set     5,(ix+$00)
            res     6,(ix+$00)
            jp      $1A6B
            ld      a,h
            cpl
            ld      h,a
            ld      a,l
            cpl
            ld      l,a
            inc     hl
            ret
            push    bc
            and     a
            sbc     hl,bc
            jp      c,$1F2D
            call    unsdiv2
            jp      $1F3B
            call    $1F18
            call    unsdiv2
            call    $1F18
            ex      de,hl
            call    $1F18
            ex      de,hl
            pop     bc
            add     hl,bc
            ret
            push    ix
            pop     hl
            ld      b,$40
            xor     a
            ld      (hl),a
            inc     hl
            djnz    $1F44
            ret
            di
            ld      (ix+$21),l
            ld      (ix+$22),h
            push    ix
            pop     hl
            ld      de,$0032
            add     hl,de
            ld      (ix+$23),l
            ld      (ix+$24),h
            ld      (ix+$0a),$00
            ei
            ret
            di
            push    iy
            push    ix
            pop     hl
            ld      iy,$D08C
            call    $1551
            pop     iy
            ei
            ret
            ld      l,(iy+$08)
            ld      h,(iy+$09)
            call    $1F49
            ld      l,(iy+$06)
            ld      h,(iy+$07)
            ld      (ix+$0b),l
            ld      (ix+$0c),h
            ld      (ix+$29),l
            ld      a,(iy+$05)
            ld      (ix+$2d),a
            ld      a,(iy+$04)
            ld      (ix+$00),a
            ld      (ix+$1b),$20
            ld      (ix+$08),$30
            ld      hl,$1D48
            ld      (ix+$05),l
            ld      (ix+$06),h
            ret
            push    ix
            pop     hl
            push    iy
            pop     de
            exx
            push    iy
            ld      iy,$0000
            add     iy,sp
            ld      l,(iy+$02)
            ld      h,(iy+$03)
            push    hl
            pop     ix
            call    $1F3E
            ld      c,(iy+$10)
            ld      b,(iy+$11)
            ld      l,(iy+$0c)
            ld      h,(iy+$0d)
            ld      e,(iy+$06)
            ld      d,(iy+$07)
            push    de
            call    $1F20
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      (ix+$0f),e
            ld      (ix+$10),d
            pop     de
            ld      c,(iy+$0e)
            ld      b,(iy+$0f)
            ld      l,(iy+$0a)
            ld      h,(iy+$0b)
            call    $1F20
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      (ix+$15),e
            ld      (ix+$16),d
            call    $1F74
            ld      a,(iy+$12)
            ld      (ix+$29),a
            call    $1F63
            pop     iy
            ld      hl,$0012
            add     hl,sp
            ld      sp,hl
            exx
            push    de
            pop     iy
            push    hl
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            dec     a
            dec     d
            xor     d
            rra
            ld      h,c
            nop
            push    ix
            pop     hl
            push    iy
            pop     de
            exx
            push    iy
            ld      iy,$0000
            add     iy,sp
            ld      l,(iy+$02)
            ld      h,(iy+$03)
            push    hl
            pop     ix
            call    $1F3E
            call    $1F74
            call    $1F63
            pop     iy
            ld      hl,$0008
            add     hl,sp
            ld      sp,hl
            exx
            push    de
            pop     iy
            push    hl
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            dec     a
            dec     d
            ld      h,$20
            ld      h,c
            nop
            di
            call    $1526
            ei
            push    hl
            push    iy
            ld      iy,$0000
            add     iy,sp
            ld      l,(iy+$02)
            ld      h,(iy+$03)
            push    hl
            pop     ix
            call    $1F3E
            ld      c,(iy+$06)
            ld      (ix+$29),c
            ld      l,(iy+$0c)
            ld      h,(iy+$0d)
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      l,(iy+$0a)
            ld      h,(iy+$0b)
            ld      (ix+$13),l
            ld      (ix+$14),h
            call    $1F74
            call    $1F63
            pop     iy
            ld      hl,$000C
            add     hl,sp
            ld      sp,hl
            ret
            push    hl
            push    de
            push    bc
            exx
            push    hl
            push    de
            push    ix
            pop     de
            push    iy
            pop     hl
            exx
            jp      $205E
            pop     de
            pop     hl
            exx
            pop     bc
            pop     de
            pop     hl
            call    $20A4
            exx
            push    de
            pop     ix
            push    hl
            pop     iy
            DW      _DSPATCH
;******************************************************************************************
            bit     7,(iy+$00)
            jp      z,$2112
            bit     5,(iy+$00)
            jp      nz,$2112
            ld      l,(ix+$1d)
            ld      h,(ix+$1e)
            ld      c,(hl)
            inc     hl
            ld      e,(hl)
            ld      l,(iy+$1d)
            ld      h,(iy+$1e)
            ld      b,(hl)
            inc     hl
            ld      d,(hl)
            ld      a,(ix+$14)
            sub     (iy+$14)
            jp      m,$20F6
            cp      d
            jp      nc,$2112
            jp      $20FA
            add     a,e
            jp      m,$2112
            ld      a,(ix+$0e)
            sub     (iy+$0e)
            jp      m,$210A
            cp      b
            jp      nc,$2112
            jp      $210E
            add     a,c
            jp      m,$2112
            ld      a,$01
            and     a
            ret
            xor     a
            ret
            ld      hl,($D08C)
            push    ix
            pop     de
            ld      a,h
            cp      d
            jr      nz,$2121
            ld      a,l
            cp      e
            ret     z
            push    hl
            pop     iy
            ld      a,(iy+$2d)
            and     c
            jr      z,$2130
            push    bc
            call    $20C6
            pop     bc
            ret     nz
            ld      l,(iy+$01)
            ld      h,(iy+$02)
            jr      $2117
            ld      a,h
            nop
            add     a,d
            nop
            add     a,d
            nop
            add     a,d
            nop
            ld      a,h
            nop
            nop
            nop
            add     a,h
            nop
            cp      $00
            add     a,b
            nop
            nop
            nop
            call    po,$9200
            nop
            sub     d
            nop
            sub     d
            nop
            adc     a,h
            nop
            ld      b,h
            nop
            add     a,d
            nop
            sub     d
            nop
            sub     d
            nop
            ld      l,h
            nop
            jr      nc,$2162
            jr      z,$2164
            inc     h
            nop
            cp      $00
            jr      nz,$216A
            ld      c,(hl)
            nop
            adc     a,d
            nop
            adc     a,d
            nop
            adc     a,d
            nop
            ld      (hl),d
            nop
            ld      a,b
            nop
            sub     h
            nop
            sub     d
            nop
            sub     d
            nop
            ld      h,b
            nop
            ld      (bc),a
            nop
            jp      po,$1200
            nop
            ld      a,(bc)
            nop
            ld      b,$00
            ld      l,h
            nop
            sub     d
            nop
            sub     d
            nop
            sub     d
            nop
            ld      l,h
            nop
            inc     c
            nop
            sub     d
            nop
            sub     d
            nop
            ld      d,d
            nop
            inc     a
            nop
            ld      e,(ix+$0d)
            ld      d,(ix+$0e)
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      bc,$0428
            push    hl
            ld      l,(ix+$1d)
            ld      h,(ix+$1e)
            inc     hl
            ld      a,(hl)
            ex      (sp),hl
            and     $0F
            call    nz,$21C8
            ex      (sp),hl
            dec     hl
            ld      a,(hl)
            rrca
            rrca
            rrca
            rrca
            ex      (sp),hl
            call    $21C8
            ex      (sp),hl
            ld      a,(hl)
            pop     hl
            push    bc
            push    hl
            push    de
            and     $0F
            rlca
            ld      e,a
            rlca
            rlca
            add     a,e
            ld      e,a
            ld      d,$00
            ld      hl,$2138
            add     hl,de
            push    hl
            pop     iy
            pop     de
            pop     hl
            push    hl
            push    de
            call    RELABS
            ld      de,$0502
            call    write               ; Call pattern transfer routine
            pop     de
            pop     hl
            ld      a,h
            add     a,$06
            ld      h,a
            pop     bc
            ret
;******************************************************************************************

            call    $1C67
            call    $1C4B
            bit     6,(ix+$00)
            jp      nz,$2201
            call    $219C
            jp      $1A6B
;*******************************************************************************
; GORF_PLAYER1
; 2 bytes/row = 8 pixels wide, 8 rows
;*******************************************************************************
GORF_PLAYER1:
        DB      $02,$08                        ; width bytes, height rows
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .
        DB      $9F,$70                        ; 2 1 3 3 1 3 . .
        DB      $80,$30                        ; 2 . . . . 3 . .
        DB      $80,$10                        ; 2 . . . . 1 . .
        DB      $9F,$F0                        ; 2 1 3 3 3 3 . .
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .

;*******************************************************************************
; GORF_PLAYER2
; 2 bytes/row = 8 pixels wide, 8 rows
;*******************************************************************************
GORF_PLAYER2:
        DB      $02,$08                        ; width bytes, height rows
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .
        DB      $83,$30                        ; 2 . . 3 . 3 . .
        DB      $81,$10                        ; 2 . . 1 . 1 . .
        DB      $99,$90                        ; 2 1 2 1 2 1 . .
        DB      $99,$90                        ; 2 1 2 1 2 1 . .
        DB      $99,$10                        ; 2 1 2 1 . 1 . .
        DB      $9C,$30                        ; 2 1 3 . . 3 . .
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .

;*******************************************************************************
; GORF_SHIP
; 4 bytes/row = 16 pixels wide, 13 rows
;*******************************************************************************
GORF_SHIP:
        DB      $04,$0D                        ; width bytes, height rows
        DB      $2A,$AA,$80,$00                ; . 2 2 2 2 2 2 2 2 . . . . . . .
        DB      $95,$55,$A8,$00                ; 2 1 1 1 1 1 1 1 2 2 2 . . . . .
        DB      $2A,$AA,$80,$00                ; . 2 2 2 2 2 2 2 2 . . . . . . .
        DB      $00,$54,$0A,$A0                ; . . . . 1 1 1 . . . 2 2 2 2 . .
        DB      $00,$15,$02,$A8                ; . . . . . 1 1 1 . . . 2 2 2 2 .
        DB      $55,$55,$52,$AA                ; 1 1 1 1 1 1 1 1 1 1 . 2 2 2 2 2
        DB      $02,$AA,$AA,$AA                ; . . . 2 2 2 2 2 2 2 2 2 2 2 2 2
        DB      $55,$55,$52,$AA                ; 1 1 1 1 1 1 1 1 1 1 . 2 2 2 2 2
        DB      $00,$15,$02,$A8                ; . . . . . 1 1 1 . . . 2 2 2 2 .
        DB      $00,$54,$0A,$A0                ; . . . . 1 1 1 . . . 2 2 2 2 . .
        DB      $2A,$AA,$80,$00                ; . 2 2 2 2 2 2 2 2 . . . . . . .
        DB      $95,$55,$A8,$00                ; 2 1 1 1 1 1 1 1 2 2 2 . . . . .
        DB      $2A,$AA,$80,$00                ; . 2 2 2 2 2 2 2 2 . . . . . . .

;*******************************************************************************
; GORF_SHIP_SMALL
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GORF_SHIP_SMALL:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $2A,$A8,$00                    ; . 2 2 2 2 2 2 . . . . .
        DB      $AA,$A0,$00                    ; 2 2 2 2 2 2 . . . . . .
        DB      $05,$00,$00                    ; . . 1 1 . . . . . . . .
        DB      $15,$40,$A0                    ; . 1 1 1 1 . . . 2 2 . .
        DB      $55,$54,$28                    ; 1 1 1 1 1 1 1 . . 2 2 .
        DB      $02,$AA,$AA                    ; . . . 2 2 2 2 2 2 2 2 2
        DB      $55,$54,$28                    ; 1 1 1 1 1 1 1 . . 2 2 .
        DB      $15,$40,$A0                    ; . 1 1 1 1 . . . 2 2 . .
        DB      $05,$00,$00                    ; . . 1 1 . . . . . . . .
        DB      $AA,$A0,$00                    ; 2 2 2 2 2 2 . . . . . .
        DB      $2A,$A8,$00                    ; . 2 2 2 2 2 2 . . . . .

;*******************************************************************************
; GORF_BOUNCE_DOWN
; 5 bytes/row = 20 pixels wide, 15 rows
;*******************************************************************************
GORF_BOUNCE_DOWN:
        DB      $05,$0F                        ; width bytes, height rows
        DB      $80,$FC,$00,$00,$04            ; 2 . . . 3 3 3 . . . . . . . . . . . 1 .
        DB      $83,$FF,$F0,$00,$10            ; 2 . . 3 3 3 3 3 3 3 . . . . . . . 1 . .
        DB      $AB,$FF,$FF,$C0,$40            ; 2 2 2 3 3 3 3 3 3 3 3 3 3 . . . 1 . . .
        DB      $03,$FF,$FF,$FF,$00            ; . . . 3 3 3 3 3 3 3 3 3 3 3 3 3 . . . .
        DB      $0F,$FD,$55,$FF,$F0            ; . . 3 3 3 3 3 1 1 1 1 1 3 3 3 3 3 3 . .
        DB      $0D,$5E,$95,$FF,$FC            ; . . 3 1 1 1 3 2 2 1 1 1 3 3 3 3 3 3 3 .
        DB      $0E,$6F,$FF,$FF,$FF            ; . . 3 2 1 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3
        DB      $0D,$5F,$FF,$FF,$FF            ; . . 3 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3
        DB      $0E,$6F,$FF,$FF,$FF            ; . . 3 2 1 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3
        DB      $0D,$5D,$55,$FF,$FC            ; . . 3 1 1 1 3 1 1 1 1 1 3 3 3 3 3 3 3 .
        DB      $0F,$FE,$95,$FF,$F0            ; . . 3 3 3 3 3 2 2 1 1 1 3 3 3 3 3 3 . .
        DB      $03,$FF,$FF,$FF,$00            ; . . . 3 3 3 3 3 3 3 3 3 3 3 3 3 . . . .
        DB      $AB,$FF,$FF,$C0,$40            ; 2 2 2 3 3 3 3 3 3 3 3 3 3 . . . 1 . . .
        DB      $83,$FF,$F0,$00,$10            ; 2 . . 3 3 3 3 3 3 3 . . . . . . . 1 . .
        DB      $80,$FC,$00,$00,$04            ; 2 . . . 3 3 3 . . . . . . . . . . . 1 .

; Legacy labels referenced elsewhere in the disassembly.
GORF                     EQU     GORF_BOUNCE_DOWN

;*******************************************************************************
; GORF_BOUNCE_UP
; 5 bytes/row = 20 pixels wide, 15 rows
;*******************************************************************************
GORF_BOUNCE_UP:
        DB      $05,$0F                        ; width bytes, height rows
        DB      $00,$FC,$00,$00,$80            ; . . . . 3 3 3 . . . . . . . . . 2 . . .
        DB      $03,$FF,$F0,$00,$20            ; . . . 3 3 3 3 3 3 3 . . . . . . . 2 . .
        DB      $57,$FF,$FF,$C0,$80            ; 1 1 1 3 3 3 3 3 3 3 3 3 3 . . . 2 . . .
        DB      $43,$FF,$FF,$FF,$00            ; 1 . . 3 3 3 3 3 3 3 3 3 3 3 3 3 . . . .
        DB      $4F,$FD,$6A,$FF,$F0            ; 1 . 3 3 3 3 3 1 1 2 2 2 3 3 3 3 3 3 . .
        DB      $0F,$BE,$AA,$FF,$FC            ; . . 3 3 2 3 3 2 2 2 2 2 3 3 3 3 3 3 3 .
        DB      $0F,$BF,$FF,$FF,$FF            ; . . 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
        DB      $0F,$BF,$FF,$FF,$FF            ; . . 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
        DB      $0F,$BF,$FF,$FF,$FF            ; . . 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
        DB      $0F,$BD,$6A,$FF,$FC            ; . . 3 3 2 3 3 1 1 2 2 2 3 3 3 3 3 3 3 .
        DB      $4F,$FE,$AA,$FF,$F0            ; 1 . 3 3 3 3 3 2 2 2 2 2 3 3 3 3 3 3 . .
        DB      $43,$FF,$FF,$FF,$00            ; 1 . . 3 3 3 3 3 3 3 3 3 3 3 3 3 . . . .
        DB      $57,$FF,$FF,$C0,$80            ; 1 1 1 3 3 3 3 3 3 3 3 3 3 . . . 2 . . .
        DB      $03,$FF,$F0,$00,$20            ; . . . 3 3 3 3 3 3 3 . . . . . . . 2 . .
        DB      $00,$FC,$00,$00,$80            ; . . . . 3 3 3 . . . . . . . . . 2 . . .

;*******************************************************************************
; GORF_EXTREME_SMALL
; 2 bytes/row = 8 pixels wide, 5 rows
;*******************************************************************************
GORF_EXTREME_SMALL:
        DB      $02,$05                        ; width bytes, height rows
        DB      $8F,$04                        ; 2 . 3 3 . . 1 .
        DB      $BF,$70                        ; 2 3 3 3 1 3 . .
        DB      $37,$FC                        ; . 3 1 3 3 3 3 .
        DB      $BF,$70                        ; 2 3 3 3 1 3 . .
        DB      $8F,$04                        ; 2 . 3 3 . . 1 .

;*******************************************************************************
; GORF_VERY_VERY_SMALL
; 3 bytes/row = 12 pixels wide, 7 rows
;*******************************************************************************
GORF_VERY_VERY_SMALL:
        DB      $03,$07                        ; width bytes, height rows
        DB      $83,$F0,$40                    ; 2 . . 3 3 3 . . 1 . . .
        DB      $AF,$FF,$00                    ; 2 2 3 3 3 3 3 3 . . . .
        DB      $0E,$E7,$C0                    ; . . 3 2 3 2 1 3 3 . . .
        DB      $3E,$FF,$C0                    ; . 3 3 2 3 3 3 3 3 . . .
        DB      $0E,$E7,$C0                    ; . . 3 2 3 2 1 3 3 . . .
        DB      $AF,$FF,$00                    ; 2 2 3 3 3 3 3 3 . . . .
        DB      $83,$F0,$40                    ; 2 . . 3 3 3 . . 1 . . .

;*******************************************************************************
; GORF_MINI
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
GORF_MINI:
        DB      $02,$03                        ; width bytes, height rows
        DB      $8F,$00                        ; 2 . 3 3 . . . .
        DB      $37,$C0                        ; . 3 1 3 3 . . .
        DB      $8F,$00                        ; 2 . 3 3 . . . .

;*******************************************************************************
; GORF_VERY_SMALL
; 3 bytes/row = 12 pixels wide, 9 rows
;*******************************************************************************
GORF_VERY_SMALL:
        DB      $03,$09                        ; width bytes, height rows
        DB      $83,$F0,$01                    ; 2 . . 3 3 3 . . . . . 1
        DB      $8F,$FF,$04                    ; 2 . 3 3 3 3 3 3 . . 1 .
        DB      $AF,$FF,$F0                    ; 2 2 3 3 3 3 3 3 3 3 . .
        DB      $0E,$E5,$FC                    ; . . 3 2 3 2 1 1 3 3 3 .
        DB      $0D,$FF,$FC                    ; . . 3 1 3 3 3 3 3 3 3 .
        DB      $0E,$E5,$FC                    ; . . 3 2 3 2 1 1 3 3 3 .
        DB      $AF,$FF,$F0                    ; 2 2 3 3 3 3 3 3 3 3 . .
        DB      $8F,$FF,$04                    ; 2 . 3 3 3 3 3 3 . . 1 .
        DB      $83,$F0,$01                    ; 2 . . 3 3 3 . . . . . 1

;*******************************************************************************
; GORF_SMALL
; 4 bytes/row = 16 pixels wide, 12 rows
;*******************************************************************************
GORF_SMALL:
        DB      $04,$0C                        ; width bytes, height rows
        DB      $80,$FF,$00,$04                ; 2 . . . 3 3 3 3 . . . . . . 1 .
        DB      $83,$FF,$FC,$10                ; 2 . . 3 3 3 3 3 3 3 3 . . 1 . .
        DB      $AB,$FF,$FF,$C0                ; 2 2 2 3 3 3 3 3 3 3 3 3 3 . . .
        DB      $03,$FE,$97,$F0                ; . . . 3 3 3 3 2 2 1 1 3 3 3 . .
        DB      $0F,$6D,$57,$FC                ; . . 3 3 1 2 3 1 1 1 1 3 3 3 3 .
        DB      $0F,$5F,$FF,$FF                ; . . 3 3 1 1 3 3 3 3 3 3 3 3 3 3
        DB      $0F,$9F,$FF,$FF                ; . . 3 3 2 1 3 3 3 3 3 3 3 3 3 3
        DB      $0F,$6E,$97,$FC                ; . . 3 3 1 2 3 2 2 1 1 3 3 3 3 .
        DB      $03,$FD,$57,$F0                ; . . . 3 3 3 3 1 1 1 1 3 3 3 . .
        DB      $AB,$FF,$FF,$C0                ; 2 2 2 3 3 3 3 3 3 3 3 3 3 . . .
        DB      $83,$FF,$FC,$10                ; 2 . . 3 3 3 3 3 3 3 3 . . 1 . .
        DB      $80,$FF,$00,$04                ; 2 . . . 3 3 3 3 . . . . . . 1 .

;*******************************************************************************
; GORF_EXPLODE_SMALL
; 3 bytes/row = 12 pixels wide, 12 rows
;*******************************************************************************
GORF_EXPLODE_SMALL:
        DB      $03,$0C                        ; width bytes, height rows
        DB      $00,$C0,$04                    ; . . . . 3 . . . . . 1 .
        DB      $C0,$F3,$00                    ; 3 . . . 3 3 . 3 . . . .
        DB      $3F,$FF,$0C                    ; . 3 3 3 3 3 3 3 . . 3 .
        DB      $0F,$FF,$FC                    ; . . 3 3 3 3 3 3 3 3 3 .
        DB      $0F,$DF,$7F                    ; . . 3 3 3 1 3 3 1 3 3 3
        DB      $FF,$55,$7C                    ; 3 3 3 3 1 1 1 1 1 3 3 .
        DB      $FD,$55,$7C                    ; 3 3 3 1 1 1 1 1 1 3 3 .
        DB      $3F,$F5,$7C                    ; . 3 3 3 3 3 1 1 1 3 3 .
        DB      $0F,$3F,$F0                    ; . . 3 3 . 3 3 3 3 3 . .
        DB      $4C,$0F,$C1                    ; 1 . 3 . . . 3 3 3 . . 1
        DB      $00,$0F,$C0                    ; . . . . . . 3 3 3 . . .
        DB      $00,$03,$00                    ; . . . . . . . 3 . . . .

;*******************************************************************************
; GORF_UNK
; 4 bytes/row = 16 pixels wide, 17 rows
;*******************************************************************************
GORF_UNK:
        DB      $04,$11                        ; width bytes, height rows
        DB      $01,$00,$00,$00                ; . . . 1 . . . . . . . . . . . .
        DB      $00,$40,$00,$00                ; . . . . 1 . . . . . . . . . . .
        DB      $00,$54,$00,$00                ; . . . . 1 1 1 . . . . . . . . .
        DB      $00,$14,$04,$00                ; . . . . . 1 1 . . . 1 . . . . .
        DB      $00,$15,$54,$00                ; . . . . . 1 1 1 1 1 1 . . . . .
        DB      $00,$1D,$54,$14                ; . . . . . 1 3 1 1 1 1 . . 1 1 .
        DB      $00,$5F,$D5,$55                ; . . . . 1 1 3 3 3 1 1 1 1 1 1 1
        DB      $00,$5F,$FF,$F5                ; . . . . 1 1 3 3 3 3 3 3 3 3 1 1
        DB      $C1,$7F,$FF,$F4                ; 3 . . 1 1 3 3 3 3 3 3 3 3 3 1 .
        DB      $15,$7F,$FF,$D4                ; . 1 1 1 1 3 3 3 3 3 3 3 3 1 1 .
        DB      $15,$FF,$FF,$D4                ; . 1 1 1 3 3 3 3 3 3 3 3 3 1 1 .
        DB      $05,$FF,$FF,$F5                ; . . 1 1 3 3 3 3 3 3 3 3 3 3 1 1
        DB      $05,$F5,$FD,$F4                ; . . 1 1 3 3 1 1 3 3 3 1 3 3 1 .
        DB      $00,$55,$55,$54                ; . . . . 1 1 1 1 1 1 1 1 1 1 1 .
        DB      $05,$14,$14,$45                ; . . 1 1 . 1 1 . . 1 1 . 1 . 1 1
        DB      $04,$00,$00,$40                ; . . 1 . . . . . . . . . 1 . . .
        DB      $50,$00,$00,$40                ; 1 1 . . . . . . . . . . 1 . . .

;*******************************************************************************
; GORF_LINE
; 3 bytes/row = 12 pixels wide, 1 rows
;*******************************************************************************
GORF_LINE:
        DB      $03,$01                        ; width bytes, height rows
        DB      $55,$55,$55                    ; 1 1 1 1 1 1 1 1 1 1 1 1

;*******************************************************************************
; GORF_LINE2
; 1 bytes/row = 4 pixels wide, 1 rows
;*******************************************************************************
GORF_LINE2:
        DB      $01,$01                        ; width bytes, height rows
        DB      $00                            ; . . . .

;*******************************************************************************
; GORF_EXPLODE
; 6 bytes/row = 24 pixels wide, 23 rows
;*******************************************************************************
GORF_EXPLODE:
        DB      $06,$17                        ; width bytes, height rows
        DB      $00,$00,$80,$00,$00,$00        ; . . . . . . . . 2 . . . . . . . . . . . . . . .
        DB      $00,$02,$60,$00,$00,$00        ; . . . . . . . 2 1 2 . . . . . . . . . . . . . .
        DB      $00,$A9,$58,$00,$00,$00        ; . . . . 2 2 2 1 1 1 2 . . . . . . . . . . . . .
        DB      $02,$55,$56,$0A,$00,$00        ; . . . 2 1 1 1 1 1 1 1 2 . . 2 2 . . . . . . . .
        DB      $0B,$DF,$F5,$A6,$80,$00        ; . . 2 3 3 1 3 3 3 3 1 1 2 2 1 2 2 . . . . . . .
        DB      $25,$FF,$FD,$7D,$60,$00        ; . 2 1 1 3 3 3 3 3 3 3 1 1 3 3 1 1 2 . . . . . .
        DB      $09,$7F,$FF,$FD,$58,$00        ; . . 2 1 1 3 3 3 3 3 3 3 3 3 3 1 1 1 2 . . . . .
        DB      $02,$FF,$FF,$FF,$56,$00        ; . . . 2 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 2 . . . .
        DB      $00,$BF,$FF,$FF,$D5,$80        ; . . . . 2 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 2 . . .
        DB      $02,$5F,$FF,$FF,$FD,$60        ; . . . 2 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 2 . .
        DB      $09,$7F,$FF,$FF,$F5,$80        ; . . 2 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 2 . . .
        DB      $25,$FF,$FF,$FF,$D6,$00        ; . 2 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 2 . . . .
        DB      $A5,$FF,$FF,$FF,$58,$00        ; 2 2 1 1 3 3 3 3 3 3 3 3 3 3 3 3 1 1 2 . . . . .
        DB      $25,$5F,$FF,$F5,$58,$00        ; . 2 1 1 1 1 3 3 3 3 3 3 3 3 1 1 1 1 2 . . . . .
        DB      $09,$FF,$FF,$FD,$A0,$00        ; . . 2 1 3 3 3 3 3 3 3 3 3 3 3 1 2 2 . . . . . .
        DB      $02,$FB,$FF,$FE,$00,$00        ; . . . 2 3 3 2 3 3 3 3 3 3 3 3 2 . . . . . . . .
        DB      $00,$ED,$FF,$FE,$00,$00        ; . . . . 3 2 3 1 3 3 3 3 3 3 3 2 . . . . . . . .
        DB      $00,$9F,$FF,$FE,$A0,$00        ; . . . . 2 1 3 3 3 3 3 3 3 3 3 2 2 2 . . . . . .
        DB      $02,$57,$DF,$FD,$58,$00        ; . . . 2 1 1 1 3 3 1 3 3 3 3 3 1 1 1 2 . . . . .
        DB      $00,$95,$5B,$75,$60,$00        ; . . . . 2 1 1 1 1 1 2 3 1 3 1 1 1 2 . . . . . .
        DB      $00,$25,$62,$55,$80,$00        ; . . . . . 2 1 1 1 2 . 2 1 1 1 1 2 . . . . . . .
        DB      $00,$09,$80,$96,$00,$00        ; . . . . . . 2 1 2 . . . 2 1 1 2 . . . . . . . .
        DB      $00,$02,$00,$28,$00,$00        ; . . . . . . . 2 . . . . . 2 2 . . . . . . . . .

;*******************************************************************************
; GORF_EXPLODE2
; 5 bytes/row = 20 pixels wide, 22 rows
;*******************************************************************************
GORF_EXPLODE2:
        DB      $05,$16                        ; width bytes, height rows
        DB      $80,$00,$08,$00,$00            ; 2 . . . . . . . . . 2 . . . . . . . . .
        DB      $20,$00,$20,$00,$02            ; . 2 . . . . . . . 2 . . . . . . . . . 2
        DB      $08,$00,$80,$00,$08            ; . . 2 . . . . . 2 . . . . . . . . . 2 .
        DB      $02,$00,$80,$00,$20            ; . . . 2 . . . . 2 . . . . . . . . 2 . .
        DB      $00,$A2,$A1,$40,$80            ; . . . . 2 2 . 2 2 2 . 1 1 . . . 2 . . .
        DB      $00,$2B,$7E,$52,$00            ; . . . . . 2 2 3 1 3 3 2 1 1 . 2 . . . .
        DB      $00,$AF,$FF,$94,$02            ; . . . . 2 2 3 3 3 3 3 3 2 1 1 . . . . 2
        DB      $00,$2D,$FF,$F4,$08            ; . . . . . 2 3 1 3 3 3 3 3 3 1 . . . 2 .
        DB      $00,$2B,$7F,$D5,$A0            ; . . . . . 2 2 3 1 3 3 3 3 1 1 1 2 2 . .
        DB      $00,$0B,$7F,$D8,$00            ; . . . . . . 2 3 1 3 3 3 3 1 2 . . . . .
        DB      $00,$AB,$7F,$DA,$00            ; . . . . 2 2 2 3 1 3 3 3 3 1 2 2 . . . .
        DB      $02,$BD,$FF,$F6,$00            ; . . . 2 2 3 3 1 3 3 3 3 3 3 1 2 . . . .
        DB      $0A,$DF,$FF,$E8,$00            ; . . 2 2 3 1 3 3 3 3 3 3 3 2 2 . . . . .
        DB      $2B,$D7,$FF,$60,$00            ; . 2 2 3 3 1 1 3 3 3 3 3 1 2 . . . . . .
        DB      $02,$B5,$FD,$68,$00            ; . . . 2 2 3 1 1 3 3 3 1 1 2 2 . . . . .
        DB      $02,$AF,$FD,$FA,$00            ; . . . 2 2 2 3 3 3 3 3 1 3 3 2 2 . . . .
        DB      $08,$2E,$AF,$F8,$00            ; . . 2 . . 2 3 2 2 2 3 3 3 3 2 . . . . .
        DB      $20,$28,$2B,$EA,$00            ; . 2 . . . 2 2 . . 2 2 3 3 2 2 2 . . . .
        DB      $80,$00,$0A,$A2,$80            ; 2 . . . . . . . . . 2 2 2 2 . 2 2 . . .
        DB      $00,$00,$08,$00,$20            ; . . . . . . . . . . 2 . . . . . . 2 . .
        DB      $00,$00,$20,$00,$08            ; . . . . . . . . . 2 . . . . . . . . 2 .
        DB      $00,$00,$80,$00,$02            ; . . . . . . . . 2 . . . . . . . . . . 2

;*******************************************************************************
; GORF_EXPLODE3
; 5 bytes/row = 20 pixels wide, 23 rows
;*******************************************************************************
GORF_EXPLODE3:
        DB      $05,$17                        ; width bytes, height rows
        DB      $00,$00,$04,$03,$00            ; . . . . . . . . . . 1 . . . . 3 . . . .
        DB      $00,$30,$41,$40,$30            ; . . . . . 3 . . 1 . . 1 1 . . . . 3 . .
        DB      $00,$8C,$20,$08,$00            ; . . . . 2 . 3 . . 2 . . . . 2 . . . . .
        DB      $00,$00,$01,$00,$00            ; . . . . . . . . . . . 1 . . . . . . . .
        DB      $30,$90,$80,$C0,$20            ; . 3 . . 2 1 . . 2 . . . 3 . . . . 2 . .
        DB      $0C,$20,$02,$00,$0F            ; . . 3 . . 2 . . . . . 2 . . . . . . 3 3
        DB      $30,$08,$28,$0C,$30            ; . 3 . . . . 2 . . 2 2 . . . 3 . . 3 . .
        DB      $00,$00,$30,$00,$03            ; . . . . . . . . . 3 . . . . . . . . . 3
        DB      $0F,$C2,$A0,$14,$05            ; . . 3 3 3 . . 2 2 2 . . . 1 1 . . . 1 1
        DB      $03,$C1,$80,$50,$1A            ; . . . 3 3 . . 1 2 . . . 1 1 . . . 1 2 2
        DB      $0F,$A1,$A1,$44,$4A            ; . . 3 3 2 2 . 1 2 2 . 1 1 . 1 . 1 . 2 2
        DB      $F0,$29,$85,$50,$54            ; 3 3 . . . 2 2 1 2 . 1 1 1 1 . . 1 1 1 .
        DB      $FC,$0A,$81,$54,$50            ; 3 3 3 . . . 2 2 2 . . 1 1 1 1 . 1 1 . .
        DB      $0C,$80,$2A,$90,$00            ; . . 3 . 2 . . . . 2 2 2 2 1 . . . . . .
        DB      $0F,$54,$08,$48,$50            ; . . 3 3 1 1 1 . . . 2 . 1 . 2 . 1 1 . .
        DB      $30,$14,$20,$40,$10            ; . 3 . . . 1 1 . . 2 . . 1 . . . . 1 . .
        DB      $01,$03,$0C,$04,$0C            ; . . . 1 . . . 3 . . 3 . . . 1 . . . 3 .
        DB      $00,$10,$00,$00,$40            ; . . . . . 1 . . . . . . . . . . 1 . . .
        DB      $20,$00,$04,$00,$00            ; . 2 . . . . . . . . 1 . . . . . . . . .
        DB      $01,$0C,$00,$0C,$04            ; . . . 1 . . 3 . . . . . . . 3 . . . 1 .
        DB      $00,$00,$08,$00,$00            ; . . . . . . . . . . 2 . . . . . . . . .
        DB      $00,$04,$00,$10,$C0            ; . . . . . . 1 . . . . . . 1 . . 3 . . .
        DB      $00,$00,$12,$0C,$00            ; . . . . . . . . . 1 . 2 . . 3 . . . . .

;*******************************************************************************
; GORF_EXPLODE4
; 5 bytes/row = 20 pixels wide, 23 rows
;*******************************************************************************
GORF_EXPLODE4:
        DB      $05,$17                        ; width bytes, height rows
        DB      $00,$00,$C0,$00,$00            ; . . . . . . . . 3 . . . . . . . . . . .
        DB      $00,$08,$00,$30,$00            ; . . . . . . 2 . . . . . . 3 . . . . . .
        DB      $00,$00,$30,$00,$04            ; . . . . . . . . . 3 . . . . . . . . 1 .
        DB      $01,$00,$00,$20,$00            ; . . . 1 . . . . . . . . . 2 . . . . . .
        DB      $00,$00,$00,$00,$00            ; . . . . . . . . . . . . . . . . . . . .
        DB      $02,$04,$0D,$01,$20            ; . . . 2 . . 1 . . . 3 1 . . . 1 . 2 . .
        DB      $02,$00,$40,$00,$00            ; . . . 2 . . . . 1 . . . . . . . . . . .
        DB      $00,$30,$01,$08,$00            ; . . . . . 3 . . . . . 1 . . 2 . . . . .
        DB      $10,$00,$00,$80,$00            ; . 1 . . . . . . . . . . 2 . . . . . . .
        DB      $01,$02,$00,$01,$30            ; . . . 1 . . . 2 . . . . . . . 1 . 3 . .
        DB      $C0,$00,$08,$00,$03            ; 3 . . . . . . . . . 2 . . . . . . . . 3
        DB      $08,$20,$00,$0C,$04            ; . . 2 . . 2 . . . . . . . . 3 . . . 1 .
        DB      $00,$01,$22,$00,$80            ; . . . . . . . 1 . 2 . 2 . . . . 2 . . .
        DB      $8C,$40,$00,$33,$00            ; 2 . 3 . 1 . . . . . . . . 3 . 3 . . . .
        DB      $01,$00,$20,$03,$01            ; . . . 1 . . . . . 2 . . . . . 3 . . . 1
        DB      $40,$04,$03,$10,$08            ; 1 . . . . . 1 . . . . 3 . 1 . . . . 2 .
        DB      $00,$00,$40,$00,$00            ; . . . . . . . . 1 . . . . . . . . . . .
        DB      $0C,$30,$00,$00,$20            ; . . 3 . . 3 . . . . . . . . . . . 2 . .
        DB      $00,$00,$00,$00,$00            ; . . . . . . . . . . . . . . . . . . . .
        DB      $00,$C4,$00,$40,$20            ; . . . . 3 . 1 . . . . . 1 . . . . 2 . .
        DB      $00,$10,$20,$03,$00            ; . . . . . 1 . . . 2 . . . . . 3 . . . .
        DB      $00,$03,$04,$00,$00            ; . . . . . . . 3 . . 1 . . . . . . . . .
        DB      $00,$01,$08,$40,$00            ; . . . . . . . 1 . . 2 . 1 . . . . . . .

;*******************************************************************************
; GORF_UNK2
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GORF_UNK2:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $00,$00                        ; . . . . . . . .
        DB      $00,$00                        ; . . . . . . . .
        DB      $00,$04                        ; . . . . . . 1 .
        DB      $10,$C0                        ; . 1 . . 3 . . .
        DB      $0F,$FC                        ; . . 3 3 3 3 3 .
        DB      $03,$70                        ; . . . 3 1 3 . .
        DB      $0D,$5C                        ; . . 3 1 1 1 3 .
        DB      $0F,$7C                        ; . . 3 3 1 3 3 .
        DB      $13,$C0                        ; . 1 . 3 3 . . .
        DB      $00,$04                        ; . . . . . . 1 .
        DB      $00,$00                        ; . . . . . . . .

;*******************************************************************************
; GORF_UNK3
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GORF_UNK3:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $41,$01                        ; 1 . . 1 . . . 1
        DB      $10,$44                        ; . 1 . . 1 . 1 .
        DB      $05,$50                        ; . . 1 1 1 1 . .
        DB      $15,$D5                        ; . 1 1 1 3 1 1 1
        DB      $07,$F4                        ; . . 1 3 3 3 1 .
        DB      $45,$D4                        ; 1 . 1 1 3 1 1 .
        DB      $17,$50                        ; . 1 1 3 1 1 . .
        DB      $05,$15                        ; . . 1 1 . 1 1 1
        DB      $11,$14                        ; . 1 . 1 . 1 1 .
        DB      $00,$04                        ; . . . . . . 1 .
        DB      $40,$41                        ; 1 . . . 1 . . 1

;*******************************************************************************
; GORF_UNK4
; 2 bytes/row = 8 pixels wide, 12 rows
;*******************************************************************************
GORF_UNK4:
        DB      $02,$0C                        ; width bytes, height rows
        DB      $00,$11                        ; . . . . . 1 . 1
        DB      $40,$00                        ; 1 . . . . . . .
        DB      $04,$00                        ; . . 1 . . . . .
        DB      $00,$C0                        ; . . . . 3 . . .
        DB      $03,$F1                        ; . . . 3 3 3 . 1
        DB      $0F,$B1                        ; . . 3 3 2 3 . 1
        DB      $03,$B0                        ; . . . 3 2 3 . .
        DB      $03,$FC                        ; . . . 3 3 3 3 .
        DB      $10,$C0                        ; . 1 . . 3 . . .
        DB      $00,$00                        ; . . . . . . . .
        DB      $01,$00                        ; . . . 1 . . . .
        DB      $00,$04                        ; . . . . . . 1 .

;*******************************************************************************
; GORF_UNK5
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GORF_UNK5:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $C1,$08                        ; 3 . . 1 . . 2 .
        DB      $C1,$0C                        ; 3 . . 1 . . 3 .
        DB      $30,$30                        ; . 3 . . . 3 . .
        DB      $01,$43                        ; . . . 1 1 . . 3
        DB      $15,$50                        ; . 1 1 1 1 1 . .
        DB      $94,$52                        ; 2 1 1 . 1 1 . 2
        DB      $11,$40                        ; . 1 . 1 1 . . .
        DB      $15,$51                        ; . 1 1 1 1 1 . 1
        DB      $81,$00                        ; 2 . . 1 . . . .
        DB      $C1,$28                        ; 3 . . 1 . 2 2 .
        DB      $08,$11                        ; . . 2 . . 1 . 1

;*******************************************************************************
; GORF_UNK6
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GORF_UNK6:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $00,$04                        ; . . . . . . 1 .
        DB      $20,$00                        ; . 2 . . . . . .
        DB      $00,$30                        ; . . . . . 3 . .
        DB      $00,$00                        ; . . . . . . . .
        DB      $80,$04                        ; 2 . . . . . 1 .
        DB      $0C,$06                        ; . . 3 . . . 1 2
        DB      $00,$40                        ; . . . . 1 . . .
        DB      $10,$00                        ; . 1 . . . . . .
        DB      $08,$8C                        ; . . 2 . 2 . 3 .
        DB      $C0,$00                        ; 3 . . . . . . .
        DB      $02,$01                        ; . . . 2 . . . 1

            DB      $13, $15
;*******************************************************************************
            ld      (de),a
            add     a,a
            ld      de,$0FA0
            ld      (bc),a
            cp      $01
            ld      d,b
            djnz    $26C0
            dec     b
            ex      af,af'
            ld      l,d
            inc     b
            ld      bc,$030A
            ld      d,$34
            dec     d
            dec     d
            inc     bc
            inc     b
            rst     $08
            ld      l,l
            nop
            ld      l,c
            ld      h,$1E
            djnz    $26EC
            nop
            inc     de
            ld      (hl),l
            ld      (de),a
            ld      h,(hl)
            ld      de,$1553
            rra
            ld      d,$FF
            rrca
            rra
            ld      bc,_Aquote
            djnz    $26A0
            dec     b
            ld      (bc),a
            inc     bc
            rst     $38
            ld      hl,($020A)
            inc     bc
            ld      a,(de)
            rrca
            nop
            rst     $38
            ex      af,af'
            dec     d
            rra
            ld      d,$FF
            ld      a,(bc)
            inc     bc
            ex      af,af'
            inc     bc
            inc     b
            inc     de
            inc     l
            ld      (de),a
            ld      ($1F11),hl
            djnz    $26CB
            dec     b
            djnz    $265E
            inc     b
            ld      bc,$010A
            rrca
            ld      (hl),b
            inc     b
            ld      bc,$1610
            ld      h,(hl)
            dec     d
            ld      h,$14
            ld      c,a
            inc     bc
            inc     b
            inc     de
            inc     l
            ld      (de),a
            ld      ($1F11),hl
            rrca
            jr      nc,$26DC
            ld      bc,$1000
            ld      h,b
            dec     b
            jr      nc,$273F
            call    m,$0A01
            ld      bc,$6616
            dec     d
            ld      d,$03
            djnz    $2715
            dec     b
            dec     b
            cpl
            call    m,$0A01
            ld      bc,$060F
            call    m,$1E01
            inc     bc
            inc     b
            rst     $08
            ld      l,l
            nop
            adc     a,h
            ld      h,$2C
            djnz    $2776
            nop
            ex      af,af'
            exx
            inc     b
            ld      l,l
            nop
            adc     a,h
            ld      h,$7B
            djnz    $276C
            nop
            rst     $08
            ld      l,l
            nop
            or      e
            ld      h,$1E
            djnz    $2775
            nop
            rst     $08
            ld      l,l
            nop
            rst     $08
            ld      h,$1E
            djnz    $277E
            nop
            inc     de
            djnz    $2733
            add     hl,bc
            ld      de,$1007
            ld      (bc),a
            ld      b,$02
            sub     b
            ld      a,(bc)
            inc     bc
            ld      a,(bc)
            ld      bc,$730F
            inc     bc
            ld      bc,$1503
            dec     de
            ld      d,$BB
            inc     bc
            djnz    $26C9
            inc     de
            ld      de,$4412
            ld      de,$053E
            ld      (hl),h
            sub     b
            rst     $38
            inc     b
            ld      a,(bc)
            ld      bc,$1003
            ld      (hl),h
            dec     b
            ld      (bc),a
            ld      (hl),h
            rst     $38
            ld      bc,$030F
            rst     $38
            ld      bc,$086B
            ld      a,(bc)
            ld      bc,$0403
            inc     de
            inc     d
            ld      (de),a
            dec     b
            ld      de,$1003
            ld      (bc),a
            ld      b,$02
            sub     b
            ld      a,(bc)
            inc     bc
            ld      a,(bc)
            ld      bc,$730F
            inc     bc
            ld      bc,$1503
            dec     de
            ld      d,$BB
            inc     bc
            djnz    $2775
            inc     de
            inc     de
            ld      (de),a
            ld      sp,$5411
            dec     b
            ld      (bc),a
            ld      ($0101),hl
            ld      a,(bc)
            ld      bc,$1003
            ld      ($2205),hl
            ld      (hl),b
            ld      bc,$0F01
            inc     bc
            cp      $01
            ld      l,e
            ex      af,af'
            ld      a,(bc)
            ld      bc,$0403
            rst     $08
            ld      l,l
            nop
            ld      e,b
            daa
            ld      a,e
            djnz    $2807
            nop
            ld      e,$27
            inc     l
            djnz    $2801
            nop
            inc     de
            inc     d
            ld      (de),a
            ret     z
            ld      de,$16FD
            ld      (hl),a
            dec     d
            rlca
            inc     d
            ld      c,b
            djnz    $27B1
            dec     b
            ld      (bc),a
            ld      d,b
            ld      bc,$0A01
            ld      bc,$1603
            ld      ($0315),hl
            dec     b
            jr      nc,$280E
            rst     $38
            ld      bc,$3A0F
            ld      (bc),a
            ld      bc,$0100
            dec     e
            rrca
            nop
            cp      $01
            ld      a,($1D01)
            ld      (bc),a
            ret     nz
            daa
; GORFOS Block 0189 names the looping dive score KBSCORE.
KBSCORE     EQU     $27A1

; ----> playkbs  Start KBSCORE on music processor 2.
;                 bmusic preserves an active priority score on this processor.
playkbs:    ld      hl,KBSCORE
            ld      iy,$D0E1
            jp      bmusic
            ld      a,(PLAYERUP)
            ld      hl,P1FBCTR
            and     a
            jp      z,$27E8
            ld      hl,P2FBCTR
            ret
            call    $27DB
            push    hl
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            ld      l,l
            nop
            nop
            ld      c,$17
            inc     bc
            ld      l,l
            nop
            nop
            ld      bc,$00CE
            ld      l,l
            nop
            ld      e,(hl)
            ld      ($0076),hl
            jr      nz,$27AD
            ld      b,$61
            nop
            rst     $08
            jp      (hl)
            daa
            ret     p
            nop
            sbc     a,(hl)
            nop
            jp      c,$EA01
            inc     bc
            inc     h
            jr      z,$27FE
            daa
            ret     p
            nop
            dec     h
            ld      bc,_0
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            rst     $28
            daa
            ld      e,a
            ld      (bc),a
            ld      h,c
            nop
            rst     $08
            rst     $08
            ld      l,l
            nop
            nop
            ld      c,l
            ld      l,l
            nop
            nop
            djnz    $289E
            nop
            inc     b
            ld      (_LIT),hl
            jr      z,$283C
            xor     c
            ld      b,$61
            nop
            rst     $08
            ld      l,l
            nop
            nop
            ld      c,l
            ld      l,l
            nop
            nop
            and     (hl)
            ld      l,l
            nop
            ld      d,$22
            ld      l,l
            nop
            jr      z,$2851
            xor     c
            ld      b,$61
            nop
            rst     $08
            ld      l,l
            nop
            ld      bc,$F0D0
            nop
            call    po,$EA01
            inc     bc
            adc     a,c
            jr      z,$28CC
            nop
            and     a
            ret     nc
            ret     p
            nop
            call    po,$EA01
            inc     bc
            adc     a,c
            jr      z,$2838
            inc     b
            ld      l,l
            nop
            jr      c,$2840
            ret     p
            nop
            jp      pe,$7C03
            jr      z,$28B3
            jr      z,$285B
            inc     bc
            ld      a,(hl)
            jr      z,$28A4
            jr      z,$284F
            inc     b
            halt
            nop
            djnz    $28F1
            nop
            and     a
            ret     nc
            cp      $00
            ld      h,c
            nop
            rst     $08
            ld      h,(hl)
            ld      a,(de)
            sbc     a,b
            rrca
            ld      d,c
            jr      z,$28F4
            nop
            rst     $08
            call    $5D04
            inc     d
            sbc     a,(hl)
            nop
            halt
            nop
            ex      af,af'
            ret     p
            ld      (bc),a
            jp      c,$B514
            inc     b
            call    m,$4E1C
            inc     d
            halt
            nop
            call    z,_LITbyte
            ld      a,(bc)
            ret     p
            ld      (bc),a
            halt
            nop
            dec     c
            halt
            nop
            add     hl,bc
            ret     p
            ld      (bc),a
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            jr      c,$288F
            ret     p
            nop
            jp      pe,$DF03
            jr      z,$2933
            nop
            rra
            ret     nc
            sub     l
            jr      $2939
            nop
            nop
            ld      c,c
            ld      l,l
            nop
            nop
            sub     (hl)
            ld      l,l
            nop
            ex      af,af'
            inc     b
            ld      l,l
            nop
            rra
            ret     nc
            jp      po,$F303
            jr      z,$294D
            nop
            inc     c
            ret     nc
            sub     l
            jr      $2953
            nop
            nop
            ld      c,c
            sbc     a,b
            nop
            ld      l,l
            nop
            ex      af,af'
            inc     b
            ld      l,l
            nop
            inc     c
            ret     nc
            call    $3504
            ld      a,(bc)
            ret     nc
            inc     b
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            add     hl,sp
            ret     nc
            ret     p
            nop
            jp      pe,$0C03
            add     hl,hl
            and     e
            inc     b
            jp      po,$0E03
            add     hl,hl
            xor     h
            inc     b
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            nop
            ld      c,c
            sbc     a,b
            nop
            ld      l,l
            nop
            ex      af,af'
            inc     b
            ld      l,l
            nop
            inc     c
            ret     nc
            dec     (hl)
            ld      a,(bc)
            daa
            jr      z,$2991
            nop
            nop
            ld      c,l
            ld      l,l
            nop
            nop
            dec     a
            ld      l,l
            nop
            jr      z,$2933
            sbc     a,b
            nop
            call    nz,$6D09
            nop
            nop
            ld      c,l
            ld      l,l
            nop
            nop
            ld      (hl),l
            ld      l,l
            nop
            jr      z,$2943
            ld      l,l
            nop
            ld      (hl),$D0
            ret     p
            nop
            ld      e,l
            ld      a,(bc)
            ld      l,l
            nop
            add     a,b
            ld      c,c
            ld      l,l
            nop
            nop
            ld      c,e
            ld      l,l
            nop
            jr      z,$2957
            ld      l,l
            nop
            ld      a,($F0D0)
            nop
            ld      l,b
            inc     b
            ld      h,a
            add     hl,hl
            and     e
            inc     b
            and     e
            inc     b
            ei
            jr      z,$295F
            jr      z,$2912
            inc     b
            dec     (hl)
            ld      a,(bc)
            ld      l,l
            nop
            ld      bc,$F0D0
            nop
            jp      pe,$7F03
            add     hl,hl
            ld      l,l
            nop
            nop
            ld      b,(hl)
            sbc     a,(hl)
            nop
            djnz    $2985
            jp      po,$8103
            add     hl,hl
            rlca
            jr      z,$29E3
            nop
            exx
            ld      bc,$0E16
            ld      a,(DEMOMODE)
            and     a
            jp      z,$2990
            set     0,b
            in      a,(c)
            exx
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            djnz    $29C1
            ld      l,l
            nop
            nop
            ld      c,c
            ld      l,l
            nop
            nop
            sub     (hl)
            ld      l,l
            nop
            ex      af,af'
            inc     b
            ld      l,l
            nop
            rra
            ret     nc
            dec     (hl)
            ld      a,(bc)
            inc     a
            jr      z,$2A0E
            nop
            rst     $08
            sub     h
            jr      z,$2A1F
            nop
            add     hl,sp
            ret     nc
            ret     p
            nop
            jp      pe,$C103
            add     hl,hl
            sub     l
            add     hl,hl
            jp      po,$C303
            add     hl,hl
            djnz    $29EC
            ld      bc,$3D1D
            dec     d
            and     h
            nop
            ld      l,l
            nop
            ld      e,e
            exx
            rst     $30
            nop
            inc     bc
            ld      bc,$2983
            ld      h,c
            nop
            ld      bc,$0402
            ex      af,af'
            djnz    $29FB
            ld      b,b
            add     a,b
            rst     $08
            ld      l,l
            nop
            nop
            ld      (bc),a
            ld      l,l
            nop
            or      a
            exx
            rst     $30
            nop
            halt
            nop
            ld      b,b
            ld      l,l
            nop
            push    bc
            exx
            rst     $30
            nop
            ld      l,l
            nop
            nop
            cp      $6D
            nop
            cp      e
            exx
            rst     $30
            nop
            halt
            nop
            nop
            ld      l,l
            nop
            cp      l
            exx
            rst     $30
            nop
            ld      l,l
            nop
            nop
            ld      b,d
            ld      l,l
            nop
            cp      a
            exx
            rst     $30
            nop
            ld      h,c
            nop
            ld      c,(ix+$07)
            ld      (ix+$07),$00
            push    bc
            ld      a,(ix+$0e)
            inc     a
            ld      (ix+$0e),a
            cp      $47
            jp      c,$2A28
            res     7,(ix+$00)
            bit     5,(ix+$00)
            jp      nz,$2A35
            call    $14BF
            jp      $2A39
            res     5,(ix+$00)
            bit     7,(ix+$00)
            jp      z,$2A43
            call    $1D32
            pop     bc
            bit     7,(ix+$00)
            jp      z,$2A4E
            dec     c
            jr      nz,$2A17
            jp      $1A6B
            ld      (hl),c
            ld      hl,($D942)
            jp      (hl)
            ld      iy,$D96B
            ld      l,(iy+$0d)
            ld      h,(iy+$0e)
            ld      de,$0500
            add     hl,de
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      de,$0700
            add     hl,de
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      (ix+$0a),$60
            ret
            call    $2A51
            call    $1F3E
            call    $2A56
            ld      (ix+$00),$B6
            ld      (ix+$1b),$20
            ld      (ix+$08),$30
            ld      hl,$2A10
            ld      (ix+$05),l
            ld      (ix+$06),h
            ld      hl,($D953)
            ld      (ix+$27),l
            ld      (ix+$28),h
            ld      hl,$2401
            ld      (ix+$1d),l
            ld      (ix+$1e),h
            ld      (ix+$10),$01
            call    $1F63
            ret
            call    $2A51
            call    $2A56
            ret
            ld      hl,$D955
            call    $061F
            ld      c,a
            xor     (hl)
            and     $10
            ret     z
            ld      a,c
            and     $10
            jr      z,$2AD0
            ld      (hl),c
            ret
            ld      ix,($D95B)
            di
            bit     7,(ix+$00)
            jp      z,$2AE7
            bit     0,(ix+$00)
            ret     nz
            call    $2AB7
            jp      $2AEF
            ld      a,(ix+$00)
            and     a
            ret     nz
            call    $2A7F
PLAYER_SHOT_SCORE EQU $2669             ; GORFOS 0186: player-fire score (1DSCORE)

; ----> play_player_shot_sound  Gameplay launcher for the player-fire score.
;                               BMUSIC preserves an active priority score on processor 1.
play_player_shot_sound:
            ld      hl,PLAYER_SHOT_SCORE
            ld      iy,$D0B1
            jp      bmusic
            ld      bc,$0E15
            ld      ix,($D95B)
            bit     7,(ix+$00)
            jp      nz,$2B09
            set     0,b
            in      a,(c)
            ret
            push    ix
            push    iy
            push    bc
            call    $2AF9
            call    $2ABE
            ei
            pop     bc
            pop     iy
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            ld      a,h
            and     a
            jr      z,$2B24
            ex      de,hl
            ld      a,l
            ld      hl,$0000
            rra
            jr      nc,$2B2C
            add     hl,de
            ex      de,hl
            add     hl,hl
            ex      de,hl
            and     a
            jr      nz,$2B28
            ret
            ld      a,h
            cpl
            ld      h,a
            ld      a,l
            cpl
            ld      l,a
            inc     hl
            ret
            push    hl
            ld      e,a
            ld      d,$00
            ld      hl,$0AE8
            add     hl,de
            ld      e,(hl)
            pop     hl
            call    $2B1F
            ld      a,h
            ret
            ld      a,($DCAB)
            ld      e,a
            ld      a,$3F
            sub     e
            jr      $2B3B
            ld      a,(ix+$00)
            and     a
            jr      z,$2B91
            dec     a
            ld      (ix+$00),a
            ld      e,(ix+$01)
            ld      d,(ix+$02)
            ld      l,(ix+$03)
            ld      h,(ix+$04)
            add     hl,de
            ld      (ix+$03),l
            ld      (ix+$04),h
            ex      de,hl
            ld      c,(ix+$05)
            ld      b,(ix+$06)
            ld      l,(ix+$07)
            ld      h,(ix+$08)
            add     hl,bc
            ld      (ix+$07),l
            ld      (ix+$08),h
            ld      c,$20
            call    RELABS
            ld      a,c
            di
            out     ($0C),a
            ld      (hl),$C0
            ei
            ret
            ld      a,(ix+$0c)
            and     a
            jr      z,$2BB5
            ld      (ix+$00),a
            ld      (ix+$0c),$00
            ld      l,(ix+$09)
            ld      h,(ix+$0a)
            ld      (ix+$03),l
            ld      (ix+$04),h
            ld      a,(ix+$0b)
            ld      (ix+$08),a
            ld      (ix+$07),$00
            ret
            ld      hl,($DCA7)
            jp      (hl)
            rst     $08
            ld      l,l
            nop
            and     a
            call    c,$00F7
            ld      l,l
            nop
            ret     nz
            ld      bc,_0
            ld      b,h
            ld      (bc),a
            sbc     a,b
            nop
            adc     a,l
            ld      (bc),a
            adc     a,b
            nop
            rst     $20
            jp      c,$00FE
            ld      e,a
            ld      (bc),a
            ld      h,c
            nop
            push    ix
            push    iy
            exx
            ld      bc,$0020
            ld      ix,$DAE7
            push    bc
            call    $2B53
            ld      de,$000E
            add     ix,de
            pop     bc
            dec     bc
            ld      a,c
            or      b
            jr      nz,$2BE2
            exx
            pop     iy
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            call    random
            ld      a,l
            and     $03
            ld      ($DCA9),a
            ld      a,d
            and     $3F
            ld      ($DCAB),a
            ld      hl,($DCB9)
            call    $2B3B
            ld      ($DCB1),a
            ld      a,($DCAB)
            ld      hl,($DCBB)
            call    $2B3B
            ld      ($DCB5),a
            ld      hl,($DCBD)
            call    $2B4A
            ld      ($DCB3),a
            ld      hl,($DCBF)
            call    $2B4A
            ld      ($DCB7),a
            ld      a,($DCA9)
            and     $02
            jp      nz,$2C3C
            ld      a,($DCC3)
            jp      $2C3F
            ld      a,($DCC1)
            ld      c,a
            ld      a,($DCB5)
            cp      c
            jr      c,$2C75
            push    bc
            ld      de,($DCB3)
            ld      a,($DCB7)
            sub     e
            ld      e,a
            ld      a,($DCB5)
            sub     c
            ld      l,a
            ld      h,$00
            ld      d,h
            call    $2B1F
            ld      de,($DCB1)
            ld      a,($DCB5)
            sub     e
            ld      e,a
            ld      d,$00
            call    unsdiv2
            ld      a,($DCB7)
            sub     e
            ld      ($DCB7),a
            pop     bc
            ld      a,c
            ld      ($DCB5),a
            ld      a,($DCA9)
            inc     a
            and     $03
            cp      $02
            jp      nc,$2C86
            ld      a,($DCC7)
            jp      $2C89
            ld      a,($DCC5)
            ld      c,a
            ld      a,($DCB7)
            cp      c
            jr      c,$2CBE
            push    bc
            ld      de,($DCB1)
            ld      a,($DCB5)
            sub     e
            ld      e,a
            ld      a,($DCB7)
            sub     c
            ld      l,a
            ld      h,$00
            call    $2B1F
            ld      de,($DCB3)
            ld      a,($DCB7)
            sub     e
            ld      e,a
            ld      d,$00
            call    unsdiv2
            ld      a,($DCB5)
            sub     e
            ld      ($DCB5),a
            pop     bc
            ld      a,c
            ld      ($DCB7),a
            ld      de,($DCB1)
            ld      a,($DCB5)
            sub     e
            ld      c,a
            ld      de,($DCB3)
            ld      a,($DCB7)
            sub     e
            ld      b,a
            cp      c
            jp      nc,$2CF3
            ld      (ix+$0c),c
            ld      (ix+$00),c
            ld      (ix+$01),$40
            ld      (ix+$02),$00
            ld      h,b
            ld      l,$00
            ld      e,c
            ld      d,l
            call    unsdiv2
            ld      (ix+$05),e
            ld      (ix+$06),d
            jp      $2D18
            ld      (ix+$0c),b
            ld      (ix+$00),b
            ld      (ix+$05),$00
            ld      (ix+$06),$01
            ld      a,c
            rrca
            rrca
            ld      h,a
            and     $C0
            ld      l,a
            ld      a,h
            and     $3F
            ld      h,a
            ld      e,b
            ld      d,$00
            call    unsdiv2
            ld      (ix+$01),e
            ld      (ix+$02),d
            ld      a,($DCB1)
            rrca
            rrca
            ld      d,a
            and     $C0
            ld      e,a
            ld      a,d
            and     $3F
            ld      d,a
            ld      hl,($DCAD)
            ld      a,($DCA9)
            and     $02
            jp      z,$2D47
            and     a
            sbc     hl,de
            ex      de,hl
            ld      l,(ix+$01)
            ld      h,(ix+$02)
            call    $2B33
            ld      (ix+$01),l
            ld      (ix+$02),h
            ex      de,hl
            jp      $2D48
            add     hl,de
            ld      (ix+$03),l
            ld      (ix+$04),h
            ld      (ix+$09),l
            ld      (ix+$0a),h
            ld      a,($DCB3)
            ld      d,a
            ld      e,$00
            ld      hl,($DCAF)
            ld      a,($DCA9)
            inc     a
            and     $03
            cp      $02
            jp      c,$2D7F
            and     a
            sbc     hl,de
            ex      de,hl
            ld      l,(ix+$05)
            ld      h,(ix+$06)
            call    $2B33
            ld      (ix+$05),l
            ld      (ix+$06),h
            ex      de,hl
            jp      $2D80
            add     hl,de
            ld      (ix+$07),$00
            ld      (ix+$08),h
            ld      (ix+$0b),h
            ret
            call    $2BF8
            ld      (ix+$0c),$00
            ret
            rst     $08
            xor     c
            nop
            ld      l,l
            nop
            xor     a
            call    c,$00F7
            ld      l,l
            nop
            xor     l
            call    c,$00F7
            ld      l,l
            nop
            nop
            ld      bc,$044A
            and     h
            nop
            ld      l,l
            nop
            push    bc
            call    c,$00F7
            halt
            nop
            ret     nz
            adc     a,$00
            ld      de,$6D01
            nop
            rst     $00
            call    c,$00F7
            halt
            nop
            ld      b,b
            ld      c,d
            inc     b
            and     h
            nop
            and     h
            nop
            halt
            nop
            rst     $38
            jp      c,$EA01
            inc     bc
            call    nc,$B12D
            nop
            halt
            nop
            rst     $38
            ld      l,l
            nop
            pop     bc
            call    c,$00F7
            ld      l,l
            nop
            inc     h
            ld      bc,$00CE
            ld      de,$A401
            nop
            halt
            nop
            rst     $38
            jp      c,$EA01
            inc     bc
            jp      p,$B12D
            nop
            halt
            nop
            rst     $38
            ld      l,l
            nop
            jp      $F7DC
            nop
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            cp      a
            call    c,$00F7
            ld      l,l
            nop
            cp      e
            call    c,$00F7
            ld      l,l
            nop
            cp      l
            call    c,$00F7
            ld      l,l
            nop
            cp      c
            call    c,$00F7
            ld      h,c
            nop
            ld      a,b
            and     a
            jr      z,$2E20
            sra     h
            rr      l
            djnz    $2E19
            ret
            ld      l,a
            ld      h,a
            ret
            ld      b,$08
            ld      (bc),a
            and     b
            ld      b,$08
            ld      (bc),a
            jr      nz,$2E3C
            inc     c
            nop
            nop
            nop
            nop
            ld      a,(de)
            inc     b
            ld      b,$00
            sub     l
            inc     hl
            ex      af,af'
            inc     hl
            ld      l,$1A
            call    m,$08FA
            inc     hl
            ld      l,$1A
            ld      bc,$0002
            cp      e
            inc     hl
            ex      af,af'
            inc     hl
            ld      l,$1A
            rst     $38
            cp      $00
            add     hl,bc
            inc     h
            ld      a,(bc)
            inc     hl
            ld      l,$08
            inc     l
            ld      l,$00
            dec     b
            dec     h
            ex      af,af'
            inc     hl
            ld      l,$00
            ld      a,d
            dec     h
            ex      af,af'
            inc     hl
            ld      l,$00
            ld      b,$24
            ld      b,$01
            ld      (de),a
            ld      a,($D94D)
            and     a
            ret     nz
            ld      a,($D94F)
            and     a
            ret     nz
            ld      a,$01
            ld      ($D94D),a
            ld      hl,$2E52
            call    $1F49
            ld      hl,$1D48
            ld      (ix+$05),l
            ld      (ix+$06),h
            ret
            and     $07
            rlca
            rlca
            ld      h,a
            ld      l,$00
            ret
            and     $38
            rlca
            ld      h,a
            ld      l,$00
            ret
            ld      a,h
            cp      b
            ret     nz
            ld      a,l
            cp      c
            ret     nz
            ld      a,d
            cpl
            ld      d,a
            ld      a,e
            cpl
            ld      e,a
            inc     de
            xor     a
            ret
            ld      a,c
            and     $07
            ld      e,a
            ld      d,$00
            ld      hl,$29D5
            add     hl,de
            ld      b,(hl)
            ld      a,c
            rrca
            rrca
            rrca
            and     $07
            ld      e,a
            ld      hl,$D9D5
            add     hl,de
            ld      a,(hl)
            and     b
            ret
            ld      a,c
            and     $07
            ld      e,a
            ld      d,$00
            ld      hl,$29D5
            add     hl,de
            ld      b,(hl)
            ld      a,c
            rrca
            rrca
            rrca
            and     $07
            ld      e,a
            ld      hl,$D9CD
            add     hl,de
            ld      a,(hl)
            and     b
            ret
            xor     d
            cp      a
;******************************************************************************************
; ----> creditcheck  Demo/coin event service.
;
;                    During DEMOMODE this routine services attract-mode joystick chatter
;                    through goyak before evaluating coin/start conditions. Outside DEMOMODE
;                    it returns after the common event update at $1A60.
;******************************************************************************************
creditcheck:
            call    $1A60
            ld      a,(DEMOMODE)
            and     a
            ret     z
            in      a,($10)
            bit     2,a
            jp      z,COLDSTRT
            call    goyak               ; Attract-mode joystick speech / coin effect
            ld      a,($D009)
            and     a
            jr      nz,$2F14
            in      a,($13)
            and     $40
            jr      nz,$2EFE
            in      a,($10)
            and     $30
            cp      $30
            jr      nz,$2F14
            ld      a,(COINSIN)
            and     a
            ret     z
            in      a,($10)
            and     $10
            jr      z,$2F14
            ld      a,(COINSIN)
            cp      $02
            ret     c
            in      a,($10)
            and     $20
            ret     nz
            ld      hl,($2ED6)
            jp      (hl)
            push    bc
            push    ix
            push    iy
            call    $2ED8
            pop     iy
            pop     ix
            pop     bc
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            rst     $30
            nop
            jr      $2F5F
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            jp      (hl)
            nop
            call    po,$EA01
            inc     bc
            ld      l,$2F
            ld      h,c
            nop
            ld      a,($D9B3)
            rlca
            rlca
            ld      e,a
            ld      d,$00
            ld      hl,$D9DD
            add     hl,de
            ex      de,hl
            ld      hl,($D9B5)
            ld      bc,($D9B7)
            bit     7,b
            jp      nz,$2F58
            add     hl,bc
            ex      de,hl
            di
            ld      (hl),e
            inc     hl
            ld      (hl),d
            inc     hl
            ld      de,($D9B9)
            ld      (hl),e
            inc     hl
            ld      (hl),d
            ei
            ret
            ld      a,c
            rlca
            rlca
            ld      e,a
            ld      d,$00
            ld      hl,$D9DD
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            push    hl
            ld      a,c
            call    $2E8E
            add     hl,de
            pop     de
            push    hl
            push    de
            bit     1,h
            ld      hl,($D9C9)
            jp      z,$2F8A
            ld      de,$0010
            add     hl,de
            ld      a,c
            and     $07
            rlca
            ld      e,a
            ld      d,$00
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            push    de
            pop     iy
            ld      a,c
            pop     hl
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            call    $2E86
            add     hl,de
            ex      de,hl
            pop     hl
            ret
            ld      a,($D94B)
            and     a
            ret     z
            ld      hl,$0000
            ld      de,$D9CD
            ld      bc,$0800
            ld      a,(de)
            and     a
            jr      nz,$2FC2
            ld      a,h
            sub     $10
            ld      h,a
            inc     de
            ld      a,c
            add     a,$08
            ld      c,a
            djnz    $2FB2
            ret
            ld      ($D9BD),hl
            ld      de,$D9D4
            ld      hl,$4200
            ld      a,c
            ld      ($D9C1),a
            ld      c,$38
            ld      a,(de)
            and     a
            jr      nz,$2FE0
            ld      a,h
            add     a,$10
            ld      h,a
            dec     de
            ld      a,c
            sub     $08
            ld      c,a
            jr      $2FD1
            ld      ($D9BF),hl
            ld      a,c
            ld      ($D9C3),a
            add     a,$08
            ld      ($D9C5),a
            ret
            ld      a,($D9B3)
            ld      c,a
            call    $2EBD
            jr      z,$3001
            di
            call    $2EA4
            jp      nz,$3015
            call    $2F3E
            ei
            ld      de,$D9B3
            ld      hl,$D9C5
            ld      a,(de)
            inc     a
            ld      (de),a
            cp      (hl)
            jr      c,$2FED
            ld      a,($D9C1)
            ld      (de),a
            ld      hl,($D9C7)
            jp      (hl)
            ld      a,($D9B3)
            call    $2E8E
            ld      de,($D9B5)
            add     hl,de
            ex      de,hl
            ld      a,($D9B3)
            and     $07
            rlca
            ld      c,a
            ld      b,$00
            ld      hl,($DAE5)
            add     hl,bc
            ld      c,(hl)
            inc     hl
            ld      b,(hl)
            push    bc
            ex      (sp),iy
            ld      bc,$0020
            bit     1,d
            jp      z,$3046
            ld      h,(iy+$01)
            dec     h
            ld      l,$00
            add     hl,de
            ex      de,hl
            ld      c,$A0
            push    bc
            ld      a,($D9B3)
            call    $2E86
            ld      bc,($D9B9)
            add     hl,bc
            ex      de,hl
            pop     bc
            bit     7,d
            jp      z,$3069
            di
            ld      ix,$D96B
            call    $2E67
            xor     a
            ld      ($D94B),a
            pop     iy
            ei
            ret
            call    RELABS
            di
            call    writep
            call    $2F3E
            pop     iy
            ld      de,$D9B3
            ld      hl,$D9C5
            ld      a,(de)
            inc     a
            ld      (de),a
            cp      (hl)
            jp      nc,$300D
            ret
            ld      a,($D0A5)
            and     a
            jp      nz,$3093
            push    bc
            push    ix
            call    $2FED
            pop     ix
            pop     bc
            DW      _DSPATCH
;******************************************************************************************
            call    $2EA4
            ret     z
            call    $2F67
            ld      bc,$0020
            call    RELABS
            di
            call    writep
            ei
            ret
            pop     hl
            push    bc
            push    iy
            ld      c,l
            call    $3095
            pop     iy
            pop     bc
            DW      _DSPATCH
;******************************************************************************************
            ld      c,(ix+$29)
            call    $2F67
            ld      b,$00
            ld      (ix+$14),h
            ld      (ix+$08),$28
            ex      de,hl
            ld      a,(ix+$0d)
            and     $C0
            ld      e,a
            ld      d,(ix+$0e)
            and     a
            sbc     hl,de
            jp      z,$30EA
            ld      a,h
            and     a
            jp      p,$30DF
            ld      hl,$FF80
            jp      $30E2
            ld      hl,$0040
            add     hl,de
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            inc     b
            ld      a,b
            and     a
            jp      nz,$3109
            push    iy
            pop     hl
            ld      (ix+$1d),l
            ld      (ix+$1e),h
            res     7,(ix+$00)
            ld      c,(ix+$29)
            call    $2EA4
            ld      a,b
            or      (hl)
            ld      (hl),a
            ld      (ix+$1b),$20
            ret
            call    $1C67
            push    bc
            bit     5,(ix+$00)
            jr      nz,$3119
            call    $14BF
            jr      $311D
            res     5,(ix+$00)
            call    $1C4B
            pop     bc
            bit     7,(ix+$00)
            jr      z,$3135
            push    bc
            call    $30B5
            pop     bc
            bit     7,(ix+$00)
            jr      z,$3135
            dec     c
            jr      nz,$3127
            bit     6,(ix+$00)
            jr      nz,$3140
            call    $1492
            jr      $314E
            bit     7,(ix+$00)
            jr      z,$314E
            res     6,(ix+$00)
            set     5,(ix+$00)
            jp      $1A6B
            ld      l,(ix+$0d)
            ld      h,(ix+$0e)
            inc     h
            inc     h
            ld      de,($D9B9)
            and     a
            sbc     hl,de
            ld      a,h
            and     a
            jp      m,$3183
            rrca
            rrca
            and     $07
            ld      c,a
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      de,($D9B5)
            and     a
            sbc     hl,de
            ld      a,h
            add     a,$02
            rrca
            and     $38
            or      c
            ld      c,a
            call    $2EA4
            ret
            xor     a
            ret
            ld      h,b
            nop
            ld      h,b
            nop
            add     a,b
            nop
            nop
            ld      bc,$0300
            nop
            ld      (bc),a
            ld      d,b
            ld      bc,$0250
            nop
            rst     $28
            dec     h
            ld      b,$05
            nop
            rlca
            ld      h,$06
            dec     b
            nop
            rra
            ld      h,$06
            dec     b
            nop
            add     hl,sp
            ld      h,$06
            dec     b
            nop
            ld      d,c
            ld      h,$06
            dec     b
            nop
            ld      b,$24
            djnz    $31BB
            sub     l
            ld      sp,$0812
            sub     l
            ld      sp,$F104
            ld      hl,$0106
            djnz    $31C6
            ld      bc,$4020
            cp      a
            ld      b,$1C
            jr      nz,$31C8
            cp      a
            ld      (de),a
            ex      af,af'
            or      (hl)
            ld      sp,$8500
            ld      sp,$BF0A
            ld      sp,$B608
            ld      sp,$8900
            ld      sp,$BF0A
            ld      sp,$B608
            ld      sp,$8B00
            ld      sp,$BF0A
            ld      sp,$B608
            ld      sp,$8D00
            ld      sp,$BF0A
            ld      sp,$B608
            ld      sp,$8F00
            ld      sp,$BF0A
            ld      sp,$B608
            ld      sp,$9100
            ld      sp,$BF0A
            ld      sp,$2C08
            ld      l,$00
            dec     b
            dec     h
            ld      b,$40
            ld      (bc),a
            and     b
            ld      b,$40
            ld      (bc),a
            jr      nz,$320F
            ld      a,d
            dec     h
            ld      b,$40
            ld      (bc),a
            and     b
            ld      b,$40
            ld      (bc),a
            jr      nz,$321A
            ld      b,$24
            inc     b
            pop     af
            ld      hl,$0106
            nop
            sub     e
            ld      sp,$BF0A
            ld      sp,$31CA
            jp      z,$D331
            ld      sp,$31DC
            push    hl
            ld      sp,$31EE
            rst     $30
            ld      sp,$3200
            dec     a
            jp      nz,$3255
            ld      a,($D9AD)
            and     $07
            rlca
            ld      c,a
            ld      b,$00
            ld      hl,$3227
            add     hl,bc
            ld      c,(hl)
            inc     hl
            ld      b,(hl)
            ld      de,($D9AF)
            ld      hl,($D9B1)
            jp      $3270
            ld      a,($D9AD)
            ld      c,a
            di
            call    $2EA4
            xor     (hl)
            ld      (hl),a
            call    $2EBD
            xor     (hl)
            ld      (hl),a
            ei
            push    bc
            call    $3099
            pop     bc
            call    $2F67
            ld      bc,$31B2
            push    de
            push    hl
            push    bc
            ld      bc,($D9AD)
            push    bc
            bit     6,c
            jp      nz,$3281
            ld      hl,$D94B
            dec     (hl)
            ld      bc,$00A2
            push    bc
            jp      $205E
            ld      a,($D9AB)
            and     a
            ret     z
            call    $3237
            call    $2FA4
            ld      de,($D9AB)
            ld      hl,($D9AD)
            xor     a
            ld      ($D9AB),a
            inc     a
            ret
            push    ix
            push    iy
            exx
            call    $3288
            pop     iy
            pop     ix
            jr      z,$32B5
            push    hl
            push    de
            ld      hl,$0001
            jr      $32B8
            ld      hl,$0000
            push    hl
            exx
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            and     b
            ld      (_0BRANCH),a
            push    hl
            ld      ($009E),a
            ld      l,h
            ld      bc,_0BRANCH
            call    c,$7632
            nop
            rlca
            dec     d
            ld      (bc),a
            sub     e
            nop
            add     a,l
            ld      sp,$00E9
            dec     d
            daa
            jp      po,$E303
            ld      ($00B1),a
            halt
            nop
            ld      d,b
            inc     c
            daa
            cp      d
            jr      z,$3337
            jr      z,$3349
            nop
            rst     $08
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            rst     $30
            nop
            cp      h
            ld      ($2F18),a
            adc     a,e
            jr      z,$3364
            nop
            sbc     a,a
            ret     nc
            jp      (hl)
            nop
            call    po,$EA01
            inc     bc
            ret     p
            ld      ($0061),a
            push    hl
            ld      de,$0030
            ld      hl,$0100
            ld      a,(SKILLFACTOR)
            and     a
            jp      z,$3318
            ld      hl,_COM
            ld      de,$0036
            ld      (ix+$15),l
            ld      (ix+$16),h
            ld      (ix+$0f),e
            ld      (ix+$10),d
            pop     hl
            ret

            jr      $3328
            ld      d,a
            ld      d,$00
            ld      b,$00
            jr      z,$3351
            ld      b,$78
            ld      a,(bc)
            inc     l
            inc     sp
            inc     b
            ld      a,d
            dec     e
            inc     d
            ld      h,a
            ld      l,$0A
            ld      h,$33
            inc     b
            jp      po,$241E
            inc     b
            inc     sp
            ld      a,(bc)
            ld      h,$33
            rst     $08
            ld      l,l
            nop
            ld      bc,$F0D0
            nop
            jp      pe,$6603
            inc     sp
            ld      l,l
            nop
            inc     (hl)
            inc     sp
            sbc     a,b
            nop
            halt
            nop
            or      d
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      l,e
            exx
            ld      h,$20
            jp      po,$7E03
            inc     sp
            ld      l,l
            nop
            ld      h,a
            exx
            jp      (hl)
            nop
            sbc     a,b
            nop
            halt
            nop
            or      d
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      l,e
            exx
            ld      h,$20
            halt
            nop
            ld      (bc),a
            jp      (hl)
            ld      ($0061),a
            rst     $08
            ld      l,l
            nop
            adc     a,e
            dec     l
            ld      l,l
            nop
            and     a
            call    c,$00F7
            halt
            nop
            dec     c
            adc     a,b
            nop
            ld      l,e
            exx
            jp      (hl)
            nop
            ld      l,l
            nop
            nop
            ld      (bc),a
            dec     bc
            ld      bc,$0076
            inc     de
            adc     a,b
            nop
            ld      l,e
            exx
            jp      (hl)
            nop
            ld      l,l
            nop
            nop
            ld      b,$0B
            ld      bc,$2D93
            halt
            nop
            ld      (bc),a
            halt
            nop
            ld      (bc),a
            halt
            nop
            jr      z,$342B
            nop
            jr      z,$33B2
            dec     l
            halt
            nop
            and     b
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            sub     $2B
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      l,e
            exx
            ret     p
            nop
            call    po,$EA01
            inc     bc
            sub     $33
            ld      l,l
            nop
            sub     a
            ret     nc
            xor     (hl)
            djnz    $3362
            jr      z,$3366
            ld      (bc),a
            halt
            nop
            ld      b,b
            ld      l,h
            ld      bc,_0BRANCH
            push    hl
            inc     sp
            ret
            inc     de
            ld      e,a
            ld      (bc),a
            ld      l,l
            nop
            sub     a
            ret     nc
            or      e
            djnz    $344F
            nop
            ld      hl,$0000
            ld      a,($D94B)
            and     a
            jp      nz,$33FA
            inc     l
            in      a,($10)
            and     $14
            jp      nz,$3402
            inc     l
            push    hl
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            ld      l,l
            nop
            ld      c,l
            exx
            jp      (hl)
            nop
            jp      pe,$EB03
            inc     (hl)
            jp      (hl)
            daa
            ld      (hl),a
            ld      d,$F8
            ld      h,$E9
            daa
            ret     p
            nop
            call    po,$EA01
            inc     bc
            ld      (hl),$34
            cp      h
            ld      ($3380),a
            and     a
            djnz    $3494
            nop
            ld      d,c
            exx
            xor     (hl)
            djnz    $349A
            nop
            ld      c,a
            exx
            xor     (hl)
            djnz    $3415
            inc     bc
            rst     $20
            inc     (hl)
            ld      l,l
            nop
            ld      b,(hl)
            exx
            ret     p
            nop
            jp      pe,$4603
            inc     (hl)
            ld      l,l
            nop
            ld      c,a
            exx
            xor     (hl)
            djnz    $34BD
            nop
            ex      af,af'
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            halt
            nop
            ld      (de),a
            jp      (hl)
            ld      (_LIT),a
            ld      ($F0D0),a
            nop
            jp      pe,$8503
            inc     (hl)
            ld      l,l
            nop
            inc     sp
            ret     nc
            ret     p
            nop
            jp      pe,$8503
            inc     (hl)
            ld      l,l
            nop
            ld      c,a
            exx
            ret     p
            nop
            call    po,$EA01
            inc     bc
            add     a,l
            inc     (hl)
            call    $6D04
            nop
            add     a,b
            inc     b
            sbc     a,b
            nop
            ld      l,l
            nop
            jr      z,$3482
            halt
            nop
            jr      nc,$3446
            add     hl,bc
            ret     nc
            inc     b
            adc     a,l
            ld      (bc),a
            halt
            nop
            inc     b
            ld      l,h
            ld      bc,_0BRANCH
            sub     d
            inc     (hl)
            nop
            inc     d
            ld      e,a
            ld      (bc),a
            ld      l,l
            nop
            ld      c,b
            exx
            xor     (hl)
            djnz    $3508
            nop
            ld      l,c
            exx
            jp      (hl)
            nop
            ld      (hl),d
            inc     d
            ld      l,l
            nop
            ld      c,b
            exx
            or      e
            djnz    $3492
            daa
            ret     p
            nop
            dec     h
            ld      bc,$04CD
            rst     $28
            daa
            ld      l,l
            nop
            ld      c,l
            exx
            inc     bc
            ld      bc,$04D0
            ld      l,l
            nop
            ld      c,e
            exx
            jp      (hl)
            nop
            jp      pe,$DF03
            inc     (hl)
            ld      l,l
            nop
            dec     (hl)
            ret     nc
            ret     p
            nop
            sbc     a,(hl)
            nop
            ld      a,e
            ld      bc,_0BRANCH
            in      a,($34)
            halt
            nop
            ld      b,b
            ld      l,l
            nop
            and     e
            ret     nc
            rst     $30
            nop
            jp      po,$E503
            inc     (hl)
            ld      l,l
            nop
            ld      c,a
            exx
            xor     (hl)
            djnz    $352C
            inc     sp
            jp      po,$FE03
            inc     (hl)
            rst     $28
            inc     sp
            jp      pe,$FE03
            inc     (hl)
            ld      l,l
            nop
            ld      c,a
            exx
            xor     (hl)
            djnz    $348A
            daa
            halt
            nop
            jr      nc,$34E6
            ld      ($0061),a
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            halt
            nop
            ld      b,b
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            xor     b
            jr      nc,$356E
            ld      (bc),a
            ld      h,c
            nop
            exx
            ld      b,$03
            ld      de,$D400
            call    $142C
            ld      a,d
            add     a,$04
            ld      d,a
            djnz    $3518
            exx
            DW      _DSPATCH

;******************************************************************************************
; setrel - set indirect memory pointers
; Description & Context: Set RELABS and FFRELABS to correct jump address for
;                        normal or cocktail mode
;******************************************************************************************
_setrel:    DB      _ENTER
            DW     _LITbyte
            DB      $C3                 ; Z80 opcode for JP
            DW     _0                   ; get address for 0th element of byte array
            DW     _BARRAY
            DW     RELABS		; at the RELABS jump address
            DW     _Bbang 		; and write the JP command
            DW     _LITbyte		; now do the same for the FFRELABS jump
            DB      $C3
            DW     _0
            DW     _BARRAY
            DW     FFRELABS
            DW     _Bbang
            DW     _LIT             ; get the value of COCKTAIL
            DW     COCKTAIL
            DW     _Bat
            DW     _0BRANCH             ; branch if zero to set normal vectors
            DW     setrel0
            DW     _LIT
            DW     cockrel
            DW     _1                   ; get address for 1st element of byte array
            DW     _BARRAY
            DW     RELABS
            DW     _bang                ; write address of COCKREL
            DW     _LIT
            DW     cockff
            DW     _1
            DW     _BARRAY
            DW     FFRELABS             ; do the same for the FFRELABS jump
            DW     _bang                ; this time writing address of COCKFF
            DW     _BRANCH              ; finished cocktail vectors so branch to end
            DW     setrel1
setrel0:    DW     _LIT             ; same as above, but normal vectors
            DW     norrel
            DW     _1
            DW     _BARRAY
            DW     RELABS               ; set to NORREL
            DW     _bang
            DW     _LIT
            DW     ffnorrel
            DW     _1
            DW     _BARRAY
            DW     FFRELABS
            DW     _bang                ; set to FFNORREL
setrel1:    DW     _RETURN

;******************************************************************************************
            rst     $08
            out     ($04),a
            ld      e,l
            inc     d
            inc     h
            dec     (hl)
            sbc     a,b
            nop
            ld      l,l
            nop
            ld      b,b
            out     ($76),a
            nop
            pop     bc
            sbc     a,c
            dec     b
            ld      (de),a
            dec     (hl)
            sbc     a,b
            nop
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            halt
            nop
            ex      af,af'
            sbc     a,c
            dec     b
            ld      l,l
            nop
            scf
            ret     nc
            ret     p
            nop
            ld      l,l
            nop
            adc     a,d
            ret     nc
            cp      $00
            ld      l,l
            nop
            or      b
            inc     b
            ld      l,l
            nop
            xor     c
            ret     nc
            rst     $30
            nop
            ld      l,l
            nop
            adc     a,e
            ret     nc
            or      e
            djnz    $3624
            nop
            and     a
            djnz    $3628
            nop
            ld      l,c
            exx
            rst     $30
            nop
            ld      l,l
            nop
            call    p,$6D05
            nop
            ld      b,d
            exx
            rst     $30
            nop
            ld      h,c
            nop

;#########################################################################################
; { GORFOS - BLOCK 0228 }
; ( STARTGAME INITIALIZER )
; : STARTGAME P1SCR ZEROSCORE P2SCR ZEROSCORE 2800 D086 ! 6400 D088 !
;  MISSIONCTR WPBONE 2 P1FBCTR WPB! PLAYERUP WPBZERO NPLAYERS WPBZERO
;  SHUTUP SKILLFACTOR WPBZERO ;
;#########################################################################################

;******************************************************************************************
; _STARTGAME - ( STARTGAME INITIALIZER )
;
; Description & Context: This high-level TERSE routine completely initializes the game
; state for a new session. It zeroes out the Player 1 and Player 2 BCD scores, caches
; their respective screen rendering coordinate offsets ($2800 and $6400), resets the
; mission counter to 1, provides Player 1 with 2 initial Fire Bases, defaults the
; active player to Player 1, clears the multi-player flag, silences the audio hardware,
; and resets the game's dynamic difficulty (SKILLFACTOR).
;******************************************************************************************
_STARTGAME:
            DB      _ENTER              ; Enter TERSE execution
            DW      _LIT            ;
            DW      P1SCR               ; / Push address of P1SCR ($D00C)
            DW      _ZEROSCORE           ; ZEROSCORE (Clears 3 BCD score bytes)
            DW      _LIT            ;
            DW      P2SCR               ; / Push address of P2SCR ($D01F)
            DW      _ZEROSCORE           ; ZEROSCORE (Clears 3 BCD score bytes)
            DW      _LIT            ;
            DW      $2800               ; / Push literal $2800 (P1 Score Screen Offset)
            DW      _LIT            ;
            DW      $D086               ; / Push address $D086
            DW      _bang               ; ! (Store $2800 into $D086)
            DW      _LIT            ;
            DW      $6400               ; / Push literal $6400 (P2 Score Screen Offset)
            DW      _LIT            ;
            DW      $D088               ; / Push address $D088
            DW      _bang               ; ! (Store $6400 into $D088)
            DW      _LIT            ;
            DW      MISSIONCTR          ; / Push address of MISSIONCTR ($D036)
            DW      _P1                 ; WPBONE (Write Protect 1 - Set Mission to 1)
            DW      _LITbyte            ;
            DB      $02                 ; / Push literal $02 (Initial Fire Bases)
            DW      _LIT            ;
            DW      P1FBCTR             ; / Push address of P1FBCTR ($D032)
            DW      _PWB                ; WPB! (Store 2 into Player 1 Fire Base count)
            DW      _LIT            ;
            DW      PLAYERUP            ; / Push address of PLAYERUP ($D038)
            DW      _P0                 ; WPBZERO (Set active player to Player 1)
            DW      _LIT            ;
            DW      NPLAYERS            ; / Push address of NPLAYERS ($D039)
            DW      _P0                 ; WPBZERO (Reset to 1-Player mode)
            DW      SHUTUP              ; SHUTUP (Silence audio hardware)
            DW      _LIT            ;
            DW      SKILLFACTOR         ; / Push address of SKILLFACTOR ($D037)
            DW      _P0                 ; WPBZERO (Reset Rank/Difficulty)
            DW      _RETURN             ; ; (End of STARTGAME routine)

;******************************************************************************************
            di
            ld      a,$11
            out     ($0E),a
            in      a,($0E)
            ld      e,a
            in      a,($0E)
            cp      e
            jp      z,$361A
            cp      $D0
            jp      c,$361A
            cp      $E0
            jp      nc,$361A
            ld      a,$08
            out     ($0E),a
            DW      _DSPATCH
;******************************************************************************************
            exx
            pop     bc
            ld      hl,$DCCD
            ld      de,($DCD5)
            ld      b,$08
            ld      a,(de)
            and     $F8
            or      c
            ld      (hl),a
            inc     hl
            inc     de
            djnz    $363B
            exx
            DW      _DSPATCH
;******************************************************************************************
            exx
            pop     bc
            ld      hl,$DCCD
            ld      e,$00
            ld      b,$08
            ld      a,(hl)
            and     $07
            cp      c
            jp      z,$3662
            ld      a,c
            and     a
            jp      nz,$3660
            dec     (hl)
            jp      $3661
            inc     (hl)
            inc     e
            inc     hl
            djnz    $3650
            ld      hl,$0000
            ld      a,e
            and     a
            jp      nz,$366E
            inc     hl
            push    hl
            exx
            DW      _DSPATCH
;******************************************************************************************
            exx
            ld      hl,$DCCD
            ld      de,($DCD5)
            ld      bc,$0800
            ld      a,(de)
            cp      (hl)
            jp      z,$368B
            jp      nc,$3689
            dec     (hl)
            jp      $368A
            inc     (hl)
            inc     c
            inc     hl
            inc     de
            djnz    $367D
            ld      hl,$0000
            ld      a,c
            and     a
            jp      nz,$3698
            inc     hl
            push    hl
            exx
            DW      _DSPATCH
;******************************************************************************************
            exx
            pop     hl
            ld      e,$0A
            ld      c,$15
            ld      a,l
            cpl
            and     $01
            or      e
            ld      b,a
            in      a,(c)
            rrc     l
            ld      a,e
            sub     $02
            ld      e,a
            cp      $02
            jr      nz,$36A2
            exx
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            sbc     a,b
            nop
            adc     a,b
            nop
            call    $12DC
            ld      (hl),$7C
            dec     b
            ret     nc
            inc     b
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            rst     $10
            call    c,$00E9
            jp      (hl)
            ld      ($0061),a
            rst     $08
            ld      l,l
            nop
            rst     $10
            call    c,$00F7
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            push    de
            call    c,$00F7
            ld      h,c
            nop
            rst     $08
            jp      c,$7636
            nop
            ex      af,af'
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            ld      l,l
            nop
            push    de
            call    c,$00E9
            dec     bc
            ld      bc,$00F0
            adc     a,l
            ld      (bc),a
            adc     a,b
            nop
            call    $FEDC
            nop
            ld      e,a
            ld      (bc),a
            or      a
            ld      (hl),$61
            nop
            rst     $08
            jp      c,$D136
            ld      (hl),$98
            nop
            adc     a,e
            dec     b
            sbc     a,h
            ld      (hl),$C6
            ld      (hl),$98
            nop
            jr      nc,$374E
            or      a
            ld      (hl),$C6
            ld      (hl),$72
            ld      (hl),$B7
            ld      (hl),$C6
            ld      (hl),$EA
            inc     bc
            inc     e
            scf
            ld      h,c
            nop
            rst     $08
            pop     de
            ld      (hl),$98
            nop
            ld      b,a
            ld      (hl),$B7
            ld      (hl),$C6
            ld      (hl),$EA
            inc     bc
            dec     hl
            scf
            sbc     a,b
            nop
            ld      (de),a
            ld      (hl),$8B
            dec     b
            sbc     a,b
            nop
            sbc     a,h
            ld      (hl),$D0
            inc     b
            ld      h,c
            nop
            ld      b,$C0
            ld      a,(COCKTAIL)
            and     a
            jp      z,$3786
            ld      hl,$3FBF
            ld      de,($DD9A)
            and     a
            sbc     hl,de
            push    hl
            ld      hl,$DCD9
            ld      a,(hl)
            and     a
            ex      (sp),hl
            jp      z,$377A
            ld      c,a
            and     $03
            or      $60
            out     ($0C),a
            ld      a,c
            rrca
            rrca
            and     $3F
            ld      e,a
            ld      d,$00
            push    hl
            sbc     hl,de
            ld      (hl),$FF
            dec     hl
            ld      (hl),$00
            pop     hl
            ld      de,$FFB0
            add     hl,de
            ex      (sp),hl
            inc     hl
            djnz    $375C
            pop     hl
            jp      $37B3
            ld      hl,($DD9A)
            push    hl
            ld      hl,$DCD9
            ld      a,(hl)
            and     a
            ex      (sp),hl
            jp      z,$37AA
            ld      c,a
            and     $03
            or      $20
            out     ($0C),a
            ld      a,c
            ex      de,hl
            rrca
            rrca
            and     $3F
            ld      l,a
            ld      h,$00
            add     hl,de
            ld      (hl),$FF
            inc     hl
            ld      (hl),$00
            ex      de,hl
            ld      de,$0050
            add     hl,de
            ex      (sp),hl
            inc     hl
            djnz    $378D
            pop     hl
            ret
            ret     p
            exx
            pop     bc
            ld      de,$0000
            ld      hl,$6400
            exx
            push    bc
            ld      b,$61
            ld      de,$DD38
            ld      hl,$DD39
            exx
            ex      de,hl
            add     hl,bc
            ex      de,hl
            add     hl,de
            ld      a,h
            exx
            ld      (hl),a
            dec     hl
            ld      (de),a
            inc     de
            djnz    $37C7
            pop     bc
            DW      _DSPATCH
;******************************************************************************************
            push    bc
            ld      a,($DD9C)
            and     a
            jp      nz,$37EF
            ld      a,($D0A3)
            and     a
            jp      nz,$37EF
            di
            inc     a
            ld      ($DD9C),a
            call    $3745
            ei
            pop     bc
            DW      _DSPATCH
;******************************************************************************************
            push    bc
            ld      a,($DD9C)
            and     a
            jp      z,$3803
            di
            xor     a
            ld      ($DD9C),a
            call    $3745
            ei
            pop     bc
            DW      _DSPATCH
;******************************************************************************************
            ld      a,($DD9C)
            and     a
            ret     z
            ld      c,(ix+$14)
            ld      b,$00
            ld      hl,$DCD9
            add     hl,bc
            ld      e,l
            ld      d,h
            ld      b,$03
            ld      a,(hl)
            and     a
            jr      nz,$3821
            inc     hl
            djnz    $3818
            and     a
            ret
            rrca
            rrca
            and     $3F
            ld      b,a
            ld      a,($DD9A)
            add     a,b
            sub     (ix+$0e)
            add     a,$04
            cp      $07
            jp      nc,$3899
            dec     c
            dec     de
            push    de
            ld      l,c
            ld      h,$00
            add     hl,hl
            add     hl,hl
            add     hl,hl
            add     hl,hl
            ld      c,l
            ld      b,h
            add     hl,hl
            add     hl,hl
            add     hl,bc
            ld      bc,($DD9A)
            add     hl,bc
            ld      b,$05
            ex      (sp),hl
            ld      a,(hl)
            and     a
            jp      z,$388C
            ld      (hl),$00
            inc     hl
            ex      (sp),hl
            ld      c,a
            rrca
            rrca
            and     $3F
            ld      e,a
            ld      d,$00
            ex      de,hl
            add     hl,de
            ld      a,(COCKTAIL)
            and     a
            ld      a,c
            jp      nz,$3875
            and     $03
            or      $20
            out     ($0C),a
            ld      (hl),$FF
            inc     hl
            ld      (hl),$00
            ex      de,hl
            jp      $3889
            and     $03
            or      $60
            out     ($0C),a
            push    de
            ex      de,hl
            ld      hl,$3FBF
            and     a
            sbc     hl,de
            ld      (hl),$FF
            dec     hl
            ld      (hl),$00
            pop     hl
            jp      $388E
            inc     hl
            ex      (sp),hl
            ld      de,$0050
            add     hl,de
            djnz    $384A
            pop     hl
            ld      a,$01
            and     a
            ret
            xor     a
            ret

            ld      a,($D9AB)
            and     a
            jr      nz,$38DA
            ld      c,$01
            call    $2114
            jr      z,$38DA
            res     7,(iy+$00)
            set     6,(iy+$00)
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      ($D9B1),hl
            ld      l,(iy+$0d)
            ld      h,(iy+$0e)
            ld      ($D9AF),hl
            ld      a,(iy+$29)
            ld      ($D9AD),a
            bit     6,a
            jp      nz,$38D3
            ld      c,a
            call    $2EBD
            xor     (hl)
            ld      (hl),a
            ld      a,$01
            ld      ($D9AB),a
            and     a
            ret

            xor     a
            ret
;******************************************************************************************
;
;    { BLOCK 0237 }
;    ( POSITION OBJECT RELATIVE TO FORMATION LEADER )
;    SUBR POSREL VFVPL X L LDX, VFVPH X H LDX, H PUSH, Y POPX,
;    VFXBL X L LDX, VFXBH X H LDX, VXL Y E LDX, VXH Y D LDX,
;    D DAD, L VXL X STX, H VXH X STX,
;    VFYBL X L LDX, VFYBH X H LDX, VYL Y E LDX, VYH Y D LDX,
;    D DAD, L VYL X STX, H VYH X STX, RET,
;
;******************************************************************************************

posrel:     ld      l,(ix+$1f)
            ld      h,(ix+$20)
            push    hl
            pop     iy
            ld      l,(ix+$2e)
            ld      h,(ix+$2f)
            ld      e,(iy+$0d)
            ld      d,(iy+$0e)
            add     hl,de
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      l,(ix+$30)
            ld      h,(ix+$31)
            ld      e,(iy+$13)
            ld      d,(iy+$14)
            add     hl,de
            ld      (ix+$13),l
            ld      (ix+$14),h
            ret

;******************************************************************************************
;
;    { BLOCK 0237 - CONTINUED }
;
;    ( INTERRUPT ROUTINE TO WRITE RELATIVE FORMATION MEMBER )
;    SUBR FWRITE TBCALC CALL,
;    PQSDE PQS X BITX, 0=, IF, verase CALL, ELSE, PQSDE PQS X RESX,
;    THEN, aup CALL, POSREL CALL,
;    PQSDW PQS X BITX, 0=, IF, vwrite CALL, ELSE, PQSDW PQS X RESX,
;    PQSDE PQS X SETX, THEN, KILLOFF JMP,
;    DECIMAL -->
;
;******************************************************************************************

fwrite:     call    $1C67
            bit     5,(ix+$00)
            jp      nz,$391C
            call    $14BF
            jp      $3920
            res     5,(ix+$00)
            call    $1C4B
            call    posrel
            bit     6,(ix+$00)
            jp      nz,$3933
            call    $1492
            jp      $393B
            res     6,(ix+$00)
            set     5,(ix+$00)
            jp      $1A6B

;******************************************************************************************
;
;    { BLOCK 0238 }
;    ( LEADER X Y ANIMATION TIME STATUS VECTOR FSTART )
;    CODE FSTART X PUSHX, H POP, Y PUSHX, D POP, EXX,
;    FRAME 2 Y L LDX, 3 Y H LDX, H PUSH, X POPX,
;    CLRVEC CALL, 6 Y C LDX, C VRACK X STX,
;    14 Y L LDX, 15 Y H LDX, L VFVPL X STX, H VFVPH X STX,
;    VXL D LXI, D DAD, M E MOV, H INX,
;    M D MOV, 12 Y L LDX, 13 Y H LDX, L VFXBL X STX, H VFXBH X STX,
;    D DAD, L VXL X STX, H VXH X STX,
;    14 Y L LDX, 15 Y H LDX, VYL D LXI, D DAD, M E MOV, H INX,
;    M E MOV, 10 Y L LDX, 11 Y H LDX, L VFYBL X STX, H VFYBH X STX,
;    D DAD, L VYL X STX, H VYH X STX,
;    SETSTDW CALL, FWRITE H LXI, L PQRL X STX, H PQRH X STX,
;    ASFLOK VAUXS X SETX,
;    STARTVEC CALL, UNFRAME 14 H LXI, SP DAD, SPHL,
;    EXX, D PUSH, Y POPX, H PUSH, X POPX, NEXT
;    DECIMAL -->
;
;******************************************************************************************

_FSTART:    push    ix
            pop     hl
            push    iy
            pop     de
            exx
            push    iy
            ld      iy,$0000
            add     iy,sp
            ld      l,(iy+$02)
            ld      h,(iy+$03)
            push    hl
            pop     ix
            call    $1F3E
            ld      c,(iy+$06)
            ld      (ix+$29),c
            ld      l,(iy+$0e)
            ld      h,(iy+$0f)
            ld      (ix+$1f),l
            ld      (ix+$20),h
            ld      de,$000D
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ld      l,(iy+$0c)
            ld      h,(iy+$0d)
            ld      (ix+$2e),l
            ld      (ix+$2f),h
            add     hl,de
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      l,(iy+$0e)
            ld      h,(iy+$0f)
            ld      de,$0013
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      e,(hl)
            ld      l,(iy+$0a)
            ld      h,(iy+$0b)
            ld      (ix+$30),l
            ld      (ix+$31),h
            add     hl,de
            ld      (ix+$13),l
            ld      (ix+$14),h
            call    $1F74
            ld      hl,fwrite
            ld      (ix+$05),l
            ld      (ix+$06),h
            set     7,(ix+$09)
            call    $1F63
            pop     iy
            ld      hl,$000E
            add     hl,sp
            ld      sp,hl
            exx
            push    de
            pop     iy
            push    hl
            pop     ix
            DW      _DSPATCH

;******************************************************************************************
;
;    { BLOCK 0239 }
;    ( EFFECT REENTRY INTO RACK OR FORMATION )
;    ( HL=TARGET X DE=TARGET Y A=TIME BASE )
;    HEX F= TBCD F= YESLOK F= NOTLOK F= STUFX
;    SUBR PLOTRENT <ASSEMBLE D PUSH, L VYL X STX, H VYH X STX,
;    VDXL X C LDX, VDXH X B LDX, 0 H LXI,
;    LABEL TBCD B DAD, A DCR, TBCD JRNZ,
;    XCHG, VXL X A LDX, 0C0 ANI, A C MOV, VXH X B LDX,
;    A ANA, B DSBC, YESLOK JP, ( IF BELOW TARGET, LOCK TO IT )
;    D DAD, 0<, IF, B H MOV, C L MOV, A ANA, D DSBC,
;    NOTLOK JMPR, THEN, ( IF ABOVE AND CLOSER THAN TBCD, LOCK IN )
;    LABEL YESLOK H POP, A XRA, STUFX JMPR,
;    LABEL NOTLOK D POP, 1 A MVI, A ANA,
;    LABEL STUFX L VXL X STX, H VXH X STX, RET,
;    ASSEMBLE> DECIMAL -->
;
;******************************************************************************************

plotrent:    push    de
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      c,(ix+$0f)
            ld      b,(ix+$10)
            ld      hl,$0000
plotrent_tbcd:
            add     hl,bc
            dec     a
            jr      nz,plotrent_tbcd
            ex      de,hl
            ld      a,(ix+$0d)
            and     $C0
            ld      c,a
            ld      b,(ix+$0e)
            and     a
            sbc     hl,bc
            jp      p,plotrent_yeslok
            add     hl,de
            jp      p,plotrent_yeslok
            ld      h,b
            ld      l,c
            and     a
            sbc     hl,de
            jr      plotrent_notlok
plotrent_yeslok:
            pop     hl
            xor     a
            jr      plotrent_stufx
plotrent_notlok:
            pop     de
            ld      a,$01
            and     a
plotrent_stufx:
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ret

;******************************************************************************************
;
;    { BLOCK 0240 }
;    ( INTERRUPT ROUTINE TO REENTER KAMIKAZE )
;    SUBR REKAMI .REL PQSDE PQS X BITX, 0=, IF, verase CALL, ELSE,
;    PQSDE PQS X RESX, THEN, TBCALC CALL, C A MOV, ( NOTE! )
;    VFVPL X L LDX, VFVPH X H LDX, H PUSH, Y POPX,
;    VFXBL X L LDX, VFXBH X H LDX, VXL Y C LDX, VXH Y B LDX, B DAD,
;    XCHG, VFYBL X L LDX, VFYBH X H LDX, VYL Y C LDX, VYH Y B LDX,
;    B DAD, PLOTRENT CALL,
;    0=, IF, FWRITE H LXI, L PQRL X STX, H PQRH X STX,
;    ASFLOK VAUXS X SETX, PQSFRZ PQS X RESX, THEN,
;    aup CALL,
;    PQSDW PQS X BITX, 0=, IF, vwrite CALL, ELSE, PQSDW PQS X RESX,
;    PQSDE PQS X SETX, THEN, KILLOFF JMP, .ABS
;    DECIMAL -->
;
;******************************************************************************************

rekami:     bit     5,(ix+$00)
            jr      nz,$3A11
            call    $14BF
            jr      $3A15
            res     5,(ix+$00)
            call    $1C67
            ld      a,c
            ld      l,(ix+$1f)
            ld      h,(ix+$20)
            push    hl
            pop     iy
            ld      l,(ix+$2e)
            ld      h,(ix+$2f)
            ld      c,(iy+$0d)
            ld      b,(iy+$0e)
            add     hl,bc
            ex      de,hl
            ld      l,(ix+$30)
            ld      h,(ix+$31)
            ld      c,(iy+$13)
            ld      b,(iy+$14)
            add     hl,bc
            call    plotrent
            jr      nz,$3A53
            ld      hl,fwrite
            ld      (ix+$05),l
            ld      (ix+$06),h
            set     7,(ix+$09)
            res     0,(ix+$00)
            call    $1C4B
            bit     6,(ix+$00)
            jr      nz,$3A61
            call    $1492
            jr      $3A69
            res     6,(ix+$00)
            set     5,(ix+$00)
            jp      $1A6B

;******************************************************************************************
;
;    { BLOCK 0241 }
;    ( ROUTINE TO RETARGET AN ATTACKER )
;    HEX SUBR AABS A ANA, RP, CMA, A INR, RET,
;    ( ACTUAL TARGETER )
;    SUBR KTARGET H PUSH, VYH X A LDX, A SRLR, A SRLR, A C MOV,
;    VYH FBVECTOR LDA, A SRLR, A SRLR, C SUB, A SRAR, A SRAR,
;    A E MOV,
;    VDYH X B LDX, B SUB, A C MOV, E A MOV, B XRA, C A MOV,
;    0<, IF, C ADD, THEN,
;    A VDDYL X STX, 7 A BIT, 0 A MVI,
;    0<>, IF, CMA, THEN, A VDDYH X STX,
;    VDDYL X A LDX, AABS CALL, 0E ANI, 6 CPI, CY~, IF, 6 A MVI,
;    THEN, A C MOV, 0 B MVI, VPTBL X L LDX, VPTBH X H LDX,
;    B DAD, M E MOV, H INX, M D MOV, E VPATL X STX,
;    D VPATH X STX, H POP, RET,
;    DECIMAL -->
;
;******************************************************************************************

aabs:       and     a
            ret     p
            cpl
            inc     a
            ret

ktarget:     push    hl
            ld      a,(ix+$14)
            srl     a
            srl     a
            ld      c,a
            ld      a,($D97F)
            srl     a
            srl     a
            sub     c
            sra     a
            sra     a
            ld      e,a
            ld      b,(ix+$16)
            sub     b
            ld      c,a
            ld      a,e
            xor     b
            ld      a,c
            jp      p,$3A93
            add     a,c
            ld      (ix+$17),a
            bit     7,a
            ld      a,$00
            jp      z,$3A9E
            cpl
            ld      (ix+$18),a
            ld      a,(ix+$17)
            call    aabs
            and     $0E
            cp      $06
            jp      c,$3AB0
            ld      a,$06
            ld      c,a
            ld      b,$00
            ld      l,(ix+$25)
            ld      h,(ix+$26)
            add     hl,bc
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ld      (ix+$1d),e
            ld      (ix+$1e),d
            pop     hl
            ret

            ld      (bc),a
            and     b
            ld      ($0600),hl
            ld      (bc),a
            ld      ($0602),hl
            ld      (bc),a
            ld      ($0604),hl
            ld      (bc),a
            ld      ($0606),hl
            ld      (bc),a
            ld      ($0608),hl
            ld      (bc),a
            ld      (bc),a
            jr      nz,$3B00
            ld      b,$06
            ld      (bc),a
            ld      ($0604),hl
            ld      (bc),a
            ld      ($0602),hl
            ld      (bc),a
            ld      ($0600),hl
            inc     b
            djnz    $3B05
            nop
            ld      c,b
            nop
            ld      b,$24
            inc     c
            nop
            nop
            nop
            nop
            ld      c,$00
            nop
            nop
            nop
            djnz    $3B04
            ld      ($0E1E),hl
            DB      $fd,$ff
            cp      $FF
            inc     c
            ld      b,b
            nop
            add     a,b
            rst     $38
            ld      ($0600),hl
            ex      af,af'
            ld      ($0602),hl
            inc     b
            ld      ($0604),hl
            inc     b
            ld      ($0606),hl
            inc     b
            ld      ($0608),hl
            inc     b
            ld      c,$FD
            rst     $38
            inc     b
            nop
            ld      b,$04
            ld      (bc),a
            and     b
            ld      ($0606),hl
            inc     b
            ld      ($0604),hl
            inc     b
            ld      ($0602),hl
            inc     b
            ld      ($0600),hl
            inc     b
            ld      (bc),a
            jr      nz,$3B42
            ex      af,af'
            ld      c,$00
            nop
            ld      bc,$1000
            ex      af,af'
            xor     $3A
            ld      b,$01
            inc     b
            ld      a,(bc)
            ld      sp,$0106
            jr      nz,$3B4F
            cp      $22
            nop
            ld      b,$04
            ex      af,af'
            push    bc
            ld      a,($7806)
            ld      (de),a
            inc     b
            ld      ($021E),hl
            and     b
            ld      c,$FD
            rst     $38
            ld      (bc),a
            nop
            inc     c
            ld      b,b
            nop
            add     a,b
            nop
            ld      ($0600),hl
            ex      af,af'
            ld      ($0602),hl
            inc     b
            ld      ($0604),hl
            inc     b
            ld      ($0606),hl
            inc     b
            ld      ($0608),hl
            inc     b
            ld      c,$FD
            rst     $38
            call    m,$06FF
            inc     b
            ld      (bc),a
            jr      nz,$3BA9
            ld      b,$06
            inc     b
            ld      ($0604),hl
            inc     b
            ld      ($0602),hl
            inc     b
            ld      ($0600),hl
            inc     b
            ld      (bc),a
            and     b
            ld      b,$08
            ld      c,$00
            nop
            rst     $38
            rst     $38

;    { BLOCK 0245 }
;    ( LEAVE RACK OR FORMATION FOREVER, RELATIVLY SPEAKING )
;    HEX SUBR RANDOMYSX .REL  LDAR, 7F ANI, 20 ADI, A VYH X STX,
;    HARDNESS LDA, 0A CPI, CY~, IF, 0A A MVI, THEN, RLC, RLC,
;    48 ADI, CMA, A VDXL X STX, 0FF VDXH X MVIX, RET,


            DB      $10                 ; trailing byte of the preceding data block

randomysx:   ld      a,r                  ; Block 0245: LDAR ($ED,$5F)
            and     $7F
            add     a,$20
            ld      (ix+$14),a
            ld      a,($D08A)
            cp      $0A
            jr      c,randomysx_scaled
            ld      a,$0A
randomysx_scaled:
            rlca
            rlca
            add     a,$48
            cpl
            ld      (ix+$0f),a
            ld      (ix+$10),$FF
            ret

;    SUBR RRK M E MOV, H INX, M D MOV, H INX, WOAFLAG LDA, A ANA,
;    0<>, IF, XCHG, THEN, RET,


rrk:         ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      a,($D948)
            and     a
            jr      z,rrk_done
            ex      de,hl
rrk_done:
            ret

;    SUBR FREEFLOW M E MOV, H INX, M D MOV, H INX, HARDNESS LDA,
;    RLC, RLC, 18 CPI, CY~, IF, 18 A MVI, THEN,
;    A C MOV,
;    INVADERSLEFT LDA, C CMP, CY, IF, XCHG, THEN, RET,

freeflow:    ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      a,($D08A)
            rlca
            rlca
            cp      $18
            jr      c,freeflow_cap_done
            ld      a,$18
freeflow_cap_done:
            ld      c,a
            ld      a,($D94B)
            cp      c
            jr      nc,freeflow_done
            ex      de,hl
freeflow_done:
            ret

;    .ABS
;    DATA RETAT ASM GOTOP ACALL RANDOMYSX ASMCALL 1 SWAIT 0FE 0 SETS
;    ARET
;    SUBR SKILDDY HARDNESS LDA, A ANA, RZ, -1 A MVI,
;    A VDDXL X STX, A VDDXH X STX, RET, DECIMAL -->

RETAT:      DB      $08,$EE,$3A,$24,$A0,$3B,$06,$01,$20,$00,$FE,$10
            ; Source: ASM GOTOP ACALL RANDOMYSX ASMCALL 1 SWAIT $FE 0 SETS ARET

skilddy:     ld      a,($D08A)
            and     a
            ret     z
            ld      a,$FF
            ld      (ix+$11),a
            ld      (ix+$12),a
            ret



            inc     e
            djnz    $3C24
            ld      (hl),c
            ld      a,($1406)
            inc     h
            rst     $28
            dec     sp
            ld      e,$FF
            dec     sp
            ld      b,$78
            djnz    $3C16
            xor     $3A
            ld      b,$01
            jr      nz,$3C14
            cp      $04
            ld      b,$3A
            inc     c
            add     a,b
            nop
            nop
            nop
            ld      b,$0A
            ex      af,af'
            push    bc
            ld      a,($0022)


            ld      b,$78
            ld      a,(bc)
            ld      ($243C),hl
            cp      a
            dec     sp
            dec     c
            inc     a
            ex      af,af'
            ex      (sp),hl
            dec     sp
            ex      af,af'
            DB      $fd,$3b
            ld      a,(bc)
            add     hl,hl
            inc     a
            ex      af,af'
            DB      $fd,$3b
            inc     h
            srl     e
            add     hl,hl
            inc     a
            ld      a,(bc)
            dec     c
            inc     a
            ex      af,af'
            rst     $38
            ld      a,($370A)
            inc     a
            ex      af,af'
            ld      e,d
            dec     sp
            ld      a,(bc)
            scf
            inc     a
            ld      b,(hl)
            inc     hl
            ld      b,(hl)
            inc     hl
            ld      b,(hl)
            inc     hl
            ld      b,(hl)
            inc     hl
            ld      b,(hl)
            inc     hl
            ld      h,$4E
            inc     a
            ld      ($0600),hl
            ld      a,b
            ld      a,(bc)
            ld      e,l
            inc     a

;******************************************************************************************
;
;    { BLOCK 0248 }
;    ( SCROLL SIDEWAYS COMMAND - USES THE PATTERN BOARD )
;    HEX F= SWLL F= OSL F= CSL
;    CODE SCROLL <ASSEMBLE D POP, B PUSH, Y PUSHX, X PUSHX,
;    E C MOV, D B MOV, LABEL OSL B PUSH, 50 D LXI,
;    0C0 B MVI, 404F H LXI, LABEL CSL M A MOV, 0F0 ANI, A M MOV,
;    D DAD, CSL DJNZ,
;    4000 D LXI, 2 A MVI, MAGIC OUT,
;    LABEL SWLL DI, 24 A MVI, PBSTAT OUT,
;    E A MOV, PBLINADRL OUT, D A MOV, PBLINADRH OUT,
;    E A MOV, PBAREADRL OUT, D A MOV, 3F ANI, PBAREADRH OUT,
;    1 A MVI, PBXMOD OUT,
;    7F A MVI, PBXWIDE OUT, 7 A MVI, PBYHIGH OUT, EI,
;    D A MOV, 4 ADI, A D MOV, SWLL JP, creditcheck CALL,
;    busaround CALL,
;    B POP, B DCX, B A MOV, C ORA, OSL JRNZ, X POPX, Y POPX, B POP,
;    NEXT ASSEMBLE> DECIMAL -->
;
;******************************************************************************************

_SCROLL:    pop     de
            push    bc
            push    iy
            push    ix
            ld      c,e
            ld      b,d
scroll_osl: push    bc                  ; LABEL OSL
            ld      de,$0050
            ld      b,$C0
            ld      hl,$404F
scroll_csl: ld      a,(hl)              ; LABEL CSL
            and     $F0
            ld      (hl),a
            add     hl,de
            djnz    scroll_csl
            ld      de,$4000
            ld      a,$02
            out     ($0C),a
scroll_swll: di                         ; LABEL SWLL
            ld      a,$24
            out     (PBSTAT),a
            ld      a,e
            out     ($78),a
            ld      a,d
            out     ($79),a
            ld      a,e
            out     ($7B),a
            ld      a,d
            and     $3F
            out     ($7C),a
            ld      a,$01
            out     ($7B),a
            ld      a,$7F
            out     ($7D),a
            ld      a,$07
            out     ($7E),a
            ei
            ld      a,d
            add     a,$04
            ld      d,a
            jp      p,scroll_swll
            call    creditcheck
            call    busaround
            pop     bc
            dec     bc
            ld      a,b
            or      c
            jr      nz,scroll_osl
            pop     ix
            pop     iy
            pop     bc
            DW      _DSPATCH

;******************************************************************************************
;
;    { BLOCK 0249 }
;    ( ROUTINE TO LIGHT UP PLAYERS RANK, RANK # ON STACK )
;    HEX
;    CODE LITERANK .REL EXX, 16 B LXI, BEGIN, A INP, B A MOV, 2 ADI,
;    A B MOV, 0C CPI, 0=, END,
;    D POP, E A MOV, 6 CPI, CY~, IF, 5 A MVI, THEN, STC, RAL,
;    A B MOV, A INP, EXX, .ABS NEXT
;    DECIMAL -->
;
;******************************************************************************************
_LITERANK:  exx
            ld      bc,$0016
            in      a,(c)
            ld      a,b
            add     a,$02
            ld      b,a
            cp      $0C
            jr      nz,$3CBF
            pop     de
            ld      a,e
            cp      $06
            jr      c,$3CD1
            ld      a,$05
            scf
            rla
            ld      b,a
            in      a,(c)
            exx
            DW      _DSPATCH

;******************************************************************************************
;
;    { BLOCK 0250 }
;    ( ASCII MESSAGE TABLE )
;    HEX CC? IFTRUE : ," 22 DELIM ! WORD HERE B@ 1+ DP+! ; IMMED
;    OTHERWISE { : ," } { 22 DELIM ! WORD HERE DUP COUNT + 1- DO
;    I B@ -1 +LOOP HERE B@ } p { 1+ 0 DO } B, { LOOP } p { ; IMMED }
;    IFEND DATA ASCMSGT ," MISSION[" ," GAME OVER"
;    ," PLAYER"
;    ," 2"
;    ," 1"
;    ," GAME"
;    ," OVER"
;    ," GET"
;    ," READY"
;    ," INSERT ADDITIONAL COIN"
;    ," SELECT 1 PLAYER GAME"
;    ," OR"
;    ," SELECT 1 OR 2 PLAYER GAME" -->
;
;    { BLOCK 0251 }
;    ( CONTINUED ASCII MESSAGE TABLE )
;    ," FOR 2 PLAYER GAME"
;    ," OR FOR EXTRA SHIPS"
;    ," WITH EXTRA SHIPS"
;    ," THE EVIL"
;    ," GORFIAN ROBOT"
;    ," EMPIRE HAS ATTACKED"
;    ," YOUR ASSIGNMENT IS TO"
;    ," REPEL THE INVASION AND"
;    ," LAUNCH A COUNTERATTACK"
;    ," YOU WILL"
;    ," ENGAGE VARIOUS"
;    ," HOSTILE SPACECRAFT"
;    ," AS YOU JOURNEY TOWARD"
;    ," A DRAMATIC CONFRONTATION"
;    -->
;
;    { BLOCK 0252 }
;    ( EVEN MORE ASCII MESSAGES )
;    ," WITH THE ENEMY FLAG SHIP"
;    ," THE HIGH"
;    ," SCORES ARE["
;    ," 2 SHIPS"
;    ," 3 SHIPS"
;    ," 4 SHIPS"
;    ," 6 SHIPS"
;    ," ASTRO BATTLES"
;    ," GALAXIANS"
;    ," LASER ATTACK"
;    ," SPACE WARP"
;    ," FLAG SHIP"
;    -->
;
;
;
;    { BLOCK 0253 }
;    ( MORE ASCII MESSAGES )
;    ," 1 PLAYER"
;    ," 2 PLAYERS"
;    ," INSERT COIN"
;    ,"  "
;    ,"  "
;    1 B, 9 B,
;    5 B, 0A B, 0B B, 9 B, 0D B, 0E B,
;    5 B, 0C B, 0B B, 9 B, 0D B, 0F B,
;    1 B, 0C B,
;    ," SAME PLAYER UP"
;    ," CREDIT SHIPS["
;    ," \1981 MIDWAY MFG CO"
;    ," ALL RIGHTS RESERVED"
;    ASCMSGT MSGLINK U!
;    DECIMAL -->
;
;******************************************************************************************
            ex      af,af'
            ld      c,l
            ld      c,c
            ld      d,e
            ld      d,e
            ld      c,c
            ld      c,a
            ld      c,(hl)
            ld      e,e
            add     hl,bc
            ld      b,a
            ld      b,c
            ld      c,l
            ld      b,l
            jr      nz,$3D38
            ld      d,(hl)
            ld      b,l
            ld      d,d
            ld      b,$50
            ld      c,h
            ld      b,c
            ld      e,c
            ld      b,l
            ld      d,d
            ld      bc,$0132
            ld      sp,$4704
            ld      b,c
            ld      c,l
            ld      b,l
            inc     b
            ld      c,a
            ld      d,(hl)
            ld      b,l
            ld      d,d
            inc     bc
            ld      b,a
            ld      b,l
            ld      d,h
            dec     b
            ld      d,d
            ld      b,l
            ld      b,c
            ld      b,h
            ld      e,c
            ld      d,$49
            ld      c,(hl)
            ld      d,e
            ld      b,l
            ld      d,d
            ld      d,h
            jr      nz,$3D55
            ld      b,h
            ld      b,h
            ld      c,c
            ld      d,h
            ld      c,c
            ld      c,a
            ld      c,(hl)
            ld      b,c
            ld      c,h
            jr      nz,$3D62
            ld      c,a
            ld      c,c
            ld      c,(hl)
            inc     d
            ld      d,e
            ld      b,l
            ld      c,h
            ld      b,l
            ld      b,e
            ld      d,h
            jr      nz,$3D5C
            jr      nz,$3D7D
            ld      c,h
            ld      b,c
            ld      e,c
            ld      b,l
            ld      d,d
            jr      nz,$3D7B
            ld      b,c
            ld      c,l
            ld      b,l
            ld      (bc),a
            ld      c,a
            ld      d,d
            add     hl,de
            ld      d,e
            ld      b,l
            ld      c,h
            ld      b,l
            ld      b,e
            ld      d,h
            jr      nz,$3D74
            jr      nz,$3D94
            ld      d,d
            jr      nz,$3D7A
            jr      nz,$3D9A
            ld      c,h
            ld      b,c
            ld      e,c
            ld      b,l
            ld      d,d
            jr      nz,$3D98
            ld      b,c
            ld      c,l
            ld      b,l
            ld      de,$4F46
            ld      d,d
            jr      nz,$3D8C
            jr      nz,$3DAC
            ld      c,h
            ld      b,c
            ld      e,c
            ld      b,l
            ld      d,d
            jr      nz,$3DAA
            ld      b,c
            ld      c,l
            ld      b,l
            ld      (de),a
            ld      c,a
            ld      d,d
            jr      nz,$3DB1
            ld      c,a
            ld      d,d
            jr      nz,$3DB4
            ld      e,b
            ld      d,h
            ld      d,d
            ld      b,c
            jr      nz,$3DC8
            ld      c,b
            ld      c,c
            ld      d,b
            ld      d,e
            djnz    $3DD2
            ld      c,c
            ld      d,h
            ld      c,b
            jr      nz,$3DC5
            ld      e,b
            ld      d,h
            ld      d,d
            ld      b,c
            jr      nz,$3DD9
            ld      c,b
            ld      c,c
            ld      d,b
            ld      d,e
            ex      af,af'
            ld      d,h
            ld      c,b
            ld      b,l
            jr      nz,$3DD5
            ld      d,(hl)
            ld      c,c
            ld      c,h
            dec     c
            ld      b,a
            ld      c,a
            ld      d,d
            ld      b,(hl)
            ld      c,c
            ld      b,c
            ld      c,(hl)
            jr      nz,$3DEF
            ld      c,a
            ld      b,d
            ld      c,a
            ld      d,h
            inc     de
            ld      b,l
            ld      c,l
            ld      d,b
            ld      c,c
            ld      d,d
            ld      b,l
            jr      nz,$3DF2
            ld      b,c
            ld      d,e
            jr      nz,$3DEF
            ld      d,h
            ld      d,h
            ld      b,c
            ld      b,e
            ld      c,e
            ld      b,l
            ld      b,h
            dec     d
            ld      e,c
            ld      c,a
            ld      d,l
            ld      d,d
            jr      nz,$3DFD
            ld      d,e
            ld      d,e
            ld      c,c
            ld      b,a
            ld      c,(hl)
            ld      c,l
            ld      b,l
            ld      c,(hl)
            ld      d,h
            jr      nz,$3E10
            ld      d,e
            jr      nz,$3E1E
            ld      c,a
            ld      d,$52
            ld      b,l
            ld      d,b
            ld      b,l
            ld      c,h
            jr      nz,$3E27
            ld      c,b
            ld      b,l
            jr      nz,$3E20
            ld      c,(hl)
            ld      d,(hl)
            ld      b,c
            ld      d,e
            ld      c,c
            ld      c,a
            ld      c,(hl)
            jr      nz,$3E21
            ld      c,(hl)
            ld      b,h
            ld      d,$4C
            ld      b,c
            ld      d,l
            ld      c,(hl)
            ld      b,e
            ld      c,b
            jr      nz,$3E2C
            jr      nz,$3E30
            ld      c,a
            ld      d,l
            ld      c,(hl)
            ld      d,h
            ld      b,l
            ld      d,d
            ld      b,c
            ld      d,h
            ld      d,h
            ld      b,c
            ld      b,e
            ld      c,e
            ex      af,af'
            ld      e,c
            ld      c,a
            ld      d,l
            jr      nz,$3E56
            ld      c,c
            ld      c,h
            ld      c,h
            ld      c,$45
            ld      c,(hl)
            ld      b,a
            ld      b,c
            ld      b,a
            ld      b,l
            jr      nz,$3E61
            ld      b,c
            ld      d,d
            ld      c,c
            ld      c,a
            ld      d,l
            ld      d,e
            ld      (de),a
            ld      c,b
            ld      c,a
            ld      d,e
            ld      d,h
            ld      c,c
            ld      c,h
            ld      b,l
            jr      nz,$3E6E
            ld      d,b
            ld      b,c
            ld      b,e
            ld      b,l
            ld      b,e
            ld      d,d
            ld      b,c
            ld      b,(hl)
            ld      d,h
            dec     d
            ld      b,c
            ld      d,e
            jr      nz,$3E82
            ld      c,a
            ld      d,l
            jr      nz,$3E77
            ld      c,a
            ld      d,l
            ld      d,d
            ld      c,(hl)
            ld      b,l
            ld      e,c
            jr      nz,$3E89
            ld      c,a
            ld      d,a
            ld      b,c
            ld      d,d
            ld      b,h
            jr      $3E7D
            jr      nz,$3E82
            ld      d,d
            ld      b,c
            ld      c,l
            ld      b,c
            ld      d,h
            ld      c,c
            ld      b,e
            jr      nz,$3E8A
            ld      c,a
            ld      c,(hl)
            ld      b,(hl)
            ld      d,d
            ld      c,a
            ld      c,(hl)
            ld      d,h
            ld      b,c
            ld      d,h
            ld      c,c
            ld      c,a
            ld      c,(hl)
            jr      $3EAC
            ld      c,c
            ld      d,h
            ld      c,b
            jr      nz,$3EAE
            ld      c,b
            ld      b,l
            jr      nz,$3EA3
            ld      c,(hl)
            ld      b,l
            ld      c,l
            ld      e,c
            jr      nz,$3EAA
            ld      c,h
            ld      b,c
            ld      b,a
            jr      nz,$3EBC
            ld      c,b
            ld      c,c
            ld      d,b
            ex      af,af'
            ld      d,h
            ld      c,b
            ld      b,l
            jr      nz,$3EBA
            ld      c,c
            ld      b,a
            ld      c,b
            dec     bc
            ld      d,e
            ld      b,e
            ld      c,a
            ld      d,d
            ld      b,l
            ld      d,e
            jr      nz,$3EBF
            ld      d,d
            ld      b,l
            ld      e,e
            rlca
            ld      ($5320),a
            ld      c,b
            ld      c,c
            ld      d,b
            ld      d,e
            rlca
            inc     sp
            jr      nz,$3EE0
            ld      c,b
            ld      c,c
            ld      d,b
            ld      d,e
            rlca
            inc     (hl)
            jr      nz,$3EE8
            ld      c,b
            ld      c,c
            ld      d,b
            ld      d,e
            rlca
            ld      (hl),$20
            ld      d,e
            ld      c,b
            ld      c,c
            ld      d,b
            ld      d,e
            dec     c
            ld      b,c
            ld      d,e
            ld      d,h
            ld      d,d
            ld      c,a
            jr      nz,$3EEB
            ld      b,c
            ld      d,h
            ld      d,h
            ld      c,h
            ld      b,l
            ld      d,e
            add     hl,bc
            ld      b,a
            ld      b,c
            ld      c,h
            ld      b,c
            ld      e,b
            ld      c,c
            ld      b,c
            ld      c,(hl)
            ld      d,e
            inc     c
            ld      c,h
            ld      b,c
            ld      d,e
            ld      b,l
            ld      d,d
            jr      nz,$3F02
            ld      d,h
            ld      d,h
            ld      b,c
            ld      b,e
            ld      c,e
            ld      a,(bc)
            ld      d,e
            ld      d,b
            ld      b,c
            ld      b,e
            ld      b,l
            jr      nz,$3F25
            ld      b,c
            ld      d,d
            ld      d,b
            add     hl,bc
            ld      b,(hl)
            ld      c,h
            ld      b,c
            ld      b,a
            jr      nz,$3F2B
            ld      c,b
            ld      c,c
            ld      d,b
            ex      af,af'
            ld      sp,$5020
            ld      c,h
            ld      b,c
            ld      e,c
            ld      b,l
            ld      d,d
            add     hl,bc
            ld      ($5020),a
            ld      c,h
            ld      b,c
            ld      e,c
            ld      b,l
            ld      d,d
            ld      d,e
            dec     bc
            ld      c,c
            ld      c,(hl)
            ld      d,e
            ld      b,l
            ld      d,d
            ld      d,h
            jr      nz,$3F3A
            ld      c,a
            ld      c,c
            ld      c,(hl)
            ld      bc,_COM
            jr      nz,$3F00
            add     hl,bc
            dec     b
            ld      a,(bc)
            dec     bc
            add     hl,bc
            dec     c
            ld      c,$05
            inc     c
            dec     bc
            add     hl,bc
            dec     c
            rrca
            ld      bc,$0E0C
            ld      d,e
            ld      b,c
            ld      c,l
            ld      b,l
            jr      nz,$3F65
            ld      c,h
            ld      b,c
            ld      e,c
            ld      b,l
            ld      d,d
            jr      nz,$3F71
            ld      d,b
            dec     c
            ld      b,e
            ld      d,d
            ld      b,l
            ld      b,h
            ld      c,c
            ld      d,h
            jr      nz,$3F79
            ld      c,b
            ld      c,c
            ld      d,b
            ld      d,e
            ld      e,e
            inc     de
            ld      e,h
            ld      sp,$3839
            ld      sp,$4D20
            ld      c,c
            ld      b,h
            ld      d,a
            ld      b,c
            ld      e,c
            jr      nz,$3F87
            ld      b,(hl)
            ld      b,a
            jr      nz,$3F81
            ld      c,a
            inc     de
            ld      b,c
            ld      c,h
            ld      c,h
            jr      nz,$3F97
            ld      c,c
            ld      b,a
            ld      c,b
            ld      d,h
            ld      d,e
            jr      nz,$3F9E
            ld      b,l
            ld      d,e
            ld      b,l
            ld      d,d
            ld      d,(hl)
            ld      b,l
            ld      b,h

;*****************************************************************************
;    { BLOCK 0254 }
;    ( *** THIS SCREEN REBUILT FROM 10\7\80 PRINTOUT ) HEX
;    CODE showport EXX, B POP, 4800 H LXI, D POP, X PUSHX, Y PUSHX,
;    C INP, 8 B MVI, BEGIN, C RALR, B PUSH, CY, IF,
;    108 B LXI, ELSE, 8 B LXI, THEN, 20 A MVI, drawchar CALL,
;    B POP, LOOP, Y POPX, X POPX, EXX, NEXT
;*****************************************************************************

showport:   exx
            pop     bc
            ld      hl,$4800
            pop     de
            push    ix
            push    iy
            in      c,(c)
            ld      b,$08
            rl      c
            push    bc
            jp      nc,$3F6D
            ld      bc,$0108
            jp      $3F70
            ld      bc,$0008
            ld      a,$20
            call    drawchar
            pop     bc
            djnz    $3F61
            pop     iy
            pop     ix
            exx
            DW      _DSPATCH

;    { Block 254 Continued }
;    : SHOW4P BARK 4000 10 showport
;    3800 11 showport 3000 12 showport 2800 13 showport ;

; SHOW4P compiles BARK as the dictionary entry at $1A66, now labeled at
; its actual implementation rather than kept as a numeric/equate-only cell.

_SHOW4P:    DB      _ENTER
            DW      _BARK
            DW      _LIT
            DW      $4000
            DW      _LITbyte
            DB      $10
            DW      showport
            DW      _LIT
            DW      $3800
            DW      _LITbyte
            DB      $11
            DW      showport
            DW      _LIT
            DW      $3000
            DW      _LITbyte
            DB      $12
            DW      showport
            DW      _LIT
            DW      $2800
            DW      _LITbyte
            DB      $13
            DW      showport
            DW      _RETURN

;    { Block 254 Continued }
;    CODE sumup EXX, D POP, E A MOV, RLC, RLC, RLC, RLC, A H MOV,
;    0 L MVI, 1000 D LXI, 0 B LXI, BEGIN, M A MOV, C ADD, A C MOV,
;    H INX, D DCX, D A MOV, E ORA, 0=, END, C INR, B PUSH, EXX, NEXT

sumup:      exx
            pop     de
            ld      a,e
            rlca
            rlca
            rlca
            rlca
            ld      h,a
            ld      l,$00
            ld      de,$1000
            ld      bc,$0000
sumup_loop: ld      a,(hl)
            add     a,c
            ld      c,a
            inc     hl
            dec     de
            ld      a,d
            or      e
            jp      nz,sumup_loop
            inc     c
            push    bc
            exx
            DW      _DSPATCH


;    { Block 254 Continued }
;    BTABLE ROMCHARS 41 B, 42 B, 43 B, 44 B, 0 B, 0 B, 0 B, 0 B,
;    45 B, 46 B, 47 B, 48 B, 58 B,
;    : CKCHIP DUP sumup IF ROMCHARS B@ 428 SWAP cpost THEN DROP
;    SHOW4P ;
;    -->

ROMCHARS:   DB      $41,$42,$43,$44,$00,$00,$00,$00
            DB      $45,$46,$47,$48,$58

;    { Block 254 Continued }
;    : CKCHIP DUP sumup IF ROMCHARS B@ 428 SWAP cpost THEN DROP
;    SHOW4P ;
;    -->

_CKCHIP:    DB      _ENTER
            DW      _DUP
            DW      sumup
            DW      _0BRANCH
            DW      ckchip_then
            DW      _BARRAY
            DW      ROMCHARS
            DW      _Bat
            DW      _LIT
            DW      $0428
            DW      _SWAP
            DW      _CPOST
ckchip_then:
            DW      _DROP
            DW      _SHOW4P
            DW      _RETURN
;*****************************************************************************
; Free Parking... (Filler bytes)
;*****************************************************************************

            DB      $FF, $FF,$FF,$FF,$FF
            DB      $FF, $FF,$FF,$FF,$FF
            DB      $FF, $FF,$FF,$FF,$FF
            DB      $FF, $FF,$FF,$FF,$FF




;*****************************************************************************
; Begin Next ROM set
;*****************************************************************************

            ORG     $8000

L8000:      and     l
            ld      h,e
            adc     a,d
            add     a,(hl)
            adc     a,d
;*******************************************************************************
; ASTRO_BATTLES_ALIEN_A_1
; 2 bytes/row = 8 pixels wide, 8 rows
;*******************************************************************************
ASTRO_BATTLES_ALIEN_A_1:
        DB      $02,$08                        ; width bytes, height rows
        DB      $11,$40                        ; . 1 . 1 1 . . .
        DB      $45,$50                        ; 1 . 1 1 1 1 . .
        DB      $01,$94                        ; . . . 1 2 1 1 .
        DB      $05,$55                        ; . . 1 1 1 1 1 1
        DB      $05,$55                        ; . . 1 1 1 1 1 1
        DB      $01,$94                        ; . . . 1 2 1 1 .
        DB      $45,$50                        ; 1 . 1 1 1 1 . .
        DB      $11,$40                        ; . 1 . 1 1 . . .

;*******************************************************************************
; ASTRO_BATTLES_ALIEN_A_2
; 2 bytes/row = 8 pixels wide, 8 rows
;*******************************************************************************
ASTRO_BATTLES_ALIEN_A_2:
        DB      $02,$08                        ; width bytes, height rows
        DB      $41,$40                        ; 1 . . 1 1 . . .
        DB      $11,$50                        ; . 1 . 1 1 1 . .
        DB      $45,$94                        ; 1 . 1 1 2 1 1 .
        DB      $11,$55                        ; . 1 . 1 1 1 1 1
        DB      $11,$55                        ; . 1 . 1 1 1 1 1
        DB      $45,$94                        ; 1 . 1 1 2 1 1 .
        DB      $11,$50                        ; . 1 . 1 1 1 . .
        DB      $41,$40                        ; 1 . . 1 1 . . .

;*******************************************************************************
; ASTRO_BATTLES_ALIEN_B_1
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
ASTRO_BATTLES_ALIEN_B_1:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $02,$A8                        ; . . . 2 2 2 2 .
        DB      $8A,$80                        ; 2 . 2 2 2 . . .
        DB      $2A,$A2                        ; . 2 2 2 2 2 . 2
        DB      $0A,$68                        ; . . 2 2 1 2 2 .
        DB      $0A,$A0                        ; . . 2 2 2 2 . .
        DB      $0A,$A0                        ; . . 2 2 2 2 . .
        DB      $0A,$A0                        ; . . 2 2 2 2 . .
        DB      $0A,$68                        ; . . 2 2 1 2 2 .
        DB      $2A,$A2                        ; . 2 2 2 2 2 . 2
        DB      $8A,$80                        ; 2 . 2 2 2 . . .
        DB      $02,$A8                        ; . . . 2 2 2 2 .

;*******************************************************************************
; ASTRO_BATTLES_ALIEN_B_2
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
ASTRO_BATTLES_ALIEN_B_2:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $2A,$00                        ; . 2 2 2 . . . .
        DB      $02,$80                        ; . . . 2 2 . . .
        DB      $2A,$A2                        ; . 2 2 2 2 2 . 2
        DB      $8A,$68                        ; 2 . 2 2 1 2 2 .
        DB      $8A,$A0                        ; 2 . 2 2 2 2 . .
        DB      $0A,$A0                        ; . . 2 2 2 2 . .
        DB      $8A,$A0                        ; 2 . 2 2 2 2 . .
        DB      $8A,$68                        ; 2 . 2 2 1 2 2 .
        DB      $2A,$A2                        ; . 2 2 2 2 2 . 2
        DB      $02,$80                        ; . . . 2 2 . . .
        DB      $2A,$00                        ; . 2 2 2 . . . .

;*******************************************************************************
; ASTRO_BATTLES_ALIEN_C_1
; 2 bytes/row = 8 pixels wide, 12 rows
;*******************************************************************************
ASTRO_BATTLES_ALIEN_C_1:
        DB      $02,$0C                        ; width bytes, height rows
        DB      $C3,$F0                        ; 3 . . 3 3 3 . .
        DB      $C3,$FC                        ; 3 . . 3 3 3 3 .
        DB      $33,$FC                        ; . 3 . 3 3 3 3 .
        DB      $3F,$3C                        ; . 3 3 3 . 3 3 .
        DB      $0F,$3F                        ; . . 3 3 . 3 3 3
        DB      $33,$FF                        ; . 3 . 3 3 3 3 3
        DB      $33,$FF                        ; . 3 . 3 3 3 3 3
        DB      $0F,$3F                        ; . . 3 3 . 3 3 3
        DB      $3F,$3C                        ; . 3 3 3 . 3 3 .
        DB      $33,$FC                        ; . 3 . 3 3 3 3 .
        DB      $C3,$FC                        ; 3 . . 3 3 3 3 .
        DB      $C3,$F0                        ; 3 . . 3 3 3 . .

;*******************************************************************************
; ASTRO_BATTLES_ALIEN_C_2
; 2 bytes/row = 8 pixels wide, 12 rows
;*******************************************************************************
ASTRO_BATTLES_ALIEN_C_2:
        DB      $02,$0C                        ; width bytes, height rows
        DB      $03,$F0                        ; . . . 3 3 3 . .
        DB      $33,$FC                        ; . 3 . 3 3 3 3 .
        DB      $FF,$FC                        ; 3 3 3 3 3 3 3 .
        DB      $CF,$3C                        ; 3 . 3 3 . 3 3 .
        DB      $0F,$3F                        ; . . 3 3 . 3 3 3
        DB      $33,$FF                        ; . 3 . 3 3 3 3 3
        DB      $33,$FF                        ; . 3 . 3 3 3 3 3
        DB      $0F,$3F                        ; . . 3 3 . 3 3 3
        DB      $CF,$3C                        ; 3 . 3 3 . 3 3 .
        DB      $FF,$FC                        ; 3 3 3 3 3 3 3 .
        DB      $33,$FC                        ; . 3 . 3 3 3 3 .
        DB      $03,$F0                        ; . . . 3 3 3 . .

;*******************************************************************************
; ASTRO_BATTLES_TALL_CRAFT
; 2 bytes/row = 8 pixels wide, 18 rows
;*******************************************************************************
ASTRO_BATTLES_TALL_CRAFT:
        DB      $02,$12                        ; width bytes, height rows
        DB      $00,$00                        ; . . . . . . . .
        DB      $0C,$00                        ; . . 3 . . . . .
        DB      $0F,$00                        ; . . 3 3 . . . .
        DB      $1F,$C0                        ; . 1 3 3 3 . . .
        DB      $5D,$F0                        ; 1 1 3 1 3 3 . .
        DB      $1F,$F0                        ; . 1 3 3 3 3 . .
        DB      $0F,$FC                        ; . . 3 3 3 3 3 .
        DB      $0D,$FC                        ; . . 3 1 3 3 3 .
        DB      $3F,$FC                        ; . 3 3 3 3 3 3 .
        DB      $3F,$FC                        ; . 3 3 3 3 3 3 .
        DB      $0D,$FC                        ; . . 3 1 3 3 3 .
        DB      $0F,$FC                        ; . . 3 3 3 3 3 .
        DB      $1F,$F0                        ; . 1 3 3 3 3 . .
        DB      $5D,$F0                        ; 1 1 3 1 3 3 . .
        DB      $1F,$C0                        ; . 1 3 3 3 . . .
        DB      $0F,$00                        ; . . 3 3 . . . .
        DB      $0C,$00                        ; . . 3 . . . . .
        DB      $00,$00                        ; . . . . . . . .

;*******************************************************************************
; ASTRO_BATTLES_COMPACT_CRAFT
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
ASTRO_BATTLES_COMPACT_CRAFT:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $0F,$00                        ; . . 3 3 . . . .
        DB      $CF,$00                        ; 3 . 3 3 . . . .
        DB      $3F,$C0                        ; . 3 3 3 3 . . .
        DB      $35,$C0                        ; . 3 1 1 3 . . .
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .
        DB      $F5,$F0                        ; 3 3 1 1 3 3 . .
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .
        DB      $35,$C0                        ; . 3 1 1 3 . . .
        DB      $3F,$C0                        ; . 3 3 3 3 . . .
        DB      $CF,$00                        ; 3 . 3 3 . . . .
        DB      $0F,$00                        ; . . 3 3 . . . .

;*******************************************************************************
; ASTRO_BATTLES_NARROW_BOLT
; 1 bytes/row = 4 pixels wide, 8 rows
;*******************************************************************************
ASTRO_BATTLES_NARROW_BOLT:
        DB      $01,$08                        ; width bytes, height rows
        DB      $30                            ; . 3 . .
        DB      $38                            ; . 3 2 .
        DB      $78                            ; 1 3 2 .
        DB      $3B                            ; . 3 2 3
        DB      $3B                            ; . 3 2 3
        DB      $78                            ; 1 3 2 .
        DB      $38                            ; . 3 2 .
        DB      $30                            ; . 3 . .

;*******************************************************************************
; ASTRO_BATTLES_PROJECTILE_1
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
ASTRO_BATTLES_PROJECTILE_1:
        DB      $02,$03                        ; width bytes, height rows
        DB      $04,$04                        ; . . 1 . . . 1 .
        DB      $11,$10                        ; . 1 . 1 . 1 . .
        DB      $40,$40                        ; 1 . . . 1 . . .

;*******************************************************************************
; ASTRO_BATTLES_PROJECTILE_2
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
ASTRO_BATTLES_PROJECTILE_2:
        DB      $02,$03                        ; width bytes, height rows
        DB      $01,$00                        ; . . . 1 . . . .
        DB      $44,$44                        ; 1 . 1 . 1 . 1 .
        DB      $10,$10                        ; . 1 . . . 1 . .

;*******************************************************************************
; ASTRO_BATTLES_INVADER_BULLET_1
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
ASTRO_BATTLES_INVADER_BULLET_1:
        DB      $02,$03                        ; width bytes, height rows
        DB      $10,$10                        ; . 1 . . . 1 . .
        DB      $44,$44                        ; 1 . 1 . 1 . 1 .
        DB      $01,$00                        ; . . . 1 . . . .

;*******************************************************************************
; ASTRO_BATTLES_PROJECTILE_4
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
ASTRO_BATTLES_PROJECTILE_4:
        DB      $02,$03                        ; width bytes, height rows
        DB      $40,$40                        ; 1 . . . 1 . . .
        DB      $11,$10                        ; . 1 . 1 . 1 . .
        DB      $04,$04                        ; . . 1 . . . 1 .

;*******************************************************************************
; ASTRO_BATTLES_PROJECTILE_5
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
ASTRO_BATTLES_PROJECTILE_5:
        DB      $02,$03                        ; width bytes, height rows
        DB      $04,$10                        ; . . 1 . . 1 . .
        DB      $55,$54                        ; 1 1 1 1 1 1 1 .
        DB      $01,$04                        ; . . . 1 . . 1 .

;*******************************************************************************
; ASTRO_BATTLES_PROJECTILE_6
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
ASTRO_BATTLES_PROJECTILE_6:
        DB      $02,$03                        ; width bytes, height rows
        DB      $10,$40                        ; . 1 . . 1 . . .
        DB      $55,$54                        ; 1 1 1 1 1 1 1 .
        DB      $41,$00                        ; 1 . . 1 . . . .

;*******************************************************************************
; ASTRO_BATTLES_PROJECTILE_7
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
ASTRO_BATTLES_PROJECTILE_7:
        DB      $02,$03                        ; width bytes, height rows
        DB      $41,$00                        ; 1 . . 1 . . . .
        DB      $55,$54                        ; 1 1 1 1 1 1 1 .
        DB      $44,$00                        ; 1 . 1 . . . . .

;*******************************************************************************
; ASTRO_BATTLES_INVADER_BULLET_2
; 2 bytes/row = 8 pixels wide, 3 rows
;*******************************************************************************
ASTRO_BATTLES_INVADER_BULLET_2:
        DB      $02,$03                        ; width bytes, height rows
        DB      $40,$00                        ; 1 . . . . . . .
        DB      $55,$54                        ; 1 1 1 1 1 1 1 .
        DB      $40,$00                        ; 1 . . . . . . .

            ld      a,(de)
            rrca
            nop
            rst     $38
            inc     bc
            dec     d
            rrca
            ld      d,$FF
            dec     de
            rrca
            inc     de
            xor     b
            ld      (de),a
            or      d
            ld      de,$10BD
            add     a,b
            ld      b,$80
            adc     a,h
            inc     b
            inc     h
            inc     bc
            rst     $08
            ld      e,b
            djnz    $819F
            nop
            dec     d
            add     a,c
            ld      a,e
            djnz    $8199
            nop
            djnz    $815F
            dec     b
            jr      nz,$816E
            ld      (bc),a
            ld      bc,$120C
            rst     $38
            djnz    $8152
            ld      (bc),a
            rst     $38
            djnz    $815C
            ld      a,$12
            ld      c,d
            ld      de,$165E
            adc     a,b
            dec     d
            ex      af,af'
            inc     bc
            inc     b
            rst     $08
            ld      l,l
            nop
            add     hl,sp
            add     a,c
            ld      a,e
            djnz    $81BD
            nop
            rst     $08
            ld      l,l
            nop
            dec     sp
            ret     nc
            ret     p
            nop
            ld      l,l
            nop
            dec     sp
            ret     nc
            ld      l,a
            ld      d,$24
            dec     (hl)
            ld      c,(hl)
            inc     d
            ld      l,l
            nop
            nop
            djnz    $81E0
            nop
            nop
            djnz    $81E4
            nop
            dec     b
            add     a,b
            halt
            nop
            jr      nz,$8127
            ld      b,$6D
            nop
            nop
            djnz    $81F1
            nop
            nop
            ld      (de),a
            ld      l,l
            nop
            rla
            add     a,b
            halt
            nop
            jr      nz,$8138
            ld      b,$76
            nop
            ld      (bc),a
            halt
            nop
            ld      a,(bc)
            sbc     a,b
            nop
            adc     a,b
            nop
            xor     (hl)
            DB      $dd, $6d
            nop
            nop
            djnz    $820E
            nop
            nop
            djnz    $81FB
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            xor     (hl)
            DB      $dd,$76
            nop
            inc     bc
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            nop
            inc     d
            ld      l,l
            nop
            nop
            jr      nz,$822B
            nop
            dec     b
            add     a,b
            halt
            nop
            jr      nz,$816E
            ld      b,$6D
            nop
            nop
            djnz    $8238
            nop
            nop
            ld      (_LIT),hl
            rla
            add     a,b
            halt
            nop
            jr      nz,$817F
            ld      b,$76
            nop
            inc     d
            halt
            nop
            ld      a,(bc)
            sbc     a,b
            nop
            adc     a,b
            nop
            adc     a,$DD
            ld      l,l
            nop
            nop
            djnz    $8255
            nop
            nop
            jr      nz,$8242
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            adc     a,$DD
            halt
            nop
            inc     bc
            sub     e
            nop
            and     (hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            nop
            jr      nz,$826E
            nop
            nop
            djnz    $8272
            nop
            add     hl,hl
            add     a,b
            halt
            nop
            jr      nz,$81B5
            ld      b,$6D
            nop
            nop
            jr      nz,$827F
            nop
            nop
            ld      (de),a
            ld      l,l
            nop
            ld      b,c
            add     a,b
            halt
            nop
            jr      nz,$81C6
            ld      b,$76
            nop
            inc     b
            halt
            nop
            dec     c
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      ($6DDE),a
            nop
            nop
            jr      nz,$829C
            nop
            nop
            djnz    $8289
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      ($76DE),a
            nop
            ld      (bc),a
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            nop
            inc     h
            ld      l,l
            nop
            nop
            jr      nz,$82B9
            nop
            add     hl,hl
            add     a,b
            halt
            nop
            jr      nz,$81FC
            ld      b,$6D
            nop
            nop
            jr      nz,$82C6
            nop
            nop
            ld      (_LIT),hl
            ld      b,c
            add     a,b
            halt
            nop
            jr      nz,$820D
            ld      b,$76
            nop
            inc     d
            halt
            nop
            dec     c
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      e,h
            sbc     a,$6D
            nop
            nop
            jr      nz,$82E3
            nop
            nop
            jr      nz,$82D0
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      e,h
            sbc     a,$76
            nop
            ld      (bc),a
            sub     e
            nop
            and     (hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            nop
            jr      nc,$82FC
            nop
            nop
            djnz    $8300
            nop
            ld      e,c
            add     a,b
            halt
            nop
            jr      nz,$8243
            ld      b,$6D
            nop
            nop
            jr      nc,$830D
            nop
            nop
            ld      (de),a
            ld      l,l
            nop
            ld      (hl),e
            add     a,b
            halt
            nop
            jr      nz,$8254
            ld      b,$76
            nop
            ld      (bc),a
            halt
            nop
            ld      c,$98
            nop
            adc     a,b
            nop
            ret     nz
            sbc     a,$6D
            nop
            nop
            jr      nc,$832A
            nop
            nop
            djnz    $8317
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            ret     nz
            sbc     a,$A4
            nop
            sbc     a,b
            nop
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            sbc     a,(hl)
            nop
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            nop
            inc     (hl)
            ld      l,l
            nop
            nop
            jr      nz,$8350
            nop
            ld      e,c
            add     a,b
            halt
            nop
            jr      nz,$8293
            ld      b,$6D
            nop
            nop
            jr      nc,$835D
            nop
            nop
            ld      (_LIT),hl
            ld      (hl),e
            add     a,b
            halt
            nop
            jr      nz,$82A4
            ld      b,$76
            nop
            inc     d
            halt
            nop
            ld      c,$98
            nop
            adc     a,b
            nop
            call    pe,$6DDE
            nop
            nop
            jr      nc,$837A
            nop
            nop
            jr      nz,$8367
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            call    pe,$A4DE
            nop
            sbc     a,b
            nop
            sub     e
            nop
            and     (hl)
            DB      $dd,$f7
            nop
            sbc     a,(hl)
            nop
            sub     e
            nop
            and     (hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            dec     sp
            ret     nc
            ld      h,b
            ld      d,$24
            dec     (hl)
            ld      h,c
            nop
            ld      e,c
            add     a,b
            ld      e,c
            add     a,b
            add     hl,hl
            add     a,b
            dec     b
            add     a,b
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            ld      (hl),e
            add     a,b
            ld      (hl),e
            add     a,b
            ld      b,c
            add     a,b
            rla
            add     a,b
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            exx
            push    ix
            ld      de,$0050
            ld      b,$C0
            ld      l,$80
            ld      a,(COCKTAIL)
            and     a
            jp      nz,$836E
            ld      ix,$0000
            ld      a,$28
            jp      $8374
            ld      ix,$03BF
            ld      a,$68
            out     ($0C),a
            ld      a,$08
            out     ($19),a
            ld      a,b
            and     $03
            jp      nz,$8391
            ld      a,(ix+$00)
            and     $01
            jp      nz,$838D
            rrc     l
            jp      $838F
            sla     l
            set     7,l
            ld      (ix+$00),l
            ld      a,(COCKTAIL)
            and     a
            ld      a,b
            jp      nz,$83AE
            cp      $88
            jp      nc,$83A7
            ld      (ix+$01),l
            jp      $83AB
            ld      (ix+$01),$00
            jp      $83BD
            cp      $38
            jp      c,$83B9
            ld      (ix-$01),l
            jp      $83BD
            ld      (ix-$01),$00
            add     ix,de
            djnz    $837A
            pop     ix
            exx
            DW      _DSPATCH
;******************************************************************************************
            ld      hl,($D9B5)
            ld      de,($D9B7)
            bit     7,d
            jp      nz,$83D9
            ld      bc,($D9BF)
            jp      $83DD
            ld      bc,($D9BD)
            call    $2E95
            jp      nz,$8403
            ld      ($D9B7),de
            ld      de,($D9BB)
            ld      hl,($D9B9)
            add     hl,de
            ld      ($D9B9),hl
            ld      a,h
            cp      $09
            jp      nc,$83FD
            ld      a,$01
            ld      ($D946),a
            ld      hl,$DDA6
            jp      $840A
            add     hl,de
            ld      ($D9B5),hl
            ld      hl,$DD9E
            ld      ($DAE5),hl
            ret
            ld      a,($DD9C)
            and     a
            ret     z
            xor     a
            ld      ($DD9C),a
            ld      a,$14
            ld      ($D0A3),a
            call    $3745
            ret
            call    $2E67
            ret
            inc     d
            jr      nz,$83AB
            ld      c,$05
            inc     c
            dec     b
            or      b
            ld      a,(bc)
            dec     a
            inc     sp
;
;******************************************************************************************
;
; Unknown Pattern
;
;******************************************************************************************
;
            ld      (bc),a
            dec     b
            ld      b,h
            nop
            djnz    $8435
            ld      d,l
            nop
            djnz    $8439
            ld      b,c
            nop
            inc     c
            nop
            nop
            nop
            nop
            nop
            cpl
            add     a,h
            ld      b,$06
            nop
            ld      b,$24
            ld      (de),a
            ld      hl,$843B
            call    $1F49
            ld      hl,$1D48
            ld      (ix+$05),l
            ld      (ix+$06),h
            ret
            ld      iy,$D96B
            call    $20C6
            jr      z,$846E
            push    iy
            ex      (sp),ix
            call    $2E67
            pop     ix
            jp      $8449
            ld      iy,($D95B)
            call    $20C6
            jr      z,$847E
            res     7,(iy+$00)
            jp      $8449
            call    $3806
            jp      $8449
            ld      a,($D9AB)
            and     a
            ret     nz
            ld      c,$03
            call    $2114
            jp      z,$84D0
            ld      a,(iy+$2d)
            and     $02
            jp      z,$84A5
            push    iy
            ex      (sp),ix
            call    $8449
            pop     ix
            xor     a
            jr      $84D6
            res     7,(iy+$00)
            set     6,(iy+$00)
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      ($D9B1),hl
            ld      l,(iy+$0d)
            ld      h,(iy+$0e)
            ld      ($D9AF),hl
            ld      c,(iy+$29)
            bit     6,c
            jp      nz,$84CC
            call    $2EBD
            xor     (hl)
            ld      (hl),a
            ld      a,$01
            jr      $84D6
            call    $3151
            ret     z
            ld      a,$02
            ld      ($D9AB),a
            ld      a,c
            ld      ($D9AD),a
            call    $14BF
            res     7,(ix+$00)
            ret
            push    de
            add     a,b
            DB      $dd,$80
            push    hl
            add     a,b
            DB      $ed,$80
            push    af
            add     a,b
            DB      $fd,$80
            dec     b
            add     a,c
            dec     c
            add     a,c
            inc     b
            ld      a,d
            dec     e
            jr      z,$8500
            inc     h
            inc     d
            ld      e,c
            add     a,h
            inc     c
            add     a,b
            rst     $38
            nop
            nop
            ld      ($0600),hl
            ld      b,$22
            ld      (bc),a
            ld      b,$06
            ld      ($0604),hl
            ld      b,$22
            ld      b,$06
            ld      b,$22
            inc     b
            ld      b,$06
            ld      ($0602),hl
            ld      b,$0A
            inc     bc
            add     a,l
            ld      h,$E5
            add     a,h
            ld      a,(bc)
            push    af
            add     a,h
            ld      h,$ED
            add     a,h
            ld      a,(bc)
            push    af
            add     a,h
            ld      a,($D0A1)
            and     a
            ret     nz
            ld      a,r
            and     $0F
            ld      c,a
            and     $08
            jp      z,$8550
            ld      a,($D9B6)
            ld      b,a
            ld      a,($D97F)
            sub     b
            jp      c,$854E
            rrca
            rrca
            rrca
            rrca
            and     $07
            ld      c,a
            jp      $8550
            res     3,c
            ld      b,$00
            ld      hl,$D9D5
            add     hl,bc
            ld      a,(hl)
            and     a
            ret     z
            rrca
            jr      c,$855F
            inc     b
            jr      $8559
            ld      a,c
            rlca
            rlca
            rlca
            or      b
            ld      c,a
            call    $2E86
            ld      de,$FE00
            add     hl,de
            ld      de,($D9B9)
            add     hl,de
            push    hl
            rl      l
            ld      a,h
            rla
            ld      b,a
            ld      a,c
            call    $2E8E
            ld      de,$0700
            add     hl,de
            ld      de,($D9B5)
            add     hl,de
            push    hl
            ld      a,r
            and     $01
            jp      nz,$8592
            ld      hl,$851E
            jp      $8595
            ld      hl,$8524
            push    hl
            ld      a,b
            sub     $08
            ld      l,a
            ld      h,$00
            push    hl
            ld      hl,$02A4
            push    hl
            ld      a,($D94B)
            ld      c,a
            ld      a,($D08A)
            and     a
            jp      nz,$85B3
            ld      a,r
            and     $3F
            jp      $85C0
            cp      $03
            jp      nc,$85BF
            ld      a,r
            and     $0F
            jp      $85C0
            xor     a
            add     a,$06
            add     a,c
            ld      ($D0A1),a
            jp      $205E
            push    ix
            push    iy
            exx
            call    $852A
            exx
            pop     iy
            pop     ix
            ld      a,($D94B)
            add     a,$05
            ld      ($D0F0),a           ; Processor 2 TIMEBASE (IY=$D0E1, offset $0F)
            DW      _DSPATCH
;******************************************************************************************
            inc     c
            nop
            nop
            add     a,b
            ld      bc,$E00E
            rst     $38
            nop
            nop
            nop
            add     a,c
            ld      ($0B06),hl
            nop
            adc     a,$22
            inc     c
            add     a,b
            ld      bc,$0180
            ld      b,$0B
            djnz    $8607
            nop
            nop
            add     a,b
            cp      $0E
            ret     po
            rst     $38
            nop
            nop
            nop
            add     a,c
            ld      ($0B06),hl
            nop
            adc     a,$22
            inc     c
            add     a,b
            ld      bc,$FE80
            ld      b,$0B
            djnz    $8631
            inc     bc
            ex      af,af'
            ret     po
            add     a,l
            ld      e,$16
            add     a,(hl)
            djnz    $863A
            inc     bc
            ex      af,af'
            jp      m,$1E85
            rra
            add     a,(hl)
            djnz    $864F
            ld      b,$24
            inc     b
            ld      ($161E),hl
            nop
            ld      b,d
            djnz    $8639
            ld      h,$86
            jr      $8635
            or      b
            inc     e
            dec     b
            ex      af,af'
            jp      m,$1E85
            jr      c,$85C4
            djnz    $8648
            inc     d
            add     a,(hl)
            ex      af,af'
            dec     e
            add     a,(hl)
            nop
            ld      b,$24
            ld      (de),a
            ex      af,af'
            jr      nc,$85D2
            ld      a,(bc)
            ccf
            add     a,(hl)
            ex      af,af'
            jr      nc,$85D8
            ld      (de),a
            ex      af,af'
            ld      h,$86
            jr      $8658
            nop
            inc     e
            dec     b
            ex      af,af'
            ret     po
            add     a,l
            ld      e,$5B
            add     a,(hl)
            ld      (de),a
            inc     b
            ld      ($161E),hl
            add     a,b
            ld      b,d
            jr      $866A
            nop
            jr      z,$8673
            inc     h
            nop
            or      e
            add     a,b
            inc     c
            nop
            nop
            nop
            ld      (bc),a
            ld      b,$20
            ld      c,$00
            nop
            ret     m
            rst     $38
            ld      b,$40
            inc     c
            nop
            nop
            nop
            nop
            ld      c,$00
            nop
            nop
            nop
            ld      b,$18
            ld      c,$00
            nop
            ret     m
            rst     $38
            ld      b,$40
            ld      c,$00
            nop
            nop
            nop
            ld      b,$3F
            ld      (de),a
            inc     b
            ld      ($161E),hl
            add     a,b
            ld      b,d
            jr      $86A2
            or      h
            jr      z,$86AB
            inc     h
            nop
            or      e
            add     a,b
            inc     c
            nop
            nop
            nop
            cp      $06
            jr      nz,$86BF
            nop
            nop
            ex      af,af'
            nop
            ld      b,$40
            inc     c
            nop
            nop
            nop
            nop
            ld      c,$00
            nop
            nop
            nop
            ld      b,$18
            ld      c,$00
            nop
            ex      af,af'
            nop
            ld      b,$40
            ld      c,$00
            nop
            nop
            nop
            ld      b,$3F
            ld      (de),a
            nop
            adc     a,l
            add     a,b
            jr      z,$86DD
            inc     h
            ld      d,$00
            ld      b,c
            jr      $86DD
            nop
            inc     c
            nop
            nop
            nop
            ld      bc,$7806
            ld      b,$3C
            ld      (de),a
            nop
            adc     a,l
            add     a,b
            jr      z,$86F3
            inc     h
            ld      d,$00
            ld      b,c
            jr      $86F3
            or      h
            inc     c
            nop
            nop
            nop
            rst     $38
            ld      b,$78
            ld      b,$3C
            ld      (de),a
            ld      b,e
            jp      nc,$4386
            jp      nc,$4386
            ret     pe
            add     a,(hl)
            ld      b,e
            ret     pe
            add     a,(hl)
            ld      b,l
            ld      h,d
            add     a,(hl)
            ld      b,l
            ld      h,d
            add     a,(hl)
            ld      b,l
            sbc     a,d
            add     a,(hl)
            ld      b,e
            ret     pe
            add     a,(hl)
            ld      b,h
            ld      c,a
            add     a,(hl)
            ld      b,h
            ld      d,e
            add     a,(hl)
            ld      b,l
            sbc     a,d
            add     a,(hl)
            ld      b,e
            jp      nc,$4486
            ld      c,a
            add     a,(hl)
            ld      b,h
            ld      d,e
            add     a,(hl)
            ld      b,e
            jp      nc,$4386
            ret     pe
            add     a,(hl)
            ld      hl,($D09F)
            ld      a,h
            or      l
            ret     nz
            ld      de,$0300
            call    rnd
            ld      de,$0180
            add     hl,de
            ld      ($D09F),hl
            ld      a,r
            and     $0F
            ld      c,a
            rlca
            add     a,c
            ld      c,a
            ld      b,$00
            ld      hl,$86FE
            add     hl,bc
            ld      c,(hl)
            inc     hl
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ld      hl,$0000
            push    hl
            push    hl
            push    de
            push    bc
            ld      hl,$01A6
            push    hl
            jp      $205E
            push    ix
            push    iy
            push    bc
            call    $872E
            pop     bc
            pop     iy
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            in      a,($65)
            and     h
            ld      c,d
            in      a,($65)
            ld      d,e
            rlca
            in      a,($65)
            ld      d,e
            rlca
            in      a,($65)
            and     h
            ld      c,d
            nop
            inc     h
            nop
            jr      nz,$8786
            jr      $8788
            inc     d
            nop
            djnz    $875B
            ld      l,l
            nop
            nop
            ld      bc,_LIT
            nop
            ld      d,b
            ld      l,l
            nop
            jr      $87A0
            halt
            nop
            ld      ($09C4),hl
            ld      h,c
            nop
            rst     $08
            sbc     a,b
            nop
            adc     a,e
            dec     b
            ld      a,e
            dec     (hl)
            ld      c,(hl)
            inc     d
            ld      e,l
            add     a,c
            xor     (hl)
            add     hl,hl
            ld      d,h
            add     a,e
            adc     a,e
            add     a,a
            add     ix,ix
            halt
            nop
            jr      $8823
            nop
            ld      c,e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            scf
            ret     nc
            ret     p
            nop
            halt
            nop
            inc     b
            ld      h,$04
            sub     e
            nop
            add     a,c
            add     a,a
            jp      (hl)
            nop
            ld      l,l
            nop
            cp      c
            exx
            rst     $30
            nop
            ld      l,l
            nop
            nop
            call    m,_LIT
            cp      e
            exx
            rst     $30
            nop
            halt
            nop
            ex      af,af'
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            halt
            nop
            ld      c,$8D
            ld      (bc),a
            adc     a,b
            nop
            call    $FED9
            nop
            ld      e,a
            ld      (bc),a
            halt
            nop
            add     a,b
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            ld      l,l
            nop
            or      l
            exx
            jp      (hl)
            nop
            adc     a,l
            ld      (bc),a
            sub     e
            nop
            DB      $dd,$d9
            rst     $30
            nop
            ld      l,l
            nop
            cp      c
            exx
            jp      (hl)
            nop
            adc     a,l
            ld      (bc),a
            ld      hl,($9301)
            nop
            DB      $dd,$d9
            rst     $30
            nop
            halt
            nop
            ld      (bc),a
            sub     a
            inc     bc
            ld      l,l
            nop
            sbc     a,h
            DB      $dd,$03
            ld      bc,_LIT
            cp      $FF
            or      l
            scf
            ld      l,l
            nop
            add     a,$83
            ld      l,l
            nop
            rst     $00
            exx
            rst     $30
            nop
            sbc     a,b
            nop
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd, $6d
            nop
            push    hl
            jp      c,$00F7
            ld      l,l
            nop
            ld      l,$81
            ld      l,l
            nop
            ld      l,c
            exx
            rst     $30
            nop
            ld      l,l
            nop
            add     a,h
            add     a,h
            ld      l,l
            nop
            ld      d,e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            ld      c,$84
            ld      l,l
            nop
            ld      b,d
            exx
            rst     $30
            nop
            ld      l,l
            nop
            inc     h
            add     a,h
            ld      l,l
            nop
            ld      h,a
            exx
            rst     $30
            nop
            ld      l,l
            nop
            xor     e
            exx
            inc     bc
            ld      bc,_LIT
            inc     (hl)
            add     a,e
            ld      l,l
            nop
            ret
            exx
            rst     $30
            nop
            ld      h,c
            nop
            ld      b,$07
            inc     c
            nop
            nop
            nop
            nop
            inc     b
            ld      a,(bc)
            ld      sp,$7806
            ld      (de),a
            nop
            dec     b
            add     a,b
            ld      a,(bc)
            halt
            adc     a,b
            nop
            add     hl,hl
            add     a,b
            ld      a,(bc)
            halt
            adc     a,b
            nop
            ld      e,c
            add     a,b
            ld      a,(bc)
            halt
            adc     a,b
            adc     a,a
            adc     a,b
            adc     a,a
            adc     a,b
            adc     a,c
            adc     a,b
            add     a,e
            adc     a,b
            rst     $08
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            rst     $30
            nop
            inc     c
            dec     hl
            cp      h
            ld      ($288B),a
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            jp      (hl)
            nop
            sbc     a,b
            nop
            ld      l,h
            ld      bc,_0BRANCH
            and     h
            adc     a,b
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            ld      l,d
            rst     $18
            rst     $30
            nop
            ld      l,l
            nop
            ld      l,b
            rst     $18
            rst     $30
            nop
            ld      l,l
            nop
            ld      l,d
            rst     $18
            jp      (hl)
            nop
            halt
            nop
            ex      af,af'
            rla
            inc     bc
            ld      l,l
            nop
            ld      l,b
            rst     $18
            jp      (hl)
            nop
            dec     bc
            ld      bc,$0076
            dec     c
            ld      l,l
            nop
            ld      h,(hl)
            rst     $18
            jp      (hl)
            nop
            dec     bc
            ld      bc,$00E9
            halt
            nop
            inc     de
            ld      l,l
            nop
            ld      h,(hl)
            rst     $18
            jp      (hl)
            nop
            dec     bc
            ld      bc,$00E9
            ld      l,l
            nop
            nop
            inc     b
            dec     bc
            ld      bc,_LIT
            ld      l,b
            rst     $18
            jp      (hl)
            nop
            ld      l,l
            nop
            nop
            inc     b
            rla
            inc     bc
            ld      l,l
            nop
            cp      c
            exx
            jp      (hl)
            nop
            dec     bc
            ld      bc,_LIT
            ld      l,d
            rst     $18
            jp      (hl)
            nop
            ld      l,l
            nop
            nop
            djnz    $8930
            inc     bc
            ld      l,l
            nop
            ld      l,b
            rst     $18
            jp      (hl)
            nop
            sub     e
            nop
            sub     l
            adc     a,b
            jp      (hl)
            nop
            halt
            nop
            ex      af,af'
            ld      l,l
            nop
            and     d
            ld      bc,$201F
            halt
            nop
            rlca
            sbc     a,l
            adc     a,b
            ld      h,c
            nop
            rst     $08
            dec     a
            dec     d
            ld      l,l
            nop
            ld      h,(hl)
            rst     $18
            rst     $30
            nop
            and     a
            djnz    $89AF
            nop
            add     hl,sp
            add     a,c
            ld      l,l
            djnz    $88E6
            nop
            halt
            nop
            ld      (bc),a
            ld      l,l
            nop
            dec     sp
            ret     nc
            ret     p
            nop
            jp      pe,$6103
            adc     a,c
            ld      l,l
            nop
            ld      a,c
            add     a,a
            halt
            nop
            dec     de
            jp      po,$6803
            adc     a,c
            ld      l,l
            nop
            ld      (hl),c
            add     a,a
            halt
            nop
            dec     c
            halt
            nop
            add     hl,bc
            ret     p
            ld      (bc),a
            rlca
            scf
            ld      b,(hl)
            inc     sp
            halt
            nop
            inc     l
            ld      l,l
            nop
            ld      c,c
            add     a,(hl)
            halt
            nop
            ld      b,a
            ld      l,l
            nop
            or      d
            ld      bc,_LIT
            ld      h,(hl)
            rst     $18
            jp      (hl)
            nop
            ld      h,$20
            sbc     a,l
            adc     a,b
            sbc     a,b
            nop
            halt
            nop
            rlca
            ld      b,h
            ld      (bc),a
            sbc     a,(hl)
            nop
            adc     a,l
            ld      (bc),a
            cp      d
            adc     a,b
            ld      l,l
            nop
            rst     $38
            rst     $38
            sub     a
            inc     bc
            halt
            nop
            ex      af,af'
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            halt
            nop
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            cp      d
            adc     a,b
            ld      e,a
            ld      (bc),a
            sbc     a,b
            nop
            halt
            nop
            rlca
            ld      b,h
            ld      (bc),a
            halt
            nop
            inc     bc
            adc     a,l
            ld      (bc),a
            cp      d
            adc     a,b
            ld      l,l
            nop
            rst     $38
            rst     $38
            sub     a
            inc     bc
            halt
            nop
            djnz    $8961
            adc     a,b
            rst     $10
            scf
            ld      h,c
            nop
            in      a,($65)
            ld      d,e
            ld      a,e
            in      a,($65)
            ld      d,e
            ld      a,e
            rst     $08
            call    $5404
            add     a,e
            adc     a,e
            add     a,a
            jp      p,$6D37
            nop
            dec     sp
            ret     nc
            ret     p
            nop
            jp      pe,$1703
            adc     a,d
            ld      (de),a
            ld      (hl),$98
            nop
            halt
            nop
            add     hl,bc
            ret     p
            ld      (bc),a
            ld      l,l
            nop
            ret
            adc     a,c
            ld      a,h
            dec     b
            sbc     a,b
            nop
            halt
            nop
            inc     b
            ret     p
            ld      (bc),a
            halt
            nop
            inc     l
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            halt
            nop
            add     hl,bc
            ld      (de),a
            ld      (hl),$F0
            ld      (bc),a
            ret     nc
            inc     b
            halt
            nop
            ld      (bc),a
            jp      (hl)
            ld      ($025F),a
            jp      po,$4A03
            adc     a,d
            ld      (de),a
            ld      (hl),$76
            nop
            inc     l
            halt
            nop
            add     hl,bc
            ret     p
            ld      (bc),a
            ld      l,l
            nop
            ret
            adc     a,c
            ld      a,h
            dec     b
            sbc     a,b
            nop
            sbc     a,b
            nop
            ret     p
            ld      (bc),a
            sbc     a,b
            nop
            halt
            nop
            inc     l
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            halt
            nop
            add     hl,bc
            ld      (de),a
            ld      (hl),$F0
            ld      (bc),a
            ret     nc
            inc     b
            halt
            nop
            ld      (bc),a
            jp      (hl)
            ld      (_LIT),a
            rst     $38
            rst     $38
            sub     a
            inc     bc
            sbc     a,b
            nop
            adc     a,e
            dec     b
            ld      h,c
            nop
            rst     $08
            adc     a,e
            jr      z,$8A60
            dec     hl
            cp      h
            ld      ($85C9),a
            rst     $10
            scf
            ld      h,d
            add     a,a
            add     a,e
            jr      nc,$8A65
            inc     (hl)
            ld      h,c
            nop
            rst     $08
            sbc     a,a
            add     a,a
            ld      (hl),$89
            ld      l,$81
            ld      d,b
            adc     a,d
            ld      l,l
            nop
            ld      c,a
            exx
            jp      (hl)
            nop
            jp      pe,$6A03
            adc     a,d
            ld      l,l
            nop
            ld      d,c
            exx
            jp      (hl)
            nop
            call    po,$EA01
            inc     bc
            add     a,h
            adc     a,d
            pop     de
            adc     a,c
            ld      h,c
            nop
            rst     $08
            sbc     a,a
            add     a,a
            ld      l,$81
            halt
            nop
            ex      af,af'
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            halt
            nop
            ld      c,$8D
            ld      (bc),a
            adc     a,b
            nop
            push    de
            exx
            cp      $00
            ld      e,a
            ld      (bc),a
            ld      (bc),a
            dec     (hl)
            rst     $10
            scf
            ld      b,(hl)
            inc     sp
            halt
            nop
            dec     c
            halt
            nop
            add     hl,bc
            ret     p
            ld      (bc),a
            sbc     a,(hl)
            nop
            sbc     a,(hl)
            nop
            ld      l,l
            nop
            ld      (hl),c
            add     a,a
            rlca
            scf
            ld      l,l
            nop
            nop
            inc     bc
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            sbc     a,b
            rrca
            add     a,e
            jr      nc,$8A8D
            add     a,l
            ld      h,d
            add     a,a
            jr      $8AF8
            ld      e,a
            ld      (bc),a
            sbc     a,(hl)
            nop
            jr      z,$8B06
            ld      h,c
            nop
            call    po,$A7A5
            sub     c
            rst     $08
            sub     c
;*******************************************************************************
; LASER_ATTACK_RED_WINGED_BUG
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
LASER_ATTACK_RED_WINGED_BUG:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $00,$00,$C0                    ; . . . . . . . . 3 . . .
        DB      $03,$FF,$C0                    ; . . . 3 3 3 3 3 3 . . .
        DB      $00,$28,$80                    ; . . . . . 2 2 . 2 . . .
        DB      $00,$28,$00                    ; . . . . . 2 2 . . . . .
        DB      $00,$A8,$28                    ; . . . . 2 2 2 . . 2 2 .
        DB      $55,$AA,$A8                    ; 1 1 1 1 2 2 2 2 2 2 2 .
        DB      $00,$A8,$28                    ; . . . . 2 2 2 . . 2 2 .
        DB      $00,$28,$00                    ; . . . . . 2 2 . . . . .
        DB      $00,$28,$80                    ; . . . . . 2 2 . 2 . . .
        DB      $03,$FF,$C0                    ; . . . 3 3 3 3 3 3 . . .
        DB      $00,$00,$C0                    ; . . . . . . . . 3 . . .

;*******************************************************************************
; LASER_ATTACK_YELLOW_WINGED_BUG_1
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
LASER_ATTACK_YELLOW_WINGED_BUG_1:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $00,$01                        ; . . . . . . . 1
        DB      $00,$05                        ; . . . . . . 1 1
        DB      $00,$21                        ; . . . . . 2 . 1
        DB      $00,$80                        ; . . . . 2 . . .
        DB      $F1,$50                        ; 3 3 . 1 1 1 . .
        DB      $C5,$54                        ; 3 . 1 1 1 1 1 .
        DB      $F1,$50                        ; 3 3 . 1 1 1 . .
        DB      $00,$80                        ; . . . . 2 . . .
        DB      $00,$21                        ; . . . . . 2 . 1
        DB      $00,$05                        ; . . . . . . 1 1
        DB      $00,$01                        ; . . . . . . . 1

;*******************************************************************************
; LASER_ATTACK_BUG_SHIP_2
; 3 bytes/row = 12 pixels wide, 10 rows
;*******************************************************************************
LASER_ATTACK_BUG_SHIP_2:
        DB      $03,$0A                        ; width bytes, height rows
        DB      $00,$10,$00                    ; . . . . . 1 . . . . . .
        DB      $00,$14,$00                    ; . . . . . 1 1 . . . . .
        DB      $00,$94,$00                    ; . . . . 2 1 1 . . . . .
        DB      $00,$80,$00                    ; . . . . 2 . . . . . . .
        DB      $00,$54,$00                    ; . . . . 1 1 1 . . . . .
        DB      $31,$54,$00                    ; . 3 . 1 1 1 1 . . . . .
        DB      $C5,$50,$00                    ; 3 . 1 1 1 1 . . . . . .
        DB      $F1,$54,$40                    ; 3 3 . 1 1 1 1 . 1 . . .
        DB      $00,$29,$50                    ; . . . . . 2 2 1 1 1 . .
        DB      $00,$00,$10                    ; . . . . . . . . . 1 . .

;*******************************************************************************
; LASER_ATTACK_BUG_SHIP_3
; 3 bytes/row = 12 pixels wide, 10 rows
;*******************************************************************************
LASER_ATTACK_BUG_SHIP_3:
        DB      $03,$0A                        ; width bytes, height rows
        DB      $04,$00,$00                    ; . . 1 . . . . . . . . .
        DB      $05,$00,$00                    ; . . 1 1 . . . . . . . .
        DB      $04,$00,$00                    ; . . 1 . . . . . . . . .
        DB      $08,$00,$00                    ; . . 2 . . . . . . . . .
        DB      $09,$10,$00                    ; . . 2 1 . 1 . . . . . .
        DB      $05,$40,$00                    ; . . 1 1 1 . . . . . . .
        DB      $15,$50,$40                    ; . 1 1 1 1 1 . . 1 . . .
        DB      $05,$69,$50                    ; . . 1 1 1 2 2 1 1 1 . .
        DB      $D1,$00,$00                    ; 3 1 . 1 . . . . . . . .
        DB      $F0,$00,$00                    ; 3 3 . . . . . . . . . .

;*******************************************************************************
; LASER_ATTACK_BUG_SHIP_4
; 3 bytes/row = 12 pixels wide, 10 rows
;*******************************************************************************
LASER_ATTACK_BUG_SHIP_4:
        DB      $03,$0A                        ; width bytes, height rows
        DB      $50,$00,$00                    ; 1 1 . . . . . . . . . .
        DB      $14,$00,$00                    ; . 1 1 . . . . . . . . .
        DB      $10,$00,$00                    ; . 1 . . . . . . . . . .
        DB      $24,$51,$40                    ; . 2 1 . 1 1 . 1 1 . . .
        DB      $25,$51,$50                    ; . 2 1 1 1 1 . 1 1 1 . .
        DB      $05,$5A,$00                    ; . . 1 1 1 1 2 2 . . . .
        DB      $05,$40,$00                    ; . . 1 1 1 . . . . . . .
        DB      $01,$00,$00                    ; . . . 1 . . . . . . . .
        DB      $0C,$C0,$00                    ; . . 3 . 3 . . . . . . .
        DB      $0F,$00,$00                    ; . . 3 3 . . . . . . . .

;*******************************************************************************
; LASER_ATTACK_BUG_SHIP_COMPACT
; 3 bytes/row = 12 pixels wide, 8 rows
;*******************************************************************************
LASER_ATTACK_BUG_SHIP_COMPACT:
        DB      $03,$08                        ; width bytes, height rows
        DB      $54,$00,$54                    ; 1 1 1 . . . . . 1 1 1 .
        DB      $10,$10,$10                    ; . 1 . . . 1 . . . 1 . .
        DB      $08,$54,$80                    ; . . 2 . 1 1 1 . 2 . . .
        DB      $02,$56,$00                    ; . . . 2 1 1 1 2 . . . .
        DB      $00,$54,$00                    ; . . . . 1 1 1 . . . . .
        DB      $00,$10,$00                    ; . . . . . 1 . . . . . .
        DB      $00,$CC,$00                    ; . . . . 3 . 3 . . . . .
        DB      $00,$FC,$00                    ; . . . . 3 3 3 . . . . .

;*******************************************************************************
; Bytes following the sprite boundary
;*******************************************************************************
        DB      $FA

            adc     a,d
            ld      (de),a
            adc     a,e
            ld      ($528B),a
            adc     a,e
            ld      (hl),d
            adc     a,e
            ld      h,$8C
            adc     a,e
            ld      ($0600),hl
            ld      a,b
            ld      a,(bc)
            sbc     a,e
            adc     a,e
            djnz    $8BCA
            inc     de
            dec     c
            ld      (de),a
            add     hl,hl
            ld      de,$163E
            cp      e
            dec     d
            dec     de
            rrca
            jr      nz,$8BB0
            ld      bc,$0100
            jr      nz,$8BC3
            nop
            rst     $38
            ld      bc,$0520
            ex      af,af'
            jr      z,$8BBB
            ld      bc,$2001
            ld      (bc),a
            and     c
            daa
            ld      a,($D9AB)
            and     a
            ret     nz
            ld      c,$01
            call    $2114
            ret     z
            res     7,(iy+$00)
            set     6,(iy+$00)
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      ($D9B1),hl
            ld      l,(iy+$0d)
            ld      h,(iy+$0e)
            ld      ($D9AF),hl
            ld      a,(iy+$29)
            ld      ($D9AD),a
            ld      a,$01
            ld      ($D9AB),a
            call    $14BF
            res     7,(ix+$00)
            ret
            ld      a,c
            and     a
            ret     z
            ld      l,(ix+$0d)
            ld      h,(ix+$0e)
            ld      e,(ix+$0f)
            ld      d,(ix+$10)
            ld      b,c
            add     hl,de
            djnz    $8C0A
            ld      a,h
            cp      (ix+$11)
            jp      nc,$8C22
            ld      h,(ix+$11)
            ld      l,$00
            ld      (ix+$0f),l
            ld      (ix+$10),l
            jp      $8C33
            cp      (ix+$12)
            jp      c,$8C33
            ld      h,(ix+$12)
            ld      l,$00
            ld      (ix+$0f),l
            ld      (ix+$10),l
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      e,(ix+$15)
            ld      d,(ix+$16)
            ld      b,c
            add     hl,de
            djnz    $8C46
            ld      a,h
            cp      $E0
            jp      c,$8C5D
            ld      h,(ix+$17)
            ld      l,$00
            ld      (ix+$15),l
            ld      (ix+$16),l
            jp      $8C6E
            cp      (ix+$18)
            jp      c,$8C6E
            ld      h,(ix+$18)
            ld      l,$00
            ld      (ix+$15),l
            ld      (ix+$16),l
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      (ix+$08),$40
            ret
            call    $1C67
            call    $8BFA
            call    $1C4B
            jp      $1A6B
            inc     b
            ld      a,c
            adc     a,h
            nop
            ld      b,$24
            ld      c,$18
            jr      c,$8C8F
            sbc     a,h
            ld      d,$00
            jr      z,$8C9A
            jr      nz,$8CA0
            sub     e
            adc     a,h
            jr      $8C9A
            jr      nz,$8CA6
            add     a,l
            adc     a,h
            jr      $8CA0
            ld      (hl),b
            ld      a,(bc)
            add     a,l
            adc     a,h
            nop
            rst     $10
            adc     a,d
            ld      b,$20
            ld      a,(bc)
            and     a
            adc     a,h
            rst     $08
            dec     a
            dec     d
            ld      l,l
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            dec     a
            dec     d
            ld      l,l
            nop
            and     b
            DB      $dd,$f7
            nop
            halt
            nop
            dec     b
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            dec     a
            dec     d
            adc     a,l
            ld      (bc),a
            sub     e
            nop
            and     d
            DB      $dd,$f7
            nop
            dec     a
            dec     d
            adc     a,l
            ld      (bc),a
            sub     e
            nop
            xor     h
            DB      $dd,$f7
            nop
            ld      e,a
            ld      (bc),a
            ld      l,l
            nop
            sbc     a,b
            adc     a,h
            sbc     a,b
            nop
            halt
            nop
            cp      d
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            ld      h,$20
            ld      l,l
            nop
            sbc     a,(hl)
            adc     a,h
            sbc     a,b
            nop
            halt
            nop
            cp      d
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            ld      h,$20
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            inc     b
            sbc     a,b
            nop
            ld      l,l
            nop
            sub     (hl)
            adc     a,e
            halt
            nop
            inc     bc
            ld      l,l
            nop
            or      d
            ld      bc,_0
            sub     e
            nop
            and     d
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            inc     b
            sbc     a,b
            nop
            ld      l,l
            nop
            sub     (hl)
            adc     a,e
            halt
            nop
            inc     bc
            ld      l,l
            nop
            or      d
            ld      bc,_0
            sub     e
            nop
            xor     h
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            inc     b
            ld      l,l
            nop
            nop
            djnz    $8DBA
            nop
            sub     (hl)
            adc     a,e
            halt
            nop
            inc     bc
            ld      l,l
            nop
            or      d
            ld      bc,$009E
            sub     e
            nop
            and     d
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            inc     b
            ld      l,l
            nop
            nop
            djnz    $8DDD
            nop
            sub     (hl)
            adc     a,e
            halt
            nop
            inc     bc
            ld      l,l
            nop
            or      d
            ld      bc,$009E
            sub     e
            nop
            xor     h
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            inc     b
            ld      l,l
            nop
            nop
            jr      nz,$8E00
            nop
            sub     (hl)
            adc     a,e
            halt
            nop
            inc     bc
            ld      l,l
            nop
            or      d
            ld      bc,$0076
            ld      (bc),a
            sub     e
            nop
            and     d
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            inc     b
            ld      l,l
            nop
            nop
            jr      nz,$8E24
            nop
            sub     (hl)
            adc     a,e
            halt
            nop
            inc     bc
            ld      l,l
            nop
            or      d
            ld      bc,$0076
            ld      (bc),a
            sub     e
            nop
            xor     h
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            ex      af,af'
            ld      l,l
            nop
            nop
            djnz    $8E48
            nop
            ld      e,b
            inc     a
            halt
            nop
            inc     bc
            ld      l,l
            nop
            or      d
            ld      bc,$0076
            inc     bc
            sub     e
            nop
            and     d
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            ex      af,af'
            ld      l,l
            nop
            nop
            djnz    $8E6C
            nop
            ld      e,b
            inc     a
            halt
            nop
            inc     bc
            ld      l,l
            nop
            or      d
            ld      bc,$0076
            inc     bc
            sub     e
            nop
            xor     h
            jp      (ix)
            nop
            ld      a,$39
            halt
            nop
            ld      a,(bc)
            ld      l,l
            nop
            ld      c,e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            nop
            djnz    $8E97
            nop
            and     h
            adc     a,h
            halt
            nop
            inc     b
            ld      l,l
            nop
            or      d
            ld      bc,$0076
            inc     b
            sub     e
            nop
            and     d
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            nop
            djnz    $8EB9
            nop
            and     h
            adc     a,h
            halt
            nop
            inc     b
            ld      l,l
            nop
            or      d
            ld      bc,$0076
            inc     b
            sub     e
            nop
            xor     h
            jp      (ix)
            nop
            ld      a,$39
            ld      h,c
            nop
            ld      a,($D0A3)
            and     a
            ret     nz
            ld      a,r
            and     $07
            cp      $04
            jp      nc,$8E77
            ld      hl,$DDA2
            jp      $8E7C
            ld      hl,$DDAC
            and     $03
            rlca
            ld      e,a
            ld      d,$00
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            push    de
            pop     ix
            di
            bit     7,(ix+$00)
            ret     z
            bit     7,(ix+$09)
            ret     z
            ld      a,(ix+$14)
            sub     $20
            cp      $90
            ret     nc
            ld      a,r
            and     $01
            jp      nz,$8EA7
            ld      hl,$3C42
            jp      $8EAA
            ld      hl,$3C48
            res     7,(ix+$09)
            call    $1F49
            ld      a,($D08A)
            and     a
            jp      nz,$8EBF
            ld      a,r
            and     $7F
            jp      $8ECC
            cp      $02
            jp      nc,$8ECB
            ld      a,r
            and     $3F
            jp      $8ECC
            xor     a
            add     a,$20
            ld      ($D0A3),a
            jp      $27D1
            push    ix
            push    iy
            push    bc
            call    $8E63
            ei
            pop     bc
            pop     iy
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            push    de
            ld      a,($D08A)
            and     a
            jp      nz,$8EF6
            ld      de,$0040
            push    de
            ld      de,$0030
            jp      $8F0C
            cp      $03
            jp      nc,$8F05
            ld      de,$0020
            push    de
            ld      de,$0018
            jp      $8F0C
            ld      de,$0010
            push    de
            ld      de,$0010
            call    rnd
            pop     de
            add     hl,de
            push    hl
            ld      de,$2000
            call    rnd
            ld      de,$1800
            add     hl,de
            pop     de
            push    de
            di
            ld      c,(ix+$0d)
            ld      b,(ix+$0e)
            call    $1F20
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      (ix+$0f),e
            ld      (ix+$10),d
            ld      a,($D94B)
            cp      $04
            jp      c,$8F4B
            ld      de,$4800
            call    rnd
            pop     de
            pop     bc
            ld      a,r
            and     $03
            jp      $8F5A
            ld      de,$8C00
            call    rnd
            pop     de
            pop     bc
            ld      bc,$0200
            ld      a,r
            and     $01
            jp      nz,$8F6C
            ld      a,($D97F)
            sub     $09
            cp      $B2
            jp      nc,$8F6C
            ld      h,a
            ld      l,$00
            ld      b,l
            ld      c,l
            push    de
            add     hl,bc
            ld      c,(ix+$13)
            ld      b,(ix+$14)
            call    $1F20
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      (ix+$15),e
            ld      (ix+$16),d
            ei
            pop     de
            ret
            inc     a
            ld      (ix+$11),a
            ld      a,(ix+$0e)
            ld      (ix+$12),a
            ld      l,(ix+$19)
            ld      h,(ix+$1a)
            ld      (ix+$17),l
            ld      (ix+$18),h
            ret
            ld      c,(ix+$07)
            ld      (ix+$07),$00
            ld      a,c
            and     a
            jp      z,$1A6B
            ld      a,(ix+$11)
            and     a
            jp      nz,$8FB3
            call    $8F86
            ld      a,(ix+$12)
            and     a
            jp      nz,$8FCF
            ld      a,(ix+$11)
            cp      $02
            jp      nz,$8FC9
            res     7,(ix+$00)
            jp      $8FCC
            call    $8F86
            jp      $9001
            ld      b,c
            sub     c
            jp      p,$8FD6
            add     a,c
            ld      b,a
            ld      a,(ix+$12)
            sub     b
            ld      (ix+$12),a
            ld      a,$20
            out     ($0C),a
            ld      l,(ix+$17)
            ld      h,(ix+$18)
            ld      a,(COCKTAIL)
            and     a
            jp      nz,$8FF6
            dec     hl
            ld      (hl),$55
            djnz    $8FEE
            jp      $8FFB
            inc     hl
            ld      (hl),$55
            djnz    $8FF6
            ld      (ix+$17),l
            ld      (ix+$18),h
            jp      $1A6B
            inc     b
            sbc     a,l
            adc     a,a
            nop
            ld      b,$24
            ld      b,$04
            di
            ld      l,(ix+$0d)
            ld      h,(ix+$0e)
            push    hl
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      de,$1500
            add     hl,de
            push    hl
            ld      hl,$9004
            push    hl
            ld      hl,$0000
            push    hl
            ld      hl,$00A2
            push    hl
            jp      $205E
            xor     a
            ld      (ix+$0f),a
            ld      (ix+$10),a
            ld      (ix+$15),a
            ld      (ix+$16),a
            ret
            ld      a,(hl)
            and     a
            jp      nz,$9067
            inc     a
            ld      (hl),a
            di
            bit     7,(iy+$00)
            jp      z,$9064
            call    $902E
            ld      a,(ix+$0e)
            inc     a
            ld      e,a
            ld      d,$00
            push    de
            call    $900C
LZSCORE     EQU     $8BA0               ; ATTACK FIGHTER 0102: laser score

; ----> play_attack_fighter_laser_sound
;       FCHECK reaches this branch after saving mission state on the native stack. PMUSIC
;       clears processor 2 and priority-starts LZSCORE; callers must preserve that context.
play_attack_fighter_laser_sound:
            ld      hl,LZSCORE
            ld      iy,$D0E1
            call    pmusic
            pop     de
            jp      $906C
            xor     a
            ld      (hl),a
            call    $8EE4
            ret
            push    ix
            push    iy
            push    bc
            ld      a,($D0A1)
            and     a
            jr      nz,$908D
            ld      iy,($DDAA)
            ld      ix,($DD9E)
            ld      de,$0200
            ld      hl,$DDB6
            call    $903C
            ld      ($D0A1),de
            ld      a,($D0A5)
            and     a
            jr      nz,$90A8
            ld      iy,($DDB4)
            ld      ix,($DDA0)
            ld      de,$4800
            ld      hl,$DDB8
            call    $903C
            ld      ($D0A5),de
            pop     bc
            pop     iy
            pop     ix
            ei
            DW      _DSPATCH
;******************************************************************************************
            ld      hl,($DDAA)
            ld      a,(hl)
            ld      hl,($DDB4)
            or      (hl)
            bit     7,a
            jp      nz,$90C1
            ld      hl,$D08A
            inc     (hl)
            DW      _DSPATCH
;******************************************************************************************
            call    $389B
            call    $2E67
            push    ix
            ld      ix,($DD9E)
            call    $902E
            ld      ix,($DDA0)
            call    $902E
            pop     ix
            ret
            inc     d
            jp      $0E90
            dec     b
            jr      nz,$90EC
            or      b
            ld      a,(bc)
            dec     a
            inc     sp
            inc     b
            ld      (hl),l
            call    m,$044A
            ld      (hl),l
            call    m,$D94A
            pop     hl
            push    ix
            ld      b,$05
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            push    de
            pop     ix
            bit     7,(ix+$00)
            jp      z,$9109
            bit     7,(ix+$09)
            jr      z,$9110
            djnz    $90F5
            ld      hl,$0001
            jr      $9113
            ld      hl,$0000
            pop     ix
            push    hl
            exx
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            adc     a,e
            jr      z,$90B5
            nop
            sub     e
            nop
            and     d
            DB      $dd,$ef
            sub     b
            sbc     a,b
            nop
            sub     e
            nop
            xor     h
            DB      $dd,$ef
            sub     b
            dec     d
            ld      (bc),a
            jp      pe,$1A03
            sub     c
            and     a
            djnz    $9196
            nop
            rst     $08
            sbc     a,b
            nop
            adc     a,e
            dec     b
            ld      a,e
            dec     (hl)
            xor     (hl)
            add     hl,hl
            halt
            nop
            inc     h
            call    $9809
            nop
            ld      l,l
            nop
            xor     e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            jp      nz,$6D8B
            nop
            ld      d,e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            add     hl,de
            sub     c
            ld      l,l
            nop
            ld      l,c
            exx
            rst     $30
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            or      (hl)
            DB      $dd,$f7
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            cp      b
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            call    c,$6D90
            nop
            ld      h,a
            exx
            rst     $30
            nop
            halt
            nop
            inc     de
            ld      l,l
            nop
            and     e
            ret     nc
            rst     $30
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            and     c
            ret     nc
            rst     $30
            nop
            halt
            nop
            inc     bc
            ld      l,l
            nop
            and     l
            ret     nc
            rst     $30
            nop
            ld      h,c
            nop
            rst     $08
            inc     c
            dec     hl
            cp      h
            ld      ($8ED4),a
            ld      l,l
            sub     b
            or      b
            sub     b
            dec     b
            inc     (hl)
            adc     a,e
            jr      z,$9207
            nop
            rst     $08
            ld      (hl),$91
            xor     h
            adc     a,h
            halt
            nop
            ex      af,af'
            halt
            nop
            ld      (bc),a
            ld      l,l
            nop
            rst     $20
            sub     b
            rlca
            scf
            and     a
            djnz    $9201
            inc     sp
            sub     (hl)
            sub     c
            ld      l,l
            nop
            ld      c,a
            exx
            jp      (hl)
            nop
            jp      pe,$BC03
            sub     c
            halt
            nop
            ld      (bc),a
            jr      z,$9204
            ld      h,c
            nop
            rst     $08
            ld      (hl),$91
            xor     h
            adc     a,h
            ld      b,(hl)
            inc     sp
            halt
            nop
            ex      af,af'
            sbc     a,(hl)
            nop
            ld      l,l
            nop
            rst     $20
            sub     b
            rlca
            scf
            ld      l,l
            nop
            add     a,b
            ld      (bc),a
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            jr      $921A
            sbc     a,b
            rrca
            call    nc,$6D8E
            sub     b
            ld      e,a
            ld      (bc),a
            sbc     a,(hl)
            nop
            jr      z,$922E
            ld      h,c
            nop
            ld      a,l
            and     l
            call    nc,$F29D
            sbc     a,l
;*******************************************************************************
; GALAXIANS_FORMATION_ALIEN_A_1
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_FORMATION_ALIEN_A_1:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $F0,$50                        ; 3 3 . . 1 1 . .
        DB      $FC,$40                        ; 3 3 3 . 1 . . .
        DB      $0C,$40                        ; . . 3 . 1 . . .
        DB      $0D,$50                        ; . . 3 1 1 1 . .
        DB      $15,$75                        ; . 1 1 1 1 3 1 1
        DB      $55,$55                        ; 1 1 1 1 1 1 1 1
        DB      $15,$75                        ; . 1 1 1 1 3 1 1
        DB      $0D,$50                        ; . . 3 1 1 1 . .
        DB      $0C,$40                        ; . . 3 . 1 . . .
        DB      $FC,$40                        ; 3 3 3 . 1 . . .
        DB      $F0,$50                        ; 3 3 . . 1 1 . .

;*******************************************************************************
; GALAXIANS_FORMATION_ALIEN_A_2
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_FORMATION_ALIEN_A_2:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $0F,$15                        ; . . 3 3 . 1 1 1
        DB      $0C,$50                        ; . . 3 . 1 1 . .
        DB      $0C,$40                        ; . . 3 . 1 . . .
        DB      $0D,$50                        ; . . 3 1 1 1 . .
        DB      $15,$D4                        ; . 1 1 1 3 1 1 .
        DB      $55,$50                        ; 1 1 1 1 1 1 . .
        DB      $15,$D4                        ; . 1 1 1 3 1 1 .
        DB      $0D,$50                        ; . . 3 1 1 1 . .
        DB      $0C,$40                        ; . . 3 . 1 . . .
        DB      $0C,$50                        ; . . 3 . 1 1 . .
        DB      $0F,$15                        ; . . 3 3 . 1 1 1

;*******************************************************************************
; GALAXIANS_FORMATION_ALIEN_B_1
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_FORMATION_ALIEN_B_1:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $50,$A0                        ; 1 1 . . 2 2 . .
        DB      $54,$80                        ; 1 1 1 . 2 . . .
        DB      $14,$80                        ; . 1 1 . 2 . . .
        DB      $06,$A0                        ; . . 1 2 2 2 . .
        DB      $2A,$6A                        ; . 2 2 2 1 2 2 2
        DB      $AA,$A0                        ; 2 2 2 2 2 2 . .
        DB      $2A,$6A                        ; . 2 2 2 1 2 2 2
        DB      $06,$A0                        ; . . 1 2 2 2 . .
        DB      $14,$80                        ; . 1 1 . 2 . . .
        DB      $54,$80                        ; 1 1 1 . 2 . . .
        DB      $50,$A0                        ; 1 1 . . 2 2 . .

;*******************************************************************************
; GALAXIANS_FORMATION_ALIEN_B_2
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_FORMATION_ALIEN_B_2:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $05,$2A                        ; . . 1 1 . 2 2 2
        DB      $04,$A0                        ; . . 1 . 2 2 . .
        DB      $04,$80                        ; . . 1 . 2 . . .
        DB      $06,$A0                        ; . . 1 2 2 2 . .
        DB      $2A,$68                        ; . 2 2 2 1 2 2 .
        DB      $AA,$A0                        ; 2 2 2 2 2 2 . .
        DB      $2A,$68                        ; . 2 2 2 1 2 2 .
        DB      $06,$A0                        ; . . 1 2 2 2 . .
        DB      $04,$80                        ; . . 1 . 2 . . .
        DB      $04,$A0                        ; . . 1 . 2 2 . .
        DB      $05,$2A                        ; . . 1 1 . 2 2 2

;*******************************************************************************
; GALAXIANS_FORMATION_ALIEN_C_1
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_FORMATION_ALIEN_C_1:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $A0,$F0                        ; 2 2 . . 3 3 . .
        DB      $A8,$C0                        ; 2 2 2 . 3 . . .
        DB      $28,$C0                        ; . 2 2 . 3 . . .
        DB      $0B,$F0                        ; . . 2 3 3 3 . .
        DB      $3F,$BF                        ; . 3 3 3 2 3 3 3
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .
        DB      $3F,$BF                        ; . 3 3 3 2 3 3 3
        DB      $0B,$F0                        ; . . 2 3 3 3 . .
        DB      $28,$C0                        ; . 2 2 . 3 . . .
        DB      $A8,$C0                        ; 2 2 2 . 3 . . .
        DB      $A0,$F0                        ; 2 2 . . 3 3 . .

;*******************************************************************************
; GALAXIANS_FORMATION_ALIEN_C_2
; 2 bytes/row = 8 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_FORMATION_ALIEN_C_2:
        DB      $02,$0B                        ; width bytes, height rows
        DB      $0A,$3F                        ; . . 2 2 . 3 3 3
        DB      $08,$F0                        ; . . 2 . 3 3 . .
        DB      $08,$C0                        ; . . 2 . 3 . . .
        DB      $0B,$F0                        ; . . 2 3 3 3 . .
        DB      $3F,$BC                        ; . 3 3 3 2 3 3 .
        DB      $FF,$F0                        ; 3 3 3 3 3 3 . .
        DB      $3F,$BC                        ; . 3 3 3 2 3 3 .
        DB      $0B,$F0                        ; . . 2 3 3 3 . .
        DB      $08,$C0                        ; . . 2 . 3 . . .
        DB      $08,$F0                        ; . . 2 . 3 3 . .
        DB      $0A,$3F                        ; . . 2 2 . 3 3 3

;*******************************************************************************
; GALAXIANS_COMMON_SHIP
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_COMMON_SHIP:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $00,$2A,$A0                    ; . . . . . 2 2 2 2 2 . .
        DB      $00,$A5,$00                    ; . . . . 2 2 1 1 . . . .
        DB      $02,$97,$00                    ; . . . 2 2 1 1 3 . . . .
        DB      $0A,$57,$C0                    ; . . 2 2 1 1 1 3 3 . . .
        DB      $00,$15,$F0                    ; . . . . . 1 1 1 3 3 . .
        DB      $55,$5F,$FC                    ; 1 1 1 1 1 1 3 3 3 3 3 .
        DB      $00,$15,$F0                    ; . . . . . 1 1 1 3 3 . .
        DB      $0A,$57,$C0                    ; . . 2 2 1 1 1 3 3 . . .
        DB      $02,$97,$00                    ; . . . 2 2 1 1 3 . . . .
        DB      $00,$A5,$00                    ; . . . . 2 2 1 1 . . . .
        DB      $00,$2A,$A0                    ; . . . . . 2 2 2 2 2 . .

;*******************************************************************************
; GALAXIANS_WHITE_RED_DIVE_1
; 3 bytes/row = 12 pixels wide, 12 rows
;*******************************************************************************
GALAXIANS_WHITE_RED_DIVE_1:
        DB      $03,$0C                        ; width bytes, height rows
        DB      $03,$C0,$00                    ; . . . 3 3 . . . . . . .
        DB      $03,$00,$00                    ; . . . 3 . . . . . . . .
        DB      $03,$0C,$00                    ; . . . 3 . . 3 . . . . .
        DB      $A3,$F0,$00                    ; 2 2 . 3 3 3 . . . . . .
        DB      $AB,$BC,$C0                    ; 2 2 2 3 2 3 3 . 3 . . .
        DB      $8B,$FF,$00                    ; 2 . 2 3 3 3 3 3 . . . .
        DB      $03,$FB,$00                    ; . . . 3 3 3 2 3 . . . .
        DB      $03,$FF,$00                    ; . . . 3 3 3 3 3 . . . .
        DB      $00,$2F,$CC                    ; . . . . . 2 3 3 3 . 3 .
        DB      $00,$2B,$3C                    ; . . . . . 2 2 3 . 3 3 .
        DB      $00,$28,$3C                    ; . . . . . 2 2 . . 3 3 .
        DB      $00,$20,$00                    ; . . . . . 2 . . . . . .

;*******************************************************************************
; GALAXIANS_WHITE_RED_DIVE_2
; 3 bytes/row = 12 pixels wide, 12 rows
;*******************************************************************************
GALAXIANS_WHITE_RED_DIVE_2:
        DB      $03,$0C                        ; width bytes, height rows
        DB      $03,$00,$00                    ; . . . 3 . . . . . . . .
        DB      $0C,$00,$00                    ; . . 3 . . . . . . . . .
        DB      $03,$03,$00                    ; . . . 3 . . . 3 . . . .
        DB      $00,$FC,$00                    ; . . . . 3 3 3 . . . . .
        DB      $28,$EF,$30                    ; . 2 2 . 3 2 3 3 . 3 . .
        DB      $AA,$FF,$C0                    ; 2 2 2 2 3 3 3 3 3 . . .
        DB      $03,$FE,$C0                    ; . . . 3 3 3 3 2 3 . . .
        DB      $03,$FF,$C0                    ; . . . 3 3 3 3 3 3 . . .
        DB      $03,$F8,$33                    ; . . . 3 3 3 2 . . 3 . 3
        DB      $00,$0A,$0C                    ; . . . . . . 2 2 . . 3 .
        DB      $00,$0A,$00                    ; . . . . . . 2 2 . . . .
        DB      $00,$08,$00                    ; . . . . . . 2 . . . . .

;*******************************************************************************
; GALAXIANS_WHITE_RED_DIVE_3
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_WHITE_RED_DIVE_3:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $3C,$00,$00                    ; . 3 3 . . . . . . . . .
        DB      $30,$00,$00                    ; . 3 . . . . . . . . . .
        DB      $0C,$03,$00                    ; . . 3 . . . . 3 . . . .
        DB      $0F,$FC,$00                    ; . . 3 3 3 3 3 . . . . .
        DB      $2B,$EF,$30                    ; . 2 2 3 3 2 3 3 . 3 . .
        DB      $AA,$FF,$C0                    ; 2 2 2 2 3 3 3 3 3 . . .
        DB      $00,$FE,$C3                    ; . . . . 3 3 3 2 3 . . 3
        DB      $00,$FF,$FF                    ; . . . . 3 3 3 3 3 3 3 3
        DB      $00,$0A,$00                    ; . . . . . . 2 2 . . . .
        DB      $00,$02,$80                    ; . . . . . . . 2 2 . . .
        DB      $00,$0A,$80                    ; . . . . . . 2 2 2 . . .

;*******************************************************************************
; GALAXIANS_WHITE_RED_DIVE_4
; 3 bytes/row = 12 pixels wide, 8 rows
;*******************************************************************************
GALAXIANS_WHITE_RED_DIVE_4:
        DB      $03,$08                        ; width bytes, height rows
        DB      $00,$33,$00                    ; . . . . . 3 . 3 . . . .
        DB      $00,$33,$00                    ; . . . . . 3 . 3 . . . .
        DB      $30,$FF,$C3                    ; . 3 . . 3 3 3 3 3 . . 3
        DB      $3F,$EE,$FF                    ; . 3 3 3 3 2 3 2 3 3 3 3
        DB      $00,$FF,$C0                    ; . . . . 3 3 3 3 3 . . .
        DB      $0A,$BF,$A8                    ; . . 2 2 2 3 3 3 2 2 2 .
        DB      $2A,$3F,$2A                    ; . 2 2 2 . 3 3 3 . 2 2 2
        DB      $28,$0C,$0A                    ; . 2 2 . . . 3 . . . 2 2

;*******************************************************************************
; GALAXIANS_RED_GREEN_DIVE_1
; 3 bytes/row = 12 pixels wide, 12 rows
;*******************************************************************************
GALAXIANS_RED_GREEN_DIVE_1:
        DB      $03,$0C                        ; width bytes, height rows
        DB      $02,$80,$00                    ; . . . 2 2 . . . . . . .
        DB      $02,$00,$00                    ; . . . 2 . . . . . . . .
        DB      $02,$08,$00                    ; . . . 2 . . 2 . . . . .
        DB      $52,$A0,$00                    ; 1 1 . 2 2 2 . . . . . .
        DB      $56,$68,$80                    ; 1 1 1 2 1 2 2 . 2 . . .
        DB      $46,$AA,$00                    ; 1 . 1 2 2 2 2 2 . . . .
        DB      $02,$A6,$00                    ; . . . 2 2 2 1 2 . . . .
        DB      $02,$AA,$00                    ; . . . 2 2 2 2 2 . . . .
        DB      $00,$2A,$88                    ; . . . . . 2 2 2 2 . 2 .
        DB      $00,$2A,$28                    ; . . . . . 2 2 2 . 2 2 .
        DB      $00,$28,$28                    ; . . . . . 2 2 . . 2 2 .
        DB      $00,$20,$00                    ; . . . . . 2 . . . . . .

;*******************************************************************************
; GALAXIANS_RED_GREEN_DIVE_2
; 3 bytes/row = 12 pixels wide, 12 rows
;*******************************************************************************
GALAXIANS_RED_GREEN_DIVE_2:
        DB      $03,$0C                        ; width bytes, height rows
        DB      $02,$00,$00                    ; . . . 2 . . . . . . . .
        DB      $08,$00,$00                    ; . . 2 . . . . . . . . .
        DB      $02,$02,$00                    ; . . . 2 . . . 2 . . . .
        DB      $00,$A8,$00                    ; . . . . 2 2 2 . . . . .
        DB      $14,$9A,$20                    ; . 1 1 . 2 1 2 2 . 2 . .
        DB      $55,$AA,$80                    ; 1 1 1 1 2 2 2 2 2 . . .
        DB      $02,$A9,$80                    ; . . . 2 2 2 2 1 2 . . .
        DB      $02,$AA,$80                    ; . . . 2 2 2 2 2 2 . . .
        DB      $02,$A4,$22                    ; . . . 2 2 2 1 . . 2 . 2
        DB      $00,$05,$08                    ; . . . . . . 1 1 . . 2 .
        DB      $00,$05,$00                    ; . . . . . . 1 1 . . . .
        DB      $00,$04,$00                    ; . . . . . . 1 . . . . .

;*******************************************************************************
; GALAXIANS_RED_GREEN_DIVE_3
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_RED_GREEN_DIVE_3:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $28,$00,$00                    ; . 2 2 . . . . . . . . .
        DB      $20,$00,$00                    ; . 2 . . . . . . . . . .
        DB      $08,$02,$00                    ; . . 2 . . . . 2 . . . .
        DB      $0A,$A8,$00                    ; . . 2 2 2 2 2 . . . . .
        DB      $16,$9A,$20                    ; . 1 1 2 2 1 2 2 . 2 . .
        DB      $55,$AA,$80                    ; 1 1 1 1 2 2 2 2 2 . . .
        DB      $00,$A9,$82                    ; . . . . 2 2 2 1 2 . . 2
        DB      $00,$AA,$AA                    ; . . . . 2 2 2 2 2 2 2 2
        DB      $00,$05,$00                    ; . . . . . . 1 1 . . . .
        DB      $00,$01,$40                    ; . . . . . . . 1 1 . . .
        DB      $00,$05,$40                    ; . . . . . . 1 1 1 . . .

;*******************************************************************************
; GALAXIANS_RED_GREEN_DIVE_4
; 3 bytes/row = 12 pixels wide, 8 rows
;*******************************************************************************
GALAXIANS_RED_GREEN_DIVE_4:
        DB      $03,$08                        ; width bytes, height rows
        DB      $00,$22,$00                    ; . . . . . 2 . 2 . . . .
        DB      $00,$22,$00                    ; . . . . . 2 . 2 . . . .
        DB      $20,$AA,$82                    ; . 2 . . 2 2 2 2 2 . . 2
        DB      $2A,$99,$AA                    ; . 2 2 2 2 1 2 1 2 2 2 2
        DB      $00,$AA,$80                    ; . . . . 2 2 2 2 2 . . .
        DB      $05,$6A,$54                    ; . . 1 1 1 2 2 2 1 1 1 .
        DB      $15,$2A,$15                    ; . 1 1 1 . 2 2 2 . 1 1 1
        DB      $14,$08,$05                    ; . 1 1 . . . 2 . . . 1 1

;*******************************************************************************
; GALAXIANS_WHITE_GREEN_DIVE_1
; 3 bytes/row = 12 pixels wide, 12 rows
;*******************************************************************************
GALAXIANS_WHITE_GREEN_DIVE_1:
        DB      $03,$0C                        ; width bytes, height rows
        DB      $01,$40,$00                    ; . . . 1 1 . . . . . . .
        DB      $01,$00,$00                    ; . . . 1 . . . . . . . .
        DB      $01,$04,$00                    ; . . . 1 . . 1 . . . . .
        DB      $F1,$50,$00                    ; 3 3 . 1 1 1 . . . . . .
        DB      $FD,$D4,$40                    ; 3 3 3 1 3 1 1 . 1 . . .
        DB      $CD,$55,$00                    ; 3 . 3 1 1 1 1 1 . . . .
        DB      $01,$5D,$00                    ; . . . 1 1 1 3 1 . . . .
        DB      $01,$55,$00                    ; . . . 1 1 1 1 1 . . . .
        DB      $00,$15,$44                    ; . . . . . 1 1 1 1 . 1 .
        DB      $00,$15,$14                    ; . . . . . 1 1 1 . 1 1 .
        DB      $00,$14,$14                    ; . . . . . 1 1 . . 1 1 .
        DB      $00,$10,$00                    ; . . . . . 1 . . . . . .

;*******************************************************************************
; GALAXIANS_WHITE_GREEN_DIVE_2
; 3 bytes/row = 12 pixels wide, 12 rows
;*******************************************************************************
GALAXIANS_WHITE_GREEN_DIVE_2:
        DB      $03,$0C                        ; width bytes, height rows
        DB      $01,$00,$00                    ; . . . 1 . . . . . . . .
        DB      $04,$00,$00                    ; . . 1 . . . . . . . . .
        DB      $01,$01,$00                    ; . . . 1 . . . 1 . . . .
        DB      $00,$54,$00                    ; . . . . 1 1 1 . . . . .
        DB      $3C,$75,$10                    ; . 3 3 . 1 3 1 1 . 1 . .
        DB      $FF,$55,$40                    ; 3 3 3 3 1 1 1 1 1 . . .
        DB      $01,$57,$40                    ; . . . 1 1 1 1 3 1 . . .
        DB      $01,$55,$40                    ; . . . 1 1 1 1 1 1 . . .
        DB      $01,$5C,$11                    ; . . . 1 1 1 3 . . 1 . 1
        DB      $00,$0F,$04                    ; . . . . . . 3 3 . . 1 .
        DB      $00,$0F,$00                    ; . . . . . . 3 3 . . . .
        DB      $00,$0C,$00                    ; . . . . . . 3 . . . . .

;*******************************************************************************
; GALAXIANS_WHITE_GREEN_DIVE_3
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_WHITE_GREEN_DIVE_3:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $14,$00,$00                    ; . 1 1 . . . . . . . . .
        DB      $10,$00,$00                    ; . 1 . . . . . . . . . .
        DB      $04,$01,$00                    ; . . 1 . . . . 1 . . . .
        DB      $05,$54,$00                    ; . . 1 1 1 1 1 . . . . .
        DB      $3D,$75,$10                    ; . 3 3 1 1 3 1 1 . 1 . .
        DB      $FF,$55,$40                    ; 3 3 3 3 1 1 1 1 1 . . .
        DB      $00,$57,$41                    ; . . . . 1 1 1 3 1 . . 1
        DB      $00,$55,$55                    ; . . . . 1 1 1 1 1 1 1 1
        DB      $00,$0F,$00                    ; . . . . . . 3 3 . . . .
        DB      $00,$03,$C0                    ; . . . . . . . 3 3 . . .
        DB      $00,$0F,$C0                    ; . . . . . . 3 3 3 . . .

;*******************************************************************************
; GALAXIANS_WHITE_GREEN_DIVE_4
; 3 bytes/row = 12 pixels wide, 8 rows
;*******************************************************************************
GALAXIANS_WHITE_GREEN_DIVE_4:
        DB      $03,$08                        ; width bytes, height rows
        DB      $00,$11,$00                    ; . . . . . 1 . 1 . . . .
        DB      $00,$11,$00                    ; . . . . . 1 . 1 . . . .
        DB      $10,$55,$41                    ; . 1 . . 1 1 1 1 1 . . 1
        DB      $15,$77,$55                    ; . 1 1 1 1 3 1 3 1 1 1 1
        DB      $00,$55,$40                    ; . . . . 1 1 1 1 1 . . .
        DB      $0F,$D5,$FC                    ; . . 3 3 3 1 1 1 3 3 3 .
        DB      $3F,$15,$3F                    ; . 3 3 3 . 1 1 1 . 3 3 3
        DB      $3C,$04,$0F                    ; . 3 3 . . . 1 . . . 3 3

;*******************************************************************************
; GALAXIANS_SHIELD_SHIP_1
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_SHIELD_SHIP_1:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $00,$A8,$00                    ; . . . . 2 2 2 . . . . .
        DB      $0A,$80,$00                    ; . . 2 2 2 . . . . . . .
        DB      $09,$5C,$00                    ; . . 2 1 1 1 3 . . . . .
        DB      $25,$5F,$C0                    ; . 2 1 1 1 1 3 3 3 . . .
        DB      $25,$57,$F0                    ; . 2 1 1 1 1 1 3 3 3 . .
        DB      $00,$5D,$C0                    ; . . . . 1 1 3 1 3 . . .
        DB      $05,$55,$C0                    ; . . 1 1 1 1 1 1 3 . . .
        DB      $14,$15,$C0                    ; . 1 1 . . 1 1 1 3 . . .
        DB      $40,$15,$08                    ; 1 . . . . 1 1 1 . . 2 .
        DB      $02,$A5,$A0                    ; . . . 2 2 2 1 1 2 2 . .
        DB      $00,$2A,$00                    ; . . . . . 2 2 2 . . . .

;*******************************************************************************
; GALAXIANS_SHIELD_SHIP_2
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_SHIELD_SHIP_2:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $02,$00,$00                    ; . . . 2 . . . . . . . .
        DB      $08,$00,$00                    ; . . 2 . . . . . . . . .
        DB      $24,$00,$00                    ; . 2 1 . . . . . . . . .
        DB      $97,$FF,$00                    ; 2 1 1 3 3 3 3 3 . . . .
        DB      $95,$5F,$00                    ; 2 1 1 1 1 1 3 3 . . . .
        DB      $95,$77,$00                    ; 2 1 1 1 1 3 1 3 . . . .
        DB      $91,$17,$00                    ; 2 1 . 1 . 1 1 3 . . . .
        DB      $81,$57,$08                    ; 2 . . 1 1 1 1 3 . . 2 .
        DB      $04,$15,$20                    ; . . 1 . . 1 1 1 . 2 . .
        DB      $10,$55,$80                    ; . 1 . . 1 1 1 1 2 . . .
        DB      $42,$AA,$00                    ; 1 . . 2 2 2 2 2 . . . .

;*******************************************************************************
; GALAXIANS_SHIELD_SHIP_3
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_SHIELD_SHIP_3:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $08,$00,$00                    ; . . 2 . . . . . . . . .
        DB      $20,$0C,$00                    ; . 2 . . . . 3 . . . . .
        DB      $23,$FF,$00                    ; . 2 . 3 3 3 3 3 . . . .
        DB      $95,$5F,$00                    ; 2 1 1 1 1 1 3 3 . . . .
        DB      $95,$77,$C8                    ; 2 1 1 1 1 3 1 3 3 . 2 .
        DB      $A5,$55,$48                    ; 2 2 1 1 1 1 1 1 1 . 2 .
        DB      $20,$55,$68                    ; . 2 . . 1 1 1 1 1 2 2 .
        DB      $20,$45,$60                    ; . 2 . . 1 . 1 1 1 2 . .
        DB      $01,$45,$A0                    ; . . . 1 1 . 1 1 2 2 . .
        DB      $01,$0A,$00                    ; . . . 1 . . 2 2 . . . .
        DB      $04,$00,$00                    ; . . 1 . . . . . . . . .

;*******************************************************************************
; GALAXIANS_SHIELD_SHIP_4
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
GALAXIANS_SHIELD_SHIP_4:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $00,$30,$00                    ; . . . . . 3 . . . . . .
        DB      $80,$FC,$08                    ; 2 . . . 3 3 3 . . . 2 .
        DB      $83,$FF,$08                    ; 2 . . 3 3 3 3 3 . . 2 .
        DB      $9F,$77,$D8                    ; 2 1 3 3 1 3 1 3 3 1 2 .
        DB      $95,$75,$58                    ; 2 1 1 1 1 3 1 1 1 1 2 .
        DB      $A5,$55,$68                    ; 2 2 1 1 1 1 1 1 1 2 2 .
        DB      $29,$11,$A0                    ; . 2 2 1 . 1 . 1 2 2 . .
        DB      $0A,$12,$80                    ; . . 2 2 . 1 . 2 2 . . .
        DB      $02,$12,$00                    ; . . . 2 . 1 . 2 . . . .
        DB      $00,$10,$00                    ; . . . . . 1 . . . . . .
        DB      $00,$10,$00                    ; . . . . . 1 . . . . . .

            rst     $38
            sub     c
            rst     $38
            sub     c
            cpl
            sub     d
            ld      e,a
            sub     d
            adc     a,a
            sub     d
            nop
            nop
            nop
            nop
            nop
            nop
            rla
            sub     d
            rla
            sub     d
            ld      b,a
            sub     d
            ld      (hl),a
            sub     d
            adc     a,a
            sub     d
            rst     $08
            ld      l,l
            nop
            dec     sp
            ret     nc
            ret     p
            nop
            ld      l,l
            nop
            dec     sp
            ret     nc
            ld      l,a
            ld      d,$24
            dec     (hl)
            ld      c,(hl)
            inc     d
            sbc     a,b
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            adc     a,a
            sub     d
            halt
            nop
            jr      nz,$94B9
            ld      b,$98
            nop
            ld      l,l
            nop
            nop
            ld      (bc),a
            ld      l,l
            nop
            adc     a,a
            sub     d
            halt
            nop
            jr      nz,$94C8
            ld      b,$76
            nop
            ex      af,af'
            halt
            nop
            dec     c
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      ($98DE),a
            nop
            sbc     a,b
            nop
            ld      d,(hl)
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      ($76DE),a
            nop
            inc     b
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            nop
            djnz    $95B3
            nop
            nop
            djnz    $95B7
            nop
            rst     $38
            sub     c
            halt
            nop
            jr      nz,$94FA
            ld      b,$6D
            nop
            nop
            djnz    $95C4
            nop
            nop
            ld      (de),a
            ld      l,l
            nop
            rla
            sub     d
            halt
            nop
            jr      nz,$950B
            ld      b,$76
            nop
            ld      (bc),a
            halt
            nop
            dec     c
            sbc     a,b
            nop
            adc     a,b
            nop
            xor     b
            DB      $dd, $6d
            nop
            nop
            djnz    $95E1
            nop
            nop
            djnz    $95CE
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            xor     b
            DB      $dd, $a4
            nop
            sbc     a,b
            nop
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            sbc     a,(hl)
            nop
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            nop
            jr      nz,$9603
            nop
            nop
            djnz    $9607
            nop
            cpl
            sub     d
            halt
            nop
            jr      nz,$954A
            ld      b,$6D
            nop
            nop
            jr      nz,$9614
            nop
            nop
            ld      (de),a
            ld      l,l
            nop
            ld      b,a
            sub     d
            halt
            nop
            jr      nz,$955B
            ld      b,$76
            nop
            ld      (bc),a
            halt
            nop
            dec     c
            sbc     a,b
            nop
            adc     a,b
            nop
            sub     $DD
            ld      l,l
            nop
            nop
            jr      nz,$9631
            nop
            nop
            djnz    $961E
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            sub     $DD
            halt
            nop
            ld      (bc),a
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            nop
            jr      nc,$964A
            nop
            nop
            djnz    $964E
            nop
            ld      e,a
            sub     d
            halt
            nop
            jr      nz,$9591
            ld      b,$6D
            nop
            nop
            jr      nc,$965B
            nop
            nop
            ld      (de),a
            ld      l,l
            nop
            ld      (hl),a
            sub     d
            halt
            nop
            jr      nz,$95A2
            ld      b,$76
            nop
            ld      (bc),a
            halt
            nop
            dec     c
            sbc     a,b
            nop
            adc     a,b
            nop
            inc     b
            sbc     a,$6D
            nop
            nop
            jr      nc,$9678
            nop
            nop
            djnz    $9665
            rlca
            sbc     a,b
            nop
            adc     a,b
            nop
            inc     b
            sbc     a,$76
            nop
            inc     bc
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            dec     sp
            ret     nc
            ld      h,b
            ld      d,$24
            dec     (hl)
            ld      h,c
            nop
            ld      hl,($D9B5)
            ld      de,($D9B7)
            bit     7,d
            jp      nz,$963C
            ld      bc,($D9BF)
            jp      $9640
            ld      bc,($D9BD)
            call    $2E95
            jp      nz,$964D
            ld      ($D9B7),de
            jp      $9651
            add     hl,de
            ld      ($D9B5),hl
            call    $2FA4
            ret
            ld      a,(COCKTAIL)
            and     a
            jp      z,$9661
            ld      a,$60
            jp      $9663
            ld      a,$20
            out     ($0C),a
            ret
            call    $9655
            ld      a,(ix+$07)
            ld      (ix+$07),$00
            push    af
            ld      hl,$DE70
            ld      a,($DE6E)
            push    af
            ld      c,(hl)
            ld      a,c
            and     a
            jr      z,$96C4
            xor     $55
            ld      (hl),a
            ld      de,$0005
            add     hl,de
            ld      d,(hl)
            dec     hl
            ld      e,(hl)
            ld      a,c
            ld      (de),a
            cp      $05
            jp      nz,$9695
            ld      a,$50
            ld      (de),a
            inc     hl
            inc     hl
            jr      $96C8
            dec     hl
            ld      b,(hl)
            dec     hl
            ld      c,(hl)
            ex      de,hl
            ld      a,(COCKTAIL)
            and     a
            jp      nz,$96A5
            add     hl,bc
            jp      $96A7
            sbc     hl,bc
            ex      de,hl
            dec     hl
            dec     (hl)
            ld      a,(hl)
            cp      $03
            jr      c,$96B3
            bit     6,d
            jr      z,$96B8
            dec     hl
            ld      (hl),$00
            jr      $96C4
            inc     hl
            inc     hl
            inc     hl
            ld      a,$05
            ld      (de),a
            ld      (hl),e
            inc     hl
            ld      (hl),d
            inc     hl
            jr      $96C8
            ld      de,$0006
            add     hl,de
            pop     af
            dec     a
            jr      nz,$9677
            pop     af
            dec     a
            jr      nz,$9670
            ret
            bit     0,(ix+$00)
            ret     nz
            ld      a,($DE6E)
            ld      b,a
            push    hl
            ld      hl,$DE70
            ld      de,$0006
            ld      a,(hl)
            and     a
            jr      z,$96EA
            add     hl,de
            djnz    $96E1
            pop     hl
            ret
            ld      (hl),$05
            inc     hl
            ld      a,(ix+$0e)
            ld      (hl),a
            inc     hl
            ld      a,(ix+$14)
            srl     a
            srl     a
            ld      c,a
            ld      a,r
            and     $01
            jp      nz,$970A
            ld      a,r
            and     $7F
            add     a,$18
            jp      $970D
            ld      a,($D97F)
            srl     a
            srl     a
            sub     c
            jp      p,$9726
            cp      $FD
            jp      c,$9720
            ld      de,$FFFF
            jp      $9723
            ld      de,$FFAF
            jp      $9734
            cp      $03
            jp      nc,$9731
            ld      de,$FFFF
            jp      $9734
            ld      de,$004F
            ld      (hl),e
            inc     hl
            ld      (hl),d
            inc     hl
            ex      de,hl
            ld      l,(ix+$19)
            ld      h,(ix+$1a)
            ld      bc,L01E0
            ld      a,(COCKTAIL)
            rrca
            xor     (ix+$1b)
            jp      m,$9750
            add     hl,bc
            jp      $9753
            xor     a
            sbc     hl,bc
            call    $9655
            ld      (hl),$05
            ex      de,hl
            ld      (hl),e
            inc     hl
            ld      (hl),d
            pop     hl
            ret
            ld      a,($DE6F)
            and     a
            ret     z
            call    $389B
            call    $2E67
            ret
            inc     d
            ld      e,(hl)
            sub     a
            ld      c,$05
            rla
            dec     b
            or      b
            ld      a,(bc)
            dec     a
            inc     sp
            inc     b
            ld      h,(hl)
            sub     (hl)
            nop
            ld      b,$24
            ld      b,$02
            inc     h
            pop     de
            sub     (hl)
            ld      b,$12
            inc     h
            pop     de
            sub     (hl)
            djnz    $979A
            ld      a,$12
            dec     h
            ld      de,$1020
            djnz    $9794
            ex      af,af'
            jr      nz,$9791
            inc     bc
            ld      a,(bc)
            ld      bc,$1817
            inc     d
            nop
            ld      d,$99
            dec     d
            add     hl,hl
            inc     bc
            inc     d
            ld      b,d
            ex      af,af'
            ld      a,(bc)
            ld      bc,$0503
            jr      nz,$97D7
            ld      bc,$1403
            ld      b,h
            ld      a,(bc)
            ld      bc,$0503
            inc     e
            ld      b,b
            ld      bc,$1403
            ld      c,d
            ld      a,(bc)
            ld      (bc),a
            inc     bc
            dec     b
            jr      $97D7
            rst     $38
            inc     b
            inc     bc
GASCORE     EQU     $9786               ; GALAXIANS 0157: attack score

; ----> play_galaxian_attack_sound  Self-contained GA sound launcher.
;                                    BMUSIC preserves a priority score on processor 2.
play_galaxian_attack_sound:
            ld      hl,GASCORE
            ld      iy,$D0E1
            jp      bmusic
            di
            ld      a,($D9AB)
            and     a
            jr      nz,$9821
            push    hl
            push    bc
            ld      de,$01A2
            push    de
            call    $1526
            push    hl
            push    iy
            ld      iy,$0000
            add     iy,sp
            ld      l,(iy+$02)
            ld      h,(iy+$03)
            push    hl
            pop     ix
            call    $1F3E
            ld      a,(iy+$07)
            ld      (ix+$31),a
            ld      c,(iy+$06)
            call    $2EA4
            xor     (hl)
            ld      (hl),a
            ei
            push    iy
            call    $2F67
            pop     iy
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      (ix+$0d),e
            ld      (ix+$0e),d
            call    $1F74
            call    $1F63
            pop     iy
            pop     bc
            pop     bc
            pop     bc
            pop     hl
            call    $3099
            jp      $97BE
            ei
            ret
            push    ix
            pop     hl
            push    iy
            pop     de
            exx
            pop     bc
            pop     hl
            call    $97C8
            exx
            push    de
            pop     iy
            push    hl
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            push    hl
            ld      a,(ix+$14)
            sub     (ix+$31)
            srl     a
            srl     a
            ld      c,a
            ld      a,($D97F)
            srl     a
            srl     a
            sub     c
            sra     a
            sra     a
            ld      e,a
            ld      b,(ix+$16)
            sub     b
            ld      c,a
            ld      a,e
            xor     b
            ld      a,c
            jp      p,$985F
            sra     a
            add     a,c
            ld      (ix+$17),a
            bit     7,a
            ld      a,$00
            jp      z,$986A
            cpl
            ld      (ix+$18),a
            ld      a,(ix+$17)
            call    aabs
            and     $0E
            cp      $06
            jp      c,$987C
            ld      a,$06
            ld      c,a
            ld      b,$00
            ld      l,(ix+$25)
            ld      h,(ix+$26)
            add     hl,bc
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ld      (ix+$1d),e
            ld      (ix+$1e),d
            pop     hl
            ret
            ld      e,a
            sub     d
            or      d
            sub     d
            ret     c
            sub     d
            cp      $92
            ld      hl,$2F93
            sub     d
            dec     sp
            sub     e
            ld      h,c
            sub     e
            add     a,a
            sub     e
            xor     d
            sub     e
            rst     $38
            sub     c
            call    nz,$EA93
            sub     e
            djnz    $9841
            inc     sp
            sub     h
            adc     a,a
            sub     d
            ld      c,l
            sub     h
            ld      (hl),b
            sub     h
            sub     e
            sub     h
            or      (hl)
            sub     h
            ld      a,(SKILLFACTOR)
            and     a
            jp      nz,$96D1
            ld      a,r
            rrca
            jp      c,$96D1
            ret
            ld      b,$0C
            inc     h
            ld      (hl),c
            ld      a,($B924)
            sbc     a,b
            djnz    $98F5
            cp      a
            dec     sp
            ld      b,e
            dec     sp
            ex      af,af'
            ex      (sp),hl
            dec     sp
            inc     e
            inc     b
            ex      af,af'
            rst     $00
            sbc     a,b
            ld      e,$DA
            sbc     a,b
            inc     h
            rst     $28
            dec     sp
            inc     e
            inc     b
            ex      af,af'
            rst     $00
            sbc     a,b
            ld      e,$E5
            sbc     a,b
            ld      b,$4E
            ld      a,(bc)
            ret     nc
            sbc     a,b
            inc     h
            srl     e
            ret     nc
            sbc     a,b
            ld      a,(bc)
            ld      b,e
            dec     sp
            inc     h
            jr      c,$9893
            ex      af,af'
            ld      a,l
            sub     a
            ld      b,$1E
            inc     h
            jr      c,$989B
            ld      b,$28
            inc     h
            jr      c,$98A0
            ld      b,$50
            ld      a,(bc)
            ret     p
            sbc     a,b
            ld      h,$91
            sbc     a,b
            ex      af,af'
            rst     $38
            ld      a,($F80A)
            sbc     a,b
            ld      h,$91
            sbc     a,b
            ex      af,af'
            ld      e,d
            dec     sp
            ld      a,(bc)
            ret     m
            sbc     a,b
            inc     h
            jr      c,$98BA
            ex      af,af'
            ld      a,l
            sub     a
            ld      b,$1C
            inc     h
            jr      c,$98C2
            inc     h
            pop     de
            sub     (hl)
            ld      b,$70
            ld      a,(bc)
            ret     p
            sbc     a,b
            ld      h,$9B
            sbc     a,b
            ex      af,af'
            rst     $38
            ld      a,($1F0A)
            sbc     a,c
            ld      h,$9B
            sbc     a,b
            ex      af,af'
            ld      e,d
            dec     sp
            ld      a,(bc)
            rra
            sbc     a,c
            inc     h
            jr      c,$98DF
            ex      af,af'
            ld      a,l
            sub     a
            ld      b,$14
            inc     h
            jr      c,$98E7
            ld      b,$6E
            ld      a,(bc)
            ret     p
            sbc     a,b
            ld      h,$A5
            sbc     a,b
            ex      af,af'
            rst     $38
            ld      a,($440A)
            sbc     a,c
            ld      h,$A5
            sbc     a,b
            ex      af,af'
            ld      e,d
            dec     sp
            ld      a,(bc)
            ld      b,h
            sbc     a,c
            inc     c
            ld      b,b
            nop
            nop
            nop
            ld      b,$11
            ld      a,(bc)
            ld      b,e
            dec     sp
            inc     c
            nop
            nop
            nop
            ld      bc,$0A1C
            nop
            add     a,c
            ld      ($0506),hl
            nop
            adc     a,$22
            ld      b,$05
            ld      e,$77
            sbc     a,c
            ld      a,(bc)
            ld      h,(hl)
            sbc     a,c
            ld      d,$00
            ld      c,b
            nop
            ld      b,$24
            inc     c
            nop
            nop
            nop
            nop
            ld      c,$00
            nop
            nop
            nop
            inc     b
            ld      a,(bc)
            ld      sp,$0106
            jr      nz,$999E
            cp      $00
            adc     a,$22
            ld      b,$08
            inc     b
            ld      ($2C1E),hl
            ld      bc,$9970
            inc     c
            nop
            nop
            nop
            rst     $38
            inc     e
            ld      a,(bc)
            nop
            add     a,c
            ld      ($0506),hl
            nop
            adc     a,$22
            ld      b,$05
            ld      e,$B2
            sbc     a,c
            ld      a,(bc)
            ld      h,(hl)
            sbc     a,c
            inc     h
            jr      c,$995D
            ex      af,af'
            ld      a,l
            sub     a
            ld      b,$14
            inc     h
            jr      c,$9965
            ld      b,$32
            inc     h
            jr      c,$996A
            ld      b,$50
            inc     l
            inc     bc
            add     a,a
            sbc     a,c
            inc     e
            ld      a,(bc)
            ld      b,$01
            ld      e,$DA
            sbc     a,c
            ld      a,(bc)
            ret     p
            sbc     a,b
            ld      h,$AF
            sbc     a,b
            ex      af,af'
            rst     $38
            ld      a,($C20A)
            sbc     a,c
            ld      h,$AF
            sbc     a,b
            ex      af,af'
            ld      e,d
            dec     sp
            ld      a,(bc)
            jp      nz,$5499
            sbc     a,c
            ld      d,h
            sbc     a,c
            ld      ($5D99),a
            sbc     a,c
            ld      e,l
            sbc     a,c
            dec     sp
            sbc     a,c
            inc     c
            dec     bc
            ret     p
            dec     c
            sbc     a,c
            inc     de
            nop
            dec     c
            sbc     a,c
            inc     d
            nop
            jp      po,$FF99
            inc     d
            inc     de
            nop
            ld      d,$99
            dec     de
            djnz    $9A2C
            sbc     a,c
            inc     d
            nop
            ex      de,hl
            sbc     a,c
            rst     $38
            inc     h
            inc     hl
            ret     p
            dec     c
            sbc     a,c
            dec     hl
            nop
            dec     c
            sbc     a,c
            inc     l
            nop
            jp      po,$FF99
            inc     l
            dec     hl
            nop
            ld      d,$99
            inc     sp
            djnz    $9A48
            sbc     a,c
            inc     l
            nop
            ex      de,hl
            sbc     a,c
            rst     $38
            nop
            sbc     a,d
            ld      c,$9A
            inc     e
            sbc     a,d
            ld      hl,($C59A)
            ld      c,a
            ld      a,($D94B)
            cp      $06
            jp      c,$9A71
            ld      a,($D08A)
            and     a
            jp      nz,$9A58
            ld      a,r
            and     $7F
            jp      $9A68
            dec     a
            jp      nz,$9A65
            ld      c,$00
            ld      a,r
            and     $3F
            jp      $9A68
            ld      c,$00
            xor     a
            ld      b,a
            ld      a,($D94B)
            add     a,b
            add     a,c
            ld      ($D0A3),a
            pop     bc
            ret
            push    hl
            ld      a,c
            call    $2E8E
            ld      de,($D9B5)
            add     hl,de
            ld      a,h
            pop     hl
            cp      $1E
            jr      c,$9A8D
            cp      $94
            jr      nc,$9A8D
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ex      de,hl
            or      a
            ret
            xor     a
            ret
            ld      a,c
            cp      $04
            jp      nc,$9A9F
            ld      a,($D9C1)
            dec     a
            ld      hl,$99F4
            jp      $9AA7
            ld      a,($D9C3)
            sub     $04
            ld      hl,$99FA
            add     a,c
            ld      c,a
            and     $03
            rlca
            ld      e,a
            ld      d,$00
            add     hl,de
            push    hl
            call    $2EA4
            pop     hl
            ret     z
            call    $9A73
            ret     z
            ld      b,$00
            call    $97C8
            ld      a,$10
            jp      $9A40
            ld      a,c
            rlca
            ld      c,a
            ld      b,$00
            ld      hl,$9A38
            add     hl,bc
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            ex      de,hl
            ld      a,($D9B6)
            sra     a
            add     a,(hl)
            sub     $14
            cp      $38
            ret     nc
            inc     hl
            push    hl
            ld      b,$00
            ld      c,(hl)
            push    hl
            call    $2EBD
            jp      z,$9AF5
            call    $2EA4
            jp      z,$9AF2
            inc     b
            jp      $9AF5
            pop     hl
            pop     hl
            ret
            pop     hl
            inc     hl
            inc     hl
            inc     hl
            inc     hl
            ld      a,(hl)
            inc     a
            jr      nz,$9AE0
            pop     hl
            or      b
            ret     z
            ld      a,$50
            call    $9A40
            ld      c,(hl)
            inc     hl
            ld      b,(hl)
            inc     hl
            ld      e,(hl)
            inc     hl
            ld      d,(hl)
            inc     hl
            ld      a,c
            inc     a
            ret     z
            push    hl
            push    de
            push    bc
            call    $2EA4
            pop     bc
            pop     hl
            jr      z,$9B1E
            call    $97C8
            pop     hl
            jr      $9B06
            push    ix
            push    iy
            exx
            ld      hl,($D0A3)
            ld      a,h
            or      l
            jr      nz,$9B4C
            ld      a,r
            and     $0F
            inc     a
            cp      $0D
            jp      nc,$9B41
            rrca
            and     $07
            ld      c,a
            call    $9A8F
            jp      $9B47
            sub     $0D
            ld      c,a
            call    $9AC4
            ld      a,$01
            ld      ($DE6F),a
            exx
            pop     iy
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            ld      a,($D9AB)
            and     a
            ret     nz
            ld      c,$01
            call    $2114
            jp      z,$9B8B
            res     7,(iy+$00)
            set     6,(iy+$00)
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      ($D9B1),hl
            ld      l,(iy+$0d)
            ld      h,(iy+$0e)
            ld      ($D9AF),hl
            ld      c,(iy+$29)
            bit     6,c
            jp      nz,$9B87
            call    $2EBD
            xor     (hl)
            ld      (hl),a
            ld      a,$01
            jr      $9B91
            call    $3151
            ret     z
            ld      a,$02
            ld      ($D9AB),a
            ld      a,c
            ld      ($D9AD),a
            call    $14BF
            res     7,(ix+$00)
            ret
            inc     b
            ld      (hl),l
            call    m,$044A
            ld      (hl),l
            call    m,$064A
            ld      c,$1E
            ld      c,$0E
            ld      e,$0E
            ld      b,$CF
            sbc     a,(hl)
            nop
            halt
            nop
            ex      af,af'
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            adc     a,b
            nop
            push    de
            exx
            ret     p
            nop
            adc     a,l
            ld      (bc),a
            adc     a,b
            nop
            call    $F0D9
            nop
            ld      a,e
            ld      bc,_0BRANCH
            call    nc,$B19B
            nop
            sbc     a,b
            nop
            ld      e,a
            ld      (bc),a
            ld      h,c
            nop
            rst     $08
            adc     a,e
            jr      z,$9B8C
            sbc     a,e
            jp      pe,$D903
            sbc     a,e
            and     a
            djnz    $9C45
            nop
            rst     $08
            sbc     a,b
            nop
            adc     a,e
            dec     b
            ld      a,e
            dec     (hl)
            add     ix,ix
            di
            sub     h
            xor     (hl)
            add     hl,hl
            halt
            nop
            inc     hl
            call    $6D09
            nop
            add     hl,hl
            sub     (hl)
            ld      l,l
            nop
            rst     $00
            exx
            rst     $30
            nop
            sbc     a,b
            nop
            sub     e
            nop
            sbc     a,(hl)
            DB      $dd, $6d
            nop
            push    hl
            jp      c,$00F7
            ld      l,l
            nop
            exx
            sub     h
            ld      l,l
            nop
            ret
            exx
            rst     $30
            nop
            ld      l,l
            nop
            nop
            jr      nc,$9C89
            nop
            cp      c
            exx
            rst     $30
            nop
            ld      l,l
            nop
            ld      d,e
            sbc     a,e
            ld      l,l
            nop
            ld      d,e
            exx
            rst     $30
            nop
            halt
            nop
            add     a,b
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            ld      l,l
            nop
            or      l
            exx
            jp      (hl)
            nop
            adc     a,l
            ld      (bc),a
            sub     e
            nop
            DB      $dd,$d9
            rst     $30
            nop
            ld      l,l
            nop
            cp      c
            exx
            jp      (hl)
            nop
            adc     a,l
            ld      (bc),a
            ld      hl,($9301)
            nop
            DB      $dd,$d9
            rst     $30
            nop
            halt
            nop
            ld      (bc),a
            sub     a
            inc     bc
            ld      l,l
            nop
            ret     c
            sbc     a,e
            ld      l,l
            nop
            ld      l,c
            exx
            rst     $30
            nop
            halt
            nop
            jr      $9CD0
            nop
            xor     b
            sbc     a,e
            ld      l,l
            nop
            scf
            ret     nc
            ret     p
            nop
            dec     sp
            ld      bc,$0076
            djnz    $9C7D
            ld      bc,$0076
            inc     d
            ld      h,$04
            ld      l,l
            nop
            ld      l,(hl)
            sbc     a,$FE
            nop
            sbc     a,b
            nop
            adc     a,b
            nop
            call    $76D9
            nop
            ex      af,af'
            ld      (hl),$02
            ld      l,l
            nop
            ld      c,e
            exx
            rst     $30
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            pop     bc
            exx
            rst     $30
            nop
            halt
            nop
            jr      c,$9D08
            nop
            jp      $F7D9
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            xor     e
            exx
            rst     $30
            nop
            halt
            nop
            ld      a,b
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            sbc     a,b
            nop
            adc     a,l
            ld      (bc),a
            adc     a,b
            nop
            ld      (hl),b
            sbc     a,$FE
            nop
            ld      e,a
            ld      (bc),a
            ld      l,l
            nop
            ld      l,d
            sub     a
            ld      l,l
            nop
            ld      h,a
            exx
            rst     $30
            nop
            ld      b,(hl)
            inc     sp
            ld      l,l
            nop
            ld      (hl),l
            sub     a
            sbc     a,b
            nop
            halt
            nop
            and     d
            ld      d,a
            jr      nz,$9D34
            nop
            rst     $08
            add     a,e
            jr      nc,$9CF9
            sbc     a,e
            inc     c
            dec     hl
            cp      h
            ld      ($3405),a
            adc     a,e
            jr      z,$9D43
            nop
            rst     $08
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            rst     $30
            nop
            add     a,e
            jr      nc,$9CF9
            dec     hl
            cp      h
            ld      ($288B),a
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            jp      (hl)
            nop
            call    po,$EA01
            inc     bc
            jp      pe,$619C
            nop
            rst     $08
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            rst     $30
            nop
            inc     c
            dec     hl
            cp      h
            ld      ($288B),a
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            jp      (hl)
            nop
            call    po,$EA01
            inc     bc
            rlca
            sbc     a,l
            ld      h,c
            nop
            ld      h,$A5
            sbc     a,b
            ld      a,(bc)
            ld      b,e
            dec     sp
            ld      h,$9B
            sbc     a,b
            ld      a,(bc)
            ld      b,e
            dec     sp
            ld      h,$91
            sbc     a,b
            ld      a,(bc)
            ld      b,e
            dec     sp
            ld      h,$AF
            sbc     a,b
            ld      a,(bc)
            ld      b,e
            dec     sp
            rst     $08
            ld      l,l
            nop
            add     a,(hl)
            sub     a
            ld      l,l
            djnz    $9D9C
            nop
            rst     $08
            halt
            nop
            ex      af,af'
            halt
            nop
            ld      (bc),a
            ld      l,l
            nop
            and     b
            sbc     a,e
            rlca
            scf
            and     a
            djnz    $9D7F
            sbc     a,l
            add     a,e
            jr      nc,$9D83
            sbc     a,l
            halt
            nop
            ld      a,($009E)
            ld      b,h
            ld      (bc),a
            ld      l,l
            nop
            dec     de
            sbc     a,l
            adc     a,l
            ld      (bc),a
            ld      l,l
            nop
            and     d
            ld      bc,$2057
            halt
            nop
            ex      af,af'
            sub     a
            inc     bc
            halt
            nop
            ld      ($9D00),a
            inc     sp
            sbc     a,l
            halt
            nop
            dec     sp
            halt
            nop
            ld      (bc),a
            ld      b,h
            ld      (bc),a
            ld      l,l
            nop
            ld      hl,$8D9D
            ld      (bc),a
            ld      l,l
            nop
            and     d
            ld      bc,$2057
            halt
            nop
            ex      af,af'
            sub     a
            inc     bc
            halt
            nop
            jr      z,$9D70
            sbc     a,h
            inc     sp
            sbc     a,l
            halt
            nop
            inc     (hl)
            halt
            nop
            dec     bc
            ld      b,h
            ld      (bc),a
            ld      l,l
            nop
            daa
            sbc     a,l
            adc     a,l
            ld      (bc),a
            ld      l,l
            nop
            and     d
            ld      bc,$2057
            halt
            nop
            ex      af,af'
            sub     a
            inc     bc
            halt
            nop
            ld      e,$E3
            sbc     a,h
            inc     sp
            sbc     a,l
            ld      l,l
            nop
            dec     l
            sbc     a,l
            halt
            nop
            inc     d
            ld      l,l
            nop
            and     d
            ld      bc,$2057
            ld      l,l
            nop
            dec     l
            sbc     a,l
            halt
            nop
            inc     l
            ld      l,l
            nop
            and     d
            ld      bc,$2057
            sbc     a,b
            nop
            ld      l,l
            nop
            and     e
            ret     nc
            rst     $30
            nop
            ld      h,c
            nop
            rst     $08
            push    hl
            sbc     a,e
            inc     a
            sbc     a,l
            call    nc,$6D9C
            nop
            ld      c,a
            exx
            jp      (hl)
            nop
            jp      pe,$D903
            sbc     a,l
            ld      l,l
            nop
            and     b
            sbc     a,e
            ex      (sp),hl
            ld      (hl),$76
            nop
            ld      (bc),a
            jr      z,$9E27
            ld      h,c
            nop
            rst     $08
            push    hl
            sbc     a,e
            halt
            nop
            ex      af,af'
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            adc     a,b
            nop
            call    $F0D9
            nop
            adc     a,l
            ld      (bc),a
            adc     a,b
            nop
            push    de
            exx
            cp      $00
            ld      e,a
            ld      (bc),a
            ld      (bc),a
            dec     (hl)
            halt
            nop
            ex      af,af'
            sbc     a,(hl)
            nop
            ld      l,l
            nop
            and     b
            sbc     a,e
            rlca
            scf
            ld      l,l
            nop
            nop
            inc     bc
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            jr      $9E54
            sbc     a,b
            rrca
            add     a,e
            jr      nc,$9E4B
            sbc     a,e
            ld      e,a
            ld      (bc),a
            sbc     a,(hl)
            nop
            jr      z,$9E68
            ld      h,c
            nop
            and     l
            ld      (de),a
            and     (hl)
            ld      c,e
            and     (hl)
;*******************************************************************************
; SPACE_WARP_OBJECT_A_LARGE
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
SPACE_WARP_OBJECT_A_LARGE:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $55,$55,$40                    ; 1 1 1 1 1 1 1 1 1 . . .
        DB      $00,$80,$00                    ; . . . . 2 . . . . . . .
        DB      $10,$81,$00                    ; . 1 . . 2 . . 1 . . . .
        DB      $02,$A0,$00                    ; . . . 2 2 2 . . . . . .
        DB      $0A,$E8,$00                    ; . . 2 2 3 2 2 . . . . .
        DB      $0A,$E8,$00                    ; . . 2 2 3 2 2 . . . . .
        DB      $0A,$E8,$00                    ; . . 2 2 3 2 2 . . . . .
        DB      $02,$A0,$00                    ; . . . 2 2 2 . . . . . .
        DB      $10,$81,$00                    ; . 1 . . 2 . . 1 . . . .
        DB      $00,$80,$00                    ; . . . . 2 . . . . . . .
        DB      $55,$55,$40                    ; 1 1 1 1 1 1 1 1 1 . . .

;*******************************************************************************
; SPACE_WARP_OBJECT_A_MEDIUM
; 2 bytes/row = 8 pixels wide, 9 rows
;*******************************************************************************
SPACE_WARP_OBJECT_A_MEDIUM:
        DB      $02,$09                        ; width bytes, height rows
        DB      $55,$54                        ; 1 1 1 1 1 1 1 .
        DB      $02,$00                        ; . . . 2 . . . .
        DB      $4A,$84                        ; 1 . 2 2 2 . 1 .
        DB      $0B,$80                        ; . . 2 3 2 . . .
        DB      $2B,$A0                        ; . 2 2 3 2 2 . .
        DB      $0B,$80                        ; . . 2 3 2 . . .
        DB      $4A,$84                        ; 1 . 2 2 2 . 1 .
        DB      $02,$00                        ; . . . 2 . . . .
        DB      $55,$54                        ; 1 1 1 1 1 1 1 .

;*******************************************************************************
; SPACE_WARP_OBJECT_A_SMALL
; 2 bytes/row = 8 pixels wide, 7 rows
;*******************************************************************************
SPACE_WARP_OBJECT_A_SMALL:
        DB      $02,$07                        ; width bytes, height rows
        DB      $55,$40                        ; 1 1 1 1 1 . . .
        DB      $08,$00                        ; . . 2 . . . . .
        DB      $2A,$00                        ; . 2 2 2 . . . .
        DB      $2E,$00                        ; . 2 3 2 . . . .
        DB      $2A,$00                        ; . 2 2 2 . . . .
        DB      $08,$00                        ; . . 2 . . . . .
        DB      $55,$40                        ; 1 1 1 1 1 . . .

;*******************************************************************************
; SPACE_WARP_OBJECT_A_TINY
; 1 bytes/row = 4 pixels wide, 4 rows
;*******************************************************************************
SPACE_WARP_OBJECT_A_TINY:
        DB      $01,$04                        ; width bytes, height rows
        DB      $54                            ; 1 1 1 .
        DB      $20                            ; . 2 . .
        DB      $20                            ; . 2 . .
        DB      $54                            ; 1 1 1 .

;*******************************************************************************
; SPACE_WARP_OBJECT_B_LARGE
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
SPACE_WARP_OBJECT_B_LARGE:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $A0,$00,$28                    ; 2 2 . . . . . . . 2 2 .
        DB      $A0,$00,$28                    ; 2 2 . . . . . . . 2 2 .
        DB      $04,$10,$40                    ; . . 1 . . 1 . . 1 . . .
        DB      $01,$11,$00                    ; . . . 1 . 1 . 1 . . . .
        DB      $01,$55,$00                    ; . . . 1 1 1 1 1 . . . .
        DB      $85,$FD,$48                    ; 2 . 1 1 3 3 3 1 1 . 2 .
        DB      $01,$55,$00                    ; . . . 1 1 1 1 1 . . . .
        DB      $01,$11,$00                    ; . . . 1 . 1 . 1 . . . .
        DB      $04,$10,$40                    ; . . 1 . . 1 . . 1 . . .
        DB      $A0,$00,$28                    ; 2 2 . . . . . . . 2 2 .
        DB      $A0,$00,$28                    ; 2 2 . . . . . . . 2 2 .

;*******************************************************************************
; SPACE_WARP_OBJECT_B_MEDIUM
; 3 bytes/row = 12 pixels wide, 9 rows
;*******************************************************************************
SPACE_WARP_OBJECT_B_MEDIUM:
        DB      $03,$09                        ; width bytes, height rows
        DB      $A0,$02,$80                    ; 2 2 . . . . . 2 2 . . .
        DB      $A0,$02,$80                    ; 2 2 . . . . . 2 2 . . .
        DB      $04,$44,$00                    ; . . 1 . 1 . 1 . . . . .
        DB      $01,$50,$00                    ; . . . 1 1 1 . . . . . .
        DB      $85,$D4,$80                    ; 2 . 1 1 3 1 1 . 2 . . .
        DB      $01,$50,$00                    ; . . . 1 1 1 . . . . . .
        DB      $04,$44,$00                    ; . . 1 . 1 . 1 . . . . .
        DB      $A0,$02,$80                    ; 2 2 . . . . . 2 2 . . .
        DB      $A0,$02,$80                    ; 2 2 . . . . . 2 2 . . .

;*******************************************************************************
; SPACE_WARP_OBJECT_B_TINY
; 1 bytes/row = 4 pixels wide, 3 rows
;*******************************************************************************
SPACE_WARP_OBJECT_B_TINY:
        DB      $01,$03                        ; width bytes, height rows
        DB      $88                            ; 2 . 2 .
        DB      $10                            ; . 1 . .
        DB      $88                            ; 2 . 2 .

;*******************************************************************************
; SPACE_WARP_RED_SPARKLE
; 2 bytes/row = 8 pixels wide, 7 rows
;*******************************************************************************
SPACE_WARP_RED_SPARKLE:
        DB      $02,$07                        ; width bytes, height rows
        DB      $80,$08                        ; 2 . . . . . 2 .
        DB      $11,$10                        ; . 1 . 1 . 1 . .
        DB      $05,$40                        ; . . 1 1 1 . . .
        DB      $97,$58                        ; 2 1 1 3 1 1 2 .
        DB      $05,$40                        ; . . 1 1 1 . . .
        DB      $11,$10                        ; . 1 . 1 . 1 . .
        DB      $80,$08                        ; 2 . . . . . 2 .

;*******************************************************************************
; SPACE_WARP_RED_SPARKLE_SMALL
; 2 bytes/row = 8 pixels wide, 5 rows
;*******************************************************************************
SPACE_WARP_RED_SPARKLE_SMALL:
        DB      $02,$05                        ; width bytes, height rows
        DB      $80,$80                        ; 2 . . . 2 . . .
        DB      $15,$00                        ; . 1 1 1 . . . .
        DB      $9D,$80                        ; 2 1 3 1 2 . . .
        DB      $15,$00                        ; . 1 1 1 . . . .
        DB      $80,$80                        ; 2 . . . 2 . . .

;*******************************************************************************
; SPACE_WARP_WHITE_SPARKLE_TINY_1
; 1 bytes/row = 4 pixels wide, 3 rows
;*******************************************************************************
SPACE_WARP_WHITE_SPARKLE_TINY_1:
        DB      $01,$03                        ; width bytes, height rows
        DB      $30                            ; . 3 . .
        DB      $DC                            ; 3 1 3 .
        DB      $30                            ; . 3 . .

;*******************************************************************************
; SPACE_WARP_WHITE_SPARKLE_TINY_2
; 1 bytes/row = 4 pixels wide, 4 rows
;*******************************************************************************
SPACE_WARP_WHITE_SPARKLE_TINY_2:
        DB      $01,$04                        ; width bytes, height rows
        DB      $34                            ; . 3 1 .
        DB      $D7                            ; 3 1 1 3
        DB      $57                            ; 1 1 1 3
        DB      $3C                            ; . 3 3 .

;*******************************************************************************
; SPACE_WARP_WHITE_OBJECT_SMALL
; 2 bytes/row = 8 pixels wide, 5 rows
;*******************************************************************************
SPACE_WARP_WHITE_OBJECT_SMALL:
        DB      $02,$05                        ; width bytes, height rows
        DB      $3F,$00                        ; . 3 3 3 . . . .
        DB      $D7,$40                        ; 3 1 1 3 1 . . .
        DB      $57,$40                        ; 1 1 1 3 1 . . .
        DB      $75,$C0                        ; 1 3 1 1 3 . . .
        DB      $37,$00                        ; . 3 1 3 . . . .

;*******************************************************************************
; SPACE_WARP_WHITE_OBJECT_MEDIUM
; 2 bytes/row = 8 pixels wide, 6 rows
;*******************************************************************************
SPACE_WARP_WHITE_OBJECT_MEDIUM:
        DB      $02,$06                        ; width bytes, height rows
        DB      $3F,$40                        ; . 3 3 3 1 . . .
        DB      $DF,$F0                        ; 3 1 3 3 3 3 . .
        DB      $D5,$F0                        ; 3 1 1 1 3 3 . .
        DB      $75,$70                        ; 1 3 1 1 1 3 . .
        DB      $75,$70                        ; 1 3 1 1 1 3 . .
        DB      $3F,$40                        ; . 3 3 3 1 . . .

;*******************************************************************************
; SPACE_WARP_WHITE_OBJECT_LARGE_1
; 3 bytes/row = 12 pixels wide, 9 rows
;*******************************************************************************
SPACE_WARP_WHITE_OBJECT_LARGE_1:
        DB      $03,$09                        ; width bytes, height rows
        DB      $01,$F0,$00                    ; . . . 1 3 3 . . . . . .
        DB      $0F,$D4,$00                    ; . . 3 3 3 1 1 . . . . .
        DB      $35,$FF,$00                    ; . 3 1 1 3 3 3 3 . . . .
        DB      $75,$7D,$C0                    ; 1 3 1 1 1 3 3 1 3 . . .
        DB      $F5,$DF,$40                    ; 3 3 1 1 3 1 3 3 1 . . .
        DB      $F5,$DF,$C0                    ; 3 3 1 1 3 1 3 3 3 . . .
        DB      $3D,$57,$00                    ; . 3 3 1 1 1 1 3 . . . .
        DB      $07,$7C,$00                    ; . . 1 3 1 3 3 . . . . .
        DB      $03,$50,$00                    ; . . . 3 1 1 . . . . . .

;*******************************************************************************
; SPACE_WARP_WHITE_OBJECT_LARGE_2
; 3 bytes/row = 12 pixels wide, 9 rows
;*******************************************************************************
SPACE_WARP_WHITE_OBJECT_LARGE_2:
        DB      $03,$09                        ; width bytes, height rows
        DB      $00,$30,$00                    ; . . . . . 3 . . . . . .
        DB      $03,$40,$00                    ; . . . 3 1 . . . . . . .
        DB      $CC,$34,$00                    ; 3 . 3 . . 3 1 . . . . .
        DB      $05,$0C,$40                    ; . . 1 1 . . 3 . 1 . . .
        DB      $35,$05,$C0                    ; . 3 1 1 . . 1 1 3 . . .
        DB      $F5,$D7,$00                    ; 3 3 1 1 3 1 1 3 . . . .
        DB      $35,$CC,$00                    ; . 3 1 1 3 . 3 . . . . .
        DB      $43,$C4,$00                    ; 1 . . 3 3 . 1 . . . . .
        DB      $04,$31,$00                    ; . . 1 . . 3 . 1 . . . .

;*******************************************************************************
; Bytes following the sprite boundary
;*******************************************************************************
        DB      $10

            ld      d,a
            dec     b
            inc     c
            ld      d,a
            ei
            ld      bc,$010A
            ld      d,$88
            dec     d
            add     hl,de
            inc     de
            jr      nz,$9F67
            dec     h
            ld      de,$032E
            inc     b
            rst     $08
            ld      e,b
            djnz    $9FCB
            nop
            ld      b,l
            sbc     a,a
            ld      l,l
            djnz    $9FC5
            nop
            inc     de
            ld      a,$12
            ld      b,(hl)
            ld      de,$1096
            ld      b,b
            dec     b
            jr      nz,$9F10
            cp      $01
            ld      a,(bc)
            ld      (bc),a
            rla
            jr      nz,$9F8D
            adc     a,b
            dec     d
            add     hl,de
            inc     bc
            inc     b
            rst     $08
            ld      e,b
            djnz    $9FED
            nop
            ld      h,l
            sbc     a,a
            ld      l,l
            djnz    $9FE7
            nop
            inc     de
            jr      nz,$9F9C
            ld      sp,$4A11
            rla
            inc     c
            djnz    $9FD1
            dec     b
            djnz    $9FD4
            rst     $38
            ld      (bc),a
            ld      a,(bc)
            ld      bc,$1416
            dec     d
            inc     d
            inc     bc
            ld      c,$01
            rst     $38
            ld      bc,$0A08
            ld      (bc),a
            inc     bc
            dec     c
            ld      (bc),a
            jp      p,$0B01
            ld      (bc),a
            ld      bc,$1701
            nop
            ex      af,af'
            ld      a,(bc)
            ld      (bc),a
            inc     bc
            dec     bc
            inc     b
            ld      (bc),a
            ld      bc,$060A
            ex      af,af'
            inc     bc
            inc     b
            inc     de
            scf
            ld      (de),a
            ld      d,h
            ld      de,$177E
            inc     c
            djnz    $A006
            dec     b
            djnz    $A009
            rst     $38
            ld      (bc),a
            ld      a,(bc)
            ld      bc,$7716
            dec     d
            rla
            inc     bc
            ld      c,$01
            rst     $38
            ld      bc,$0A08
            ld      (bc),a
            inc     bc
            dec     c
            ld      (bc),a
            jp      p,$0B01
            ld      (bc),a
            ld      bc,$1701
            nop
            ex      af,af'
            ld      a,(bc)
            ld      (bc),a
            inc     bc
            dec     bc
            inc     b
            ld      (bc),a
            ld      bc,$060A
            ex      af,af'
            inc     bc
            inc     b
            rst     $08
            ld      l,l
            nop
            add     a,a
            sbc     a,a
            inc     l
            djnz    $A066
            nop
            cp      h
            sbc     a,a
            ld      a,e
            djnz    $A060
            nop
            ld      a,($D9AB)
            and     a
            ret     nz
            ld      iy,($DD9E)
            call    $20C6
            ret     z
            res     7,(iy+$00)
            set     6,(iy+$00)
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      ($D9B1),hl
            ld      l,(iy+$0d)
            ld      h,(iy+$0e)
            ld      ($D9AF),hl
            ld      a,(iy+$29)
            ld      ($D9AD),a
            ld      a,$01
            ld      ($D9AB),a
            call    $14BF
            res     7,(ix+$00)
            ret
            ld      iy,$D96B
            call    $20C6
            ret     z
            push    ix
            ld      ix,$D96B
            call    $2E67
            pop     ix
            res     7,(ix+$00)
            set     6,(ix+$00)
            call    $14BF
            ld      a,$01
            and     a
            ret
            ld      a,($D9AB)
            and     a
            ret     nz
            call    $A03A
            ret     z
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      ($D9B1),hl
            ld      l,(ix+$0d)
            ld      h,(ix+$0e)
            ld      ($D9AF),hl
            ld      a,(ix+$29)
            ld      ($D9AD),a
            ld      a,$01
            ld      ($D9AB),a
            ret
            ld      hl,($D0A5)
            ld      a,h
            or      l
            jp      nz,$A094
            ld      hl,($DDA2)
            ld      ($DDA2),bc
            ld      c,l
            ld      b,h
            DW      _DSPATCH
;******************************************************************************************
            pop     hl
            ld      ($D0A5),hl
            ld      hl,($DDA2)
            ld      ($DDA2),bc
            ld      c,l
            ld      b,h
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            ld      hl,($6D01)
            nop
            and     d
            DB      $dd,$f7
            nop
            ld      h,c
            nop
            ld      l,(ix+$15)
            ld      h,(ix+$16)
            ld      b,(ix+$11)
            call    $2E19
            ex      de,hl
            ld      l,(ix+$0f)
            ld      h,(ix+$10)
            and     a
            sbc     hl,de
            ld      e,l
            ld      d,h
            ld      b,(ix+$12)
            call    $2E15
            add     hl,de
            ld      (ix+$0f),l
            ld      (ix+$10),h
            ld      bc,($DCC9)
            add     hl,bc
            ld      a,h
            cp      $4A
            jr      nc,$A11D
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ex      de,hl
            ld      b,(ix+$17)
            call    $2E19
            ld      e,(ix+$15)
            ld      d,(ix+$16)
            add     hl,de
            ld      e,l
            ld      d,h
            ld      b,(ix+$18)
            call    $2E15
            add     hl,de
            ld      (ix+$15),l
            ld      (ix+$16),h
            bit     7,(ix+$2d)
            jp      z,$A10C
            call    $2B33
            ld      bc,($DCCB)
            add     hl,bc
            ld      a,h
            cp      $B0
            jr      nc,$A11D
            ld      (ix+$13),l
            ld      (ix+$14),h
            ret
            res     7,(ix+$00)
            set     6,(ix+$00)
            ret
            call    $1C67
            ld      a,($D944)
            push    af
            call    $A0B0
            pop     af
            dec     a
            jr      nz,$A12C
            bit     5,(ix+$00)
            jp      nz,$A141
            call    $14BF
            jp      $A145
            res     5,(ix+$00)
            call    $1C4B
            bit     6,(ix+$00)
            jp      nz,$A152
            call    $1D32
            jp      $1A6B
            add     a,b
            inc     sp
            ld      b,b
            inc     sp
            ret     nz
            ld      ($3200),a
            ld      b,b
            ld      sp,$30C0
            add     a,b
            jr      nc,$A124
            jr      nc,$A1A6
            ld      sp,$3200
            ret     nz
            ld      ($3340),a
            add     a,b
            ld      ($3180),a
            add     a,b
            ld      sp,$3280
            nop
            ld      h,h
            nop
            ld      h,a
            nop
            ld      l,c
            nop
            ld      l,d
            nop
            ld      l,c
            nop
            ld      h,a
            nop
            ld      h,h
            nop
            ld      h,c
            nop
            ld      e,a
            nop
            ld      e,(hl)
            nop
            ld      e,a
            nop
            ld      h,c
            nop
            ld      h,(hl)
            nop
            ld      h,(hl)
            nop
            ld      h,d
            nop
            ld      h,d
            rst     $08
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            sub     e
            nop
            ld      d,l
            and     c
            jp      (hl)
            nop
            adc     a,l
            ld      (bc),a
            sub     e
            nop
            ld      (hl),l
            and     c
            jp      (hl)
            nop
            halt
            nop
            ld      (bc),a
            adc     a,$0A
            ld      e,a
            ld      (bc),a
            ld      h,c
            nop
            nop
            ld      a,$23
            ld      b,$5A
            nop
            dec     de
            inc     hl
            ld      b,$50
            nop
            daa
            inc     hl
            nop
            ld      b,(hl)
            nop
            nop
            ld      b,(hl)
            inc     hl
            ld      b,$3C
            nop
            ld      h,e
            inc     hl
            ld      b,$32
            nop
            add     a,c
            ld      ($7806),hl
            ld      a,(bc)
            sub     $A1
            nop
            push    bc
            sbc     a,(hl)
            ld      b,$6E
            nop
            jp      c,$069E
            ld      e,d
            nop
            jp      z,$069E
            ld      d,b
            nop
            xor     b
            sbc     a,(hl)
            ld      b,$46
            nop
            add     a,l
            sbc     a,(hl)
            ld      b,$78
            ld      a,(bc)
            jp      p,$00A1
            ld      b,$24
            inc     b
            ld      h,$A1
            inc     d
            ld      e,h
            and     b
            ld      b,$01
            inc     l
            inc     bc
            cp      c
            and     c
            inc     l
            ld      bc,$A1DB
            nop
            ld      a,a
            sbc     a,(hl)
            ld      b,$78
            nop
            ld      l,a
            sbc     a,(hl)
            ld      b,$6E
            nop
            ld      e,e
            sbc     a,(hl)
            ld      b,$5A
            nop
            jr      c,$A1BA
            ld      b,$78
            ld      a,(bc)
            inc     e
            and     d
            inc     c
            ld      e,a
            cp      $19
            inc     bc
            ld      c,$06
            nop
            inc     b
            ld      b,$0A
            rst     $30
            and     c
            inc     c
            ld      l,h
            cp      $39
            inc     bc
            ld      c,$06
            ex      af,af'
            inc     b
            ld      b,$0A
            rst     $30
            and     c
            inc     c
            ld      c,$00
            cp      $FC
            ld      c,$06
            ex      af,af'
            inc     b
            ld      b,$0A
            rst     $30
            and     c
            inc     c
            add     hl,bc
            nop
            cp      a
            call    m,$060E
            nop
            inc     b
            ld      b,$0A
            rst     $30
            and     c
            inc     c
            rra
            rst     $38
            ld      l,a
            inc     b
            ld      c,$06
            nop
            ld      (bc),a
            ld      b,$0A
            rst     $30
            and     c
            inc     c
            cp      d
            cp      $7D
            ld      b,$0E
            rlca
            nop
            inc     bc
            ld      b,$0A
            rst     $30
            and     c
            inc     c
            push    de
            cp      $05
            call    m,$060E
            nop
            ld      bc,$0A06
            rst     $30
            and     c
            inc     c
            adc     a,d
            rst     $38
            and     c
            ld      b,$0E
            rlca
            ex      af,af'
            inc     bc
            ld      b,$0A
            rst     $30
            and     c
            ld      hl,$2EA2
            and     d
            dec     sp
            and     d
            ld      c,b
            and     d
            ld      d,l
            and     d
            ld      h,d
            and     d
            ld      l,a
            and     d
            ld      a,h
            and     d
            ld      a,h
            and     d
            ld      a,h
            and     d
            rst     $08
            halt
            nop
            ld      (bc),a
            ld      e,b
            dec     b
            call    po,$EA01
            inc     bc
            or      c
            and     d
            ld      l,l
            nop
            or      d
            add     a,b
            jp      po,$B403
            and     d
            halt
            nop
            or      d
            ld      h,c
            nop
            rst     $08
            halt
            nop
            jr      nc,$A328
            nop
            adc     a,d
            ret     nc
            ret     p
            nop
            halt
            nop
            dec     b
            ld      h,$04
            halt
            nop
            rlca
            rla
            inc     bc
            ld      de,$6D01
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            halt
            nop
            ld      c,$0B
            ld      bc,$00F0
            halt
            nop
            ld      c,$88
            nop
            ld      l,e
            exx
            ret     p
            nop
            ld      de,$4801
            ld      bc,$0076
            ld      (bc),a
            ld      c,d
            inc     b
            dec     bc
            ld      bc,$0061
            push    iy
            ld      iy,($DD9E)
            ld      hl,$0000
            ld      a,(iy+$00)
            cp      $02
            jp      nc,$A31D
            and     a
            jp      nz,$A313
            inc     (iy+$00)
            ld      a,r
            and     $1F
            add     a,$10
            ld      ($D0A3),a
            jp      $A31D
            ld      de,($D0A3)
            ld      a,d
            or      e
            jp      nz,$A31D
            inc     hl
            pop     iy
            push    hl
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            rst     $28
            and     d
            jp      pe,$8703
            and     e
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            jp      pe,$7F03
            and     e
            ld      l,l
            nop
            and     b
            DB      $dd,$e2
            ld      (bc),a
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            sub     e
            nop
            ld      d,l
            and     c
            jp      (hl)
            nop
            ld      l,l
            nop
            and     b
            jp      (ix)
            nop
            sub     e
            nop
            ld      (hl),l
            and     c
            jp      (hl)
            nop
            halt
            nop
            ld      (bc),a
            adc     a,$0A
            ld      e,d
            sbc     a,a
            halt
            nop
            ld      de,_LIT
            and     c
            ret     nc
            ret     nc
            ld      (bc),a
            halt
            nop
            ex      af,af'
            ld      e,b
            dec     b
            sub     e
            nop
            adc     a,c
            and     d
            jp      (hl)
            nop
            halt
            nop
            ld      b,e
            sbc     a,l
            and     d
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            ld      h,$20
            sbc     a,b
            rrca
            jp      po,$8703
            and     e
            sbc     a,b
            nop
            ld      l,l
            nop
            ld      c,e
            exx
            rst     $30
            nop
            ld      h,c
            nop
            ld      (bc),a
            jr      nz,$A392
            inc     b
            ld      (bc),a
            and     b
            ld      b,$04
            djnz    $A3A7
            ld      a,($04A0)
            ld      a,d
            dec     e
            nop
            and     $9E
            ld      b,$0A
            nop
            ex      de,hl
            sbc     a,(hl)
            ld      b,$0C
            nop
            pop     af
            sbc     a,(hl)
            ex      af,af'
            adc     a,c
            and     e
            ex      af,af'
            adc     a,c
            and     e
            nop
            sbc     a,(iy+$1c)
            inc     bc
            ex      af,af'
            adc     a,c
            and     e
            ld      e,$B0
            and     e
            nop
            dec     bc
            sbc     a,a
            inc     e
            inc     b
            ex      af,af'
            adc     a,c
            and     e
            ld      e,$BB
            and     e
            nop
            jr      z,$A363
            ex      af,af'
            adc     a,c
            and     e
            ex      af,af'
            adc     a,c
            and     e
            nop
            ld      b,$24
            ld      b,$01
            ld      (de),a
            push    iy
            ld      hl,$0000
            ld      de,($D0A1)
            ld      a,d
            or      e
            jr      nz,$A3EF
            ld      iy,($DD9E)
            bit     7,(iy+$00)
            jr      z,$A3EF
            bit     5,(iy+$00)
            jr      nz,$A3EF
            inc     hl
            di
            pop     iy
            push    hl
            DW      _DSPATCH
;******************************************************************************************
            ld      d,b
            nop
            jr      z,$A3F8
            ld      h,$00
            inc     h
            nop
            jr      z,$A3FE
            jr      nz,$A400
            inc     e
            nop
            ld      a,(de)
            nop
            rst     $08
            ret     nc
            and     e
            jp      pe,$9903
            and     h
            ld      l,l
            nop
            adc     a,d
            ret     nc
            ret     p
            nop
            halt
            nop
            inc     bc
            ld      h,$04
            ld      l,l
            nop
            scf
            ret     nc
            ret     p
            nop
            call    po,$EA01
            inc     bc
            ld      h,$A4
            sbc     a,(hl)
            nop
            ld      h,$04
            and     h
            nop
            sub     e
            nop
            call    m,$E9A3
            nop
            adc     a,$00
            sub     e
            nop
            call    p,$E9A3
            nop
            ld      e,b
            dec     b
            dec     bc
            ld      bc,_LIT
            adc     a,d
            ret     nc
            ret     p
            nop
            sbc     a,(hl)
            nop
            jp      c,$EA01
            inc     bc
            ld      e,b
            and     h
            halt
            nop
            dec     b
            ld      e,b
            dec     b
            call    po,$EA01
            inc     bc
            ld      e,b
            and     h
            or      c
            nop
            halt
            nop
            ex      af,af'
            ld      l,l
            nop
            and     c
            ret     nc
            rst     $30
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            halt
            nop
            dec     c
            dec     bc
            ld      bc,$00E9
            ld      l,l
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            halt
            nop
            inc     de
            dec     bc
            ld      bc,$00E9
            halt
            nop
            dec     c
            adc     a,b
            nop
            ld      l,e
            exx
            jp      (hl)
            nop
            halt
            nop
            inc     de
            adc     a,b
            nop
            ld      l,e
            exx
            jp      (hl)
            nop
            ld      l,l
            nop
            sub     d
            and     e
            or      (hl)
            and     d
            halt
            nop
            and     d
            rra
            jr      nz,$A514
            sbc     a,a
            ld      h,c
            nop
            inc     d
            call    p,$0E05
            dec     b
            inc     e
            dec     b
            or      b
            ld      a,(bc)
            dec     a
            inc     sp
            inc     b
            ld      (hl),l
            call    m,$044B
            ld      (hl),l
            call    m,$CF4B
            ld      l,l
            nop
            ld      b,h
            exx
            ret     p
            nop
            halt
            nop
            ld      (bc),a
            ld      l,l
            nop
            ld      b,h
            exx
            cp      $00
            adc     a,e
            jr      z,$A52E
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            ret     p
            nop
            halt
            nop
            add     a,b
            dec     d
            ld      (bc),a
            call    po,$EA01
            inc     bc
            cp      (hl)
            and     h
            and     a
            djnz    $A543
            nop
            ld      b,h
            exx
            cp      $00
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            ret     m
            dec     hl
            cp      c
            dec     hl
            ld      l,l
            nop
            nop
            ld      (_LIT),a
            nop
            ld      h,h
            sub     e
            dec     l
            halt
            nop
            djnz    $A568
            nop
            djnz    $A56B
            nop
            jr      c,$A56E
            nop
            jr      c,$A4F5
            dec     l
            ld      l,l
            nop
            nop
            ld      (_LIT),a
            ret
            call    c,$00F7
            ld      l,l
            nop
            nop
            ld      h,h
            ld      l,l
            nop
            set     3,h
            rst     $30
            nop
            halt
            nop
            ex      af,af'
            sub     (hl)
            and     b
            ld      l,l
            nop
            call    p,$6D05
            nop
            and     a
            call    c,$00F7
            halt
            nop
            ld      (hl),b
            sub     (hl)
            and     b
            ld      l,l
            nop
            ret     m
            dec     hl
            ld      l,l
            nop
            and     a
            call    c,$00F7
            halt
            nop
            djnz    $A5A8
            nop
            djnz    $A5AB
            nop
            rst     $38
            halt
            nop
            rst     $38
            jp      m,$6D2D
            nop
            rst     $38
            rst     $38
            sub     (hl)
            and     b
            sbc     a,b
            nop
            jp      pe,$3C03
            and     l
            ld      h,c
            nop
            rst     $08
            sbc     a,b
            nop
            adc     a,e
            dec     b
            ld      a,e
            dec     (hl)
            xor     (hl)
            add     hl,hl
            halt
            nop
            dec     h
            call    $6D09
            nop
            scf
            ret     nc
            ret     p
            nop
            jp      pe,$6C03
            and     l
            halt
            nop
            djnz    $A5DC
            nop
            ld      (bc),a
            jp      po,$7103
            and     l
            halt
            nop
            inc     c
            sbc     a,(hl)
            nop
            ld      l,l
            nop
            ld      b,h
            exx
            rst     $30
            nop
            ld      l,l
            nop
            and     b
            DB      $dd,$f7
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            xor     e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            nop
            and     b
            ld      l,l
            nop
            ld      d,e
            exx
            rst     $30
            nop
            halt
            nop
            add     a,b
            ld      l,l
            nop
            ld      c,e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            xor     (hl)
            and     h
            ld      l,l
            nop
            ld      l,c
            exx
            rst     $30
            nop
            sub     l
            and     c
            ld      l,l
            nop
            sbc     a,e
            and     h
            ld      l,l
            nop
            ld      h,a
            exx
            rst     $30
            nop
            ld      b,(hl)
            inc     sp
            dec     a
            dec     d
            and     h
            nop
            ld      l,l
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            sbc     a,b
            nop
            adc     a,$00
            rst     $30
            nop
            halt
            nop
            ld      b,b
            ld      l,l
            nop
            and     e
            ret     nc
            rst     $30
            nop
            halt
            nop
            ld      d,b
            ld      l,l
            nop
            and     c
            ret     nc
            rst     $30
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            and     l
            ret     nc
            rst     $30
            nop
            ld      l,l
            nop
            DB      $dd, $a4
            and     l
            and     b
            ld      h,c
            nop
            rst     $08
            add     a,e
            and     b
            sub     $2B
            inc     c
            dec     hl
            ld      ($04A3),hl
            and     h
            dec     b
            inc     (hl)
            cp      h
            ld      ($288B),a
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            rst     $30
            nop
            add     a,e
            and     b
            sub     $2B
            cp      h
            ld      ($288B),a
            ld      l,l
            nop
            sbc     a,a
            ret     nc
            jp      (hl)
            nop
            call    po,$EA01
            inc     bc
            call    m,$61A5
            nop
            rst     $08
            ld      c,d
            and     l
            add     hl,bc
            djnz    $A670
            djnz    $A60B
            sbc     a,a
            halt
            nop
            add     hl,bc
            halt
            nop
            ld      (bc),a
            ld      l,l
            nop
            and     (hl)
            and     h
            rlca
            scf
            jp      po,$6DA5
            nop
            ld      c,a
            exx
            jp      (hl)
            nop
            jp      pe,$2703
            and     (hl)
            ld      l,l
            nop
            ld      d,c
            exx
            jp      (hl)
            nop
            call    po,$EA01
            inc     bc
            ld      b,h
            and     (hl)
            halt
            nop
            ex      af,af'
            push    af
            and     l
            halt
            nop
            ld      (bc),a
            jr      z,$A680
            ld      h,c
            nop
            rst     $08
            ld      c,d
            and     l
            ld      l,l
            nop
            ret     m
            dec     hl
            cp      c
            dec     hl
            ld      l,l
            nop
            nop
            ld      (_LIT),a
            nop
            ld      h,h
            sub     e
            dec     l
            ld      l,l
            nop
            nop
            ld      (_LIT),a
            ret
            call    c,$00F7
            ld      l,l
            nop
            nop
            ld      h,h
            ld      l,l
            nop
            set     3,h
            rst     $30
            nop
            halt
            nop
            djnz    $A6EC
            nop
            djnz    $A6EF
            nop
            rst     $38
            halt
            nop
            rst     $38
            jp      m,$762D
            nop
            add     hl,bc
            sbc     a,(hl)
            nop
            ld      l,l
            nop
            and     (hl)
            and     h
            rlca
            scf
            halt
            nop
            dec     b
            ld      l,l
            nop
            and     e
            ret     nc
            rst     $30
            nop
            halt
            nop
            and     b
            ld      l,l
            nop
            and     c
            ret     nc
            rst     $30
            nop
            halt
            nop
            ret     nz
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            sub     $2B
            ld      ($04A3),hl
            and     h
            jr      $A6DB
            ld      h,(hl)
            ld      a,(de)
            sbc     a,b
            rrca
            ld      e,a
            ld      (bc),a
            sbc     a,(hl)
            nop
            jr      z,$A6ED
            ld      h,c
            nop
            ld      d,b
            rst     $38
            and     l
            sub     (hl)
            or      d
            cp      (hl)
            or      d
            ld      b,$40
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            nop
            djnz    $A6CC
            nop
            nop
            nop
            nop
            djnz    $A6D2
            nop
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            nop
            ld      d,h
            nop
            nop
            nop
            nop
            ld      bc,$0055
            nop
            nop
            nop
            ld      bc,$0055
            nop
            nop
            nop
            dec     b
            ld      h,l
            ld      b,b
            nop
            nop
            nop
            dec     b
            ld      h,l
            ld      b,b
            nop
            nop
            nop
            dec     b
            xor     c
            ld      b,b
            nop
            nop
            nop
            dec     d
            xor     c
            ld      d,b
            nop
            inc     c
            inc     c
            dec     d
            xor     c
            ld      d,b
            nop
            inc     bc
            jr      nc,$A746
            xor     d
            ld      d,b
            nop
            nop
            ret     nz
            ld      d,$AA
            ld      d,b
            nop
            nop
            jr      nc,$A792
            xor     d
            ld      d,h
            nop
            nop
            rrca
            ld      d,(hl)
            xor     d
            ld      d,h
            nop
            nop
            nop
            ld      e,d
            xor     d
            sub     h
            nop
            nop
            nop
            ld      e,d
            xor     d
            sub     h
            nop
            nop
            nop
            ld      e,d
            xor     d
            sub     h
            ret     nz
            nop
            nop
            ld      e,d
            xor     d
            sub     h
            ret     nz
            nop
            nop
            ld      e,d
            xor     d
            sub     h
            ret     nz
            nop
            ld      (bc),a
            ld      e,d
            cp      d
            sub     h
            nop
            nop
            ld      (bc),a
            ld      e,d
            cp      $94
            nop
            nop
            ld      (bc),a
            ld      e,e
            rst     $38
            sub     h
            nop
            nop
            nop
            rrca
            rst     $38
            sub     h
            ret     nz
            nop
            ld      (bc),a
            ld      e,e
            rst     $38
            sub     h
            ret     nz
            nop
            ld      a,(bc)
            ld      e,e
            rst     $38
            sub     h
            ret     nz
            nop
            ld      hl,($FF5B)
            sub     h
            nop
            xor     b
            xor     d
            ld      e,e
            rst     $38
            sub     h
            nop
            xor     d
            xor     d
            ld      e,e
            rst     $38
            sub     h
            nop
            cp      d
            xor     d
            ld      e,e
            rst     $38
            sub     h
            ret     nz
            cp      d
            xor     d
            ld      e,e
            rst     $38
            sub     h
            ret     nz
            cp      d
            xor     b
            ld      e,e
            rst     $38
            sub     h
            ret     nz
            cp      d
            and     b
            ld      e,d
            cp      $94
            nop
            cp      d
            add     a,b
            ld      e,d
            cp      d
            sub     h
            nop
            cp      d
            nop
            ld      e,d
            xor     d
            sub     h
            nop
            cp      b
            nop
            ld      e,d
            xor     d
            sub     h
            ret     nz
            cp      b
            nop
            ld      e,d
            xor     d
            sub     h
            ret     nz
            cp      b
            nop
            ld      e,d
            xor     d
            sub     h
            ret     nz
            cp      b
            nop
            ld      d,(hl)
            xor     d
            sub     h
            nop
            cp      b
            nop
            ld      d,l
            ld      d,l
            ld      d,h
            nop
            cp      b
            nop
            ld      d,l
            ld      d,l
            ld      d,h
            nop
            cp      b
            nop
            dec     d
            ld      d,l
            ld      d,b
            nop
            cp      b
            nop
            djnz    $A7EB
            djnz    $A7ED
            cp      b
            nop
            ld      (de),a
            ld      ($0010),hl
            cp      b
            nop
            ld      (de),a
            ld      ($0010),hl
            cp      b
            nop
            ld      (de),a
            ld      ($0010),hl
            cp      b
            nop
            ld      (de),a
            ld      ($0010),hl
            cp      b
            nop
            ld      (bc),a
            ld      ($0000),hl
            cp      b
            nop
            ld      (bc),a
            ld      ($0000),hl
            cp      b
            nop
            nop
            nop
            nop
            nop
            cp      b
            nop
            nop
            nop
            nop
            nop
            cp      b
            nop
            nop
            nop
            nop
            nop
            cp      b
            nop
            nop
            nop
            nop
            nop
            cp      b
            nop
            nop
            nop
            nop
            nop
            cp      b
            nop
            nop
            nop
            nop
            nop
            xor     b
            nop
            nop
            nop
            nop
            nop
            xor     b
            nop
            nop
            nop
            nop
            nop
;*******************************************************************************
; FLAG_SHIP_FIREBALL_SEED
; 2 bytes/row = 8 pixels wide, 5 rows
;*******************************************************************************
FLAG_SHIP_FIREBALL_SEED:
        DB      $02,$05                        ; width bytes, height rows
        DB      $CC,$00                        ; 3 . 3 . . . . .
        DB      $03,$00                        ; . . . 3 . . . .
        DB      $57,$C0                        ; 1 1 1 3 3 . . .
        DB      $0F,$00                        ; . . 3 3 . . . .
        DB      $30,$00                        ; . 3 . . . . . .

;*******************************************************************************
; FLAG_SHIP_FIREBALL_STREAK
; 3 bytes/row = 12 pixels wide, 3 rows
;*******************************************************************************
FLAG_SHIP_FIREBALL_STREAK:
        DB      $03,$03                        ; width bytes, height rows
        DB      $14,$00,$00                    ; . 1 1 . . . . . . . . .
        DB      $D5,$55,$55                    ; 3 1 1 1 1 1 1 1 1 1 1 1
        DB      $1C,$00,$00                    ; . 1 3 . . . . . . . . .

;*******************************************************************************
; FLAG_SHIP_FIREBALL_SMALL
; 3 bytes/row = 12 pixels wide, 5 rows
;*******************************************************************************
FLAG_SHIP_FIREBALL_SMALL:
        DB      $03,$05                        ; width bytes, height rows
        DB      $07,$00,$00                    ; . . 1 3 . . . . . . . .
        DB      $35,$40,$00                    ; . 3 1 1 1 . . . . . . .
        DB      $D5,$55,$55                    ; 3 1 1 1 1 1 1 1 1 1 1 1
        DB      $3D,$40,$00                    ; . 3 3 1 1 . . . . . . .
        DB      $0D,$00,$00                    ; . . 3 1 . . . . . . . .

;*******************************************************************************
; FLAG_SHIP_FIREBALL_MEDIUM
; 3 bytes/row = 12 pixels wide, 7 rows
;*******************************************************************************
FLAG_SHIP_FIREBALL_MEDIUM:
        DB      $03,$07                        ; width bytes, height rows
        DB      $03,$D0,$00                    ; . . . 3 3 1 . . . . . .
        DB      $0F,$5C,$00                    ; . . 3 3 1 1 3 . . . . .
        DB      $3D,$75,$00                    ; . 3 3 1 1 3 1 1 . . . .
        DB      $FF,$55,$55                    ; 3 3 3 3 1 1 1 1 1 1 1 1
        DB      $17,$D7,$00                    ; . 1 1 3 3 1 1 3 . . . .
        DB      $0D,$5C,$00                    ; . . 3 1 1 1 3 . . . . .
        DB      $01,$50,$00                    ; . . . 1 1 1 . . . . . .

;*******************************************************************************
; FLAG_SHIP_FIREBALL_LARGE
; 3 bytes/row = 12 pixels wide, 9 rows
;*******************************************************************************
FLAG_SHIP_FIREBALL_LARGE:
        DB      $03,$09                        ; width bytes, height rows
        DB      $01,$70,$00                    ; . . . 1 1 3 . . . . . .
        DB      $05,$FC,$00                    ; . . 1 1 3 3 3 . . . . .
        DB      $35,$75,$00                    ; . 3 1 1 1 3 1 1 . . . .
        DB      $7D,$55,$40                    ; 1 3 3 1 1 1 1 1 1 . . .
        DB      $DF,$5D,$55                    ; 3 1 3 3 1 1 3 1 1 1 1 1
        DB      $D5,$5F,$C0                    ; 3 1 1 1 1 1 3 3 3 . . .
        DB      $35,$7D,$00                    ; . 3 1 1 1 3 3 1 . . . .
        DB      $07,$74,$00                    ; . . 1 3 1 3 1 . . . . .
        DB      $03,$D0,$00                    ; . . . 3 3 1 . . . . . .

;*******************************************************************************
; FLAG_SHIP_FIREBALL_FULL
; 3 bytes/row = 12 pixels wide, 11 rows
;*******************************************************************************
FLAG_SHIP_FIREBALL_FULL:
        DB      $03,$0B                        ; width bytes, height rows
        DB      $00,$D4,$00                    ; . . . . 3 1 1 . . . . .
        DB      $0F,$57,$C0                    ; . . 3 3 1 1 1 3 3 . . .
        DB      $3F,$F5,$D0                    ; . 3 3 3 3 3 1 1 3 1 . .
        DB      $3D,$FD,$50                    ; . 3 3 1 3 3 3 1 1 1 . .
        DB      $F5,$75,$7C                    ; 3 3 1 1 1 3 1 1 1 3 3 .
        DB      $57,$55,$FD                    ; 1 1 1 3 1 1 1 1 3 3 3 1
        DB      $55,$5D,$F7                    ; 1 1 1 1 1 1 3 1 3 3 1 3
        DB      $1D,$DF,$F0                    ; . 1 3 1 3 1 3 3 3 3 . .
        DB      $3F,$57,$F0                    ; . 3 3 3 1 1 1 3 3 3 . .
        DB      $05,$5F,$40                    ; . . 1 1 1 1 3 3 1 . . .
        DB      $00,$5C,$00                    ; . . . . 1 1 3 . . . . .

            ex      de,hl
            ld      a,r
            and     $07
            cp      $03
            jp      nc,$A8CF
            ld      a,r
            jp      $A8D3
            ld      a,($D97F)
            rrca
            and     $7F
            ld      l,a
            ld      h,$00
            ld      a,(ix+$14)
            rrca
            and     $7F
            ld      c,a
            ld      a,r
            and     $1F
            sub     $0F
            add     a,c
            ld      c,a
            ld      b,$00
            sbc     hl,bc
            add     hl,hl
            ld      c,l
            ld      b,h
            add     hl,hl
            add     hl,bc
            ld      (ix+$15),l
            ld      (ix+$16),h
            ex      de,hl
            ret
            push    hl
            ld      a,($D08A)
            cp      $04
            jp      c,$A903
            ld      a,$04
            rlca
            ld      e,a
            ld      d,$00
            ld      hl,$0000
            and     a
            sbc     hl,de
            ld      (ix+$11),l
            ld      (ix+$12),h
            pop     hl
            ret
            inc     b
            ld      ($281E),hl
            ld      b,$24
            inc     c
            dec     b
            rst     $38
            nop
            nop
            inc     h
            ret     nz
            xor     b
            ld      c,$03
            nop
            nop
            nop
            nop
            ld      bc,$0624
            dec     b
            ld      a,(de)
            nop
            rst     $38
            nop
            ld      c,l
            xor     b
            ld      b,$06
            ld      a,(de)
            nop
            rst     $38
            inc     e
            ld      (bc),a
            nop
            ld      e,b
            xor     b
            ld      (bc),a
            jr      nz,$A946
            inc     bc
            ld      (bc),a
            and     b
            ld      b,$03
            ld      e,$3A
            xor     c
            ld      a,(de)
            nop
            rst     $38
            nop
            ld      l,c
            xor     b
            ld      (bc),a
            jr      nz,$A957
            inc     bc
            ld      (bc),a
            and     b
            ld      b,$03
            ld      a,(de)
            nop
            rst     $38
            ld      c,$00
            nop
            nop
            nop
            inc     h
            ret     m
            xor     b
            inc     e
            inc     bc
            nop
            add     a,b
            xor     b
            ld      (bc),a
            jr      nz,$A96F
            inc     bc
            ld      (bc),a
            and     b
            ld      b,$03
            ld      e,$63
            xor     c
            ld      a,(de)
            nop
            rst     $38
            inc     e
            djnz    $A977
            sbc     a,l
            xor     b
            ld      (bc),a
            jr      nz,$A982
            inc     bc
            ld      (bc),a
            and     b
            ld      b,$03
            ld      e,$76
            xor     c
            ld      (de),a
            ld      ($7B0D),hl
            ld      e,c
            rra
            ld      l,d
            ld      hl,($0015)
            add     hl,hl
            inc     c
            dec     l
            ld      h,a
            ld      e,b
            ld      c,$2C
            dec     de
            ld      (hl),e
            ld      l,e
            ld      e,$3A
            ld      a,$0E
            inc     sp
            ld      hl,($261D)
            dec     hl
            dec     c
            ld      d,l
            ld      (hl),l
            ld      (hl),a
            dec     l
            ld      a,$3E
            jr      $A9D1
            ld      c,l
            add     hl,sp
            inc     sp
            inc     e
            inc     b
            ld      (hl),l
            ld      l,e
            ld      e,l
            add     hl,hl
            ld      b,$0D
            add     hl,de
            dec     hl
            ld      c,b
            ld      h,e
            ld      c,l
            add     hl,bc
            add     hl,de
            inc     hl
            inc     (hl)
            jr      $A9D2
            ld      a,$15
            dec     e
            ld      h,$2B
            inc     bc
            dec     de
            ld      c,e
            ld      l,d
            dec     bc
            inc     d
            inc     c
            ex      af,af'
            add     hl,bc
            add     hl,hl
            ld      e,l
            ld      e,b
            dec     sp
            inc     e
            ld      de,$2527
            ld      a,$10
            inc     hl
            dec     b
            djnz    $AA2E
            ld      (bc),a
            ld      bc,$8816
            dec     d
            ex      af,af'
            inc     de
            rst     $38
            inc     de
            DB      $fd,$12
            cp      $11
            rst     $38
            ld      bc,$1304
            jp      m,$FC12
            ld      de,$01FE
            inc     b
            inc     de
            rst     $30
            ld      (de),a
            jp      m,$FD11
            ld      bc,$1304
            call    p,$F812
            ld      de,$01FC
            inc     b
            inc     de
            pop     af
            ld      (de),a
            or      $11
            ei
            ld      bc,$1304
            xor     $12
            call    p,$FA11
            ld      bc,$1304
            ex      de,hl
            ld      (de),a
            jp      p,$F911
            ld      bc,$0204
            call    po,$CFA9
            ld      l,l
            nop
            rst     $10
            xor     c
            ld      l,l
            djnz    $AA88
            nop
            inc     de
            dec     h
            ld      (de),a
            add     hl,hl
            ld      de,$162E
            ld      h,(hl)
            dec     d
            ld      b,$10
            add     a,b
            dec     b
            ld      (bc),a
            add     a,b
            rst     $38
            ld      bc,$010A
            inc     bc
            rla
            djnz    $AA44
            ld      (bc),a
            rrca
            ld      bc,$0A06
            dec     b
            ld      d,$FF
            dec     d
            ccf
            inc     bc
            ld      a,(de)
            rrca
            nop
            rst     $38
            ld      d,$15
            rra
            ld      d,$FF
            dec     b
            rrca
            or      b
            ld      bc,$0F02
            nop
            nop
            nop
            or      a
            ld      a,(bc)
            ld      bc,$0403
            ld      bc,$1306
            dec     de
            ld      (de),a
            rra
            ld      de,$0222
            ld      l,$AA
            rst     $08
            ld      l,l
            nop
            ld      h,c
            xor     d
            ld      a,e
            djnz    $AAE1
            nop
            jr      z,$AA21
            inc     l
            djnz    $AADB
            nop
            djnz    $AA8D
            dec     b
            djnz    $AAB0
            ld      bc,$0A02
            ld      bc,$2913
            ld      (de),a
            dec     h
            ld      de,$1627
            xor     c
            dec     d
            inc     e
            inc     bc
            rrca
            ld      l,h
            inc     bc
            ld      (bc),a
            nop
            dec     b
            jr      nc,$AA26
            inc     bc
            ld      (bc),a
            ld      a,(bc)
            ld      bc,$0203
            rst     $10
            xor     c
            rst     $08
            ld      e,b
            djnz    $AB10
            nop
            ld      a,e
            xor     d
            ld      l,l
            djnz    $AB0A
            nop
            djnz    $AABC
            dec     b
            ld      b,$10
            cp      $02
            rrca
            nop
            nop
            nop
            dec     b
            ld      d,$AA
            dec     d
            ld      a,(de)
            inc     de
            jr      nz,$AACF
            cp      $11
            rst     $38
            ld      bc,$1304
            inc     e
            ld      (de),a
            call    m,$FE11
            ld      bc,$1304
            ld      a,(de)
            ld      (de),a
            jp      m,$FD11
            ld      bc,$0404
            rst     $08
            add     hl,bc
            djnz    $AB44
            nop
            xor     d
            xor     d
            ld      e,$10
            ld      h,c
            nop
            inc     de
            ld      h,d
            ld      (de),a
            ld      b,b
            ld      de,$1011
            djnz    $AAEC
            djnz    $AAA9
            inc     b
            ld      bc,$010A
            rrca
            or      b
            inc     b
            ld      bc,$1600
            ld      b,h
            dec     d
            dec     d
            inc     bc
            djnz    $AAB9
            ld      b,$02
            ret     nz
            ret     m
            ld      bc,$000F
            ret     m
            ld      bc,$0AB0
            ld      bc,$1003
            add     a,b
            ld      b,$02
            add     a,b
            ret     m
            ld      bc,$000F
            ret     m
            ld      bc,$0A70
            ld      bc,$1003
            add     a,b
            ld      b,$02
            add     a,b
            ret     m
            ld      bc,$000F
            ret     m
            ld      bc,$0A70
            ld      bc,$1003
            ld      (bc),a
            dec     b
            ld      (bc),a
            rst     $38
            ld      bc,$0A03
            ld      bc,$0403
            inc     de
            ld      d,b
            ld      (de),a
            jr      nc,$AB45
            inc     de
            djnz    $AB47
            dec     b
            djnz    $AAFA
            inc     b
            ld      bc,$010A
            rrca
            or      b
            inc     b
            ld      bc,$1600
            ld      (hl),a
            dec     d
            jr      $AB4B
            djnz    $AB0A
            ld      b,$02
            ret     nz
            ret     m
            ld      bc,$000F
            ret     m
            ld      bc,$0AB0
            ld      bc,$1003
            add     a,b
            ld      b,$02
            add     a,b
            ret     m
            ld      bc,$000F
            ret     m
            ld      bc,$0A70
            ld      bc,$1003
            add     a,b
            ld      b,$02
            add     a,b
            ret     m
            ld      bc,$000F
            ret     m
            ld      bc,$0A70
            ld      bc,$1003
            ld      (bc),a
            dec     b
            ld      (bc),a
            rst     $38
            ld      bc,$0A03
            ld      bc,$0403
            rst     $08
            ld      l,l
            nop
            sbc     a,$AA
            inc     l
            djnz    $ABF5
            nop
            cpl
            xor     e
            ld      a,e
            djnz    $ABEF
            nop
            exx
            ld      hl,$A6BF
            ld      de,$DDB0
            ld      bc,$0182
            ldir
            exx
            DW      _DSPATCH
;******************************************************************************************
            inc     c
            nop
            nop
            nop
            nop
            nop
            or      b
            DB      $dd,$06
            ld      bc,$081A
            ld      a,(de)
            nop
            sub     l
            inc     hl
            ld      b,$14
            ld      a,(de)
            DB      $fd,$fc
            nop
            cp      e
            inc     hl
            ld      b,$14
            ld      a,(de)
            rst     $38
            cp      $00
            add     hl,bc
            inc     h
            ld      b,$14
            nop
            sub     l
            inc     h
            ld      b,$14
            nop
            ld      b,$24
            ld      b,$01
            ld      (de),a
            bit     0,(ix+$00)
            jp      nz,$AC06
            call    $1C67
            push    bc
            bit     5,(ix+$00)
            jp      nz,$ABE3
            call    $14BF
            jp      $ABE7
            res     5,(ix+$00)
            pop     bc
            call    $1DAC
            call    $1C4B
            bit     6,(ix+$00)
            jp      nz,$ABFB
            call    $1492
            jp      $AC03
            res     6,(ix+$00)
            set     5,(ix+$00)
            jp      $AC0A
            ld      (ix+$07),$00
            jp      $1A6B
            inc     b
            res     5,e
            nop
            or      b
            ld      ix,($1650)
            nop
            ld      b,b
            jr      $AC1A
            djnz    $AC38
            ld      b,$02
            and     b
            inc     c
            cp      $FF
            ld      b,b
            nop
            ld      c,$00
            nop
            ld      (bc),a
            nop
            ld      b,$60
            ld      c,$00
            nop
            cp      $FF
            ld      b,$60
            ld      (bc),a
            jr      nz,$AC41
            cp      $FF
            ret     nz
            rst     $38
            ld      c,$00
            nop
            cp      $FF
            ld      b,$60
            ld      c,$00
            nop
            ld      (bc),a
            nop
            ld      b,$60
            ld      e,$1D
            xor     h
            inc     e
            ld      b,$02
            and     b
            inc     c
            ld      (bc),a
            nop
            ld      b,b
            nop
            ld      c,$00
            nop
            ld      (bc),a
            nop
            ld      b,$60
            ld      c,$00
            nop
            cp      $FF
            ld      b,$60
            ld      (bc),a
            jr      nz,$AC70
            ld      (bc),a
            nop
            ret     nz
            rst     $38
            ld      c,$00
            nop
            cp      $FF
            ld      b,$60
            ld      c,$00
            nop
            ld      (bc),a
            nop
            ld      b,$60
            ld      e,$4C
            xor     h
            ld      a,(bc)
            dec     de
            xor     h
            inc     c
            nop
            nop
            nop
            nop
            ld      a,(de)
            ex      af,af'
            cp      $28
            ld      b,$24
            nop
            ld      b,c
            xor     b
            ld      b,$03
            ld      (de),a
            ld      iy,($DDA2)
            call    $20C6
            jr      z,$ACB4
            set     0,(iy+$00)
            ld      l,(iy+$0d)
            ld      h,(iy+$0e)
            ld      ($DDA6),hl
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      ($DDA8),hl
            ld      a,(ix+$14)
            ld      ($DDA0),a
            jr      $ACE4
            call    $3806
            jr      nz,$ACE4
            ld      c,$09
            call    $2114
            ret     z
            res     7,(iy+$00)
            set     6,(iy+$00)
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      ($D9B1),hl
            ld      l,(iy+$0d)
            ld      h,(iy+$0e)
            ld      ($D9AF),hl
            ld      a,$01
            ld      ($D9AB),a
            ld      a,(iy+$29)
            ld      ($D9AD),a
            ld      hl,$AC7C
            call    $1F49
            set     0,(ix+$00)
            ld      hl,$1D48
            ld      (ix+$05),l
            ld      (ix+$06),h
            ret
            jr      z,$AD39
            jr      nz,$AD1B
            inc     e
            rrca
            djnz    $AD0F
            inc     c
            rrca
            ex      af,af'
            rrca
            ld      iy,($DDA2)
            ld      c,(iy+$0d)
            ld      b,(iy+$0e)
            dec     b
            push    bc
            ld      l,(iy+$13)
            ld      h,(iy+$14)
            ld      de,$2000
            add     hl,de
            push    hl
            ld      hl,$A915
            push    hl
            ld      a,b
            rlca
            sub     $0A
            ld      l,a
            ld      h,$00
            push    hl
            ld      hl,$04A4
            push    hl
            ld      a,($D08A)
            cp      $05
            jp      c,$AD35
            ld      a,$05
            rlca
            ld      e,a
            ld      d,$00
            ld      hl,$ACF8
            add     hl,de
            ld      e,(hl)
            inc     hl
            ld      a,r
            and     (hl)
            add     a,e
            ld      ($D0A1),a
            ld      a,(iy+$0e)
            sub     $08
            ld      ($D0A3),a
            jp      $205E
            push    ix
            push    iy
            exx
            call    $AD04
            exx
            pop     iy
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            ld      a,r
            and     $01
            jp      nz,$AD6E
            ld      ix,($DDAC)
            jp      $AD72
            ld      ix,($DDAE)
            di
            bit     7,(ix+$00)
            ret     z
            bit     7,(ix+$09)
            ret     z
            ld      a,(ix+$14)
            sub     $20
            cp      $90
            ret     nc
            ld      a,r
            and     $01
            jp      nz,$AD92
            ld      hl,$3C42
            jp      $AD95
            ld      hl,$3C48
            res     7,(ix+$09)
            call    $1F49
            ld      a,r
            and     $1F
            add     a,$18
            ld      ($D0A1),a
            jp      $27D1
            push    ix
            push    iy
            push    bc
            call    $AD60
            ei
            pop     bc
            pop     iy
            pop     ix
            DW      _DSPATCH
;******************************************************************************************
            rst     $08
            sbc     a,a
            xor     d
            ld      d,c
            xor     l
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            and     c
            ret     nc
            jp      (hl)
            nop
            sbc     a,b
            nop
            ld      l,h
            ld      bc,_0BRANCH
            rst     $28
            xor     l
            ld      l,l
            nop
            scf
            ret     nc
            ret     p
            nop
            jp      pe,$ED03
            xor     l
            halt
            nop
            ld      (bc),a
            ld      e,b
            dec     b
            jp      pe,$E703
            xor     l
            xor     b
            xor     l
            jp      po,$E903
            xor     l
            cp      b
            xor     l
            jp      po,$EF03
            xor     l
            cp      b
            xor     l
            ld      h,c
            nop
            add     a,b
            ret     nz
            ret     p
            djnz    $AE36
            ld      a,a
            ld      (de),a
            bit     7,(iy+$1b)
            jp      z,$AE06
            ld      a,$3F
            sub     b
            ld      l,a
            jp      $AE07
            ld      l,b
            ld      h,$00
            add     hl,hl
            ld      e,l
            ld      d,h
            add     hl,hl
            add     hl,de
            ld      e,c
            ld      d,$00
            add     hl,de
            ld      de,$DDB2
            add     hl,de
            ret
            di
            ld      iy,($DDA2)
            push    bc
            call    $1526
            push    hl
            pop     ix
            call    $1F3E
            pop     bc
            call    $ADF8
            push    bc
            push    hl
            push    ix
            pop     hl
            ld      de,$0032
            add     hl,de
            ld      (ix+$1d),l
            ld      (ix+$1e),h
            ld      (hl),$02
            inc     hl
            ld      (hl),$03
            inc     hl
            ex      de,hl
            pop     hl
            ld      b,$03
            ld      a,(hl)
            ld      (de),a
            ld      (hl),$00
            inc     de
            inc     de
            bit     7,(iy+$1b)
            jp      nz,$AE59
            inc     hl
            inc     hl
            inc     hl
            inc     hl
            inc     hl
            inc     hl
            jp      $AE5F
            dec     hl
            dec     hl
            dec     hl
            dec     hl
            dec     hl
            dec     hl
            djnz    $AE43
            pop     bc
            ld      hl,($DDA6)
            ld      d,c
            ld      e,$00
            add     hl,de
            ld      (ix+$0d),l
            ld      (ix+$0e),h
            ld      de,($DDA4)
            ld      a,d
            or      e
            jp      nz,$AE84
            ld      hl,$ADF1
            ld      e,c
            add     hl,de
            ld      e,(hl)
            bit     7,e
            jp      z,$AE84
            dec     d
            ld      l,(iy+$0f)
            ld      h,(iy+$10)
            add     hl,de
            ld      (ix+$0f),l
            ld      (ix+$10),h
            ld      hl,($DDA8)
            ld      d,b
            ld      e,$00
            add     hl,de
            ld      (ix+$13),l
            ld      (ix+$14),h
            ld      l,b
            ld      h,$00
            ld      de,$FFE0
            add     hl,de
            add     hl,hl
            add     hl,hl
            add     hl,hl
            ld      e,(iy+$15)
            ld      d,(iy+$16)
            add     hl,de
            ld      (ix+$15),l
            ld      (ix+$16),h
            ld      (ix+$1b),$20
            ld      (ix+$2d),$08
            ld      (ix+$29),$46
            ld      hl,($DDAA)
            ld      (ix+$0b),l
            ld      (ix+$0c),h
            ld      (ix+$0a),$78
            ld      (ix+$00),$A4
            ld      (ix+$08),$30
            ld      hl,$2406
            ld      (ix+$2a),l
            ld      (ix+$2b),h
            ld      hl,$1D48
            ld      (ix+$05),l
            ld      (ix+$06),h
            ld      hl,$ADF7
            ld      (ix+$21),l
            ld      (ix+$22),h
            call    $1492
            call    $1F63
            ret
            exx
            push    ix
            push    iy
            ld      iy,($DDA2)
            ld      hl,$0000
            ld      a,($DDA0)
            and     a
            jp      z,$AF9F
            sub     (iy+$14)
            cp      $40
            jp      nc,$AF94
            ld      b,a
            ld      c,$00
            call    $ADF8
            ld      a,(hl)
            and     a
            jr      nz,$AF26
            inc     c
            ld      a,c
            cp      $06
            jr      c,$AF16
            jp      $AF94
            bit     7,(iy+$1b)
            jp      z,$AF33
            ld      a,$3F
            sub     b
            jp      $AF34
            ld      a,b
            cp      $1C
            jp      c,$AF7B
            cp      $2A
            jp      nc,$AF7B
            ld      a,c
            cp      $02
            jp      c,$AF7B
            cp      $04
            jp      nc,$AF7B
            ld      e,$04
            ld      d,(hl)
            ld      a,d
            rlca
            rlca
            ld      d,a
            and     $03
            cp      $03
            jr      z,$AF5C
            dec     e
            jr      nz,$AF4C
            jp      $AF7B
            di
            ld      ($DD9E),a
            push    iy
            ex      (sp),ix
            push    bc
            ld      hl,$AB9E
            call    $1F49
            ld      hl,$1D48
            ld      (ix+$05),l
            ld      (ix+$06),h
            set     5,(ix+$00)
            pop     bc
            pop     ix
            ld      a,b
            dec     a
            jp      p,$AF84
            xor     a
            jp      $AF8B
            cp      $3E
            jp      c,$AF8B
            ld      a,$3D
            ld      b,a
            call    $AE17
            ld      hl,$0001
            jr      $AF97
            ld      hl,$0000
            xor     a
            ld      ($DDA0),a
            res     0,(iy+$00)
            pop     iy
            pop     ix
            push    hl
            exx
            DW      _DSPATCH
;******************************************************************************************
            exx
            pop     de
            ld      b,e
            pop     de
            ld      c,e
            push    ix
            push    iy
            ld      iy,($DDA2)
            call    $ADF8
            ld      a,(hl)
            and     a
            jp      z,$AFC7
            ld      hl,($D940)
            ld      a,h
            or      l
            jp      z,$AFC7
            call    $AE17
            pop     iy
            pop     ix
            exx
            DW      _DSPATCH
;******************************************************************************************
            scf
            ld      (de),a
            add     a,l
            xor     c
            xor     b
            xor     c
            pop     bc
            xor     c
            rst     $08
            jp      p,$9837
            nop
            ld      l,l
            nop
            and     h
            DB      $dd,$f7
            nop
            halt
            nop
            jr      nz,$B052
            nop
            xor     d
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            adc     a,e
            dec     l
            ld      l,l
            nop
            and     a
            call    c,$00F7
            ld      l,l
            nop
            and     (hl)
            jp      (ix)
            nop
            ld      l,l
            nop
            add     a,b
            ld      (bc),a
            dec     bc
            ld      bc,$00A4
            ld      l,l
            nop
            add     a,(hl)
            ret     nc
            rst     $30
            nop
            ld      l,l
            nop
            xor     b
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            jr      nz,$B01E
            ld      bc,$00A4
            ld      l,l
            nop
            adc     a,b
            ret     nc
            rst     $30
            nop
            sub     e
            dec     l
            halt
            nop
            ld      b,$76
            nop
            ld      b,$76
            nop
            jr      nc,$B09E
            nop
            jr      nc,$B025
            dec     l
            call    $D604
            dec     hl
            ret     nc
            inc     b
            ld      l,h
            xor     d
            adc     a,e
            jr      z,$AFD5
            nop
            sbc     a,h
            ld      (hl),$76
            nop
            djnz    $AFD6
            nop
            ld      b,h
            ld      (bc),a
            halt
            nop
            ex      af,af'
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            halt
            nop
            inc     bc
            ld      e,b
            dec     b
            ld      hl,($7601)
            nop
            jr      nc,$B0AB
            dec     b
            halt
            nop
            ex      af,af'
            dec     bc
            ld      bc,$AFA7
            sub     $2B
            adc     a,e
            jr      z,$AFF6
            ld      (bc),a
            halt
            nop
            djnz    $B07C
            inc     bc
            adc     a,l
            ld      (bc),a
            dec     bc
            ld      bc,$00A4
            ld      (de),a
            ld      (hl),$98
            nop
            ret     p
            ld      (bc),a
            halt
            nop
            inc     b
            ret     p
            ld      (bc),a
            ret     nc
            inc     b
            ld      e,a
            ld      (bc),a
            halt
            nop
            ld      b,$76
            nop
            ld      b,$8D
            ld      (bc),a
            halt
            nop
            ex      af,af'
            rla
            inc     bc
            halt
            nop
            ld      b,b
            dec     bc
            ld      bc,$00A4
            jp      m,$5F2D
            ld      (bc),a
            halt
            nop
            add     hl,bc
            sbc     a,h
            ld      (hl),$12
            ld      (hl),$76
            nop
            rlca
            sbc     a,b
            nop
            ret     p
            ld      (bc),a
            halt
            nop
            rlca
            halt
            nop
            inc     b
            ret     p
            ld      (bc),a
            ret     nc
            inc     b
            halt
            nop
            inc     b
            ld      e,b
            dec     b
            sub     e
            nop
            adc     a,$AF
            jp      (hl)
            nop
            pop     hl
            djnz    $B126
            nop
            scf
            ret     nc
            ret     p
            nop
            halt
            nop
            dec     b
            xor     b
            ld      bc,_0BRANCH
            call    $6DB0
            nop
            sbc     a,b
            ld      (de),a
            pop     hl
            djnz    $B13B
            nop
            add     a,l
            ld      de,$10E1
            ld      l,l
            nop
            scf
            ret     nc
            ret     p
            nop
            ld      hl,($7601)
            nop
            dec     b
            ld      h,$04
            and     h
            nop
            cp      e
            inc     a
            sub     e
            nop
            ld      h,$13
            jp      (hl)
            nop
            pop     hl
            djnz    $B163
            nop
            and     b
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            halt
            nop
            ld      b,$58
            dec     b
            halt
            nop
            inc     a
            ld      e,b
            dec     b
            halt
            nop
            ld      (bc),a
            dec     bc
            ld      bc,$AFA7
            sub     $2B
            adc     a,e
            jr      z,$B168
            ld      (bc),a
            sub     d
            daa
            sbc     a,(hl)
            nop
            ld      l,l
            nop
            ld      c,a
            exx
            rst     $30
            nop
            ld      h,c
            nop
            rst     $08
            ret     m
            xor     (hl)
            jp      pe,$3A03
            or      c
            halt
            nop
            jr      nz,$B0DB
            jr      z,$B190
            nop
            sbc     a,(hl)
            jp      (ix)
            nop
            jp      pe,$3803
            or      c
            ld      l,l
            nop
            nop
            djnz    $B0EB
            jr      z,$B109
            xor     a
            jp      po,$3A03
            or      c
            out     ($AA),a
            ld      h,c
            nop
            ld      a,($DD9E)
            and     a
            ret     nz
            ld      c,$0D
            call    $2114
            ret     z
            res     7,(iy+$00)
            set     6,(iy+$00)
            call    $2E67
            ret
            inc     d
            inc     a
            or      c
            ld      c,$05
            rra
            dec     b
            or      b
            ld      a,(bc)
            dec     a
            inc     sp
            inc     b
            ld      (hl),l
            call    m,$0454
            ld      (hl),l
            call    m,$CF54
            halt
            nop
            ld      b,b
            jp      (hl)
            ld      (_LIT),a
            adc     a,d
            ret     nc
            ret     p
            nop
            halt
            nop
            ld      (bc),a
            ld      c,d
            inc     b
            ld      l,l
            nop
            adc     a,d
            ret     nc
            cp      $00
            rra
            xor     d
            ld      h,c
            nop
            rst     $08
            sbc     a,b
            nop
            adc     a,e
            dec     b
            ld      a,e
            dec     (hl)
            xor     (hl)
            add     hl,hl
            halt
            nop
            ld      h,$CD
            add     hl,bc
            halt
            nop
            inc     c
            ld      l,l
            nop
            sbc     a,d
            DB      $dd,$f7
            nop
            halt
            nop
            ld      (bc),a
            or      l
            scf
            ld      l,l
            nop
            ld      h,(hl)
            or      c
            ld      l,l
            nop
            ld      l,c
            exx
            rst     $30
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            xor     e
            exx
            rst     $30
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            and     b
            DB      $dd,$f7
            nop
            sbc     a,b
            nop
            ld      l,l
            nop
            sbc     a,(hl)
            DB      $dd,$f7
            nop
            adc     a,a
            xor     e
            ld      l,l
            nop
            adc     a,l
            xor     h
            ld      l,l
            nop
            ld      d,e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            rst     $38
            rst     $38
            ld      l,l
            nop
            ld      c,e
            exx
            rst     $30
            nop
            ld      l,l
            nop
            ld      d,e
            or      c
            ld      l,l
            nop
            ld      h,a
            exx
            rst     $30
            nop
            ld      b,(hl)
            inc     sp
            dec     a
            dec     d
            ld      l,l
            nop
            and     d
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            and     c
            ret     nc
            inc     bc
            ld      bc,_LIT
            scf
            ret     nc
            ret     p
            nop
            sbc     a,(hl)
            nop
            jp      c,$EA01
            inc     bc
            dec     b
            or      d
            ld      l,l
            nop
            ld      b,b
            rst     $38
            jp      po,$0903
            or      d
            ld      l,l
            nop
            add     a,b
            rst     $38
            ld      l,l
            nop
            and     h
            DB      $dd,$f7
            nop
            halt
            nop
            ld      (hl),b
            ld      l,l
            nop
            xor     d
            DB      $dd,$f7
            nop
            rst     $10
            scf
            ld      l,l
            nop
            dec     c
            xor     h
            sbc     a,b
            nop
            halt
            nop
            or      d
            ld      l,l
            nop
            and     d
            jp      (ix)
            nop
            ld      h,$20
            ld      l,l
            nop
            scf
            ret     nc
            ret     p
            nop
            jp      pe,$8503
            or      d
            dec     a
            dec     d
            ld      l,l
            nop
            xor     h
            DB      $dd,$f7
            nop
            dec     a
            dec     d
            ld      l,l
            nop
            xor     (hl)
            DB      $dd,$f7
            nop
            ld      l,l
            nop
            and     d
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            ld      b,$98
            nop
            ld      l,l
            nop
            ld      e,b
            inc     a
            halt
            nop
            ld      b,e
            ld      l,l
            nop
            or      d
            ld      bc,_LIT
            xor     h
            jp      (ix)
            nop
            ld      a,$39
            ld      l,l
            nop
            and     d
            jp      (ix)
            nop
            ld      l,l
            nop
            nop
            ld      b,$6D
            nop
            nop
            ld      (hl),$6D
            nop
            ld      e,b
            inc     a
            halt
            nop
            ld      b,e
            ld      l,l
            nop
            or      d
            ld      bc,_LIT
            xor     (hl)
            jp      (ix)
            nop
            ld      a,$39
            ld      h,c
            nop
            rst     $08
            inc     c
            dec     hl
            cp      a
            xor     l
            adc     a,e
            jr      z,$B294
            inc     (hl)
            cp      h
            ld      ($B116),a
            ld      h,c
            nop
            rst     $08
            add     a,c
            or      c
            and     a
            djnz    $B21C
            xor     e
            rra
            xor     d
            halt
            nop
            add     hl,bc
            halt
            nop
            ld      (bc),a
            ld      l,l
            nop
            ld      e,(hl)
            or      c
            rlca
            scf
            add     a,a
            or      d
            ld      l,l
            nop
            ld      c,a
            exx
            jp      (hl)
            nop
            jp      pe,$AB03
            or      d
            halt
            nop
            ld      (bc),a
            jr      z,$B2F3
            ld      h,c
            nop
            rst     $08
            add     a,c
            or      c
            halt
            nop
            add     hl,bc
            sbc     a,(hl)
            nop
            ld      l,l
            nop
            ld      e,(hl)
            or      c
            rlca
            scf
            add     a,b
            xor     e
            rra
            xor     d
            ld      l,l
            nop
            nop
            ld      b,$98
            nop
            ld      b,h
            ld      (bc),a
            jr      $B309
            sbc     a,b
            rrca
            cp      a
            xor     l
            ld      e,a
            ld      (bc),a
            sbc     a,(hl)
            nop
            jr      z,$B31B
            ld      h,c
            nop
            and     d
            nop
            add     a,b
            jp      nc,$FA8A
            sub     c
            inc     sp
            sbc     a,(hl)
            cp      d
            and     (hl)
            nop
            ld      (hl),l
            call    m,$004A
            ld      (hl),l
            call    m,$044A
            ld      (hl),l
            call    m,$004A
            nop
            nop
            nop
            rst     $08
            ld      l,l
            nop
            dec     sp
            ret     nc
            ld      l,a
            ld      d,$6D
            nop
            jr      c,$B2DC
            ret     p
            nop
            jp      pe,$2803
            or      e
            halt
            nop
            djnz    $B30E
            ld      (bc),a
            halt
            nop
            ld      b,b
            dec     d
            ld      (bc),a
            call    po,$EA01
            inc     bc
            jr      z,$B2D5
            ld      l,l
            nop
            dec     sp
            ret     nc
            ld      h,a
            ld      d,$24
            dec     (hl)
            ld      h,c
            nop
            rst     $08
            call    $9804
            nop
            adc     a,e
            dec     b
            ld      bc,$7BB3
            dec     (hl)
            xor     (hl)
            add     hl,hl
            sbc     a,b
            nop
            sbc     a,h
            ld      (hl),$CD
            inc     b
            ld      h,c
            nop
            rst     $08
            ld      l,l
            nop
            pop     af
            or      d
            ld      a,h
            dec     b
            ld      h,c
            nop
            exx
            ld      bc,$0016
            in      a,(c)
            ld      a,b
            add     a,$02
            ld      b,a
            cp      $0C
            jp      nz,$B34E
            exx
            DW      _DSPATCH
;******************************************************************************************
LB35C:      exx
            ld      bc,$0E15
            in      a,(c)
            exx
            DW      _DSPATCH

;******************************************************************************************
W_B365:
            DB      _ENTER
            DW      _LIT
            DW      MISSION
            DW      _Bat
            DW      _1minus
            DW      _ARRAY
            DW      $B2E7
            DW      _at
            DW      _plus
            DW      _at
            DW      $B301
            DW      $1472
            DW      _RETURN

;******************************************************************************************
W_B37E:
            DB      _ENTER
            DW      _LIT
            DW      $2000
            DW      _0
            DW      _LITbyte
            DB      $04
            DW      _0
            DW      _DO
            DW      _I
            DW      $3FD4
            DW      _LOOP
            DW      _LITbyte
            DB      $0D
            DW      _LITbyte
            DB      $08
            DW      _DO
            DW      _I
            DW      $3FD4
            DW      _LOOP
            DW      _2DROP
            DW      _LIT
            DW      $0400
            DW      _0
            DW      _DO
            DW      _I
            DW      _LITbyte
            DB      $20
            DW      _slash
            DW      _LITbyte
            DB      $06
            DW      _MOD
            DW      $3CBB
            DW      $3F7F
            DW      _LOOP
            DW      _RETURN

;##########################################################################################
;       DATA 'PUSH TALK PRIM
;       P OO1 IU SH PA1 AY Y1 P L PA2 AY ER PA0 pB UH1 DT T EH2 N N PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Push a player button."
; Role:   Complete ready-to-play utterance; SPKCOIN.
;******************************************************************************************
SPK_PUSH:
            DB      $15
            DB      $25,$16,$76,$51,$06,$21,$22,$25
            DB      $18,$45,$61,$3A,$03,$4E,$72,$04
            DB      $2A,$01,$0D,$0D,$3E

;##########################################################################################
;       DATA 'DOOM TALK PRIM
;       Y1 IU U1 U1 W I1 L M pE pE1 T pA1 pE1 G DT O1 R pF Y pA1 N pD U1 U M PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "You will meet a Gorfian doom"
; Role:   Mission-start prefix; wrapper appends SPACE + current rank.
;******************************************************************************************
SPK_DOOM:
            DB      $1A
            DB      $22,$36,$77,$77,$2D,$0B,$58,$0C
            DB      $6C,$7C,$6A,$06,$3C,$1C,$04,$75
            DB      $6B,$1D,$29,$06,$0D,$5E,$77,$68
            DB      $0C,$3E

;##########################################################################################
;       DATA 'SURVIVAL TALK PRIM
;       S ER V AH2 I1 Y1 V UH3 L I1 S PA0 I1 M P AH1 S I1 pB L PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Survival is impossible"
; Role:   Mission-start prefix; wrapper appends SPACE + current rank.
;******************************************************************************************
SPK_SURVIVAL:
            DB      $15
            DB      $5F,$7A,$0F,$08,$0B,$62,$4F,$23
            DB      $18,$0B,$1F,$03,$0B,$4C,$65,$15
            DB      $5F,$4B,$0E,$18,$3E

;##########################################################################################
;       DATA 'ROBOWARRIOR TALK PRIM
;       R O1 U1 pB AH1 T W O R AY Y1 EH3 R S PA1 S pE K0
;       pAE1 EH3 N pD pD pE1 S T R O1 I1 Y THV UH ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Robot warriors, seek and destroy the"
; Role:   Mission-start prefix; wrapper appends SPACE + current rank.
;******************************************************************************************
SPK_ROBOWARRIOR:
            DB      $20
            DB      $2B,$75,$77,$0E,$15,$2A,$2D,$66
            DB      $6B,$61,$62,$00,$2B,$1F,$3E,$1F
            DB      $6C,$59,$2F,$00,$0D,$1E,$1E,$3C
            DB      $1F,$2A,$2B,$35,$0B,$29,$38,$33

;##########################################################################################
;       DATA 'GORFIAN TALK PRIM
;       M AH2 I1 Y G DT O1 R pF Y pA1 N PA0 R O1 U1 pB AH1 T S
;       PA0 AH1 R UH2 N pB pE1 AY T UH3 pB L PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "My Gorfian robots are unbeatable!"
; Role:   Complete mission-start utterance; SPEAKSTART.
;******************************************************************************************
SPK_GORFIAN:
            DB      $22
            DB      $0C,$08,$4B,$69,$1C,$04,$75,$6B
            DB      $1D,$29,$06,$0D,$03,$2B,$75,$77
            DB      $0E,$15,$2A,$1F,$03,$55,$6B,$31
            DB      $0D,$4E,$7C,$61,$6A,$23,$0E,$23
            DB      $18,$3E

;##########################################################################################
;       DATA 'IAM TALK PRIM
;       AH1 I1 Y pAE1 EH2 M THV UH G DT O1 R pF Y pA1 N
;       PA0 K0 AH1 N CH EH S N EH S PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "I am a Gorfian consciousness."
; Role:   Complete mission-start utterance; SPEAKSTART.
;******************************************************************************************
SPK_IAM:

            DB      $1B
            DB      $15,$0B,$69,$2F,$41,$4C,$38,$33
            DB      $1C,$04,$35,$2B,$5D,$29,$06,$0D
            DB      $03,$59,$55,$0D,$50,$3B,$1F,$4D
            DB      $3B,$1F,$3E

;##########################################################################################
;       DATA 'PREPARE TALK PRIM
;       P R pE1 P pAE1 ER Y U1 O1 R S EH1 L pF PA0 pF O1 R PA0
;       UH1 N AH2 I1 Y H pA1 SH UH2 N PA1 ENDPRIM
;##########################################################################################
;******************************************************************************************
; Speech: "Prepare yourself for annihilation"
; Role:   Mission-start prefix; wrapper appends SPACE + current rank.
;******************************************************************************************
SPK_PREPARE:

            DB      $1E
            DB      $25,$2B,$7C,$25,$6F,$7A,$29,$37
            DB      $75,$6B,$5F,$02,$18,$1D,$03,$1D
            DB      $35,$2B,$03,$32,$0D,$08,$4B,$69
            DB      $18,$46,$11,$31,$0D,$3E

;******************************************************************************************
; ----> SPKCOIN    Ready-to-play credit speech selector.
;
;                  Runs only while no primitive is active and COINSIN is non-zero. It sets
;                  GOYFLAG and queues one of:
;
;                       "Push a player button."
;                       "Long live Gorf!"
;
;                  This is the ready-to-play speech path, distinct from joystick-triggered
;                  attract chatter in goyak.
;******************************************************************************************
SPKCOIN:
            DB      _ENTER
            DW      _LIT
            DW      PHONECOUNT
            DW      _Bat
            DW      _zeroequal
            DW      _0BRANCH
            DW      spkcoin2
            DW      _LIT
            DW      COINSIN
            DW      _Bat
            DW      _0BRANCH
            DW      spkcoin2
            DW      _LIT
            DW      GOYFLAG
            DW      _BONE
            DW      _LITbyte
            DB      $02
            DW      _RND
            DW      _0BRANCH
            DW      spkcoin0
            DW      _LIT
            DW      SPK_PUSH            ; Push a player button
            DW      _BRANCH
            DW      spkcoin1
spkcoin0:   DW      _LIT
            DW      SPK_LONG
spkcoin1:   DW      _SPEAK
spkcoin2:   DW      _RETURN

;******************************************************************************************
; ----> SPEAKGORF        Return complete mission-start primitive: "I am the Gorfian Empire."
;******************************************************************************************
SPEAKGORF:
            DB      _ENTER
            DW      _LIT
            DW      SPK_GORF
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKROBOTS      Return complete mission-start primitive: "Gorfian robots... attack! attack!"
;******************************************************************************************
SPEAKROBOTS:
            DB      _ENTER
            DW      _LIT
            DW      SPK_ROBOTS
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKDOOM        Queue "You will meet a Gorfian doom", queue "Space", return current-rank primitive.
;******************************************************************************************
SPEAKDOOM:
            DB      _ENTER
            DW      _LIT
            DW      SPK_DOOM
            DW      _SPEAK
            DW      _GETRANK
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKSURVIVAL    Queue "Survival is impossible", queue "Space", return current-rank primitive.
;******************************************************************************************
SPEAKSURVIVAL:
            DB      _ENTER
            DW      _LIT
            DW      SPK_SURVIVAL
            DW      _SPEAK
            DW      _GETRANK
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKESCAPE      Return complete mission-start primitive: "You cannot escape the Gorfian robots."
;******************************************************************************************
SPEAKESCAPE:
            DB      _ENTER
            DW      _LIT
            DW      SPK_ESCAPE
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKROBOWARRIOR Queue "Robot warriors, seek and destroy the", queue "Space", return current-rank primitive.
;******************************************************************************************
SPEAKROBOWARRIOR:
            DB      _ENTER
            DW      _LIT
            DW      SPK_ROBOWARRIOR
            DW      _SPEAK
            DW      _GETRANK
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKGORFIAN     Return complete mission-start primitive: "My Gorfian robots are unbeatable!"
;******************************************************************************************
SPEAKGORFIAN:
            DB      _ENTER
            DW      _LIT
            DW      SPK_GORFIAN
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKIAM         Return complete mission-start primitive: "I am a Gorfian consciousness."
;******************************************************************************************
SPEAKIAM:
            DB      _ENTER
            DW      _LIT
            DW      SPK_IAM
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKPREPARE     Queue "Prepare yourself for annihilation", queue "Space", return current-rank primitive.
;******************************************************************************************
SPEAKPREPARE:
            DB      _ENTER
            DW      _LIT
            DW      SPK_PREPARE
            DW      _SPEAK
            DW      _GETRANK
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKPRIS        Return complete mission-start primitive: "Gorfians take no prisoners!"
;******************************************************************************************
SPEAKPRIS:
            DB      _ENTER
            DW      _LIT
            DW      SPK_PRIS
            DW      _RETURN
;******************************************************************************************
; ----> SPEAKSTART  Mission-start taunt selector.
;
; MSYRND drives UNEQRND across ten entries, preventing the immediately preceding mission
; start message from repeating. Each CASES entry either returns a complete TALK PRIM or
; queues a prefix plus GETRANK and returns the final rank primitive. The common SPEAK after
; the table submits that returned primitive.
;
; Complete entries:
;       I am the Gorfian Empire.
;       Gorfian robots... attack! attack!
;       You cannot escape the Gorfian robots.
;       My Gorfian robots are unbeatable.
;       I am a Gorfian consciousness.
;       Gorfians take no prisoners.
;
; Rank-completed entries:
;       You will meet a Gorfian doom, Space <rank>.
;       Survival is impossible, Space <rank>.
;       Robot warriors, seek and destroy the Space <rank>.
;       Prepare yourself for annihilation, Space <rank>.
;******************************************************************************************
SPEAKSTART:
            DB      _ENTER
            DW      _LIT
            DW      MSYRND
            DW      _LITbyte
            DB      $0A
            DW      _UNEQRND
            DW      _CASES
            DW      $B532
            DW      SPEAKGORF          ; I am the Gorfian Empire.
            DW      SPEAKROBOTS        ; Gorfian robots... attack! attack!
            DW      SPEAKDOOM          ; You will meet a Gorfian doom, Space <rank>.
            DW      SPEAKSURVIVAL      ; Survival is impossible, Space <rank>.
            DW      SPEAKESCAPE        ; You cannot escape the Gorfian robots.
            DW      SPEAKROBOWARRIOR   ; Robot warriors, seek and destroy the Space <rank>.
            DW      SPEAKGORFIAN       ; My Gorfian robots are unbeatable!
            DW      SPEAKIAM           ; I am a Gorfian consciousness.
            DW      SPEAKPREPARE       ; Prepare yourself for annihilation, Space <rank>.
            DW      SPEAKPRIS          ; Gorfians take no prisoners!
            DW      _SPEAK
            DW      _RETURN
;******************************************************************************************
; ----> TURNINTRO   Common player-turn mission presentation.
;
;                   Clears the display, updates the cabinet rank lamps from SKILLFACTOR,
;                   starts one non-repeating mission taunt, then invokes the current-mission
;                   presentation path through W_B365. The one-player and two-player start
;                   paths and the player-turn transition path all call this word.
;******************************************************************************************
_TURNINTRO:
            DB      _ENTER
            DW      _0
            DW      _FLOOD
            DW      LB35C
            DW      _LIT
            DW      SKILLFACTOR
            DW      _Bat
            DW      _LITERANK
            DW      SPEAKSTART
            DW      _1
            DW      W_B365
            DW      LB35C
            DW      _RETURN
            DW      $C031
            DW      $DDD2
            DW      $4021
            DW      $FDD3
            DW      $5A21
            DW      $0300
            DW      _DSPATCH
            DW      $C3C1
            DW      $B54F
;******************************************************************************************
; ----> W_B561      Mission/rank progression.
;
;                   Advances MISSIONCTR and MISSION. When MISSION reaches 6, the five-mission
;                   cycle is complete: qualifying fire-base awards are applied, SKILLFACTOR
;                   is incremented, and MISSION wraps to 1. This is the rank-state transition
;                   following completion of the Flag Ship cycle.
;
;                   The speech vocabulary contains the matching compound promotion line:
;                       SPK_PROMOTE + SPK_SPACE + current rank
;                   No decoded resident control-flow path directly references that primitive.
;******************************************************************************************
W_B561:
            DB      _ENTER
            DW      _LIT
            DW      $D951
            DW      _at
            DW      _zeroequal
            DW      _0BRANCH
            DW      $B5CF
            DW      _LIT
            DW      MISSIONCTR
            DW      _BCDBUMP
            DW      _LIT
            DW      MISSION
            DW      _PINC
            DW      _LIT
            DW      MISSION
            DW      _Bat
            DW      _LITbyte
            DB      $06
            DW      _equal
            DW      _0BRANCH
            DW      $B5CF
            DW      _LIT
            DW      SKILLFACTOR
            DW      _Bat
            DW      _zeroequal
            DW      _0BRANCH
            DW      $B5C3
            DW      _LITbyte
            DB      $13
            DW      _INP
            DW      _LITbyte
            DB      $20
            DW      _AND
            DW      _0BRANCH
            DW      $B5C3
            DW      _LIT
            DW      P1FBCTR
            DW      _Bat
            DW      _0BRANCH
            DW      $B5B3
            DW      _LIT
            DW      P1FBCTR
            DW      _PINC
            DW      _LIT
            DW      P2FBCTR
            DW      _Bat
            DW      _0BRANCH
            DW      $B5C3
            DW      _LIT
            DW      P2FBCTR
            DW      _PINC
            DW      _LIT
            DW      SKILLFACTOR
            DW      _PINC
            DW      _LIT
            DW      MISSION
            DW      _P1
            DW      _RETURN

;******************************************************************************************
W_B5D1:
            DB      _ENTER
            DW      _LIT
            DW      $D09F
            DW      _bang
            DW      $288B
            DW      $2F18
            DW      _LIT
            DW      $D09F
            DW      _at
            DW      _zeroequal
            DW      _0BRANCH
            DW      $B5D8
            DW      _RETURN
;******************************************************************************************
W_B5EA:
            DB      _ENTER
            DW      $B32C
            DW      _DI
            DW      _LITbyte
            DB      $F0
            DW      _XUP
            DW      _LITbyte
            DB      $02
            DW      _SPELL
            DW      _LITbyte
            DB      $C8
            DW      _XUP
            DW      _LITbyte
            DB      $04
            DW      _LIT
            DW      PLAYERUP
            DW      _Bat
            DW      _minussign
            DW      _SPELL
            DW      $B341
            DW      _LITbyte
            DB      $05
            DW      _0
            DW      _DO
            DW      _DUP
            DW      _LITbyte
            DB      $A0
            DW      _XUP
            DW      _SWAP
            DW      _SPELL
            DW      _DUP
            DW      _1plus
            DW      _LITbyte
            DB      $78
            DW      _XUP
            DW      _SWAP
            DW      _SPELL
            DW      _EI
            DW      _LITbyte
            DB      $12
            DW      W_B5D1
            DW      _DI
            DW      _LOOP
            DW      _DROP
            DW      _RETURN
;******************************************************************************************
W_B63B:
            DB      _ENTER
            DW      _LITbyte
            DB      $13
            DW      _INP
            DW      _LITbyte
            DB      $10
            DW      _AND
            DW      _zeroequal
            DW      _2plus
            DW      _RETURN
;******************************************************************************************
W_B64C:
            DB      _ENTER
            DW      W_B63B
            DW      _LIT
            DW      COINSIN
            DW      _Bat
            DW      _1
            DW      _gt
            DW      _0BRANCH
            DW      $B666
            DW      _2splat
            DW      _LITbyte
            DB      $02
            DW      _BRANCH
            DW      $B668
            DW      _1
            DW      _RETURN
;******************************************************************************************
W_B66A:
            DB      _ENTER
            DW      W_B63B
            DW      _LIT
            DW      COINSIN
            DW      _Bat
            DW      _LITbyte
            DB      $03
            DW      _gt
            DW      _0BRANCH
            DW      $B687
            DW      _2splat
            DW      _DUP
            DW      _LITbyte
            DB      $04
            DW      _BRANCH
            DW      $B68C
            DW      _DUP
            DW      _LITbyte
            DB      $02
            DW      _RETURN
;******************************************************************************************
W_B68E:
            DB      _ENTER
            DW      _LITbyte
            DB      $13
            DW      _INP
            DW      _LITbyte
            DB      $40
            DW      _AND
            DW      _zeroequal
            DW      _RETURN
;******************************************************************************************
W_B69D:
            DB      _ENTER
            DW      W_B68E
            DW      _0BRANCH
            DW      $B6AD
            DW      _LITbyte
            DB      $04
            DW      _LIT
            DW      COINSIN
            DW      _PWB
            DW      _RETURN
;******************************************************************************************
W_B6AF:
            DB      _ENTER
            DW      _STARTGAME
            DW      _LIT
            DW      DEMOMODE
            DW      _P0
            DW      _1
            DW      _LIT
            DW      MUSICFLAG
            DW      _Bbang
            DW      _LIT
            DW      MISSION
            DW      _P1
            DW      _LIT
            DW      $D951
            DW      _ZERO
            DW      _LIT
            DW      RIP
            DW      _P0
            DW      _RETURN
;******************************************************************************************
W_B6D4:
            DB      _ENTER
            DW      _LIT
            DW      $B2F9
            DW      _COLOR
            DW      _LITbyte
            DB      $08
            DW      $369C
            DW      _LITbyte
            DB      $02
            DW      _LITbyte
            DB      $09
            DW      _OUTP
            DW      _RETURN

;******************************************************************************************
CHECKSCORE:
            exx
            pop     de
            pop     hl
            ld      b,$03
            inc     hl
            inc     hl
            inc     de
            inc     de
CS0:        ld      a,(de)
            cp      (hl)
            jr      c,CS2
            jr      nz,CS1
            dec     hl
            dec     de
            djnz    CS0
CS1:        ld      hl,$0000
            jr      CS3
CS2:        ld      hl,$0001
CS3:        push    hl
            exx
            DW      _DSPATCH

;******************************************************************************************
WRITESCORE:
            exx
            pop     hl
            pop     de
            ld      b,$03
WS0:        ld      a,(de)
            push    de
            ld      e,a
            call    wpb_bang            ; Write byte to protected memory
            pop     de
            inc     hl
            inc     de
            djnz    WS0
            exx
            DW      _DSPATCH

;******************************************************************************************
; get byte at $DDA0
;******************************************************************************************
READ_DDA0:
            DB      _ENTER
            DW      _LIT
            DW      $DDA0
            DW      _at
            DW      _RETURN

;******************************************************************************************
; Check score against high score table - insert score if good enough
;******************************************************************************************

SCANHST:
            DB      _ENTER              ; Player score and Address of relevant high score table on stack
            DW      _LITbyte
            DB      $05
            DW      _0
            DW      _DO                 ; Start loop I - 0 to 4
            DW      _2DUP               ; Make copy of passed variables
            DW      _I                  ; Loop Counter
            DW      _LITbyte
            DB      $03
            DW      _star               ; * 3
            DW      _plus               ; add to base address of array
            DW      CHECKSCORE          ; Compare against this High Score entry
            DW      _0BRANCH            ; Lower, so next loop
            DW      scanhstlp

            DW      _I                  ; is the score position the final one
            DW      _LITbyte
            DB      $04
            DW      _not_equal		; pushes a 1 if I not equal 4
            DW      _0BRANCH            ; but branches if 0 on stack
            DW      scanhst0            ; I=4 so replace final score

            DW      _I
            DW      _LITbyte
            DB      $03
            DW      _DO                 ; start new loop from 3 downto I
            DW      _DUP                ; duplicate address of table
            DW      _I                  
            DW      _LITbyte
            DB      $03
            DW      _star               ; I * 3
            DW      _plus               ; plus base of table
            DW      _DUP                ; duplicate
            DW      _LITbyte
            DB      $03                 
            DW      _plus               ; add 3 so we have address of score I and score I+1
            DW      WRITESCORE          ; copy score I to score I+1
            DW      _LIT
            DW      $FFFF               ; -1
            DW      _plusLOOP           ; loop until we reach I (from first loop)

scanhst0:   DW      _2DUP
            DW      _I
            DW      _LITbyte
            DB      $03
            DW      _star
            DW      _plus               ; get address of score we replace
            DW      _2DUP
            DW      WRITESCORE          ; write it out
            DW      _SWAP
            DW      _LITbyte
            DB      $03
            DW      _plus
            DW      _PWB                ; write E to HL
            DW      _1
            DW      _LIT
            DW      $DD9E
            DW      _bang               ; write 1 to DD9E
            DW      _LEAVE              ; exit loops
scanhstlp:  DW      _LOOP
            DW      _DROP               ; remove duplicates from stack
            DW      _DROP
            DW      _RETURN             ; return

;******************************************************************************************
; Get the base address of the relevant high score table
;******************************************************************************************
GETHSARRAY:
            DB      _ENTER
            DW      _LIT
            DW      INITFB
            DW      _Bat
            DW      _less
            DW      _0BRANCH
            DW      geths0
            DW      _HS4
            DW      _BRANCH
            DW      geths1
geths0:     DW      _HS2
geths1:     DW      SCANHST             ; Check to see if score makes table
            DW      _RETURN

;******************************************************************************************
W_B7AD:
            DB      _ENTER
            DW      READ_DDA0
            DW      _0BRANCH
            DW      $B7CD
            DW      _LITbyte
            DB      $80
            DW      _star
            DW      READ_DDA0
            DW      _SWAP
            DW      _minussign
            DW      _LIT
            DW      $DDA0
            DW      _bang
            DW      _EI
            DW      $2F18
            DW      _BRANCH
            DW      $B7CF
            DW      $3C62
            DW      _RETURN

;******************************************************************************************
W_B7D1:
            DB      _ENTER
            DW      _LITbyte
            DB      $FF
            DW      _AND
            DW      _DUP
            DW      _LITbyte
            DB      $03
            DW      _BARRAY
            DW      P1SCR
            DW      _Bat
            DW      _equal
            DW      _0BRANCH
            DW      $B7FE
            DW      READ_DDA0
            DW      _LIT
            DW      $7800
            DW      _LIT
            DW      GORF_PLAYER1
            DW      _LIT
            DW      $0428
            DW      _WRITEP
            DW      _DROP
            DW      _BRANCH
            DW      $B81D
            DW      _LITbyte
            DB      $03
            DW      _BARRAY
            DW      P2SCR
            DW      _Bat
            DW      _equal
            DW      _0BRANCH
            DW      $B81D
            DW      READ_DDA0
            DW      _LIT
            DW      $7800
            DW      _LIT
            DW      GORF_PLAYER2
            DW      _LIT
            DW      $0428
            DW      _WRITEP
            DW      _RETURN

;******************************************************************************************
W_B81F:
            DB      _ENTER
            DW      _LITbyte
            DB      $13
            DW      _INP
            DW      _LITbyte
            DB      $10
            DW      _AND
            DW      _zeroequal
            DW      _0BRANCH
            DW      $B832
            DW      _1plus
            DW      _RETURN

;******************************************************************************************
W_B834:
            DB      _ENTER
            DW      READ_DDA0
            DW      _LIT
            DW      $0428
            DW      _ROT
            DW      _INXMSG
            DW      _CSPELL
            DW      W_B7AD
            DW      _RETURN

;******************************************************************************************
W_B845:
            DB      _ENTER
            DW      W_B81F
            DW      W_B834
            DW      _LITbyte
            DB      $05
            DW      _0
            DW      _DO
            DW      READ_DDA0
            DW      _LIT
            DW      $4B00
            DW      _LIT
            DW      $0428
            DW      _LITbyte
            DB      $04
            DW      _PICK
            DW      _LITbyte
            DB      $03
            DW      _I
            DW      _star
            DW      _plus
            DW      _DUP
            DW      W_B7D1
            DW      _DISPBCD6
            DW      _LITbyte
            DB      $08
            DW      W_B7AD
            DW      _LOOP
            DW      _DROP
            DW      _RETURN

;******************************************************************************************
W_B87A:
            DB      _ENTER
            DW      _0
            DW      _BARRAY
            DW      HISCR2
            DW      _LITbyte
            DB      $0A
            DW      _LITbyte
            DB      $1E
            DW      W_B845
            DW      _LITbyte
            DB      $02
            DW      W_B7AD
            DW      _0
            DW      _BARRAY
            DW      HISCR4
            DW      _LITbyte
            DB      $0A
            DW      _LITbyte
            DB      $20
            DW      W_B845
            DW      _RETURN

;******************************************************************************************
W_B89E:
            DB      _ENTER
            DW      _LIT
            DW      P1HSP
            DW      _P0
            DW      _LIT
            DW      P2HSP
            DW      _P0
            DW      _LIT
            DW      $DD9E
            DW      _ZERO
            DW      _1
            DW      _LIT
            DW      $D097
            DW      _Bbang
            DW      _RETURN

;******************************************************************************************
W_B8BB:
            DB      _ENTER
            DW      _LIT
            DW      COINS
            DW      _P0
            DW      _LIT
            DW      DEMOMODE
            DW      _P1
            DW      _LIT
            DW      PLAYERUP
            DW      _P0
            DW      _LIT
            DW      $DD9E
            DW      _at
            DW      _0BRANCH
            DW      $B8FA
            DW      $B32C
            DW      _LIT
            DW      GOYFLAG
            DW      _BONE
            DW      $B341
            DW      _LIT
            DW      $4000
            DW      _LIT
            DW      $DDA0
            DW      _bang
            DW      W_B87A
            DW      W_B69D
            DW      _LIT
            DW      $01C2
            DW      $B5D1
            DW      _0
            DW      _FLOOD
            DW      _0
            DW      $1472
            DW      _RETURN

;******************************************************************************************
LB900:
            DB      _ENTER
            DW      _TURNINTRO
            DW      $B561
            DW      _LIT
            DW      $D951
            DW      _at
            DW      _0BRANCH
            DW      $B901
            DW      W_B89E
            DW      _0
            DW      _BARRAY
            DW      P1SCR
            DW      _1
            DW      GETHSARRAY
            DW      W_B8BB
            DW      _RETURN

;******************************************************************************************
W_B91F:
            DB      _ENTER
            DW      _LIT
            DW      COINSIN
            DW      _Bat
            DW      _0BRANCH
            DW      $B950
            DW      $B6AF
            DW      $B63B
            DW      $B64C
            DW      _DUP
            DW      _LIT
            DW      COINSIN
            DW      _PSUB
            DW      _LIT
            DW      INITFB
            DW      _PWB
            DW      _LIT
            DW      P1FBCTR
            DW      _PWB
            DW      _LIT
            DW      P2FBCTR
            DW      _P0
            DW      _LIT
            DW      LB900
            DW      $B55D
            DW      _RETURN

;******************************************************************************************
W_B952:
            DB      _ENTER
            DW      _TURNINTRO
            DW      $B561
            DW      _LIT
            DW      $D951
            DW      _at
            DW      _0BRANCH
            DW      $B953
            DW      W_B89E
            DW      _0
            DW      _BARRAY
            DW      P1SCR
            DW      _LITbyte
            DB      $02
            DW      GETHSARRAY
            DW      _0
            DW      _BARRAY
            DW      P2SCR
            DW      _LITbyte
            DB      $03
            DW      GETHSARRAY
            DW      _LIT
            DW      INITFB
            DW      _Bat
            DW      _LITbyte
            DB      $03
            DW      _not_equal
            DW      _0BRANCH
            DW      $B9AF
            DW      _LIT
            DW      P1HSP
            DW      _Bat
            DW      _LIT
            DW      P2HSP
            DW      _Bat
            DW      _DUP
            DW      _0BRANCH
            DW      $B9AD
            DW      _gt_equal
            DW      _0BRANCH
            DW      $B9A9
            DW      _LITbyte
            DB      $03
            DW      _LIT
            DW      P1HSP
            DW      _PADD
            DW      _BRANCH
            DW      $B9AF
            DW      _2DROP
            DW      W_B8BB
            DW      _RETURN

;******************************************************************************************
W_B9B3:
            DB      _ENTER
            DW      _LITbyte
            DB      $07
            DW      W_B5EA
            DW      _LIT
            DW      RIP
            DW      _Bat
            DW      _0BRANCH
            DW      $B9C9
            DW      _LIT
            DW      W_B952
            DW      $B55D
            DW      _TURNINTRO
            DW      _LIT
            DW      $D951
            DW      _at
            DW      _0BRANCH
            DW      $B9E6
            DW      _LITbyte
            DB      $05
            DW      W_B5EA
            DW      _LIT
            DW      RIP
            DW      _P1
            DW      _LIT
            DW      $D951
            DW      _ZERO
            DW      _LIT
            DW      PLAYERUP
            DW      _Bat
            DW      _1plus
            DW      _1
            DW      _AND
            DW      _DUP
            DW      _LIT
            DW      PLAYERUP
            DW      _PWB
            DW      _zeroequal
            DW      _0BRANCH
            DW      $BA02
            DW      W_B561
            DW      _0
            DW      _0BRANCH
            DW      $B9B4
            DW      _RETURN
;******************************************************************************************
W_BA0A:
            DB      _ENTER
            DW      _LIT
            DW      COINSIN
            DW      _Bat
            DW      _1
            DW      _gt
            DW      _0BRANCH
            DW      $BA43
            DW      W_B6AF
            DW      W_B66A
            DW      _DUP
            DW      _LIT
            DW      COINSIN
            DW      _PSUB
            DW      _LIT
            DW      INITFB
            DW      _PWB
            DW      _LIT
            DW      P1FBCTR
            DW      _PWB
            DW      _LIT
            DW      P2FBCTR
            DW      _PWB
            DW      _LIT
            DW      NPLAYERS
            DW      _P1
            DW      _LIT
            DW      W_B9B3
            DW      $B55D
            DW      _RETURN

;******************************************************************************************

            exx
            pop     hl
            ld      a,l
            sub     $20
            jr      z,$BA54
            sub     $0F
            cp      $0B
            jr      c,$BA54
            sub     $07
            ld      l,a
            ld      h,$00
            add     hl,hl
            add     hl,hl
            ld      e,l
            ld      d,h
            add     hl,hl
            add     hl,de
            ld      de,CHRTBL
            add     hl,de
            ex      de,hl
            pop     hl
            ld      (hl),$0C
            inc     hl
            ld      (hl),$18
            inc     hl
            ld      c,$06
            ld      b,$04
            push    bc
            ld      a,(de)
            ld      b,$08
            rla
            jr      nc,$BA78
            ld      (hl),$FF
            jr      $BA7A
            ld      (hl),$00
            inc     hl
            djnz    $BA71
            ld      b,$04
            inc     de
            ld      a,(de)
            rla
            jr      nc,$BA88
            ld      (hl),$FF
            jr      $BA8A
            ld      (hl),$00
            inc     hl
            djnz    $BA81
            dec     de
            pop     bc
            djnz    $BA6D
            inc     de
            inc     de
            dec     c
            jr      nz,$BA6B
            exx
            DW      _DSPATCH
;******************************************************************************************
            inc     c
            nop
            nop
            nop
            rst     $38
            ld      c,$F0
            rst     $38
            nop
            nop
            ld      b,$10
            djnz    $BAB3
            djnz    $BAAA
            nop
            rst     $38
            ld      b,$10
            djnz    $BAB3
            ld      ($141E),hl
            call    p,$1605
            nop
            jr      c,$BAD0
            nop
            or      b
            ld      hl,($1060)
            ex      af,af'
            sbc     a,c
            cp      d
            ld      (de),a
            ex      af,af'
            sbc     a,c
            cp      d
            ex      af,af'
            and     (hl)
            cp      d
            ld      e,$C1
            cp      d
            ld      a,(bc)
            cp      l
            cp      d
            ex      af,af'
            xor     (hl)
            cp      d
            inc     e
            dec     b
            nop
            add     a,c
            ld      ($9908),hl
            cp      d
            nop
            adc     a,$22
            ex      af,af'
            and     (hl)
            cp      d
            ld      e,$D2
            cp      d
            nop
            add     a,c
            ld      ($BD0A),hl
            cp      d
            ex      af,af'
            xor     (hl)
            cp      d
            nop
            ld      b,d
            exx
            inc     e
            inc     b
            ld      a,(bc)
            pop     bc
            cp      d
            ex      af,af'
            xor     (hl)
            cp      d
            nop
            ld      h,h
            jp      c,$031C
            ld      a,(bc)
            pop     bc
            cp      d
            ex      af,af'
            xor     (hl)
            cp      d
            nop
            add     a,(hl)
            in      a,($1C)
            ld      (bc),a
            ld      a,(bc)
            pop     bc
            cp      d
            ex      af,af'
            xor     (hl)
            cp      d
            nop
            xor     b
            call    c,$011C
            ld      a,(bc)
            pop     bc
            cp      d
            ld      e,(ix+$0d)
            ld      d,(ix+$0e)
            ld      l,(ix+$13)
            ld      h,(ix+$14)
            ld      c,(ix+$1b)
            ld      b,(ix+$1c)
            ld      a,(ix+$29)
            call    drawchar
            ret
            call    $1C67
            push    bc
            bit     5,(ix+$00)
            jr      nz,$BB3B
            call    $BB13
            jr      $BB3F
            res     5,(ix+$00)
            pop     bc
            call    $1CB5
            call    $1C4B
            bit     6,(ix+$00)
            jr      nz,$BB51
            call    $BB13
            jr      $BB59
            set     5,(ix+$00)
            res     6,(ix+$00)
            jp      $1A6B
            inc     b
            inc     l
            cp      e
            nop
            ld      b,$24
            ld      l,$04
            jr      nz,$BBA6
            cp      a
            ld      (bc),a
            jr      z,$BB70
            ld      (hl),b
            ld      a,(bc)
            ld      l,c
            cp      e
            rst     $08
            halt
            nop
            jr      z,$BB44
            or      l
            halt
            nop
            ld      b,a
            halt
            nop
            and     d
            ld      d,a
            jr      nz,$BBDE
            nop
            rst     $08
            inc     l
            or      e
            call    nc,$98B6
            nop
            adc     a,b
            nop
            ld      b,d
            exx
            halt
            nop
            ld      b,a
            ld      b,l
            cp      d
            sbc     a,b
            nop
            adc     a,b
            nop
            ld      h,h
            jp      c,$0076
            ld      c,a
            ld      b,l
            cp      d
            sbc     a,b
            nop
            adc     a,b
            nop
            add     a,(hl)
            in      a,($76)
            nop
            ld      d,d
            ld      b,l
            cp      d
            sbc     a,b
            nop
            adc     a,b
            nop
            xor     b
            call    c,$0076
            ld      b,(hl)
            ld      b,l
            cp      d
            dec     a
            dec     d
            ld      l,l
            nop
            jp      z,$F7DD
            nop
            ld      l,l
            nop
            call    $76BA
            nop
            ld      b,a
            ld      l,l
            nop
            or      d
            ld      bc,_LIT
            jp      z,$E9DD
            nop
            ld      h,$20
            ld      l,l
            nop
            rst     $20
            cp      d
            ld      l,(hl)
            cp      e
            ld      l,l
            nop
            jp      p,$6EBA
            cp      e
            ld      l,l
            nop
            DB      $fd,$ba
            ld      l,(hl)
            cp      e
            ld      l,l
            nop
            ex      af,af'
            cp      e
            ld      l,(hl)
            cp      e
            halt
            nop
            jr      nc,$BBB7
            or      l
            halt
            nop
            add     hl,hl
            cp      e
            add     hl,bc
            ld      e,b
            ld      bc,$00A4
            ld      l,l
            nop
            add     a,b
            inc     bc
            rla
            inc     bc
            ld      l,l
            nop
            nop
            ld      h,b
            adc     a,$00
            ld      de,$6D01
            nop
            call    z,$F7DD
            nop
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            and     h
            nop
            adc     a,l
            ld      (bc),a
            dec     bc
            ld      bc,$00F0
            halt
            nop
            dec     c
            ld      l,l
            nop
            jp      z,$E9DD
            nop
            dec     bc
            ld      bc,$00E9
            halt
            nop
            inc     de
            ld      l,l
            nop
            jp      z,$E9DD
            nop
            dec     bc
            ld      bc,$00E9
            ld      l,l
            nop
            nop
            jr      $BBBC
            ld      (bc),a
            ld      l,l
            nop
            nop
            rlca
            rla
            inc     bc
            ld      l,l
            nop
            call    z,$E9DD
            nop
            dec     bc
            ld      bc,_LIT
            ld      e,h
            cp      e
            halt
            nop
            jr      nz,$BCBC
            nop
            and     b
            rra
            jr      nz,$BCC1
            nop
            djnz    $BC1F
            or      l
            ld      e,a
            ld      (bc),a
            or      c
            nop
            halt
            nop
            ld      b,b
            pop     de
            or      l
            ld      h,c
            nop
            ex      af,af'
            ex      af,af'
            ld      a,(bc)
            ex      af,af'
            ex      af,af'
            djnz    $BC69
            ex      af,af'
            ex      af,af'
            ex      af,af'
            ex      af,af'
            djnz    $BC6F
            inc     c
            rst     $08
            ld      a,(hl)
            cp      e
            ld      l,l
            nop
            sub     a
            ret     nc
            xor     (hl)
            djnz    $BCDF
            nop
            and     b
            DB      $dd,$03
            ld      bc,$04CD
            halt
            nop
            ld      c,$98
            nop
            ld      b,h
            ld      (bc),a
            adc     a,l
            ld      (bc),a
            adc     a,b
            nop
            ld      e,d
            cp      h
            ret     p
            nop
            halt
            nop
            djnz    $BC34
            ld      (bc),a
            inc     (hl)
            cp      b
            ld      e,a
            ld      (bc),a
            ld      a,d
            cp      b
            halt
            nop
            ex      af,af'
            ld      h,d
            inc     a
            halt
            nop
            ex      af,af'
            halt
            nop
            ld      ($B834),a
            halt
            nop
            ex      af,af'
            halt
            nop
            inc     sp
            inc     (hl)
            cp      b
            ld      l,l
            nop
            sub     a
            ret     nc
            or      e
            djnz    $BD25
            nop
            ld      b,b
            pop     de
            or      l
            sbc     a,b
            nop
            adc     a,e
            dec     b
            ld      h,c
            nop
            in      a,($10)
            ld      d,a
            ld      e,$FF
            ld      hl,$0002
            exx
            exx
            ld      a,e
            exx
            ld      hl,$4000
            ld      de,$4001
            ld      bc,$3FFF
            ld      (hl),a
            ldir
            in      a,($10)
            exx
            cp      d
            exx
            jr      z,$BCDB
            xor     a
            jr      $BCF5
            ld      a,(hl)
            exx
            cp      e
            jr      z,$BCEE
            inc     hl
            inc     hl
            exx
            inc     a
            out     ($00),a
            out     ($04),a
            dec     de
            ld      a,d
            or      e
            jr      nz,$BCE3
            exx
            ld      a,e
            cpl
            ld      e,a
            dec     hl
            ld      a,l
            or      h
            exx
            jr      nz,$BCC2
            exx
            di
            xor     a
            ld      i,a
            in      a,($10)
            ld      d,a
            ld      e,$FF
            ld      hl,$0002
            exx
            ld      hl,WPRAMSTART
            ld      de,DEMOMODE
            ld      bc,$0FFF
            ld      a,$A5
            out     ($5B),a
            exx
            ld      a,e
            exx
            ld      (hl),a
            ld      a,(hl)
            ex      af,af'
            ld      a,$A5
            out     ($5B),a
            ex      af,af'
            ld      (de),a
            inc     de
            inc     hl
            dec     bc
            ld      a,b
            or      c
            jr      nz,$BD16
            in      a,($10)
            exx
            cp      d
            exx
            jr      z,$BD2F
            xor     a
            jr      $BD49
            ld      a,(hl)
            exx
            cp      e
            jr      z,$BD42
            inc     hl
            inc     hl
            exx
            inc     a
            out     ($00),a
            out     ($04),a
            dec     de
            ld      a,d
            or      e
            jr      nz,$BD37
            exx
            ld      a,e
            cpl
            ld      e,a
            dec     hl
            ld      a,l
            or      h
            exx
            jr      nz,$BD05
            exx
            DW      _DSPATCH
;******************************************************************************************
GOSHOW:
            DB      _ENTER
            DW      _LITbyte
            DB      $10
            DW      _INP
            DW      _LITbyte
            DB      $20
            DW      _AND
            DW      _zeroequal
            DW      _0BRANCH
            DW      $BD61
            DW      $BA0A
            DW      _LITbyte
            DB      $10
            DW      _INP
            DW      _LITbyte
            DB      $10
            DW      _AND
            DW      _zeroequal
            DW      _0BRANCH
            DW      $BD73
            DW      W_B91F
            DW      _RETURN
;******************************************************************************************
W_BD75:
            DB      _ENTER
            DW      _0
            DW      _DO
            DW      GOSHOW
            DW      $2F18
            DW      $0F98
            DW      _LOOP
            DW      _RETURN

;****************************************************************************************
; { GAME OVER - BLOCK 0115 }
; : CHECKBUTT 10 INP 20 AND 0= IF TRYZ THEN 10 INP 10 AND 0= IF TRY1 THEN ;
;****************************************************************************************
_CHECKBUTT: DB      _ENTER
            DW      _LITbyte
            DB      $10
            DW      _INP

;****************************************************************************************
            DW      _LITbyte
            DB      $04
            DW      _AND
            DW      _0
            DW      $016C               ; get two words from PSP,
                            ;   if P2-P1 > 0, then push zero to PSP,
                            ;   if p2-p1 <= 0, then push 1 to PSP

            DW      _0BRANCH            ; Get word from PSP,
                            ;   if word != 0, then IP+2 (keep going)
                            ;   if word = 0, then jump to ...
            DW      $BDA5               ;     ... here.

            DW      $BCB9               ; Enters here when diagnostics switch flipped first time
            DW      $16C7               ;

            DW      $B32C               ; ??? RST 8 - ENTER ???

            DW      $B341               ; ??? RST 8 - ENTER ???

            DB      $7E
            DB      $B3
            DB      $98
            DB      $00
            DB      $72
            DB      $14

LBDA5:      DW      _RETURN             ; RETURN - gets RSP value and goes to it

 ;****************************************************************************************

            DB      $CF
            DB      $6D
            DB      $00
            DB      $00
            DB      $05
            DB      $6D
            DB      $00
            DB      $00
            DB      $12
            DB      $6D
            DB      $00
            DB      $04

            ld      (_LIT),hl
            jr      z,$BDBC
            call    $A904
            ld      b,$98
            nop
            ld      b,h
            ld      (bc),a
            sbc     a,b
            nop
            adc     a,l
            ld      (bc),a
            ld      l,l
            nop
            nop
            ld      c,$17
            inc     bc
            ld      l,l
            nop
            nop
            ld      (bc),a
            dec     bc
            ld      bc,_LIT
            ld      e,(hl)
            ld      ($0076),hl
            jr      nz,$BD81
            ld      b,$5F
            ld      (bc),a
            ret     nc
            inc     b
            ld      h,c
            nop

;****************************************************************************************

            rst     $08
            ld      l,l
            nop
            nop
            dec     b
            ld      l,l
            nop
            nop
            xor     b
            ld      l,l
            nop
            ld      d,$22
            ld      l,l
            nop
            jr      z,$BDF4
            call    $A904
            ld      b,$98
            nop
            ld      b,h
            ld      (bc),a
            sbc     a,b
            nop
            ld      l,l
            nop
            nop
            or      e
            adc     a,l
            ld      (bc),a
            ld      l,l
            nop
            nop
            ld      c,$17
            inc     bc
            ld      de,$6D01
            nop
            ld      e,(hl)
            ld      ($0076),hl
            jr      nz,$BDB9
            ld      b,$5F
            ld      (bc),a
            ret     nc
            inc     b
            ld      h,c
            nop

;****************************************************************************************

            rst     $08
            ld      l,l
            nop
            nop
            ld      b,$76
            nop
            daa
            djnz    $BE2B
            ld      c,h
            or      (hl)
            or      c
            nop
            and     a
            cp      l
            ld      h,c
            nop

;****************************************************************************************

            rst     $08
            ld      l,l
            nop
            nop
            ld      b,$76
            nop
            jr      z,$BE42
            ld      a,(bc)
            ld      l,d
            or      (hl)
            or      c
            nop
            and     a
            cp      l
            rst     $18
            cp      l
            ld      h,c
            nop

;****************************************************************************************
            rst     $08
            ret     nc
            inc     b
            ld      l,l
            nop
            nop
            ld      h,b
            ld      (hl),l
            cp      l
            ld      h,c
            nop

;****************************************************************************************

            rst     $08
            dec     sp
            or      (hl)
            rla
            cp      (hl)
            ld      l,l
            nop
            nop
            ld      h,b
            ld      (hl),l
            cp      l
            ld      h,c
            nop

;****************************************************************************************

            rst     $08
            halt
            nop
            djnz    $BDF2
            nop
            ld      b,h
            ld      (bc),a
            rla
            cp      (hl)
            ld      l,l
            nop
            nop
            inc     bc
            ld      (hl),l
            cp      l
            rla
            cp      (hl)
            add     hl,hl
            cp      (hl)
            ld      l,l
            nop
            nop
            inc     bc
            ld      (hl),l
            cp      l
            add     hl,hl
            cp      (hl)
            ld      e,a
            ld      (bc),a
            ld      h,c
            nop
            inc     l
            dec     l
            ld      l,$2E
            cpl

;****************************************************************************************

            rst     $08
            ld      l,l
            nop
            inc     bc
            ret     nc
            ret     p
            nop
            and     h
            nop
            jp      pe,$C603
            cp      (hl)
            sbc     a,b
            nop
            adc     a,b
            nop
            ccf
            ret     nc
            xor     l
            jr      $BE34
            nop
            sbc     a,b
            nop
            ld      b,h
            ld      (bc),a
            dec     sp
            or      (hl)
            sbc     a,b
            nop
            adc     a,b
            nop
            ccf
            ret     nc
            sub     l
            jr      $BEFF
            ld      (bc),a
            ld      l,l
            nop
            nop
            ld      (de),a
            ld      l,l
            nop
            nop
            ld      l,(hl)
            ld      l,l
            nop
            ex      af,af'
            inc     c
            sbc     a,b
            nop
            adc     a,b
            nop
            ccf
            ret     nc
            dec     (hl)
            ld      a,(bc)
            ld      l,l
            nop
            nop
            ld      (de),a
            ld      l,l
            nop
            nop
            jr      z,$BF2B
            nop
            ex      af,af'
            inc     c
            halt
            nop
            ld      sp,$09C4
            ld      h,c
            nop

;****************************************************************************************

            rst     $08
            inc     l
            or      e
            ld      l,l
            nop
            add     hl,bc
            ret     nc
            ret     p
            nop
            jp      pe,$E703
            cp      (hl)
            ld      l,l
            nop
            add     hl,bc
            ret     nc
            ld      l,a
            ld      d,$6D
            nop
            xor     a
            ret     nc
            xor     (hl)
            djnz    $BE89
            djnz    $BF51
            inc     de
            add     a,h
            or      h
            ld      a,d
            cp      (hl)
            halt
            nop
            inc     b
            ld      h,$04
            and     h
            nop
            adc     a,b
            nop
            ld      (hl),l
            cp      (hl)
            ret     p
            nop
            cp      e
            add     hl,bc
            ld      e,b
            ld      bc,_0
            ld      b,h
            ld      (bc),a
            and     h
            nop
            xor     b
            ld      (bc),a
            ret     p
            nop
            ld      l,l
            nop
            nop
            inc     a
            adc     a,l
            ld      (bc),a
            ld      l,l
            nop
            nop
            ld      b,$17
            inc     bc
            ld      de,$CE01
            nop
            djnz    $BF20
            ld      e,a
            ld      (bc),a
            or      c
            nop
            ld      b,c
            or      e
            halt
            nop
            ld      (bc),a
            ld      h,$04
            ld      l,b
            inc     b
            dec     hl
            cp      a
            dec     a
            cp      (hl)
            ld      c,b
            cp      (hl)
            ld      d,l
            cp      (hl)
            ld      h,c
            nop

;#########################################################################
; { GAME OVER - BLOCK 0118 }
;    : GOS DEMOMODE WPBONE SOUNDSET INP 40 AND IF 0 ELSE 1
;    MUSICFLAG B! 0 20 OUTP 0 21 OUTP PLAYERUP WPBZERO
;    SKILLFACTOR WPBZERO GSAB 3 + @ DOIT GOSHOW
;    CHECKBUTT SHUTUP
;    COINS? B@ COINSIN B@ OR FREEPLAY OR IF DISPCOIN
;
; NOTE: The compiled ROM differs from the printed source. It zeroes the
; CRASHCTR, reads SETTINGS ($13) instead of SOUNDSET, uses different
; logic for the MUSICFLAG ($D0AF), and removes the OUTP commands.
;#########################################################################

;******************************************************************************************
; ----> GOS         Game Over Sequence. Sets the DEMOMODE flag, resets player
;                   variables, checks the hardware settings for music/sound,
;                   silences the audio, and begins checking for coins to
;                   either start a game or loop the Attract Mode.
;******************************************************************************************
_GOS:               DB      _ENTER              ; Enter TERSE execution
                    DW      _LIT            ;
                    DW      DEMOMODE            ; / Push address of DEMOMODE ($D001)
                    DW      _P1                 ; WPBONE (Write Protect 1 - Sets Game Over flag)
                    DW      _LIT            ;
                    DW      CRASHCTR            ; / Push address of CRASHCTR ($D03C) [ROM PATCH]
                    DW      _P0                 ; WPBZERO (Write Protect 0) [ROM PATCH]
                    DW      _LITbyte            ;
                    DB      $13                 ; / Push literal $13 (SETTINGS hardware port)
                    DW      _INP                ; INP (Read SETTINGS dip switches)
                    DW      _LITbyte            ;
                    DB      $80                 ; / Push literal $80 (Bit 7 Mask)
                    DW      _AND                ; AND (Test if bit 7 is set)
                    DW      _0BRANCH            ; IF (Is the bit set?)
                    DW      gos_push0           ; / Branch to gos_push0 if false (0)
                    DW      _1                  ; Push 1 (True)
                    DW      _BRANCH             ; ELSE
                    DW      gos_setmus          ; / Branch over the false block

gos_push0:          DW      _0                  ; Push 0 (False)

gos_setmus:         DW      _LIT            ;
                    DW      MUSICFLAG           ; / Push address of MUSICFLAG ($D0AF)
                    DW      _Bbang              ; B! (Store the 1 or 0 flag)
                    DW      _LIT            ;
                    DW      PLAYERUP            ; / Push address of PLAYERUP ($D038)
                    DW      _P0                 ; WPBZERO (Write Protect 0 - Reset to Player 1)
                    DW      _LIT            ;
                    DW      SKILLFACTOR         ; / Push address of SKILLFACTOR ($D037)
                    DW      _P0                 ; WPBZERO (Write Protect 0 - Reset Rank)
                    DW      W_B69D              ; [ROM PATCH] Unknown vector call (Likely GSAB DOIT)
                    DW      GOSHOW              ; GOSHOW (Title screen display routine at $BD4E)
                    DW      SHUTUP              ; SHUTUP (Silence audio hardware)
                    DW      _LIT            ;
                    DW      COINFRAC            ; / Push address of COINFRAC ($D002) [ROM PATCH]
                    DW      _Bat                ; B@ (Fetch COINFRAC byte)
                    DW      _LIT            ;
                    DW      COINS               ; / Push address of COINS ($D009)
                    DW      _Bat                ; B@ (Fetch COINS byte)

;#########################################################################
;   { GAME OVER - BLOCK 0120 } (Continued)
;   ... $D002 B@ COINS B@ OR COINSIN B@ OR
;   IF $BEC8 EI THEN $BC68 6 1 DO I MISSION WPB! SHUTUP
;   3 $B365 LOOP SHUTUP $B34A 0 $1472 ;
;#########################################################################

;******************************************************************************************
; ----> LBF74       Continuation of the Game Over Sequence (GOS) starting at _GOS ($BF2D).
;                   Checks the coin/start flags. If zero, it enters the game's
;                   Attract Mode loop. The DO..LOOP cycles from 1 to 5, setting the
;                   MISSION variable to each number to briefly show every game screen.
;******************************************************************************************
                    DW      _OR                 ; OR (Combine $D002 and COINS flags)
                    DW      _LIT            ;
                    DW      COINSIN             ; / Push address of COINSIN ($D003)
                    DW      _Bat                ; B@ (Fetch COINSIN byte)
                    DW      _OR                 ; OR (Combine with previous flags)
                    DW      _0BRANCH            ; IF (Are there any coins / active game?)
                    DW      gos_attract         ; / Branch to gos_attract if false (no coins)
                    DW      $BEC8               ; [Call unknown routine $BEC8]
                    DW      _EI                 ; EI (Enable Interrupts)

gos_attract:        DW      $BC68               ; [Call unknown routine $BC68]
                    DW      _LITbyte            ;
                    DB      $06                 ; / Push literal 6 (DO Loop Limit)
                    DW      _1                  ; Push 1 (DO Loop Index Start)
                    DW      _DO                 ; DO (Start 6 1 DO loop. Saves IP to IX stack)

gos_loop:           DW      _I                  ; I (Fetch current loop index 1-5)
                    DW      _LIT            ;
                    DW      MISSION             ; / Push address of MISSION ($D035)
                    DW      $1660               ; WPB! (Write Protect Byte Store - Sets MISSION to I)
                    DW      SHUTUP              ; SHUTUP (Silence audio for mission transition)
                    DW      _LITbyte            ;
                    DB      $03                 ; / Push literal 3
                    DW      $B365               ; [Call unknown routine $B365 - Attract Mode Display]
                    DW      _LOOP               ; LOOP (Increments I, loops back to gos_loop if I < 6)

                    DW      SHUTUP              ; SHUTUP (Silence audio again)
                    DW      $B34A               ; [Call unknown routine $B34A]
                    DW      _0                  ; Push 0
                    DW      $1472               ; [Call unknown routine $1472]
                    DW      _RETURN             ; ; (End of Game Over Sequence)

;******************************************************************************************
; Z80 Vector / Jump table snippet
;******************************************************************************************
LBFAA:              ld      bc, _GOS            ; Load BC with address of GOS routine ($BF2D)
                    jp      $B54F               ; Jump to unknown handler


;#########################################################################
;   { GAME OVER - BLOCK 0120 }
;   : RESTART WPCLEAR P1FBCTR B@ P2FBCTR B@ OR IF
;   DEMOMODE B@ 0= IF NPLAYERS B@
;   IF PLAY2 ELSE PLAY1 THEN THEN THEN GOS ;
;
;   NOTE: The compiled ROM contains inline patches not present in the
;   original printed source. It executes an initial routine (_CHECKBUTT)
;   and increments the CRASHCTR right before checking NPLAYERS.
;#########################################################################
_RESTART:
            DB      _ENTER
            DW      _CHECKBUTT          ; [ROM PATCH] (Calls the newly renamed button check)
            DW      _WPCLEAR            ; WPCLEAR (Clear write-protected RAM)
            DW      _LIT            ;
            DW      P1FBCTR             ; / Push address of P1FBCTR
            DW      _Bat                ; B@ (Fetch Player 1 fire base count)
            DW      _LIT            ;
            DW      P2FBCTR             ; / Push address of P2FBCTR
            DW      _Bat                ; B@ (Fetch Player 2 fire base count)
            DW      _OR                 ; OR (Combine P1 and P2 counts)
            DW      _0BRANCH            ; IF (Are there any active bases?)
            DW      go_gos              ; / Branch to go_gos if false (no bases)
            DW      _LIT            ;
            DW      DEMOMODE            ; / Push address of DEMOMODE flag
            DW      _Bat                ; B@ (Fetch DEMOMODE byte)
            DW      _zeroequal          ; 0= (Is DEMOMODE equal to 0?)
            DW      _0BRANCH            ; IF (If not in DEMOMODE...)
            DW      go_gos              ; / Branch to go_gos if false (in DEMOMODE)
            DW      _LIT            ;
            DW      CRASHCTR            ; / Push address of CRASHCTR [ROM PATCH]
            DW      _PINC               ; 1+WPB! (Increment crash counter) [ROM PATCH]
            DW      _LIT            ;
            DW      NPLAYERS            ; / Push address of NPLAYERS
            DW      _Bat                ; B@ (Fetch number of players)
            DW      _0BRANCH            ; IF (Is it a 2-player game?)
            DW      play_one            ; / Branch to play_one if false (1 player)
            DW      $B9B3               ; PLAY2 (Launch Player 2 routine at $B9B3)
            DW      _BRANCH             ; ELSE
            DW      go_gos              ; / Branch to go_gos (Fall through)

play_one:   DW      LB900               ; PLAY1 (Launch Player 1 routine at LB900)

go_gos:     DW      LB35C               ; [ROM PATCH] Unknown routine call
            DW      _GOS                ; GOS (Game Over Sequence at $BF2D)
            DW      _RETURN             ; ; (End of RESTART routine)

;****************************************************************************************
; ASCII identification "Gorf DNA". This area is accessed by the TEST DISPLAY ROUTINE.
; It also contains a hidden developer signature and build date right before the ROM ends.
;****************************************************************************************
            DB      "GORF", $00, "DNA", $00 ; 9 Bytes: "GORF DNA" Identifier

            ; Hidden ROM Build Date & Programmer Signature

            DB      $02, $24, $81           ; 3 Bytes: BCD Date 02-24-81 (February 24, 1981)
            DB      $4A, $46                ; 2 Bytes: ASCII "JF" (Jay Fenton initials)
            DB      $FF                     ; 1 Byte:  End of ROM padding / terminator


;****************************************************************************************
; Master EQUATES Section
;****************************************************************************************
;##########################################################################################
;    { BLOCK 0029 } (Snippet)
;    0C C= MAGIC  19 C= XPAND
;
;    { BLOCK 0030 }
;    ( COIN RAM-*WRITE PROTECT* , RAM ORDER CAN BE CRUCIAL ) HEX
;    RAMBASE C= WPRAMSTART RAMBASE 1+ VPTR !
;    0 BV= DEMOMODE ( GAME OVER MODE FLAG )
;    0 BV= COINFRAC 0 BV= COINSIN
;    0 BV= COINCOUNTERR ( holds # of ticks to hold counter1 on )
;    0 BV= COINBACKLOGR ( holds # of coins owed to right counter )
;    0 BV= COINCOUNTERL ( holds # of ticks to hold counter2 on )
;    0 BV= COINBACKLOGL 0 BV= COUNTERBITS
;    0 BV= COINS? 0 BV= SLAM? ( flags for COINS and SLAM events )
;    0 BV= OLDCREDITS ( old reading from coin port 10H )
;    3 BA= P1SCR 0 BV= P1HSP 0F BA= HISCR2 : HS2 0 HISCR2 ;
;    3 BA= P2SCR 0 BV= P2HSP 0F BA= HISCR4 : HS4 0 HISCR4 ;
;    0 BV= P1FBCTR 0 BV= P2FBCTR 0 BV= RIP 0 BV= MISSION
;    0 BV= MISSIONCTR 0 BV= SKILLFACTOR 0 BV= PLAYERUP
;    0 BV= NPLAYERS 0 BV= INITFB 0 BV= COCKTAIL 0 BV= CRASHCTR
;    0 V= COMBO2 3 BA= CRBASES 0 V= COMBO1 -->
;
;    { BLOCK 0031 } (Snippet)
;    ( UNPROTECTED UNCLEARED MEMORY AREA ) HEX
;    3 BA= rellink 3 BA= fflink 0 rellink C= relabs
;    0 fflink C= ffrelabs
;
;    { BLOCK 0032 }
;    ( EQUATES for COINS, DOORS, PORTS, BITS ) DECIMAL
;    10 C= COINTICKS ( how long to hold counters up and down )
;    HEX 5B C= RIGHTPORT A5 C= RIGHTVALUE ( WPRAM STUFF )
;    10 C= COINPORT
;    2 C= TESTBIT ( TEST ) 3 C= SLAMBIT ( doorslam )
;    0 C= COINBITR 1 C= COINBITL ( coin doors )
;    12 C= BUTTS ( buttons, 12 = coctail 2nd player )
;    13 C= SETTINGS ( SETTING SWITCH )
;    15 C= CCMISC ( COIN COUNTER/MISC )
;    DECIMAL
;    -->
;
;    { BLOCK 0034 } (Snippet)
;    2 A= RND#      ( you must seed RND# !!!!!!!!! )
;
;    { BLOCK 0037 }
;    ( VGS                  pattern board and magic equates )
;    HEX
;    ( pattern board ports )
;    78 C= PBLINADRL 79 C= PBLINADRH 7A C= PBSTAT 7B C= PBAREADRL
;    7B C= PBXMOD    7C C= PBAREADRH 7D C= PBXWIDE 7E C= PBYHIGH
;    ( pattern board status port bits )
;    0 C= PBDIR 1 C= PBEXP 2 C= PBCONS 3 C= PBFLUSH
;    4 C= PBFLIP 5 C= PBFLOP
;    ( magic register bits )
;    2 C= MRROT 3 C= MREXP 4 C= MROR 5 C= MRXOR
;    6 C= MRFLOP 7 C= MRFLIP
;    -->
;##########################################################################################

;******************************************************************************************
; Write Protected RAM (WPRAM) Equates (From Block 30)
;******************************************************************************************
WPRAMSTART      EQU     $D000                   ; Beginning of Static WPRAM
DEMOMODE        EQU     $D001                   ; GAME OVER MODE FLAG
COINFRAC        EQU     $D002                   ; Coin Fraction
COINSIN         EQU     $D003                   ; Coins In
COINCOUNTERR    EQU     $D004                   ; # of ticks to hold right counter 1 on
COINBACKLOGR    EQU     $D005                   ; # of coins owed to right counter
COINCOUNTERL    EQU     $D006                   ; # of ticks to hold left counter 2 on
COINBACKLOGL    EQU     $D007                   ; # of coins owed to left counter
COUNTERBITS     EQU     $D008                   ; Hardware counter bits
COINS           EQU     $D009                   ; Flags for COINS events
SLAM            EQU     $D00A                   ; Flags for SLAM events
OLDCREDITS      EQU     $D00B                   ; Old reading from coin port $10
P1SCR           EQU     $D00C                   ; P1 Score (3 bytes: $D00C-$D00E)
P1HSP           EQU     $D00F                   ; P1 High Score Pointer
HISCR2          EQU     $D010                   ; P1 High Scores (15 bytes: $D010-$D01E)
P2SCR           EQU     $D01F                   ; P2 Score (3 bytes: $D01F-$D021)
P2HSP           EQU     $D022                   ; P2 High Score Pointer
HISCR4          EQU     $D023                   ; P2 High Scores (15 bytes: $D023-$D031)
P1FBCTR         EQU     $D032                   ; P1 Fire Base Counter
P2FBCTR         EQU     $D033                   ; P2 Fire Base Counter
RIP             EQU     $D034                   ; Player Death Flag
MISSION         EQU     $D035                   ; Current Mission
MISSIONCTR      EQU     $D036                   ; Mission Counter
SKILLFACTOR     EQU     $D037                   ; Game Difficulty/Rank
PLAYERUP        EQU     $D038                   ; Which player's turn it is
NPLAYERS        EQU     $D039                   ; Number of players (1 or 2)
INITFB          EQU     $D03A                   ; Initial Fire Bases
COCKTAIL        EQU     $D03B                   ; Cocktail Mode Flag
CRASHCTR        EQU     $D03C                   ; Crash Counter
COMBO2          EQU     $D03D                   ; Combo 2 Vector (2 bytes)
CRBASES         EQU     $D03F                   ; CR Bases Array (3 bytes)
COMBO1          EQU     $D042                   ; Combo 1 Vector (2 bytes)

;******************************************************************************************
; Unprotected RAM & Misc Engine Variables (From Blocks 31 & 34)
;******************************************************************************************
RELABS          EQU     $D080                   ; relabs pointer
FFRELABS        EQU     $D083                   ; ffrelabs pointer
GOYFLAG         EQU     $D08B                   ; Attract/ready speech re-entry flag
RND_SEED        EQU     $D0AB                   ; 32-bit Random Seed RND#0
MUSICFLAG       EQU     $D0AF                   ; Global native music-processor enable

;******************************************************************************************
; Votrax Speech Queue RAM (GORFOS Blocks 0099-0102)
;
; TOPTALK through BOTTOMTALK is an eight-entry circular array of 16-bit TALK PRIM pointers.
; TOPTALK and BOTTOMTALK are queue-slot addresses; TALKIN and TALKOUT hold producer/consumer
; pointers into that array.
;******************************************************************************************
ONHOLD          EQU     $D111                   ; Delay ticks before PHONE resumes service
TOPTALK         EQU     $D112                   ; First TALK PRIM pointer slot
BOTTOMTALK      EQU     $D120                   ; Final TALK PRIM pointer slot
TALKHERE        EQU     $D122                   ; Next phoneme byte in active primitive
PHONECOUNT      EQU     $D124                   ; Source PHONE#: phonemes remaining
TALKIN          EQU     $D125                   ; Producer pointer: next queue slot to fill
TALKOUT         EQU     $D127                   ; Consumer pointer: next queue slot to play
MSYRND          EQU     $D129                   ; Mission-start anti-repeat selector
HTYRND          EQU     $D12A                   ; Hit-taunt anti-repeat selector

FOREIGN_SPEECH_ENTRY EQU $C000                  ; Foreign-language speech entry selected by SETTINGS bit 3

;******************************************************************************************
; Upper-ROM Program-2 Speech Keys
;
; These three count-prefixed SC-01 records form the Flag Ship completion announcement.
; The recovered TERSE material does not provide source symbols for the records; these names
; are the disassembly's semantic speech-key labels. They are also required translation keys
; for the Program-2 X11 language interface.
;******************************************************************************************
SPK_FLAGSHIP_INTRO       EQU     $A985           ; "Next time will be harder, but for now"
SPK_GORFIAN_CHRONICLES   EQU     $A9A8           ; "In the Gorfian chronicles"
SPK_FLAGSHIP_HIT         EQU     $A9C1           ; "For hitting my flagship"

;******************************************************************************************
; TERSE System Stack Pointers
;******************************************************************************************
ISP             EQU     $BFB1                   ; TERSE Instruction Stack Pointer (ISP)
PSP             EQU     $D2C0                   ; TERSE Parameter Stack Pointer (PSP)
RSP             EQU     $D340                   ; TERSE Return Stack Pointer (RSP)

;******************************************************************************************
; Hardware, Ports, and Settings Equates (From Blocks 29 & 32)
;******************************************************************************************
COINTICKS       EQU     10                      ; How long to hold counters up and down
RIGHTPORT       EQU     $5B                     ; Write Protect RAM Port
RIGHTVALUE      EQU     $A5                     ; Write Protect RAM Unlock Value
COINPORT        EQU     $10                     ; Coin switch input port
TESTBIT         EQU     2                       ; Test switch bit
SLAMBIT         EQU     3                       ; Door slam bit
COINBITR        EQU     0                       ; Right coin door bit
COINBITL        EQU     1                       ; Left coin door bit
BUTTS           EQU     $12                     ; Buttons (12 = cocktail 2nd player)
SETTINGS        EQU     $13                     ; Setting Switch port
CCMISC          EQU     $15                     ; Coin Counter/Misc port
MAGIC           EQU     $0C                     ; Magic RAM control port
XPAND           EQU     $19                     ; Expand register port

;******************************************************************************************
; Pattern Board & Magic Register Equates (From Block 37)
;******************************************************************************************
PBSTAT          EQU     $7A                     ; Pattern Board Status Port
PBAREADRL       EQU     $7B                     ; Pattern Board Area Read Low
PBAREADRH       EQU     $7C                     ; Pattern Board Area Read High
PBXWIDE         EQU     $7D                     ; Pattern Board X Width
PBLINADRL       EQU     $78                     ; Pattern Board Line Address Low
PBLINADRH       EQU     $79                     ; Pattern Board Line Address High
PBXMOD          EQU     $7B                     ; Pattern Board X Mod
PBYHIGH         EQU     $7E                     ; Pattern Board Y Height

PBDIR           EQU     0                       ; Pattern Board Status Bit 0
PBEXP           EQU     1                       ; Pattern Board Status Bit 1
PBCONS          EQU     2                       ; Pattern Board Status Bit 2
PBFLUSH         EQU     3                       ; Pattern Board Status Bit 3
PBFLIP          EQU     4                       ; Pattern Board Status Bit 4
PBFLOP          EQU     5                       ; Pattern Board Status Bit 5

MRROT           EQU     2                       ; Magic Register Bit 2
MREXP           EQU     3                       ; Magic Register Bit 3
MROR            EQU     4                       ; Magic Register Bit 4
MRXOR           EQU     5                       ; Magic Register Bit 5
MRFLOP          EQU     6                       ; Magic Register Bit 6
MRFLIP          EQU     7                       ; Magic Register Bit 7


                END                             ;END of Gorf Version 2

;****************************************************************************************
; END OF GORF CODE. ANYTHING BELOW IS NOT USED OR COMPILED. IT IS JUST FOR REFERENCE.
;****************************************************************************************
