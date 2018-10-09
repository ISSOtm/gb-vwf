
SECTION "VWF engine", ROM0

; Sets the pen's position somewhere in memory
; The code is designed for a pen in VRAM, but it can be anywhere
; Please call this after PrintVWFText, so wTextCurTile is properly updated
; @param hl The address to print to (usually in the tilemap)
SetPenPosition::
; Note: relied upon preserving HL
    ld a, l
    ld [wPenPosition], a
    ld [wPenStartingPosition], a
    ld a, h
    ld [wPenPosition + 1], a
    ld [wPenStartingPosition + 1], a

    ld a, [wTextCurTile]
    ld [wPenCurTile], a
    ret

DrawVWFChars::
    xor a
    ldh [rVBK], a

    ld hl, wPenPosition
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld d, a
    ; ld hl, wPenCurTile
    ld c, [hl]

    ld hl, wNewlineTiles
    ld a, [wFlushedTiles]
    and a
    jr z, .noNewTiles
    ld b, a
    xor a
    ld [wFlushedTiles], a
.writeNewTile
    ; Check if current tile is subject to a newline
    ld a, c
    cp [hl]
    jr z, .newline
    wait_vram
    ld a, c
    ld [de], a
    sub $7F
    jr nz, .nowrap
    ld c, a ; ld c, 0
.nowrap
    inc de
    inc c
    dec b
    jr nz, .writeNewTile
.noNewTiles

.tryAgain
    ld a, c
    cp [hl]
    jr z, .finalNewline
    xor a
    ld [wNbNewlines], a

    ld hl, wPenCurTile
    ld a, c
    ld [hld], a
    ld a, d
    ld [hld], a
    ld [hl], e

    ; If the current tile is empty (1 px == 1 space)
    ld a, [wTextCurPixel]
    cp 2
    ret c
    wait_vram
    ld a, c
    ld [de], a
    ret

.newline
    ld a, [wPenStartingPosition]
    and SCRN_VX_B - 1
    ld c, a ; Get offset from column 0
    ld a, e
    and -SCRN_VX_B ; Get to column 0
    add a, c ; Get to starting column
    add a, SCRN_VX_B ; Go to next row (this might overflow)
    ld e, a
    jr nc, .nocarry
    inc d
.nocarry
    ld c, [hl] ; Get back tile ID
    xor a ; Clear this newline tile
    ld [hli], a ; Go to the next newline (if any)
    jr .writeNewTile

.finalNewline
    xor a
    ld [hli], a ; Clear that
    ld a, [wPenStartingPosition]
    and SCRN_VX_B - 1
    ld b, a
    ld a, e
    and -SCRN_VX_B
    add a, b
    add a, SCRN_VX_B
    ld e, a
    jr nc, .tryAgain ; noCarry
    inc d
    jr .tryAgain



; Sets up the VWF engine to start printing text
; @param hl Pointer to the string to be displayed
; @param b  Bank containing the string
; @param a  Non-zero to flush the current string (use zero if you want to keep printing the same string)
PrintVWFText::
    and a ; Set Z flag for test below

    ; Write src ptr
    ld a, b
    ld [wTextSrcBank], a
    ld a, l
    ld [wTextSrcPtr], a
    ld a, h
    ld [wTextSrcPtr + 1], a

    ; Flag preserved from `and a`
    jr z, .dontFlush
    xor a
    ldh [rVBK], a
    ; Don't flush if current tile is empty
    ld a, [wTextCurPixel]
    cp 2
    ; Flush buffer to VRAM
    call nc, FlushVWFBuffer
    ; Reset position always, though
    xor a
    ld [wTextCurPixel], a
.dontFlush

    ; Use black color by default (which is normally loaded into color #3)
    ld a, 3
    ld [wTextColorID], a

    ; Preserve the language but reset the decoration
    ld hl, wTextCharset
    ld a, [hl]
    and $F0
    ld [hl], a

    ; Set initial letter delay to 0, to start printing directly
    xor a
    ld [wTextNextLetterDelay], a
    ret

; Prints a VWF char (or more), applying delay if necessary
; Might print more than 1 char, eg. if wTextLetterDelay is zero
; Sets the high byte of the source pointer to $FF when finished
; **DO NOT CALL WITH SOURCE DATA IN FF00-FFFF, THIS WILL CAUSE AN EARLY RETURN!!
; Number of tiles to write to the tilemap is written in wFlushedTiles
PrintVWFChar::
    ld hl, wTextNextLetterDelay
    ld a, [hl]
    and a
    jr z, .delayFinished
    dec a
    ld [hl], a
    ret

.delayFinished
    ; xor a
    ld [wFlushedTiles], a
    ldh [rVBK], a

    ; Save current ROM bank
    ldh a, [hCurROMBank]
    push af

    ; Get read to read char
    ld hl, wTextSrcBank
    ld a, [hli]
    rst bankswitch
    ld a, [hli]
    ld h, [hl]
    ld l, a

.setDelayAndNextChar
    ; Reload delay
    ld a, [wTextLetterDelay]
    ld [wTextNextLetterDelay], a

.nextChar
    ; Read byte from string stream
    ld a, [hli]
    and a ; Check for terminator
    jp z, .checkForReturn
    cp " "
    jp c, .controlChar

; Print char

    ; Save src ptr & letter ID
    push hl

    sub " "
    add a, a
    ld c, a
    ld b, 0

    ; Get ptr to charset table
    ld a, [wTextCharset]
    add a, a
    add a, LOW(CharsetPtrs)
    ld l, a
    adc a, HIGH(CharsetPtrs)
    sub l
    ld h, a
    ld a, [hli]
    ld h, [hl]
    ld l, a

    ; Get ptr to letter
    add hl, bc
    ld a, [hli]
    ld d, [hl]
    ld e, a

    ; Get dest buffer ptr
    ld hl, wTextTileBuffer

    ld c, 8
.printOneLine
    ld a, [wTextCurPixel]
    ld b, a
    and a

    ld a, [de]
    inc de
    push de

    ld e, 0
    jr z, .doneShifting
.shiftRight
    rra ; Actually `srl a`, since a 0 bit is always shifted in
    rr e
    dec b
    jr nz, .shiftRight
.doneShifting
    ld d, a

    ld a, [wTextColorID]
    rra
    jr nc, .noLSB
    ld a, d
    or [hl]
    ld [hl], a

    ld a, l
    add a, $10
    ld l, a
    ld a, e
    or [hl]
    ld [hl], a
    ld a, l
    sub $10
    ld l, a
.noLSB
    inc hl

    ld a, [wTextColorID]
    and 2
    jr z, .noMSB
    ld a, d
    or [hl]
    ld [hl], a

    ld a, l
    add a, $10
    ld l, a
    ld a, e
    or [hl]
    ld [hl], a
    ld a, l
    sub $10
    ld l, a
.noMSB
    inc hl

    pop de
    dec c
    jr nz, .printOneLine

    ; Advance read by size
    ld hl, wTextCurPixel
    ld a, [de]
    add a, [hl]
    ld [hl], a

    ; Restore src ptr
    pop hl

.charPrinted
    ; Save src ptr
    ld a, l
    ld [wTextSrcPtr], a
    ld a, h
    ld [wTextSrcPtr + 1], a

    ; Check if flushing needs to be done
    ld a, [wTextCurPixel]
    sub 8
    jr c, .noTilesToFlush

    ; Move back by 8 pixels
    ld [wTextCurPixel], a
    ; Flush them to VRAM
    call FlushVWFBuffer
    ; Check if the second tile needs to be flushed as well (happens with characters 9 pixels wide)
    ; We might never use 9-px chars, but if we do, there'll be support for them ^^
    ld a, [wTextCurPixel]
    sub 8
    jr c, .flushed
    ld [wTextCurPixel], a
    call FlushVWFBuffer
.flushed

.noTilesToFlush
    ; If not printing next char immediately, force to flush
    ld a, [wTextNextLetterDelay]
    and a
    jp z, .setDelayAndNextChar
    dec a
    ld [wTextNextLetterDelay], a

.flushAndFinish
    ; Check if flushing is necessary
    ld a, [wTextCurPixel]
    cp 2
    jr c, .flushingNotNeeded

    ld a, [wTextCurTile]
    swap a
    ld h, a
    and $F0
    ld l, a
    ld a, h
    and $0F
    add a, HIGH(vVWFTiles)
    ld h, a
    ld de, wTextTileBuffer
    ld c, $10
    call LCDMemcpySmall

.flushingNotNeeded
    ; Restore ROM bank
    pop af
    rst bankswitch
    ret


.checkForReturn
    ; Tell caller we're done (if we're not, this'll be overwritten)
    ld a, $FF
    ld [wTextSrcPtr + 1], a

    ; Check if stack is empty
    ld hl, wTextStackSize ; Ok to trash hl, it doesn't matter anyways
    ld a, [hl]
    add a, a ; Assuming this can't be > $7F (shouldn't be > 8 anyways)
    jr z, .flushAndFinish

    add a, [hl] ; *3
    dec [hl] ; Decrement entry count
    add a, LOW(wTextStack)
    ld l, a
    adc a, HIGH(wTextStack)
    sub l
    ld h, a
    ; hl points to first byte of free entry, so decrement
    dec hl

    ; Restore ROM bank
    ld a, [hld]
    rst bankswitch
    
    ; Read new src ptr
    ld a, [hld]
    ld l, [hl]
    ld h, a
    jp .nextChar


.controlChar
    ; Check if ctrl char is valid
    cp TEXT_BAD_CTRL_CHAR
    call nc, TextCtrlCharError

    ; Control char, run the associated function
    ld de, .charPrinted
    push de

    ; Push the func's addr (so we can preserve hl when calling)
    add a, a
    add a, LOW(.controlCharFuncs - 2)
    ld e, a
    adc a, HIGH(.controlCharFuncs - 2)
    sub e
    ld d, a
    ld a, [de]
    ld c, a
    inc de
    ld a, [de]
    ld b, a
    push bc
    ret ; Actually jump to the function, passing `hl` as a parameter for it to read (and advance)


.controlCharFuncs
    dw TextSetLanguage
    dw TextRestoreLanguage
    dw TextSetDecoration
    dw TextRestoreDecoration
    dw TextSetColor
    dw TextPrintBlank
    dw TextJumpTo
    dw TextCall
    dw TextDelay
    dw TextNewline


TextSetLanguage:
    ld de, wTextCharset
    ld a, [de]
    ld b, a
    ld [wPreviousLanguage], a
    ld a, b
    and $0F
    ld b, a
    ld a, [hli]
    swap a
    and $F0
    or b
    jr _TextSetCharset

TextRestoreLanguage:
    ld de, wTextCharset
    ld a, [de]
    and $0F
    ld b, a
    ld a, [wPreviousLanguage]
    and $F0
    or b
    jr _TextSetCharset

TextSetDecoration:
    ld de, wTextCharset
    ld a, [de]
    ld b, a
    ld [wPreviousDecoration], a
    ld a, b
    and $F0
    ld b, a
    ld a, [hli]
    and $0F
    or b
    jr _TextSetCharset

TextRestoreDecoration:
    ld de, wTextCharset
    ld a, [de]
    and $F0
    ld b, a
    ld a, [wPreviousDecoration]
    and $0F
    or b

_TextSetCharset:
    ld [de], a
    jr PrintNextCharInstant


TextSetColor:
    ld a, [hli]
    and 3
    ld [wTextColorID], a
    jr PrintNextCharInstant


TextPrintBlank:
    ld a, [hli]
    ld c, a
    ld a, [wTextCurPixel]
    add a, c
    ld c, a
    and $F8
    jr z, .noNewTiles
    rrca
    rrca
    rrca
    ld b, a
.printNewTile
    push bc
    call FlushVWFBuffer ; Preserves HL
    pop bc
    dec b
    jr nz, .printNewTile
.noNewTiles
    ld a, c
    and 7
    ld [wTextCurPixel], a
    jr PrintNextCharInstant


TextDelay:
    ld a, [hli]
    ld [wTextNextLetterDelay], a
    ret


; Sets text ptr to given location (must be within same bank!)
TextJumpTo:
    ld a, [hli]
    rst bankswitch
    ld a, [hli]
    ld h, [hl]
    ld l, a
    jr PrintNextCharInstant

; Start printing a new string, then keep writing this one
; NOTE: avoids corruption by preventing too much recursion, but this shouldn't happen at all
TextCall:
    ld a, [wTextStackSize]
    cp TEXT_STACK_CAPACITY
    call nc, TextStackOverflowError

    ; Read target ptr
    ld b, a ; Save current size for later (to get ptr to 1st free entry)
    inc a ; Increase stack size
    ld [wTextStackSize], a

    ; Get target ptr
    ld a, [hli]
    rst bankswitch
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld d, a

    ; Get ptr to stack top (1st empty entry)
    ld a, b
    add a, a
    add a, b
    add a, LOW(wTextStack)
    ld c, a
    adc a, HIGH(wTextStack)
    sub c
    ld b, a
    ld a, l
    ld [bc], a
    inc bc
    ld a, h
    ld [bc], a
    inc bc
    ldh a, [hCurROMBank]
    ld [bc], a

    ; Get new src ptr
    ld h, d
    ld l, e
    jr PrintNextCharInstant


TextNewline:
    ; Flush the current tile if non-blank
    ld a, [wTextCurPixel]
    cp 2
    call nc, FlushVWFBuffer
    ; Reset position
    xor a
    ld [wTextCurPixel], a

    ld de, wNbNewlines
    ld a, [de]
    inc a
    ld [de], a
    dec a
    add a, LOW(wNewlineTiles)
    ld e, a
    adc a, HIGH(wNewlineTiles)
    sub e
    ld d, a
    ld a, [wTextCurTile]
    ld [de], a
    ; Fall through

PrintNextCharInstant:
    xor a
    ld [wTextNextLetterDelay], a
    ret


FlushVWFBuffer::
    push hl

    ; Calculate ptr to next tile
    ld a, [wTextCurTile]
    swap a
    ld d, a
    and $F0
    ld e, a
    ld a, d
    and $0F
    add a, HIGH(vVWFTiles)
    ld d, a

    ; Copy buffer 1 to VRAM, buffer 2 to buffer 1, and clear buffer 2
    ld hl, wTextTileBuffer
    ld bc, wTextTileBuffer + $10
.copyByte
    ldh a, [rSTAT]
    and STATF_BUSY
    jr nz, .copyByte
    ; Write tile buf to VRAM
    ld a, [hl]
    ld [de], a
    inc e ; Faster than inc de, guaranteed thanks to ALIGN[4]
    ; Copy second tile to first one
    ld a, [bc]
    ld [hli], a
    ; Clear second tile
    xor a
    ld [bc], a
    inc c

    ld a, l
    cp LOW(wTextTileBuffer + $10)
    jr nz, .copyByte

    ; Go to next tile
    ld hl, wTextCurTile
    ld a, [hl]
    inc a
    and $7F
    jr nz, .noWrap
    inc a ; Don't touch tile 0, it must stay blank
.noWrap
    ld [hl], a

    ld hl, wFlushedTiles
    inc [hl]
    pop hl
    ret



CharsetPtrs::
    dw LatinCharsetBasic
    ; TODO: add more charsets (bold, JP, etc.)


LatinCharsetBasic::
    dw .space

    dw .exclMark
    dw .quote
    dw .hash
    dw .space ; $
    dw .space ; %
    dw .amp
    dw .apos
    dw .openBrack
    dw .closeBrack
    dw .star
    dw .plus
    dw .comma
    dw .dash
    dw .dot
    dw .slash

    dw .zero
    dw .one
    dw .two
    dw .three
    dw .four
    dw .five
    dw .six
    dw .seven
    dw .eight
    dw .nine

    dw .colon
    dw .halfColon
    dw .lt
    dw .equal
    dw .gt
    dw .questMark
    dw .space ; @

    dw .A
    dw .B
    dw .C
    dw .D
    dw .E
    dw .F
    dw .G
    dw .H
    dw .I
    dw .J
    dw .K
    dw .L
    dw .M
    dw .N
    dw .O
    dw .P
    dw .Q
    dw .R
    dw .S
    dw .T
    dw .U
    dw .V
    dw .W
    dw .X
    dw .Y
    dw .Z

    dw .openSqBrack
    dw .backslash
    dw .closeSqBrack
    dw .caret
    dw .underscore
    dw .backtick

    dw .a
    dw .b
    dw .c
    dw .d
    dw .e
    dw .f
    dw .g
    dw .h
    dw .i
    dw .j
    dw .k
    dw .l
    dw .m
    dw .n
    dw .o
    dw .p
    dw .q
    dw .r
    dw .s
    dw .t
    dw .u
    dw .v
    dw .w
    dw .x
    dw .y
    dw .z

    dw .openCurlBrack
    dw .pipe
    dw .closeCurlBrack

    dw .space ; DEL

REPT " "
    dw .space
ENDR


.space
    db $00, $00, $00, $00, $00, $00, $00, $00,  2
.exclMark
    db $40, $40, $40, $40, $40, $00, $40, $00,  5
.quote
    db $D8, $90, $00, $00, $00, $00, $00, $00,  6
.hash
    db $50, $F8, $50, $50, $50, $F8, $50, $00,  6
.amp
    db $40, $A0, $60, $40, $A0, $A0, $50, $00,  4
.apos
    db $C0, $40, $80, $00, $00, $00, $00, $00,  3
.openBrack
    db $40, $80, $80, $80, $80, $80, $40, $00,  3
.closeBrack
    db $80, $40, $40, $40, $40, $40, $80, $00,  3
.star
    db $00, $20, $A8, $70, $A8, $20, $00, $00,  6
.plus
    db $00, $20, $20, $F8, $20, $20, $00, $00,  6
.comma
    db $00, $00, $00, $00, $00, $00, $40, $80,  3
.dash
    db $00, $00, $00, $F0, $00, $00, $00, $00,  5
.dot
    db $00, $00, $00, $00, $00, $00, $80, $00,  2
.slash
    db $00, $10, $20, $20, $40, $40, $80, $00,  5

.zero
    db $60, $90, $B0, $D0, $90, $90, $60, $00,  5
.one
    db $C0, $40, $40, $40, $40, $40, $40, $00,  3
.two
    db $E0, $10, $10, $20, $40, $80, $F0, $00,  5
.three
    db $E0, $10, $10, $60, $10, $10, $E0, $00,  5
.four
    db $30, $50, $90, $F0, $10, $10, $10, $00,  5
.five
    db $F0, $80, $80, $E0, $10, $10, $E0, $00,  5
.six
    db $60, $80, $80, $E0, $90, $90, $60, $00,  5
.seven
    db $F0, $10, $10, $20, $20, $40, $40, $00,  5
.eight
    db $60, $90, $90, $60, $90, $90, $60, $00,  5
.nine
    db $60, $90, $90, $70, $10, $10, $60, $00,  5

.colon
    db $00, $80, $80, $00, $80, $80, $00, $00,  2
.halfColon
    db $00, $40, $40, $00, $40, $40, $80, $00,  3
.lt
    db $00, $10, $60, $80, $60, $10, $00, $00,  5
.equal
    db $00, $00, $F0, $00, $F0, $00, $00, $00,  5
.gt
    db $00, $80, $60, $10, $60, $80, $00, $00,  5
.questMark
    db $60, $90, $10, $20, $40, $00, $40, $00,  5

.A	db $60, $90, $90, $F0, $90, $90, $90, $00,  5
.B	db $E0, $90, $90, $E0, $90, $90, $E0, $00,  5
.C	db $70, $80, $80, $80, $80, $80, $70, $00,  5
.D	db $E0, $90, $90, $90, $90, $90, $E0, $00,  5
.E	db $F0, $80, $80, $E0, $80, $80, $E0, $00,  5
.F	db $F0, $80, $80, $F0, $80, $80, $80, $00,  5
.G	db $70, $80, $80, $B0, $90, $90, $60, $00,  5
.H	db $90, $90, $90, $F0, $90, $90, $90, $00,  5
.I	db $E0, $40, $40, $40, $40, $40, $E0, $00,  4
.J	db $10, $10, $10, $10, $10, $10, $E0, $00,  5
.K	db $90, $90, $90, $A0, $E0, $90, $90, $00,  5
.L	db $80, $80, $80, $80, $80, $80, $F0, $00,  5
.M	db $88, $D8, $A8, $88, $88, $88, $88, $00,  6
.N	db $90, $D0, $B0, $90, $90, $90, $90, $00,  5
.O	db $60, $90, $90, $90, $90, $90, $60, $00,  5
.P	db $E0, $90, $90, $E0, $80, $80, $80, $00,  5
.Q	db $60, $90, $90, $90, $90, $A0, $70, $00,  5
.R	db $E0, $90, $90, $E0, $A0, $90, $90, $00,  5
.S	db $70, $80, $80, $60, $10, $10, $E0, $00,  5
.T	db $F8, $20, $20, $20, $20, $20, $20, $00,  6
.U	db $90, $90, $90, $90, $90, $90, $60, $00,  5
.V	db $90, $90, $90, $90, $90, $A0, $40, $00,  5
.W	db $88, $88, $88, $88, $A8, $D8, $88, $00,  6
.X	db $90, $90, $90, $60, $90, $90, $90, $00,  5
.Y	db $90, $90, $90, $A0, $40, $40, $40, $00,  5
.Z	db $F0, $10, $20, $40, $80, $80, $F0, $00,  5

.openSqBrack
    db $C0, $80, $80, $80, $80, $80, $C0, $00,  3
.backslash
    db $00, $80, $40, $40, $20, $20, $10, $00,  5
.closeSqBrack
    db $C0, $40, $40, $40, $40, $40, $C0, $00,  3
.caret
    db $40, $00, $00, $00, $00, $00, $00, $00,  4
.underscore
    db $00, $00, $00, $00, $00, $00, $00, $F0,  5
.backtick
    db $80, $40, $00, $00, $00, $00, $00, $00,  3

.a	db $00, $00, $60, $10, $70, $90, $60, $00,  5
.b	db $00, $80, $80, $E0, $90, $90, $60, $00,  5
.c	db $00, $00, $70, $80, $80, $80, $70, $00,  5
.d	db $00, $10, $10, $70, $90, $90, $60, $00,  5
.e	db $00, $00, $60, $90, $E0, $80, $60, $00,  5
.f	db $00, $70, $80, $F0, $80, $80, $80, $00,  5
.g	db $00, $00, $60, $90, $90, $70, $10, $E0,  5
.h	db $00, $80, $80, $E0, $90, $90, $90, $00,  5
.i	db $00, $80, $00, $80, $80, $80, $80, $00,  2
.j	db $00, $20, $00, $20, $20, $20, $C0, $00,  4
.k	db $00, $80, $90, $A0, $C0, $A0, $90, $00,  5
.l	db $00, $80, $80, $80, $80, $80, $80, $00,  2
.m	db $00, $00, $D0, $A8, $A8, $A8, $A8, $00,  6
.n	db $00, $00, $E0, $90, $90, $90, $90, $00,  5
.o	db $00, $00, $60, $90, $90, $90, $60, $00,  5
.p	db $00, $00, $60, $90, $90, $E0, $80, $80,  5
.q	db $00, $00, $60, $90, $90, $70, $10, $10,  5
.r	db $00, $00, $B0, $C0, $80, $80, $80, $00,  5
.s	db $00, $00, $70, $80, $60, $10, $E0, $00,  5
.t	db $00, $40, $F0, $40, $40, $40, $30, $00,  5
.u	db $00, $00, $90, $90, $90, $90, $60, $00,  5
.v	db $00, $00, $90, $90, $90, $A0, $40, $00,  5
.w	db $00, $00, $88, $88, $88, $A8, $D0, $00,  6
.x	db $00, $00, $B0, $40, $40, $40, $B0, $00,  5
.y	db $00, $00, $90, $90, $90, $70, $10, $E0,  5
.z	db $00, $00, $F0, $10, $60, $80, $F0, $00,  5

.openCurlBrack
    db $
.pipe
    db $80, $80, $80, $80, $80, $80, $80, $00, 2
.closeCurlBrack
    db $
