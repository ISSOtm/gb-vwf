
; SPDX-License-Identifier: MIT
;
; MIT License
;
; Copyright (c) 2018-2021 Eldred Habert
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.


; Number of elements the text stack has room for
; Having more will cause a soft crash
; This must not exceeed $7F, as the return logic discards bit 7 when checking for zero
IF !DEF(TEXT_STACK_CAPACITY)
TEXT_STACK_CAPACITY equ 8
ENDC

; IMPORTANT NOTE REGARDING NEWLINES!!!
; DO NOT PRINT MORE THAN THIS NEWLINES AT ONCE
; THIS **WILL** CAUSE A BUFFER OVERFLOW
IF !DEF(TEXT_NEWLINE_CAPACITY)
TEXT_NEWLINE_CAPACITY equ 10
ENDC


; `wTextFlags` bits
rsset 6
text_flag: MACRO
TEXTB_\1 rb 1
TEXTF_\1 equ 1 << TEXTB_\1
	EXPORT TEXTB_\1, TEXTF_\1
ENDM
	text_flag WAITBUTTON
	text_flag SYNC


CHARACTER_HEIGHT equ 8
CHARACTER_SIZE equ CHARACTER_HEIGHT + 1



SECTION "VWF engine", ROM0


CTRL_CHAR_PTRS equs ""
	rsreset
control_char: MACRO
	IF DEF(PRINT_CHARMAP)
		PRINTT "charmap \"<\1>\", {d:_RS}\n"
	ENDC
TEXT_\1 rb 1
	IF DEF(EXPORT_CONTROL_CHARS)
		EXPORT TEXT_\1
	ENDC

	IF _NARG > 1
		dw \2
TMP equs "{CTRL_CHAR_PTRS}\ndw \3"
		PURGE CTRL_CHAR_PTRS
CTRL_CHAR_PTRS equs "{TMP}"
		PURGE TMP
	ENDC
ENDM

	;;;;;;;;;;;;;;;;;;;;; "Regular" control chars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	control_char END
RefillerControlChars:
	control_char SET_FONT,        ReaderSetFont,                 TextSetFont
	control_char RESTORE_FONT,    ReaderRestoreFont,             TextRestoreFont
	control_char SET_VARIANT,     ReaderSetVariant,              TextSetVariant
	control_char RESTORE_VARIAN,  ReaderRestoreVariant,          TextRestoreVariant
	control_char SET_COLOR,       Reader2ByteNop,                TextSetColor
	control_char BLANKS,          ReaderPrintBlank,              TextPrintBlank
	control_char DELAY,           Reader2ByteNop,                TextDelay
	control_char WAITBTN,         ReaderWaitButton,              TextWaitButton
	control_char CLEAR,           ReaderClear,                   TextClear
	control_char NEWLINE,         ReaderNewline,                 TextNewline
	control_char SYNC,            Reader1ByteNop,                TextSync
	control_char SCROLL,          ReaderScroll,                  TextScroll
	control_char WAITBTN_SCROLL,  ReaderWaitButtonScroll,        TextWaitButtonScroll
	control_char ZWS,             _RefillCharBuffer.canNewline,  PrintNextCharInstant
TEXT_BAD_CTRL_CHAR rb 0

	assert TEXT_NEWLINE == "\n"

PTRS equs ""
	rsset 256
reader_only_control_char: MACRO
_RS = _RS - 1
	IF DEF(PRINT_CHARMAP)
		PRINTT "charmap \"<\1>\", {d:_RS}\n"
	ENDC
TEXT_\1 equ _RS
	IF DEF(EXPORT_CONTROL_CHARS)
		EXPORT TEXT_\1
	ENDC

TMP equs "dw \2\n{PTRS}"
	PURGE PTRS
PTRS equs "{TMP}"
	PURGE TMP
ENDM

	;;;;;;;;;;;;;;;;;;;; Reader-only control chars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	reader_only_control_char CALL,         ReaderCall
	reader_only_control_char JUMP,         ReaderJumpTo
	reader_only_control_char SOFT_HYPHEN,  ReaderSoftHyphen

	; The base of the table is located at its end
	; Unusual, I know, but it works better!
	PTRS
RefillerOnlyControlChars:
FIRST_READER_ONLY_CONTROL_CHAR rb 0

NB_FONT_CHARACTERS equ FIRST_READER_ONLY_CONTROL_CHAR - " "



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
	ld hl, wPenPosition
	ld a, [hli]
	ld e, a
	ld a, [hli]
	ld d, a
	assert wPenPosition + 2 == wPenCurTile
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
	ld a, [wNbNewlines]
	and a
	jr z, .writeTile
	ld a, c
	cp [hl]
	jr z, .newline
.writeTile
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .writeTile
	ld a, c
	ld [de], a
	ld a, [wLastTextTile]
	scf
	sbc c
	jr nc, .nowrap
	ld a, [wWrapTileID]
	ld c, a
	db $FE ; cp imm8
.nowrap
	inc c
	inc de
	dec b
	jr nz, .writeNewTile
.noNewTiles

.tryAgain
	ld a, [wNbNewlines]
	and a
	jr z, .noFinalNewline
	ld a, c
	cp [hl]
	jr z, .finalNewline
.noFinalNewline
	xor a
	ld [wNbNewlines], a

	ld hl, wPenCurTile
	ld a, c
	ld [hld], a
	assert wPenCurTile - 1 == wPenPosition + 1
	ld a, d
	ld [hld], a
	ld [hl], e

	; If the current tile is empty (1 px == 1 space)
	ld a, [wTextCurPixel]
	cp 2
	ret c
.waitFinalTile
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .waitFinalTile
	ld a, c
	ld [de], a
	ret

.newline
	ld a, [wNbNewlines]
	dec a
	ld [wNbNewlines], a
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
	ld a, [wNbNewlines]
	dec a
	ld [wNbNewlines], a
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



TEXT_CONT_STR equ 0
TEXT_NEW_STR  equ 1
	EXPORT TEXT_CONT_STR
	EXPORT TEXT_NEW_STR
; Sets up the VWF engine to start printing text
; WARNING: If flushing the string, the auto-wordwrapper assumes that a new line is started
;          (You might get odd gfx otherwise if the text ended close enough to the tile border)
;          If needed, manually modify `wLineRemainingPixels` after calling this
; WARNING: If you want to print to a specific tile ID, set wTextCurTile *after* calling this
;          (Obviously, don't do this if you're not flushing the string)
; WARNING: Variant is reset when calling this, but the language is preserved
; WARNING: Source data located between $FF00 and $FFFF will probably cause some malfunctioning
; NOTICE: Source data outside of ROM banks is fine, but banking won't be performed.
;         Any ROM bank number is thus fine, but avoid 0 or values too high as this triggers spurious warnings in BGB
; @param hl Pointer to the string to be displayed
; @param b  Bank containing the string
; @param a  Non-zero to flush the current string (use zero if you want to keep printing the same string)
; @return a 0
; @return hl wTextCharset
; @return f NC and Z
; @destroy bc de
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
	; Reset auto line-wrapper (assuming we're starting a new line)
	ld a, [wTextLineLength]
	ld [wLineRemainingPixels], a
	; Don't flush if current tile is empty
	ld a, [wTextCurPixel]
	cp 2 ; Again, last pixel of all tiles is empty
	call nc, FlushVWFBuffer
	; Reset position always, though
	xor a
	ld [wTextCurPixel], a
	ld [wNbNewlines], a
.dontFlush

	; Force buffer refill by making these two identical
	; wTextReadPtrLow needs to be this to refill the full buffer
	ld a, LOW(wTextCharBufferEnd)
	ld [wTextFillPtrEnd], a
	ld [wTextReadPtrEnd], a
	ld [wTextReadPtrLow], a

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

FlushVWFBuffer::
	push hl

	; Calculate ptr to next tile
	ld a, [wTextTileBlock]
	ld h, a
	ld a, [wTextCurTile]
	swap a ; Multiply by 16
	ld d, a
	and $F0
	ld e, a
	xor d
	add a, h
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
	ld hl, wLastTextTile
	ld a, [hld]
	ld c, a
	assert wLastTextTile - 1 == wTextCurTile
	ld a, [hl]
	cp c
	inc a
	jr c, .noWrap
	ld a, [wWrapTileID]
.noWrap
	ld [hl], a

	ld hl, wFlushedTiles
	inc [hl]
	pop hl
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
	jr nz, .delay
	; xor a
	ld [wFlushedTiles], a

	ldh a, [hCurROMBank]
	push af
	ld a, BANK(_PrintVWFChar)
	ldh [hCurROMBank], a
	ld [rROMB0], a
	call _PrintVWFChar
	pop af
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ret

.delay
	dec a
	ld [hl], a
	ret


RefillerOnlyControlChar:
	ld bc, _RefillCharBuffer
	push bc

	push hl
	add a, a
	add a, LOW(RefillerOnlyControlChars)
	ld l, a
	ld a, $FF ; If we're here, the value in A is negative
	adc a, HIGH(RefillerOnlyControlChars)
	jr RefillerJumpControlChar

RefillerControlChar:
	add a, " " ; Restore the original value
	add a, a ; Double for pointer calculation, and check for 0
	jr z, RefillerTryReturning

	ld bc, _RefillCharBuffer.afterControlChar
	push bc
	inc e ; Otherwise the char isn't counted to be written!
	push hl
	add a, LOW(RefillerControlChars - 2)
	ld l, a
	adc a, HIGH(RefillerControlChars - 2)
	sub l
RefillerJumpControlChar:
	ld h, a
	ld a, [hli]
	ld b, [hl]
	ld c, a
	pop hl
	push bc
	ret


RefillerTryReturning:
	ld hl, wTextStackSize
	ld a, [hl]
	ld b, a
	add a, a
	ld [de], a ; If we're returning, we will need to write that $00; otherwise, it'll be overwritten
	jp z, _RefillCharBuffer.done ; Too far to `jr`
	dec b
	ld [hl], b
	add a, b ; a = stack size * 3 + 2
	add a, LOW(wTextStack)
	ld l, a
	adc a, HIGH(wTextStack)
	sub l
	ld h, a
	jr RestartCharBufRefill

; Refills the char buffer, assuming at least half of it has been read
; Newlines are injected into the buffer to implement auto line-wrapping
; @param hl The current read ptr into the buffer
RefillCharBuffer:
	ld de, wTextCharBuffer
	; First, copy remaining chars into the buffer
	ld a, [wTextFillPtrEnd]
	sub l
	ld c, a
	jr z, .charBufferEmpty
.copyLeftovers
	ld a, [hli]
	ld [de], a
	inc e
	dec c
	jr nz, .copyLeftovers
.charBufferEmpty

	; Cache charset ptr to speed up calculations
	ld a, [wTextCharset]
	ld [wRefillerCharset], a
	add a, LOW(CharsetPtrs)
	ld l, a
	adc a, HIGH(CharsetPtrs)
	sub l
	ld h, a
	ld a, [hli]
	add a, 8 ; Code later on will want a +8 offset
	ld [wCurCharsetPtr], a
	ld a, 0 ; If you try to optimize this to `xor a` I will kick you in the nuts
	adc a, [hl]
	ld [wCurCharsetPtr+1], a

	; Set a position to insert a newline at if the first word overflows
	xor a
	ld [wNewlinePtrLow], a

	; Get ready to read chars into the buffer
	ld hl, wTextSrcBank
RestartCharBufRefill:
	ld a, [hld]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	assert wTextSrcBank - 1 == wTextSrcPtr + 1
	ld a, [hld]
	ld l, [hl]
	ld h, a

_RefillCharBuffer:
	ld a, [hli]
	cp FIRST_READER_ONLY_CONTROL_CHAR
	jr nc, RefillerOnlyControlChar
	ld [de], a
	sub " "
	jr c, RefillerControlChar ; The refiller needs to be aware of some control chars

	; Add char length to accumulated one
	push hl
	ld hl, wCurCharsetPtr
	ld c, [hl]
	inc hl
	ld b, [hl]
	ld l, a
	ld h, 0
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, bc ; Char * 8 + base + 8
	ld c, a ; Stash this for later
	ld b, 0
	add hl, bc ; Char * 9 + base + 8
	ld a, [wLineRemainingPixels]
	sub a, [hl]
.insertCustomSize
	jr nc, .noNewline
	ld b, a ; Stash this for later
	; Line length was overflowed, inject newline into buffer
	; Get ptr to newline injection point
	ld h, d ; ld h, HIGH(wTextCharBuffer)
	ld a, [wNewlinePtrLow]
	ld l, a
	ld d, "\n"
	ld a, [wTextRemainingLines]
	dec a
	jr nz, .linesRemain
	inc a
	ld d, TEXT_SCROLL
.linesRemain
	ld [wTextRemainingLines], a
	ld a, [wNewlinesUntilFull]
	dec a
	jr nz, .dontPause
	ld d, TEXT_WAITBTN_SCROLL
	ld a, [wTextNbLines]
.dontPause
	ld [wNewlinesUntilFull], a
	; Dashes aren't overwritten on newlines, instead the newline is inserted right after
	ld a, [hl]
	cp " "
	jr z, .overwritingNewline
	cp TEXT_ZWS
	jr nz, .noSoftHyphen
	; A soft hyphen causes a hyphen to be placed over it, after which the newline is inserted
	ld a, "-"
	ld [hli], a
.noSoftHyphen
	; We're going to shift the entire buffer right, so count an extra char...
	; ...unless doing so would overflow the buffer.
	ld a, e
	cp LOW(wTextCharBufferEnd - 1)
	jr z, .bufferFull
	inc e
.bufferFull
	; Swap characters between `d` and `[hl]` (we already have the char to be inserted in `d`)
.copyNewlinedChars
	ld a, d
	ld d, [hl]
	ld [hli], a
	ld a, e ; Stop when we're about to write the last char
	cp l
	jr nz, .copyNewlinedChars
	; But write it, of course!
.overwritingNewline
	ld [hl], d
	; Restore dest ptr high byte
	ld d, h ; ld d, HIGH(wTextCharBuffer)
	; Compute the amount of pixels remaining after inserting the newline
	; pixels now = pixels before word - word length ⇒ word length = pixels before word - pixels now
	ld a, [wPixelsRemainingAtNewline]
	sub b
	ld b, a
	; new line's pixels = line length - word length
	ld a, [wTextLineLength]
	sub b
.noNewline
	ld [wLineRemainingPixels], a
	pop hl

	ld a, c
	; If the character is a dash or a space, a newline can be inserted
	and a ; cp " " - " "
	jr z, .canNewline
	inc e ; This increment is also shared by the main loop
	cp "-" - " " ; Dashes aren't *overwritten* by the newline, instead it's inserted after
	ld a, e ; The increment has to be placed in an awkward way because it alters flags
	jr z, .canNewlineAfter

.afterControlChar
	ld a, e
	cp LOW(wTextCharBufferEnd - 2) ; Give ourselves some margin due to multi-byte control chars
	jr c, _RefillCharBuffer

	dec e ; Compensate for what's below
.done
	inc e ; If we jumped to .done, we have written a terminator, account for it
	; Write src ptr for later
	ld a, l
	ld [wTextSrcPtr], a
	ld a, h
	ld [wTextSrcPtr+1], a
	ldh a, [hCurROMBank]
	ld [wTextSrcBank], a
	; End the buffer refill at newlineable chars only
	ld a, [wNewlinePtrLow]
	cp 2
	jr nc, .foundNewlineableChar
	ld a, e
.foundNewlineableChar
	ld [wTextReadPtrEnd], a
	ld a, e
	ld [wTextFillPtrEnd], a

	ld a, BANK(_PrintVWFChar)
	ldh [hCurROMBank], a
	ld [rROMB0], a
	; Restart printer's reading
	ld hl, wTextCharBuffer
	ret


.canNewline
	ld a, e
	inc e
.canNewlineAfter
	ld [wNewlinePtrLow], a
	ld a, [wLineRemainingPixels]
	ld [wPixelsRemainingAtNewline], a
	jr .afterControlChar


Reader2ByteNop:
	ld a, [hli]
	ld [de], a
	inc e
Reader1ByteNop:
	ret

ReaderSoftHyphen:
	; TODO: the added hyphen might overflow the line when wrapping occurs
	pop bc ; We will take a detour instead of returning
	ld a, TEXT_ZWS
	ld [de], a
	jr _RefillCharBuffer.canNewline

ReaderSetFont:
	ld a, [wRefillerCharset]
	ld c, a
	ld [wRefillerPrevFont], a
	ld a, [hli]
	ld [de], a
	inc e
	xor c
	and $F0
	jr ReaderUpdateCharset

ReaderRestoreFont:
	ld a, [wRefillerCharset]
	and $0F
	ld c, a
	ld a, [wRefillerPrevFont]
	and $F0
	jr ReaderUpdateCharset

ReaderSetVariant:
	ld a, [wRefillerCharset]
	ld c, a
	ld [wRefillerPrevVariant], a
	ld a, [hli]
	ld [de], a
	inc e
	xor c
	and $0F
	jr ReaderUpdateCharset

ReaderRestoreVariant:
	ld a, [wRefillerCharset]
	and $F0
	ld c, a
	ld a, [wRefillerPrevVariant]
	and $0F
	; Fall through
ReaderUpdateCharset:
	xor c
	ld [wRefillerCharset], a
	add a, LOW(CharsetPtrs)
	ld c, a
	adc a, HIGH(CharsetPtrs)
	sub c
	ld b, a
	ld a, [bc]
	add a, 8 ; Add the offset to the character widths
	ld [wCurCharsetPtr], a
	inc bc
	ld a, [bc]
	adc a, 0
	ld [wCurCharsetPtr+1], a
	ret

ReaderPrintBlank:
	pop bc ; We're not gonna return because we're gonna insert a custom size instead
	ld a, [hli] ; Read number of blanks
	ld [de], a
	; inc e ; Don't increment dest ptr because the code path we'll jump into will do it
	ld c, a
	ld a, [wLineRemainingPixels]
	sub c
	; We'll be jumping straight in the middle of some code path, make sure not to break it
	push hl
	ld c, "A" ; Make sure we won't get a newline
	jp _RefillCharBuffer.insertCustomSize ; Too far to `jr`

ReaderWaitButton:
	; Don't auto-wait for user input until the textbox has been entirely freshened
	ld a, [wTextNbLines]
	inc a ; The next newline will actually start introducing new text
	ld [wNewlinesUntilFull], a
	ret

	; For the purpose of line length counting, newline, clearing and scrolling are the same
	; For height counting, however...
ReaderNewline:
	ld a, [wTextRemainingLines]
	dec a
	ld [wTextRemainingLines], a
	jr nz, ReaderScroll.checkFullBox
	dec e ; dec de
	ld a, TEXT_SCROLL
	ld [de], a
	inc e ; inc de

ReaderScroll:
	ld a, [wTextRemainingLines]
	inc a
	ld [wTextRemainingLines], a
.checkFullBox
	ld a, [wNewlinesUntilFull]
	dec a
	jr nz, StartNewLine
	dec e ; dec de
	ld a, TEXT_WAITBTN_SCROLL
	ld [de], a
	inc e ; inc de
ReaderWaitButtonScroll:
	ld a, [wTextRemainingLines]
	inc a
	ld [wTextRemainingLines], a
	ld a, [wTextNbLines]
	jr StartNewLine

ReaderClear:
	ld a, [wTextNbLines]
	ld [wTextRemainingLines], a
StartNewLine:
	ld [wNewlinesUntilFull], a
	; Reset line length, since we're forcing a newline
	ld a, [wTextLineLength]
	ld [wLineRemainingPixels], a
	ret

; Sets text ptr to given location
ReaderJumpTo:
	ld a, [hli]
	ld b, a
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, b
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ret

; Start printing a new string, then keep writing this one
; NOTE: avoids corruption by preventing too much recursion, but this shouldn't happen at all
ReaderCall:
	ld a, [wTextStackSize]
	IF DEF(STACK_OVERFLOW_HANDLER)
		cp TEXT_STACK_CAPACITY
		call nc, STACK_OVERFLOW_HANDLER
	ENDC

	; Read target ptr
	inc a ; Increase stack size
	ld [wTextStackSize], a

	; Get ptr to end of 1st empty entry
	ld b, a
	add a, a
	add a, b
	add a, LOW(wTextStack - 1)
	ld c, a
	adc a, HIGH(wTextStack - 1)
	sub c
	ld b, a
	; Save ROM bank immediately, as we're gonna bankswitch
	ldh a, [hCurROMBank]
	ld [bc], a
	dec bc

	; Swap src ptrs
	ld a, [hli]
	ld [de], a ; Use current byte in char buffer as scratch
	; Save src ptr now
	inc hl
	inc hl
	ld a, h
	ld [bc], a
	dec bc
	ld a, l
	ld [bc], a
	; Read new src ptr
	dec hl
	ld a, [hld]
	ld l, [hl]
	ld h, a
	; Perform bankswitch now that all bytecode has been read
	ld a, [de]
	ldh [hCurROMBank], a
	ld [rROMB0], a
	ret


SECTION "VWF ROMX functions + data", ROMX

PrintVWFControlChar:
	IF DEF(BAD_CTRL_CHAR_HANDLER)
		; Check if ctrl char is valid
		cp TEXT_BAD_CTRL_CHAR
		call nc, BAD_CTRL_CHAR_HANDLER
	ENDC

	; Control char, run the associated function
	ld de, _PrintVWFChar.charPrinted
	push de

	; Push the func's addr (so we can preserve hl when calling)
	add a, a
	add a, LOW(ControlCharFuncs - 2)
	ld e, a
	adc a, HIGH(ControlCharFuncs - 2)
	sub e
	ld d, a
	ld a, [de]
	ld c, a
	inc de
	ld a, [de]
	ld b, a
	push bc
	ret ; Actually jump to the function, passing `hl` as a parameter for it to read (and advance)

_PrintVWFChar:
	ld h, HIGH(wTextCharBuffer)
	ld a, [wTextReadPtrLow]
	ld l, a

.setDelayAndNextChar
	; Reload delay
	ld a, [wTextLetterDelay]
	ld [wTextNextLetterDelay], a

.nextChar
	; First, check if the buffer is sufficiently full
	; Making the buffer wrap would be costly, so we're keeping a safety margin
	; Especially since control codes are multi-byte
	ld a, [wTextReadPtrEnd]
	cp l
	IF DEF(OVERREAD_HANDLER)
		call c, OVERREAD_HANDLER ; This needs to be first as it's a no-return
	ENDC
	call z, RefillCharBuffer ; If it was second this function could destroy carry and trigger it

	; Read byte from string stream
	ld a, [hli]
	and a ; Check for terminator
	jr z, .return
	cp " "
	jr c, PrintVWFControlChar

; Print char

	; Save src ptr & letter ID
	push hl

	sub " "
	ld e, a

	; Get ptr to charset table
	ld a, [wTextCharset]
	add a, LOW(CharsetPtrs)
	ld l, a
	adc a, HIGH(CharsetPtrs)
	sub l
	ld h, a
	ld a, [hli]
	ld b, [hl]
	ld c, a

	; Get ptr to letter
	ld d, 0
	ld l, e
	ld h, d ; ld h, 0
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, de ; * 9
	add hl, bc
	ld d, h
	ld e, l

	; Get dest buffer ptr
	ld hl, wTextTileBuffer

	ld a, 8
.printOneLine
	ldh [hVWFRowCount], a

	ld a, [wTextCurPixel]
	ld b, a
	and a ; Check now if shifting needs to happen

	ld a, [de]
	inc de

	ld c, 0
	jr z, .doneShifting
.shiftRight
	rra ; Actually `srl a`, since a 0 bit is always shifted in
	rr c
	dec b
	jr nz, .shiftRight
.doneShifting
	ld b, a

	ld a, [wTextColorID]
	rra
	jr nc, .noLSB
	ld a, b
	or [hl]
	ld [hl], a

	set 4, l
	ld a, c
	or [hl]
	ld [hl], a
	res 4, l

	ld a, [wTextColorID]
	rra
.noLSB
	inc l

	rra
	jr nc, .noMSB
	ld a, b
	or [hl]
	ld [hl], a

	set 4, l
	ld a, c
	or [hl]
	ld [hl], a
	res 4, l
.noMSB
	inc l

	ldh a, [hVWFRowCount]
	dec a
	jr nz, .printOneLine

	; Advance read by size
	ld hl, wTextCurPixel
	ld a, [de]
	add a, [hl]
	ld [hl], a

	; Restore src ptr
	pop hl

.charPrinted
	; Check if flushing needs to be done
	ld a, [wTextCurPixel]
	sub 8
	jr c, .noTilesToFlush

	; Move back by 8 pixels
	ld [wTextCurPixel], a
	; Flush them to VRAM
	call FlushVWFBuffer
	; Try flushing again (happens with characters 9 pixels wide)
	; We might never use 9-px chars, but if we do, there'll be support for them ^^
	jr .charPrinted

; This block of code is only here to avoid turning a `jp` into a `jr`
.return
	; Tell caller we're done (if we're not, this'll be overwritten)
	ld a, $FF
	ld [wTextSrcPtr + 1], a
	jr .flushAndFinish

.noTilesToFlush
	; If not printing next char immediately, force to flush
	ld a, [wTextNextLetterDelay]
	and a
	jp z, .setDelayAndNextChar
	dec a
	ld [wTextNextLetterDelay], a
	; Write back new read ptr into buffer for next iteration
	ld a, l
	ld [wTextReadPtrLow], a

.flushAndFinish
	; Check if flushing is necessary
	ld a, [wTextCurPixel]
	cp 2
	ret c

	; We're not using FlushVWFBuffer because we don't want to advance the tile
	ld a, [wTextTileBlock]
	ld d, a
	ld a, [wTextCurTile]
	swap a
	ld h, a
	and $F0
	ld l, a
	xor h
	add a, d
	ld h, a
	ld de, wTextTileBuffer
.copyTile
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .copyTile
	ld a, [de]
	ld [hli], a
	inc e ; inc de
	ld a, [de]
	ld [hli], a
	inc e ; inc de
	bit 4, e
	jr z, .copyTile
	ret


ControlCharFuncs:
	CTRL_CHAR_PTRS


TextDelay:
	ld a, [hli]
	ld [wTextNextLetterDelay], a
	ret


TextRestoreFont:
	ld de, wTextCharset
	ld a, [de]
	and $0F
	ld b, a
	ld a, [wPreviousFont]
	and $F0
	jr _TextSetCharset

TextRestoreVariant:
	ld de, wTextCharset
	ld a, [de]
	and $F0
	ld b, a
	ld a, [wPreviousVariant]
	and $0F
	jr _TextSetCharset

TextSetVariant:
	ld de, wTextCharset
	ld a, [de]
	ld [wPreviousVariant], a
	and $F0
	ld b, a
	ld a, [hli]
	and $0F
	jr _TextSetCharset

TextSetFont:
	ld de, wTextCharset
	ld a, [de]
	ld [wPreviousFont], a
	and $0F
	ld b, a
	ld a, [hli]
	and $F0
_TextSetCharset:
	or b
	ld [de], a
	jr PrintNextCharInstant


TextSetColor:
	ld a, [hli]
	and 3
	ld [wTextColorID], a
	jr PrintNextCharInstant


skip_key: MACRO
	IF !DEF(\1)
		FAIL "Please define \1"
	ELSE
		; Do not use ELIF to work around https://github.com/gbdev/rgbds/issues/764
		IF \1 != 0
			ldh a, [\2]
			IF \1 == 1
				rra
				jr c, \3
			ELIF \1 == 1 << 7
				add a, a
				jr c, \3
			ELSE
				and \1
				jr nz, \3
			ENDC
		ENDC
	ENDC
ENDM
TextWaitButton:
	xor a ; FIXME: if other bits than 7 and 6 get used, this is gonna be problematic
	ld [wTextFlags], a
	; End this char if suitable user input is found
	skip_key SKIP_HELD_KEYS, hHeldKeys, PrintNextCharInstant
	skip_key SKIP_PRESSED_KEYS, hPressedKeys, PrintNextCharInstant
	; If no button has been pressed, keep reading this char
	; Ensure the engine reacts on the very next frame to avoid swallowing buttons
	ld a, 1
	ld [wTextNextLetterDelay], a
	; We know that text is running, so it's fine to overwrite bit 7
	ld a, $40
	ld [wTextFlags], a
	; Decrement src ptr so this char keeps getting read
	dec hl
	ret


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
	; Fall through

PrintNextCharInstant:
	xor a
	ld [wTextNextLetterDelay], a
	ret


TextWaitButtonScroll:
	call TextWaitButton
	; The function returns with a = 0 iff the user has input something
	and a
	ret nz
	; fallthrough

TextScroll:
	push hl

	ld b, b ; You'll have to write your own code here
		ld hl, vText
		ld de, vText + SCRN_VX_B
		ld b, TEXT_HEIGHT_TILES - 1
	.shiftTilemapRows
		ld c, TEXT_WIDTH_TILES
	.shiftRow
		ldh a, [rSTAT]
		and STATF_BUSY
		jr nz, .shiftRow
		ld a, [de]
		ld [hli], a
		inc e
		dec c
		jr nz, .shiftRow
		ld a, e
		add a, SCRN_VX_B - TEXT_WIDTH_TILES
		ld e, a
		adc a, d
		sub e
		ld d, a
		ld hl, -SCRN_VX_B
		add hl, de
		dec b
		jr nz, .shiftTilemapRows
		lb bc, 0, TEXT_WIDTH_TILES
		call LCDMemsetSmallFromB

	ld hl, wPenPosition
	ld a, [hl]
	sub SCRN_VX_B
	ld [hli], a
	jr nc, .noCarry
	dec [hl]
.noCarry
	pop hl
	; fallthrough

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
	jr PrintNextCharInstant


TextSync:
	ld a, [wTextFlags]
	set 7, a
	ld [wTextFlags], a
	ret


TextClear:
	push hl
	;;;; You'll probably want to clear some tilemap here ;;;;
	ld b, b
		ld hl, vText
		ld e, TEXT_HEIGHT_TILES
	.clearTilemap
		lb bc, 0, TEXT_WIDTH_TILES
		call LCDMemsetSmallFromB
		ld a, l
		add a, SCRN_VX_B - TEXT_WIDTH_TILES
		ld l, a
		adc a, h
		sub l
		ld h, a
		dec e
		jr nz, .clearTilemap


	; Reset text printing
	; Don't flush if current tile is empty
	ld a, [wTextCurPixel]
	cp 2
	; Flush buffer to VRAM
	call nc, FlushVWFBuffer
	; Reset position always, though
	xor a
	ld [wTextCurPixel], a
	; The pen should not advance, should we have flushed a tile
	ld [wFlushedTiles], a
	ld [wNbNewlines], a

	;;;; You will probably want to reset the pen position, too ;;;;
	ld b, b
		ld hl, vText
		call SetPenPosition

	pop hl
	ret


SECTION "Charset data", ROM0

IF !DEF(NB_CHARSETS)
	FAIL "Please define NB_CHARSETS!"
ENDC

CharsetPtrs::
	rsreset
	REPT NB_CHARSETS
CHARSET equs "CHARSET_{d:_RS}"
CHARSET_DEFINED equs "DEF({CHARSET})"

		IF CHARSET_DEFINED
CHARSET_LABEL equs "Charset{d:_RS}"
			dw CHARSET_LABEL
			PUSHS
SECTION "Charset {d:_RS}", ROM0
CHARSET_LABEL:
				INCBIN "{{CHARSET}}"
				IF @ - CHARSET_LABEL > CHARACTER_SIZE * NB_FONT_CHARACTERS
					WARN "Charset {d:_RS} is larger than expected; keep in mind they can only contain {d:NB_FONT_CHARACTERS} characters"
				ENDC
			POPS
			PURGE CHARSET_LABEL

		ELSE
			dw 0
		ENDC
		PURGE CHARSET_DEFINED
		PURGE CHARSET
		rsset _RS + 2
	ENDR


SECTION "VWF engine memory", WRAM0,ALIGN[7]

wTextCharBuffer::
	ds 64
wTextCharBufferEnd:: ; We need this not to be on a 256-byte boundary
	assert HIGH(wTextCharBuffer) == HIGH(wTextCharBufferEnd)

	assert @ & -(1 << 6)
wTextTileBuffer::
	ds $10 * 2

	assert @ & -(1 << 5)
; Format of entries: ptr(16bit LE), bank
wTextStack::
	ds TEXT_STACK_CAPACITY * 3
; Number of entries in the stack
wTextStackSize::
	db


wTextCurPixel::
	db
wTextCurTile::
	db
; ID of the last tile the VWF engine is allowed to write to
wLastTextTile::
	db
; Tells which tile to wrap to when going past the above
wWrapTileID::
	db
; This allows selecting the "tile block" to use
; Write $80 for tiles in $8000-8FFF
; Write $90 for tiles in $9000-97FF
; Other values are not officially supported, experiment yourself
wTextTileBlock::
	db

; Tells which color to use in the palette for the text (in range 0-3)
wTextColorID::
	db

; Defines which character table to use
; Upper nibble is language-defined, lower nibble is decoration
wTextCharset::
	db

wPreviousFont::
	db
wPreviousVariant::
	db

wTextSrcPtr::
	dw
wTextSrcBank:
	db

; Number of frames between each character
wTextLetterDelay::
	db
; Number of frames till next character
wTextNextLetterDelay::
	db

; Bit 6 - Whether the text engine is currently waiting for a button press
; Bit 7 - Whether the text engine is halted, for syncing (can be reset)
wTextFlags::
	db

; Number of tiles flushed, used to know how many tiles should be written to the tilemap
wFlushedTiles::
	db

; Number of newlines that occurred during this print
wNbNewlines::
	db
; ID of the tiles during which newlines occurred (NOTE: there can be duplicates due to empty lines!!)
wNewlineTiles::
	ds TEXT_NEWLINE_CAPACITY

wPenStartingPosition::
	dw
wPenPosition::
	dw
wPenCurTile::
	db

; Low byte of the read ptr into wTextCharBuffer
wTextReadPtrLow::
	db
; Where the refiller ended, i.e. where the printer needs to stop
wTextReadPtrEnd::
	db
; Where the refiller's read ended; characters between the above and this may be candidate for an
; auto linebreak, so they shouldn't be passed to the printer yet
wTextFillPtrEnd::
	db

; Number of lines of the current text area
wTextNbLines::
	db
; How many newlines remain before the text box is full
wTextRemainingLines::
	db
; How many newlines remain until the text box has been filled with fresh text
wNewlinesUntilFull::
	db
; Length, in pixels, of the current text line
wTextLineLength::
	db
wLineRemainingPixels::
	db
; Ptr to last newlineable location
wNewlinePtrLow::
	db
; wLineRemainingPixels at the time wNewlinePtrLow is updated
wPixelsRemainingAtNewline::
	db
; Charset ptr is cached by refiller to speed up reads
wCurCharsetPtr::
	dw
wRefillerCharset::
	db
wRefillerPrevFont::
	db
wRefillerPrevVariant::
	db

SECTION "VWF engine fast memory", HRAM

; How many rows are left to be drawn in the current tile
hVWFRowCount::
	db
