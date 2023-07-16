
; SPDX-License-Identifier: MIT
;
; MIT License
;
; Copyright (c) 2018-2023 Eldred Habert
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


;;; First, everything that can be configured.


; Number of elements the text stack has room for.
; Having more will cause a soft crash!
; This must not exceeed $7F, as the return logic discards bit 7 when checking for zero.
IF !DEF(TEXT_STACK_CAPACITY)
	def TEXT_STACK_CAPACITY equ 8
ENDC

; Do **NOT** print more than this amount of newlines in a single call to `PrintVWFChars`!
; This would overflow an internal buffer.
; If you want to be safe, set this to the maximum textbox height you will be using.
IF !DEF(TEXT_NEWLINE_CAPACITY)
	def TEXT_NEWLINE_CAPACITY equ 10
ENDC


;; Please ignore this line and the macro definition below it.
SECTION "VWF engine", ROM0

def CTRL_CHAR_PTRS equs ""
	rsreset
MACRO control_char
	IF DEF(PRINT_CHARMAP)
		PRINTLN "charmap \"<\1>\", {d:_RS}"
	ENDC
	def TEXT_\1 rb 1
	IF DEF(EXPORT_CONTROL_CHARS)
		EXPORT TEXT_\1
	ENDC

	IF _NARG > 1
		dw \2
		IF _NARG == 2
			IF !DEF(FIRST_READER_ONLY_CONTROL_CHAR)
				def FIRST_READER_ONLY_CONTROL_CHAR rb 0
			ENDC
		ELIF DEF(FIRST_READER_ONLY_CONTROL_CHAR)
			FAIL "All reader-only control chars must be consecutive! ("
		ELSE
			redef CTRL_CHAR_PTRS equs "{CTRL_CHAR_PTRS}\ndw \3"
		ENDC
	ENDC
ENDM

	;;;;;;;;;;;;;;;;;;;;;;;;;;;; Control characters ;;;;;;;;;;;;;;;;;;;;;;;;

	control_char END ; Handled specially throughout the engine. (TODO: should it?)
ReaderControlChars: ; Please ignore this line.
	control_char SET_FONT,        ReaderSetFont,                TextSetFont
	control_char RESTORE_FONT,    ReaderRestoreFont,            TextRestoreFont
	control_char SET_VARIANT,     ReaderSetVariant,             TextSetVariant
	control_char RESTORE_VARIAN,  ReaderRestoreVariant,         TextRestoreVariant
	control_char SET_COLOR,       Reader2ByteNop,               TextSetColor
	control_char BLANKS,          ReaderPrintBlank,             TextPrintBlank
	control_char DELAY,           Reader2ByteNop,               TextDelay
	control_char WAITBTN,         ReaderWaitButton,             TextWaitButton
	control_char CLEAR,           ReaderClear,                  TextClear
	control_char NEWLINE,         ReaderNewline,                TextNewline
	assert TEXT_NEWLINE == "\n" ; This ensures that you can safely write "\n" instead of "<NEWLINE>".
	control_char SYNC,            Reader1ByteNop,               TextSync
	control_char SCROLL,          ReaderScroll,                 TextScroll
	control_char WAITBTN_SCROLL,  ReaderWaitButtonScroll,       TextWaitButtonScroll
	control_char ZWS,             ProcessInputChar.canNewline,  PrintNextCharInstant

	; Reader-only control chars.
	control_char CALL,            ReaderCall
	control_char JUMP,            ReaderJumpTo
	control_char SOFT_HYPHEN,     ReaderSoftHyphen


	def FIRST_FONT_CHAR rb 0
	redef FIRST_FONT_CHAR equ 32 ; HACK: temporary until charset definitions are overhauled
	IF DEF(EXPORT_CONTROL_CHARS)
		EXPORT FIRST_FONT_CHAR
	ENDC
	def NB_FONT_CHARACTERS equ 256 - FIRST_FONT_CHAR



;;; Now, some things that the rest of the engine uses.


; `wFlags` bits, defined in descending order.
rsset 8
MACRO text_flag
	rsset _RS - 1
	def TEXTB_\1 equ _RS
	def TEXTF_\1 equ 1 << TEXTB_\1
	EXPORT TEXTB_\1, TEXTF_\1
ENDM
	text_flag WAITBUTTON
	text_flag SYNC


IF !DEF(lb)
	FAIL "Please define the `lb` macro before `INCLUDE \"vwf.asm\"`."
ENDC

IF !DEF(get_rom_bank_id)
	FAIL "Please define the `get_bank_id` macro before `INCLUDE \"vwf.asm\"`."
ENDC

IF !DEF(switch_rom_bank)
	FAIL "Please define the `switch_rom_bank` macro before `INCLUDE \"vwf.asm\"`."
ENDC


; Debugfile-related macros.

IF DEF(PRINT_DEBUGFILE)
	PRINTLN "@debugfile 1.0.0"
	MACRO dbg_action ; <function>, <action:str> [, <condition:dbg_expr>]
		def OFS_FROM_BASE equ @ - \1
		def ACTION_COND equs ""
		IF _NARG > 2
			redef ACTION_COND equs "\3"
		ENDC
		PRINTLN "\1+{d:OFS_FROM_BASE} x {ACTION_COND}: ", \2
		PURGE OFS_FROM_BASE, ACTION_COND
	ENDM
	MACRO runtime_assert ; <function>, <condition:dbg_expr> [, <message:dbg_str>]
		def MSG equs "assert failure"
		IF _NARG > 2
			redef MSG equs \3
		ENDC
		dbg_action \1, "alert \"{MSG}\"", !(\2)
		PURGE MSG
	ENDM
	MACRO unreachable ; <function> [, <message:dbg_str>]
		def MSG equs "unreachable code reached!"
		IF _NARG > 1
			redef MSG equs \2
		ENDC
		dbg_action \1, "alert \"In \1: {MSG}\""
		PURGE MSG
	ENDM
ELSE
	def runtime_assert equs ";"
	def unreachable equs ";"
ENDC


def CHARACTER_HEIGHT equ 8 ; pixels
def CHARACTER_NB_BYTES equ CHARACTER_HEIGHT + 1 ; How many bytes a glyph takes in ROM.


;;; Then, the interface functions.
;;; First are the two "setup" functions, then the two "stepping" functions.


; Sets up the VWF engine to start printing text.
; WARNING: If flushing the string, the auto-wordwrapper assumes that a new line is started!
;          (You might get odd gfx otherwise if the text ended close enough to the tile border.)
;          If needed, manually modify `wLineRemainingPixels` after calling this.
; WARNING: If you want to print to a specific tile ID, set `wCurTile` *after* calling this!
;          (Obviously, don't do this if you're not flushing the string.)
; WARNING: Variant is reset when calling this, but the font is preserved!
; WARNING: Source data located between $FF00 and $FFFF will probably cause some malfunctioning!
; NOTICE: Source data outside of ROM banks is fine, but banking won't be performed.
;         Any ROM bank number is thus fine, but avoid 0 or values too high as this triggers spurious warnings in BGB!
;
; @param hl: Pointer to the string to be displayed
; @param b:  Bank containing the string
; @param a:  Whether to flush the current string (either of the constants below).
;            Use `CONT_STR` if you want to keep printing to the same string you previously were.
	def TEXT_CONT_STR equ 0
	def TEXT_NEW_STR  equ 1
	EXPORT TEXT_CONT_STR, TEXT_NEW_STR
; @return a:  0
; @return f:  NC and Z
; @return hl: wCharsetID
; @destroy bc de
SetupVWFEngine::
	runtime_assert SetupVWFEngine, h != $FF, "VWF engine set up with weird source $\{hl,$\}"
	and a ; Set Z flag for test below.

	; Write source pointer.
	; We do this now because `FlushVWFBuffer` destroys these regs.
	ld a, b
	ld [wSrcBank], a
	ld a, l
	ld [wSrcPtr], a
	ld a, h
	ld [wSrcPtr + 1], a

	; Flag preserved from `and a` above.
	jr z, .dontFlush
	; Reset auto line-wrapper (it's assumed that we're starting a new line).
	ld a, [wLineLength]
	ld [wLineRemainingPixels], a
	; Don't flush if current tile is empty.
	ld a, [wCurPixel]
	cp 2 ; As always, the last pixel of all tiles is empty.
	call nc, FlushVWFBuffer
	; Reset the position always, though!
	xor a
	ld [wCurPixel], a
	ld [wNbNewlines], a
.dontFlush

	; Force the entire char buffer to be refilled.
	; wReadPtrLow needs to be this to refill the full buffer
	ld a, LOW(wInputBuffer.end) ; TODO: I don't think the actual value matters? `xor a` may be sufficient?
	ld [wReadPtrEnd], a ; This being equal to the next one causes `RefillInputBuffer` to be called.
	ld [wReadPtrLow], a
	ld [wFillPtrEnd], a ; This being equal to the previous one causes the entire buffer to be refilled.

	; Usually, colour #3 is black, so use that.
	ld a, 3
	ld [wColorID], a

	; Preserve the font but reset the decoration.
	; TODO: is this really useful?
	ld hl, wCharsetID
	ld a, [hl]
	and $F0
	ld [hli], a
	assert wCharsetID + 1 == wReaderCharsetID
	ld [hl], a
	; Compute the charset's pointer for the "reader" component.
	; It will be re-computed whenever a control character changes it.
	add a, LOW(CharsetPtrs)
	ld l, a
	adc a, HIGH(CharsetPtrs)
	sub l
	ld h, a
	ld a, [hli]
	add a, 8 ; The reader only cares about glyph widths; pre-adding 8 allows speeding up each access.
	ld [wCurCharsetPtr], a
	ld a, 0 ; Carry needs to be preserved for the below `adc`.
	adc a, [hl]
	ld [wCurCharsetPtr+1], a

	; Set initial letter delay to 0, so the next call to `TickVWFEngine` does not delay.
	xor a
	ld [wNextLetterDelay], a
	ret


; Points the printer to somewhere in memory.
; Note that the printer is primarily intended to write to VRAM, but it can realistically write to
; anywhere in memory; as long as it is a 32-tiles-wide tilemap that can accomodate however many lines
; are configured in `wNbLines`.
; NOTICE: Please call this after `SetupVWFEngine`, so wCurTile is properly updated!
;
; @param hl: The address to print to (usually in the tilemap).
; @destroy a
SetPrinterPosition::
; Note: some callers rely on this presetving `hl`.
	ld a, l
	ld [wPrinterHeadPtr], a
	ld [wPrinterStartingPosition], a
	ld a, h
	ld [wPrinterHeadPtr + 1], a
	ld [wPrinterStartingPosition + 1], a

	ld a, [wCurTile]
	ld [wPrinterCurTile], a
	ret


; Ticks down the delay if it's not zero, or performs one drawing step otherwise.
; Might print more than 1 char, eg. if wLetterDelay is zero.
; When finished, the high byte of `wSrcPtr` is set to $FF.
; @destroy af bc de hl
TickVWFEngine::
	ld hl, wNextLetterDelay
	ld a, [hl]
	and a
	jr nz, .delay
	; xor a ; a = 0 already.
	ld [wNbFlushedTiles], a

	get_rom_bank_id
	push af
	ld a, BANK(_PrintVWFChar)
	switch_rom_bank
	call _PrintVWFChar ; Since that's a *lot* of code, jump to ROMX.
	pop af
	switch_rom_bank
	ret

.delay
	dec a
	ld [hl], a
	ret


; "Prints" any tiles that may have been "painted" to the tilemap.
; Call this after `TickVWFEngine`!
; NOTICE: *Both* this and `TickVWFEngine` write tile data directly to VRAM!
;         To be precise, every time a tile has been entirely written to, `TickVWFEngine` will
;         commit it to VRAM; however, the "incomplete" tile is only written by this function.
; @destroy af bc de hl
PrintVWFChars::
	ld hl, wPrinterHeadPtr
	ld a, [hli]
	ld e, a
	ld a, [hli]
	ld d, a
	assert wPrinterHeadPtr + 2 == wPrinterCurTile
	ld c, [hl]

	; We need to load this pointer even if no tiles have been flushed, because a newline does not
	; flush an empty tile.
	ld hl, wNewlineTiles

	; If any tiles have been flushed, we must write their IDs to the tilemap.
	ld a, [wNbFlushedTiles]
	and a
	jr z, .noNewTiles
	ld b, a ; TODO: would it be more efficient to put this in HRAM and use it as the loop counter?
	xor a
	ld [wNbFlushedTiles], a
.writeNewTile
	; Check if a newline was emitted here.
	; FIXME: we end up jumping really often, since most of the time no newlines were printed...
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
	inc de
	; Go to the next tile ID.
	inc c
	ld a, [wLastTextTile]
	cp c
	jr nc, .tileIDDoesNotWrap
	ld a, [wWrapTileID]
	ld c, a
.tileIDDoesNotWrap
	dec b
	jr nz, .writeNewTile
.noNewTiles

	; There may have been newlines before the "active" tile.
	; FIXME: can't this be merged with the pervious loop?
.tryAgain
	ld a, [wNbNewlines]
	and a
	jr z, .noFinalNewline
	ld a, c
	cp [hl]
	jr z, .finalNewline
.noFinalNewline
	; FIXME: is this actually useful here? Isn't it already zero?
	xor a
	ld [wNbNewlines], a

	; Write back the variables for the next iteration.
	ld hl, wPrinterCurTile
	ld a, c
	ld [hld], a
	assert wPrinterCurTile - 1 == wPrinterHeadPtr + 1
	ld a, d
	ld [hld], a
	ld [hl], e

	; If the current tile is not empty, write its ID as well.
	ld a, [wCurPixel]
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
	; FIXME: "merge" position using `xor ; and ; xor` instead
	ld a, [wPrinterStartingPosition]
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
	ld a, [wPrinterStartingPosition]
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



;;; From this point on, all functions are "internal" so to speak.



ReaderControlChar:
	add a, FIRST_FONT_CHAR ; Cancel out the `sub FIRST_FONT_CHAR` before jumping here (see `ProcessInputChar`).
	add a, a ; Double the ID to get a pointer table offset, and also to check for 0.
	jr z, .tryReturning

	ld bc, ProcessInputChar.afterControlChar
	push bc
	inc e ; Otherwise the char isn't counted to be written!
	push hl
	add a, LOW(ReaderControlChars - 2)
	ld l, a
	adc a, HIGH(ReaderControlChars - 2)
.jump
	sub l
	ld h, a
	ld a, [hli]
	ld b, [hl]
	ld c, a
	pop hl
	push bc
	ret

.tryReturning
	ld hl, wStackSize
	ld a, [hl]
	ld b, a
	assert TEXT_STACK_CAPACITY & $80 == 0, "Bit 7 of the stack size is discarded when checking its size!"
	add a, a
	ld [de], a ; If we're returning, we will need to write that $00; otherwise, it'll be overwritten
	jp z, ProcessInputChar.done ; Too far to `jr`
	dec b
	ld [hl], b
	add a, b ; a = stack size * 3 + 2
	add a, LOW(wStack)
	ld l, a
	adc a, HIGH(wStack)
	sub l
	ld h, a
	jr RestartCharBufRefill


; Refills the input buffer. (TODO: does at least half of it *have* to have been consumed?)
; Note that the input is not read as-is into the buffer: some control characters are processed by
; this function, and automatic line-wrapping is also performed by it (by injecting newlines).
; This is largely why it's significantly more complex than just copying bytes around...
; @param hl: The current read ptr into the buffer
RefillInputBuffer:
	ld de, wInputBuffer
	; We can be called with some bytes left in the buffer (a safety margin is kept for )
	ld a, [wFillPtrEnd]
	sub l
	ld [wNewlinePtrLow], a ; If the first word overflows, write a newline at the buffer's cut point.
	jr z, .charBufferEmpty
	ld c, a
.copyLeftovers
	ld a, [hli]
	ld [de], a
	assert HIGH(wInputBuffer) == HIGH(wInputBuffer.end - 1)
	inc e
	dec c
	jr nz, .copyLeftovers
.charBufferEmpty

	; Get ready to read chars into the buffer
	; FIXME: reading directly from a stack entry would be better, I think
	ld hl, wSrcBank
RestartCharBufRefill:
	ld a, [hld]
	switch_rom_bank
	assert wSrcBank - 1 == wSrcPtr + 1
	ld a, [hld]
	ld l, [hl]
	ld h, a

ProcessInputChar:
	ld a, [hli]
	ld [de], a
	sub FIRST_FONT_CHAR
	jr c, ReaderControlChar
	; We have a normal character.

	; Subtract the glyph's length from the remaining pixels in the line.
	push hl ; Save the input pointer while we compute a pointer to the glyph width.
	ld hl, wCurCharsetPtr ; This is offset so that
	ld c, [hl]
	inc hl
	ld b, [hl]
	ld l, a
	ld h, 0
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, bc ; Char * 8 + base + 8
	ld c, a ; We will need the glyph ID again.
	ld b, 0
	add hl, bc ; Char * 9 + base + 8

	ld a, [wLineRemainingPixels]
	sub a, [hl]
.insertedCustomSize
	jr nc, .noNewline
	; This glyph would overflow the line, so a newline must be injected.

	ld b, a ; This is by how many pixels the line was overflowed (well, it's negative).
	; TODO: if the current character is a space, write the newline to `de` directly, instead. (Might complicate logic too much?)
	; Make `hl` point to the byte where we should inject the newline.
	ld h, d ; ld h, HIGH(wInputBuffer)
	ld a, [wNewlinePtrLow]
	ld l, a
	; Determine which character we will inject.
	ld d, "\n" ; By default, that would be a newline.
	ld a, [wRemainingLines] ; If we are at the bottom of the textbox, it should be scrolled upwards instead.
	dec a
	jr nz, .linesRemain
	ld d, TEXT_SCROLL
	jr :+ ; Leave `wNbRemainingLines` at 1. (This is one byte larger, but two cycles faster.)
.linesRemain
	ld [wRemainingLines], a
:
	ld a, [wNbFreshLines] ; If this would scroll a "fresh" line off-screen, then wait for input first.
	dec a
	jr nz, .dontPause
	ld d, TEXT_WAITBTN_SCROLL
	ld a, [wNbLines]
.dontPause
	ld [wNbFreshLines], a

	; Time to actually insert the newline!
	; Whitespace characters are plainly overwritten (since newlines can be considered a form of
	; whitespace), whereas other characters (e.g. hyphens) are not.
	ld a, [hl] ; This points to the character that is considered "newline-able".
	cp " " ; Currently, space is the only character considered whitespace, but you can add more. (TODO: make this configurable via an `equs` at the top of the file?)
	jr z, .overwritingNewline
	cp TEXT_ZWS
	jr nz, .noSoftHyphen
	; A soft hyphen triggering a newline is replaced by (and behaves like) a hyphen.
	ld a, "-"
	ld [hli], a
.noSoftHyphen
	; We're going to shift the entire buffer right, so count an extra char...
	; ...unless doing so would overflow the buffer.
	ld a, e
	cp LOW(wInputBuffer.end - 1)
	jr z, .bufferFull ; TODO: ??? don't we end up dropping a character, then???
	inc e
.bufferFull
	; Swap characters between `d` and `[hl]` (we already have the char to be inserted in `d`).
.copyNewlinedChars
	ld a, d
	ld d, [hl]
	ld [hli], a
	ld a, e ; Stop when we're about to write the last character...
	cp l
	jr nz, .copyNewlinedChars
	; ...but still write it.
.overwritingNewline
	ld [hl], d
	; Restore `de` to point into the input buffer.
	ld d, h ; ld d, HIGH(wInputBuffer)
	; Compute how many pixels will remain in the new line.
	; pixels now = pixels before word - word length ⇒ word length = pixels before word - pixels now
	ld a, [wPixelsRemainingAtNewline]
	sub b
	ld b, a
	; new line's pixels = line length - word length
	ld a, [wLineLength]
	sub b

.noNewline
	ld [wLineRemainingPixels], a
	pop hl

	ld a, c ; Retrieve the ID of the character we are processing.
	; If the character is a dash or a space, register that a newline can be inserted here.
	; (A soft hyphen is a control char, so it is registered by its handler instead.)
	cp " " - FIRST_FONT_CHAR
	jr z, .canNewline
	inc e ; This increment is also shared by the main loop
	cp "-" - FIRST_FONT_CHAR ; Dashes aren't *overwritten* by the newline, instead it's inserted after
	ld a, e ; The increment has to be placed in an awkward way because it alters flags
	jr z, .canNewlineAfter

.afterControlChar
	; Keep processing characters if the buffer has room to accomodate more.
	ld a, e
	cp LOW(wInputBuffer.end - 2) ; Give ourselves some margin due to multi-byte control chars
	jr c, ProcessInputChar

	dec e ; Compensate for what's below
.done
	inc e ; If we jumped to .done, we have written a terminator, account for it
	; Write src ptr for later
	ld a, l
	ld [wSrcPtr], a
	ld a, h
	ld [wSrcPtr+1], a
	get_rom_bank_id
	ld [wSrcBank], a
	; End the buffer refill at newlineable chars only
	ld a, [wNewlinePtrLow]
	cp 2
	jr nc, .foundNewlineableChar
	ld a, e
.foundNewlineableChar
	ld [wReadPtrEnd], a
	ld a, e
	ld [wFillPtrEnd], a

	ld a, BANK(_PrintVWFChar)
	switch_rom_bank
	; Restart printer's reading
	ld hl, wInputBuffer
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
	jr ProcessInputChar.canNewline

ReaderSetFont:
	ld a, [wReaderCharsetID]
	ld c, a
	ld [wReaderPrevFont], a
	ld a, [hli]
	ld [de], a
	inc e
	xor c
	and $F0
	jr ReaderUpdateCharset

ReaderRestoreFont:
	ld a, [wReaderCharsetID]
	and $0F
	ld c, a
	ld a, [wReaderPrevFont]
	and $F0
	jr ReaderUpdateCharset

ReaderSetVariant:
	ld a, [wReaderCharsetID]
	ld c, a
	ld [wReaderPrevVariant], a
	ld a, [hli]
	ld [de], a
	inc e
	xor c
	and $0F
	jr ReaderUpdateCharset

ReaderRestoreVariant:
	ld a, [wReaderCharsetID]
	and $F0
	ld c, a
	ld a, [wReaderPrevVariant]
	and $0F
	; Fall through
ReaderUpdateCharset:
	xor c
	ld [wReaderCharsetID], a
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
	jp ProcessInputChar.insertedCustomSize ; Too far to `jr`

ReaderWaitButton:
	; Don't auto-wait for user input until the textbox has been entirely freshened
	ld a, [wNbLines]
	inc a ; The next newline will actually start introducing new text
	ld [wNbFreshLines], a
	ret

	; For the purpose of line length counting, newline, clearing and scrolling are the same
	; For height counting, however...
ReaderNewline:
	ld a, [wRemainingLines]
	dec a
	ld [wRemainingLines], a
	jr nz, ReaderScroll.checkFullBox
	dec e ; dec de
	ld a, TEXT_SCROLL
	ld [de], a
	inc e ; inc de

ReaderScroll:
	ld a, [wRemainingLines]
	inc a
	ld [wRemainingLines], a
.checkFullBox
	ld a, [wNbFreshLines]
	dec a
	jr nz, StartNewLine
	dec e ; dec de
	ld a, TEXT_WAITBTN_SCROLL
	ld [de], a
	inc e ; inc de
ReaderWaitButtonScroll:
	ld a, [wRemainingLines]
	inc a
	ld [wRemainingLines], a
	ld a, [wNbLines]
	jr StartNewLine

ReaderClear:
	ld a, [wNbLines]
	ld [wRemainingLines], a
StartNewLine:
	ld [wNbFreshLines], a
	; Reset line length, since we're forcing a newline
	ld a, [wLineLength]
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
	switch_rom_bank
	ret

; Start printing a new string, then keep writing this one
; NOTE: avoids corruption by preventing too much recursion, but this shouldn't happen at all
ReaderCall:
	ld a, [wStackSize]
	IF DEF(STACK_OVERFLOW_HANDLER)
		cp TEXT_STACK_CAPACITY
		call nc, STACK_OVERFLOW_HANDLER
	ENDC

	; Read target ptr
	inc a ; Increase stack size
	ld [wStackSize], a

	; Get ptr to end of 1st empty entry
	ld b, a
	add a, a
	add a, b
	add a, LOW(wStack - 1)
	ld c, a
	adc a, HIGH(wStack - 1)
	sub c
	ld b, a
	; Save ROM bank immediately, as we're gonna bankswitch
	get_rom_bank_id
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
	switch_rom_bank
	ret


FlushVWFBuffer::
	push hl

	; Calculate ptr to next tile
	ld a, [wTileBlock]
	ld h, a
	ld a, [wCurTile]
	swap a ; Multiply by 16
	ld d, a
	and $F0
	ld e, a
	xor d
	add a, h
	ld d, a

	; Copy buffer 1 to VRAM, buffer 2 to buffer 1, and clear buffer 2
	ld hl, wTileBuffer
	ld bc, wTileBuffer + $10
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
	cp LOW(wTileBuffer + $10)
	jr nz, .copyByte

	; Go to next tile
	ld hl, wLastTextTile
	ld a, [hld]
	ld c, a
	assert wLastTextTile - 1 == wCurTile
	ld a, [hl]
	cp c
	inc a
	jr c, .noWrap
	ld a, [wWrapTileID]
.noWrap
	ld [hl], a

	ld hl, wNbFlushedTiles
	inc [hl]
	pop hl
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
	ld h, HIGH(wInputBuffer)
	ld a, [wReadPtrLow]
	ld l, a

.setDelayAndNextChar
	; Reload delay
	ld a, [wLetterDelay]
	ld [wNextLetterDelay], a

.nextChar
	; First, check if the buffer is sufficiently full
	; Making the buffer wrap would be costly, so we're keeping a safety margin
	; Especially since control codes are multi-byte
	ld a, [wReadPtrEnd]
	cp l
	IF DEF(OVERREAD_HANDLER)
		call c, OVERREAD_HANDLER ; This needs to be first as it's a no-return
	ENDC
	call z, RefillInputBuffer ; If it was second this function could destroy carry and trigger it

	; Read byte from string stream
	ld a, [hli]
	and a ; Check for terminator
	jr z, .return
	cp " "
	jr c, PrintVWFControlChar

; Print char

	; Save src ptr & letter ID
	push hl

	sub FIRST_FONT_CHAR
	ld e, a

	; Get ptr to charset table
	ld a, [wCharsetID]
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
	ld hl, wTileBuffer

	ld a, 8
.printOneLine
	ldh [hVWFRowCount], a

	ld a, [wCurPixel]
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

	ld a, [wColorID]
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

	ld a, [wColorID]
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
	ld hl, wCurPixel
	ld a, [de]
	add a, [hl]
	ld [hl], a

	; Restore src ptr
	pop hl

.charPrinted
	; Check if flushing needs to be done
	ld a, [wCurPixel]
	sub 8
	jr c, .noTilesToFlush

	; Move back by 8 pixels
	ld [wCurPixel], a
	; Flush them to VRAM
	call FlushVWFBuffer
	; Try flushing again (happens with characters 9 pixels wide)
	; We might never use 9-px chars, but if we do, there'll be support for them ^^
	jr .charPrinted

; This block of code is only here to avoid turning a `jp` into a `jr`
.return
	; Tell caller we're done (if we're not, this'll be overwritten)
	ld a, $FF
	ld [wSrcPtr + 1], a
	jr .flushAndFinish

.noTilesToFlush
	; If not printing next char immediately, force to flush
	ld a, [wNextLetterDelay]
	and a
	jp z, .setDelayAndNextChar
	dec a
	ld [wNextLetterDelay], a
	; Write back new read ptr into buffer for next iteration
	ld a, l
	ld [wReadPtrLow], a

.flushAndFinish
	; Check if flushing is necessary
	ld a, [wCurPixel]
	cp 2
	ret c

	; We're not using FlushVWFBuffer because we don't want to advance the tile
	ld a, [wTileBlock]
	ld d, a
	ld a, [wCurTile]
	swap a
	ld h, a
	and $F0
	ld l, a
	xor h
	add a, d
	ld h, a
	ld de, wTileBuffer
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
	ld [wNextLetterDelay], a
	ret


TextRestoreFont:
	ld de, wCharsetID
	ld a, [de]
	and $0F
	ld b, a
	ld a, [wPreviousFont]
	and $F0
	jr _TextSetCharset

TextRestoreVariant:
	ld de, wCharsetID
	ld a, [de]
	and $F0
	ld b, a
	ld a, [wPreviousVariant]
	and $0F
	jr _TextSetCharset

TextSetVariant:
	ld de, wCharsetID
	ld a, [de]
	ld [wPreviousVariant], a
	and $F0
	ld b, a
	ld a, [hli]
	and $0F
	jr _TextSetCharset

TextSetFont:
	ld de, wCharsetID
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
	ld [wColorID], a
	jr PrintNextCharInstant


MACRO skip_key
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
	ld [wFlags], a
	; End this char if suitable user input is found
	skip_key SKIP_HELD_KEYS, hHeldKeys, PrintNextCharInstant
	skip_key SKIP_PRESSED_KEYS, hPressedKeys, PrintNextCharInstant
	; If no button has been pressed, keep reading this char
	; Ensure the engine reacts on the very next frame to avoid swallowing buttons
	ld a, 1
	ld [wNextLetterDelay], a
	; We know that text is running, so it's fine to overwrite bit 7
	ld a, $40
	ld [wFlags], a
	; Decrement src ptr so this char keeps getting read
	dec hl
	ret


TextPrintBlank:
	ld a, [hli]
	ld c, a
	ld a, [wCurPixel]
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
	ld [wCurPixel], a
	; Fall through

PrintNextCharInstant:
	xor a
	ld [wNextLetterDelay], a
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

	ld hl, wPrinterHeadPtr
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
	ld a, [wCurPixel]
	cp 2
	call nc, FlushVWFBuffer
	; Reset position
	xor a
	ld [wCurPixel], a

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
	ld a, [wCurTile]
	ld [de], a
	jr PrintNextCharInstant


TextSync:
	ld a, [wFlags]
	set 7, a
	ld [wFlags], a
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
	ld a, [wCurPixel]
	cp 2
	; Flush buffer to VRAM
	call nc, FlushVWFBuffer
	; Reset position always, though
	xor a
	ld [wCurPixel], a
	; The printer should not advance, should we have flushed a tile
	ld [wNbFlushedTiles], a
	ld [wNbNewlines], a

	;;;; You will probably want to reset the printer position, too ;;;;
	ld b, b
		ld hl, vText
		call SetPrinterPosition

	pop hl
	ret


SECTION "Charset data", ROM0

IF !DEF(NB_CHARSETS)
	FAIL "Please define NB_CHARSETS!"
ENDC

CharsetPtrs::
	rsreset
	REPT NB_CHARSETS
		def CHARSET equs "CHARSET_{d:_RS}"
		def CHARSET_DEFINED equs "DEF({CHARSET})"

		IF CHARSET_DEFINED
			def CHARSET_LABEL equs "Charset{d:_RS}"
			dw CHARSET_LABEL
			PUSHS
SECTION "Charset {d:_RS}", ROM0
CHARSET_LABEL:
				INCBIN "{{CHARSET}}"
				IF @ - CHARSET_LABEL > CHARACTER_NB_BYTES * NB_FONT_CHARACTERS
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

wInputBuffer::
	ds 64
wInputBuffer.end:: ; We need this not to be on a 256-byte boundary
	assert HIGH(wInputBuffer) == HIGH(wInputBuffer.end)

	assert @ & -(1 << 6)
wTileBuffer::
	ds $10 * 2

	assert @ & -(1 << 5)
; Format of entries: ptr(16bit LE), bank
wStack::
	ds TEXT_STACK_CAPACITY * 3
; Number of entries in the stack
wStackSize::
	db


wCurPixel::
	db
wCurTile::
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
wTileBlock::
	db

; Tells which color to use in the palette for the text (in range 0-3)
wColorID::
	db

; Defines which character table to use
; Upper nibble is font-defined, lower nibble is decoration
wCharsetID::
	db
wReaderCharsetID:: ; The reader has a separate copy, since it keeps track of the charset ID in parallel.
	db

wPreviousFont::
	db
wPreviousVariant::
	db

wSrcPtr::
	dw
wSrcBank:
	db

; Number of frames between each character
wLetterDelay::
	db
; Number of frames till next character
wNextLetterDelay::
	db

; Bit 6 - Whether the text engine is currently waiting for a button press
; Bit 7 - Whether the text engine is halted, for syncing (can be reset)
wFlags::
	db

; Number of tiles flushed, used to know how many tiles should be written to the tilemap
wNbFlushedTiles::
	db

; Number of newlines that occurred during this print
wNbNewlines::
	db
; ID of the tiles during which newlines occurred (NOTE: there can be duplicates due to empty lines!!)
wNewlineTiles::
	ds TEXT_NEWLINE_CAPACITY

wPrinterStartingPosition::
	dw
wPrinterHeadPtr::
	dw
wPrinterCurTile::
	db

; Low byte of the read ptr into wInputBuffer
wReadPtrLow::
	db
; Where the reader ended, i.e. where the printer needs to stop
wReadPtrEnd::
	db
; Where the reader's read ended; characters between the above and this may be candidate for an
; auto linebreak, so they shouldn't be passed to the printer yet
wFillPtrEnd::
	db

; Number of lines of the current text area
wNbLines::
	db
; How many newlines remain before the text box is full
wRemainingLines::
	db
; How many lines does the textbox contain that the user hasn't yet "acknowledged" (by pressing a button).
wNbFreshLines::
	db
; Length, in pixels, of the current text line
wLineLength::
	db
wLineRemainingPixels::
	db
; Ptr to last newlineable location
wNewlinePtrLow::
	db
; wLineRemainingPixels at the time wNewlinePtrLow is updated
wPixelsRemainingAtNewline::
	db
; Charset ptr is cached by reader, due to how often it accesses glyph widths.
wCurCharsetPtr::
	dw
wReaderPrevFont::
	db
wReaderPrevVariant::
	db

SECTION "VWF engine fast memory", HRAM

; How many rows are left to be drawn in the current tile
hVWFRowCount::
	db
