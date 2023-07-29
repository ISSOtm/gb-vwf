
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

PUSHO
; Control characters, as well as some normal characters, are special to the engine. Can't leave them undefined!
OPT Werror=unmapped-char



;;; Include the user-provided configuration file.

IF !DEF(VWF_CFG_FILE)
	FAIL "Before including `vwf.asm`, please create a config file, and define `VWF_CFG_FILE` to contain a path to it."
ENDC

; Macros used by the config file.

def charmap_idx = 0
MACRO chars
	REPT _NARG
		IF STRLEN(\1) != 0
			vwf_charmap \1, charmap_idx
		ENDC
		def charmap_idx += 1
		shift
	ENDR
ENDM

def font_id = 0
def font_ptrs equs ""
def font_data equs ""
MACRO font
	def \1 equ font_id
	EXPORT \1
	def font_id += 1
	redef font_ptrs equs "{font_ptrs}\ndw Font\1Ptr"
	redef font_data equs "{font_data}\nFont\1Ptr:INCBIN \2"
	; TODO: when RGBASM adds multi-byte charmap support, provide short-hand charmaps that emit the control code and the index
ENDM

; Macros required by the above.

MACRO vwf_charmap
	IF _NARG > 1
		def TMP equ (\2)
	ELSE
		def TMP equ \1
	ENDC
	charmap \1, TMP

	IF DEF(PRINT_DEBUGFILE)
	ELIF DEF(PRINT_CHARMAP)
		IF !DEF(BEGUN_CHARMAP)
			def BEGUN_CHARMAP equ 1
			PRINTLN "newcharmap vwf"
		ENDC
		PRINTLN "charmap \1,{TMP}"
	ELIF DEF(PRINT_TBL)
		IF !DEF(BEGUN_CHARMAP)
			def BEGUN_CHARMAP equ 1
			PRINTLN "@VWF"
		ENDC
		def name equs ""
		IF _NARG > 1
			redef name equs "\2"
		ENDC
		IF !STRCMP("{name}", "VWF_END") || !STRCMP("{name}", "VWF_JUMP")
			PRINTLN "/{X:TMP}=[{name}]"
		ELIF !STRCMP("{name}", "VWF_NEWLINE")
			PRINTLN "{X:TMP}=\\n"
		ELIF !STRCMP(STRSUB("{name}", 1, 4), "VWF_")
			shift 2
			PRINTLN "${X:TMP}=[{name}],\#"
		ELSE
			PRINTLN "{X:TMP}=", \1
		ENDC
		PURGE name
	ENDC

	purge TMP
ENDM

def NB_VWF_CTRL_CHARS equ 0
EXPORT NB_VWF_CTRL_CHARS
def ctrl_char_ptrs equs ""
def ctrl_char_lens equs ""
; A bang before the handler pointer means that the control char terminates lines.
; Args should specify .tbl file control code parameters; it is expected that 1 param = 1 operand byte.
; (See https://transcorp.romhacking.net/scratchpad/Table%20File%20Format.txt, section 2.5.1 for syntax.)
; Note that the contents of the <arg>s is only for the .tbl file, but their number is crucial to the lookahead!
MACRO control_char ; <name>, [!]<handler ptr> [, <arg>... ]
	IF _NARG < 2
		FAIL "`control_char` expects at least 2 arguments, not {d:_NARG}"
	ENDC

	redef NB_VWF_CTRL_CHARS equ NB_VWF_CTRL_CHARS + 1
	def char_name equs "\2"
	def nb_operand_bytes equ (_NARG - 2) << 1
	IF !STRCMP(STRSUB("{char_name}", 1, 1), "!")
		redef char_name equs STRSUB("{char_name}", 2)
		redef nb_operand_bytes equ nb_operand_bytes | 1
	ENDC

	redef ctrl_char_ptrs equs "dw {char_name}\n{ctrl_char_ptrs}"
	IF NB_VWF_CTRL_CHARS > NB_SPECIAL_CTRL_CHARS
		redef ctrl_char_lens equs "db {nb_operand_bytes}\n{ctrl_char_lens}"
	ENDC

	def charmap_def equs "\"<\1>\", VWF_\1"
	def VWF_\1 equ 256 - NB_VWF_CTRL_CHARS
	EXPORT VWF_\1
	shift 2
	vwf_charmap {charmap_def}, \#
	PURGE char_name, nb_operand_bytes, charmap_def
ENDM

; Define the built-in control chars first, as their numeric values are important to the engine.

	def NB_SPECIAL_CTRL_CHARS equ 6 ; All of the special chars must be contiguous!
	control_char END,           TextReturn
	control_char CALL,          TextCall
	control_char JUMP,          TextJumpTo
	control_char SET_FONT,      SetFont
	control_char SET_VARIANT,   SetVariant
	control_char ZWS,           SoftHyphen ; "Zero-Width Space"
	control_char NEWLINE,       !Newline
	control_char SET_COLOR,     SetColor,        color=%D
	control_char DELAY,         DelayNextChar,   nb_frames=%D
	control_char SYNC,          ExternalSync
	control_char WAIT,          Wait
	control_char SCROLL,        Scroll
	control_char WAIT_SCROLL,   WaitAndScroll

; Process the config file.

INCLUDE "{VWF_CFG_FILE}"

IF DEF(PRINT_TBL)
	PURGE PRINT_TBL ; Don't print this one in the .tbl file, as it's a duplicate.
ENDC
; People will likely want to write "\n" rather than "<NEWLINE>". Or want to use multi-line strings.
; This is done after processing the config file, to ensure we override whatever the user set.
	vwf_charmap "\n", VWF_NEWLINE



;;; Process all config elements that aren't part of the above macros.


; Number of elements the text stack has room for.
; This must not exceeed $7F, as the return logic discards bit 7 when checking for zero.
IF !DEF(STACK_CAPACITY)
	def STACK_CAPACITY equ 8
ENDC

; Do **NOT** print more than this amount of newlines in a single call to `PrintVWFChars`!
; This would overflow an internal buffer.
; If you want to be safe, set this to the maximum textbox height you will be using.
IF !DEF(NEWLINE_CAPACITY)
	def NEWLINE_CAPACITY equ 10
ENDC

; This adjusts how many bits (starting from the LSB) of the font ID are treated as the "variant".
; 2 bits is enough for Regular, Bold, Italic, Bold+Italic; this should cover most use cases.
IF !DEF(NB_VARIANT_BITS)
	def NB_VARIANT_BITS equ 2
ENDC



;;; Now, some things that the rest of the engine uses.

IF !DEF(lb)
	FAIL "Please define the `lb` macro in \"{VWF_CFG_FILE}\"."
ENDC

IF !DEF(get_cur_rom_bank_id)
	FAIL "Please define the `get_bank_id` macro in \"{VWF_CFG_FILE}\"."
ENDC

IF !DEF(switch_rom_bank)
	FAIL "Please define the `switch_rom_bank` macro in \"{VWF_CFG_FILE}\"."
ENDC

IF !DEF(VWF_EMPTY_TILE_ID)
	FAIL "Please define the `VWF_EMPTY_TILE_ID` symbol in \"{VWF_CFG_FILE}\"."
ENDC

IF !DEF(wait_vram)
	; This one is provided by default, because it should be good enough for most everyone.
	; And the requirements for a replacement are fairly complex, so I don't want to overwhelm less savvy users.
	MACRO wait_vram
	.waitVRAM\@
		ldh a, [rSTAT]
		and STATF_BUSY
		jr nz, .waitVRAM\@
	ENDM
ENDC

; Definitions for the various bits in `wFlags`.
; FIXME: it would be neater to define these next to `wFlags`, but RGBDS 0.6.1 requires the first
; operand to `bit`, `set`, and `res` to be constant...
MACRO flag
	def TEXTB_\2 equ (\1)
	def TEXTF_\2 equ 1 << TEXTB_\2
	EXPORT TEXTB_\2, TEXTF_\2
ENDM
	flag 7, WAITING ; If set, the engine will not process ticks.
	flag 6, SYNC ; Set by the "<SYNC>" control char, otherwise ignored.
	flag 2, SCROLL ; Internal. If this is set, the textbox will be scrolled upwards (unless WAITING is set).
	flag 1, NEWLINE ; Internal. If set, the next character flush will move to the next line.
	flag 0, COLOR ; If set, color #1 will be emitted instead of #3. Changed by "<COLOR>".


;; Debugfile-related macros.

IF DEF(PRINT_DEBUGFILE)
	PRINTLN "@debugfile 1.0.0"
	MACRO dbg_var ; <name>, <default value>
		def DEFAULT_VALUE equs "0"
		IF _NARG > 1
			redef DEFAULT_VALUE equs "\2"
		ENDC
		PRINTLN "@var \1 {DEFAULT_VALUE}"
		purge DEFAULT_VALUE
	ENDM
	MACRO dbg_action ; <function>, <action:str> [, <condition:dbg_expr>]
		def OFS_FROM_BASE equ @ - \1
		def ACTION_COND equs ""
		IF _NARG > 2
			redef ACTION_COND equs "\3"
		ENDC
		PRINTLN "\1+{d:OFS_FROM_BASE} x {ACTION_COND}: ", \2
		purge OFS_FROM_BASE, ACTION_COND
	ENDM
	MACRO runtime_assert ; <function>, <condition:dbg_expr> [, <message:dbg_str>]
		def MSG equs "assert failure"
		IF _NARG > 2
			redef MSG equs \3
		ENDC
		dbg_action \1, "alert \"{MSG}\"", !(\2)
		purge MSG
	ENDM
	MACRO unreachable ; <function> [, <message:dbg_str>]
		def MSG equs "unreachable code reached!"
		IF _NARG > 1
			redef MSG equs \2
		ENDC
		dbg_action \1, "alert \"In \1: {MSG}\""
		purge MSG
	ENDM
ELSE
	def dbg_var equs ";"
	def dbg_action equs ";"
	def runtime_assert equs ";"
	def unreachable equs ";"
ENDC



;;; Now, the engine itself.
; First lie the “setup” function, then the two “stepping” functions.
; Note that the latter are surrounded by a lot of auxiliary functions due to `jr` range concerns.



	; Holds the value of SP when entering `TickVWFEngine`, used to check the stack is balanced on exit.
	dbg_var _vwfEntrySp


; @param hl: Pointer to the string to be displayed
; @param b:  Bank containing the string
; @param a:  Whether to flush the current string (either of the constants below).
;            Use `VWF_CONT_STR` if you want to keep printing to the same string you previously were.
;            Note that `VWF_NEW_STR` causes the auto-linewrapper to assume a new line is being started.
	def VWF_CONT_STR equ 0
	def VWF_NEW_STR  equ 1
	EXPORT VWF_CONT_STR, VWF_NEW_STR
; @return a: 1
SetupVWFEngine::
	and a ; Setting Z for the jump further below.

	ld a, b
	ld [wSourceBank], a
	ld a, h
	ld [wSourceStack.entries], a
	ld a, l
	ld [wSourceStack.entries + 1], a

	jr z, .continuingString
	ld hl, wNbPixelsDrawn
	ld a, [hl]
	cp 2
	jr c, .curTileIsBlank
	; We must increment the tile ID, since we're about to begin a new tile.
	ld a, [wCurTileID.max]
	ld c, a
	ld a, [wCurTileID]
	inc a
	cp c
	jr nz, .noIDWrap
	ld a, [wCurTileID.min]
.noIDWrap
	ld [wCurTileID], a
.curTileIsBlank
	; Clear the tile buffer to start afresh.
	assert wTileBuffer.end == wNbPixelsDrawn
	ld c, wTileBuffer.end - wTileBuffer + 1
	xor a
.resetTileBuffer
	ld [hld], a
	dec c
	jr nz, .resetTileBuffer
	; Reset some more variables.
	; a = 0
	ld [wFlags], a ; Reset all flags, too.
	inc a ; ld a, 1
	ld [wNbTicksToNextPrint], a ; Print on the next tick.
	; Reset the lookahead's cache.
	ld a, [wTextbox.width]
	ld [wLookahead.nbTilesRemaining], a
	; Compute how far down into the textbox we are.
	ld hl, wTextbox.origin
	ld a, [wPrinterHeadPtr]
	sub [hl]
	ld c, a
	inc hl
	ld a, [wPrinterHeadPtr + 1]
	sbc a, [hl]
	xor c
	and $1F
	xor c
	rlca
	rlca
	rlca
	ld b, a
	ld a, [wTextbox.height]
	sub b
	ld [wNbLinesRemaining], a
.continuingString

	ld a, 1
	ld [wSourceStack.len], a
	ret


; Control character handlers jump back into `TickVWFEngine`, so for performance's sake (`jr`),
; some are positioned before it, and others after.
; All unexported functions (labels without `::`) are meant to be internal only.


TextReturn:
	pop hl ; We're not returning to the normal code path.
	ld hl, wSourceStack.len
	dec [hl]
	jr nz, TickVWFEngine.reReadCurStackEntry ; Reuse the normal "read stack entry" path.
	; Yes, we bypass all of the "cleanup"; we aren't planning to do anything else anyway.
	runtime_assert TextReturn, sp == @_vwfEntrySp, "SP (\{sp,4$\}) != entry SP (\{_vwfEntrySp,4$\})!"
	ret ; Conditional returns never perform better than avoiding them conditionally.

HandleControlChar:
	runtime_assert HandleControlChar, (a / 2 | $80) >= 256 - {NB_VWF_CTRL_CHARS}, "Invalid control character \{a / 2 | $80,2$\}"
	; Keep processing characters after this one.
	ld hl, TickVWFEngine.readInputChar
	push hl
	; The label points to one past the table, and the indices are negative (kind of); subtracting
	; 256 allows using the standard "unsigned" addition technique.
	add a, LOW(ControlChars.handlers - 256)
	ld l, a
	adc a, HIGH(ControlChars.handlers - 256)
	sub l
	ld h, a
	ld a, [hli]
	ld h, [hl]
	ld l, a
	jp hl

SoftHyphen:
	pop hl ; We're not returning to the normal code path.
	call ShouldBreakLine
	jr z, TickVWFEngine.readInputChar ; If the line needs not be broken, then act as a no-op.
	; Act as if we just read a hyphen; the line should break right after.
	ld a, "-" * 2
	jr TickVWFEngine.pretendCharRead


TickVWFEngine:: ; Note that a lot of local labels in this loop are jumped to from other functions.
	ld hl, wFlags
	; Do nothing if either the WAITING or SCROLL flags are set; WAITING is obvious, but for SCROLL:
	ld a, [hli]
	; if WAIT_SCROLL's scrolling is still pending, refuse processing any chars to avoid queuing
	; another scroll (which would overwrite the pending one).
	and TEXTF_WAITING | TEXTF_SCROLL
	ret nz

	runtime_assert TickVWFEngine, [wSourceStack.len] != 0, "VWF engine called with empty stack!!!"
	dbg_action TickVWFEngine, "set @_vwfEntrySp := @sp"
	; TODO: runtime_assert that control chars declare the same length that they actually use

	assert wFlags + 1 == wNbTicksToNextPrint
	dec [hl]
	ret nz
	inc hl
	assert wNbTicksToNextPrint + 1 == wNbTicksBetweenPrints
	; Reload the timer.
	ld a, [hld]
	sub 1
	adc a, 1 ; Schedule for 1 tick instead of 256 if insta-printing!
	ld [hl], a
	; Alright gang, time to print characters and kick ass!

	ld a, [wSourceBank]
	switch_rom_bank

	; Get the pointer to the active stack entry.
	ld hl, wSourceStack.len
.reReadCurStackEntry
	ld a, [hld] ; This ensures that an offset of 1 (× 2) points to the first entry.
	assert wSourceStack.len + 1 == wSourceStack.entries
	add a, a
	add a, l
	ld l, a
	adc a, h ; TODO: alignment may make a carry impossible; check this everywhere the stack is accessed, too
	sub l
	ld h, a
	; Read the active stack entry. (Big-endian!)
	ld a, [hli]
	ld e, [hl]
	ld d, a

.readInputChar
	; This assertion is not checked at the function's entry point, so as to catch bugs caused by
	; the engine looping into itself incorrectly.
	; It is checked even for control chars, so as to report bugs even if they don't end up having
	; any effect in a particular instance.
	runtime_assert TickVWFEngine, [wNbPixelsDrawn] < 8, "VWF engine cannot draw correctly with un-flushed tile! (Either you forgot to call PrintVWFChars, or you found a bug internal to gb-vwf :D)"

	runtime_assert TickVWFEngine, &de == [wSourceBank], "Text should be read from \{[wSourceBank],$\}:\{de:4$\}, \{&de,$\}:\{de:4$\} is loaded instead"
	ld a, [de]
	inc de ; By default, a character should be consumed. This will seldom be cancelled.
	add a, a
	jr c, HandleControlChar
	; We have a printable character: print it!

.pretendCharRead
	ldh [hCurChar], a ; Save this for after the draw phase.
	push de ; We cave in to register pressure for the draw phase. It's okay.

	; Read the font's base pointer.
	ld hl, wCurFont.ptr
	ld c, [hl]
	inc hl
	ld b, [hl]
	; Index into the table of glyphs, which are 8 bytes each.
	ld l, a ; Char ID * 2
	ld h, 0
	add hl, hl ; Char ID * 4
	add hl, hl ; Char ID * 8
	add hl, bc ; Char ID * 8 + font ptr, i.e. `font->glyphs[char_id]` in C terms.
	ld b, h ; We will only be reading from this into `a`, so it's fine.
	ld c, l

	ld a, BANK("VWF fonts and barrel shift table")
	switch_rom_bank

	; Read by how much the glyph's pixels will need to be shifted right.
	ld a, [wNbPixelsDrawn]
	ldh [hNbPixelsDrawn], a
	ld e, a
	; Increase that by the glyph's width.
	ld a, [hl] ; The width is stored in the 3 lower bits of the first byte.
	and %111
	add a, e
	ld [wNbPixelsDrawn], a

	ld d, HIGH(ShiftLUT) ; Prepare to index this table a whole lot.

	; Draw the high bitplane if and only if its bit is set in wFlags.
	; This is done in a separate loop to reduce the overhead incurred by checking the bit.
	ld a, [wFlags]
	bit TEXTB_COLOR, a
	jr nz, .noHighBitplane
	push bc ; Save the glyph data pointer for drawing the low bitplane.
	ld hl, wTileBuffer + 2
.drawHighBitplane
	ld a, [bc]
	inc bc
	xor e ; Set the upper 5 bits to the row of pixels; the lower 3 are already the shift amount.
	and $F8
	xor e
	assert LOW(ShiftLUT) == 0
	ld e, a ; That Frankenstein mix is designed to work as an index into ShiftLUT.
	ld a, [de] ; ...so these are the shifted pixels for the first tile.
	or [hl]
	ld [hli], a
	inc d ; Switch to the second table.
	ld a, [de] ; Read the shifted pixels for the second tile.
	or [hl]
	ld [hli], a
	dec d ; Go back to the first table.
	assert HIGH(wTileBuffer) == HIGH(wTileBuffer.end - 1)
	inc l ; Skip the other bitplane.
	inc l ;   (2 bytes.)
	assert LOW(wTileBuffer) & (1 << 5) == 0
	assert LOW(wTileBuffer.end) & (1 << 5) != 0
	bit 5, l ; TODO: there may be potential to use one of the `inc l`s instead of this, using greater alignment
	jr z, .drawHighBitplane
	pop bc ; Restore the glyph data pointer.
.noHighBitplane

	; The low bitplane is drawn unconditionally. (Colour can be toggled between #1 and #3.)
	ld hl, wTileBuffer
.drawLowBitplane
	ld a, [bc]
	inc bc
	xor e ; Set the upper 5 bits to the row of pixels; the lower 3 are already the shift amount.
	and $F8
	xor e
	assert LOW(ShiftLUT) == 0
	ld e, a ; That Frankenstein mix is designed to work as an index into ShiftLUT.
	ld a, [de] ; ...so these are the shifted pixels for the first tile.
	or [hl]
	ld [hli], a
	inc d ; Switch to the second table.
	ld a, [de] ; Read the shifted pixels for the second tile.
	or [hl]
	ld [hli], a
	dec d ; Go back to the first table.
	assert HIGH(wTileBuffer) == HIGH(wTileBuffer.end - 1)
	inc l ; Skip the other bitplane.
	inc l ;   (2 bytes.)
	assert LOW(wTileBuffer) & (1 << 5) == 0
	assert LOW(wTileBuffer.end) & (1 << 5) != 0
	bit 5, l ; TODO: there may be potential to use one of the `inc l`s instead of this, using greater alignment
	jr z, .drawLowBitplane

	pop de ; Restore the read pointer.
	; Restore the source ROM bank also. This must be done now for the potential jump to `Newline`.
	ld a, [wSourceBank]
	switch_rom_bank

	ldh a, [hCurChar]
	IF " " == 0
		and a
	ELSE
		cp " " * 2
	ENDC
	jr z, AfterBreakableChar
	IF "-" == 0
		and a
	ELSE
		cp "-" * 2
	ENDC
	jr z, AfterBreakableChar

.processedChar
	ld a, [wNbPixelsDrawn]
	cp 8
	jr nc, :+ ; Do not draw more characters if flushing is required.
	ld a, [wNbTicksBetweenPrints] ; If insta-printing, batch characters.
	and a
	jp z, .readInputChar ; Too far to `jr` T_T
:

.doneProcessingChars
	; Get the pointer to the active stack entry.
	ld hl, wSourceStack.len
	ld a, [hld] ; This ensures that an offset of 1 (× 2) points to the first entry.
	assert wSourceStack.len + 1 == wSourceStack.entries
	add a, a
	add a, l
	ld l, a
	adc a, h
	sub l
	ld h, a
	; Write back the updated source pointer.
	ld a, d
	ld [hli], a
	ld [hl], e
	runtime_assert TextReturn, sp == @_vwfEntrySp, "SP (\{sp,4$\}) != entry SP (\{_vwfEntrySp,4$\})!"
	ret

; Some special control chars.

AfterBreakableChar:
	call ShouldBreakLine
	jr z, TickVWFEngine.processedChar
	ldh a, [hCurChar]
	IF " " == 0
		and a
	ELSE
		cp " " * 2
	ENDC
	jr nz, :+
	; Pretend that character was never drawn.
	; TODO: I don't like that hack :( but it's necessary to take into account the space's own width
	; for determining the length... ideally we'd do this right after advancing the width, but how?
	ldh a, [hNbPixelsDrawn]
	ld [wNbPixelsDrawn], a
:
	db $FE ; cp <imm8>, skipping the following `pop hl`.
Newline:
	pop hl ; We're not returning to the normal code path.
	; If there are no more lines remaining, scroll up to make room.
	ld hl, wNbLinesRemaining
	dec [hl]
	jr nz, .noNeedToScroll
	inc [hl] ; Increment it back!
.forceScroll
	ld hl, wFlags
	set TEXTB_SCROLL, [hl]
.noNeedToScroll

	; Force flushing of the current tile (unless it's blank).
	ld hl, wNbPixelsDrawn
	ld a, [hl]
	cp 2
	jr c, .curTileIsBlank
	ld a, 8
.curTileIsBlank
	ld [hli], a
	assert wNbPixelsDrawn + 1 == wFlags
	set TEXTB_NEWLINE, [hl]

	; Avoid scrolling text off-screen that the user hasn't had a chance to "acknowledge" yet.
	ld hl, wNbLinesRead
	dec [hl]
	jr nz, TickVWFEngine.doneProcessingChars
	ld hl, wFlags
	set TEXTB_WAITING, [hl]
	jr TickVWFEngine.doneProcessingChars ; Flushing was just forced, so no more chars can be processed!

WaitAndScroll:
	ld hl, wFlags
	set TEXTB_WAITING, [hl]
	; fallthrough
Scroll:
	ld hl, wFlags
	set TEXTB_SCROLL, [hl]
	jr Newline.forceScroll ; Also skips decrementing `wNbLinesRemaining`.

Wait:
	pop hl ; We're not returning to the normal code path.
	ld hl, wFlags
	set TEXTB_WAITING, [hl]
	jr TickVWFEngine.doneProcessingChars

DelayNextChar:
	pop hl ; We're not returning to the normal code path.
	ld a, [de]
	inc de
	ld [wNbTicksToNextPrint], a
	jr TickVWFEngine.doneProcessingChars ; A delay of 0 shall be interpreted as 256 frames.

; "Regular" control chars, that just return normally to `TickVWFEngine.readInputChar`.

TextCall:
	; Reserve a new stack entry.
	runtime_assert TextCall, [wSourceStack.len] <= {STACK_CAPACITY}, "VWF stack overflow! (Please reduce your text call depth, or increase STACK_CAPACITY)."

	ld hl, wSourceStack.len
	inc [hl]
	ld a, [hl] ; Not decrementing means that we'll point at the entry's second byte.
	add a, a ; Each entry is 2 bytes.
	add a, l
	ld l, a
	adc a, h
	sub l
	ld h, a
	; Write the new entry. (Stack entries are big-endian, but we're writing it backwards!)
	; Note that we do this before writing back the current entry, because the "return pointer"
	; still needs to advance to read the operand.
	; TODO: wait, why write it to the stack now? Isn't setting `de` enough?
	ld a, [de]
	inc de
	ld [hld], a
	ld c, a ; Cache this to avoid re-reading the entry we're writing.
	ld a, [de]
	inc de
	ld [hld], a
	; Write back the pointer to "return" to. Note that `a` is preserved throughout.
	ld [hl], e
	dec hl
	ld [hl], d
	; Resume reading from the new pointer. Note that we reuse a `ld d, a` from the "main" path.
	ld e, c
	ld d, a
	ret

TextJumpTo:
	; Simply read the operand, and continue reading from there.
	ld a, [de]
	inc de
	ld l, a
	ld a, [de]
	ld e, l
	ld d, a
	ret

SetVariant:
	ld hl, wCurFont.id
	ld a, [de] ; New variant.
	xor [hl]
	and (1 << NB_VARIANT_BITS) - 1 ; Keep only the variant bits from the operand.
	xor [hl]
	jr SetFont.updatePtr
SetFont:
	ld hl, wCurFont.id
	ld a, [de]
.updatePtr
	inc de
	ld [hli], a
	; Refresh the cached font pointer.
	assert wCurFont.id + 1 == wCurFont.ptr
	add a, a ; Font table entries are 2 bytes each.
	add a, LOW(FontPtrTable)
	ld c, a
	adc a, HIGH(FontPtrTable)
	sub c
	ld b, a
	ld a, [bc]
	ld [hli], a
	inc bc
	ld a, [bc]
	ld [hli], a
	ret

SetColor:
	ld hl, wFlags
	ld a, [de]
	inc de
	; Keep the "color" flag from `a`, and all other bits from `wFlags`.
	xor [hl]
	and TEXTF_COLOR
	xor [hl]
	ld [hl], a
	ret

ExternalSync:
	ld hl, wFlags
	set TEXTB_SYNC, [hl]
	ret


; The "lookahead" used by the auto-linewrapper.

; Looks ahead in the character stream to check whether a line break should be inserted here.
; @return zf: Clear if the line should be broken right now.
; @destroy a
ShouldBreakLine:
	runtime_assert ShouldBreakLine, [wTextbox.width] != 0, "Textbox must have a non-zero width!"
	runtime_assert ShouldBreakLine, [wTextbox.width] <= 32, "Textbox cannot be wider than a tilemap!"
	push de ; Save the source pointer.

	; Copy all shadow variables.
	; (Unrolling is, at 4 bytes, faster and as small as the standard loop.)
	ld bc, wCurFont.id
	ld hl, wLookahead.fontID
	assert wCurFont.id + 1 == wCurFont.ptr
	assert wLookahead.fontID + 1 == wLookahead.fontPtr
	assert wCurFont.ptr + 2 == wSourceStack.len
	assert wLookahead.fontPtr + 2 == wLookahead.stackLen
	REPT 4 - 1
		ld a, [bc]
		inc bc
		ld [hli], a
	ENDR
	ld a, [bc]
	ld [hl], a

	ld a, [wNbPixelsDrawn]
	ld l, a
	ld a, [wLookahead.nbTilesRemaining]
	add a, a ; × 2
	add a, a ; × 4
	add a, a ; × 8 (pixels/tile)
	inc a ; Since the last column of all glyphs is transparent, allow one extra pixel to compensate.
	sub l ; The active tile is currently being drawn to, subtract those pixels.
	; It's possible that the textbox has been overflowed, but it only happens with spaces.
	; TODO: it'd be nice to runtime_assert that...
	jr c, .return ; Breaking is required, and Z cannot be set right now.
	ld [wLookahead.nbPixelsRemaining], a

.readInputChar
	ld a, [de]
	inc de
	add a, a
	jr c, .controlChar
	; A space is breakable, so we wouldn't need to break before it.
	IF " " == 0
		and a
	ELSE
		cp " " << 1
	ENDC
	jr z, .return
	ld c, a ; Remember the glyph ID for later.
	; Compute the pointer to the glyph's width.
	ld l, a
	ld h, 0
	add hl, hl ; × 4
	add hl, hl ; × 8
	ld a, [wLookahead.fontPtr]
	add a, l
	ld l, a
	ld a, [wLookahead.fontPtr + 1]
	adc a, h
	ld h, a
	; Read the glyph's width, which is in a different bank (and restore the bank right after).
	ld a, BANK("VWF fonts and barrel shift table")
	switch_rom_bank
	ld a, [hl] ; The length is in the bottom 3 bits of the first byte.
	and %111
	ld l, a
	ld a, [wSourceBank]
	switch_rom_bank
	; Subtract the glyph's length from the remaining pixels.
	ld a, [wLookahead.nbPixelsRemaining]
	sub l
	jr c, .return ; We would overflow! Z cannot be set right now.
	ld [wLookahead.nbPixelsRemaining], a
	; Check for printable characters that can be broken after.
	ld a, c
	IF "-" == 0
		and a
	ELSE
		cp "-" << 1
	ENDC
	jr nz, .readInputChar

.returnShouldntBreak
	xor a
.return
	pop de ; Restore the source pointer.
	ret


.controlChar
	runtime_assert ShouldBreakLine, cf, "Internal bug: carry isn't set!?"
	rra ; Restore the original value, since it's actually easier to process that way.
	assert VWF_END == $FF
	inc a
	jr z, .end
	assert VWF_CALL == $FE
	inc a
	jr z, .call
	assert VWF_JUMP == $FD
	inc a
	jr z, .jump
	assert VWF_SET_FONT == $FC
	inc a
	jr z, .setFont
	assert VWF_SET_VARIANT == $FB
	inc a
	jr z, .setVariant
	assert VWF_ZWS == $FA
	inc a
	jr z, .zws ; A ZWS is breakable, so we can just return Z!
	; Other control chars will skip however many operand bytes the table specifies.
	assert BANK(ControlChars.lengths) == 0
	add a, LOW(ControlChars.lengths - 256)
	ld l, a
	adc a, HIGH(ControlChars.lengths - 256)
	sub l
	ld h, a
	ld a, [hl] ; The length.
	rrca ; The LSB indicates that the control char ends a line.
	jr c, .returnShouldntBreak
	add a, e
	ld e, a
	adc a, d
	sub e
	ld d, a
	jr .readInputChar

.end
	ld hl, wLookahead.stackLen
	ld a, [hl]
	add a, a ; Check if we went through a "one-way call"; if so, we can't return.
	jr c, .returnShouldntBreak ; We want to return Z, but we don't know Z's value right now.
	dec [hl] ; Decrement the stack length.
	jr z, .return ; We wouldn't overflow before the text stream ends, and Z is set.
	; Compute the address of the stack entry we should read from.
	add a, LOW(wSourceStack.entries - 2) ; `a` contains the original length, doubled.
	ld l, a
	adc a, HIGH(wSourceStack.entries - 2)
	sub l
	ld h, a
	; Read it.
	ld a, [hli]
	ld e, [hl]
	ld d, a
	jr .readInputChar

.jump
	runtime_assert ShouldBreakLine, zf, "Internal bug: Z isn't set!?"
	ld a, [de]
	inc de
	ld c, a
	db $C4 ; call nz, <imm16>, skipping the following `set 7, [hl]`.
.oneWayCall
	set 7, [hl] ; Set the "one-way" flag.
	; Jump to the callee.
	ld a, [de]
	ld d, a
	ld e, c
	jp .readInputChar ; TODO: too far to `jr` :(

.zws
	; A ZWS is breakable, but prints a hyphen when broken at.
	; If there aren't enough pixels to do that, we will have to break now!
	; (Note: this would be suboptimal if the character *after* was breakable, but it seems
	; preferable to assume that whoever writes the text is reasonable, for performance's sake.)
	; Get the current font's hyphen width.
	ld a, [wLookahead.fontPtr]
	add a, LOW("-" * 8) ; Font offset computed at compile time :)
	ld l, a
	ld a, [wLookahead.fontPtr + 1]
	adc a, HIGH("-" * 8)
	ld h, a
	ld a, [hl]
	and %111
	ld l, a
	; Check if enough pixels remain.
	ld a, [wLookahead.nbPixelsRemaining]
	sub l
	jr c, .return ; If there aren't enough pixels, return with NZ (zero doesn't generate a carry).
	; Otherwise, return that we don't need to break now, as there will be enough room to break at the ZWS
	; (should the check that will be performed when handling it fail).
	jr .returnShouldntBreak

.setFont
	ld hl, wLookahead.fontID
	ld a, [de]
.updateFontPtr
	inc de
	ld [hli], a
	; Refresh the cached font pointer.
	assert wLookahead.fontID + 1 == wLookahead.fontPtr
	add a, a ; Font table entries are 2 bytes each.
	add a, LOW(FontPtrTable)
	ld c, a
	adc a, HIGH(FontPtrTable)
	sub c
	ld b, a
	ld a, [bc]
	ld [hli], a
	inc bc
	ld a, [bc]
	ld [hli], a
	jp .readInputChar ; TODO: too far to `jr` :(

.setVariant
	ld hl, wLookahead.fontID
	ld a, [de] ; New variant.
	xor [hl]
	and (1 << NB_VARIANT_BITS) - 1 ; Keep only the variant bits from the operand.
	xor [hl]
	jr .updateFontPtr

.call
	runtime_assert ShouldBreakLine, [wLookahead.stackLen] < STACK_CAPACITY, "VWF stack overflow during lookahead! (Please reduce your text call depth, or increase STACK_CAPACITY.)"
	ld hl, wLookahead.stackLen
	inc [hl] ; Increment the stack depth.
	; Read the first byte of the new pointer, since we have to do that on all code paths.
	ld a, [de]
	inc de
	ld c, a
	; If the simulated stack is shallower than the real one, we can't store the return addr.
	ld a, [wSourceStack.len]
	cp [hl]
	jr nc, .oneWayCall ; Can't return from this one.
	; Compute the pointer to the new entry.
	add a, a
	add a, LOW(wSourceStack.entries - 2)
	ld l, a
	adc a, HIGH(wSourceStack.entries - 2)
	sub l
	ld h, a
	; Read the second half of the target pointer.
	ld a, [de]
	inc de
	; Save the current read pointer. Stack entries are big-endian!
	ld [hl], d
	inc hl
	ld [hl], e
	; Switch to the new source pointer.
	ld d, a
	ld e, c
	jp .readInputChar ; TODO: too far to `jr` :(


PrintVWFChars::
	ld a, [wNbPixelsDrawn]
	sub 8
	jr c, .noNeedToFlush
	ld [wNbPixelsDrawn], a

	; Decrement the count for the lookahead.
	ld hl, wLookahead.nbTilesRemaining
	dec [hl]

	; TODO: storing the font upside-down may be more efficient than writing backwards?

	; Compute the pointer to the tile in VRAM.
	ld a, [wCurTileID]
	call .computeTilePtr
	; We need a pointer to the end of the tile, since we run through it backwards.
	; TODO: what about storing fonts backwards, instead? Might complicate writing the "active" tile...
	ld a, e
	add a, 16
	ld e, a
	adc a, d
	sub e
	ld d, a

	ld hl, wTileBuffer.end - 1
.shiftTileData
	dec de
	ld b, [hl] ; Grab the right tile's pixels,
	wait_vram
	runtime_assert PrintVWFChars, a == 0, "`wait_vram` macro returned a == \{a,2$\}, not 0!"
	ld [hld], a ; ...and reset them.
	ld a, [hl]
	ld [de], a
	ld a, b
	ld [hld], a
	assert LOW(wTileBuffer.end - 1) & (1 << 5) == 0
	assert LOW(wTileBuffer - 1) & (1 << 5) != 0
	bit 5, l
	jr z, .shiftTileData

	; Write the new tile's ID to the tilemap.
	; It would be more optimised to write it earlier, since we read `wCurTileID` already;
	; but it could lead to showing a partially-written tile, so let's not.
	ld hl, wPrinterHeadPtr
	ld a, [hli]
	ld h, [hl]
	ld l, a
	; TODO: check that the target is in-bounds
	ld a, [wCurTileID.max]
	ld b, a
	wait_vram
	ld a, [wCurTileID]
	ld [hli], a
	; We interrupt your regular schedule to increment the tile ID, since we have it in `a`.
	inc a
	cp b
	jr nz, .noIDWrap
	ld a, [wCurTileID.min]
.noIDWrap
	ld [wCurTileID], a
	; Now, back to updating the "printer head".
	ld a, l
	ld [wPrinterHeadPtr], a
	ld a, h
	ld [wPrinterHeadPtr + 1], a
.noNeedToFlush

	ld a, [wFlags]
	bit TEXTB_NEWLINE, a
	jr z, .noNewline
	res TEXTB_NEWLINE, a
	ld [wFlags], a
	; A new line refreshes the width (obviously).
	ld a, [wTextbox.width]
	ld [wLookahead.nbTilesRemaining], a
	; Move back to the beginning of the line...
	ld hl, wPrinterHeadPtr
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wTextbox.origin]
	xor l
	and SCRN_VX_B - 1
	xor l
	; ...then down by one line.
	add a, SCRN_VX_B
	ld l, a
	ld [wPrinterHeadPtr], a
	adc a, h
	sub l
	ld [wPrinterHeadPtr + 1], a
.noNewline

	; This must be checked for *outside* of the newline block, because WAIT_SCROLL causes the newline
	; to be processed on the same tick, but the scroll to be delayed until the WAITING flag is cleared.
	ld hl, wFlags
	bit TEXTB_SCROLL, [hl]
	jr z, .notScrolling
	bit TEXTB_WAITING, [hl] ; To support WAIT_SCROLL.
	jr nz, .notScrolling
	res TEXTB_SCROLL, [hl]

	ld hl, wTextbox.origin
	ld a, [hli]
	ld d, [hl]
	ld e, a
.scrollNextRow
	; The source row is one below the target.
	ld hl, SCRN_VX_B
	add hl, de
	; Check if we are about to shift into the active row.
	; Must do the check before shifting for performance and correctness' sake.
	ld a, [wPrinterHeadPtr + 1]
	cp h
	jr nz, :+
	ld a, [wPrinterHeadPtr]
	cp l
	jr z, .doneScrolling
:
	push hl ; Save the address of the beginning of the next row.
	; Copy the row.
	ld a, [wTextbox.width]
	ld c, a
.scrollRow
	wait_vram
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .scrollRow
	pop de ; Retrieve the address of the beginning of the next row.
	jr .scrollNextRow

.doneScrolling
	; Move the "print head" back by one row.
	ld hl, wPrinterHeadPtr
	ld a, e
	ld [hli], a
	ld [hl], d
	; Clear the new row.
	ld a, [wTextbox.width]
	ld c, a
	; Writing to [hl] is far more efficient.
	ld l, e
	ld h, d
:
	wait_vram
	; We are very careful with the comparison, to support e.g. `-DVWF_EMPTY_TILE_ID=[wEmptyTileID]`
	IF !STRCMP("{VWF_EMPTY_TILE_ID}", "$0") || !STRCMP("{VWF_EMPTY_TILE_ID}", "0") ; best-effort optimisation.
		; a = 0 right now, from `wait_vram`.
	ELSE
		ld a, VWF_EMPTY_TILE_ID
	ENDC
	ld [hli], a
	dec c
	jr nz, :-

.notScrolling

	; Flush the "active" tile to VRAM (and write its ID to the tilemap) if necessary.
	ld a, [wNbPixelsDrawn] ; If that tile is blank, don't bother.
	cp 2
	ret c
	runtime_assert PrintVWFChars, [wPrinterHeadPtr!] & $1F < ([wTextbox.origin!] & $1F) + [wTextbox.width], "Went past textbox right side!"
	; If "insta-printing", don't bother showing the incomplete tile, it will be filled next time.
	; This saves CPU for each call to this function.
	ld a, [wNbTicksBetweenPrints]
	and a
	jr nz, :+
	ld a, [wSourceStack.len] ; Well, unless there isn't going to be a "next time".
	and a
	ret nz
:

	ld a, [wCurTileID]
	call .computeTilePtr
	ld hl, wTileBuffer - 1
	ld c, 16 / 2
.copyPartialTile
	inc hl
	wait_vram
	ld a, [hli]
	ld [de], a
	inc de
	inc hl
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .copyPartialTile
	; Write the tile ID to the tilemap.
	ld hl, wPrinterHeadPtr
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wCurTileID]
	ld [hl], a
	ret

.computeTilePtr
	swap a
	ld d, a
	and $F0
	ld e, a
	xor d ; [wCurTileID] & $0F
	and $08 ; The "sign" bit.
	add a, a ; Shift it in position...
	xor HIGH($9000) ; ...for this.
	xor d
	and $F0 ; Keep the upper 4 bits that were just computed, and the lower 4 bits of `d`.
	xor d
	ld d, a
	ret


FontPtrTable:
	font_ptrs

ControlChars:
	ctrl_char_ptrs
.handlers ; Note that this must point to the *end* of the table!

	ctrl_char_lens
.lengths ; Same.


PUSHS

SECTION FRAGMENT "VWF fonts and barrel shift table", ROMX

; 512 bytes to (barrel-)shift 5 bits across two bytes. Seems like a fair tradeoff?
ShiftLUT: align 8
	FOR i, 0, 256
		def pixels = i & $F8
		def shift_amt = i & $07
		db pixels >> shift_amt
	ENDR
	FOR i, 0, 256
		def pixels = i & $F8
		def shift_amt = i & $07
		db LOW((pixels << 8) >> shift_amt)
	ENDR


	font_data


SECTION "VWF engine memory", WRAM0

;; "Pen" memory.

; The two tiles are interleaved, i.e.:
;  - Row #0,  low bitplane,  left tile
;  - Row #0,  low bitplane, right tile
;  - Row #0, high bitplane,  left tile
; ...
; TODO: grouping by bitplanes first would make the rendering code faster, at the cost of making the tile copy slower... but by how much?
wTileBuffer: align 6 ; `align 5` ensures fast iteration (8-bit `inc`), and `align 6` ensures a fast loop check (`bit 5`).
	ds 16 * 2
.end:

; X coordinate within the "leftmost" tile.
; If 8 or more, that tile must be "flushed" to VRAM.
wNbPixelsDrawn::
	db

; Some miscellaneous flags, see the `flag` macro's definition for a list.
; Please leave the flags marked "internal" unchanged! And, for forwards compat, the unused bits as well.
wFlags::
	db

; How many ticks before the next character is printed.
; Note that 0 here behaves as "256 ticks", not 0.
wNbTicksToNextPrint:
	db
; How many ticks between printing two characters.
; If this is zero, then `TickVWFEngine` will batch as many characters as can fit in a single tile
; per call, but will return after that (you must still call `PrintVWFChars` as usual).
wNbTicksBetweenPrints::
	db

wCurFont:
	.id
		db
	.ptr ; The font pointer is cached to avoid re-computing it more often than it's changed.
		dw

wSourceStack:
	.len::
		db
	.entries ; Pointers only, no bank IDs. Note that they are stored big-endian.
		ds 2 * STACK_CAPACITY

wSourceBank:
	db

;; "Printer" memory.

; ID of the next tile in VRAM that will be written to.
wCurTileID::
	db
.min:: ; Minimum value wCurTileID can take.
	db
.max:: ; Maximum value wCurTileID can take. (Exclusive.)
	db

; Pointer to the tilemap location where the next tile ID will be written.
; NOTE: if modifying this, you should probably also update `wNbLinesRemaining`!
wPrinterHeadPtr::
	dw

; Textbox rectangle specification.
wTextbox:
	.origin:: dw
	.width::  db ; In tiles.
	.height:: db ; In tiles.


; Variables used by the "lookahead". Most are shadows of real variables.
wLookahead:
	.fontID    db ; Shadow of `wCurFont.id`.
	.fontPtr   dw ; Shadow of `wCurFont.
	.stackLen  db ; Shadow of `wSourceStack.len`.
	.nbTilesRemaining  db ; However many tiles can still be flushed in this line.
	.nbPixelsRemaining db ; Working memory for the lookahead.

; How many lines of the textbox haven't been fully written yet.
; Serves as a cache, instead of constantly re-computing from `wPrinterHeadPtr`.
wNbLinesRemaining:: ; Not strictly a lookahead variable, but it's thematically coherent.
	db
; How many lines the user has already acknowledged by "waiting".
; The engine decrements this every time a new line is printed, and sets the "waiting" flag when it
; reaches 0; however, **it does NOT reload it afterwards**!
; It is intended that whatever clears the "waiting" flag *also* reloads this.
wNbLinesRead::
	db


SECTION "VWF engine fast memory", HRAM

hCurChar: ; Temporary storage during the drawing phase.
	db

hNbPixelsDrawn: ; Temporary storage to allow reverting a char's printing.
	db

POPS
POPO
