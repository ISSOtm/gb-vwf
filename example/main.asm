
INCLUDE "hardware.inc/hardware.inc"
INCLUDE "charmap.asm"

	rev_Check_hardware_inc 4.8


charmap "♥", $89


TEXT_WIDTH_TILES equ 16
TEXT_HEIGHT_TILES equ 8
	EXPORT TEXT_WIDTH_TILES
	EXPORT TEXT_HEIGHT_TILES
BTN_ANIM_PERIOD equ 16

macro lb
	ld \1, (\2) << 8 | (\3)
endm


SECTION "Header", ROM0[$100]

	di
	jr EntryPoint

	ds $150 - @, 0

EntryPoint:
	; Clear tilemap
	ld hl, _SCRN0
	ld de, SCRN_VX_B - SCRN_X_B
	ld c, SCRN_Y_B
.waitVBlank
	ldh a, [rLY]
	sub SCRN_Y
	jr nz, .waitVBlank
	; xor a ; ld a, 0
.clearTilemap
	REPT SCRN_X_B
	ld [hli], a
	ENDR
	add hl, de
	dec c
	jr nz, .clearTilemap
	; Init LCD regs
	; xor a ; ld a, 0
	ldh [rSCY], a
	ldh [rSCX], a
	ld a, %11100100
	ldh [rBGP], a
	ldh [rOBP0], a
	ld a, LCDCF_ON | LCDCF_BGON
	ldh [rLCDC], a

	; Init interrupt handler vars
	xor a
	ldh [hVBlankFlag], a
	dec a ; ld a, $FF
	ldh [hHeldButtons], a

	ld hl, OAMDMA
	lb bc, OAMDMA.end - OAMDMA, LOW(hOAMDMA)
.copyOAMDMA
	ld a, [hli]
	ldh [c], a
	inc c
	dec b
	jr nz, .copyOAMDMA

	; Init OAM
	ld hl, wShadowOAM
	ld de, .sprites
	ld c, .spritesEnd - .sprites
	rst MemcpySmall
	; Send unused sprites off-screen
	ld c, NB_UNUSED_SPRITES * sizeof_OAM_ATTRS
	xor a ; ld a, 0
	rst MemsetSmall


	ld a, IEF_VBLANK
	ldh [rIE], a
	xor a
	ei
	ldh [rIF], a


	assert .spritesEnd == .tiles
	; ld de, .tiles
	ld hl, vButtonTiles
	ld bc, (.tilesEnd - .tiles) / 2
	call LCDMemcpy

	ld hl, vTextboxTopRow
	lb bc, LOW(vBorderTiles.top / 16), NB_BORDER_TOP_TILES
	assert NB_BORDER_TOP_TILES == TEXT_WIDTH_TILES + 1
.writeTopRow
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .writeTopRow
	ld a, b
	ld [hli], a
	inc b
	dec c
	jr nz, .writeTopRow

	ld hl, vText - 1
	ld c, TEXT_HEIGHT_TILES
	assert NB_BORDER_VERT_TILES == TEXT_HEIGHT_TILES * 2
.writeVertBorders
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .writeVertBorders
	ld a, b
	ld [hli], a
	inc b
	ld a, l
	add a, TEXT_WIDTH_TILES
	ld l, a
	ld a, b
	ld [hli], a
	inc b
	ld a, l
	add a, SCRN_VX_B - TEXT_WIDTH_TILES - 2
	ld l, a
	adc a, h
	sub l
	ld h, a
	dec c
	jr nz, .writeVertBorders

	inc hl
	ld c, TEXT_WIDTH_TILES + 1
	assert NB_BORDER_BOTTOM_TILES == TEXT_WIDTH_TILES + 1
.writeBottomRow
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .writeBottomRow
	ld a, b
	ld [hli], a
	inc b
	dec c
	jr nz, .writeBottomRow

	; Assuming OAM has correctly been written, start displaying sprites
	ld a, LCDCF_ON | LCDCF_OBJON | LCDCF_OBJ16 | LCDCF_BGON
	ldh [rLCDC], a


	;;;;;;;;;;;;;;;; TEXT ENGINE GLOBAL INIT ;;;;;;;;;;;;;;;;;;;;

	; You need to write these on game startup
	xor a ; ld a, 0
	ld [wTextCurPixel], a
	; xor a ; ld a, 0
	ld [wTextCharset], a
	; xor a ; ld a, 0
	ld c, $10 * 2
	ld hl, wTextTileBuffer
	rst MemsetSmall


	ld a, BANK(PerformAnimation)
	ldh [hCurROMBank], a
	ld [rROMB0], a
	jp PerformAnimation


.sprites
	db 0, 142 + 8, LOW(vButtonTiles / 16) + 0, 0
	db 0, 142 + 8, LOW(vButtonTiles / 16) + 2, 0
	db 0, 150 + 8, LOW(vButtonTiles / 16) + 4, 0
	db 0, 150 + 8, LOW(vButtonTiles / 16) + 6, 0
.spritesEnd

.tiles
.buttonTiles
INCBIN "button.2bpp"
NB_BUTTON_TILES equ (@ - .buttonTiles) / 16
NB_BUTTON_SPRITES equ NB_BUTTON_TILES / 2

.borderTopTiles
INCBIN "border_top.2bpp"
NB_BORDER_TOP_TILES equ (@ - .borderTopTiles) / 16
.borderVertTiles
INCBIN "border_vert.2bpp"
NB_BORDER_VERT_TILES equ (@ - .borderVertTiles) / 16
.borderBottomTiles
INCBIN "border_bottom.2bpp"
NB_BORDER_BOTTOM_TILES equ (@ - .borderBottomTiles) / 16

.tilesEnd


OAMDMA:
	ldh [rDMA], a
	ld a, OAM_COUNT
.wait
	dec a
	jr nz, .wait
	ret
.end


; This is intentionally placed in bank 2 to demonstrate the VWF engine working fine from ROMX
assert BANK(_PrintVWFChar) != 2
SECTION "Animation", ROMX,BANK[2]

PerformAnimation:
	ld a, BTN_ANIM_PERIOD
	ld [wBtnAnimCounter], a


	;;;;;;;;;;;;;;;; TEXT ENGINE LOCAL INIT ;;;;;;;;;;;;;;;;;;;;;
	; You should write these when appropriate
	ld a, 18 * 8 + 1 ; The last pixel of all chars is blank!
	ld [wTextLineLength], a
	ld a, LOW(vStaticTextTiles / 16)
	ld [wTextCurTile], a
	; The following text doesn't wrap, so no need to init this
	; ld [wWrapTileID], a
	ld a, LOW(vStaticTextTiles.end / 16) - 1
	ld [wLastTextTile], a
	ld a, 2
	ld [wTextNbLines], a
	ld [wTextRemainingLines], a
	ld [wNewlinesUntilFull], a
	xor a ; ld a, 0
	ld [wTextStackSize], a
	; xor a ; ld a, 0
	ld [wTextFlags], a
	; xor a ; ld a, 0
	ld [wTextLetterDelay], a
	ld a, $80
	ld [wTextTileBlock], a

	ld a, TEXT_NEW_STR
	ld hl, StaticText
	ld b, BANK(StaticText)
	call PrintVWFText
	ld hl, vStaticText
	call SetPenPosition
	; Since wTextLetterDelay is 0, this will process all of the string at once
	call PrintVWFChar
	call DrawVWFChars


	;;;;;;;;;;;;;;; This is "local" initialization for printing the "main" text ;;;;;;;;;;;;;;;;;
	ld a, TEXT_WIDTH_TILES * 8 + 1
	ld [wTextLineLength], a
	ld a, LOW(vTextTiles / 16)
	ld [wTextCurTile], a
	ld [wWrapTileID], a
	ld a, LOW(vTextTiles.end / 16) - 1
	ld [wLastTextTile], a
	ld a, 8
	ld [wTextNbLines], a
	ld [wTextRemainingLines], a
	ld [wNewlinesUntilFull], a
	ld a, 2
	ld [wTextLetterDelay], a
	ld a, $90
	ld [wTextTileBlock], a

.restartText
	ld a, TEXT_NEW_STR
	ld hl, Text
	ld b, BANK(Text)
	call PrintVWFText
	ld hl, vText
	call SetPenPosition

.loop
	rst WaitVBlank

	call PrintVWFChar
	call DrawVWFChars

	ld hl, wTextFlags
	bit 7, [hl]
	jr z, .noSpeedToggling
	res 7, [hl]
	; Toggle the text speed
	ld a, [wTextLetterDelay]
	xor 8
	ld [wTextLetterDelay], a
.noSpeedToggling

	; Draw a button animation if waiting for button input
	bit 6, [hl]
	ld a, 110 + 16
	jr nz, .drawButton
	ld a, SCRN_Y + 16
.drawButton
	ld [wButtonSprites], a
	ld [wButtonSprites + 8], a
	add a, 16
	ld [wButtonSprites + 4], a
	ld [wButtonSprites + 12], a
	ld hl, wBtnAnimCounter
	dec [hl]
	jr nz, .noBtnAnim
	ld [hl], BTN_ANIM_PERIOD
	ld hl, wButtonSprites + OAMA_TILEID
	ld c, NB_BUTTON_SPRITES
.toggleBtnFrame
	ld a, [hl]
	xor LOW(vButtonTiles.frame0 / 16) ^ LOW(vButtonTiles.frame1 / 16)
	ld [hli], a
	inc l ; inc hl
	inc l ; inc hl
	inc l ; inc hl
	dec c
	jr nz, .toggleBtnFrame
.noBtnAnim


	ld a, [wTextSrcPtr + 1]
	inc a ; cp $FF
	jr nz, .loop

.waitRestart
	rst WaitVBlank
	ldh a, [hPressedButtons]
	and PADF_START
	jr z, .waitRestart
	jr .restartText


SECTION "Called text", ROMX[$5000],BANK[2]

CalledText:
	db "Toto, I don't think we're in Bank ", BANK(Text) + "0", " anymore.<END>"

; For demonstration purposes, both of these pieces of text are in different banks,
; and both in a bank other than the VWF engine
SECTION "Text", ROMX,BANK[3]

Text:
	db "<CLEAR>Hello World!\n"
	db "Text should break here... automatically!<WAITBTN>\n"
	db "\n"
	db "Text resumes printing when pressing A, but holding B works too.<WAITBTN>\n"

	db "<CLEAR>Let's tour through most of the functionality, shall we?<WAITBTN>\n"
	; Need to split this line because of RGBDS' stupid limitations...
	db "The engine is also aware of textbox height, and will replace newlines (including those inserted automatically) with commands to scroll the textbox. ","It also keeps track of how many lines have been written since the last user input, and automagically inserts pauses at the right time!<WAITBTN>"

	db "<CLEAR>Note that automatic hyphenation is not supported, but line breaking is hyphen-aware.<WAITBTN>\n"
	db "Breaking of long words can be hinted at using \"soft hyphens\". Isn't it totally ama<SOFT_HYPHEN>zing?<WAITBTN>"

	db "<CLEAR>It is, <DELAY>",5,"of course, <DELAY>",10,"possible to insert ma<DELAY>",20,"nu<DELAY>",20,"al<DELAY>",20," delays, manual line\nbreaks, and, as you probably already noticed, manual button waits.<WAITBTN>\n"

	db "<CLEAR>The engine also supports synchronisation! It's how <SYNC>these words<SYNC><DELAY>",1," are made to print slowly, and back again. <DELAY>",20,"It could be useful e.g. for sound cues.<WAITBTN>"

	; `SET_COLOR 0` also works, but it's pretty pointless, so I'm not showcasing it
	db "<CLEAR>It's also possible to <SET_COLOR>",1,"change <SET_COLOR>",2,"the color <SET_COLOR>",3,"of text!<WAITBTN>\n"
	db "You can also switch to <SET_VARIANT>",2,"variations of the font<RESTORE_VARIAN>, <SET_FONT>",$10,"a different font, or <SET_VARIANT>",2,"a variation of a different font<RESTORE_VARIAN><RESTORE_FONT>, why not!<WAITBTN>\n"
	db "Each font can have up to ", NB_FONT_CHARACTERS / 100 + "0", (NB_FONT_CHARACTERS / 10) % 10 + "0", NB_FONT_CHARACTERS % 10 + "0", " characters. The encoding is left up to you--make good use of RGBASM's `charmap` feature!<WAITBTN>"

	db "<CLEAR>The engine also supports a `call`-like mechanism. The following string is pulled from ROM2:{X:CalledText}: \""
	; Control chars are also made available as `TEXT_*` symbols if `EXPORT_CONTROL_CHARS` is passed to the engine
	db TEXT_CALL, BANK(CalledText), LOW(CalledText), HIGH(CalledText)
	db "\".<WAITBTN>\n"
	db "A \"jump\" is also supported.<WAITBTN>"
	db TEXT_JUMP, BANK(CreditsText), LOW(CreditsText), HIGH(CreditsText)

SECTION "Credits text", ROMX,BANK[1]

CreditsText:
	db "<CLEAR>♥ Credits ♥\n"
	db "VWF engine by ISSOtm; graphics by BlitterObject; fonts by PinoBatch & Optix, with edits by ISSOtm.<WAITBTN>\n"
	db "\n"
	db "Text will now end, press START to begin again.<END>"

SECTION "Static text", ROMX,BANK[2]

StaticText:
	db "VWF engine 1.0.0\n"
	db "github.com/ISSOtm/gb-vwf<END>"


SECTION "LCDMemcpy", ROM0

LCDMemcpy::
	; Increment B if C is nonzero
	dec bc
	inc b
	inc c
.loop
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .loop
	ld a, [de]
	ld [hli], a
	inc de
	ld a, [de]
	ld [hli], a
	inc de
	dec c
	jr nz, .loop
	dec b
	jr nz, .loop
	ret

LCDMemsetSmall::
	ld b, a
LCDMemsetSmallFromB::
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, LCDMemsetSmallFromB
	ld a, b
	ld [hli], a
	dec c
	jr nz, LCDMemsetSmallFromB
	ret


SECTION "Vectors", ROM0[0]

	ret
	ds $08 - @

WaitVBlank::
	ld a, 1
	ldh [hVBlankFlag], a
.wait
	halt
	jr .wait
	ds $10 - @

MemsetSmall:
	ld [hli], a
	dec c
	jr nz, MemsetSmall
	ret
	ds $18 - @

MemcpySmall:
	ld a, [de]
	ld [hli], a
	inc de
	dec c
	jr nz, MemcpySmall
	ret
	ds $20 - @

	ret
	ds $28 - @

	ret
	ds $30 - @

	ret
	ds $38 - @

	ret
	ds $40 - @

	; VBlank handler
	push af
	ld a, HIGH(wShadowOAM)
	call hOAMDMA

	ldh a, [hVBlankFlag]
	and a
	jr z, .noVBlank

	ld c, LOW(rP1)
	ld a, P1F_GET_DPAD
	ldh [c], a
	REPT 4
	ldh a, [c]
	ENDR
	or $F0
	ld b, a
	swap b
	ld a, P1F_GET_BTN
	ldh [c], a
	REPT 4
	ldh a, [c]
	ENDR
	or $F0
	xor b
	ld b, a
	ld a, P1F_GET_NONE
	ldh [c], a
	ldh a, [hHeldButtons]
	cpl
	and b
	ldh [hPressedButtons], a
	ld a, b
	ldh [hHeldButtons], a

	pop af ; Pop return address to exit `waitVBlank`
	xor a
	ldh [hVBlankFlag], a
.noVBlank
	pop af
	reti


SECTION UNION "8800 tiles", VRAM[$8800]

vButtonTiles:
.frame0
	ds 16 * NB_BUTTON_SPRITES
.frame1
	ds 16 * NB_BUTTON_SPRITES

vBorderTiles:
.top
	ds 16 * NB_BORDER_TOP_TILES
.vert
	ds 16 * NB_BORDER_VERT_TILES
.bottom
	ds 16 * NB_BORDER_BOTTOM_TILES

vStaticTextTiles:
	ds 16 * 32
.end


SECTION UNION "9000 tiles", VRAM[$9000]

vBlankTile:
	ds 16
vTextTiles:: ; Random position for demonstration purposes
	ds 16 * 127
.end

SECTION UNION "9800 tilemap", VRAM[_SCRN0]

	ds SCRN_VX_B * 3

	ds 1
vTextboxTopRow:
	ds TEXT_WIDTH_TILES + 2
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 3

	ds 2
vText::
.row0
	ds TEXT_WIDTH_TILES
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 2

	ds 2
.row1
	ds TEXT_WIDTH_TILES
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 2

	ds 2
.row2
	ds TEXT_WIDTH_TILES
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 2

	ds 2
.row3
	ds TEXT_WIDTH_TILES
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 2

	ds 2
.row4
	ds TEXT_WIDTH_TILES
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 2

	ds 2
.row5
	ds TEXT_WIDTH_TILES
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 2

	ds 2
.row6
	ds TEXT_WIDTH_TILES
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 2

	ds 2
.row7
	ds TEXT_WIDTH_TILES
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 2

	ds 1
vTextboxBottomRow:
	ds TEXT_WIDTH_TILES + 2
	ds SCRN_VX_B - TEXT_WIDTH_TILES - 3

	ds SCRN_VX_B

	ds 3
vStaticText:
	ds SCRN_VX_B - 3


SECTION "Shadow OAM", WRAM0,ALIGN[8]

wShadowOAM:
wButtonSprites:
	ds sizeof_OAM_ATTRS * NB_BUTTON_TILES / 4

NB_UNUSED_SPRITES equ OAM_COUNT - (@ - wShadowOAM) / sizeof_OAM_ATTRS
	ds NB_UNUSED_SPRITES * sizeof_OAM_ATTRS

SECTION "WRAM0", WRAM0

wBtnAnimCounter:
	db


SECTION "HRAM", HRAM

hCurROMBank::
	db

hVBlankFlag:
	db
hHeldButtons::
	db
hPressedButtons::
	db
hOAMDMA:
	ds OAMDMA.end - OAMDMA
