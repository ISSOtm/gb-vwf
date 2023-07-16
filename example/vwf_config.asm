INCLUDE "hardware.inc/hardware.inc"


macro lb
	assert (\2) < 256
	assert (\3) < 256
	ld \1, (\2) << 8 | (\3)
endm

macro get_rom_bank_id
	ldh a, [hCurROMBank]
endm

macro switch_rom_bank
	ldh [hCurROMBank], a ; For interrupt safety, the HRAM variable *must* be updated BEFORE the actual switch.
	ld [rROMB0], a
endm


;; The following defines are a sort of "configuration" passed to the VWF engine
;; The engine generally cares about whether they're present, not their value


; The engine expects to be able to read held buttons from `hHeldKeys`, and buttons just pressed from `hPressedKeys`
; `main.asm` (intentionally) defines `hHeldButtons` and `hPressedButtons` instead
; If this problem arises, here's how to work around it:
def hPressedKeys equs "hPressedButtons"
def hHeldKeys equs "hHeldButtons"
; The engine similarly expects to be able to read the current ROM bank from `hCurROMBank`.
; Do like above if necessary.


; Charset IDs increase 2 by 2
def CHARSET_0  equs "fonts/BaseSeven.vwf"
def CHARSET_2  equs "fonts/BaseSevenBold_vx8.vwf"
def CHARSET_16 equs "fonts/optix.vwf"
def CHARSET_18 equs "fonts/optixBold.vwf"
def NB_CHARSETS equ 10

def SKIP_HELD_KEYS equ PADF_B
def SKIP_PRESSED_KEYS equ PADF_A

def EXPORT_CONTROL_CHARS equ 1

INCLUDE "../vwf.asm"


; It's possible to export symbols from `vwf.asm`, and use them elsewhere, like so
; Do so at your own risk, this isn't officially supported functionality
	EXPORT _PrintVWFChar, NB_FONT_CHARACTERS
