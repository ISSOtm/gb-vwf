INCLUDE "hardware.inc/hardware.inc"


; The example code uses `lb`. Only it, though, so it's defined here.
lb: macro
	ld \1, (\2) << 8 | (\3)
endm


;; The following defines are a sort of "configuration" passed to the VWF engine
;; The engine generally cares about whether they're present, not their value


; The engine expects to be able to read held buttons from `hHeldKeys`, and buttons just pressed from `hPressedKeys`
; `main.asm` (intentionally) defines `hHeldButtons` and `hPressedButtons` instead
; If this problem arises, here's how to work around it:
hPressedKeys equs "hPressedButtons"
hHeldKeys equs "hHeldButtons"
; The engine similarly expects to be able to read the current ROM bank from `hCurROMBank`.
; Do like above if necessary.


; Charset IDs increase 2 by 2
CHARSET_0  equs "fonts/BaseSeven.vwf"
CHARSET_2  equs "fonts/BaseSevenBold_vx8.vwf"
CHARSET_16 equs "fonts/optix.vwf"
CHARSET_18 equs "fonts/optixBold.vwf"
NB_CHARSETS equ 10

SKIP_HELD_KEYS equ PADF_B
SKIP_PRESSED_KEYS equ PADF_A

EXPORT_CONTROL_CHARS equ 1

PRINT_CHARMAP equ 1
INCLUDE "../vwf.asm"


; It's possible to export symbols from `vwf.asm`, and use them elsewhere, like so
; Do so at your own risk, this isn't officially supported functionality
	EXPORT _PrintVWFChar
	EXPORT NB_FONT_CHARACTERS
