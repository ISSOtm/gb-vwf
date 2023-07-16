#!/bin/bash

set -e

for font in fonts/*.png; do
	python3 ../make_font.py $font ${font%.png}.vwf
done
rgbasm -Wall -Wextra -o vwf_config.o vwf_config.asm -DPRINT_CHARMAP > charmap.asm
rgbasm vwf_config.asm -DPRINT_DEBUGFILE > vwf.dbg

rgbgfx -h -o button.2bpp button.png
rgbgfx -o border_top.2bpp border_top.png
rgbgfx -o border_vert.2bpp border_vert.png
rgbgfx -o border_bottom.2bpp border_bottom.png
rgbasm -Wall -Wextra -o main.o main.asm
rgblink -d -n vwf.sym -m vwf.map -o vwf.gb vwf_config.o main.o
rgbfix -v -p 0xff -m 0x11 vwf.gb
rm fonts/*.vwf charmap.asm button.2bpp border_top.2bpp border_vert.2bpp border_bottom.2bpp main.o vwf_config.o
