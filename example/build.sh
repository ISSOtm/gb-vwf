#!/bin/bash

set -e

cd ../makefont
cargo build --release
cd ../example

for font in fonts/*.png; do
	../makefont/target/release/makefont $font ${font%.png}.vwf
done
rgbasm -Wall -Wextra -o vwf_config.o vwf_config.asm > charmap.asm

rgbgfx -Z -o button.2bpp button.png
rgbgfx -o border_top.2bpp border_top.png
rgbgfx -o border_vert.2bpp border_vert.png
rgbgfx -o border_bottom.2bpp border_bottom.png
rgbasm -Wall -Wextra -Wno-unmapped-char -h -o main.o main.asm
rgblink -d -n vwf.sym -m vwf.map -o vwf.gb vwf_config.o main.o
rgbfix -v -p 0xff -m 0x11 vwf.gb
rm fonts/*.vwf charmap.asm button.2bpp border_top.2bpp border_vert.2bpp border_bottom.2bpp main.o vwf_config.o
