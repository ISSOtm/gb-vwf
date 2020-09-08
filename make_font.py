#!/usr/bin/env python3

from PIL import Image
from sys import argv,stderr


assert len(argv) > 2, "Usage:\n\t{} input_file output_file".format(argv[0])

CHARACTER_WIDTH  = 9
CHARACTER_HEIGHT = 8

with Image.open(argv[1]) as img:
    bg_color   = img.getpixel((0, 0))
    fg_color   = img.getpixel((1, 0))
    null_color = img.getpixel((2, 0))

    width, height = img.size
    assert   width % CHARACTER_WIDTH == 0, f"The source image's width must be a multiple of {CHARACTER_WIDTH}!"
    assert height % CHARACTER_HEIGHT == 1, f"The source image's height must be a multiple of {CHARACTER_HEIGHT}, plus 1!"

    data = []

    for ty in range(0, height - 1, CHARACTER_HEIGHT):
        for tx in range(0, width, CHARACTER_WIDTH):
            size = 0
            for y in range(ty + 1, ty + 1 + CHARACTER_HEIGHT):
                byte = 0
                size = CHARACTER_WIDTH
                for x in range(tx, tx + CHARACTER_WIDTH):
                    byte <<= 1
                    pixel = img.getpixel((x, y))
                    if pixel == fg_color:
                        byte |= 1
                    elif pixel == null_color:
                        size -= 1
                    elif pixel != bg_color:
                        print(f"WARNING: pixel at (x:{x}, y:{y}) is none of the 3 font pixels; treating as blank. Note: only monochrome fonts are supported!", file=stderr)
                if byte & (1 << (CHARACTER_WIDTH - 8) - 1):
                    raise ValueError(f"Row at (x:{tx}, y:{y}): only the first 8 pixels of a character can be non-blank!")
                data.append(byte >> (CHARACTER_WIDTH - 8))
            data.append(size)

with open(argv[2], "wb") as output:
    output.write(bytes(data))
