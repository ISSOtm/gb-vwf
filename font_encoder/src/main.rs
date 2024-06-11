use std::{fmt::Display, fs::File, io::Write, path::PathBuf, process::ExitCode};

use plumers::prelude::*;
use slicedisplay::SliceDisplay;

fn main() -> ExitCode {
    let args = xflags::parse_or_exit! {
        /// Font image file to process
        required input_path: PathBuf
        /// Path to write the encoded font to
        required output_path: PathBuf
    };

    let input = match File::open(&args.input_path) {
        Ok(input) => input,
        Err(err) => {
            eprintln!(
                "Error opening input image (\"{}\"): {err}",
                args.input_path.display()
            );
            return ExitCode::FAILURE;
        }
    };
    let img = match PalettedImage16::load(
        input,
        LoadFlags {
            remove_alpha: false,
            // No matter the mode, #00000000 (the most common transparent colour) will get sorted *after* #000000ff;
            // sorting black first ensures that the former ends up in the slot expected of the "filler colour".
            palette_sort: PaletteSort::DarkFirst,
            sort_existing: true,
            reduce_palette: true,
        },
        AlphaMode::ZeroIsOpaque,
    ) {
        Ok(img) => img,
        Err(err) => {
            eprintln!(
                "Error reading input image (\"{}\"): {err}",
                args.input_path.display()
            );
            return ExitCode::FAILURE;
        }
    };

    if img.palette().len() != 3 || img.palette()[0] != Rgb16(0) || img.palette()[2] != Rgb16(0x7fff)
    {
        eprintln!(
            "\
Error: the input image must only contain black, white, and a \"filler\" colour
       Image palette: {}",
            img.palette().display(),
        );
        return ExitCode::FAILURE;
    }
    if img.height() % 8 != 0 {
        eprintln!(
            "Error: the input image's height ({}) must be a multiple of 8",
            img.height()
        );
        return ExitCode::FAILURE;
    }
    if img.nb_frames() != 1 {
        eprintln!(
            "Warning: the input image contains {} frames, but only the first one will be processed",
            img.nb_frames()
        );
    }

    let charset = extract_charset(&img);

    output_charset(&args.output_path, &charset);

    ExitCode::SUCCESS
}

// The three colour indices used in the image.
const ON_PIXEL: u8 = 0; // Black.
const FILLER_PIXEL: u8 = 1; // Any.
const OFF_PIXEL: u8 = 2; // White.

#[derive(Debug, Clone)]
struct Glyph {
    width: u8,
    pixels: [u8; 8],
}

fn extract_charset(img: &PalettedImage16) -> Vec<Glyph> {
    let frame = img.frame(0);

    let mut glyphs = Vec::new();
    for glyph_y in 0..img.height() / 8 {
        let y = glyph_y * 8;

        let mut x = 0;
        while x < img.width() {
            let mut pixels = [0; 8];
            let mut mask = 0x80u8;

            // While we haven't reached the left edge of the image, and we are looking at an "on" or "off" pixel...
            while x < img.width() && frame[(x, y)] != FILLER_PIXEL {
                // Process one column of the glyph.
                for dy in 0..8 {
                    match frame[(x, y + dy)] {
                        ON_PIXEL => pixels[dy] |= mask,
                        OFF_PIXEL => {} // Do nothing.
                        FILLER_PIXEL => {
                            eprintln!(
                                "\
Error: the input image is malformed
       Saw a non-filler pixel at ({x}, {y}), but then a filler pixel at ({x}, {})",
                                y + dy
                            );
                            std::process::exit(1);
                        }
                        _ => unreachable!(),
                    }
                }

                x += 1;
                mask >>= 1;
            }

            glyphs.push(Glyph {
                // The last column of all glyphs is white, so add one extra to the width.
                // That assumption is made throughout the engine.
                width: mask.leading_zeros() as u8 + 1,
                pixels,
            });
            // TODO: check that the glyph has an acceptable width

            // Skip any columns of filler.
            while x < img.width() && frame[(x, y)] == FILLER_PIXEL {
                // Ensure that they are proper *columns* of filler.
                for dy in 1..8 {
                    if frame[(x, y + dy)] != FILLER_PIXEL {
                        eprintln!(
                            "\
Error: the input image is malformed
       Saw a filler pixel at ({x}, {y}), but then a non-filler pixel at ({x}, {})",
                            y + dy
                        );
                        std::process::exit(1);
                    }
                }

                x += 1;
            }
        }
    }

    glyphs
}

fn output_charset(path: &PathBuf, charset: &[Glyph]) {
    let mut file = File::create(path).unwrap_or_else(|err| {
        eprintln!(
            "Error: failed to create output file \"{}\": {err}",
            path.display()
        );
        std::process::exit(1);
    });

    for glyph in charset {
        let mut buf = glyph.pixels;
        buf[0] |= glyph.width;
        file.write_all(&buf).unwrap_or_else(|err| {
            eprintln!(
                "Error: failed to write to output file \"{}\": {err}",
                path.display()
            );
            std::process::exit(1);
        })
    }
}

#[derive(Debug, Clone, Copy)]
struct Nth(usize);
impl Display for Nth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.0,
            match self.0 % 10 {
                1 => "st",
                2 => "nd",
                3 => "rd",
                _ => "th",
            }
        )
    }
}