use std::env::args;
use std::fs::write;
use std::process::exit;
use image::GenericImageView;

const CHARACTER_WIDTH: u32 = 9;
const CHARACTER_HEIGHT: u32 = 8;

fn main() {
	let args: Vec<String> = args().collect();

	if args.len() != 3 {
		eprintln!("Usage:");
		eprintln!("\t{} input_file output_file", args[0]);
		exit(1);
	}

	let img = image::open(&args[1]).unwrap_or_else(|error| {
		eprintln!("Failed to open image {}: {}", args[1], error);
		exit(1);
	});
	let mut output = Vec::<u8>::new();

	if img.width() % CHARACTER_WIDTH != 0 {
		eprintln!("Image width must be a multiple of {CHARACTER_WIDTH}");
		exit(1);
	}
	if img.height() % CHARACTER_HEIGHT != 1 {
		eprintln!("Image width must be a multiple of {}", CHARACTER_HEIGHT + 1);
		exit(1);
	}

	let bg_color = img.get_pixel(0, 0);
	let fg_color = img.get_pixel(1, 0);
	let null_color = img.get_pixel(2, 0);

	for ty in (0..img.height() - 1).step_by(CHARACTER_HEIGHT as usize) {
		for tx in (0..img.width()).step_by(CHARACTER_WIDTH as usize) {
			let mut size = 0;
			for y in (ty + 1)..(ty + 1 + CHARACTER_HEIGHT) {
				let mut byte: u32 = 0;
				size = CHARACTER_WIDTH as u8;
				for x in tx..(tx + CHARACTER_WIDTH) {
					byte <<= 1;
					let pixel = img.get_pixel(x, y);

					if pixel == fg_color {
						byte |= 1;
					} else if pixel == null_color {
						size -= 1;
					} else if pixel != bg_color {
						eprintln!("WARNING: pixel at ({x}, {y}) is none of the 3 font pixels; treating as blank. Note: only monochrome fonts are supported.");
					}
				}
				if byte & (1 << (CHARACTER_WIDTH - 8) - 1) != 0 {
					eprintln!("ERROR: Row at ({tx}, {y}): only the first 8 pixels of a character can be non-blank");
				}
				output.push((byte >> CHARACTER_WIDTH - 8) as u8);
			}
			output.push(size);
		}
	}

	write(&args[2], output).unwrap_or_else(|error| {
		eprintln!("Failed to write to {}: {}", args[2], error);
		exit(1);
	});
}
