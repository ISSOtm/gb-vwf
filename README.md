# gb-vwf

A Variable-Width Font library for the Game Boy & Game Boy Color.

## Demo

https://user-images.githubusercontent.com/15271137/172129074-6d0266de-e0c2-49bb-9519-7779dcb5ac2a.mp4

You can also run [the demo](https://github.com/ISSOtm/gb-vwf/releases/download/v1.0.0/vwf.gb) yourself in your favorite Game Boy emulator.

Do you want to build the demo yourself, or to check out an example of how gb-vwf could be integrated into your project?
Then please browse [the demo's source code](https://github.com/ISSOtm/gb-vwf-demo)!

## Projects using gb-vwf

- [Esprit](https://github.com/eievui5/esprit)
- [Shock Lobster](https://github.com/tbsp/shock-lobster)

Are you using gb-vwf, and would like to be featured in this list?
Open a pull request, or get in touch!

## Known bugs

- If the “textbox” touches the right edge of the tilemap, newlines may occasionally skip one row of tiles.

## How to use

For detailed instructions, including installation and usage, please check [the wiki](https://github.com/ISSOtm/gb-vwf/wiki) out.

Bugs can be reported on [the issue tracker](https://github.com/ISSOtm/gb-vwf/issues) (please check that it hasn't been reported already!), or by [bugging me on GBDev](https://gbdev.io/chat).

## Licensing

The VWF engine itself is licensed under the MPL 2.0.

> **In a nutshell**: if and only if you modify gb-vwf, you must provide your modified `vwf.asm` to anyone who got a compiled version of it and asks for it.

According to US law at least, bitmap fonts cannot be copyrighted.
You can find some [here](https://github.com/pinobatch/bitmap-fonts/tree/master/vwf).
