# GMSD
A (non-YYC) GameMaker Studio decompiler in F#.

## Installation and Usage
Retrieve the `data.win` file from your game's `.exe`. Compile this project with Visual Studio 2013 or later, and run it as `GMSD.exe [-f] data.win`. If you pass the `-f` flag, each script will be written to a text file in a new directory called `./src`; if you don't, you'll see it all in your console.

## Example output
To show off what the program currently can and can't do: disassembly and decompilation work fine, but are limited. Expression folding is very powerful, but branch instructions lack structure, and some names get lost.

Original nonsense code:

    x += 1
    if (x == 10)
        show_debug_message("Hi, world!")
    instance_create(0, 0, foo)

Decompiled:

    =================== gml_Object_foo_Create_0 ===================
    00000a48 Self.x = Self.x + 1s
    00000a60 push Self.x == 10s
    00000a70 IfFalse goto 00000a8c
    00000a74 show_debug_message[]("Hi, world!")
    00000a8c instance_create[](0s, 0s, 0s)

## Miscellaneous stuff
This is currently still being worked on, slowly. Right now, the output is readable if you squint, but it's still spaghetti code, and looks nothing like GML... Expect more cool results soon.

I based this program on these two documents: [(1)](http://rawr.ws/undertale/unpacking) [(2)](http://rawr.ws/undertale/decompilation) Countless thanks to Mirrawrs for helping me out with some details.

This project is MIT-licensed. Read `LICENSE.md` for more info.
