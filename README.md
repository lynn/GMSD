# GMSD
A (non-YYC) GameMaker Studio decompiler in F#.

Retrieve the `data.win` file from your game's `.exe`. Compile this project with Visual Studio 2013 or later, and run it as `GMSD.exe [-f] data.win`. If you pass the `-f` file, output will go to the `src` directory; if you don't, you'll see it in your console. 

This is currently still being worked on (slowly). Right now, the output is readable if you squint, but it's still spaghetti code, and looks nothing like GML... Expect more cool results soon.

I based this program on these two documents: [(1)](http://rawr.ws/undertale/unpacking) [(2)](http://rawr.ws/undertale/decompilation) Countless thanks to Mirrawrs for helping me out with some details.
