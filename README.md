# CHIP-8

This is a CHIP-8 interpreter in Common Lisp using CL-SDL2. The only
exported function, CHIP-8:MAIN takes a filename as an argument. The
file can either be a .txt, in which case it will read each line as a
four character string representing an opcode, or a .ch8, in which case
it will read it as a regular binary file.

It doesn't work in SLIME; you need to launch it from a Lisp running
separately in a shell.
