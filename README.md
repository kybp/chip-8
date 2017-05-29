# CHIP-8

This is a CHIP-8 simulator using Lisp and CL-SDL2. It defines a package CHIP-8,
and exports the lone symbol CHIP-8:MAIN, which is a function that accepts a
filespec and runs it in the simulator in a graphical window.

The file must have either a `.txt` or `.ch8` extension; if the extension is
`.txt`, then the file will be interpreted as a text file describing machine
language extensions, so for example, in such a file the ASCII characters `D23F`
would be interpreted as the byte `0xD23F`. This is provided to make writing
machine language programs to run in the simulator slightly easier. If the file
extension is `.ch8`, then the file will be interpreted as a regular binary
CHIP-8 executable.

The escape key can be used to kill the simulator at any point while it is
running. Additionally, the CHIP-8's 16 hex keys can be entered with the keyboard
according to the following mapping:

    1 - 1
    2 - 2
    3 - 3
    4 - C
    Q - 4
    W - 5
    E - 6
    R - D
    A - 7
    S - 8
    D - 9
    F - E
    Z - A
    X - 0
    C - B
    V - F
