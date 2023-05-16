# clox
Bytecode Interpreter in C.

## Arch
Scanner => Compiler => Virtual Machine
SourceCode -> **Scanner** -> Tokens -> **Compiler** -> BytecodeChunk -> **VM**

## Main Modules
**scanner** => impl scanner **source -> tokens**.
**compiler**=> impl compiler **tokens -> bytecode**.
**vm**      => impl virtual machine **bytecode -> execution**.

## Helper Modules
**chunk**   => impl data structure representating sequence of **bytecode** (chunk).
**memory**  => macros and functions for **dynamic memory management**.
**value**   => define structure to store **constant types**.
