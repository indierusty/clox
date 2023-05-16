# clox
Bytecode Interpreter in C.

## Arch
* **Sanning -> Parsing -> Compiling -> Executing**
* SourceCode -> **Scanner** -> Tokens -> **Compiler** -> Bytecode -> **VM**

## Main Modules
* **scanner** => impl scanner **source -> tokens**.
* **compiler**=> impl compiler **tokens -> bytecode**.
* **vm**      => impl virtual machine **bytecode -> execution**.

## Helper Modules
* **chunk**   => impl data structure representating sequence of **bytecode** (chunk).
* **memory**  => macros and functions for **dynamic memory management**.
* **value**   => define structure to store **constant types**.

NOTE: In clox Parsing and Compiling done in one pass in compiler module 
      which is good for simplicity but bad for optimization.

TODO: separate Parsing and compiling for Optimization.
