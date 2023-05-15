#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, char* argv[]) {
    init_vm();

    Chunk chunk;
    init_chunk(&chunk);

    // -((10.2 + 30.4) / 20.0)
    int constant = add_constant(&chunk, 10.2);
    write_chunk(&chunk, OP_CONSTANT, 1);
    write_chunk(&chunk, constant, 1);

    // 30.4
    constant = add_constant(&chunk, 30.4);
    write_chunk(&chunk, OP_CONSTANT, 2);
    write_chunk(&chunk, constant, 2);

    // +
    write_chunk(&chunk, OP_ADD, 2);

    // 20.0
    constant = add_constant(&chunk, 20);
    write_chunk(&chunk, OP_CONSTANT, 3);
    write_chunk(&chunk, constant, 3);

    // / and negate
    write_chunk(&chunk, OP_DIVIDE, 3);
    write_chunk(&chunk, OP_NEGATE, 3);

    write_chunk(&chunk, OP_RETURN, 4);

    disassemble_chunk(&chunk, "test chunk");

    interpret(&chunk);

    free_vm();
    free_chunk(&chunk);

    return 0;
}
