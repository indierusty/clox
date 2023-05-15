#include "chunk.h"
#include "debug.h"

int main(int argc, char* argv[]) {

    Chunk chunk;
    init_chunk(&chunk);

    int constant = add_constant(&chunk, 1.2);
    write_chunk(&chunk, OP_CONSTANT);
    write_chunk(&chunk, constant);
    write_chunk(&chunk, OP_RETURN);

    disassemble_chunk(&chunk, "test chunk");
    free_chunk(&chunk);

    return 0;
}
