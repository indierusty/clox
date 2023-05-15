#pragma once

#include "chunk.h"

void disassemble_chunk(Chunk* chunk, const char* name);
int dissassemble_instruction(Chunk* chunk, int offset);
