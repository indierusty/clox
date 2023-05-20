#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool had_error;
    bool panic_mode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool can_assign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
} Local;

/// NOTE: Zero is the global scope, one is the first top-level block, so on
typedef struct {
    /// flat array of all locals that are in scope 
    /// during each point in the compilation process.
    Local locals[UINT8_COUNT];
    /// number of locals
    int local_count;
    /// number of blocks surrounding the current bit of code we’re compiling
    int scope_depth;
} Compiler;

Parser parser;
Compiler* current = NULL;
Chunk* compiling_chunk;

static Chunk* current_chunk() {
    return compiling_chunk;
}

/// [helper functions] Parsing ///
static void error_at(Token* token, const char* message) {
    if (parser.panic_mode) return;
    parser.panic_mode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // 
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.had_error = true;
}

static void error(const char* message) {
    error_at(&parser.previous, message);
}

static void error_at_current(const char* message) {
    error_at(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scan_token();
        if (parser.current.type != TOKEN_ERROR) break;

        error_at_current(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    error_at_current(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

/// [helper functions] Compiling ///
static void emit_byte(uint8_t byte) {
    write_chunk(current_chunk(), byte, parser.previous.line);
}

static void emit_bytes(uint8_t byte1, uint8_t byte2) {
    emit_byte(byte1);
    emit_byte(byte2);
}

static void emit_return() {
    emit_byte(OP_RETURN);
}

static uint8_t make_constant(Value value) {
    int constant_index = add_constant(current_chunk(), value);
    if (constant_index > UINT8_MAX) {
        error("Too many constants in one chunk");
        return 0;
    }

    return (uint8_t) constant_index;
}

static void emit_constant(Value value) {
    /// add value to constant array of current chunk, then
    /// emit two byte = [OP_CONSTANT, INDEX_TO_CONSTANT]
    emit_bytes(OP_CONSTANT, make_constant(value));
}

static void init_compiler(Compiler* compiler) {
    compiler->local_count = 0;
    compiler->scope_depth = 0;
    current = compiler;
}

static void end_compiler() {
    emit_return();
#ifdef DEBUG_PRINT_CODE
    if (!parser.had_error) {
        disassemble_chunk(current_chunk(), "code");
    }
#endif
}

static void begin_scope() {
    current->scope_depth += 1;
}

static void end_scope() {
    current->scope_depth -= 1;

    /// emit byte to pop all local variables of scope just left
    while (current->local_count > 0 && 
        current->locals[current->local_count - 1].depth > current->scope_depth) 
    {
        emit_byte(OP_POP);
        current->local_count--;
    }
}

/// Compiling ///
static void expression();
static void statement();
static void declaration();
static ParseRule* get_rule(TokenType type);
static void parse_precedence(Precedence precedence);

/// This function takes the given token and adds its lexeme to the chunk’s constant table 
/// as a string. It then returns the index of that constant in the constant table.
static uint8_t identifier_constant(Token* name) {
    return make_constant(OBJ_VAL(copy_string(name->start, name->length)));
}

static bool identifiers_equal(Token *a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolve_local(Compiler* compiler, Token* name) {
    for (int i = compiler->local_count - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiers_equal(name, &local->name)) {
            if (local->depth == -1) {
                // var a = 10; 
                // {
                //     var a = a; // error
                // }
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    // no local found
    return -1;
}

static void add_local(Token name) {
    if (current->local_count == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->local_count++];
    local->name = name;
    local->depth = -1; // -1 means not initialized yet.
}

/// add name of local var in compiler state. so it know it exits and its scope
static void declare_variable() {
    if (current->scope_depth == 0) return;

    Token* name = &parser.previous;

    for (int i = current->local_count - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scope_depth) {
            break;
        }

        if (identifiers_equal(name, &local->name)) {
            error("Already  a variable with name in this scope.");
        }
    }

    add_local(*name);
}

static uint8_t parse_variable(const char* message) {
    consume(TOKEN_IDENTIFIER, message);

    declare_variable();
    /// if compiling local variable we don't store its lexeme
    if (current->scope_depth > 0) {
        return 0; 
    }

    return identifier_constant(&parser.previous);
}

static void mark_initialized() {
    current->locals[current->local_count - 1].depth = current->scope_depth;
}

static void define_variable(uint8_t global) {
    if (current->scope_depth > 0) {
        mark_initialized();
        return;
    }

    emit_bytes(OP_DEFINE_GLOBAL, global);
}

///*** Expressions ***///
static void binary(bool can_assign) {
    TokenType operator_type = parser.previous.type;
    ParseRule* rule = get_rule(operator_type);    
    parse_precedence((Precedence)(rule->precedence + 1));

    // compile operator 
    switch (operator_type) {
        // NOTE: (a <= b) is same as !(a > b), for all equality and comarision operator;
        case TOKEN_BANG_EQUAL:    emit_bytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emit_byte(OP_EQUAL); break;
        case TOKEN_GREATER:       emit_byte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emit_bytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emit_byte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emit_bytes(OP_GREATER, OP_NOT); break;        
        //
        case TOKEN_PLUS:    emit_byte(OP_ADD); break;
        case TOKEN_MINUS:   emit_byte(OP_SUBTRACT); break;
        case TOKEN_STAR:    emit_byte(OP_MULTIPLY); break;
        case TOKEN_SLASH:   emit_byte(OP_DIVIDE); break;
        default: return; // unreachable
    }

    // E.g.   expr1 + expr2      Note: expr1 is already on stack
    // stack     => expr1 expr2 
    // bytecode  => OP_ADD
}

static void literal(bool can_assign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emit_byte(OP_FALSE); break;
        case TOKEN_NIL: emit_byte(OP_NIL); break;
        case TOKEN_TRUE: emit_byte(OP_TRUE); break;
        default: return; // unreachable
    }
}

static void grouping(bool can_assign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool can_assign) {
    double value = strtod(parser.previous.start, NULL);
    emit_constant(NUMBER_VAL(value));
}

static void string(bool can_assign) {
    /// NOTE: The + 1 and - 2 parts trim the leading and trailing quotation marks
    emit_constant(
        OBJ_VAL(copy_string(parser.previous.start + 1, parser.previous.length - 2)));
}

static void named_variable(Token name, bool can_assign) {
    /* first find local variable with the name if found compiles for local var
    else it might be global var; if none identifier_constant() will report error.*/
    uint8_t get_op, set_op;
    int arg = resolve_local(current, &name);
    if (arg != -1) {
        // the variable user is reading/writing is local
        get_op = OP_GET_LOCAL;
        set_op = OP_SET_LOCAL;
    } else {
        // the variable user is reading/writing might be global  or no variable with that name.
        arg = identifier_constant(&name);
        get_op = OP_GET_GLOBAL;
        set_op = OP_SET_GLOBAL;
    }

    if (can_assign && match(TOKEN_EQUAL)) {
        // EXAMPLE: name = "myname";
        expression();
        emit_bytes(set_op, (uint8_t)arg);
    } else {
        // EXMAPLE: print name;
        emit_bytes(get_op, (uint8_t)arg);
    }
}

static void variable(bool can_assign) {
    named_variable(parser.previous, can_assign);
}

static void unary(bool can_assign) {
    // eg. -10 or !is_cool
    // 1. We evaluate the operand first which leaves its value on the stack.
    // 2. Then we pop that value, negate it, and push the result.
    TokenType operator_type = parser.previous.type;
    // Compile the operand.
    parse_precedence(PREC_UNARY);
    // Emit the operator instruction.
    switch (operator_type) {
        case TOKEN_MINUS: emit_byte(OP_NEGATE); break;
        case TOKEN_BANG:  emit_byte(OP_NOT); break;
        default: return;
    }
}

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};
/*  
* Parse expression at the given precendence level or higher. e.g.
* -10 + 20 => we call parsePrecedence(PREC_UNARY), 
    it will compile the -10 and stop there. 
    It doesn’t keep going through the + because 
    the addition has lower precedence than unary operators. 
*/
// EXAMPLE: 10 + -20
// reading expr from left to right, first expr is alway a prefix / unary.
static void parse_precedence(Precedence precedence) {
    advance();
    ParseFn prefix_rule = get_rule(parser.previous.type)->prefix;
    if (prefix_rule == NULL) {
        error("Expect expression.");
        return;
    }

    /// EXAMPLE: compile the number 10, calling prifix func for number is number();
    // If the variable is nested inside some expression with higher precedence, 
    // canAssign will be false and this will ignore the = even if there is one there. 
    bool can_assign = precedence <= PREC_ASSIGNMENT;
    prefix_rule(can_assign); 

    while (precedence <= get_rule(parser.current.type)->precedence) {
        advance();
        ParseFn infix_rule = get_rule(parser.previous.type)->infix;
        infix_rule(can_assign);
    }

    // REF: a * b = c + d; gets error
    if (can_assign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static ParseRule* get_rule(TokenType type) {
    return &rules[type];
}

///*** Statements And Declarations ***///
static void expression() {
    parse_precedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void var_declaration() {
    /// global variable in lox is looked up by name so add its lexeme/name as 
    /// OBJ_STRING value to constant table and get its index  
    uint8_t global = parse_variable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression(); /// compile expression after =, and put it on stack.
    } else {
        emit_byte(OP_NIL);
    }

    consume(TOKEN_SEMICOLON, "Expected ';' after var declaration.");

    /// then add an instruction to define the only global variable not locals
    define_variable(global);
}

static void expression_statement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emit_byte(OP_POP);
}

static void print_statement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emit_byte(OP_PRINT);
}

static void synchronize() {
    parser.panic_mode = false;

    /// keep advancing parser.token until reaches sychronize point.
    /// In lox statement are sychronize points.
    while (!match(TOKEN_EOF)) {
        switch (parser.previous.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN: return;
            default: ; // else keep skipping the token.
        }
        advance();
    }
}

// declaration    → classDecl
//                | funDecl
//                | varDecl
//                | statement ;    
static void declaration() {
    if (match(TOKEN_VAR)) {
        var_declaration();
    } else {
        statement();
    }

    // if current declaration has err compilling then 
    // compile next declaration by moving parser to 
    // next declaration token e.g. class, function, var, if, while
    if (parser.panic_mode) synchronize();
}

// statement      → exprStmt
//                | forStmt
//                | ifStmt
//                | printStmt
//                | returnStmt
//                | whileStmt
//                | block ;
static void statement() {
    if (match(TOKEN_PRINT)) {
        print_statement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        begin_scope();
        block();
        end_scope();
    } else {
        expression_statement();
    }
}

/// Entry ///
bool compile(const char* source, Chunk* chunk) {
    init_scanner(source);
    Compiler compiler;
    init_compiler(&compiler);
    compiling_chunk = chunk;

    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    end_compiler();
    return !parser.had_error;
}
