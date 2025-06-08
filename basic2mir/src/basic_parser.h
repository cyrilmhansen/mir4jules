#ifndef BASIC_PARSER_H
#define BASIC_PARSER_H

#include "basic_ast.h"
#include "basic_lexer.h"

typedef struct {
    BasicLexer *lexer;
    BasicToken current_token;
    BasicToken peek_token;

    // Error handling
    int error_count;
    char error_message[256]; // Buffer for the last error message
    // TODO: Add a flag to indicate if parsing should stop or try to recover
} BasicParser;

// Public Parser Functions
BasicParser *basic_parser_new(BasicLexer *lexer);
void basic_parser_free(BasicParser *parser);
BasicAstNode *basic_parser_parse_program(BasicParser *parser);

// Internal helper function declarations (prototypes for functions to be defined in basic_parser.c)
// These would typically be static in basic_parser.c, but listed here for clarity of structure.
// If they are to be used only within basic_parser.c, they don't strictly need to be in the .h
// However, for a large parser, sometimes these are grouped into different files.

// Forward declarations for recursive descent parser functions
static BasicAstNode *parse_line(BasicParser *parser);
static BasicAstNode *parse_statement(BasicParser *parser);
static BasicAstNode *parse_let_statement(BasicParser *parser);
static BasicAstNode *parse_print_statement(BasicParser *parser);
static BasicAstNode *parse_goto_statement(BasicParser *parser);
static BasicAstNode *parse_rem_statement(BasicParser *parser);
static BasicAstNode *parse_end_statement(BasicParser *parser);
// ... other statement types ...

static BasicAstNode *parse_variable_reference(BasicParser *parser); // Parses A or A(1,2) or A$ etc.
static BasicAstNode *parse_expression(BasicParser *parser);
static BasicAstNode *parse_term(BasicParser *parser); // Precedence level for * /
static BasicAstNode *parse_factor(BasicParser *parser); // Precedence level for numbers, vars, (expr)
// ... other expression precedence levels if needed ...

// Utility functions
static void parser_advance_token(BasicParser *parser);
static int parser_eat(BasicParser *parser, BasicTokenType type, const char *error_message_on_fail);
static void parser_error(BasicParser *parser, const char *message_format, ...);

#endif // BASIC_PARSER_H
