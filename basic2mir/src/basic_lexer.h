#ifndef BASIC_LEXER_H
#define BASIC_LEXER_H

#include <stdint.h> // For int32_t
#include "basic_ast.h" // For BasicVariableType, though maybe not strictly needed here yet

// Maximum length for lexemes (identifiers, string literals, etc.)
#define MAX_LEXEME_LENGTH 256

typedef enum {
    TOKEN_EOF = 0, // End of File
    TOKEN_EOL,     // End of Line (\n or \r\n)
    TOKEN_ERROR,   // Lexical error

    // Literals
    TOKEN_NUMBER,
    TOKEN_STRING_LITERAL,
    TOKEN_LINE_NUMBER, // Special token for BASIC line numbers at the start of a line

    // Identifiers
    TOKEN_IDENTIFIER, // Variables, function names if any

    // Keywords (subset for now)
    TOKEN_LET,
    TOKEN_PRINT,
    TOKEN_INPUT,
    TOKEN_IF,
    TOKEN_THEN,
    TOKEN_ELSE,
    TOKEN_FOR,
    TOKEN_TO,
    TOKEN_STEP,
    TOKEN_NEXT,
    TOKEN_GOTO,
    TOKEN_GOSUB,
    TOKEN_RETURN,
    TOKEN_DIM,
    TOKEN_REM, // Remark/comment
    TOKEN_END,

    // Operators
    TOKEN_PLUS,     // +
    TOKEN_MINUS,    // -
    TOKEN_ASTERISK, // *
    TOKEN_SLASH,    // /
    TOKEN_BACKSLASH,// \ (integer division)
    TOKEN_CARET,    // ^ (exponentiation)
    TOKEN_EQUAL,    // =
    TOKEN_LESS,     // <
    TOKEN_GREATER,  // >
    TOKEN_LESS_EQUAL,    // <=
    TOKEN_GREATER_EQUAL, // >=
    TOKEN_NOT_EQUAL,     // <>

    // Relational and Logical Operators (some might be keywords in BASIC like AND, OR, NOT)
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_NOT,
    TOKEN_XOR,
    TOKEN_IMP,
    TOKEN_EQV,
    TOKEN_MOD,      // Keyword operator

    // Punctuators
    TOKEN_LPAREN,   // (
    TOKEN_RPAREN,   // )
    TOKEN_COMMA,    // ,
    TOKEN_SEMICOLON,// ;
    TOKEN_COLON,    // : (statement separator)
    TOKEN_HASH,     // # (used for file numbers, double precision suffix)
    TOKEN_BANG,     // ! (single precision suffix)
    TOKEN_PERCENT,  // % (integer suffix)
    TOKEN_DOLLAR,   // $ (string suffix)

    // Add more tokens as needed
} BasicTokenType;

typedef struct {
    BasicTokenType type;
    char lexeme[MAX_LEXEME_LENGTH]; // Textual representation of the token
    int32_t line_number;            // Line number where the token starts
    int32_t column_number;          // Column number where the token starts

    union {
        double number_value;   // For TOKEN_NUMBER
        char string_value[MAX_LEXEME_LENGTH]; // For TOKEN_STRING_LITERAL (already in lexeme, but cleaner)
        int32_t line_num_value; // For TOKEN_LINE_NUMBER
    } literal_value;

    BasicVariableType var_type_suffix; // For identifiers, e.g. A$, B%

} BasicToken;

typedef struct {
    const char *source_code;
    size_t source_length;
    size_t current_pos;
    char current_char;
    int32_t current_line_number;
    int32_t current_column_number; // Position on the current line
    int at_line_start; // Flag to indicate if we are at the start of a new line (for TOKEN_LINE_NUMBER)
} BasicLexer;

// Lexer API
BasicLexer *basic_lexer_new(const char *source_code);
void basic_lexer_free(BasicLexer *lexer);
BasicToken basic_lexer_next_token(BasicLexer *lexer);

// Helper function declarations (primarily for internal use, but good to declare)
void lexer_advance(BasicLexer *lexer);
char lexer_peek(BasicLexer *lexer);
char lexer_peek_next(BasicLexer *lexer);
void lexer_skip_whitespace(BasicLexer *lexer);
int is_eof(BasicLexer *lexer);
BasicToken make_token(BasicLexer *lexer, BasicTokenType type, const char *lexeme_override);
BasicToken make_number_token(BasicLexer *lexer, double value, BasicVariableType num_type);
BasicToken make_identifier_token(BasicLexer *lexer, const char *lexeme);
BasicToken make_string_token(BasicLexer *lexer, const char* value);
BasicToken make_error_token(BasicLexer *lexer, const char *message);


#endif // BASIC_LEXER_H
