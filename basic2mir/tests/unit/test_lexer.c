#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h> // For fabs in number checks

// Adjust path as necessary if your test structure is different
#include "../../src/basic_lexer.h"
#include "../../src/basic_ast.h" // For BasicVariableType in BasicToken

// Helper to convert token type to string for better error messages
const char* basic_token_type_to_string_test(BasicTokenType type) {
    // This function should ideally be shared from basic_lexer.c or a utility file.
    // Re-defining a minimal version here for test self-containment.
    switch(type) {
        case TOKEN_EOF: return "TOKEN_EOF";
        case TOKEN_EOL: return "TOKEN_EOL";
        case TOKEN_ERROR: return "TOKEN_ERROR";
        case TOKEN_LINE_NUMBER: return "TOKEN_LINE_NUMBER";
        // Keywords
        case TOKEN_PRINT: return "TOKEN_PRINT";
        case TOKEN_LET: return "TOKEN_LET";
        case TOKEN_IF: return "TOKEN_IF";
        case TOKEN_THEN: return "TOKEN_THEN";
        case TOKEN_ELSE: return "TOKEN_ELSE";
        case TOKEN_GOTO: return "TOKEN_GOTO";
        case TOKEN_FOR: return "TOKEN_FOR";
        case TOKEN_TO: return "TOKEN_TO";
        case TOKEN_STEP: return "TOKEN_STEP";
        case TOKEN_NEXT: return "TOKEN_NEXT";
        case TOKEN_END: return "TOKEN_END";
        case TOKEN_REM: return "TOKEN_REM";
        case TOKEN_DIM: return "TOKEN_DIM";
        case TOKEN_GOSUB: return "TOKEN_GOSUB";
        case TOKEN_RETURN: return "TOKEN_RETURN";
        case TOKEN_INPUT: return "TOKEN_INPUT";
        // Identifiers and Literals
        case TOKEN_IDENTIFIER: return "TOKEN_IDENTIFIER";
        case TOKEN_NUMBER: return "TOKEN_NUMBER";
        case TOKEN_STRING_LITERAL: return "TOKEN_STRING_LITERAL";
        // Operators
        case TOKEN_PLUS: return "TOKEN_PLUS";
        case TOKEN_MINUS: return "TOKEN_MINUS";
        case TOKEN_ASTERISK: return "TOKEN_ASTERISK";
        case TOKEN_SLASH: return "TOKEN_SLASH";
        case TOKEN_BACKSLASH: return "TOKEN_BACKSLASH";
        case TOKEN_CARET: return "TOKEN_CARET";
        case TOKEN_EQUAL: return "TOKEN_EQUAL";
        case TOKEN_LESS: return "TOKEN_LESS";
        case TOKEN_GREATER: return "TOKEN_GREATER";
        case TOKEN_LESS_EQUAL: return "TOKEN_LESS_EQUAL";
        case TOKEN_GREATER_EQUAL: return "TOKEN_GREATER_EQUAL";
        case TOKEN_NOT_EQUAL: return "TOKEN_NOT_EQUAL";
        // Punctuators
        case TOKEN_LPAREN: return "TOKEN_LPAREN";
        case TOKEN_RPAREN: return "TOKEN_RPAREN";
        case TOKEN_COMMA: return "TOKEN_COMMA";
        case TOKEN_SEMICOLON: return "TOKEN_SEMICOLON";
        case TOKEN_COLON: return "TOKEN_COLON";
        // Type Suffixes - these are not distinct tokens from lexer, but part of IDENTIFIER/NUMBER
        // case TOKEN_SUFFIX_STRING: return "TOKEN_SUFFIX_STRING";
        // case TOKEN_SUFFIX_INTEGER: return "TOKEN_SUFFIX_INTEGER";
        // case TOKEN_SUFFIX_SINGLE: return "TOKEN_SUFFIX_SINGLE";
        // case TOKEN_SUFFIX_DOUBLE: return "TOKEN_SUFFIX_DOUBLE";
        case TOKEN_AND: return "TOKEN_AND";
        case TOKEN_OR: return "TOKEN_OR";
        case TOKEN_NOT: return "TOKEN_NOT";
        case TOKEN_XOR: return "TOKEN_XOR";
        case TOKEN_IMP: return "TOKEN_IMP";
        case TOKEN_EQV: return "TOKEN_EQV";
        case TOKEN_MOD: return "TOKEN_MOD";
        case TOKEN_HASH: return "TOKEN_HASH";
        case TOKEN_BANG: return "TOKEN_BANG";
        case TOKEN_PERCENT: return "TOKEN_PERCENT";
        case TOKEN_DOLLAR: return "TOKEN_DOLLAR";

        default:
            fprintf(stderr, "Unknown token type in to_string: %d\n", type);
            return "UNKNOWN_TOKEN_TYPE";
    }
}


// Helper to compare a token with expected values
int check_token(BasicToken token, BasicTokenType expected_type, const char* expected_lexeme,
                int32_t expected_line, int32_t expected_col, const char* test_name, int test_case_num) {
    int pass = 1;
    if (token.type != expected_type) {
        fprintf(stderr, "%s (TC%d): Line %d, Col %d: Expected type %d (%s), got %d (%s) for lexeme '%s'\n",
            test_name, test_case_num, token.line_number, token.column_number,
            expected_type, basic_token_type_to_string_test(expected_type),
            token.type, basic_token_type_to_string_test(token.type), token.lexeme ? token.lexeme : "");
        pass = 0;
    }
    if (expected_lexeme != NULL && token.lexeme != NULL && strcmp(token.lexeme, expected_lexeme) != 0) {
        fprintf(stderr, "%s (TC%d): Line %d, Col %d: Expected lexeme '%s', got '%s'\n",
            test_name, test_case_num, token.line_number, token.column_number, expected_lexeme, token.lexeme);
        pass = 0;
    } else if (expected_lexeme != NULL && token.lexeme == NULL) {
        fprintf(stderr, "%s (TC%d): Line %d, Col %d: Expected lexeme '%s', got NULL\n",
            test_name, test_case_num, token.line_number, token.column_number, expected_lexeme);
        pass = 0;
    }
    if (token.line_number != expected_line) {
        fprintf(stderr, "%s (TC%d): Expected line %d, got %d for lexeme '%s' (type %s)\n",
            test_name, test_case_num, expected_line, token.line_number,
            token.lexeme ? token.lexeme : "N/A", basic_token_type_to_string_test(token.type));
        pass = 0;
    }
    if (token.column_number != expected_col) {
         fprintf(stderr, "%s (TC%d): Expected col %d, got %d for lexeme '%s' (type %s)\n",
             test_name, test_case_num, expected_col, token.column_number,
             token.lexeme ? token.lexeme : "N/A", basic_token_type_to_string_test(token.type));
         pass = 0;
     }
    return pass;
}

int test_lex_line_numbers_keywords(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_lex_line_numbers_keywords";
    int pass = 1;
    const char *source = "10 PRINT\n20 END\n"; // Added trailing newline for consistent EOL
    BasicLexer *lexer = basic_lexer_new(source);
    BasicToken token;
    int tc = 0;

    token = basic_lexer_next_token(lexer);
    pass &= check_token(token, TOKEN_LINE_NUMBER, "10", 1, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer);
    pass &= check_token(token, TOKEN_PRINT, "PRINT", 1, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer);
    pass &= check_token(token, TOKEN_EOL, "\\n", 1, 9, test_name, ++tc);

    token = basic_lexer_next_token(lexer);
    pass &= check_token(token, TOKEN_LINE_NUMBER, "20", 2, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer);
    pass &= check_token(token, TOKEN_END, "END", 2, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer);
    pass &= check_token(token, TOKEN_EOL, "\\n", 2, 7, test_name, ++tc);

    token = basic_lexer_next_token(lexer);
    pass &= check_token(token, TOKEN_EOF, "EOF", 3, 1, test_name, ++tc);

    basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_lex_identifiers_and_suffixes(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_lex_identifiers_and_suffixes";
    int pass = 1;
    const char *source = "100 A\n110 B$\n120 C%\n130 D!\n140 E#\n150 F.G"; // F.G might be specific to some BASICs
    BasicLexer *lexer = basic_lexer_new(source);
    BasicToken token;
    int tc = 0;

    // Line 100
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "100", 1, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "A", 1, 5, test_name, ++tc);
    if(token.var_type_suffix != BASIC_VAR_TYPE_DEFAULT && token.var_type_suffix != BASIC_VAR_TYPE_SINGLE) { pass = 0; fprintf(stderr, "%s (TC%d): Expected A to have default/single type, got %d\n", test_name, tc, token.var_type_suffix); }
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 1, 6, test_name, ++tc);
    // Line 110
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "110", 2, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "B$", 2, 5, test_name, ++tc);
    if(token.var_type_suffix != BASIC_VAR_TYPE_STRING) { pass = 0; fprintf(stderr, "%s (TC%d): Expected B$ to have string type, got %d\n", test_name, tc, token.var_type_suffix); }
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 2, 7, test_name, ++tc);
    // Line 120
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "120", 3, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "C%", 3, 5, test_name, ++tc);
    if(token.var_type_suffix != BASIC_VAR_TYPE_INTEGER) { pass = 0; fprintf(stderr, "%s (TC%d): Expected C%% to have integer type, got %d\n", test_name, tc, token.var_type_suffix); }
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 3, 7, test_name, ++tc);
    // Line 130
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "130", 4, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "D!", 4, 5, test_name, ++tc);
    if(token.var_type_suffix != BASIC_VAR_TYPE_SINGLE) { pass = 0; fprintf(stderr, "%s (TC%d): Expected D! to have single type, got %d\n", test_name, tc, token.var_type_suffix); }
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 4, 7, test_name, ++tc);
    // Line 140
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "140", 5, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "E#", 5, 5, test_name, ++tc);
    if(token.var_type_suffix != BASIC_VAR_TYPE_DOUBLE) { pass = 0; fprintf(stderr, "%s (TC%d): Expected E# to have double type, got %d\n", test_name, tc, token.var_type_suffix); }
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 5, 7, test_name, ++tc);
    // Line 150
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "150", 6, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "F.G", 6, 5, test_name, ++tc); // Assuming lexer handles '.' in identifiers
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 6, 8, test_name, ++tc);


    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOF, "EOF", 7, 1, test_name, ++tc);

    basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_lex_numbers(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_lex_numbers";
    int pass = 1;
    // Adjusted column numbers, assuming numbers are tokenized right after space/start.
    const char *source = "10 N1 = 123\n20 N2 = 3.14\n30 N3# = 1.23D+5\n40 N4! = 123.45!\n50 N5% = 42%\n60 N6 = .5";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicToken token;
    int tc = 0;

    // 10 N1 = 123
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "10", 1, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "N1", 1, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 1, 7, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_NUMBER, "123", 1, 9, test_name, ++tc);
    if(fabs(token.literal_value.number_value - 123.0) > 1e-6 || token.var_type_suffix != BASIC_VAR_TYPE_SINGLE) {pass=0; fprintf(stderr, "N1 val/type error\n");}
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 1, 12, test_name, ++tc);

    // 20 N2 = 3.14
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "20", 2, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "N2", 2, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 2, 7, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_NUMBER, "3.14", 2, 9, test_name, ++tc);
    if(fabs(token.literal_value.number_value - 3.14) > 1e-6 || token.var_type_suffix != BASIC_VAR_TYPE_DOUBLE) {pass=0; fprintf(stderr, "N2 val/type error. Got type %d\n", token.var_type_suffix);} // Lexer defaults float with decimal to double
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 2, 13, test_name, ++tc);

    // 30 N3# = 1.23D+5
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "30", 3, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "N3#", 3, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 3, 8, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_NUMBER, "1.23D+5", 3, 10, test_name, ++tc);
    if(fabs(token.literal_value.number_value - 1.23e5) > 1e-6 || token.var_type_suffix != BASIC_VAR_TYPE_DOUBLE) {pass=0; fprintf(stderr, "N3 val/type error\n");}
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 3, 17, test_name, ++tc);

    // 40 N4! = 123.45!
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "40", 4, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "N4!", 4, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 4, 8, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_NUMBER, "123.45", 4, 10, test_name, ++tc); // Lexeme doesn't include suffix for number itself
    if(fabs(token.literal_value.number_value - 123.45) > 1e-6 || token.var_type_suffix != BASIC_VAR_TYPE_SINGLE) {pass=0; fprintf(stderr, "N4 val/type error\n");}
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 4, 17, test_name, ++tc);

    // 50 N5% = 42%
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "50", 5, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "N5%", 5, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 5, 8, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_NUMBER, "42", 5, 10, test_name, ++tc); // Lexeme doesn't include suffix
    if(fabs(token.literal_value.number_value - 42.0) > 1e-6 || token.var_type_suffix != BASIC_VAR_TYPE_INTEGER) {pass=0; fprintf(stderr, "N5 val/type error\n");}
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 5, 13, test_name, ++tc);

    // 60 N6 = .5
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "60", 6, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "N6", 6, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 6, 7, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_NUMBER, ".5", 6, 9, test_name, ++tc);
    if(fabs(token.literal_value.number_value - 0.5) > 1e-6 || token.var_type_suffix != BASIC_VAR_TYPE_DOUBLE) {pass=0; fprintf(stderr, "N6 val/type error. Got type %d\n", token.var_type_suffix);} // Lexer defaults float with decimal to double
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 6, 11, test_name, ++tc);


    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOF, "EOF", 7, 1, test_name, ++tc);

    basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_lex_strings(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_lex_strings";
    int pass = 1;
    const char *source = "10 S$ = \"HELLO\"\n20 T$ = \"QUOTE \"\" HERE\"\n30 U$=\"\""; // Empty string
    BasicLexer *lexer = basic_lexer_new(source);
    BasicToken token;
    int tc = 0;

    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "10", 1, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "S$", 1, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 1, 7, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_STRING_LITERAL, "HELLO", 1, 9, test_name, ++tc);
    if(strcmp(token.literal_value.string_value, "HELLO") != 0) { pass=0; fprintf(stderr, "S$ val error\n");}
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 1, 16, test_name, ++tc);

    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "20", 2, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "T$", 2, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 2, 7, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_STRING_LITERAL, "QUOTE \" HERE", 2, 9, test_name, ++tc); // Lexer should handle "" escape
    if(strcmp(token.literal_value.string_value, "QUOTE \" HERE") != 0) { pass=0; fprintf(stderr, "T$ val error, got '%s'\n", token.literal_value.string_value);}
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 2, 25, test_name, ++tc);

    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "30", 3, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "U$", 3, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 3, 6, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_STRING_LITERAL, "", 3, 7, test_name, ++tc);
    if(strcmp(token.literal_value.string_value, "") != 0) { pass=0; fprintf(stderr, "U$ val error, got '%s'\n", token.literal_value.string_value);}
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 3, 10, test_name, ++tc);


    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOF, "EOF", 4, 1, test_name, ++tc);

    basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_lex_operators_and_punct(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_lex_operators_and_punct";
    int pass = 1;
    const char *source = "10 A = B + C - D * E / F \\ G ^ H\n20 IF K < L OR M > N AND P <> Q THEN GOTO 100\n30 X = (Y + Z) : REM COLON";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicToken token;
    int tc = 0;

    // Line 1
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "10", 1, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "A", 1, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 1, 6, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "B", 1, 8, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_PLUS, "+", 1, 10, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "C", 1, 12, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_MINUS, "-", 1, 14, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "D", 1, 16, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_ASTERISK, "*", 1, 18, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "E", 1, 20, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_SLASH, "/", 1, 22, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "F", 1, 24, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_BACKSLASH, "\\", 1, 26, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "G", 1, 28, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_CARET, "^", 1, 30, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "H", 1, 32, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 1, 33, test_name, ++tc);

    // Line 2
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "20", 2, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IF, "IF", 2, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "K", 2, 7, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LESS, "<", 2, 9, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "L", 2, 11, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_OR, "OR", 2, 13, test_name, ++tc); // Assuming OR is a keyword
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "M", 2, 16, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_GREATER, ">", 2, 18, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "N", 2, 20, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_AND, "AND", 2, 22, test_name, ++tc); // Assuming AND is a keyword
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "P", 2, 26, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_NOT_EQUAL, "<>", 2, 28, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "Q", 2, 31, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_THEN, "THEN", 2, 33, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_GOTO, "GOTO", 2, 38, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_NUMBER, "100", 2, 43, test_name, ++tc); // GOTO target is number
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 2, 46, test_name, ++tc);

    // Line 3
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LINE_NUMBER, "30", 3, 1, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "X", 3, 4, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EQUAL, "=", 3, 6, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_LPAREN, "(", 3, 8, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "Y", 3, 9, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_PLUS, "+", 3, 11, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_IDENTIFIER, "Z", 3, 13, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_RPAREN, ")", 3, 14, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_COLON, ":", 3, 16, test_name, ++tc);
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_REM, "REM COLON", 3, 18, test_name, ++tc); // REM consumes rest of line
    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOL, "\\n", 3, 27, test_name, ++tc);


    token = basic_lexer_next_token(lexer); pass &= check_token(token, TOKEN_EOF, "EOF", 4, 1, test_name, ++tc);

    basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}


int main() {
    int tests_passed = 0;
    int tests_failed = 0;

    test_lex_line_numbers_keywords(&tests_passed, &tests_failed);
    test_lex_identifiers_and_suffixes(&tests_passed, &tests_failed);
    test_lex_numbers(&tests_passed, &tests_failed);
    test_lex_strings(&tests_passed, &tests_failed);
    test_lex_operators_and_punct(&tests_passed, &tests_failed);
    // Add calls to more specific tests as they are written

    fprintf(stdout, "\nLexer Test Summary: %d passed, %d failed.\n", tests_passed, tests_failed);
    return (tests_failed == 0) ? 0 : 1;
}
