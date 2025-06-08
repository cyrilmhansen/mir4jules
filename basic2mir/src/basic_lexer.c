#include "basic_lexer.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h> // For printf in error reporting or debugging

// Helper to create a token easily
BasicToken create_token(BasicTokenType type, const char* lexeme, int32_t line, int32_t col) {
    BasicToken token;
    token.type = type;
    strncpy(token.lexeme, lexeme, MAX_LEXEME_LENGTH - 1);
    token.lexeme[MAX_LEXEME_LENGTH - 1] = '\0'; // Ensure null termination
    token.line_number = line;
    token.column_number = col;
    token.var_type_suffix = BASIC_VAR_TYPE_DEFAULT; // Default, can be changed for identifiers
    // Initialize literal values to sensible defaults
    token.literal_value.number_value = 0.0;
    memset(token.literal_value.string_value, 0, MAX_LEXEME_LENGTH);
    token.literal_value.line_num_value = 0;
    return token;
}

BasicLexer *basic_lexer_new(const char *source_code) {
    if (!source_code) {
        return NULL;
    }
    BasicLexer *lexer = (BasicLexer *)malloc(sizeof(BasicLexer));
    if (!lexer) {
        //perror("Failed to allocate BasicLexer");
        return NULL;
    }

    lexer->source_code = source_code;
    lexer->source_length = strlen(source_code);
    lexer->current_pos = 0;
    lexer->current_line_number = 1;
    lexer->current_column_number = 1;
    lexer->at_line_start = 1; // True at the beginning of the source

    if (lexer->source_length > 0) {
        lexer->current_char = lexer->source_code[0];
    } else {
        lexer->current_char = '\0'; // EOF
    }
    return lexer;
}

void basic_lexer_free(BasicLexer *lexer) {
    if (lexer) {
        // If source_code was strdup'd, free it here. Assuming it's a const ptr for now.
        free(lexer);
    }
}

int is_eof(BasicLexer *lexer) {
    return lexer->current_pos >= lexer->source_length || lexer->current_char == '\0';
}

void lexer_advance(BasicLexer *lexer) {
    if (is_eof(lexer)) {
        lexer->current_char = '\0'; // Ensure current_char is EOF sentinel
        return;
    }

    if (lexer->current_char == '\n') {
        lexer->current_line_number++;
        lexer->current_column_number = 1;
        lexer->at_line_start = 1;
    } else {
        lexer->current_column_number++;
        // Only set at_line_start to 0 if it's not a newline character that we just processed.
        // If it was a newline, at_line_start is already set to 1.
        // If it was not a newline, then we are no longer at line start.
        if (lexer->current_char != '\r') { // ignore CR for at_line_start logic if followed by LF
             lexer->at_line_start = 0;
        }
    }

    lexer->current_pos++;
    if (lexer->current_pos < lexer->source_length) {
        lexer->current_char = lexer->source_code[lexer->current_pos];
    } else {
        lexer->current_char = '\0'; // EOF
    }
}

char lexer_peek(BasicLexer *lexer) {
    if (is_eof(lexer)) {
        return '\0';
    }
    return lexer->current_char;
}

char lexer_peek_next(BasicLexer *lexer) {
    if (lexer->current_pos + 1 >= lexer->source_length) {
        return '\0'; // EOF
    }
    return lexer->source_code[lexer->current_pos + 1];
}

void lexer_skip_whitespace(BasicLexer *lexer) {
    while (!is_eof(lexer) && (lexer->current_char == ' ' || lexer->current_char == '\t')) {
        // If we skip whitespace, we are no longer at the start of the line for line number parsing,
        // unless the whitespace itself is at the very start.
        // This will be naturally handled as at_line_start is only set by lexer_advance on newlines.
        // If we advance past a space, at_line_start becomes 0.
        lexer_advance(lexer);
    }
}

// Internal helper to build tokens
BasicToken make_token_internal(BasicLexer *lexer, BasicTokenType type, const char *lexeme_start, size_t length, int32_t start_line, int32_t start_col) {
    BasicToken token;
    token.type = type;
    if (length >= MAX_LEXEME_LENGTH) {
        length = MAX_LEXEME_LENGTH - 1; // Truncate if too long
        // Consider logging a warning or error here
    }
    strncpy(token.lexeme, lexeme_start, length);
    token.lexeme[length] = '\0';
    token.line_number = start_line;
    token.column_number = start_col;
    token.var_type_suffix = BASIC_VAR_TYPE_DEFAULT;
    token.literal_value.number_value = 0.0;
    memset(token.literal_value.string_value, 0, MAX_LEXEME_LENGTH);
    token.literal_value.line_num_value = 0;
    return token;
}


BasicToken lexer_parse_number(BasicLexer *lexer) {
    char buffer[MAX_LEXEME_LENGTH];
    int i = 0;
    int32_t start_line = lexer->current_line_number;
    int32_t start_col = lexer->current_column_number;
    int has_decimal = 0;
    int has_exponent = 0;
    BasicVariableType num_type = BASIC_VAR_TYPE_DEFAULT; // Default, might become SINGLE or DOUBLE

    while (!is_eof(lexer) && (isdigit(lexer->current_char) || lexer->current_char == '.')) {
        if (lexer->current_char == '.') {
            if (has_decimal) break; // Only one decimal point allowed
            has_decimal = 1;
        }
        buffer[i++] = lexer->current_char;
        lexer_advance(lexer);
        if (i >= MAX_LEXEME_LENGTH - 1) break;
    }

    // GW-BASIC exponent (E, D, e, d)
    if (!is_eof(lexer) && (toupper(lexer->current_char) == 'E' || toupper(lexer->current_char) == 'D')) {
        has_exponent = 1;
        num_type = BASIC_VAR_TYPE_DOUBLE; // Exponents imply double precision
        buffer[i++] = lexer->current_char;
        lexer_advance(lexer);
        if (!is_eof(lexer) && (lexer->current_char == '+' || lexer->current_char == '-')) {
            if (i < MAX_LEXEME_LENGTH -1) buffer[i++] = lexer->current_char;
            lexer_advance(lexer);
        }
        while (!is_eof(lexer) && isdigit(lexer->current_char)) {
            if (i < MAX_LEXEME_LENGTH -1) buffer[i++] = lexer->current_char;
            lexer_advance(lexer);
            if (i >= MAX_LEXEME_LENGTH - 1) break;
        }
    }
    buffer[i] = '\0';

    // Check for type suffixes if no exponent was found to override
    if (!has_exponent) {
        if (lexer->current_char == '!') {
            num_type = BASIC_VAR_TYPE_SINGLE;
            // No need to add '!' to buffer for atof, advance past it
            lexer_advance(lexer);
        } else if (lexer->current_char == '#') {
            num_type = BASIC_VAR_TYPE_DOUBLE;
            lexer_advance(lexer);
        } else if (lexer->current_char == '%') {
            // Integer requires special handling if we are to store it as int
            // For now, let's treat all as double, suffix indicates original type
            num_type = BASIC_VAR_TYPE_INTEGER;
            lexer_advance(lexer);
        }
    }

    // If no explicit type, and no decimal, it's an integer by default contextually,
    // but numbers without suffix are usually single precision in GW-BASIC.
    // Let's default to single if no decimal/exp/suffix, and double if decimal/exp.
    if (num_type == BASIC_VAR_TYPE_DEFAULT) {
        if (has_decimal || has_exponent) {
            num_type = BASIC_VAR_TYPE_DOUBLE; // Or single, GW-BASIC defaults to single
        } else {
             num_type = BASIC_VAR_TYPE_SINGLE; // Default for numbers without suffix/decimal
        }
    }


    BasicToken token = make_token_internal(lexer, TOKEN_NUMBER, buffer, strlen(buffer), start_line, start_col);
    token.literal_value.number_value = atof(buffer); // atof handles E/D notation
    token.var_type_suffix = num_type; // Store the determined type
    // If it was explicitly integer (%), could use atol or strtol and store in a different union member if desired.
    // For now, all go to double.
    if (num_type == BASIC_VAR_TYPE_INTEGER) {
         token.literal_value.number_value = (double)atol(buffer);
    }

    return token;
}

// Keyword lookup table
typedef struct {
    const char *name;
    BasicTokenType type;
} Keyword;

// GW-BASIC keywords are case-insensitive
// This list needs to be comprehensive
static Keyword keywords[] = {
    {"LET", TOKEN_LET},
    {"PRINT", TOKEN_PRINT},
    {"INPUT", TOKEN_INPUT},
    {"IF", TOKEN_IF},
    {"THEN", TOKEN_THEN},
    {"ELSE", TOKEN_ELSE},
    {"FOR", TOKEN_FOR},
    {"TO", TOKEN_TO},
    {"STEP", TOKEN_STEP},
    {"NEXT", TOKEN_NEXT},
    {"GOTO", TOKEN_GOTO},
    {"GOSUB", TOKEN_GOSUB},
    {"RETURN", TOKEN_RETURN},
    {"DIM", TOKEN_DIM},
    {"REM", TOKEN_REM},
    {"END", TOKEN_END},
    {"AND", TOKEN_AND},
    {"OR", TOKEN_OR},
    {"NOT", TOKEN_NOT},
    {"XOR", TOKEN_XOR},
    {"IMP", TOKEN_IMP},
    {"EQV", TOKEN_EQV},
    {"MOD", TOKEN_MOD}
    // Add all other GW-BASIC keywords here
};
static size_t num_keywords = sizeof(keywords) / sizeof(Keyword);

// Case-insensitive string comparison
// Note: strcasecmp is POSIX, _stricmp is Windows. For portability, implement manually or use a library.
// Simple manual implementation for ASCII:
int my_strcasecmp(const char *s1, const char *s2) {
    while (*s1 && *s2) {
        if (tolower((unsigned char)*s1) != tolower((unsigned char)*s2)) {
            return tolower((unsigned char)*s1) - tolower((unsigned char)*s2);
        }
        s1++;
        s2++;
    }
    return tolower((unsigned char)*s1) - tolower((unsigned char)*s2);
}

BasicToken lexer_parse_identifier_or_keyword(BasicLexer *lexer) {
    char buffer[MAX_LEXEME_LENGTH];
    int i = 0;
    int32_t start_line = lexer->current_line_number;
    int32_t start_col = lexer->current_column_number;
    BasicVariableType var_type = BASIC_VAR_TYPE_DEFAULT;

    // Identifiers start with a letter
    if (!isalpha(lexer->current_char)) {
        // This function should only be called if current_char is alpha
        return make_error_token(lexer, "Identifier started with non-alpha character");
    }

    // Read potential identifier (letters and digits)
    while (!is_eof(lexer) && (isalnum(lexer->current_char) || lexer->current_char == '.')) { // Allow '.' in names for now, though not standard for vars
        if (i < MAX_LEXEME_LENGTH - 1) {
            buffer[i++] = lexer->current_char;
        }
        lexer_advance(lexer);
    }
    buffer[i] = '\0';

    // Check for type declaration character ($ % ! #)
    // These are part of the identifier in GW-BASIC for tokenization purposes
    // The type suffix is the last character if it's one of these
    // and the preceding character is not part of an exponent for a number (already handled by parse_number)
    char last_char_of_ident = '\0';
    if (i > 0) { // If buffer has content
        // Peek at current char, which is AFTER the main identifier part
        if (lexer->current_char == '$') {
            var_type = BASIC_VAR_TYPE_STRING;
            if (i < MAX_LEXEME_LENGTH - 1) buffer[i++] = lexer->current_char;
            lexer_advance(lexer);
        } else if (lexer->current_char == '%') {
            var_type = BASIC_VAR_TYPE_INTEGER;
            if (i < MAX_LEXEME_LENGTH - 1) buffer[i++] = lexer->current_char;
            lexer_advance(lexer);
        } else if (lexer->current_char == '!') {
            var_type = BASIC_VAR_TYPE_SINGLE;
            if (i < MAX_LEXEME_LENGTH - 1) buffer[i++] = lexer->current_char;
            lexer_advance(lexer);
        } else if (lexer->current_char == '#') {
            var_type = BASIC_VAR_TYPE_DOUBLE;
            if (i < MAX_LEXEME_LENGTH - 1) buffer[i++] = lexer->current_char;
            lexer_advance(lexer);
        }
        buffer[i] = '\0'; // Re-terminate after potentially adding suffix
    }


    // Check if it's a keyword (case-insensitive)
    // Keywords generally don't have type suffixes, but REM is an exception.
    // Let's assume for now keywords don't have suffixes. If "LETTERS$" is a keyword, this logic needs adjustment.
    // Typically, "PRINT" is a keyword, "PRINT$" is a variable.
    // So, we check for keyword *before* considering the suffix as part of a variable name for keyword matching.
    // For example, if buffer is "PRINT$", we first check "PRINT$". If not a keyword, then "PRINT" is checked.
    // Or, more simply, keywords are typically shorter and don't have these suffixes.
    // Let's assume keywords do not have suffixes.

    char check_buffer[MAX_LEXEME_LENGTH];
    strncpy(check_buffer, buffer, i); // Use i before it's modified by suffix addition if we change logic
    check_buffer[i] = '\0';
    // Strip suffix for keyword check if var_type was set
    if (var_type != BASIC_VAR_TYPE_DEFAULT && i > 0) {
        // Check if the character before suffix is part of keyword
        // e.g. if buffer is "END#", check_buffer becomes "END"
        char last_char = buffer[i-1];
        if (last_char == '$' || last_char == '%' || last_char == '!' || last_char == '#') {
            if (i > 1) { // ensure there is something before the suffix
                 strncpy(check_buffer, buffer, i-1);
                 check_buffer[i-1] = '\0';
            } else {
                 check_buffer[0] = '\0'; // effectively no keyword if only suffix
            }
        }
    }


    for (size_t j = 0; j < num_keywords; ++j) {
        if (my_strcasecmp(check_buffer, keywords[j].name) == 0) {
            // If it's REM, the rest of the line is part of the token
            if (keywords[j].type == TOKEN_REM) {
                // Consume and store the rest of the line as part of REM's lexeme
                // For now, just return TOKEN_REM, parser can skip.
                // Or, advanced lexer could gather it.
                // Let's try to gather it.
                size_t rem_start_idx = i; // current length of "REM"
                // buffer already contains "REM" (or "rem", etc.)
                // We want to append the comment.
                // The lexer_advance calls for suffix might have moved past useful chars, reset i to length of "REM"
                i = strlen(keywords[j].name); // Reset i to length of "REM"
                if (lexer->current_char == ' ') lexer_advance(lexer); // consume one space after REM if present

                while(!is_eof(lexer) && lexer->current_char != '\n' && lexer->current_char != '\r') {
                    if (i < MAX_LEXEME_LENGTH - 1) {
                        buffer[i++] = lexer->current_char;
                    }
                    lexer_advance(lexer);
                }
                buffer[i] = '\0';
                return make_token_internal(lexer, TOKEN_REM, buffer, i, start_line, start_col);
            }
            return make_token_internal(lexer, keywords[j].type, check_buffer, strlen(check_buffer), start_line, start_col);
        }
    }

    // Not a keyword, so it's an identifier
    BasicToken token = make_token_internal(lexer, TOKEN_IDENTIFIER, buffer, strlen(buffer), start_line, start_col);
    token.var_type_suffix = var_type;
    return token;
}

BasicToken lexer_parse_string_literal(BasicLexer *lexer) {
    char buffer[MAX_LEXEME_LENGTH];
    int i = 0;
    int32_t start_line = lexer->current_line_number;
    int32_t start_col = lexer->current_column_number;

    if (lexer->current_char != '"') {
        return make_error_token(lexer, "String literal did not start with a quote");
    }
    lexer_advance(lexer); // Consume opening quote

    while (!is_eof(lexer)) {
        if (lexer->current_char == '"') {
            // Check for "" escape sequence (two double quotes)
            if (lexer_peek_next(lexer) == '"') {
                if (i < MAX_LEXEME_LENGTH - 1) {
                    buffer[i++] = '"'; // Store one quote
                }
                lexer_advance(lexer); // Consume the first quote of the pair
                lexer_advance(lexer); // Consume the second quote of the pair
            } else {
                lexer_advance(lexer); // Consume closing quote
                buffer[i] = '\0';
                BasicToken token = make_token_internal(lexer, TOKEN_STRING_LITERAL, buffer, strlen(buffer), start_line, start_col);
                // The actual string value is already in token.lexeme, but can also be copied to string_value
                strncpy(token.literal_value.string_value, buffer, MAX_LEXEME_LENGTH -1);
                token.literal_value.string_value[MAX_LEXEME_LENGTH -1] = '\0';
                return token;
            }
        } else {
            if (lexer->current_char == '\n' || lexer->current_char == '\r') {
                // Unterminated string literal
                buffer[i] = '\0';
                 BasicToken err_token = make_token_internal(lexer, TOKEN_ERROR, buffer, strlen(buffer), start_line, start_col);
                 strncpy(err_token.literal_value.string_value, "Unterminated string literal", MAX_LEXEME_LENGTH - 1);
                 return err_token;
            }
            if (i < MAX_LEXEME_LENGTH - 1) {
                buffer[i++] = lexer->current_char;
            }
            lexer_advance(lexer);
        }
    }

    // EOF reached before closing quote
    buffer[i] = '\0';
    BasicToken err_token = make_token_internal(lexer, TOKEN_ERROR, buffer, strlen(buffer), start_line, start_col);
    strncpy(err_token.literal_value.string_value, "EOF in string literal", MAX_LEXEME_LENGTH-1);
    return err_token;
}


BasicToken basic_lexer_next_token(BasicLexer *lexer) {
    if (!lexer) return create_token(TOKEN_ERROR, "Lexer not initialized", 0, 0);

    lexer_skip_whitespace(lexer); // Skip leading/inter-token whitespace

    if (is_eof(lexer)) {
        return create_token(TOKEN_EOF, "EOF", lexer->current_line_number, lexer->current_column_number);
    }

    char ch = lexer->current_char;
    int32_t start_line = lexer->current_line_number;
    int32_t start_col = lexer->current_column_number;

    // Handle line numbers at the very start of a line
    if (lexer->at_line_start && isdigit(ch)) {
        char buffer[MAX_LEXEME_LENGTH];
        int i = 0;
        do {
            buffer[i++] = lexer->current_char;
            lexer_advance(lexer);
        } while (!is_eof(lexer) && isdigit(lexer->current_char) && i < MAX_LEXEME_LENGTH - 1);
        buffer[i] = '\0';

        lexer->at_line_start = 0;

        BasicToken token = create_token(TOKEN_LINE_NUMBER, buffer, start_line, start_col);
        token.literal_value.line_num_value = atoi(buffer);
        return token;
    }
    lexer->at_line_start = 0;

    // Newlines
    if (ch == '\n') {
        lexer_advance(lexer);
        return create_token(TOKEN_EOL, "\\n", start_line, start_col);
    }
    if (ch == '\r') {
        lexer_advance(lexer);
        if (lexer->current_char == '\n') {
            lexer_advance(lexer);
            return create_token(TOKEN_EOL, "\\r\\n", start_line, start_col);
        }
        return create_token(TOKEN_EOL, "\\r", start_line, start_col);
    }

    // String Literals
    if (ch == '"') {
        return lexer_parse_string_literal(lexer);
    }

    // Numbers
    if (isdigit(ch) || (ch == '.' && isdigit(lexer_peek_next(lexer)))) {
        return lexer_parse_number(lexer);
    }

    // Identifiers and Keywords
    if (isalpha(ch)) {
        return lexer_parse_identifier_or_keyword(lexer);
    }

    // Operators and Punctuators
    // Ensure to advance lexer for each token recognized.
    // make_token_internal expects the lexer to be positioned *after* the token.
    // create_token is simpler and uses current lexer state, so advance after.
    switch (ch) {
        case '+':
            lexer_advance(lexer);
            return create_token(TOKEN_PLUS, "+", start_line, start_col);
        case '-':
            lexer_advance(lexer);
            return create_token(TOKEN_MINUS, "-", start_line, start_col);
        case '*':
            lexer_advance(lexer);
            return create_token(TOKEN_ASTERISK, "*", start_line, start_col);
        case '/':
            lexer_advance(lexer);
            return create_token(TOKEN_SLASH, "/", start_line, start_col);
        case '\\':
            lexer_advance(lexer);
            return create_token(TOKEN_BACKSLASH, "\\", start_line, start_col);
        case '^':
            lexer_advance(lexer);
            return create_token(TOKEN_CARET, "^", start_line, start_col);
        case '(':
            lexer_advance(lexer);
            return create_token(TOKEN_LPAREN, "(", start_line, start_col);
        case ')':
            lexer_advance(lexer);
            return create_token(TOKEN_RPAREN, ")", start_line, start_col);
        case ',':
            lexer_advance(lexer);
            return create_token(TOKEN_COMMA, ",", start_line, start_col);
        case ';':
            lexer_advance(lexer);
            return create_token(TOKEN_SEMICOLON, ";", start_line, start_col);
        case ':':
            lexer_advance(lexer);
            return create_token(TOKEN_COLON, ":", start_line, start_col);
        // Suffix characters, if they appear standalone, might be specific tokens or errors.
        // For now, let's assume they can be tokens if not part of other constructs.
        case '#':
            lexer_advance(lexer);
            return create_token(TOKEN_HASH, "#", start_line, start_col);
        case '!':
             lexer_advance(lexer);
            return create_token(TOKEN_BANG, "!", start_line, start_col);
        case '%':
             lexer_advance(lexer);
            return create_token(TOKEN_PERCENT, "%", start_line, start_col);
        case '$':
             lexer_advance(lexer);
            return create_token(TOKEN_DOLLAR, "$", start_line, start_col);

        case '=':
            lexer_advance(lexer);
            // No combined assignment like '==' in standard BASIC, just '='
            return create_token(TOKEN_EQUAL, "=", start_line, start_col);
        case '<':
            lexer_advance(lexer);
            if (lexer->current_char == '=') {
                lexer_advance(lexer);
                return create_token(TOKEN_LESS_EQUAL, "<=", start_line, start_col);
            } else if (lexer->current_char == '>') {
                lexer_advance(lexer);
                return create_token(TOKEN_NOT_EQUAL, "<>", start_line, start_col);
            }
            return create_token(TOKEN_LESS, "<", start_line, start_col);
        case '>':
            lexer_advance(lexer);
            if (lexer->current_char == '=') {
                lexer_advance(lexer);
                return create_token(TOKEN_GREATER_EQUAL, ">=", start_line, start_col);
            }
            return create_token(TOKEN_GREATER, ">", start_line, start_col);
    }

    // Fallback for unrecognized characters
    char lexeme[2] = {ch, '\0'};
    lexer_advance(lexer);
    BasicToken err_token = create_token(TOKEN_ERROR, lexeme, start_line, start_col);
    strncpy(err_token.literal_value.string_value, "Unrecognized character", MAX_LEXEME_LENGTH -1);
    return err_token;
}

// Implementation of other helper functions from header (make_token, make_number_token etc.)
// For now, using create_token and make_token_internal.
// These can be fleshed out if more specific construction logic is needed outside basic_lexer_next_token.

BasicToken make_token(BasicLexer *lexer, BasicTokenType type, const char *lexeme_override) {
    char temp_lexeme[MAX_LEXEME_LENGTH] = {0};
    size_t current_len = 0;
    if (lexeme_override) {
        strncpy(temp_lexeme, lexeme_override, MAX_LEXEME_LENGTH -1);
        current_len = strlen(temp_lexeme);
    } else if (!is_eof(lexer)){
        temp_lexeme[0] = lexer->current_char;
        temp_lexeme[1] = '\0';
        current_len = 1;
    }
    return make_token_internal(lexer, type, temp_lexeme, current_len, lexer->current_line_number, lexer->current_column_number);
}

BasicToken make_number_token(BasicLexer *lexer, double value, BasicVariableType num_type) {
    // This would typically be called after parsing the number string
    // For now, lexer_parse_number uses make_token_internal directly.
    char temp_lexeme[MAX_LEXEME_LENGTH];
    snprintf(temp_lexeme, MAX_LEXEME_LENGTH, "%g", value); // A bit simplistic for exact lexeme
    BasicToken token = make_token_internal(lexer, TOKEN_NUMBER, temp_lexeme, strlen(temp_lexeme), lexer->current_line_number, lexer->current_column_number);
    token.literal_value.number_value = value;
    token.var_type_suffix = num_type;
    return token;
}

BasicToken make_identifier_token(BasicLexer *lexer, const char *lexeme) {
    // For now, lexer_parse_identifier_or_keyword uses make_token_internal directly.
    BasicToken token = make_token_internal(lexer, TOKEN_IDENTIFIER, lexeme, strlen(lexeme), lexer->current_line_number, lexer->current_column_number);
    // Suffix detection would happen during parsing of identifier
    return token;
}

BasicToken make_string_token(BasicLexer *lexer, const char* value) {
    BasicToken token = make_token_internal(lexer, TOKEN_STRING_LITERAL, value, strlen(value), lexer->current_line_number, lexer->current_column_number);
    strncpy(token.literal_value.string_value, value, MAX_LEXEME_LENGTH -1);
    return token;
}

BasicToken make_error_token(BasicLexer *lexer, const char *message) {
    BasicToken token = make_token_internal(lexer, TOKEN_ERROR, message, strlen(message), lexer->current_line_number, lexer->current_column_number);
    // strncpy(token.literal_value.string_value, message, MAX_LEXEME_LENGTH-1); // Already done by create_token if message is lexeme
    return token;
}
