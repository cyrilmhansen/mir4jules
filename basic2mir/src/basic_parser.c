#include "basic_parser.h"
#include "basic_ast.h" // Assuming AST node creation functions are declared here
#include "basic_lexer.h" // Already included via basic_parser.h, but good for clarity

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h> // For va_list, va_start, va_end in parser_error

// AST Node creation functions are now in basic_ast.c and declared in basic_ast.h
// The append_to_list function is also now in basic_ast.c

// Parser Utility Functions
static void parser_error(BasicParser *parser, const char *message_format, ...) {
    parser->error_count++;
    char buffer[sizeof(parser->error_message)]; // Temporary buffer

    va_list args;
    va_start(args, message_format);
    vsnprintf(buffer, sizeof(buffer), message_format, args);
    va_end(args);

    snprintf(parser->error_message, sizeof(parser->error_message), "Parse Error (L%d:%d): %s. Current token: '%s' (Type: %d)",
             parser->current_token.line_number, parser->current_token.column_number,
             buffer, parser->current_token.lexeme, parser->current_token.type);

    // In a more robust parser, you might have mechanisms to decide whether to halt or try to recover.
    // For now, we'll just print it and expect the caller to decide.
    fprintf(stderr, "%s\n", parser->error_message);
}

static void parser_advance_token(BasicParser *parser) {
    if (parser->current_token.type == TOKEN_EOF) {
        // Avoid advancing past EOF. peek_token will also be EOF.
        return;
    }
    parser->current_token = parser->peek_token;
    parser->peek_token = basic_lexer_next_token(parser->lexer);
    if (parser->peek_token.type == TOKEN_ERROR) {
        parser_error(parser, "Lexical error: %s", parser->peek_token.lexeme);
        // Potentially advance past error token to allow parsing to continue, or halt.
        // For now, the error token will become current_token on next advance if not handled.
    }
}

// Consumes current_token if it matches 'type', otherwise reports error.
// Returns 1 on success, 0 on failure.
static int parser_eat(BasicParser *parser, BasicTokenType type, const char *error_message_on_fail) {
    if (parser->current_token.type == type) {
        parser_advance_token(parser);
        return 1;
    } else {
        if (error_message_on_fail) {
            parser_error(parser, "%s. Expected token type %d, got %d ('%s')",
                         error_message_on_fail, type, parser->current_token.type, parser->current_token.lexeme);
        } else {
            parser_error(parser, "Unexpected token. Expected token type %d, got %d ('%s')",
                         type, parser->current_token.type, parser->current_token.lexeme);
        }
        return 0;
    }
}


// Public Parser Functions
BasicParser *basic_parser_new(BasicLexer *lexer) {
    if (!lexer) return NULL;
    BasicParser *parser = (BasicParser *)calloc(1, sizeof(BasicParser));
    if (!parser) {
        perror("Failed to allocate BasicParser");
        return NULL;
    }
    parser->lexer = lexer;
    parser->error_count = 0;
    parser->error_message[0] = '\0';

    // Prime the tokens
    parser->current_token = basic_lexer_next_token(parser->lexer);
    parser->peek_token = basic_lexer_next_token(parser->lexer);

    // Handle initial lexical errors if any
    if (parser->current_token.type == TOKEN_ERROR) {
        parser_error(parser, "Initial lexical error: %s", parser->current_token.lexeme);
    }
     if (parser->peek_token.type == TOKEN_ERROR) {
        parser_error(parser, "Initial lexical error (peek): %s", parser->peek_token.lexeme);
    }

    return parser;
}

void basic_parser_free(BasicParser *parser) {
    if (parser) {
        // basic_lexer_free(parser->lexer); // Lexer lifetime managed externally
        free(parser);
    }
}

// Parsing functions (stubs for now, to be implemented)
static BasicAstNode *parse_factor(BasicParser *parser) {
    // Placeholder: Parses numbers and variables for now
    int32_t line = parser->current_token.line_number;
    BasicAstNode *node = NULL;

    if (parser->current_token.type == TOKEN_NUMBER) {
        BasicToken num_token = parser->current_token;
        parser_advance_token(parser);
        node = create_number_literal_node(line, num_token.literal_value.number_value, num_token.var_type_suffix);
    } else if (parser->current_token.type == TOKEN_STRING_LITERAL) {
        BasicToken str_token = parser->current_token;
        parser_advance_token(parser);
        // The lexer stores the raw string (quotes stripped, escapes handled) in lexeme or string_value
        node = create_string_literal_node(line, str_token.literal_value.string_value);
    } else if (parser->current_token.type == TOKEN_IDENTIFIER) {
        node = parse_variable_reference(parser);
    } else if (parser->current_token.type == TOKEN_LPAREN) {
        parser_advance_token(parser); // Consume '('
        node = parse_expression(parser); // Parse the inner expression
        if (!parser_eat(parser, TOKEN_RPAREN, "Expected ')' after parenthesized expression")) {
            // Error already reported by parser_eat. Cleanup node? Or let higher levels handle.
            // free_ast_node(node); // If we had such a function
            return NULL;
        }
    } else if (parser->current_token.type == TOKEN_MINUS) { // Unary minus
        parser_advance_token(parser); // Consume '-'
        BasicAstNode *operand = parse_factor(parser); // Parse the operand (could be another factor)
        if (!operand) {
            parser_error(parser, "Expected operand after unary minus.");
            return NULL;
        }
        node = create_unary_expr_node(line, "-", operand);
    }
    // TODO: Handle other unary operators like NOT if they are prefix
    else {
        parser_error(parser, "Unexpected token in factor: '%s'", parser->current_token.lexeme);
        return NULL;
    }
    return node;
}

static BasicAstNode *parse_term(BasicParser *parser) { // Handles *, /
    int32_t line = parser->current_token.line_number; // Line of the first factor
    BasicAstNode *node = parse_factor(parser);
    if (!node) return NULL;

    while (parser->current_token.type == TOKEN_ASTERISK || parser->current_token.type == TOKEN_SLASH) {
        BasicToken op_token = parser->current_token;
        parser_advance_token(parser);
        BasicAstNode *right = parse_factor(parser);
        if (!right) {
            parser_error(parser, "Expected factor after '%s' operator.", op_token.lexeme);
            // free_ast_node(node); // If error, free left side
            return NULL;
        }
        // Use line number of the operator for the binary expression node
        node = create_binary_expr_node(op_token.line_number, op_token.lexeme, node, right);
    }
    return node;
}

static BasicAstNode *parse_expression(BasicParser *parser) { // Handles +, - (binary)
    int32_t line = parser->current_token.line_number; // Line of the first term
    BasicAstNode *node = parse_term(parser);
    if (!node) return NULL;

    while (parser->current_token.type == TOKEN_PLUS || parser->current_token.type == TOKEN_MINUS) {
        BasicToken op_token = parser->current_token;
        parser_advance_token(parser);
        BasicAstNode *right = parse_term(parser);
        if (!right) {
            parser_error(parser, "Expected term after '%s' operator.", op_token.lexeme);
            // free_ast_node(node);
            return NULL;
        }
        node = create_binary_expr_node(op_token.line_number, op_token.lexeme, node, right);
    }
    // TODO: Add relational operators (>, <, =, <=, >=, <>) here or in a higher precedence function
    return node;
}


static BasicAstNode *parse_variable_reference(BasicParser *parser) {
    if (parser->current_token.type != TOKEN_IDENTIFIER) {
        parser_error(parser, "Expected identifier for variable.");
        return NULL;
    }
    BasicToken id_token = parser->current_token;
    parser_advance_token(parser); // Consume identifier

    BasicAstNode *dimensions_head = NULL;
    if (parser->current_token.type == TOKEN_LPAREN) {
        parser_advance_token(parser); // Consume '('

        // Parse comma-separated list of dimension expressions
        BasicAstNode *current_dim_expr = parse_expression(parser);
        if (!current_dim_expr) {
            parser_error(parser, "Expected expression for array dimension.");
            return NULL; // Or cleanup previously parsed dimensions
        }
        dimensions_head = current_dim_expr;

        while (parser->current_token.type == TOKEN_COMMA) {
            parser_advance_token(parser); // Consume ','
            BasicAstNode *next_dim_expr = parse_expression(parser);
            if (!next_dim_expr) {
                parser_error(parser, "Expected expression after comma in array dimensions.");
                // free_ast_list(dimensions_head); // Cleanup
                return NULL;
            }
            // Append to list (using generic 'next' pointer)
            current_dim_expr->next = next_dim_expr;
            current_dim_expr = next_dim_expr;
        }

        if (!parser_eat(parser, TOKEN_RPAREN, "Expected ')' after array dimensions")) {
            // free_ast_list(dimensions_head); // Cleanup
            return NULL;
        }
    }

    return create_variable_node(id_token.line_number, id_token.lexeme, id_token.var_type_suffix, dimensions_head);
}

static BasicAstNode *parse_rem_statement(BasicParser *parser) {
    if (parser->current_token.type != TOKEN_REM) {
        parser_error(parser, "Expected REM statement.");
        return NULL;
    }
    BasicToken rem_token = parser->current_token;
    parser_advance_token(parser); // Consume REM token

    // The lexer for REM includes the rest of the line in its lexeme.
    // The "comment" part starts after "REM ".
    const char *comment_body = rem_token.lexeme;
    if (strncasecmp(comment_body, "REM", 3) == 0) { // case insensitive check for "REM"
        comment_body += 3;
        if (*comment_body == ' ') comment_body++; // Skip space after REM
    }

    return create_rem_node(rem_token.line_number, comment_body);
}

static BasicAstNode *parse_end_statement(BasicParser *parser) {
    BasicAstNode* node = create_end_node(parser->current_token.line_number);
    parser_advance_token(parser); // Consume END
    return node;
}

static BasicAstNode *parse_let_statement(BasicParser *parser) {
    int32_t line = parser->current_token.line_number;
    BasicAstNode *var_node;

    if (parser->current_token.type == TOKEN_LET) {
        parser_advance_token(parser); // Consume LET
    }
    // Now expect a variable
    var_node = parse_variable_reference(parser);
    if (!var_node) {
        parser_error(parser, "Expected variable in LET statement.");
        return NULL;
    }

    if (!parser_eat(parser, TOKEN_EQUAL, "Expected '=' in LET statement")) return NULL;

    BasicAstNode *expr_node = parse_expression(parser);
    if (!expr_node) {
        parser_error(parser, "Expected expression in LET statement.");
        // Cleanup var_node? Or let overall cleanup handle it.
        return NULL;
    }
    return create_let_node(line, var_node, expr_node);
}

static BasicAstNode *parse_print_statement(BasicParser *parser) {
    int32_t line = parser->current_token.line_number;
    parser_advance_token(parser); // Consume PRINT token

    BasicAstNode *print_items_head = NULL;
    BasicAstNode *current_item = NULL;

    // Handle empty PRINT statement (prints a newline)
    if (parser->current_token.type == TOKEN_EOL || parser->current_token.type == TOKEN_EOF || parser->current_token.type == TOKEN_COLON) {
        return create_print_node(line, NULL);
    }

    do {
        // TODO: Handle special print formatters like SPC(), TAB() - these would be factors
        BasicAstNode *expr_node = parse_expression(parser);
        if (!expr_node) {
            parser_error(parser, "Expected expression or print item in PRINT statement.");
            // Cleanup list?
            return NULL;
        }
        // For now, each expression is a separate item. The 'next' pointer in BasicAstNode will link them.
        if (!print_items_head) {
            print_items_head = expr_node;
            current_item = expr_node;
        } else {
            current_item->next = expr_node; // Using generic 'next'
            current_item = expr_node;
        }

        // Check for print separators , or ;
        // These affect print formatting but for AST structure, we just list expressions.
        // We might want to store the separator type on the AST node if it's important for IR.
        if (parser->current_token.type == TOKEN_COMMA || parser->current_token.type == TOKEN_SEMICOLON) {
            // Optionally store this separator type in the AST node or a wrapper.
            // For now, just consume it.
            parser_advance_token(parser);
            // If a separator is followed by EOL/EOF, it means a trailing separator (affects cursor)
             if (parser->current_token.type == TOKEN_EOL || parser->current_token.type == TOKEN_EOF || parser->current_token.type == TOKEN_COLON) {
                break;
            }
        } else {
            break; // No more items if not separated by , or ;
        }
    } while (parser->current_token.type != TOKEN_EOL && parser->current_token.type != TOKEN_EOF && parser->current_token.type != TOKEN_COLON);

    return create_print_node(line, print_items_head);
}

static BasicAstNode *parse_goto_statement(BasicParser *parser) {
    int32_t line = parser->current_token.line_number;
    parser_advance_token(parser); // Consume GOTO

    if (parser->current_token.type != TOKEN_NUMBER) {
        parser_error(parser, "Expected line number after GOTO.");
        return NULL;
    }
    int32_t target_line = atoi(parser->current_token.lexeme); // Or use literal_value.number_value if lexer stores it as int
    parser_advance_token(parser); // Consume line number

    return create_goto_node(line, target_line);
}


static BasicAstNode *parse_statement(BasicParser *parser) {
    // Check for EOF before attempting to parse a statement, could happen with empty lines
    if (parser->current_token.type == TOKEN_EOF || parser->current_token.type == TOKEN_EOL) {
        return NULL; // No statement on an empty or fully consumed line
    }

    switch (parser->current_token.type) {
        case TOKEN_REM:
            return parse_rem_statement(parser);
        case TOKEN_END:
            return parse_end_statement(parser);
        case TOKEN_LET:
            return parse_let_statement(parser);
        case TOKEN_IDENTIFIER: // Implicit LET
            return parse_let_statement(parser);
        case TOKEN_PRINT:
            return parse_print_statement(parser);
        case TOKEN_GOTO:
            return parse_goto_statement(parser);
        case TOKEN_IF:
            return parse_if_statement(parser);
        case TOKEN_FOR:
            return parse_for_statement(parser);
        case TOKEN_NEXT:
            return parse_next_statement(parser);
        // TODO: Add cases for other statements (INPUT, DIM, GOSUB, RETURN, etc.)
        default:
            parser_error(parser, "Unexpected token at start of statement: %s", parser->current_token.lexeme);
            // Attempt to recover by advancing until EOL? For now, just return NULL.
            // To recover, one might skip tokens until TOKEN_EOL or TOKEN_EOF.
            // while(parser->current_token.type != TOKEN_EOL && parser->current_token.type != TOKEN_EOF) {
            //    parser_advance_token(parser);
            // }
            return NULL;
    }
}

static BasicAstNode *parse_line(BasicParser *parser) {
    if (parser->current_token.type != TOKEN_LINE_NUMBER) {
        // This could happen if the source doesn't start with a line number,
        // or after an error trying to find the next line.
        // For GW-BASIC, every line must have a number.
        if (parser->current_token.type != TOKEN_EOF) { // Don't error on clean EOF
           parser_error(parser, "Expected line number, got %s", parser->current_token.lexeme);
        }
        return NULL;
    }
    int32_t line_number = atoi(parser->current_token.literal_value.string_value); // Or use .line_num_value from lexer
    if (parser->current_token.type == TOKEN_LINE_NUMBER) { // ensure it's actually a line number token
         line_number = parser->current_token.literal_value.line_num_value;
    }

    parser_advance_token(parser); // Consume line number

    BasicAstNode *statement = NULL;
    // Check if the line is empty (just a line number followed by EOL/EOF)
    if (parser->current_token.type != TOKEN_EOL && parser->current_token.type != TOKEN_EOF) {
        statement = parse_statement(parser);
        // TODO: Handle multiple statements on a line separated by ':'
        // while (parser->current_token.type == TOKEN_COLON) {
        //    parser_advance_token(parser); // Consume ':'
        //    BasicAstNode *next_statement_on_line = parse_statement(parser);
        //    // Link statements on the same line if AST supports it, or flatten.
        // }
    }


    // After a statement (or if the line was empty after the number), expect EOL or EOF.
    if (parser->current_token.type != TOKEN_EOL && parser->current_token.type != TOKEN_EOF) {
        parser_error(parser, "Expected EOL or EOF after statement, got %s ('%s')",
            BasicTokenTypeToString(parser->current_token.type), // Assumes BasicTokenTypeToString exists
            parser->current_token.lexeme);
        // Try to recover by skipping to next EOL/EOF to find next line
        while(parser->current_token.type != TOKEN_EOL &&
              parser->current_token.type != TOKEN_EOF &&
              parser->current_token.type != TOKEN_LINE_NUMBER) { // Stop if we find a new line number
            parser_advance_token(parser);
            if(parser->current_token.type == TOKEN_EOF) break; // safety break
        }
    }

    if (parser->current_token.type == TOKEN_EOL) {
         parser_advance_token(parser); // Consume EOL
    }


    // If statement parsing failed but we found a line number, create a line node anyway.
    // Or, decide based on error strategy.
    if (statement || (parser->error_count == 0) ) { // Only create line if no error or statement was parsed
      return create_program_line_node(line_number, statement, NULL);
    }
    return NULL; // Error occurred and no statement parsed
}


// Forward declaration
static BasicAstNode *parse_single_statement_on_line(BasicParser *parser);

static BasicAstNode *parse_if_statement(BasicParser *parser) {
    int32_t line = parser->current_token.line_number;
    if (!parser_eat(parser, TOKEN_IF, "Expected IF token")) return NULL;

    BasicAstNode *condition = parse_expression(parser);
    if (!condition) {
        parser_error(parser, "Expected condition after IF.");
        return NULL;
    }

    if (!parser_eat(parser, TOKEN_THEN, "Expected THEN after IF condition.")) {
        // basic_ast_node_free_recursive(condition); // Clean up condition if THEN is missing
        return NULL;
    }

    BasicAstNode *then_branch = NULL;
    if (parser->current_token.type == TOKEN_NUMBER) {
        then_branch = create_goto_node(line, atoi(parser->current_token.lexeme));
        parser_advance_token(parser); // Consume line number
    } else {
        then_branch = parse_single_statement_on_line(parser);
        if (!then_branch) {
            parser_error(parser, "Expected statement or line number after THEN.");
            // basic_ast_node_free_recursive(condition);
            return NULL;
        }
    }

    BasicAstNode *else_branch = NULL;
    if (parser->current_token.type == TOKEN_ELSE) {
        parser_advance_token(parser); // Consume ELSE
        if (parser->current_token.type == TOKEN_NUMBER) {
            else_branch = create_goto_node(line, atoi(parser->current_token.lexeme));
            parser_advance_token(parser); // Consume line number
        } else {
            else_branch = parse_single_statement_on_line(parser);
            if (!else_branch) {
                parser_error(parser, "Expected statement or line number after ELSE.");
                // basic_ast_node_free_recursive(condition);
                // basic_ast_node_free_recursive(then_branch);
                return NULL;
            }
        }
    }

    return create_if_then_else_node(line, condition, then_branch, else_branch);
}


// Simplified: Parses one statement. Does not handle multiple statements separated by ':' on the same IF line yet.
static BasicAstNode *parse_single_statement_on_line(BasicParser *parser) {
    // This is a simplified version of the main parse_statement logic,
    // but without expecting a new line number.
    // It also should not consume EOL, as that's for the main parse_line.
    switch (parser->current_token.type) {
        case TOKEN_PRINT:
            return parse_print_statement(parser);
        case TOKEN_LET:
            return parse_let_statement(parser);
        case TOKEN_IDENTIFIER: // Implicit LET
            return parse_let_statement(parser);
        case TOKEN_GOTO:
            return parse_goto_statement(parser);
        case TOKEN_END:
            return parse_end_statement(parser);
        case TOKEN_REM: // REM after THEN/ELSE is unusual but parse it if lexer allows
            return parse_rem_statement(parser);
        // TODO: Add other simple, non-compound statements (e.g., GOSUB, RETURN)
        // More complex ones like FOR/NEXT on the same IF line would need more work or might be disallowed here.
        default:
            parser_error(parser, "Expected a simple statement after THEN/ELSE, got %s", parser->current_token.lexeme);
            return NULL;
    }
}


BasicAstNode *basic_parser_parse_program(BasicParser *parser) {
    BasicAstNode *program_node = create_program_node(0); // Line 0 for program itself
    // Program lines are appended using append_program_line.
    BasicAstNode *current_line_node = NULL;
    BasicAstNode *head_line_node = NULL;

    while (parser->current_token.type != TOKEN_EOF && parser->error_count < 10) { // Stop after too many errors
        BasicAstNode *line_node = parse_line(parser);
        if (line_node) {
            if (!head_line_node) {
                head_line_node = line_node;
                current_line_node = line_node;
            } else {
                current_line_node->data.program_line.next_line = line_node;
                current_line_node = line_node;
            }
        } else {
            // If parse_line returns NULL, it means either EOF or an error that prevented line creation.
            // If it's not EOF, we might be in an error state. Try to find next line or EOF.
            if (parser->current_token.type != TOKEN_EOF) {
                 // If not already at EOL or EOF, advance until we find a new line or end.
                if(parser->current_token.type != TOKEN_EOL) {
                    parser_error(parser, "Attempting to recover by finding next line.");
                    while(parser->current_token.type != TOKEN_EOL &&
                          parser->current_token.type != TOKEN_EOF &&
                          parser->current_token.type != TOKEN_LINE_NUMBER) { // Stop if we find a new line number
                        parser_advance_token(parser);
                         if(parser->current_token.type == TOKEN_EOF) break;
                    }
                }
                if (parser->current_token.type == TOKEN_EOL) { // Consume EOL to prep for next line number
                    parser_advance_token(parser);
                }
            }
        }
        if (parser->current_token.type == TOKEN_ERROR && parser->peek_token.type == TOKEN_EOF) {
            // If stuck on an error token at EOF, break.
            break;
        }
    }

    program_node->data.program.program_lines = head_line_node;
    if (parser->error_count > 0) {
        fprintf(stderr, "Parsing completed with %d error(s).\n", parser->error_count);
        // Depending on strategy, might return NULL or partially built AST
    }
    return program_node;
}

// Helper to convert token type to string (for error messages)
// This is often auto-generated or part of a utility library for tokens.
const char* BasicTokenTypeToString(BasicTokenType type) {
    switch(type) {
        case TOKEN_EOF: return "EOF";
        case TOKEN_EOL: return "EOL";
        case TOKEN_ERROR: return "ERROR";
        case TOKEN_NUMBER: return "NUMBER";
        case TOKEN_STRING_LITERAL: return "STRING_LITERAL";
        case TOKEN_LINE_NUMBER: return "LINE_NUMBER";
        case TOKEN_IDENTIFIER: return "IDENTIFIER";
        case TOKEN_LET: return "LET";
        case TOKEN_PRINT: return "PRINT";
        // ... add all token types
        default: return "UNKNOWN_TOKEN_TYPE";
    }
}


static BasicAstNode *parse_for_statement(BasicParser *parser) {
    int32_t line = parser->current_token.line_number;
    if (!parser_eat(parser, TOKEN_FOR, "Expected FOR token")) return NULL;

    BasicAstNode *loop_var = parse_variable_reference(parser);
    if (!loop_var) {
        parser_error(parser, "Expected loop variable after FOR.");
        return NULL;
    }
    // Ensure it's a simple variable, not an array element, for the counter
    if (loop_var->data.variable.dimensions != NULL) {
        parser_error(parser, "FOR loop counter variable cannot be an array element.");
        // basic_ast_node_free_recursive(loop_var); // If parse_variable_reference allocates and returns on error
        return NULL;
    }


    if (!parser_eat(parser, TOKEN_EQUAL, "Expected '=' after FOR loop variable.")) {
        // basic_ast_node_free_recursive(loop_var);
        return NULL;
    }

    BasicAstNode *start_value = parse_expression(parser);
    if (!start_value) {
        parser_error(parser, "Expected start value expression in FOR statement.");
        // basic_ast_node_free_recursive(loop_var);
        return NULL;
    }

    if (!parser_eat(parser, TOKEN_TO, "Expected TO in FOR statement.")) {
        // basic_ast_node_free_recursive(loop_var);
        // basic_ast_node_free_recursive(start_value);
        return NULL;
    }

    BasicAstNode *end_value = parse_expression(parser);
    if (!end_value) {
        parser_error(parser, "Expected end value expression in FOR statement.");
        // basic_ast_node_free_recursive(loop_var);
        // basic_ast_node_free_recursive(start_value);
        return NULL;
    }

    BasicAstNode *step_value = NULL;
    if (parser->current_token.type == TOKEN_STEP) {
        parser_advance_token(parser); // Consume STEP
        step_value = parse_expression(parser);
        if (!step_value) {
            parser_error(parser, "Expected step value expression after STEP.");
            // basic_ast_node_free_recursive(loop_var);
            // basic_ast_node_free_recursive(start_value);
            // basic_ast_node_free_recursive(end_value);
            return NULL;
        }
    }

    BasicAstNode *for_node = create_for_node(line, loop_var, start_value, end_value, step_value);
    // loop_check_label, loop_exit_label, next_node_ptr are initialized to NULL/0 by create_for_node (via calloc in create_ast_node)
    return for_node;
}

static BasicAstNode *parse_next_statement(BasicParser *parser) {
    int32_t line = parser->current_token.line_number;
    if (!parser_eat(parser, TOKEN_NEXT, "Expected NEXT token")) return NULL;

    BasicAstNode *loop_var = NULL;
    // GW-BASIC allows multiple variables for NEXT, e.g. NEXT I, J
    // For now, we parse only one optional variable.
    // The parser could be extended to create a list of variable nodes if multiple are present.
    if (parser->current_token.type == TOKEN_IDENTIFIER) {
        loop_var = parse_variable_reference(parser);
        // TODO: If supporting NEXT I, J - loop here for comma and more variables.
    }
    // If loop_var is NULL, it's a simple "NEXT" which matches the innermost FOR.

    BasicAstNode *next_node = create_next_node(line, loop_var);
    // for_node_ptr is initialized to NULL by create_next_node (via calloc in create_ast_node)
    return next_node;
}