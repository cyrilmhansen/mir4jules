#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h> // For fabs

#include "../../src/basic_lexer.h"
#include "../../src/basic_parser.h" // Will include basic_ast.h indirectly
#include "../../src/basic_ast.h"   // For direct access to BasicAstNode and enums

// Forward declaration for basic_token_type_to_string_test from test_lexer (or a shared utility)
// For now, let's duplicate it here for self-containment.
const char* basic_token_type_to_string_parser_test(BasicTokenType type) {
    switch(type) {
        case TOKEN_EOF: return "TOKEN_EOF";
        case TOKEN_EOL: return "TOKEN_EOL";
        case TOKEN_ERROR: return "TOKEN_ERROR";
        case TOKEN_LINE_NUMBER: return "TOKEN_LINE_NUMBER";
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
        case TOKEN_IDENTIFIER: return "TOKEN_IDENTIFIER";
        case TOKEN_NUMBER: return "TOKEN_NUMBER";
        case TOKEN_STRING_LITERAL: return "TOKEN_STRING_LITERAL";
        case TOKEN_PLUS: return "TOKEN_PLUS";
        case TOKEN_MINUS: return "TOKEN_MINUS";
        case TOKEN_ASTERISK: return "TOKEN_ASTERISK";
        case TOKEN_SLASH: return "TOKEN_SLASH";
        // Add other token types as needed by parser tests
        default: return "UNKNOWN_TOKEN_TYPE_PARSER";
    }
}

// Basic AST Node Comparison Helper (very simplified for now)
// Returns 1 if key fields match, 0 otherwise.
int compare_ast_nodes(BasicAstNode *actual, BasicAstNodeType expected_type, int32_t expected_line, const char* test_name, const char* detail) {
    if (!actual) {
        fprintf(stderr, "%s - %s: Actual AST node is NULL.\n", test_name, detail);
        return 0;
    }
    if (actual->type != expected_type) {
        fprintf(stderr, "%s - %s: Expected AST node type %d, got %d.\n", test_name, detail, expected_type, actual->type);
        return 0;
    }
    if (actual->line_number != expected_line && expected_line != -1) { // -1 to ignore line check if not relevant
        fprintf(stderr, "%s - %s: Expected AST node line number %d, got %d.\n", test_name, detail, expected_line, actual->line_number);
        return 0;
    }
    return 1;
}


int test_parse_simple_let(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_simple_let";
    int pass = 1;
    const char *source = "10 LET A = 123\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed or produced NULL AST. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        // Program -> LineNode(10) -> LetNode -> VariableNode("A"), NumberLiteralNode(123)
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        pass &= compare_ast_nodes(line_node, AST_NODE_TYPE_PROGRAM_LINE, 10, test_name, "LineNode");
        if (pass && line_node) {
            BasicAstNode *let_node = line_node->data.program_line.statement;
            pass &= compare_ast_nodes(let_node, AST_NODE_TYPE_LET, 10, test_name, "LetNode");
            if (pass && let_node) {
                BasicAstNode *var_node = let_node->data.let_stmt.variable;
                pass &= compare_ast_nodes(var_node, AST_NODE_TYPE_VARIABLE, 10, test_name, "VariableNode");
                if (pass && var_node && strcmp(var_node->data.variable.name, "A") != 0) {
                    fprintf(stderr, "%s: Expected var name 'A', got '%s'.\n", test_name, var_node->data.variable.name);
                    pass = 0;
                }

                BasicAstNode *num_node = let_node->data.let_stmt.expression;
                pass &= compare_ast_nodes(num_node, AST_NODE_TYPE_NUMBER_LITERAL, 10, test_name, "NumberLiteralNode");
                if (pass && num_node && fabs(num_node->data.number_literal.value - 123.0) > 1e-6) {
                    fprintf(stderr, "%s: Expected num value 123, got %f.\n", test_name, num_node->data.number_literal.value);
                    pass = 0;
                }
            }
        }
    }

    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_implicit_let(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_implicit_let";
    int pass = 1;
    const char *source = "20 B$ = \"HELLO\"\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        pass &= compare_ast_nodes(line_node, AST_NODE_TYPE_PROGRAM_LINE, 20, test_name, "LineNode");
        if (pass && line_node) {
            BasicAstNode *let_node = line_node->data.program_line.statement;
            pass &= compare_ast_nodes(let_node, AST_NODE_TYPE_LET, 20, test_name, "LetNode (Implicit)");
             if (pass && let_node) {
                BasicAstNode *var_node = let_node->data.let_stmt.variable;
                pass &= compare_ast_nodes(var_node, AST_NODE_TYPE_VARIABLE, 20, test_name, "VariableNode B$");
                if (pass && var_node) {
                    if (strcmp(var_node->data.variable.name, "B$") != 0) {
                        fprintf(stderr, "%s: Expected var name 'B$', got '%s'.\n", test_name, var_node->data.variable.name);
                        pass = 0;
                    }
                    // Type suffix check implicitly done by lexer, parser should preserve it.
                    // Semantic analyzer would confirm type.
                }

                BasicAstNode *str_node = let_node->data.let_stmt.expression;
                pass &= compare_ast_nodes(str_node, AST_NODE_TYPE_STRING_LITERAL, 20, test_name, "StringLiteralNode");
                 if (pass && str_node && strcmp(str_node->data.string_literal.value, "HELLO") != 0) {
                    fprintf(stderr, "%s: Expected string value 'HELLO', got '%s'.\n", test_name, str_node->data.string_literal.value);
                    pass = 0;
                }
            }
        }
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_print_statement(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_print_statement";
    int pass = 1;
    const char *source = "30 PRINT \"AGE:\", AGE%\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        pass &= compare_ast_nodes(line_node, AST_NODE_TYPE_PROGRAM_LINE, 30, test_name, "LineNode");
        if (pass && line_node) {
            BasicAstNode *print_node = line_node->data.program_line.statement;
            pass &= compare_ast_nodes(print_node, AST_NODE_TYPE_PRINT, 30, test_name, "PrintNode");
            if (pass && print_node) {
                BasicAstNode *item1 = print_node->data.print_stmt.print_items;
                pass &= compare_ast_nodes(item1, AST_NODE_TYPE_STRING_LITERAL, 30, test_name, "PrintItem1 String");
                if(pass && item1 && strcmp(item1->data.string_literal.value, "AGE:") != 0) {pass=0;}

                if(pass && item1) {
                    BasicAstNode *item2 = item1->next;
                    pass &= compare_ast_nodes(item2, AST_NODE_TYPE_VARIABLE, 30, test_name, "PrintItem2 Variable");
                     if(pass && item2 && strcmp(item2->data.variable.name, "AGE%") != 0) {pass=0;}
                     if(pass && item2 && item2->next != NULL) {
                        fprintf(stderr, "%s: Too many print items.\n", test_name);
                        pass = 0;
                     }
                } else if (pass) {
                     fprintf(stderr, "%s: Missing second print item.\n", test_name);
                     pass = 0;
                }
            }
        }
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}


int test_parse_goto(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_goto";
    int pass = 1;
    const char *source = "40 GOTO 100\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        pass &= compare_ast_nodes(line_node, AST_NODE_TYPE_PROGRAM_LINE, 40, test_name, "LineNode");
        if (pass && line_node) {
            BasicAstNode *goto_node = line_node->data.program_line.statement;
            pass &= compare_ast_nodes(goto_node, AST_NODE_TYPE_GOTO, 40, test_name, "GotoNode");
            if (pass && goto_node && goto_node->data.goto_stmt.target_line_number != 100) {
                 fprintf(stderr, "%s: Expected GOTO target 100, got %d.\n", test_name, goto_node->data.goto_stmt.target_line_number);
                 pass = 0;
            }
        }
    }

    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_rem_and_end(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_rem_and_end";
    int pass = 1;
    const char *source = "50 REM COMMENT\n60 END\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line50 = program_ast->data.program.program_lines;
        pass &= compare_ast_nodes(line50, AST_NODE_TYPE_PROGRAM_LINE, 50, test_name, "Line50");
        if(pass && line50) {
            BasicAstNode *rem_node = line50->data.program_line.statement;
            pass &= compare_ast_nodes(rem_node, AST_NODE_TYPE_REM, 50, test_name, "RemNode");
            if(pass && rem_node && strcmp(rem_node->data.rem_stmt.comment, "COMMENT") != 0) { // Lexer captures "COMMENT" after "REM "
                fprintf(stderr, "%s: Expected REM comment 'COMMENT', got '%s'.\n", test_name, rem_node->data.rem_stmt.comment);
                pass = 0;
            }

            BasicAstNode *line60 = line50->data.program_line.next_line;
            pass &= compare_ast_nodes(line60, AST_NODE_TYPE_PROGRAM_LINE, 60, test_name, "Line60");
            if(pass && line60) {
                BasicAstNode *end_node = line60->data.program_line.statement;
                pass &= compare_ast_nodes(end_node, AST_NODE_TYPE_END, 60, test_name, "EndNode");
            }
        }
    }

    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_expression_precedence(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_expression_precedence";
    int pass = 1;
    const char *source = "10 LET A = 1 + 2 * 3\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        BasicAstNode *let_node = line_node ? line_node->data.program_line.statement : NULL;
        BasicAstNode *expr_node = let_node ? let_node->data.let_stmt.expression : NULL;

        pass &= compare_ast_nodes(expr_node, AST_NODE_TYPE_BINARY_EXPR, 10, test_name, "Root Expr (+)");
        if (pass && expr_node) {
            if (strcmp(expr_node->data.binary_expr.operator, "+") != 0) {
                fprintf(stderr, "%s: Expected root op '+', got '%s'.\n", test_name, expr_node->data.binary_expr.operator);
                pass = 0;
            }
            // Left of + should be Num(1)
            BasicAstNode *left_plus = expr_node->data.binary_expr.left;
            pass &= compare_ast_nodes(left_plus, AST_NODE_TYPE_NUMBER_LITERAL, 10, test_name, "Left of +");
            if (pass && left_plus && fabs(left_plus->data.number_literal.value - 1.0) > 1e-6) pass = 0;

            // Right of + should be BinaryOp(*, Num(2), Num(3))
            BasicAstNode *right_plus = expr_node->data.binary_expr.right;
            pass &= compare_ast_nodes(right_plus, AST_NODE_TYPE_BINARY_EXPR, 10, test_name, "Right of + (*)");
             if (pass && right_plus) {
                 if (strcmp(right_plus->data.binary_expr.operator, "*") != 0) {
                    fprintf(stderr, "%s: Expected mult op '*', got '%s'.\n", test_name, right_plus->data.binary_expr.operator);
                    pass = 0;
                }
                BasicAstNode *left_mul = right_plus->data.binary_expr.left;
                pass &= compare_ast_nodes(left_mul, AST_NODE_TYPE_NUMBER_LITERAL, 10, test_name, "Left of *");
                if (pass && left_mul && fabs(left_mul->data.number_literal.value - 2.0) > 1e-6) pass = 0;

                BasicAstNode *right_mul = right_plus->data.binary_expr.right;
                pass &= compare_ast_nodes(right_mul, AST_NODE_TYPE_NUMBER_LITERAL, 10, test_name, "Right of *");
                if (pass && right_mul && fabs(right_mul->data.number_literal.value - 3.0) > 1e-6) pass = 0;
            }
        }
    }

    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}


int main() {
    int tests_passed = 0;
    int tests_failed = 0;

    test_parse_simple_let(&tests_passed, &tests_failed);
    test_parse_implicit_let(&tests_passed, &tests_failed);
    test_parse_print_statement(&tests_passed, &tests_failed);
    test_parse_goto(&tests_passed, &tests_failed);
    test_parse_rem_and_end(&tests_passed, &tests_failed);
    test_parse_expression_precedence(&tests_passed, &tests_failed);

    // New IF tests
    test_parse_if_then_statement(&tests_passed, &tests_failed);
    test_parse_if_then_goto_line(&tests_passed, &tests_failed);
    test_parse_if_then_else_statement(&tests_passed, &tests_failed);
    test_parse_if_then_else_line(&tests_passed, &tests_failed);

    // New FOR/NEXT tests
    test_parse_for_next_simple(&tests_passed, &tests_failed);
    test_parse_for_next_with_step(&tests_passed, &tests_failed);
    test_parse_next_no_var(&tests_passed, &tests_failed);

    fprintf(stdout, "\nParser Test Summary: %d passed, %d failed.\n", tests_passed, tests_failed);
    return (tests_failed == 0) ? 0 : 1;
}


int test_parse_if_then_statement(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_if_then_statement";
    int pass = 1;
    const char *source = "10 IF A > 10 THEN PRINT \"GREATER\"\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        pass &= compare_ast_nodes(line_node, AST_NODE_TYPE_PROGRAM_LINE, 10, test_name, "LineNode");
        if (pass && line_node) {
            BasicAstNode *if_node = line_node->data.program_line.statement;
            pass &= compare_ast_nodes(if_node, AST_NODE_TYPE_IF_THEN_ELSE, 10, test_name, "IfNode");
            if (pass && if_node) {
                // Condition: A > 10
                BasicAstNode *cond_node = if_node->data.if_then_else_stmt.condition;
                pass &= compare_ast_nodes(cond_node, AST_NODE_TYPE_BINARY_EXPR, 10, test_name, "Condition BinaryOp");
                if(pass && cond_node && strcmp(cond_node->data.binary_expr.operator, ">") != 0) pass = 0;

                // Then branch: PRINT "GREATER"
                BasicAstNode *then_branch = if_node->data.if_then_else_stmt.then_branch;
                pass &= compare_ast_nodes(then_branch, AST_NODE_TYPE_PRINT, 10, test_name, "Then Branch PrintNode");
                if(pass && then_branch && then_branch->data.print_stmt.print_items &&
                   strcmp(then_branch->data.print_stmt.print_items->data.string_literal.value, "GREATER") != 0) pass = 0;

                if (if_node->data.if_then_else_stmt.else_branch != NULL) {
                    fprintf(stderr, "%s: Expected NULL else_branch, got a node.\n", test_name);
                    pass = 0;
                }
            }
        }
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_if_then_goto_line(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_if_then_goto_line";
    int pass = 1;
    const char *source = "20 IF X = 0 THEN 100\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        pass &= compare_ast_nodes(line_node, AST_NODE_TYPE_PROGRAM_LINE, 20, test_name, "LineNode");
        if (pass && line_node) {
            BasicAstNode *if_node = line_node->data.program_line.statement;
            pass &= compare_ast_nodes(if_node, AST_NODE_TYPE_IF_THEN_ELSE, 20, test_name, "IfNode");
            if (pass && if_node) {
                BasicAstNode *then_branch = if_node->data.if_then_else_stmt.then_branch;
                pass &= compare_ast_nodes(then_branch, AST_NODE_TYPE_GOTO, 20, test_name, "Then Branch GotoNode");
                if(pass && then_branch && then_branch->data.goto_stmt.target_line_number != 100) pass = 0;
                if (if_node->data.if_then_else_stmt.else_branch != NULL) pass = 0;
            }
        }
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_if_then_else_statement(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_if_then_else_statement";
    int pass = 1;
    const char *source = "30 IF Y < 5 THEN PRINT \"Y\" ELSE PRINT \"Z\"\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        BasicAstNode *if_node = line_node ? line_node->data.program_line.statement : NULL;
        pass &= compare_ast_nodes(if_node, AST_NODE_TYPE_IF_THEN_ELSE, 30, test_name, "IfNode");
        if (pass && if_node) {
            BasicAstNode *then_b = if_node->data.if_then_else_stmt.then_branch;
            BasicAstNode *else_b = if_node->data.if_then_else_stmt.else_branch;
            pass &= compare_ast_nodes(then_b, AST_NODE_TYPE_PRINT, 30, test_name, "Then Print");
            if(pass && then_b && then_b->data.print_stmt.print_items &&
               strcmp(then_b->data.print_stmt.print_items->data.string_literal.value, "Y") != 0) pass = 0;

            pass &= compare_ast_nodes(else_b, AST_NODE_TYPE_PRINT, 30, test_name, "Else Print");
            if(pass && else_b && else_b->data.print_stmt.print_items &&
               strcmp(else_b->data.print_stmt.print_items->data.string_literal.value, "Z") != 0) pass = 0;
        }
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_if_then_else_line(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_if_then_else_line";
    int pass = 1;
    const char *source = "40 IF K THEN 200 ELSE 300\n";
     BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line_node = program_ast->data.program.program_lines;
        BasicAstNode *if_node = line_node ? line_node->data.program_line.statement : NULL;
        pass &= compare_ast_nodes(if_node, AST_NODE_TYPE_IF_THEN_ELSE, 40, test_name, "IfNode");
        if(pass && if_node){
            BasicAstNode *then_b = if_node->data.if_then_else_stmt.then_branch;
            BasicAstNode *else_b = if_node->data.if_then_else_stmt.else_branch;
            pass &= compare_ast_nodes(then_b, AST_NODE_TYPE_GOTO, 40, test_name, "Then GOTO");
            if(pass && then_b && then_b->data.goto_stmt.target_line_number != 200) pass = 0;
            pass &= compare_ast_nodes(else_b, AST_NODE_TYPE_GOTO, 40, test_name, "Else GOTO");
            if(pass && else_b && else_b->data.goto_stmt.target_line_number != 300) pass = 0;
        }
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_for_next_simple(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_for_next_simple";
    int pass = 1;
    const char *source = "10 FOR I = 1 TO 10\n20 NEXT I\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line10 = program_ast->data.program.program_lines;
        BasicAstNode *for_node = line10 ? line10->data.program_line.statement : NULL;
        pass &= compare_ast_nodes(for_node, AST_NODE_TYPE_FOR, 10, test_name, "ForNode");
        if(pass && for_node) {
            if(strcmp(for_node->data.for_stmt.loop_variable->data.variable.name, "I")!=0) pass=0;
            if(fabs(for_node->data.for_stmt.start_value->data.number_literal.value - 1.0) > 1e-6) pass=0;
            if(fabs(for_node->data.for_stmt.end_value->data.number_literal.value - 10.0) > 1e-6) pass=0;
            if(for_node->data.for_stmt.step_value != NULL) pass=0;
        }
        BasicAstNode *line20 = line10 ? line10->data.program_line.next_line : NULL;
        BasicAstNode *next_node = line20 ? line20->data.program_line.statement : NULL;
        pass &= compare_ast_nodes(next_node, AST_NODE_TYPE_NEXT, 20, test_name, "NextNode");
        if(pass && next_node && strcmp(next_node->data.next_stmt.loop_variable->data.variable.name, "I")!=0) pass=0;
        // Semantic analysis will link for_node_ptr and next_node_ptr
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_for_next_with_step(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_for_next_with_step";
    int pass = 1;
    const char *source = "30 FOR J% = 100 TO 1 STEP -2\n40 NEXT J%\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line30 = program_ast->data.program.program_lines;
        BasicAstNode *for_node = line30 ? line30->data.program_line.statement : NULL;
        pass &= compare_ast_nodes(for_node, AST_NODE_TYPE_FOR, 30, test_name, "ForNode");
        if(pass && for_node) {
            if(strcmp(for_node->data.for_stmt.loop_variable->data.variable.name, "J%")!=0) pass=0;
            if(fabs(for_node->data.for_stmt.start_value->data.number_literal.value - 100.0) > 1e-6) pass=0;
            if(fabs(for_node->data.for_stmt.end_value->data.number_literal.value - 1.0) > 1e-6) pass=0;
            BasicAstNode* step_node = for_node->data.for_stmt.step_value;
            pass &= compare_ast_nodes(step_node, AST_NODE_TYPE_UNARY_EXPR, 30, test_name, "Step UnaryOp");
            if(pass && step_node && strcmp(step_node->data.unary_expr.operator, "-")!=0) pass=0;
            if(pass && step_node && fabs(step_node->data.unary_expr.operand->data.number_literal.value - 2.0) > 1e-6) pass=0;
        }
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_parse_next_no_var(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_parse_next_no_var";
    int pass = 1;
    const char *source = "50 FOR K = 0 TO 1\n60 NEXT\n";
    BasicLexer *lexer = basic_lexer_new(source);
    BasicParser *parser = basic_parser_new(lexer);
    BasicAstNode *program_ast = basic_parser_parse_program(parser);

    if (parser->error_count > 0 || !program_ast) {
        fprintf(stderr, "%s: Parsing failed. Errors: %d\n", test_name, parser->error_count);
        pass = 0;
    } else {
        BasicAstNode *line60 = program_ast->data.program.program_lines ? program_ast->data.program.program_lines->data.program_line.next_line : NULL;
        BasicAstNode *next_node = line60 ? line60->data.program_line.statement : NULL;
        pass &= compare_ast_nodes(next_node, AST_NODE_TYPE_NEXT, 60, test_name, "NextNode");
        if(pass && next_node && next_node->data.next_stmt.loop_variable != NULL) {
            fprintf(stderr, "%s: Expected NULL loop_variable for NEXT, got a node.\n", test_name);
            pass = 0;
        }
    }
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}
