#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../../src/basic_lexer.h"
#include "../../src/basic_parser.h"
#include "../../src/basic_ast.h"
#include "../../src/basic_semantic.h"

// Helper to build AST from source string for testing
BasicAstNode* build_ast_from_string(const char* source, int *parser_error_count) {
    BasicLexer *lexer = basic_lexer_new(source);
    if (!lexer) {
        fprintf(stderr, "Test AST Build: Lexer creation failed.\n");
        return NULL;
    }
    BasicParser *parser = basic_parser_new(lexer);
    if (!parser) {
        fprintf(stderr, "Test AST Build: Parser creation failed.\n");
        basic_lexer_free(lexer);
        return NULL;
    }
    BasicAstNode *program_ast = basic_parser_parse_program(parser);
    if (parser_error_count) {
        *parser_error_count = parser->error_count;
    }
    if (parser->error_count > 0) {
         fprintf(stderr, "Test AST Build: Parsing generated %d errors.\n", parser->error_count);
    }

    // Important: The semantic analyzer expects the parser (and lexer) to be alive
    // if it needs to access token details not fully copied to AST, or for error reporting context.
    // However, for these tests, we build AST then free lexer/parser before semantic analysis.
    // This is okay if semantic analysis only relies on the fully-formed AST.
    // If semantic analyzer needs lexer/parser, test structure must change.
    basic_parser_free(parser); // Parser does not free lexer
    basic_lexer_free(lexer);   // Free lexer separately

    return program_ast;
}

int test_semantic_variable_declaration_and_type(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_semantic_variable_declaration_and_type";
    int pass = 1;
    const char *source = "10 A = 10\n"
                         "20 B$ = \"S\"\n"
                         "30 C% = 1\n"
                         "40 D! = 1.0\n"
                         "50 E# = 2.0\n";
    int parse_errors = 0;
    BasicAstNode *ast = build_ast_from_string(source, &parse_errors);
    if (!ast || parse_errors > 0) {
        fprintf(stderr, "%s: Failed to build AST or parsing errors occurred.\n", test_name);
        if(ast) basic_ast_node_free_recursive(ast);
        (*tests_failed)++;
        return 0;
    }

    SemanticAnalyzer *analyzer = semantic_analyzer_new();
    int semantic_errors = semantic_analyzer_analyze(analyzer, ast);

    if (semantic_errors > 0) {
        fprintf(stderr, "%s: Semantic analysis reported %d errors unexpectedly.\n", test_name, semantic_errors);
        pass = 0;
    } else {
        Symbol *s_a = symbol_table_get(analyzer->symbol_table, "A");
        Symbol *s_bs = symbol_table_get(analyzer->symbol_table, "B$");
        Symbol *s_cp = symbol_table_get(analyzer->symbol_table, "C%");
        Symbol *s_ds = symbol_table_get(analyzer->symbol_table, "D!");
        Symbol *s_ed = symbol_table_get(analyzer->symbol_table, "E#");

        if (!s_a || s_a->type != BASIC_VAR_TYPE_SINGLE) { fprintf(stderr, "%s: Var A type mismatch or not found. Expected SINGLE, got %d\n", test_name, s_a ? s_a->type: -1); pass = 0; }
        if (!s_bs || s_bs->type != BASIC_VAR_TYPE_STRING) { fprintf(stderr, "%s: Var B$ type mismatch or not found.\n", test_name); pass = 0; }
        if (!s_cp || s_cp->type != BASIC_VAR_TYPE_INTEGER) { fprintf(stderr, "%s: Var C%% type mismatch or not found.\n", test_name); pass = 0; }
        if (!s_ds || s_ds->type != BASIC_VAR_TYPE_SINGLE) { fprintf(stderr, "%s: Var D! type mismatch or not found.\n", test_name); pass = 0; }
        if (!s_ed || s_ed->type != BASIC_VAR_TYPE_DOUBLE) { fprintf(stderr, "%s: Var E# type mismatch or not found.\n", test_name); pass = 0; }
    }

    semantic_analyzer_free(analyzer);
    basic_ast_node_free_recursive(ast);

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_semantic_let_type_checking(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_semantic_let_type_checking";
    int pass = 1;
    const char* sources[] = {
        "10 A = \"HELLO\"", // Error: type mismatch
        "20 B$ = 123",    // Error: type mismatch
        "30 C% = \"TEST\"", // Error
        "40 D! = \"FLOAT\"",// Error
        "50 A = 10\n60 A = \"NO\"", // Error on line 60
        "70 X = 1\n80 X% = X", // OK (or specific int assignment)
        "90 Y$ = \"ok\"\n100 Z$ = Y$" // OK
    };
    int expected_error_counts[] = {1, 1, 1, 1, 1, 0, 0};
    int num_tests = sizeof(sources) / sizeof(sources[0]);

    for (int i = 0; i < num_tests; ++i) {
        int tc_pass = 1;
        int parse_errors = 0;
        BasicAstNode *ast = build_ast_from_string(sources[i], &parse_errors);
        if (!ast || parse_errors > 0) {
            fprintf(stderr, "%s (Case %d): Failed to build AST or parsing errors.\n", test_name, i);
            if(ast) basic_ast_node_free_recursive(ast);
            tc_pass = 0;
            pass = 0;
            continue;
        }
        SemanticAnalyzer *analyzer = semantic_analyzer_new();
        int semantic_errors = semantic_analyzer_analyze(analyzer, ast);

        if (semantic_errors != expected_error_counts[i]) {
            fprintf(stderr, "%s (Case %d - Source: %s): Expected %d semantic errors, got %d.\n",
                    test_name, i, sources[i], expected_error_counts[i], semantic_errors);
            tc_pass = 0;
        }

        semantic_analyzer_free(analyzer);
        basic_ast_node_free_recursive(ast);
        if (!tc_pass) pass = 0;
    }

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_semantic_goto_validation(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_semantic_goto_validation";
    int pass = 1;
    const char* sources[] = {
        "10 GOTO 100\n20 PRINT \"HELLO\"", // Error: undefined line 100
        "10 GOTO 20\n20 PRINT \"OK\""     // OK
    };
    int expected_error_counts[] = {1, 0};
    int num_tests = sizeof(sources) / sizeof(sources[0]);

    for (int i = 0; i < num_tests; ++i) {
        int tc_pass = 1;
        int parse_errors = 0;
        BasicAstNode *ast = build_ast_from_string(sources[i], &parse_errors);
         if (!ast || parse_errors > 0) {
            fprintf(stderr, "%s (Case %d): Failed to build AST or parsing errors.\n", test_name, i);
            if(ast) basic_ast_node_free_recursive(ast);
            tc_pass = 0;
            pass = 0;
            continue;
        }
        SemanticAnalyzer *analyzer = semantic_analyzer_new();
        int semantic_errors = semantic_analyzer_analyze(analyzer, ast);

        if (semantic_errors != expected_error_counts[i]) {
            fprintf(stderr, "%s (Case %d - Source: %s): Expected %d semantic errors, got %d.\n",
                    test_name, i, sources[i], expected_error_counts[i], semantic_errors);
            tc_pass = 0;
        }

        semantic_analyzer_free(analyzer);
        basic_ast_node_free_recursive(ast);
        if(!tc_pass) pass = 0;
    }
    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

// test_expression_type_determination would need more access to internal AST states
// or specific functions to retrieve expression types post-analysis.
// For now, type determination is implicitly tested via LET statement checks.

int main() {
    int tests_passed = 0;
    int tests_failed = 0;

    test_semantic_variable_declaration_and_type(&tests_passed, &tests_failed);
    test_semantic_let_type_checking(&tests_passed, &tests_failed);
    test_semantic_goto_validation(&tests_passed, &tests_failed);
    test_semantic_if_statement(&tests_passed, &tests_failed);
    test_semantic_for_next_matching_and_types(&tests_passed, &tests_failed);

    fprintf(stdout, "\nSemantic Analyzer Test Summary: %d passed, %d failed.\n", tests_passed, tests_failed);
    return (tests_failed == 0) ? 0 : 1;
}


int test_semantic_if_statement(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_semantic_if_statement";
    int pass = 1;
    const char* sources[] = {
        "10 X=1\n20 IF X THEN PRINT \"OK\"",                                       // OK
        "30 Y$=\"Y\"\n40 IF Y$ THEN PRINT \"FAIL\"",                                 // Error: condition is string
        "50 IF 1 THEN 100",                                                      // Error: GOTO target 100 undefined
        "60 Z=0\n70 IF Z THEN 80 ELSE 90\n80 PRINT \"T\"\n90 PRINT \"F\"",          // OK
        "100 IF A > B THEN 110 ELSE PRINT \"A<=B\"\n110 PRINT \"A>B\""           // OK
    };
    int expected_error_counts[] = {0, 1, 1, 0, 0}; // Expected semantic errors for each case
    int num_tests = sizeof(sources) / sizeof(sources[0]);

    for (int i = 0; i < num_tests; ++i) {
        int tc_pass = 1;
        int parse_errors = 0;
        BasicAstNode *ast = build_ast_from_string(sources[i], &parse_errors);
        if (!ast || parse_errors > 0) {
            fprintf(stderr, "%s (Case %d): Failed to build AST or parsing errors occurred (parse_errors=%d).\n", test_name, i, parse_errors);
            if(ast) basic_ast_node_free_recursive(ast);
            tc_pass = 0; // Mark this test case as failed
        } else {
            SemanticAnalyzer *analyzer = semantic_analyzer_new();
            int semantic_errors = semantic_analyzer_analyze(analyzer, ast);

            if (semantic_errors != expected_error_counts[i]) {
                fprintf(stderr, "%s (Case %d - Source: \"%s\"): Expected %d semantic errors, got %d.\n",
                        test_name, i, sources[i], expected_error_counts[i], semantic_errors);
                if (semantic_errors > 0 && expected_error_counts[i] == 0) { // Print last error if unexpected
                     fprintf(stderr, "    Last error: %s\n", analyzer->last_error_message);
                }
                tc_pass = 0;
            }
            semantic_analyzer_free(analyzer);
            basic_ast_node_free_recursive(ast);
        }
        if (!tc_pass) pass = 0; // If any test case fails, the whole test function fails
    }

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}

int test_semantic_for_next_matching_and_types(int *tests_passed, int *tests_failed) {
    const char *test_name = "test_semantic_for_next_matching_and_types";
    int pass = 1;
    const char* sources[] = {
        "10 FOR I = 1 TO 5\n20 PRINT I\n30 NEXT I",                             // OK
        "10 FOR I = 1 TO 5\n20 NEXT J",                                         // Error: NEXT variable mismatch
        "10 FOR I = 1 TO 5",                                                    // Error: FOR without NEXT
        "10 NEXT I",                                                            // Error: NEXT without FOR
        "10 FOR I=1 TO 10\n20 FOR J=1 TO 5\n30 NEXT I",                          // Error: NEXT I (expected J), also FOR J without NEXT
        "10 FOR I$ = 1 TO 10\n20 NEXT I$",                                      // Error: Loop variable I$ is string
        "10 FOR I = \"1\" TO \"10\"\n20 NEXT I",                                  // Error: Start/end values are strings
        "10 FOR I = 1 TO 10 STEP \"1\"\n20 NEXT I",                             // Error: Step value is string
        "10 FOR I = 1 TO 10\n20 FOR I = 1 TO 5\n30 NEXT I\n40 NEXT I"             // OK: Nested loops with same var name
    };
    // Expected errors: Mismatch, ForNoNext, NextNoFor, ForJNoNext(after I pop)/NextIMismatch, StrLoopVar, StrStartEnd, StrStep, OK
    int expected_error_counts[] = {0, 1, 1, 1, 2, 1, 2, 1, 0};
    // Note: "10 FOR I=1 TO 10\n20 FOR J=1 TO 5\n30 NEXT I" typically results in:
    // 1. NEXT I does not match FOR J.
    // 2. FOR J has no NEXT J (at end of program).
    int num_tests = sizeof(sources) / sizeof(sources[0]);

    for (int i = 0; i < num_tests; ++i) {
        int tc_pass = 1;
        int parse_errors = 0;
        BasicAstNode *ast = build_ast_from_string(sources[i], &parse_errors);
        if (!ast || parse_errors > 0) {
            fprintf(stderr, "%s (Case %d): Failed to build AST or parsing errors (parse_errors=%d).\n", test_name, i, parse_errors);
            if(ast) basic_ast_node_free_recursive(ast);
            tc_pass = 0;
        } else {
            SemanticAnalyzer *analyzer = semantic_analyzer_new();
            int semantic_errors = semantic_analyzer_analyze(analyzer, ast);

            if (semantic_errors != expected_error_counts[i]) {
                fprintf(stderr, "%s (Case %d - Source: \"%s\"): Expected %d semantic errors, got %d.\n",
                        test_name, i, sources[i], expected_error_counts[i], semantic_errors);
                 if (semantic_errors > 0 && expected_error_counts[i] == 0) {
                     fprintf(stderr, "    Last error: %s\n", analyzer->last_error_message);
                } else if (semantic_errors == 0 && expected_error_counts[i] > 0) {
                     fprintf(stderr, "    Expected an error, but none reported.\n");
                } else if (semantic_errors > 0 && expected_error_counts[i] > 0 && semantic_errors != expected_error_counts[i]) {
                    // Could print last_error_message here too if needed for diagnostics
                     fprintf(stderr, "    Last error: %s\n", analyzer->last_error_message);
                }
                tc_pass = 0;
            }
            semantic_analyzer_free(analyzer);
            basic_ast_node_free_recursive(ast);
        }
         if (!tc_pass) pass = 0; // If any test case fails, the whole test function fails
    }

    if (pass) { (*tests_passed)++; fprintf(stdout, "Test %s: PASSED\n", test_name); }
    else { (*tests_failed)++; fprintf(stderr, "Test %s: FAILED\n", test_name); }
    return pass;
}
