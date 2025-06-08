#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "basic_lexer.h"
#include "basic_parser.h"   // Includes basic_ast.h
#include "basic_semantic.h"
#include "basic_mir_emitter.h"
#include "mir.h"
#include "mir-interp.h" // For MIR_interp
#include "mir-gen.h"    // For MIR_set_interp_interface (usually via mir-gen.h)

// --- Forward declarations for helpers in this file ---
static char *read_file_to_string(const char *filename);
static void print_ast_node(BasicAstNode *node, int indent); // Simple AST printer
static void basic_ast_node_free_recursive(BasicAstNode *node); // AST freeing function

// --- Main Driver ---
int main(int argc, char *argv[]) {
    const char *input_filename = NULL;
    const char *output_mir_filename = NULL;
    int print_ast_flag = 0;
    int print_mir_flag = 0;
    int run_mir_flag = 1; // Run by default if no output MIR file specified

    // Argument Parsing
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            output_mir_filename = argv[++i];
            run_mir_flag = 0; // Don't run if outputting to file
        } else if (strcmp(argv[i], "--ast") == 0) {
            print_ast_flag = 1;
        } else if (strcmp(argv[i], "--mir") == 0) {
            print_mir_flag = 1;
            run_mir_flag = 0; // Don't run if just printing MIR text
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            fprintf(stderr, "Usage: basic2mir <inputfile.bas> [-o <output.mir>] [--ast] [--mir]\n");
            return EXIT_FAILURE;
        } else {
            if (input_filename == NULL) {
                input_filename = argv[i];
            } else {
                fprintf(stderr, "Multiple input files not supported.\n");
                return EXIT_FAILURE;
            }
        }
    }

    if (input_filename == NULL) {
        fprintf(stderr, "No input file specified.\n");
        fprintf(stderr, "Usage: basic2mir <inputfile.bas> [-o <output.mir>] [--ast] [--mir]\n");
        return EXIT_FAILURE;
    }

    // File Reading
    char *source_code = read_file_to_string(input_filename);
    if (source_code == NULL) {
        // Error message already printed by read_file_to_string
        return EXIT_FAILURE;
    }

    // Initialization
    MIR_context_t ctx = MIR_init();
    if (ctx == NULL) {
        fprintf(stderr, "Failed to initialize MIR context.\n");
        free(source_code);
        return EXIT_FAILURE;
    }

    BasicLexer *lexer = NULL;
    BasicParser *parser = NULL;
    BasicAstNode *program_ast = NULL;
    SemanticAnalyzer *semantic_analyzer = NULL;
    MirEmitter *mir_emitter = NULL;
    int exit_status = EXIT_SUCCESS;


    // Lexing
    lexer = basic_lexer_new(source_code);
    if (lexer == NULL) {
        fprintf(stderr, "Failed to create lexer.\n");
        exit_status = EXIT_FAILURE;
        goto cleanup;
    }

    // Parsing
    parser = basic_parser_new(lexer);
    if (parser == NULL) {
        fprintf(stderr, "Failed to create parser.\n");
        exit_status = EXIT_FAILURE;
        goto cleanup;
    }
    program_ast = basic_parser_parse_program(parser);
    if (parser->error_count > 0 || program_ast == NULL) {
        fprintf(stderr, "Parsing failed with %d error(s).\n", parser->error_count);
        // Parser might have already printed specific errors.
        exit_status = EXIT_FAILURE;
        goto cleanup;
    }
    printf("Parsing successful.\n");

    if (print_ast_flag) {
        printf("\n--- Abstract Syntax Tree ---\n");
        print_ast_node(program_ast, 0);
        printf("--- End of AST ---\n\n");
    }

    // Semantic Analysis
    semantic_analyzer = semantic_analyzer_new();
    if (semantic_analyzer == NULL) {
        fprintf(stderr, "Failed to create semantic analyzer.\n");
        exit_status = EXIT_FAILURE;
        goto cleanup;
    }
    int semantic_errors = semantic_analyzer_analyze(semantic_analyzer, program_ast);
    if (semantic_errors > 0) {
        fprintf(stderr, "Semantic analysis failed with %d error(s).\n", semantic_errors);
        // Semantic analyzer should have printed specific errors.
        exit_status = EXIT_FAILURE;
        goto cleanup;
    }
    printf("Semantic analysis successful.\n");

    // MIR Emission
    mir_emitter = mir_emitter_new(ctx, semantic_analyzer->symbol_table);
    if (mir_emitter == NULL) {
        fprintf(stderr, "Failed to create MIR emitter.\n");
        exit_status = EXIT_FAILURE;
        goto cleanup;
    }

    char module_name[256] = "BASIC_MODULE"; // Default module name
    // Derive module name from input file if possible
    const char *last_slash = strrchr(input_filename, '/');
    const char *base_name = last_slash ? last_slash + 1 : input_filename;
    const char *dot = strrchr(base_name, '.');
    if (dot) {
        size_t len = dot - base_name;
        if (len < sizeof(module_name) -1) {
            strncpy(module_name, base_name, len);
            module_name[len] = '\0';
        }
    } else { // No extension
         strncpy(module_name, base_name, sizeof(module_name)-1);
         module_name[sizeof(module_name)-1] = '\0';
    }


    MIR_module_t module = mir_emitter_generate_module(mir_emitter, program_ast, module_name);
    if (module == NULL) { // Check if emitter sets an error flag or returns NULL on failure
        fprintf(stderr, "MIR emission failed.\n");
        exit_status = EXIT_FAILURE;
        goto cleanup;
    }
    printf("MIR emission successful for module: %s.\n", module_name);

    if (output_mir_filename != NULL) {
        FILE *f_out = fopen(output_mir_filename, "w");
        if (f_out == NULL) {
            perror("Failed to open output MIR file");
            exit_status = EXIT_FAILURE;
            goto cleanup; // Module will be freed by MIR_finish if loaded, or needs specific free
        }
        MIR_output_module(ctx, f_out, module);
        fclose(f_out);
        printf("MIR module written to %s.\n", output_mir_filename);
    } else if (print_mir_flag) {
        printf("\n--- MIR Code ---\n");
        MIR_output_module(ctx, stdout, module);
        printf("--- End of MIR Code ---\n\n");
    }

    if (run_mir_flag) {
        printf("\n--- Running MIR Code ---\n");
        MIR_load_module(ctx, module);
        MIR_load_external(ctx, "printf", printf);
        MIR_load_external(ctx, "exit", exit);
        // Add other external functions if needed by generated MIR (e.g. string ops)

        MIR_link(ctx, MIR_set_interp_interface, NULL);

        MIR_item_t main_func_item = MIR_get_module_item(ctx, module, "basic_main");
        if (main_func_item && main_func_item->item_type == MIR_func_item) {
            MIR_val_t result;
            MIR_interp(ctx, main_func_item, &result, 0); // 0 args
            // GW-BASIC main doesn't really "return" a value in the C sense that's used,
            // but END (via exit) terminates.
            printf("--- MIR Execution Finished ---\n");
        } else {
            fprintf(stderr, "Error: basic_main function not found in MIR module or is not a function.\n");
            exit_status = EXIT_FAILURE;
        }
    }

cleanup:
    if (mir_emitter) mir_emitter_free(mir_emitter);
    if (semantic_analyzer) semantic_analyzer_free(semantic_analyzer);
    if (program_ast) basic_ast_node_free_recursive(program_ast);
    if (parser) basic_parser_free(parser);
    if (lexer) basic_lexer_free(lexer);
    if (ctx) MIR_finish(ctx);
    free(source_code);

    return exit_status;
}

// --- Helper Implementations ---

static char *read_file_to_string(const char *filename) {
    FILE *file = fopen(filename, "rb"); // Open in binary mode to handle line endings consistently
    if (file == NULL) {
        perror("Error opening input file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (length < 0) {
        fprintf(stderr, "Error determining file size for %s\n", filename);
        fclose(file);
        return NULL;
    }

    char *buffer = (char *)malloc(length + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Memory error allocating buffer for file %s\n", filename);
        fclose(file);
        return NULL;
    }

    size_t read_length = fread(buffer, 1, length, file);
    if (read_length != (size_t)length) {
        fprintf(stderr, "Error reading file %s (expected %ld, got %zu)\n", filename, length, read_length);
        free(buffer);
        fclose(file);
        return NULL;
    }
    buffer[length] = '\0';
    fclose(file);
    return buffer;
}

// Simple recursive AST printing function (example)
// NOTE: This is a basic version. A more robust one would handle all node types
// and use the BasicTokenTypeToString from the parser for token names.
static void print_ast_node(BasicAstNode *node, int indent) {
    if (node == NULL) return;

    for (int i = 0; i < indent; ++i) printf("  ");

    printf("Type: %d, Line: %d", node->type, node->line_number);

    switch (node->type) {
        case AST_NODE_TYPE_PROGRAM:
            printf(" (Program)\n");
            for (int i = 0; i < indent; ++i) printf("  ");
            printf("  Lines:\n");
            BasicAstNode *line = node->data.program.program_lines;
            while (line) {
                print_ast_node(line, indent + 2);
                line = line->data.program_line.next_line;
            }
            break;
        case AST_NODE_TYPE_PROGRAM_LINE:
            printf(" (Line %d)\n", node->line_number); // Line number is already printed
             if (node->data.program_line.statement) {
                print_ast_node(node->data.program_line.statement, indent + 1);
            } else {
                for (int i = 0; i < indent+1; ++i) printf("  ");
                printf("(Empty Statement)\n");
            }
            break;
        case AST_NODE_TYPE_NUMBER_LITERAL:
            printf(" (Number: %f, Type: %d)\n", node->data.number_literal.value, node->data.number_literal.num_type);
            break;
        case AST_NODE_TYPE_STRING_LITERAL:
            printf(" (String: \"%s\")\n", node->data.string_literal.value);
            break;
        case AST_NODE_TYPE_VARIABLE:
            printf(" (Variable: %s, TypeSuffix: %d)\n", node->data.variable.name, node->data.variable.var_type);
            if (node->data.variable.dimensions) {
                for (int i = 0; i < indent; ++i) printf("  ");
                printf("  Dimensions:\n");
                BasicAstNode *dim = node->data.variable.dimensions;
                while(dim) {
                    print_ast_node(dim, indent + 2);
                    dim = dim->next;
                }
            }
            break;
        case AST_NODE_TYPE_LET:
            printf(" (LET)\n");
            print_ast_node(node->data.let_stmt.variable, indent + 1);
            for (int i = 0; i < indent + 1; ++i) printf("  "); printf("  =\n");
            print_ast_node(node->data.let_stmt.expression, indent + 1);
            break;
        case AST_NODE_TYPE_PRINT:
            printf(" (PRINT)\n");
            BasicAstNode *item = node->data.print_stmt.print_items;
            while (item) {
                print_ast_node(item, indent + 1);
                item = item->next;
            }
            break;
        case AST_NODE_TYPE_GOTO:
            printf(" (GOTO %d)\n", node->data.goto_stmt.target_line_number);
            break;
        case AST_NODE_TYPE_REM:
            printf(" (REM: %s)\n", node->data.rem_stmt.comment ? node->data.rem_stmt.comment : "");
            break;
        case AST_NODE_TYPE_END:
            printf(" (END)\n");
            break;
        case AST_NODE_TYPE_BINARY_EXPR:
            printf(" (Binary Op: %s)\n", node->data.binary_expr.operator);
            print_ast_node(node->data.binary_expr.left, indent + 1);
            print_ast_node(node->data.binary_expr.right, indent + 1);
            break;
        case AST_NODE_TYPE_UNARY_EXPR:
            printf(" (Unary Op: %s)\n", node->data.unary_expr.operator);
            print_ast_node(node->data.unary_expr.operand, indent + 1);
            break;
        // Add other node types as needed
        default:
            printf(" (Unknown Node Type: %d)\n", node->type);
            break;
    }
}

// Recursive AST freeing function
// Needs to be careful about shared subtrees if any (not typical in this AST structure)
// and string literals (strdup'd members).
static void basic_ast_node_free_recursive(BasicAstNode *node) {
    if (node == NULL) return;

    switch (node->type) {
        case AST_NODE_TYPE_PROGRAM:
            BasicAstNode *line = node->data.program.program_lines;
            while (line) {
                BasicAstNode *next_line = line->data.program_line.next_line;
                basic_ast_node_free_recursive(line);
                line = next_line;
            }
            break;
        case AST_NODE_TYPE_PROGRAM_LINE:
            basic_ast_node_free_recursive(node->data.program_line.statement);
            // next_line is handled by the PROGRAM loop
            break;
        case AST_NODE_TYPE_STRING_LITERAL:
            free(node->data.string_literal.value); // strdup'd
            break;
        case AST_NODE_TYPE_VARIABLE:
            free(node->data.variable.name); // strdup'd
            BasicAstNode *dim = node->data.variable.dimensions;
            while(dim) {
                BasicAstNode *next_dim = dim->next;
                basic_ast_node_free_recursive(dim);
                dim = next_dim;
            }
            break;
        case AST_NODE_TYPE_LET:
            basic_ast_node_free_recursive(node->data.let_stmt.variable);
            basic_ast_node_free_recursive(node->data.let_stmt.expression);
            break;
        case AST_NODE_TYPE_PRINT:
            BasicAstNode *item = node->data.print_stmt.print_items;
            while (item) {
                 BasicAstNode *next_item = item->next;
                basic_ast_node_free_recursive(item);
                item = next_item;
            }
            break;
        case AST_NODE_TYPE_REM:
            free(node->data.rem_stmt.comment); // strdup'd
            break;
        case AST_NODE_TYPE_BINARY_EXPR:
            basic_ast_node_free_recursive(node->data.binary_expr.left);
            basic_ast_node_free_recursive(node->data.binary_expr.right);
            break;
        case AST_NODE_TYPE_UNARY_EXPR:
            basic_ast_node_free_recursive(node->data.unary_expr.operand);
            break;
        // For GOTO, END, NUMBER_LITERAL, no specific deep free needed beyond the node itself
        // assuming numbers are not pointers and target line numbers are just values.
        case AST_NODE_TYPE_GOTO:
        case AST_NODE_TYPE_END:
        case AST_NODE_TYPE_NUMBER_LITERAL:
            break;
        default:
            // Should not happen if all node types are covered
            break;
    }
    // Free the list items' "next" pointers if they are AST nodes themselves and part of a generic list
    // This is generally handled by the specific list logic (like program_lines, dimensions, print_items)
    // if (node->next) basic_ast_node_free_recursive(node->next); // Only if 'next' is always part of heap allocated AST structure

    free(node);
}
