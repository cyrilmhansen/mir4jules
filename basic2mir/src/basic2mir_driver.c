#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "basic_lexer.h"
#include "basic_parser.h"   // Includes basic_ast.h
#include "basic_semantic.h"
#include "basic_mir_emitter.h"
#include "mir.h"
#include "mir-interp.h" // For MIR_interp
#include "mir-gen.h"    // For MIR_set_interp_interface, MIR_set_gen_interface etc.

// --- Forward declarations for helpers in this file ---
static char *read_file_to_string(const char *filename);
static void print_ast_node(BasicAstNode *node, int indent); // Simple AST printer
static void basic_ast_node_free_recursive(BasicAstNode *node); // AST freeing function
static void print_usage(const char *prog_name);

// --- Main Driver ---
int main(int argc, char *argv[]) {
    const char *input_filename = NULL;
    const char *output_mir_filename = NULL;
    int print_ast_flag = 0;
    int print_mir_flag = 0;
    const char *execution_mode_str = "interp"; // Default execution mode
    int run_mir_flag;

    // Argument Parsing
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return EXIT_SUCCESS;
        } else if (strcmp(argv[i], "-o") == 0) {
            if (i + 1 < argc) {
                output_mir_filename = argv[++i];
            } else {
                fprintf(stderr, "Error: -o option requires an argument (output file name).\n");
                print_usage(argv[0]);
                return EXIT_FAILURE;
            }
        } else if (strcmp(argv[i], "--ast") == 0) {
            print_ast_flag = 1;
        } else if (strcmp(argv[i], "--mir") == 0) {
            print_mir_flag = 1;
        } else if (strcmp(argv[i], "--exec-mode") == 0) {
            if (i + 1 < argc) {
                execution_mode_str = argv[++i];
                if (strcmp(execution_mode_str, "interp") != 0 &&
                    strcmp(execution_mode_str, "jit") != 0 &&
                    strcmp(execution_mode_str, "lazyjit") != 0) {
                    fprintf(stderr, "Error: Invalid execution mode '%s'. Options are: interp, jit, lazyjit.\n", execution_mode_str);
                    print_usage(argv[0]);
                    return EXIT_FAILURE;
                }
            } else {
                fprintf(stderr, "Error: --exec-mode option requires an argument (interp, jit, or lazyjit).\n");
                print_usage(argv[0]);
                return EXIT_FAILURE;
            }
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            print_usage(argv[0]);
            return EXIT_FAILURE;
        } else {
            if (input_filename == NULL) {
                input_filename = argv[i];
            } else {
                fprintf(stderr, "Error: Multiple input files not supported. '%s' is extraneous.\n", argv[i]);
                print_usage(argv[0]);
                return EXIT_FAILURE;
            }
        }
    }

    if (input_filename == NULL) {
        fprintf(stderr, "Error: No input file specified.\n");
        print_usage(argv[0]);
        return EXIT_FAILURE;
    }

    // Determine if MIR should be run based on options
    run_mir_flag = (output_mir_filename == NULL && !print_mir_flag);

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

    char module_name[256] = "BASIC_MODULE";
    const char *last_slash = strrchr(input_filename, '/');
    const char *base_name = last_slash ? last_slash + 1 : input_filename;
    const char *dot = strrchr(base_name, '.');
    if (dot) {
        size_t len = dot - base_name;
        if (len < sizeof(module_name) -1) {
            strncpy(module_name, base_name, len);
            module_name[len] = '\0';
        }
    } else {
         strncpy(module_name, base_name, sizeof(module_name)-1);
         module_name[sizeof(module_name)-1] = '\0';
    }

    MIR_module_t module = mir_emitter_generate_module(mir_emitter, program_ast, module_name);
    if (module == NULL) {
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
            goto cleanup;
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
        // Add other external functions if needed

        // Link MIR module based on execution mode
        if (strcmp(execution_mode_str, "jit") == 0) {
            MIR_link(ctx, MIR_set_gen_interface, NULL);
            fprintf(stdout, "INFO: Using JIT execution mode.\n");
        } else if (strcmp(execution_mode_str, "lazyjit") == 0) {
             MIR_link(ctx, MIR_set_lazy_gen_interface, NULL);
            fprintf(stdout, "INFO: Using Lazy JIT execution mode.\n");
        } else { // Default to "interp"
            MIR_link(ctx, MIR_set_interp_interface, NULL);
            fprintf(stdout, "INFO: Using Interpreter execution mode.\n");
        }

        MIR_item_t main_func_item = MIR_get_module_item(ctx, module, "basic_main");
        if (main_func_item && main_func_item->item_type == MIR_func_item) {
            MIR_val_t result;

            if (strcmp(execution_mode_str, "jit") == 0 || strcmp(execution_mode_str, "lazyjit") == 0) {
                typedef int (*main_func_ptr_t)(void); // Assuming basic_main returns int (like C main) or void
                MIR_gen_init(ctx, 1); // Set optimization level for JIT
                main_func_ptr_t compiled_main = MIR_gen(ctx, 0, main_func_item);
                MIR_gen_finish(ctx);

                if (compiled_main) {
                    compiled_main();
                } else {
                    fprintf(stderr, "Error: Failed to JIT compile basic_main function.\n");
                    exit_status = EXIT_FAILURE;
                }
            } else { // Interpreter mode
                 MIR_interp(ctx, main_func_item, &result, 0);
            }
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

static void print_usage(const char *prog_name) {
    fprintf(stderr, "Usage: %s <inputfile.bas> [options]\n", prog_name);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -o <output.mir>     Specify MIR output file (textual format).\n");
    fprintf(stderr, "  --ast               Print the Abstract Syntax Tree to stdout.\n");
    fprintf(stderr, "  --mir               Print the generated MIR code to stdout.\n");
    fprintf(stderr, "  --exec-mode <mode>  Set MIR execution mode. Options: interp, jit, lazyjit. Default: interp.\n");
    fprintf(stderr, "  -h, --help          Display this help message.\n");
}

static char *read_file_to_string(const char *filename) {
    FILE *file = fopen(filename, "rb");
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
            printf(" (Line %d)\n", node->line_number);
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
        default:
            printf(" (Unknown Node Type: %d)\n", node->type);
            break;
    }
}

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
            break;
        case AST_NODE_TYPE_STRING_LITERAL:
            free(node->data.string_literal.value);
            break;
        case AST_NODE_TYPE_VARIABLE:
            free(node->data.variable.name);
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
            free(node->data.rem_stmt.comment);
            break;
        case AST_NODE_TYPE_BINARY_EXPR:
            basic_ast_node_free_recursive(node->data.binary_expr.left);
            basic_ast_node_free_recursive(node->data.binary_expr.right);
            break;
        case AST_NODE_TYPE_UNARY_EXPR:
            basic_ast_node_free_recursive(node->data.unary_expr.operand);
            break;
        case AST_NODE_TYPE_GOTO:
        case AST_NODE_TYPE_END:
        case AST_NODE_TYPE_NUMBER_LITERAL:
            break;
        default:
            break;
    }
// basic_ast_node_free_recursive is now implemented in basic_ast.c
// Ensure it's declared in basic_ast.h and basic_ast.c is linked.
