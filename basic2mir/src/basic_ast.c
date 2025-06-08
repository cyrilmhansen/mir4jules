#include "basic_ast.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h> // For perror

// --- AST Node Creation Functions ---

BasicAstNode *create_ast_node(BasicAstNodeType type, int32_t line_number) {
    BasicAstNode *node = (BasicAstNode *)calloc(1, sizeof(BasicAstNode)); // Use calloc for zero-initialization
    if (!node) {
        perror("Failed to allocate AST node");
        // In a real application, might use longjmp or a more robust error handling
        exit(EXIT_FAILURE);
    }
    node->type = type;
    node->line_number = line_number;
    // All pointer members in the union and 'next' are now NULL due to calloc.
    return node;
}

BasicAstNode *create_program_node(int32_t line_number) { // line_number typically 0 for the root program node
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_PROGRAM, line_number);
    // node->data.program.program_lines is already NULL due to calloc
    // node->data.program.next_program is also NULL
    return node;
}

BasicAstNode *create_program_line_node(int32_t line_number, BasicAstNode *statement, BasicAstNode *next_line) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_PROGRAM_LINE, line_number);
    node->data.program_line.statement = statement;
    node->data.program_line.next_line = next_line; // This is for linking by parser/list helper
    return node;
}

BasicAstNode *create_number_literal_node(int32_t line_number, double value, BasicVariableType num_type) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_NUMBER_LITERAL, line_number);
    node->data.number_literal.value = value;
    node->data.number_literal.num_type = num_type;
    return node;
}

BasicAstNode *create_string_literal_node(int32_t line_number, const char *value) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_STRING_LITERAL, line_number);
    if (value) {
        node->data.string_literal.value = strdup(value);
        if (!node->data.string_literal.value) {
            perror("Failed to strdup string literal value");
            free(node); // Free the allocated node
            exit(EXIT_FAILURE);
        }
    } else {
        node->data.string_literal.value = NULL; // Should not happen if parser validates
    }
    return node;
}

BasicAstNode *create_variable_node(int32_t line_number, const char *name, BasicVariableType var_type, BasicAstNode *dimensions) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_VARIABLE, line_number);
    if (name) {
        node->data.variable.name = strdup(name);
        if (!node->data.variable.name) {
            perror("Failed to strdup variable name");
            free(node);
            exit(EXIT_FAILURE);
        }
    } else {
        node->data.variable.name = NULL; // Should not happen
    }
    node->data.variable.var_type = var_type;
    node->data.variable.dimensions = dimensions; // Managed by parser
    return node;
}

BasicAstNode *create_unary_expr_node(int32_t line_number, const char *op, BasicAstNode *operand) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_UNARY_EXPR, line_number);
    strncpy(node->data.unary_expr.operator, op, sizeof(node->data.unary_expr.operator) - 1);
    node->data.unary_expr.operator[sizeof(node->data.unary_expr.operator) - 1] = '\0';
    node->data.unary_expr.operand = operand;
    return node;
}

BasicAstNode *create_binary_expr_node(int32_t line_number, const char *op, BasicAstNode *left, BasicAstNode *right) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_BINARY_EXPR, line_number);
    strncpy(node->data.binary_expr.operator, op, sizeof(node->data.binary_expr.operator) - 1);
    node->data.binary_expr.operator[sizeof(node->data.binary_expr.operator) - 1] = '\0';
    node->data.binary_expr.left = left;
    node->data.binary_expr.right = right;
    return node;
}

BasicAstNode *create_let_node(int32_t line_number, BasicAstNode *variable, BasicAstNode *expression) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_LET, line_number);
    node->data.let_stmt.variable = variable;
    node->data.let_stmt.expression = expression;
    return node;
}

BasicAstNode *create_print_node(int32_t line_number, BasicAstNode *print_items) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_PRINT, line_number);
    node->data.print_stmt.print_items = print_items; // Managed by parser
    // node->data.print_stmt.next_print_item is NULL by calloc
    return node;
}

BasicAstNode *create_input_node(int32_t line_number, const char *prompt, BasicAstNode *variables) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_INPUT, line_number);
    if (prompt) {
        node->data.input_stmt.prompt = strdup(prompt);
        if (!node->data.input_stmt.prompt) {
            perror("Failed to strdup input prompt");
            free(node);
            exit(EXIT_FAILURE);
        }
    } // else it's NULL by calloc
    node->data.input_stmt.variables = variables; // Managed by parser
    // node->data.input_stmt.next_variable_item is NULL by calloc
    return node;
}

BasicAstNode *create_if_then_else_node(int32_t line_number, BasicAstNode *condition, BasicAstNode *then_branch, BasicAstNode *else_branch) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_IF_THEN_ELSE, line_number);
    node->data.if_then_else_stmt.condition = condition;
    node->data.if_then_else_stmt.then_branch = then_branch;
    node->data.if_then_else_stmt.else_branch = else_branch;
    return node;
}

BasicAstNode *create_for_node(int32_t line_number, BasicAstNode *counter_var, BasicAstNode *start_value, BasicAstNode *end_value, BasicAstNode *step_value) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_FOR, line_number);
    node->data.for_stmt.counter_var = counter_var;
    node->data.for_stmt.start_value = start_value;
    node->data.for_stmt.end_value = end_value;
    node->data.for_stmt.step_value = step_value; // Can be NULL if no STEP
    // node->data.for_stmt.first_loop_statement_line is NULL by calloc
    return node;
}

BasicAstNode *create_next_node(int32_t line_number, BasicAstNode *counter_var) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_NEXT, line_number);
    node->data.next_stmt.counter_var = counter_var; // Can be NULL
    return node;
}

BasicAstNode *create_goto_node(int32_t line_number, int32_t target_line) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_GOTO, line_number);
    node->data.goto_stmt.target_line_number = target_line;
    return node;
}

BasicAstNode *create_gosub_node(int32_t line_number, int32_t target_line) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_GOSUB, line_number);
    node->data.gosub_stmt.target_line_number = target_line;
    return node;
}

BasicAstNode *create_return_node(int32_t line_number) {
    return create_ast_node(AST_NODE_TYPE_RETURN, line_number);
}

BasicAstNode *create_dim_node(int32_t line_number, BasicAstNode *declarations) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_DIM, line_number);
    node->data.dim_stmt.declarations = declarations; // Managed by parser
    // node->data.dim_stmt.next_declaration is NULL by calloc
    return node;
}

BasicAstNode *create_rem_node(int32_t line_number, const char *comment) {
    BasicAstNode *node = create_ast_node(AST_NODE_TYPE_REM, line_number);
    if (comment) {
        node->data.rem_stmt.comment = strdup(comment);
        if (!node->data.rem_stmt.comment) {
            perror("Failed to strdup REM comment");
            free(node);
            exit(EXIT_FAILURE);
        }
    } // else it's NULL by calloc
    return node;
}

BasicAstNode *create_end_node(int32_t line_number) {
    return create_ast_node(AST_NODE_TYPE_END, line_number);
}


// --- List Helper Functions ---

// Appends a new program line to the program's list of lines.
void append_program_line(BasicAstNode *program_node, BasicAstNode *new_line_node) {
    if (!program_node || program_node->type != AST_NODE_TYPE_PROGRAM ||
        !new_line_node || new_line_node->type != AST_NODE_TYPE_PROGRAM_LINE) {
        // Or handle error appropriately
        return;
    }

    if (!program_node->data.program.program_lines) {
        program_node->data.program.program_lines = new_line_node;
    } else {
        BasicAstNode *current = program_node->data.program.program_lines;
        while (current->data.program_line.next_line) {
            current = current->data.program_line.next_line;
        }
        current->data.program_line.next_line = new_line_node;
    }
}

// Generic append to a list of AST nodes linked by the 'next' field.
// Used for print items, input variables, DIM declarations, array dimensions.
BasicAstNode *append_to_list(BasicAstNode *head, BasicAstNode *new_item) {
    if (!new_item) return head; // Nothing to append
    new_item->next = NULL; // Ensure the new item is the end of the list segment

    if (!head) {
        return new_item; // List was empty, new item is now the head
    }

    BasicAstNode *current = head;
    while (current->next) {
        current = current->next;
    }
    current->next = new_item;
    return head;
}


// --- AST Node Freeing Function ---

void basic_ast_node_free_recursive(BasicAstNode *node) {
    if (node == NULL) return;

    switch (node->type) {
        case AST_NODE_TYPE_PROGRAM:
            BasicAstNode *line = node->data.program.program_lines;
            while (line) {
                BasicAstNode *next_line = line->data.program_line.next_line;
                basic_ast_node_free_recursive(line); // This will free the line and its statement
                line = next_line;
            }
            // next_program is not typically used in this compiler for single files
            basic_ast_node_free_recursive(node->data.program.next_program);
            break;

        case AST_NODE_TYPE_PROGRAM_LINE:
            basic_ast_node_free_recursive(node->data.program_line.statement);
            // next_line is handled by the PROGRAM case or the caller if it's a standalone list
            break;

        case AST_NODE_TYPE_STRING_LITERAL:
            free(node->data.string_literal.value);
            break;

        case AST_NODE_TYPE_VARIABLE:
            free(node->data.variable.name);
            // Dimensions are a list of expression nodes, free them using the generic list logic
            BasicAstNode *dim_expr = node->data.variable.dimensions;
            while(dim_expr) {
                BasicAstNode *next_dim_expr = dim_expr->next;
                basic_ast_node_free_recursive(dim_expr);
                dim_expr = next_dim_expr;
            }
            break;

        case AST_NODE_TYPE_UNARY_EXPR:
            basic_ast_node_free_recursive(node->data.unary_expr.operand);
            break;

        case AST_NODE_TYPE_BINARY_EXPR:
            basic_ast_node_free_recursive(node->data.binary_expr.left);
            basic_ast_node_free_recursive(node->data.binary_expr.right);
            break;

        case AST_NODE_TYPE_LET:
            basic_ast_node_free_recursive(node->data.let_stmt.variable);
            basic_ast_node_free_recursive(node->data.let_stmt.expression);
            break;

        case AST_NODE_TYPE_PRINT:
            BasicAstNode *item = node->data.print_stmt.print_items;
            while (item) {
                BasicAstNode *next_item = item->next; // Assuming generic 'next' for print items
                basic_ast_node_free_recursive(item);
                item = next_item;
            }
            // next_print_item is not used if print_items is a list via generic 'next'
            break;

        case AST_NODE_TYPE_INPUT:
            free(node->data.input_stmt.prompt);
            BasicAstNode *var_item = node->data.input_stmt.variables;
             while (var_item) {
                BasicAstNode *next_var_item = var_item->next;
                basic_ast_node_free_recursive(var_item);
                var_item = next_var_item;
            }
            break;

        case AST_NODE_TYPE_IF_THEN_ELSE:
            basic_ast_node_free_recursive(node->data.if_then_else_stmt.condition);
            basic_ast_node_free_recursive(node->data.if_then_else_stmt.then_branch);
            basic_ast_node_free_recursive(node->data.if_then_else_stmt.else_branch);
            break;

        case AST_NODE_TYPE_FOR:
            basic_ast_node_free_recursive(node->data.for_stmt.counter_var);
            basic_ast_node_free_recursive(node->data.for_stmt.start_value);
            basic_ast_node_free_recursive(node->data.for_stmt.end_value);
            basic_ast_node_free_recursive(node->data.for_stmt.step_value);
            // first_loop_statement_line is just a reference, not owned here
            break;

        case AST_NODE_TYPE_NEXT:
            basic_ast_node_free_recursive(node->data.next_stmt.counter_var); // May be NULL
            break;

        case AST_NODE_TYPE_DIM:
            BasicAstNode *decl = node->data.dim_stmt.declarations;
            while(decl) {
                BasicAstNode *next_decl = decl->next;
                basic_ast_node_free_recursive(decl); // Each decl is an AST_NODE_TYPE_VARIABLE
                decl = next_decl;
            }
            break;

        case AST_NODE_TYPE_REM:
            free(node->data.rem_stmt.comment);
            break;

        // Nodes with no dynamically allocated members or children to free:
        case AST_NODE_TYPE_NUMBER_LITERAL: // value is not a pointer
        case AST_NODE_TYPE_GOTO:
        case AST_NODE_TYPE_GOSUB:
        case AST_NODE_TYPE_RETURN:
        case AST_NODE_TYPE_END:
            break;

        default:
            // This case should ideally not be reached if all node types are handled.
            // Can add a fprintf(stderr, ...) here if needed.
            break;
    }

    // Free the 'next' pointer only if it's a generic list item not handled by specific parent logic
    // This is tricky. Generally, lists are freed by their owners (e.g. PROGRAM frees its lines).
    // If 'next' is used for ad-hoc lists of expressions (like print items or dimensions),
    // the owner of that list head (e.g. PRINT node, VARIABLE node) is responsible for iterating and freeing.
    // basic_ast_node_free_recursive(node->next); // Usually WRONG here - will cause double frees or break parent's list traversal.

    free(node);
}
