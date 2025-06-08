#ifndef BASIC_AST_H
#define BASIC_AST_H

#include <stdint.h> // For int32_t, double
#include <stddef.h> // For size_t
#include "mir.h"    // For MIR_label_t

// Forward declaration for expressions and variable references
struct BasicAstNode_s;

// Node types
typedef enum {
    // Literals
    AST_NODE_TYPE_NUMBER_LITERAL,
    AST_NODE_TYPE_STRING_LITERAL,

    // Variables
    AST_NODE_TYPE_VARIABLE,       // Simple variable like A, B%; also used for array references A(1)

    // Expressions
    AST_NODE_TYPE_UNARY_EXPR,     // e.g., -A, NOT X
    AST_NODE_TYPE_BINARY_EXPR,    // e.g., A + B, X AND Y

    // Statements
    AST_NODE_TYPE_LET,
    AST_NODE_TYPE_PRINT,
    AST_NODE_TYPE_INPUT,
    AST_NODE_TYPE_IF_THEN_ELSE,
    AST_NODE_TYPE_FOR,
    AST_NODE_TYPE_NEXT,
    AST_NODE_TYPE_GOTO,
    AST_NODE_TYPE_GOSUB,
    AST_NODE_TYPE_RETURN,
    AST_NODE_TYPE_DIM,            // Array dimensioning
    AST_NODE_TYPE_REM,
    AST_NODE_TYPE_END,
    AST_NODE_TYPE_PROGRAM_LINE,   // Represents a single line of BASIC code
    AST_NODE_TYPE_PROGRAM         // Root of the AST, a list of program lines
} BasicAstNodeType;

// Variable types in BASIC
typedef enum {
    BASIC_VAR_TYPE_DEFAULT = 0, // Determined by context or DEFtype (float/single by default in many BASICs)
    BASIC_VAR_TYPE_STRING,      // $
    BASIC_VAR_TYPE_INTEGER,     // %
    BASIC_VAR_TYPE_SINGLE,      // !
    BASIC_VAR_TYPE_DOUBLE       // #
} BasicVariableType;

// Structure for all AST nodes
typedef struct BasicAstNode_s {
    BasicAstNodeType type;
    int32_t line_number; // Original BASIC line number for statements/program lines

    union {
        // Program structure
        struct {
            struct BasicAstNode_s *program_lines; // Linked list of PROGRAM_LINE nodes
            struct BasicAstNode_s *next_program;  // For chaining multiple programs (not typical for single file)
        } program;

        struct {
            // Line number is in parent BasicAstNode_s
            struct BasicAstNode_s *statement;  // The statement on this line (can be NULL for empty line number)
            struct BasicAstNode_s *next_line;  // Pointer to the next BasicAstNode_s of type PROGRAM_LINE
        } program_line;

        // Literals
        struct {
            double value;
            BasicVariableType num_type;
        } number_literal;

        struct {
            char *value; // strdup'd
        } string_literal;

        // Variables
        struct {
            char *name; // strdup'd
            BasicVariableType var_type; // Type determined by suffix or context/DIM
            struct BasicAstNode_s *dimensions; // Linked list of expression nodes for array dimensions or NULL
        } variable;

        // Expressions
        struct {
            char operator[5]; // e.g., "-", "NOT"
            struct BasicAstNode_s *operand;
        } unary_expr;

        struct {
            char operator[5]; // e.g., "+", "AND"
            struct BasicAstNode_s *left;
            struct BasicAstNode_s *right;
        } binary_expr;

        // Statements
        struct {
            struct BasicAstNode_s *variable;
            struct BasicAstNode_s *expression;
        } let_stmt;

        struct {
            struct BasicAstNode_s *print_items; // Linked list of expressions to print (using generic 'next')
            // next_print_item was removed in favor of generic next for lists
        } print_stmt;

        struct {
            char *prompt; // Optional prompt string (strdup'd)
            struct BasicAstNode_s *variables; // Linked list of AST_NODE_TYPE_VARIABLE nodes (using generic 'next')
            // next_variable_item was removed
        } input_stmt;

        struct {
            struct BasicAstNode_s *condition;
            struct BasicAstNode_s *then_branch;  // Single statement or block (e.g. could be another PROGRAM_LINE list implicitly)
            struct BasicAstNode_s *else_branch;  // Optional
        } if_then_else_stmt;

        struct {
            struct BasicAstNode_s *counter_var;
            struct BasicAstNode_s *start_value;
            struct BasicAstNode_s *end_value;
            struct BasicAstNode_s *step_value; // Optional, defaults to 1
            // struct BasicAstNode_s *first_loop_statement_line; // Not used directly, body is between FOR and NEXT
            MIR_label_t loop_check_label; // Label for the loop condition check (start of loop)
            MIR_label_t loop_exit_label;  // Label for jumping out of the loop
            struct BasicAstNode_s *next_node_ptr; // Pointer to the corresponding NEXT node
        } for_stmt;

        struct {
            struct BasicAstNode_s *loop_variable; // Optional: AST_NODE_TYPE_VARIABLE. If NULL, matches the innermost loop.
            struct BasicAstNode_s *for_node_ptr; // Pointer to the corresponding FOR node
        } next_stmt;

        struct {
            int32_t target_line_number;
        } goto_stmt;

        struct {
            int32_t target_line_number;
        } gosub_stmt;

        // RETURN has no specific fields beyond type and line_number

        struct {
            struct BasicAstNode_s *declarations; // Linked list of AST_NODE_TYPE_VARIABLE nodes (arrays with dimensions)
            // next_declaration was removed
        } dim_stmt;

        struct {
            char* comment; // strdup'd
        } rem_stmt;

        // END has no specific fields

    } data;

    // Generic 'next' pointer for use in lists like print items, dimensions, input variables, DIM declarations.
    // For program_lines, program_line.next_line is used.
    struct BasicAstNode_s *next;

} BasicAstNode;

// --- AST Node Creation Helper Function Declarations ---
BasicAstNode *create_ast_node(BasicAstNodeType type, int32_t line_number);

BasicAstNode *create_program_node(int32_t line_number);
BasicAstNode *create_program_line_node(int32_t line_number, BasicAstNode *statement, BasicAstNode *next_line);

BasicAstNode *create_number_literal_node(int32_t line_number, double value, BasicVariableType num_type);
BasicAstNode *create_string_literal_node(int32_t line_number, const char *value);

BasicAstNode *create_variable_node(int32_t line_number, const char *name, BasicVariableType var_type, BasicAstNode *dimensions);

BasicAstNode *create_unary_expr_node(int32_t line_number, const char *op, BasicAstNode *operand);
BasicAstNode *create_binary_expr_node(int32_t line_number, const char *op, BasicAstNode *left, BasicAstNode *right);

BasicAstNode *create_let_node(int32_t line_number, BasicAstNode *variable, BasicAstNode *expression);
BasicAstNode *create_print_node(int32_t line_number, BasicAstNode *print_items);
BasicAstNode *create_input_node(int32_t line_number, const char *prompt, BasicAstNode *variables);
BasicAstNode *create_if_then_else_node(int32_t line_number, BasicAstNode *condition, BasicAstNode *then_branch, BasicAstNode *else_branch);
BasicAstNode *create_for_node(int32_t line_number, BasicAstNode *counter_var, BasicAstNode *start_value, BasicAstNode *end_value, BasicAstNode *step_value);
BasicAstNode *create_next_node(int32_t line_number, BasicAstNode *counter_var);
BasicAstNode *create_goto_node(int32_t line_number, int32_t target_line);
BasicAstNode *create_gosub_node(int32_t line_number, int32_t target_line);
BasicAstNode *create_return_node(int32_t line_number);
BasicAstNode *create_dim_node(int32_t line_number, BasicAstNode *declarations);
BasicAstNode *create_rem_node(int32_t line_number, const char *comment); // Changed from create_rem_statement_node for consistency
BasicAstNode *create_end_node(int32_t line_number);

// --- List Helper Function Declarations ---
void append_program_line(BasicAstNode *program_node, BasicAstNode *new_line_node);
BasicAstNode *append_to_list(BasicAstNode *head, BasicAstNode *new_item); // For generic lists using 'next'

// --- AST Node Freeing Function Declaration ---
void basic_ast_node_free_recursive(BasicAstNode *node);

#endif // BASIC_AST_H
