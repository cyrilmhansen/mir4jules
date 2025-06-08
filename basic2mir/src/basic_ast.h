#ifndef BASIC_AST_H
#define BASIC_AST_H

#include <stdint.h> // For int32_t, double
#include <stddef.h> // For size_t

// Forward declaration for expressions and variable references
struct BasicAstNode_s;

// Node types
typedef enum {
    // Literals
    AST_NODE_TYPE_NUMBER_LITERAL, // For numeric literals (integer, single, double)
    AST_NODE_TYPE_STRING_LITERAL,

    // Variables
    AST_NODE_TYPE_VARIABLE,       // Simple variable like A, B%
    AST_NODE_TYPE_ARRAY_VARIABLE, // Array variable like A(10), B$(5,5) (Note: Represented by AST_NODE_TYPE_VARIABLE with dimensions)

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
    AST_NODE_TYPE_DIM,
    AST_NODE_TYPE_REM,
    AST_NODE_TYPE_END,
    AST_NODE_TYPE_PROGRAM_LINE,   // Represents a single line of BASIC code
    AST_NODE_TYPE_PROGRAM         // Root of the AST, a list of program lines
} BasicAstNodeType;

// Variable types in BASIC
typedef enum {
    BASIC_VAR_TYPE_DEFAULT,     // Determined by context or DEFtype statements (float by default)
    BASIC_VAR_TYPE_STRING,      // $
    BASIC_VAR_TYPE_INTEGER,     // %
    BASIC_VAR_TYPE_SINGLE,      // !
    BASIC_VAR_TYPE_DOUBLE       // #
} BasicVariableType;

// Structure for all AST nodes
typedef struct BasicAstNode_s {
    BasicAstNodeType type;
    int32_t line_number; // All statements are associated with a line number

    union {
        // Program structure
        struct {
            struct BasicAstNode_s *program_lines; // Linked list of PROGRAM_LINE nodes
            struct BasicAstNode_s *next_program;  // For chaining multiple programs (not typical for single file)
        } program;

        struct {
            // int32_t number; // The BASIC line number - already in parent struct as line_number for PROGRAM_LINE
            struct BasicAstNode_s *statement;  // The statement on this line
            struct BasicAstNode_s *next_line;  // Pointer to the next BasicAstNode_s of type PROGRAM_LINE
        } program_line;

        // Literals
        struct {
            double value; // Store all numbers as double for simplicity, can refine later
            BasicVariableType num_type; // To distinguish INT#, SINGLE!, DOUBLE#
        } number_literal;

        struct {
            char *value;
        } string_literal;

        // Variables
        // This structure is used for variable references (e.g. in expressions)
        // and also in DIM statements for declarations.
        // For simple variables, dimensions will be null.
        // For array variables, dimensions will be a linked list of expression nodes.
        struct {
            char *name;
            BasicVariableType var_type;
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
            struct BasicAstNode_s *variable; // LHS (can be simple or array element, an AST_NODE_TYPE_VARIABLE)
            struct BasicAstNode_s *expression; // RHS
        } let_stmt;

        struct {
            struct BasicAstNode_s *print_items; // Linked list of expressions to print
            struct BasicAstNode_s *next_print_item; // Used to chain print items
            // TODO: Add support for USING clause, print zones (,), ;, SPC, TAB
        } print_stmt;

        struct {
            char *prompt; // Optional prompt string
            struct BasicAstNode_s *variables; // Linked list of AST_NODE_TYPE_VARIABLE nodes
            struct BasicAstNode_s *next_variable_item; // Used to chain variable items
        } input_stmt;

        struct {
            struct BasicAstNode_s *condition;
            struct BasicAstNode_s *then_branch;  // Can be a single statement or a list of statements (implicit via next_line)
            struct BasicAstNode_s *else_branch;  // Optional (can also be a single statement or list)
        } if_then_else_stmt;

        struct {
            struct BasicAstNode_s *counter_var; // AST_NODE_TYPE_VARIABLE
            struct BasicAstNode_s *start_value;
            struct BasicAstNode_s *end_value;
            struct BasicAstNode_s *step_value; // Optional, defaults to 1
            // Body is implicitly the statements between FOR and NEXT
            // We might need a pointer to the NEXT node or the first statement of the body for execution
            struct BasicAstNode_s *first_loop_statement_line; // Pointer to the PROGRAM_LINE of the first statement in the loop
        } for_stmt;

        struct {
            struct BasicAstNode_s *counter_var; // Optional: AST_NODE_TYPE_VARIABLE. If NULL, matches the innermost loop.
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
            struct BasicAstNode_s *next_declaration; // Used to chain declarations
        } dim_stmt;

        struct {
            char* comment;
        } rem_stmt;

        // END has no specific fields beyond type and line_number

    } data;

    // General purpose next pointer for lists like print items, input variables, dim declarations.
    // For program_lines, the program_line.next_line is used.
    // For array dimensions, variable.dimensions points to the head of a list,
    // and each dimension expression node can use this 'next' to point to the next dimension.
    struct BasicAstNode_s *next;

} BasicAstNode;

// Helper functions for creating AST nodes (declarations)
BasicAstNode *create_ast_node(BasicAstNodeType type, int32_t line_number);
BasicAstNode *create_program_node(int32_t line_number, BasicAstNode *program_lines);
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
BasicAstNode *create_for_node(int32_t line_number, BasicAstNode *counter_var, BasicAstNode *start, BasicAstNode *end, BasicAstNode *step);
BasicAstNode *create_next_node(int32_t line_number, BasicAstNode *counter_var);
BasicAstNode *create_goto_node(int32_t line_number, int32_t target_line);
BasicAstNode *create_gosub_node(int32_t line_number, int32_t target_line);
BasicAstNode *create_return_node(int32_t line_number);
BasicAstNode *create_dim_node(int32_t line_number, BasicAstNode *declarations);
BasicAstNode *create_rem_node(int32_t line_number, const char *comment);
BasicAstNode *create_end_node(int32_t line_number);

// Helper to append to a list of nodes (e.g. print items, dimensions, etc.)
BasicAstNode *append_to_list(BasicAstNode *head, BasicAstNode *new_item);


#endif // BASIC_AST_H
