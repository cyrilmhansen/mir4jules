#ifndef BASIC_SEMANTIC_H
#define BASIC_SEMANTIC_H

#include "basic_ast.h"

// --- Symbol Definition ---
typedef struct Symbol_s {
    char *name;                     // Symbol name (variable, array, etc.)
    BasicVariableType type;         // Variable type (string, integer, single, double, default)
    BasicAstNode *declaration_node; // AST node where it was declared or first used (for DIM, or LET/INPUT)
    // For arrays, we might store dimension info here if not directly on AST_NODE_TYPE_VARIABLE
    // int is_array;
    // BasicAstNode *array_dimensions_ast; // from DIM statement
    // int num_dimensions;
    // int *dimension_sizes; // Resolved sizes after DIM evaluated (later stage)

    // For functions (if GW-BASIC had user-defined functions in this way)
    // int is_function;
    // BasicAstNode *function_def_ast;

    struct Symbol_s *next; // For linked list in symbol table hash bucket
} Symbol;

// --- Symbol Table Definition ---
#define SYMBOL_TABLE_SIZE 256 // Example size for hash table

typedef struct SymbolTable_s {
    Symbol *table[SYMBOL_TABLE_SIZE];
    struct SymbolTable_s *parent_scope; // For future: nested scopes (not typical for classic BASIC)

    // For resolving GOTO/GOSUB line numbers
    BasicAstNode *program_lines_head; // Head of the AST_NODE_TYPE_PROGRAM_LINE list
    // We might also build a direct map from line_number to BasicAstNode* for faster lookups
    // For example, a hash map or a sorted array if line numbers are dense.
    // For now, iterating the program_lines_head list is fine for smaller programs.

} SymbolTable;

// --- Semantic Analyzer Definition ---
typedef struct {
    SymbolTable *symbol_table;
    BasicAstNode *current_program_ast; // Root of the AST being analyzed

    // Error tracking
    int error_count;
    char last_error_message[256]; // Buffer for the last error message
    // TODO: Add a list of errors if multiple errors need to be stored/reported
} SemanticAnalyzer;

// --- Public API Functions ---
SemanticAnalyzer *semantic_analyzer_new(void); // AST can be passed to analyze method
void semantic_analyzer_free(SemanticAnalyzer *analyzer);

// Performs semantic analysis on the provided program AST.
// Returns 0 on success, or the number of errors encountered.
int semantic_analyzer_analyze(SemanticAnalyzer *analyzer, BasicAstNode *program_ast_root);


// --- Internal Helper Function Declarations (for basic_semantic.c) ---
// These would typically be static in basic_semantic.c. Declared here for clarity.

// Symbol Table Helpers
static SymbolTable *symbol_table_new(BasicAstNode *program_lines_head);
static void symbol_table_free(SymbolTable *st);
static unsigned int hash_function(const char *key);
static Symbol *symbol_table_put(SymbolTable *st, const char *name, BasicVariableType type, BasicAstNode *decl_node);
static Symbol *symbol_table_get(SymbolTable *st, const char *name);
static BasicAstNode *symbol_table_get_line_node(SymbolTable *st, int32_t line_number);

// AST Traversal (Visitor Pattern)
static void visit_node(SemanticAnalyzer *analyzer, BasicAstNode *node);
static void visit_program(SemanticAnalyzer *analyzer, BasicAstNode *program_node);
static void visit_program_line(SemanticAnalyzer *analyzer, BasicAstNode *line_node);
static void visit_statement(SemanticAnalyzer *analyzer, BasicAstNode *stmt_node);
// Specific statement visitors
static void visit_let_statement(SemanticAnalyzer *analyzer, BasicAstNode *let_node);
static void visit_print_statement(SemanticAnalyzer *analyzer, BasicAstNode *print_node);
static void visit_goto_statement(SemanticAnalyzer *analyzer, BasicAstNode *goto_node);
static void visit_rem_statement(SemanticAnalyzer *analyzer, BasicAstNode *rem_node);
static void visit_end_statement(SemanticAnalyzer *analyzer, BasicAstNode *end_node);
// ... other statements (IF, FOR, NEXT, DIM, etc.)

static void visit_variable_reference(SemanticAnalyzer *analyzer, BasicAstNode *var_node, int is_declaration);
static void visit_expression(SemanticAnalyzer *analyzer, BasicAstNode *expr_node);
// Specific expression visitors (if needed beyond just calling visit_node)
// static void visit_binary_expr(SemanticAnalyzer *analyzer, BasicAstNode *bin_expr_node);
// static void visit_unary_expr(SemanticAnalyzer *analyzer, BasicAstNode *un_expr_node);
// static void visit_literal_node(SemanticAnalyzer *analyzer, BasicAstNode *literal_node);


// Type System Helpers
static BasicVariableType get_expression_type(SemanticAnalyzer *analyzer, BasicAstNode *expr_node);
static const char* variable_type_to_string(BasicVariableType type); // For error messages

// Error reporting helper
static void semantic_error(SemanticAnalyzer *analyzer, BasicAstNode* node, const char *message_format, ...);

#endif // BASIC_SEMANTIC_H
