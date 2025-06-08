#ifndef BASIC_MIR_EMITTER_H
#define BASIC_MIR_EMITTER_H

#include "basic_ast.h"
#include "basic_semantic.h" // For SymbolTable, Symbol, BasicVariableType
#include "mir.h"
#include "mir-gen.h" // For MIR_gen_init, MIR_gen_finish if used directly

// --- Label Map for GOTO/GOSUB ---
typedef struct LabelMapEntry_s {
    int32_t basic_line_number;
    MIR_label_t mir_label;
    struct LabelMapEntry_s *next; // For simple linked list hash bucket
} LabelMapEntry;

#define LABEL_MAP_SIZE 128 // Adjust as needed

// --- MIR Emitter Structure ---
// Structure to hold MIR related items for a variable
typedef struct VarMirInfo_s {
    char *name;             // BASIC variable name
    MIR_reg_t mir_reg;      // MIR register holding the value or pointer (for strings)
    MIR_type_t mir_type;    // MIR type of the register
    BasicVariableType basic_type; // Original BASIC type
    int is_global_or_static_mem; // If it's allocated in static memory (future use)
    MIR_item_t static_mem_item;  // MIR item for static memory (future use for complex vars)
    struct VarMirInfo_s *next; // For linked list in hash bucket
} VarMirInfo;

#define VAR_MIR_MAP_SIZE 256

typedef struct MirEmitter_s {
    MIR_context_t ctx;                  // MIR context
    MIR_module_t current_module;        // Current MIR module being built
    MIR_item_t current_func_item;       // Current MIR function (e.g., "basic_main")
    SymbolTable *symbol_table;          // Symbol table from semantic analysis

    // Label mapping for BASIC line numbers to MIR labels
    LabelMapEntry *label_map[LABEL_MAP_SIZE];

    // Variable to MIR register/memory mapping
    VarMirInfo *var_map[VAR_MIR_MAP_SIZE];

    int temp_reg_count;             // Counter for generating unique temporary register names
    int string_literal_count;       // Counter for unique string literal names

    // Commonly used items
    MIR_item_t printf_proto_item;      // Prototype for printf
    MIR_item_t exit_proto_item;        // Prototype for exit (for END statement)
    // Add more for other C library functions if needed (e.g. string manipulation)

} MirEmitter;

// --- Public API Functions ---
MirEmitter *mir_emitter_new(MIR_context_t ctx, SymbolTable *symbol_table);
void mir_emitter_free(MirEmitter *emitter);
MIR_module_t mir_emitter_generate_module(MirEmitter *emitter, BasicAstNode *program_ast, const char *module_name);

// --- Internal Helper Function Declarations (for basic_mir_emitter.c) ---
// These would typically be static.

// Label Management
static MIR_label_t get_or_create_mir_label(MirEmitter *emitter, int32_t basic_line_number);
// (label_map_put and label_map_get will be static within .c)

// Variable Management
static MIR_reg_t get_mir_reg_for_variable(MirEmitter *emitter, Symbol* symbol);
static MIR_type_t basic_type_to_mir_type(BasicVariableType basic_type);
static VarMirInfo* get_var_mir_info(MirEmitter *emitter, const char* var_name);


// Temp Register Management
static MIR_reg_t get_new_temp_mir_reg(MirEmitter *emitter, MIR_type_t type);
static MIR_reg_t get_new_temp_reg_same_type(MirEmitter *emitter, MIR_op_t op);


// Statement Emitters
static void emit_program_lines(MirEmitter *emitter, BasicAstNode *program_node);
static void emit_statement(MirEmitter *emitter, BasicAstNode *stmt_node);
static void emit_let_statement(MirEmitter *emitter, BasicAstNode *let_node);
static void emit_print_statement(MirEmitter *emitter, BasicAstNode *print_node);
static void emit_goto_statement(MirEmitter *emitter, BasicAstNode *goto_node);
static void emit_end_statement(MirEmitter *emitter, BasicAstNode *end_node);
// ... other statement emitters (IF, FOR, etc.)

// Expression Emitters
// as_lvalue: TRUE if the expression's address is needed, FALSE for its value.
// For simple variables, this might not change much if they are directly in regs.
// For array elements or future complex types, it would matter.
static MIR_op_t emit_expression(MirEmitter *emitter, BasicAstNode *expr_node);
static MIR_op_t emit_variable_expr(MirEmitter *emitter, BasicAstNode *var_node);
static MIR_op_t emit_literal_expr(MirEmitter *emitter, BasicAstNode *literal_node);
static MIR_op_t emit_binary_expr(MirEmitter *emitter, BasicAstNode *bin_expr_node);
static MIR_op_t emit_unary_expr(MirEmitter *emitter, BasicAstNode *unary_expr_node);

// Utilities
static MIR_item_t ensure_printf_prototype(MirEmitter *emitter);
static MIR_item_t ensure_exit_prototype(MirEmitter *emitter);
static MIR_item_t create_string_literal_item(MirEmitter *emitter, const char* str_val);


#endif // BASIC_MIR_EMITTER_H
