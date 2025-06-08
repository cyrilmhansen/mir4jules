#include "basic_mir_emitter.h"
#include "mir-gen.h" // For MIR_gen_... utilities like MIR_gen_mov, etc.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// --- Forward declarations for static functions ---
static MIR_label_t get_or_create_mir_label(MirEmitter *emitter, int32_t basic_line_number);
static void label_map_put(MirEmitter *emitter, int32_t basic_line, MIR_label_t mir_label);
static MIR_label_t label_map_get(MirEmitter *emitter, int32_t basic_line);
static unsigned int label_map_hash(int32_t basic_line_number);

static VarMirInfo* get_var_mir_info(MirEmitter *emitter, const char* var_name);
static VarMirInfo* add_var_mir_info(MirEmitter *emitter, const char* var_name, MIR_reg_t reg, MIR_type_t mir_type, BasicVariableType basic_type);
static unsigned int var_map_hash(const char* var_name);

static MIR_type_t basic_type_to_mir_type(BasicVariableType basic_type);
static MIR_reg_t get_mir_reg_for_variable(MirEmitter *emitter, Symbol* symbol);
static MIR_reg_t get_new_temp_mir_reg(MirEmitter *emitter, MIR_type_t type);
static MIR_op_t emit_expression(MirEmitter *emitter, BasicAstNode *expr_node);
static void emit_statement(MirEmitter *emitter, BasicAstNode *stmt_node);

// --- Label Map Implementation ---
static unsigned int label_map_hash(int32_t basic_line_number) {
    return (unsigned int)basic_line_number % LABEL_MAP_SIZE;
}

static void label_map_put(MirEmitter *emitter, int32_t basic_line, MIR_label_t mir_label) {
    unsigned int index = label_map_hash(basic_line);
    LabelMapEntry *entry = (LabelMapEntry *)malloc(sizeof(LabelMapEntry));
    entry->basic_line_number = basic_line;
    entry->mir_label = mir_label;
    entry->next = emitter->label_map[index];
    emitter->label_map[index] = entry;
}

static MIR_label_t label_map_get(MirEmitter *emitter, int32_t basic_line) {
    unsigned int index = label_map_hash(basic_line);
    LabelMapEntry *curr = emitter->label_map[index];
    while (curr) {
        if (curr->basic_line_number == basic_line) {
            return curr->mir_label;
        }
        curr = curr->next;
    }
    return NULL; // Should use get_or_create_mir_label to ensure it exists
}

static MIR_label_t get_or_create_mir_label(MirEmitter *emitter, int32_t basic_line_number) {
    MIR_label_t label = label_map_get(emitter, basic_line_number);
    if (label == NULL) {
        char label_name[32];
        snprintf(label_name, sizeof(label_name), "L%d", basic_line_number);
        label = MIR_new_label(emitter->ctx); // Rely on MIR to make unique names based on this
        // MIR_set_label_name (if available and needed, or let MIR handle internal naming)
        label_map_put(emitter, basic_line_number, label);
    }
    return label;
}

// --- Var MIR Info Map Implementation ---
static unsigned int var_map_hash(const char* var_name) {
    unsigned long hash = 5381;
    int c;
    while ((c = *var_name++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    return hash % VAR_MIR_MAP_SIZE;
}

static VarMirInfo* get_var_mir_info(MirEmitter *emitter, const char* var_name) {
    unsigned int index = var_map_hash(var_name);
    VarMirInfo *curr = emitter->var_map[index];
    while (curr) {
        if (strcmp(curr->name, var_name) == 0) {
            return curr;
        }
        curr = curr->next;
    }
    return NULL;
}

static VarMirInfo* add_var_mir_info(MirEmitter *emitter, const char* var_name, MIR_reg_t reg, MIR_type_t mir_type, BasicVariableType basic_type) {
    if (get_var_mir_info(emitter, var_name) != NULL) {
        // This shouldn't happen if semantic analysis ensures unique effective names,
        // or if we always look up first.
        fprintf(stderr, "Error: VarMirInfo for '%s' already exists.\n", var_name);
        return get_var_mir_info(emitter, var_name);
    }
    unsigned int index = var_map_hash(var_name);
    VarMirInfo *entry = (VarMirInfo *)calloc(1, sizeof(VarMirInfo));
    entry->name = strdup(var_name);
    entry->mir_reg = reg;
    entry->mir_type = mir_type;
    entry->basic_type = basic_type;
    entry->next = emitter->var_map[index];
    emitter->var_map[index] = entry;
    return entry;
}


// --- Type Conversion ---
static MIR_type_t basic_type_to_mir_type(BasicVariableType basic_type) {
    switch (basic_type) {
        case BASIC_VAR_TYPE_INTEGER: return MIR_T_I64; // Or I32 if targetting 32-bit, adjust as per MIR capabilities
        case BASIC_VAR_TYPE_SINGLE:  return MIR_T_F;
        case BASIC_VAR_TYPE_DOUBLE:  return MIR_T_D;
        case BASIC_VAR_TYPE_STRING:  return MIR_T_P; // Pointer to string data
        case BASIC_VAR_TYPE_DEFAULT: return MIR_T_F; // GW-BASIC default is single-precision float
        default:
            fprintf(stderr, "Unknown BasicVariableType: %d\n", basic_type);
            return MIR_T_I64; // Fallback, should not happen
    }
}

// --- MIR Emitter Public API ---
MirEmitter *mir_emitter_new(MIR_context_t ctx, SymbolTable *symbol_table) {
    MirEmitter *emitter = (MirEmitter *)calloc(1, sizeof(MirEmitter));
    if (!emitter) {
        perror("Failed to allocate MirEmitter");
        return NULL;
    }
    emitter->ctx = ctx;
    emitter->symbol_table = symbol_table;
    emitter->temp_reg_count = 0;
    emitter->string_literal_count = 0;
    // Label map and var map are already zeroed by calloc

    emitter->printf_proto_item = NULL; // Will be created on demand
    emitter->exit_proto_item = NULL;   // Will be created on demand
    return emitter;
}

void mir_emitter_free(MirEmitter *emitter) {
    if (!emitter) return;

    for (int i = 0; i < LABEL_MAP_SIZE; ++i) {
        LabelMapEntry *curr = emitter->label_map[i];
        while (curr) {
            LabelMapEntry *next = curr->next;
            free(curr);
            curr = next;
        }
    }
    for (int i = 0; i < VAR_MIR_MAP_SIZE; ++i) {
        VarMirInfo *curr = emitter->var_map[i];
        while (curr) {
            VarMirInfo *next = curr->next;
            free(curr->name);
            free(curr);
            curr = next;
        }
    }
    // Note: MIR_item_t (like printf_proto_item) are owned by the MIR_module / MIR_context
    // and should not be freed here directly unless they were uniquely created and not added.
    // Module itself is freed by caller of mir_emitter_generate_module.
    free(emitter);
}

// --- Register Management ---
static MIR_reg_t get_mir_reg_for_variable(MirEmitter *emitter, Symbol* symbol) {
    if (!symbol || !symbol->name) {
        fprintf(stderr, "Error: Cannot get MIR register for null symbol or unnamed symbol.\n");
        // Return a dummy/invalid reg or handle error appropriately
        return MIR_REG_BOUND;
    }

    VarMirInfo *var_info = get_var_mir_info(emitter, symbol->name);
    if (var_info) {
        // Ensure consistent MIR type if symbol's basic_type has been refined
        // This is a bit of a sanity check; type should be stable after first creation.
        MIR_type_t expected_mir_type = basic_type_to_mir_type(symbol->type);
        if (var_info->mir_type != expected_mir_type) {
            fprintf(stderr, "Warning: MIR type mismatch for %s. Expected %d, found %d. Re-evaluating.\n",
                    symbol->name, expected_mir_type, var_info->mir_type);
            // This case is tricky. If a register was already created with a different MIR type,
            // it cannot simply be changed. This implies an issue in semantic analysis or type inference.
            // For now, we'll trust the existing var_info->mir_type if it exists,
            // or prefer the symbol's current type if creating new.
        }
        return var_info->mir_reg;
    }

    char reg_name_basic[256]; // For debug name in MIR_new_func_reg
    snprintf(reg_name_basic, sizeof(reg_name_basic), "%s", symbol->name);

    MIR_type_t mir_type = basic_type_to_mir_type(symbol->type);
    MIR_reg_t mir_reg = MIR_new_func_reg(emitter->ctx, emitter->current_func_item->u.func, mir_type, reg_name_basic);

    add_var_mir_info(emitter, symbol->name, mir_reg, mir_type, symbol->type);
    return mir_reg;
}


static MIR_reg_t get_new_temp_mir_reg(MirEmitter *emitter, MIR_type_t type) {
    char reg_name[32];
    snprintf(reg_name, sizeof(reg_name), "t%d", emitter->temp_reg_count++);
    return MIR_new_func_reg(emitter->ctx, emitter->current_func_item->u.func, type, reg_name);
}

static MIR_reg_t get_new_temp_reg_same_type(MirEmitter *emitter, MIR_op_t op) {
    assert(op.mode == MIR_OP_REG || op.mode == MIR_OP_HARD_REG); // Should be a register
    MIR_reg_t old_reg = op.u.reg;
    MIR_reg_type_t reg_type = MIR_reg_type(emitter->ctx, old_reg, emitter->current_func_item->u.func);
    MIR_type_t type;
    if (reg_type == MIR_INSTANCE_TYPE) type = MIR_reg_obj_type(emitter->ctx, old_reg, emitter->current_func_item->u.func);
    else type = reg_type;

    return get_new_temp_mir_reg(emitter, type);
}


// --- Utility for C lib prototypes ---
static MIR_item_t ensure_printf_prototype(MirEmitter *emitter) {
    if (emitter->printf_proto_item == NULL) {
        MIR_type_t ret_type = MIR_T_I32;
        MIR_type_t arg_types[] = {MIR_T_P}; // const char* fmt
        MIR_var_t args[] = {{"fmt", MIR_T_P}}; // Variadic, so only specify fixed args

        emitter->printf_proto_item = MIR_new_proto(emitter->ctx, "printf_proto", 1, &ret_type, 1, args, TRUE); // TRUE for variadic
        // To make it importable, it seems it should be an import item referencing a name.
        // For now, using proto directly if possible, or adjust to use import if needed for linking.
        // Let's assume we need an import for external functions.
        MIR_item_t import_item = MIR_new_import(emitter->ctx, "printf");
        // Link proto to module
        MIR_module_t current_module = emitter->current_module; // Assuming current_module is set
        // Add proto to module if not already (MIR might handle this implicitly with new_proto, or require explicit add)
        // This part might need adjustment based on how MIR handles module population.
        // For now, let's assume the prototype itself is enough to be referenced in CALL.
        // The actual "printf" symbol will be resolved at link time.
    }
    return emitter->printf_proto_item;
}

static MIR_item_t ensure_exit_prototype(MirEmitter *emitter) {
    if (emitter->exit_proto_item == NULL) {
        MIR_type_t ret_type = MIR_T_I32; // exit "returns" int but doesn't return to caller
        MIR_type_t arg_types[] = {MIR_T_I32}; // int status
        MIR_var_t args[] = {{"status", MIR_T_I32}};
        emitter->exit_proto_item = MIR_new_proto(emitter->ctx, "exit_proto", 1, &ret_type, 1, args, FALSE);
        // Similar to printf, create an import
        MIR_item_t import_item = MIR_new_import(emitter->ctx, "exit");
    }
    return emitter->exit_proto_item;
}

static MIR_item_t create_string_literal_item(MirEmitter *emitter, const char* str_val) {
    char item_name[64];
    snprintf(item_name, sizeof(item_name), "str%d", emitter->string_literal_count++);
    MIR_item_t str_item = MIR_new_string_data(emitter->ctx, item_name, str_val);
    // String items are typically added to the current module
    // MIR_add_data(emitter->ctx, str_item); // Check MIR API for adding data to module
    return str_item;
}

// --- Main Module Generation ---
MIR_module_t mir_emitter_generate_module(MirEmitter *emitter, BasicAstNode *program_ast, const char *module_name) {
    assert(program_ast && program_ast->type == AST_NODE_TYPE_PROGRAM);

    emitter->current_module = MIR_new_module(emitter->ctx, module_name);

    // Create main function for the BASIC program
    MIR_type_t main_ret_type = MIR_T_I32; // main returns int
    emitter->current_func_item = MIR_new_func(emitter->ctx, "basic_main", 1, &main_ret_type, 0, NULL);

    // Process program lines
    emit_program_lines(emitter, program_ast);

    MIR_finish_func(emitter->ctx);
    MIR_finish_module(emitter->ctx);

    return emitter->current_module;
}

static void emit_program_lines(MirEmitter *emitter, BasicAstNode *program_node) {
    BasicAstNode *current_line_node = program_node->data.program.program_lines;
    while (current_line_node) {
        if (current_line_node->type == AST_NODE_TYPE_PROGRAM_LINE) {
            // Ensure a label exists for this line number and append it
            MIR_label_t line_label = get_or_create_mir_label(emitter, current_line_node->line_number);
            MIR_append_insn(emitter->ctx, emitter->current_func_item, line_label);

            if (current_line_node->data.program_line.statement) {
                emit_statement(emitter, current_line_node->data.program_line.statement);
            }
        }
        current_line_node = current_line_node->data.program_line.next_line;
    }
}

// --- Statement Emitters (Stubs/Simplified) ---
static void emit_statement(MirEmitter *emitter, BasicAstNode *stmt_node) {
    if (!stmt_node) return;

    switch (stmt_node->type) {
        case AST_NODE_TYPE_REM:
            // Comments generate no code
            break;
        case AST_NODE_TYPE_END:
            // emit_end_statement(emitter, stmt_node);
            {
                MIR_item_t exit_proto = ensure_exit_prototype(emitter);
                MIR_op_t status_op = MIR_new_int_op(emitter->ctx, 0); // Exit with status 0
                MIR_op_t exit_args[] = {status_op};
                MIR_append_insn(emitter->ctx, emitter->current_func_item,
                                MIR_new_call_insn(emitter->ctx, 0, exit_proto, 0, NULL, 1, exit_args)); // 0 for reg_ops result
                // MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_ret_insn(emitter->ctx, 1, &status_op)); // if main returns status
            }
            break;
        case AST_NODE_TYPE_GOTO:
            // emit_goto_statement(emitter, stmt_node);
            {
                MIR_label_t target_label = get_or_create_mir_label(emitter, stmt_node->data.goto_stmt.target_line_number);
                MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_jmp_insn(emitter->ctx, 0, target_label));
            }
            break;
        case AST_NODE_TYPE_LET:
            // emit_let_statement(emitter, stmt_node);
            emit_let_statement(emitter, stmt_node);
            break;
        case AST_NODE_TYPE_PRINT:
            // emit_print_statement(emitter, stmt_node);
            emit_print_statement(emitter, stmt_node);
            break;
        // TODO: Add other statements
        default:
            fprintf(stderr, "MIR Emitter: Unsupported statement type: %d\n", stmt_node->type);
            // Potentially use semantic_error or a new emitter_error
            break;
    }
}

// --- Expression Emitters ---

// Forward declare complex expression emitters if they call each other recursively in a way that needs it
static MIR_op_t emit_binary_expr(MirEmitter *emitter, BasicAstNode *bin_expr_node);
static MIR_op_t emit_unary_expr(MirEmitter *emitter, BasicAstNode *unary_expr_node);
static MIR_op_t emit_variable_expr(MirEmitter *emitter, BasicAstNode *var_node);


static MIR_op_t emit_literal_expr(MirEmitter *emitter, BasicAstNode *literal_node) {
    switch (literal_node->type) {
        case AST_NODE_TYPE_NUMBER_LITERAL: {
            double val = literal_node->data.number_literal.value;
            BasicVariableType basic_type = literal_node->data.number_literal.num_type;
            switch (basic_type) {
                case BASIC_VAR_TYPE_INTEGER: return MIR_new_int_op(emitter->ctx, (int64_t)val);
                case BASIC_VAR_TYPE_SINGLE:  return MIR_new_float_op(emitter->ctx, (float)val);
                case BASIC_VAR_TYPE_DOUBLE:  return MIR_new_double_op(emitter->ctx, val);
                default: // Should be typed by semantic pass, fallback to double if unsure
                    fprintf(stderr, "Warning: Number literal with unspecific type %d, defaulting to double.\n", basic_type);
                    return MIR_new_double_op(emitter->ctx, val);
            }
        }
        case AST_NODE_TYPE_STRING_LITERAL: {
            MIR_item_t str_data_item = create_string_literal_item(emitter, literal_node->data.string_literal.value);
            return MIR_new_ref_op(emitter->ctx, 0, str_data_item); // module_id 0 for current module
        }
        default:
            fprintf(stderr, "MIR Emitter: emit_literal_expr called with non-literal type %d\n", literal_node->type);
            return MIR_new_int_op(emitter->ctx, 0); // Error/dummy
    }
}

static MIR_op_t emit_variable_expr(MirEmitter *emitter, BasicAstNode *var_node) {
    assert(var_node->type == AST_NODE_TYPE_VARIABLE);
    Symbol *symbol = symbol_table_get(emitter->symbol_table, var_node->data.variable.name);
    if (!symbol) {
        // This should ideally be caught in semantic analysis
        fprintf(stderr, "Error: Variable '%s' not found in symbol table during MIR emission.\n", var_node->data.variable.name);
        // Create a dummy on the fly to allow continuation, or handle error more robustly
        symbol = symbol_table_put(emitter->symbol_table, var_node->data.variable.name,
                                  var_node->data.variable.var_type, var_node);
        if(!symbol) return MIR_new_int_op(emitter->ctx, 0); // Total failure
    }

    // TODO: Handle array variable access var_node->data.variable.dimensions
    if (var_node->data.variable.dimensions) {
        fprintf(stderr, "MIR Emitter: Array variable access for '%s' not yet implemented.\n", symbol->name);
        // Placeholder: return dummy op for array access
        return MIR_new_int_op(emitter->ctx, 0);
    }

    MIR_reg_t var_reg = get_mir_reg_for_variable(emitter, symbol);
    return MIR_new_reg_op(emitter->ctx, var_reg);
}


static MIR_op_t emit_expression(MirEmitter *emitter, BasicAstNode *expr_node) {
    if (!expr_node) {
        fprintf(stderr, "MIR Emitter: emit_expression called with NULL node.\n");
        return MIR_new_int_op(emitter->ctx, 0); // Error/dummy
    }

    switch (expr_node->type) {
        case AST_NODE_TYPE_NUMBER_LITERAL:
        case AST_NODE_TYPE_STRING_LITERAL:
            return emit_literal_expr(emitter, expr_node);
        case AST_NODE_TYPE_VARIABLE:
            return emit_variable_expr(emitter, expr_node);
        case AST_NODE_TYPE_BINARY_EXPR:
            return emit_binary_expr(emitter, expr_node);
        case AST_NODE_TYPE_UNARY_EXPR:
            return emit_unary_expr(emitter, expr_node);
        // TODO: Handle other expression types (function calls, etc.)
        default:
            fprintf(stderr, "MIR Emitter: Unsupported expression AST node type: %d\n", expr_node->type);
            return MIR_new_int_op(emitter->ctx, 0); // Error/dummy
    }
}


// Helper for emit_binary_expr and emit_let_statement to handle type conversions
static MIR_op_t ensure_operand_type(MirEmitter *emitter, MIR_op_t op, MIR_type_t target_type, MIR_type_t current_op_type) {
    if (current_op_type == target_type) {
        return op;
    }

    MIR_insn_code_t conv_code = MIR_INSN_BOUND;
    if (target_type == MIR_T_D) {
        if (current_op_type == MIR_T_F) conv_code = MIR_F2D;
        else if (current_op_type == MIR_T_I64) conv_code = MIR_I2D; // Assuming I64 for int
        // Add other integer types if used (I32, I16 etc.)
    } else if (target_type == MIR_T_F) {
        if (current_op_type == MIR_T_D) conv_code = MIR_D2F;
        else if (current_op_type == MIR_T_I64) conv_code = MIR_I2F;
    } else if (target_type == MIR_T_I64) { // Assuming target is I64 for integer operations
        if (current_op_type == MIR_T_D) conv_code = MIR_D2I; // Truncates
        else if (current_op_type == MIR_T_F) conv_code = MIR_F2I; // Truncates
    }
    // Add more conversions as needed (e.g., between different int sizes, or to/from P for specific cases)

    if (conv_code != MIR_INSN_BOUND) {
        MIR_reg_t temp_reg = get_new_temp_mir_reg(emitter, target_type);
        MIR_op_t temp_op = MIR_new_reg_op(emitter->ctx, temp_reg);
        MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_insn(emitter->ctx, conv_code, 0, 2, temp_op, op));
        return temp_op;
    }
    fprintf(stderr, "Warning: No conversion path from MIR type %d to %d. Returning original operand.\n", current_op_type, target_type);
    return op; // Return original if no conversion found (might lead to MIR validation errors)
}


static MIR_op_t emit_binary_expr(MirEmitter *emitter, BasicAstNode *bin_expr_node) {
    MIR_op_t left_op_orig = emit_expression(emitter, bin_expr_node->data.binary_expr.left);
    MIR_op_t right_op_orig = emit_expression(emitter, bin_expr_node->data.binary_expr.right);

    BasicVariableType left_basic_type = get_expression_type(emitter->symbol_table, bin_expr_node->data.binary_expr.left);
    BasicVariableType right_basic_type = get_expression_type(emitter->symbol_table, bin_expr_node->data.binary_expr.right);

    MIR_type_t left_mir_type = MIR_op_type(emitter->ctx, left_op_orig, emitter->current_func_item->u.func);
    MIR_type_t right_mir_type = MIR_op_type(emitter->ctx, right_op_orig, emitter->current_func_item->u.func);

    MIR_type_t result_mir_type = MIR_T_UNDEF;
    const char* op_str = bin_expr_node->data.binary_expr.operator;

    // Determine result type based on BASIC rules (e.g. float division, string concat)
    // and operand types.
    if (strcmp(op_str, "+") == 0 && (left_basic_type == BASIC_VAR_TYPE_STRING || right_basic_type == BASIC_VAR_TYPE_STRING)) {
        // TODO: String concatenation - needs runtime call (e.g. strcat)
        fprintf(stderr, "MIR Emitter: String concatenation '+' not yet implemented.\n");
        return MIR_new_ref_op(emitter->ctx, 0, create_string_literal_item(emitter, "CONCAT_TODO"));
    } else if (strcmp(op_str, "/") == 0) { // Float division
        result_mir_type = MIR_T_D; // Prefer double for division result generally
        if (left_basic_type == BASIC_VAR_TYPE_SINGLE && right_basic_type == BASIC_VAR_TYPE_SINGLE) {
            result_mir_type = MIR_T_F;
        }
    } else { // Numeric operations
        if (left_mir_type == MIR_T_D || right_mir_type == MIR_T_D) result_mir_type = MIR_T_D;
        else if (left_mir_type == MIR_T_F || right_mir_type == MIR_T_F) result_mir_type = MIR_T_F;
        else if (left_mir_type == MIR_T_I64 || right_mir_type == MIR_T_I64) result_mir_type = MIR_T_I64;
        else {
             fprintf(stderr, "Could not determine result type for binary op '%s' with MIR types %d and %d\n", op_str, left_mir_type, right_mir_type);
             result_mir_type = MIR_T_D; // Fallback
        }
    }

    // Ensure operands are of the chosen result_mir_type (or compatible for the op)
    MIR_op_t left_op = ensure_operand_type(emitter, left_op_orig, result_mir_type, left_mir_type);
    MIR_op_t right_op = ensure_operand_type(emitter, right_op_orig, result_mir_type, right_mir_type);

    MIR_reg_t dest_reg = get_new_temp_mir_reg(emitter, result_mir_type);
    MIR_op_t dest_op = MIR_new_reg_op(emitter->ctx, dest_reg);
    MIR_insn_code_t mir_code = MIR_INSN_BOUND; // Invalid default

    const char* op_str = bin_expr_node->data.binary_expr.operator; // op_str defined twice

    if (strcmp(op_str, "+") == 0) {
        if (result_mir_type == MIR_T_D) mir_code = MIR_DADD;
        else if (result_mir_type == MIR_T_F) mir_code = MIR_FADD;
        else mir_code = MIR_ADD; // Default to integer ADD
    } else if (strcmp(op_str, "-") == 0) {
        if (result_mir_type == MIR_T_D) mir_code = MIR_DSUB;
        else if (result_mir_type == MIR_T_F) mir_code = MIR_FSUB;
        else mir_code = MIR_SUB;
    } else if (strcmp(op_str, "*") == 0) {
        if (result_mir_type == MIR_T_D) mir_code = MIR_DMUL;
        else if (result_mir_type == MIR_T_F) mir_code = MIR_FMUL;
        else mir_code = MIR_MUL;
    } else if (strcmp(op_str, "/") == 0) {
        // BASIC '/' is float division
        if (result_mir_type == MIR_T_D || left_mir_type == MIR_T_D || right_mir_type == MIR_T_D) {
             mir_code = MIR_DDIV; result_mir_type = MIR_T_D; // Ensure result is double
        } else {
             mir_code = MIR_FDIV; result_mir_type = MIR_T_F; // Default to float
        }
        // TODO: ensure operands are converted to float/double before division if they are int
    } else if (strcmp(op_str, "\\") == 0) { // Integer division
        mir_code = MIR_DIV; // Signed integer division
        result_mir_type = MIR_T_I64; // ensure result is int
        // TODO: ensure operands are converted to int
    }
    // TODO: Add MOD, relational operators (CMP then JMP), logical operators (AND, OR, XOR)

    if (mir_code != MIR_INSN_BOUND) {
         // Update dest_op if result_mir_type changed due to op (e.g. / always float)
        if (MIR_op_type(emitter->ctx, dest_op, emitter->current_func_item->u.func) != result_mir_type) {
            dest_reg = get_new_temp_mir_reg(emitter, result_mir_type);
            dest_op = MIR_new_reg_op(emitter->ctx, dest_reg);
        }
        MIR_append_insn(emitter->ctx, emitter->current_func_item,
                        MIR_new_insn(emitter->ctx, mir_code, 0, 3, dest_op, left_op, right_op));
    } else {
        fprintf(stderr, "MIR Emitter: Unsupported binary operator: %s\n", op_str);
        return MIR_new_int_op(emitter->ctx, 0); // Error/dummy
    }
    return dest_op;
}

static MIR_op_t emit_unary_expr(MirEmitter *emitter, BasicAstNode *unary_expr_node) {
    MIR_op_t operand_op = emit_expression(emitter, unary_expr_node->data.unary_expr.operand);
    MIR_type_t operand_mir_type = MIR_op_type(emitter->ctx, operand_op, emitter->current_func_item->u.func);

    MIR_reg_t dest_reg = get_new_temp_mir_reg(emitter, operand_mir_type); // Result type same as operand
    MIR_op_t dest_op = MIR_new_reg_op(emitter->ctx, dest_reg);
    MIR_insn_code_t mir_code = MIR_INSN_BOUND;

    const char* op_str = unary_expr_node->data.unary_expr.operator;

    if (strcmp(op_str, "-") == 0) {
        if (operand_mir_type == MIR_T_D) mir_code = MIR_DNEG;
        else if (operand_mir_type == MIR_T_F) mir_code = MIR_FNEG;
        else mir_code = MIR_NEG; // Integer NEG
    }
    // TODO: Handle NOT (usually on integers)

    if (mir_code != MIR_INSN_BOUND) {
        MIR_append_insn(emitter->ctx, emitter->current_func_item,
                        MIR_new_insn(emitter->ctx, mir_code, 0, 2, dest_op, operand_op));
    } else {
        fprintf(stderr, "MIR Emitter: Unsupported unary operator: %s\n", op_str);
        return MIR_new_int_op(emitter->ctx, 0); // Error/dummy
    }
    return dest_op;
}


// Dummy get_expression_type needed for emit_binary_expr, should be from semantic analysis phase
// or part of MirEmitter if it needs to query types dynamically.
// For now, this is a simplified stub.
static BasicVariableType get_expression_type(SymbolTable *st, BasicAstNode *expr_node) {
    if (!expr_node) return BASIC_VAR_TYPE_DEFAULT;
    switch (expr_node->type) {
        case AST_NODE_TYPE_NUMBER_LITERAL: return expr_node->data.number_literal.num_type;
        case AST_NODE_TYPE_STRING_LITERAL: return BASIC_VAR_TYPE_STRING;
        case AST_NODE_TYPE_VARIABLE: {
            Symbol *s = symbol_table_get(st, expr_node->data.variable.name);
            return s ? s->type : expr_node->data.variable.var_type; // Use annotated type or fallback
        }
        // For binary/unary, it's more complex, this is oversimplified:
        case AST_NODE_TYPE_BINARY_EXPR: {
             BasicVariableType lt = get_expression_type(st, expr_node->data.binary_expr.left);
             BasicVariableType rt = get_expression_type(st, expr_node->data.binary_expr.right);
             if (expr_node->data.binary_expr.operator[0] == '+') { // '+' can be string concat
                 if (lt == BASIC_VAR_TYPE_STRING && rt == BASIC_VAR_TYPE_STRING) return BASIC_VAR_TYPE_STRING;
             }
             // Simple promotion for numerics
             if (lt == BASIC_VAR_TYPE_DOUBLE || rt == BASIC_VAR_TYPE_DOUBLE) return BASIC_VAR_TYPE_DOUBLE;
             if (lt == BASIC_VAR_TYPE_SINGLE || rt == BASIC_VAR_TYPE_SINGLE) return BASIC_VAR_TYPE_SINGLE;
             return BASIC_VAR_TYPE_INTEGER;
        }
        case AST_NODE_TYPE_UNARY_EXPR: return get_expression_type(st, expr_node->data.unary_expr.operand);
        default: return BASIC_VAR_TYPE_DEFAULT;
    }
}
// TODO: Implement detailed statement and expression emitters
// emit_let_statement, emit_print_statement


// --- Statement Emitters (Continued) ---

static void emit_let_statement(MirEmitter *emitter, BasicAstNode *let_node) {
    assert(let_node->type == AST_NODE_TYPE_LET);
    BasicAstNode *var_ast_node = let_node->data.let_stmt.variable;
    BasicAstNode *expr_ast_node = let_node->data.let_stmt.expression;

    // Get LHS variable's MIR register and type
    Symbol *lhs_symbol = symbol_table_get(emitter->symbol_table, var_ast_node->data.variable.name);
    if (!lhs_symbol) {
        // Should have been caught by semantic analysis or created if implicit
        fprintf(stderr, "Critical Error: LHS variable '%s' not in symbol table for LET.\n", var_ast_node->data.variable.name);
        // Attempt to recover by creating a symbol on the fly based on AST info
        // This indicates a probable issue in the semantic analysis pass or its usage.
        lhs_symbol = symbol_table_put(emitter->symbol_table, var_ast_node->data.variable.name,
                                      var_ast_node->data.variable.var_type, var_ast_node);
        if (!lhs_symbol) return; // Cannot proceed
    }
    MIR_reg_t dest_reg = get_mir_reg_for_variable(emitter, lhs_symbol);
    MIR_type_t dest_mir_type = basic_type_to_mir_type(lhs_symbol->type);

    // Emit RHS expression
    MIR_op_t src_op_orig = emit_expression(emitter, expr_ast_node);
    MIR_type_t src_mir_type_orig = MIR_op_type(emitter->ctx, src_op_orig, emitter->current_func_item->u.func);

    // Ensure RHS operand type is compatible with LHS type
    MIR_op_t src_op_converted = ensure_operand_type(emitter, src_op_orig, dest_mir_type, src_mir_type_orig);

    // Select MIR MOV instruction based on type
    MIR_insn_code_t mov_code;
    switch (dest_mir_type) {
        case MIR_T_F:  mov_code = MIR_FMOV; break;
        case MIR_T_D:  mov_code = MIR_DMOV; break;
        case MIR_T_I8: case MIR_T_I16: case MIR_T_I32: case MIR_T_I64:
        case MIR_T_U8: case MIR_T_U16: case MIR_T_U32: case MIR_T_U64:
        case MIR_T_P: // For string pointers
            mov_code = MIR_MOV; break;
        default:
            fprintf(stderr, "Error: Unsupported destination type %d in LET statement for var '%s'\n",
                    dest_mir_type, lhs_symbol->name);
            return;
    }
    MIR_append_insn(emitter->ctx, emitter->current_func_item,
                    MIR_new_insn(emitter->ctx, mov_code, 0, 2, MIR_new_reg_op(emitter->ctx, dest_reg), src_op_converted));
}


// Helper to get/create format string items for printf
static MIR_item_t get_printf_format_item(MirEmitter *emitter, BasicVariableType var_type, int add_newline) {
    char fmt_name_buf[32];
    char fmt_str_buf[10];
    const char* nl = add_newline ? "\\n" : ""; // MIR uses C escapes for strings

    switch (var_type) {
        case BASIC_VAR_TYPE_INTEGER:
            snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_i64%s", add_newline ? "_nl" : "");
            snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%lld%s", nl); // Assuming I64 maps to lld
            break;
        case BASIC_VAR_TYPE_SINGLE:
            snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_f%s", add_newline ? "_nl" : "");
            snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%f%s", nl);
            break;
        case BASIC_VAR_TYPE_DOUBLE:
        case BASIC_VAR_TYPE_DEFAULT: // Default numeric often double for printf
            snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_d%s", add_newline ? "_nl" : "");
            snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%lf%s", nl);
            break;
        case BASIC_VAR_TYPE_STRING:
            snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_s%s", add_newline ? "_nl" : "");
            snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%s%s", nl);
            break;
        default:
            fprintf(stderr, "Warning: Unknown variable type %d for printf format string. Defaulting to string.\n", var_type);
            snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_s_unknown%s", add_newline ? "_nl" : "");
            snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%s%s", nl); // Fallback
            break;
    }

    MIR_item_t fmt_item = MIR_get_module_item(emitter->ctx, emitter->current_module, fmt_name_buf);
    if (fmt_item == NULL) {
        fmt_item = MIR_new_string_data(emitter->ctx, fmt_name_buf, fmt_str_buf);
    }
    return fmt_item;
}


static void emit_print_statement(MirEmitter *emitter, BasicAstNode *print_node) {
    assert(print_node->type == AST_NODE_TYPE_PRINT);
    MIR_item_t printf_proto = ensure_printf_prototype(emitter);

    BasicAstNode *current_item_ast = print_node->data.print_stmt.print_items;
    if (current_item_ast == NULL) {
        MIR_item_t nl_only_fmt = MIR_get_module_item(emitter->ctx, emitter->current_module, "fmt_just_nl");
        if (nl_only_fmt == NULL) {
             nl_only_fmt = MIR_new_string_data(emitter->ctx, "fmt_just_nl", "\\n");
        }
        MIR_op_t printf_args[] = {MIR_new_ref_op(emitter->ctx, 0, nl_only_fmt)};
        MIR_append_insn(emitter->ctx, emitter->current_func_item,
                        MIR_new_call_insn(emitter->ctx, 0, printf_proto, 0, NULL, 1, printf_args));
        return;
    }

    while (current_item_ast) {
        MIR_op_t val_op_orig = emit_expression(emitter, current_item_ast);
        BasicVariableType basic_type = get_expression_type(emitter->symbol_table, current_item_ast);
        MIR_type_t val_mir_type = MIR_op_type(emitter->ctx, val_op_orig, emitter->current_func_item->u.func);

        // For PRINT, always add a newline after each resolved item for now.
        // TODO: Implement comma (tab) and semicolon (close print) logic.
        MIR_item_t fmt_item = get_printf_format_item(emitter, basic_type, TRUE);
        MIR_op_t val_op_for_call = val_op_orig;

        if (val_mir_type == MIR_T_F) { // Floats promoted to double for variadic printf
            val_op_for_call = ensure_operand_type(emitter, val_op_orig, MIR_T_D, MIR_T_F);
        }
        // Other types like I8,I16,I32 would also be promoted (typically to I64 or int)
        // MIR_T_I64 is fine for %lld. MIR_T_P for %s.

        MIR_op_t printf_args[] = {MIR_new_ref_op(emitter->ctx, 0, fmt_item), val_op_for_call};
        MIR_append_insn(emitter->ctx, emitter->current_func_item,
                        MIR_new_call_insn(emitter->ctx, 0, printf_proto, 0, NULL, 2, printf_args));

        current_item_ast = current_item_ast->next;
    }
}
