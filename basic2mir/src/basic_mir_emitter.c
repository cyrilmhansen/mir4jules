#include "basic_mir_emitter.h"
#include "mir-gen.h"

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
static MIR_op_t ensure_operand_type(MirEmitter *emitter, MIR_op_t op, MIR_type_t target_type, MIR_type_t current_op_type);
static BasicVariableType get_expression_type(SymbolTable *st, BasicAstNode *expr_node); // Stub from previous steps

// New forward declarations for FOR/NEXT
static void emit_for_statement(MirEmitter *emitter, BasicAstNode *for_node);
static void emit_next_statement(MirEmitter *emitter, BasicAstNode *next_node);
static void emit_if_statement(MirEmitter *emitter, BasicAstNode *if_node); // Already declared if split before
static void emit_let_statement(MirEmitter *emitter, BasicAstNode *let_node);
static void emit_print_statement(MirEmitter *emitter, BasicAstNode *print_node);


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
    return NULL;
}

static MIR_label_t get_or_create_mir_label(MirEmitter *emitter, int32_t basic_line_number) {
    MIR_label_t label = label_map_get(emitter, basic_line_number);
    if (label == NULL) {
        char label_name[32];
        snprintf(label_name, sizeof(label_name), "L%d", basic_line_number);
        label = MIR_new_label(emitter->ctx);
        label_map_put(emitter, basic_line_number, label);
    }
    return label;
}

// --- Var MIR Info Map Implementation ---
static unsigned int var_map_hash(const char* var_name) {
    unsigned long hash = 5381;
    int c;
    while ((c = *var_name++))
        hash = ((hash << 5) + hash) + c;
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
        case BASIC_VAR_TYPE_INTEGER: return MIR_T_I64;
        case BASIC_VAR_TYPE_SINGLE:  return MIR_T_F;
        case BASIC_VAR_TYPE_DOUBLE:  return MIR_T_D;
        case BASIC_VAR_TYPE_STRING:  return MIR_T_P;
        case BASIC_VAR_TYPE_DEFAULT: return MIR_T_F;
        default:
            fprintf(stderr, "Unknown BasicVariableType: %d\n", basic_type);
            return MIR_T_I64;
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
    emitter->printf_proto_item = NULL;
    emitter->exit_proto_item = NULL;
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
    free(emitter);
}

// --- Register Management ---
static MIR_reg_t get_mir_reg_for_variable(MirEmitter *emitter, Symbol* symbol) {
    if (!symbol || !symbol->name) {
        fprintf(stderr, "Error: Cannot get MIR register for null symbol or unnamed symbol.\n");
        return MIR_REG_BOUND;
    }
    VarMirInfo *var_info = get_var_mir_info(emitter, symbol->name);
    if (var_info) {
        MIR_type_t expected_mir_type = basic_type_to_mir_type(symbol->type);
        if (var_info->mir_type != expected_mir_type) {
             // This might happen if a variable is used then DIM'd, or type changes.
             // For now, we trust the symbol table's current type.
             // A more complex system might re-allocate or error.
            var_info->mir_type = expected_mir_type; // Update it. Reg itself might be an issue if type changed fundamentally.
        }
        return var_info->mir_reg;
    }
    char reg_name_basic[256];
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

// --- Utility for C lib prototypes ---
static MIR_item_t ensure_printf_prototype(MirEmitter *emitter) {
    if (emitter->printf_proto_item == NULL) {
        MIR_type_t ret_type = MIR_T_I32;
        MIR_var_t args[] = {{"fmt", MIR_T_P}};
        emitter->printf_proto_item = MIR_new_proto(emitter->ctx, "printf_proto", 1, &ret_type, 1, args, TRUE);
        MIR_new_import(emitter->ctx, "printf"); // Actual link name
    }
    return emitter->printf_proto_item;
}

static MIR_item_t ensure_exit_prototype(MirEmitter *emitter) {
    if (emitter->exit_proto_item == NULL) {
        MIR_type_t ret_type = MIR_T_I32;
        MIR_var_t args[] = {{"status", MIR_T_I32}};
        emitter->exit_proto_item = MIR_new_proto(emitter->ctx, "exit_proto", 1, &ret_type, 1, args, FALSE);
        MIR_new_import(emitter->ctx, "exit");
    }
    return emitter->exit_proto_item;
}

static MIR_item_t create_string_literal_item(MirEmitter *emitter, const char* str_val) {
    char item_name[64];
    snprintf(item_name, sizeof(item_name), "str%d", emitter->string_literal_count++);
    return MIR_new_string_data(emitter->ctx, item_name, str_val);
}

// --- Main Module Generation ---
MIR_module_t mir_emitter_generate_module(MirEmitter *emitter, BasicAstNode *program_ast, const char *module_name) {
    assert(program_ast && program_ast->type == AST_NODE_TYPE_PROGRAM);
    emitter->current_module = MIR_new_module(emitter->ctx, module_name);
    MIR_type_t main_ret_type = MIR_T_I32;
    emitter->current_func_item = MIR_new_func(emitter->ctx, "basic_main", 1, &main_ret_type, 0, NULL);
    emit_program_lines(emitter, program_ast);
    MIR_finish_func(emitter->ctx);
    MIR_finish_module(emitter->ctx);
    return emitter->current_module;
}

static void emit_program_lines(MirEmitter *emitter, BasicAstNode *program_node) {
    BasicAstNode *current_line_node = program_node->data.program.program_lines;
    while (current_line_node) {
        if (current_line_node->type == AST_NODE_TYPE_PROGRAM_LINE) {
            MIR_label_t line_label = get_or_create_mir_label(emitter, current_line_node->line_number);
            MIR_append_insn(emitter->ctx, emitter->current_func_item, line_label);
            if (current_line_node->data.program_line.statement) {
                emit_statement(emitter, current_line_node->data.program_line.statement);
            }
        }
        current_line_node = current_line_node->data.program_line.next_line;
    }
}

// --- Statement Emitters ---
static void emit_statement(MirEmitter *emitter, BasicAstNode *stmt_node) {
    if (!stmt_node) return;
    switch (stmt_node->type) {
        case AST_NODE_TYPE_REM: break;
        case AST_NODE_TYPE_END: emit_end_statement(emitter, stmt_node); break;
        case AST_NODE_TYPE_GOTO: emit_goto_statement(emitter, stmt_node); break;
        case AST_NODE_TYPE_LET: emit_let_statement(emitter, stmt_node); break;
        case AST_NODE_TYPE_PRINT: emit_print_statement(emitter, stmt_node); break;
        case AST_NODE_TYPE_IF_THEN_ELSE: emit_if_statement(emitter, stmt_node); break;
        case AST_NODE_TYPE_FOR: emit_for_statement(emitter, stmt_node); break;
        case AST_NODE_TYPE_NEXT: emit_next_statement(emitter, stmt_node); break;
        default:
            fprintf(stderr, "MIR Emitter: Unsupported statement type: %d at line %d\n", stmt_node->type, stmt_node->line_number);
            break;
    }
}

static void emit_end_statement(MirEmitter *emitter, BasicAstNode *end_node) {
    MIR_item_t exit_proto = ensure_exit_prototype(emitter);
    MIR_op_t status_op = MIR_new_int_op(emitter->ctx, 0);
    MIR_op_t exit_args[] = {status_op};
    MIR_append_insn(emitter->ctx, emitter->current_func_item,
                    MIR_new_call_insn(emitter->ctx, 0, exit_proto, 0, NULL, 1, exit_args));
}

static void emit_goto_statement(MirEmitter *emitter, BasicAstNode *goto_node) {
    MIR_label_t target_label = get_or_create_mir_label(emitter, goto_node->data.goto_stmt.target_line_number);
    MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_jmp_insn(emitter->ctx, 0, target_label));
}


// --- Expression Emitters (simplified stubs from previous steps, to be expanded) ---
static MIR_op_t emit_literal_expr(MirEmitter *emitter, BasicAstNode *literal_node);
static MIR_op_t emit_variable_expr(MirEmitter *emitter, BasicAstNode *var_node);
static MIR_op_t emit_binary_expr(MirEmitter *emitter, BasicAstNode *bin_expr_node);
static MIR_op_t emit_unary_expr(MirEmitter *emitter, BasicAstNode *unary_expr_node);

// (Implementations for emit_literal_expr, emit_variable_expr, emit_binary_expr, emit_unary_expr,
// ensure_operand_type, get_expression_type, emit_let_statement, emit_print_statement, emit_if_statement
// are assumed to be here from previous steps. They are long, so not repeated in this diff block)

// Dummy get_expression_type (should use semantic info)
static BasicVariableType get_expression_type(SymbolTable *st, BasicAstNode *expr_node) {
    if (!expr_node) return BASIC_VAR_TYPE_DEFAULT;
    switch (expr_node->type) {
        case AST_NODE_TYPE_NUMBER_LITERAL: return expr_node->data.number_literal.num_type;
        case AST_NODE_TYPE_STRING_LITERAL: return BASIC_VAR_TYPE_STRING;
        case AST_NODE_TYPE_VARIABLE: {
            Symbol *s = st ? symbol_table_get(st, expr_node->data.variable.name) : NULL;
            return s ? s->type : expr_node->data.variable.var_type;
        }
        case AST_NODE_TYPE_BINARY_EXPR: {
             BasicVariableType lt = get_expression_type(st, expr_node->data.binary_expr.left);
             BasicVariableType rt = get_expression_type(st, expr_node->data.binary_expr.right);
             if (expr_node->data.binary_expr.operator[0] == '+') {
                 if (lt == BASIC_VAR_TYPE_STRING && rt == BASIC_VAR_TYPE_STRING) return BASIC_VAR_TYPE_STRING;
             }
             if (lt == BASIC_VAR_TYPE_DOUBLE || rt == BASIC_VAR_TYPE_DOUBLE) return BASIC_VAR_TYPE_DOUBLE;
             if (lt == BASIC_VAR_TYPE_SINGLE || rt == BASIC_VAR_TYPE_SINGLE) return BASIC_VAR_TYPE_SINGLE;
             if (lt == BASIC_VAR_TYPE_INTEGER && rt == BASIC_VAR_TYPE_INTEGER) return BASIC_VAR_TYPE_INTEGER;
             return (lt == BASIC_VAR_TYPE_DEFAULT) ? rt : lt; // Simplistic promotion
        }
        case AST_NODE_TYPE_UNARY_EXPR: return get_expression_type(st, expr_node->data.unary_expr.operand);
        default: return BASIC_VAR_TYPE_DEFAULT;
    }
}


static MIR_op_t ensure_operand_type(MirEmitter *emitter, MIR_op_t op, MIR_type_t target_type, MIR_type_t current_op_type) {
    if (current_op_type == target_type) return op;
    MIR_insn_code_t conv_code = MIR_INSN_BOUND;
    if (target_type == MIR_T_D) {
        if (current_op_type == MIR_T_F) conv_code = MIR_F2D;
        else if (current_op_type == MIR_T_I64) conv_code = MIR_I2D;
    } else if (target_type == MIR_T_F) {
        if (current_op_type == MIR_T_D) conv_code = MIR_D2F;
        else if (current_op_type == MIR_T_I64) conv_code = MIR_I2F;
    } else if (target_type == MIR_T_I64) {
        if (current_op_type == MIR_T_D) conv_code = MIR_D2I;
        else if (current_op_type == MIR_T_F) conv_code = MIR_F2I;
    }
    if (conv_code != MIR_INSN_BOUND) {
        MIR_reg_t temp_reg = get_new_temp_mir_reg(emitter, target_type);
        MIR_op_t temp_op = MIR_new_reg_op(emitter->ctx, temp_reg);
        MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_insn(emitter->ctx, conv_code, 0, 2, temp_op, op));
        return temp_op;
    }
    return op;
}

static MIR_op_t emit_literal_expr(MirEmitter *emitter, BasicAstNode *literal_node) {
    switch (literal_node->type) {
        case AST_NODE_TYPE_NUMBER_LITERAL: {
            double val = literal_node->data.number_literal.value;
            BasicVariableType basic_type = literal_node->data.number_literal.num_type;
            switch (basic_type) {
                case BASIC_VAR_TYPE_INTEGER: return MIR_new_int_op(emitter->ctx, (int64_t)val);
                case BASIC_VAR_TYPE_SINGLE:  return MIR_new_float_op(emitter->ctx, (float)val);
                case BASIC_VAR_TYPE_DOUBLE:  return MIR_new_double_op(emitter->ctx, val);
                default: return MIR_new_double_op(emitter->ctx, val);
            }
        }
        case AST_NODE_TYPE_STRING_LITERAL: {
            MIR_item_t str_data_item = create_string_literal_item(emitter, literal_node->data.string_literal.value);
            return MIR_new_ref_op(emitter->ctx, 0, str_data_item);
        }
        default: return MIR_new_int_op(emitter->ctx, 0);
    }
}

static MIR_op_t emit_variable_expr(MirEmitter *emitter, BasicAstNode *var_node) {
    assert(var_node->type == AST_NODE_TYPE_VARIABLE);
    Symbol *symbol = symbol_table_get(emitter->symbol_table, var_node->data.variable.name);
    if (!symbol) {
        symbol = symbol_table_put(emitter->symbol_table, var_node->data.variable.name,
                                  var_node->data.variable.var_type, var_node);
        if(!symbol) return MIR_new_int_op(emitter->ctx, 0);
    }
    if (var_node->data.variable.dimensions) {
        fprintf(stderr, "MIR Emitter: Array variable access for '%s' not yet implemented.\n", symbol->name);
        return MIR_new_int_op(emitter->ctx, 0);
    }
    MIR_reg_t var_reg = get_mir_reg_for_variable(emitter, symbol);
    return MIR_new_reg_op(emitter->ctx, var_reg);
}

static MIR_op_t emit_unary_expr(MirEmitter *emitter, BasicAstNode *unary_expr_node) {
    MIR_op_t operand_op = emit_expression(emitter, unary_expr_node->data.unary_expr.operand);
    MIR_type_t operand_mir_type = MIR_op_type(emitter->ctx, operand_op, emitter->current_func_item->u.func);
    MIR_reg_t dest_reg = get_new_temp_mir_reg(emitter, operand_mir_type);
    MIR_op_t dest_op = MIR_new_reg_op(emitter->ctx, dest_reg);
    MIR_insn_code_t mir_code = MIR_INSN_BOUND;
    const char* op_str = unary_expr_node->data.unary_expr.operator;
    if (strcmp(op_str, "-") == 0) {
        if (operand_mir_type == MIR_T_D) mir_code = MIR_DNEG;
        else if (operand_mir_type == MIR_T_F) mir_code = MIR_FNEG;
        else mir_code = MIR_NEG;
    } else if (strcasecmp(op_str, "NOT") == 0) { // Assuming NOT results in I64 (0 or -1)
        MIR_op_t zero_op = MIR_new_int_op(emitter->ctx, 0);
        operand_op = ensure_operand_type(emitter, operand_op, MIR_T_I64, operand_mir_type); // Ensure operand is integer for comparison
        dest_op = MIR_new_reg_op(emitter->ctx, get_new_temp_mir_reg(emitter, MIR_T_I64)); // Result is integer
        MIR_append_insn(emitter->ctx, emitter->current_func_item,
                        MIR_new_insn(emitter->ctx, MIR_EQ, 0, 3, dest_op, operand_op, zero_op)); // dest = (operand == 0)
        // To make it GW-BASIC like (-1 for true, 0 for false), if dest is 1 (was 0), make it -1. If 0 (was non-zero), keep 0.
        // This is a bit more complex. Simpler: just use 0/1 and let IF handle it.
        // For now, 0/1 is fine.
        return dest_op; // Early exit for NOT as it's more of a comparison
    }
    if (mir_code != MIR_INSN_BOUND) {
        MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_insn(emitter->ctx, mir_code, 0, 2, dest_op, operand_op));
    } else { return MIR_new_int_op(emitter->ctx, 0); }
    return dest_op;
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
    MIR_insn_code_t mir_code = MIR_INSN_BOUND;

    if (strcmp(op_str, "+") == 0 && (left_basic_type == BASIC_VAR_TYPE_STRING || right_basic_type == BASIC_VAR_TYPE_STRING)) {
        fprintf(stderr, "MIR Emitter: String concatenation '+' not yet implemented.\n");
        return MIR_new_ref_op(emitter->ctx, 0, create_string_literal_item(emitter, "CONCAT_TODO"));
    } else if (strcmp(op_str, "/") == 0) {
        result_mir_type = (left_basic_type == BASIC_VAR_TYPE_SINGLE && right_basic_type == BASIC_VAR_TYPE_SINGLE) ? MIR_T_F : MIR_T_D;
    } else {
        if (left_mir_type == MIR_T_D || right_mir_type == MIR_T_D) result_mir_type = MIR_T_D;
        else if (left_mir_type == MIR_T_F || right_mir_type == MIR_T_F) result_mir_type = MIR_T_F;
        else result_mir_type = MIR_T_I64;
    }

    MIR_op_t left_op = ensure_operand_type(emitter, left_op_orig, result_mir_type, left_mir_type);
    MIR_op_t right_op = ensure_operand_type(emitter, right_op_orig, result_mir_type, right_mir_type);
    MIR_reg_t dest_reg = get_new_temp_mir_reg(emitter, result_mir_type);
    MIR_op_t dest_op = MIR_new_reg_op(emitter->ctx, dest_reg);

    if (strcmp(op_str, "+") == 0) mir_code = (result_mir_type == MIR_T_D) ? MIR_DADD : (result_mir_type == MIR_T_F) ? MIR_FADD : MIR_ADD;
    else if (strcmp(op_str, "-") == 0) mir_code = (result_mir_type == MIR_T_D) ? MIR_DSUB : (result_mir_type == MIR_T_F) ? MIR_FSUB : MIR_SUB;
    else if (strcmp(op_str, "*") == 0) mir_code = (result_mir_type == MIR_T_D) ? MIR_DMUL : (result_mir_type == MIR_T_F) ? MIR_FMUL : MIR_MUL;
    else if (strcmp(op_str, "/") == 0) mir_code = (result_mir_type == MIR_T_D) ? MIR_DDIV : MIR_FDIV;
    else if (strcmp(op_str, "\\") == 0) { // Integer division
        left_op = ensure_operand_type(emitter, left_op_orig, MIR_T_I64, left_mir_type);
        right_op = ensure_operand_type(emitter, right_op_orig, MIR_T_I64, right_mir_type);
        dest_op = MIR_new_reg_op(emitter->ctx, get_new_temp_mir_reg(emitter, MIR_T_I64)); // Result is I64
        mir_code = MIR_DIV;
    }
    // Relational and Logical ops return I64 (boolean 0 or -1/1)
    else if (strcmp(op_str, "=") == 0) mir_code = (left_mir_type == MIR_T_D) ? MIR_DEQ : (left_mir_type == MIR_T_F) ? MIR_FEQ : MIR_EQ;
    else if (strcmp(op_str, "<>") == 0) mir_code = (left_mir_type == MIR_T_D) ? MIR_DNE : (left_mir_type == MIR_T_F) ? MIR_FNE : MIR_NE;
    else if (strcmp(op_str, "<") == 0) mir_code = (left_mir_type == MIR_T_D) ? MIR_DLT : (left_mir_type == MIR_T_F) ? MIR_FLT : MIR_LT;
    else if (strcmp(op_str, "<=") == 0) mir_code = (left_mir_type == MIR_T_D) ? MIR_DLE : (left_mir_type == MIR_T_F) ? MIR_FLE : MIR_LE;
    else if (strcmp(op_str, ">") == 0) mir_code = (left_mir_type == MIR_T_D) ? MIR_DGT : (left_mir_type == MIR_T_F) ? MIR_FGT : MIR_GT;
    else if (strcmp(op_str, ">=") == 0) mir_code = (left_mir_type == MIR_T_D) ? MIR_DGE : (left_mir_type == MIR_T_F) ? MIR_FGE : MIR_GE;
    // TODO: AND, OR, XOR, etc. Need to ensure operands are integer.

    if (mir_code != MIR_INSN_BOUND) {
        MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_insn(emitter->ctx, mir_code, 0, 3, dest_op, left_op, right_op));
    } else { return MIR_new_int_op(emitter->ctx, 0); }
    return dest_op;
}


static MIR_op_t emit_expression(MirEmitter *emitter, BasicAstNode *expr_node) {
    if (!expr_node) return MIR_new_int_op(emitter->ctx, 0);
    switch (expr_node->type) {
        case AST_NODE_TYPE_NUMBER_LITERAL:
        case AST_NODE_TYPE_STRING_LITERAL: return emit_literal_expr(emitter, expr_node);
        case AST_NODE_TYPE_VARIABLE: return emit_variable_expr(emitter, expr_node);
        case AST_NODE_TYPE_BINARY_EXPR: return emit_binary_expr(emitter, expr_node);
        case AST_NODE_TYPE_UNARY_EXPR: return emit_unary_expr(emitter, expr_node);
        default: return MIR_new_int_op(emitter->ctx, 0);
    }
}


static void emit_let_statement(MirEmitter *emitter, BasicAstNode *let_node) {
    assert(let_node->type == AST_NODE_TYPE_LET);
    BasicAstNode *var_ast_node = let_node->data.let_stmt.variable;
    BasicAstNode *expr_ast_node = let_node->data.let_stmt.expression;
    Symbol *lhs_symbol = symbol_table_get(emitter->symbol_table, var_ast_node->data.variable.name);
    if (!lhs_symbol) {
        lhs_symbol = symbol_table_put(emitter->symbol_table, var_ast_node->data.variable.name,
                                      var_ast_node->data.variable.var_type, var_ast_node);
        if (!lhs_symbol) return;
    }
    MIR_reg_t dest_reg = get_mir_reg_for_variable(emitter, lhs_symbol);
    MIR_type_t dest_mir_type = basic_type_to_mir_type(lhs_symbol->type);
    MIR_op_t src_op_orig = emit_expression(emitter, expr_ast_node);
    MIR_type_t src_mir_type_orig = MIR_op_type(emitter->ctx, src_op_orig, emitter->current_func_item->u.func);
    MIR_op_t src_op_converted = ensure_operand_type(emitter, src_op_orig, dest_mir_type, src_mir_type_orig);
    MIR_insn_code_t mov_code;
    switch (dest_mir_type) {
        case MIR_T_F:  mov_code = MIR_FMOV; break;
        case MIR_T_D:  mov_code = MIR_DMOV; break;
        default: mov_code = MIR_MOV; break; // For I64, P
    }
    MIR_append_insn(emitter->ctx, emitter->current_func_item,
                    MIR_new_insn(emitter->ctx, mov_code, 0, 2, MIR_new_reg_op(emitter->ctx, dest_reg), src_op_converted));
}

static MIR_item_t get_printf_format_item(MirEmitter *emitter, BasicVariableType var_type, int add_newline) {
    char fmt_name_buf[32]; char fmt_str_buf[10];
    const char* nl = add_newline ? "\\n" : "";
    switch (var_type) {
        case BASIC_VAR_TYPE_INTEGER: snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_i64%s", nl?"_nl":""); snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%lld%s", nl); break;
        case BASIC_VAR_TYPE_SINGLE:  snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_f%s", nl?"_nl":""); snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%f%s", nl); break;
        case BASIC_VAR_TYPE_DOUBLE: default: snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_d%s", nl?"_nl":""); snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%lf%s", nl); break;
        case BASIC_VAR_TYPE_STRING:  snprintf(fmt_name_buf, sizeof(fmt_name_buf), "fmt_s%s", nl?"_nl":""); snprintf(fmt_str_buf, sizeof(fmt_str_buf), "%%s%s", nl); break;
    }
    MIR_item_t fmt_item = MIR_get_module_item(emitter->ctx, emitter->current_module, fmt_name_buf);
    if (fmt_item == NULL) fmt_item = MIR_new_string_data(emitter->ctx, fmt_name_buf, fmt_str_buf);
    return fmt_item;
}

static void emit_print_statement(MirEmitter *emitter, BasicAstNode *print_node) {
    assert(print_node->type == AST_NODE_TYPE_PRINT);
    MIR_item_t printf_proto = ensure_printf_prototype(emitter);
    BasicAstNode *current_item_ast = print_node->data.print_stmt.print_items;
    if (current_item_ast == NULL) {
        MIR_item_t nl_only_fmt = MIR_get_module_item(emitter->ctx, emitter->current_module, "fmt_just_nl");
        if (nl_only_fmt == NULL) nl_only_fmt = MIR_new_string_data(emitter->ctx, "fmt_just_nl", "\\n");
        MIR_op_t printf_args[] = {MIR_new_ref_op(emitter->ctx, 0, nl_only_fmt)};
        MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_call_insn(emitter->ctx, 0, printf_proto, 0, NULL, 1, printf_args));
        return;
    }
    while (current_item_ast) {
        MIR_op_t val_op_orig = emit_expression(emitter, current_item_ast);
        BasicVariableType basic_type = get_expression_type(emitter->symbol_table, current_item_ast);
        MIR_type_t val_mir_type = MIR_op_type(emitter->ctx, val_op_orig, emitter->current_func_item->u.func);
        MIR_item_t fmt_item = get_printf_format_item(emitter, basic_type, TRUE);
        MIR_op_t val_op_for_call = val_op_orig;
        if (val_mir_type == MIR_T_F) {
            val_op_for_call = ensure_operand_type(emitter, val_op_orig, MIR_T_D, MIR_T_F);
        }
        MIR_op_t printf_args[] = {MIR_new_ref_op(emitter->ctx, 0, fmt_item), val_op_for_call};
        MIR_append_insn(emitter->ctx, emitter->current_func_item, MIR_new_call_insn(emitter->ctx, 0, printf_proto, 0, NULL, 2, printf_args));
        current_item_ast = current_item_ast->next;
    }
}

static void emit_if_statement(MirEmitter *emitter, BasicAstNode *if_node) {
    assert(if_node->type == AST_NODE_TYPE_IF_THEN_ELSE);
    MIR_context_t ctx = emitter->ctx; MIR_item_t func_item = emitter->current_func_item;
    BasicAstNode *cond_ast = if_node->data.if_then_else_stmt.condition;
    BasicAstNode *then_b = if_node->data.if_then_else_stmt.then_branch;
    BasicAstNode *else_b = if_node->data.if_then_else_stmt.else_branch;
    MIR_label_t then_l = MIR_new_label(ctx), end_if_l = MIR_new_label(ctx);
    MIR_label_t else_l = else_b ? MIR_new_label(ctx) : end_if_l;
    MIR_op_t cond_op = emit_expression(emitter, cond_ast);
    MIR_type_t cond_mir_type = MIR_op_type(ctx, cond_op, func_item->u.func);
    if (cond_mir_type == MIR_T_F || cond_mir_type == MIR_T_D) {
        MIR_reg_t temp_int_reg = get_new_temp_mir_reg(emitter, MIR_T_I64);
        MIR_op_t temp_int_op = MIR_new_reg_op(ctx, temp_int_reg);
        MIR_op_t zero_op = (cond_mir_type == MIR_T_F) ? MIR_new_float_op(ctx,0.0f) : MIR_new_double_op(ctx,0.0);
        MIR_append_insn(ctx, func_item, MIR_new_insn(ctx, (cond_mir_type==MIR_T_F)?MIR_FNE:MIR_DNE, 0,3,temp_int_op,cond_op,zero_op));
        cond_op = temp_int_op;
    } else if (cond_mir_type != MIR_T_I64 && cond_mir_type != MIR_T_I32 && cond_mir_type != MIR_T_I16 && cond_mir_type != MIR_T_I8 &&
               cond_mir_type != MIR_T_U64 && cond_mir_type != MIR_T_U32 && cond_mir_type != MIR_T_U16 && cond_mir_type != MIR_T_U8) {
        // Ensure it's some form of integer if not float/double
        cond_op = ensure_operand_type(emitter, cond_op, MIR_T_I64, cond_mir_type);
    }
    MIR_append_insn(ctx, func_item, MIR_new_insn(ctx, MIR_BT, 0, 2, MIR_new_label_op(ctx, then_l), cond_op));
    MIR_append_insn(ctx, func_item, MIR_new_insn(ctx, MIR_JMP, 0, 1, MIR_new_label_op(ctx, else_l)));
    MIR_append_insn(ctx, func_item, then_l);
    if (then_b) {
        if (then_b->type == AST_NODE_TYPE_GOTO) emit_goto_statement(emitter, then_b);
        else emit_statement(emitter, then_b);
    }
    if (else_b) MIR_append_insn(ctx, func_item, MIR_new_insn(ctx, MIR_JMP, 0, 1, MIR_new_label_op(ctx, end_if_l)));
    if (else_b) {
        MIR_append_insn(ctx, func_item, else_l);
        if (else_b->type == AST_NODE_TYPE_GOTO) emit_goto_statement(emitter, else_b);
        else emit_statement(emitter, else_b);
    }
    MIR_append_insn(ctx, func_item, end_if_l);
}

// --- FOR/NEXT Emitters ---
static void emit_for_statement(MirEmitter *emitter, BasicAstNode *for_node) {
    MIR_context_t ctx = emitter->ctx;
    MIR_item_t func_item = emitter->current_func_item;

    BasicAstNode *var_node = for_node->data.for_stmt.loop_variable;
    Symbol *loop_sym = symbol_table_get(emitter->symbol_table, var_node->data.variable.name);
    MIR_reg_t loop_var_reg = get_mir_reg_for_variable(emitter, loop_sym);
    MIR_type_t loop_mir_type = basic_type_to_mir_type(loop_sym->type);

    // Emit start_value and assign to loop_var_reg
    MIR_op_t start_val_op = emit_expression(emitter, for_node->data.for_stmt.start_value);
    start_val_op = ensure_operand_type(emitter, start_val_op, loop_mir_type, MIR_op_type(ctx, start_val_op, func_item->u.func));
    MIR_append_insn(ctx, func_item, MIR_new_insn(ctx,
        (loop_mir_type == MIR_T_F) ? MIR_FMOV : (loop_mir_type == MIR_T_D) ? MIR_DMOV : MIR_MOV,
        0, 2, MIR_new_reg_op(ctx, loop_var_reg), start_val_op));

    // Store end_value and step_value in temporary registers
    MIR_op_t end_val_op_orig = emit_expression(emitter, for_node->data.for_stmt.end_value);
    MIR_reg_t end_val_reg = get_new_temp_mir_reg(emitter, loop_mir_type);
    MIR_op_t end_val_op = ensure_operand_type(emitter, end_val_op_orig, loop_mir_type, MIR_op_type(ctx, end_val_op_orig, func_item->u.func));
    MIR_append_insn(ctx, func_item, MIR_new_insn(ctx,
        (loop_mir_type == MIR_T_F) ? MIR_FMOV : (loop_mir_type == MIR_T_D) ? MIR_DMOV : MIR_MOV,
        0, 2, MIR_new_reg_op(ctx, end_val_reg), end_val_op));

    MIR_reg_t step_val_reg = get_new_temp_mir_reg(emitter, loop_mir_type);
    if (for_node->data.for_stmt.step_value) {
        MIR_op_t step_val_op_orig = emit_expression(emitter, for_node->data.for_stmt.step_value);
        MIR_op_t step_val_op = ensure_operand_type(emitter, step_val_op_orig, loop_mir_type, MIR_op_type(ctx, step_val_op_orig, func_item->u.func));
        MIR_append_insn(ctx, func_item, MIR_new_insn(ctx,
            (loop_mir_type == MIR_T_F) ? MIR_FMOV : (loop_mir_type == MIR_T_D) ? MIR_DMOV : MIR_MOV,
            0, 2, MIR_new_reg_op(ctx, step_val_reg), step_val_op));
    } else { // Default step is 1
        MIR_op_t one_op = (loop_mir_type == MIR_T_F) ? MIR_new_float_op(ctx, 1.0f) :
                          (loop_mir_type == MIR_T_D) ? MIR_new_double_op(ctx, 1.0) :
                                                       MIR_new_int_op(ctx, 1);
        MIR_append_insn(ctx, func_item, MIR_new_insn(ctx,
            (loop_mir_type == MIR_T_F) ? MIR_FMOV : (loop_mir_type == MIR_T_D) ? MIR_DMOV : MIR_MOV,
            0, 2, MIR_new_reg_op(ctx, step_val_reg), one_op));
    }

    // Create labels and store them in the AST node for NEXT to use
    for_node->data.for_stmt.loop_check_label = MIR_new_label(ctx);
    for_node->data.for_stmt.loop_exit_label = MIR_new_label(ctx);

    MIR_append_insn(ctx, func_item, for_node->data.for_stmt.loop_check_label);

    // Condition check: (step >= 0 AND var > end_val) OR (step < 0 AND var < end_val) -> exit
    // This requires checking sign of step. For simplicity, assume step is constant or its sign is known.
    // Or, generate more complex branching if step sign can vary.
    // Simplified: assume step is positive for now for the comparison direction.
    // TODO: Implement proper step sign handling for comparison.
    MIR_insn_code_t cmp_code;
    if (loop_mir_type == MIR_T_F) cmp_code = MIR_FGT;
    else if (loop_mir_type == MIR_T_D) cmp_code = MIR_DGT;
    else cmp_code = MIR_GT; // Integer comparison

    // This comparison is if step is positive: var > end -> exit
    // If step is negative: var < end -> exit
    // For now, only positive step:
    MIR_append_insn(ctx, func_item, MIR_new_insn(ctx, cmp_code, 0, 3,
        MIR_new_label_op(ctx, for_node->data.for_stmt.loop_exit_label), // Target if true (var > end)
        MIR_new_reg_op(ctx, loop_var_reg),
        MIR_new_reg_op(ctx, end_val_reg)  // Using the temp reg holding end_value
    ));
    // If the condition is false (var <= end for positive step), execution falls through to loop body.
}

static void emit_next_statement(MirEmitter *emitter, BasicAstNode *next_node) {
    MIR_context_t ctx = emitter->ctx;
    MIR_item_t func_item = emitter->current_func_item;

    BasicAstNode *for_node = next_node->data.next_stmt.for_node_ptr;
    if (!for_node) {
        fprintf(stderr, "MIR Emitter: NEXT statement at line %d has no corresponding FOR node (semantic error missed?).\n", next_node->line_number);
        return;
    }

    BasicAstNode *var_node = for_node->data.for_stmt.loop_variable; // Use FOR's variable
    Symbol *loop_sym = symbol_table_get(emitter->symbol_table, var_node->data.variable.name);
    MIR_reg_t loop_var_reg = get_mir_reg_for_variable(emitter, loop_sym);
    MIR_type_t loop_mir_type = basic_type_to_mir_type(loop_sym->type);

    // Retrieve step value (assuming it was stored in a temp reg by FOR)
    // This requires a way to map FOR node to its step_val_reg.
    // For now, re-evaluate step or assume it's in a known reg (simplification needed here)
    // Let's assume step_value is 1 for now if not stored/retrieved.
    MIR_op_t step_op;
    if (for_node->data.for_stmt.step_value) {
        // Ideally, FOR would have saved the step_val_reg on its AST node or similar.
        // For now, re-emit the step expression:
        step_op = emit_expression(emitter, for_node->data.for_stmt.step_value);
    } else {
        step_op = (loop_mir_type == MIR_T_F) ? MIR_new_float_op(ctx, 1.0f) :
                  (loop_mir_type == MIR_T_D) ? MIR_new_double_op(ctx, 1.0) :
                                               MIR_new_int_op(ctx, 1);
    }
    step_op = ensure_operand_type(emitter, step_op, loop_mir_type, MIR_op_type(ctx, step_op, func_item->u.func));


    MIR_insn_code_t add_code;
    if (loop_mir_type == MIR_T_F) add_code = MIR_FADD;
    else if (loop_mir_type == MIR_T_D) add_code = MIR_DADD;
    else add_code = MIR_ADD;

    MIR_append_insn(ctx, func_item, MIR_new_insn(ctx, add_code, 0, 3,
        MIR_new_reg_op(ctx, loop_var_reg),
        MIR_new_reg_op(ctx, loop_var_reg),
        step_op
    ));

    // Jump back to the loop condition check
    MIR_append_insn(ctx, func_item, MIR_new_insn(ctx, MIR_JMP, 0, 1,
        MIR_new_label_op(ctx, for_node->data.for_stmt.loop_check_label)));

    // Emit the exit label for this loop (where execution continues after loop)
    MIR_append_insn(ctx, func_item, for_node->data.for_stmt.loop_exit_label);
}
