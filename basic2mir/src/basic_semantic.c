#include "basic_semantic.h"
#include "basic_ast.h" // For BasicVariableType and node structures

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h> // For va_list in semantic_error

// --- Helper: Variable Type to String ---
static const char* variable_type_to_string(BasicVariableType type) {
    switch (type) {
        case BASIC_VAR_TYPE_DEFAULT: return "DEFAULT (FLOAT)";
        case BASIC_VAR_TYPE_STRING: return "STRING";
        case BASIC_VAR_TYPE_INTEGER: return "INTEGER";
        case BASIC_VAR_TYPE_SINGLE: return "SINGLE";
        case BASIC_VAR_TYPE_DOUBLE: return "DOUBLE";
        default: return "UNKNOWN_TYPE";
    }
}

// --- Error Reporting ---
static void semantic_error(SemanticAnalyzer *analyzer, BasicAstNode* node, const char *message_format, ...) {
    analyzer->error_count++;
    char buffer[sizeof(analyzer->last_error_message)];

    va_list args;
    va_start(args, message_format);
    vsnprintf(buffer, sizeof(buffer), message_format, args);
    va_end(args);

    int line_num = node ? node->line_number : -1;

    snprintf(analyzer->last_error_message, sizeof(analyzer->last_error_message),
             "Semantic Error (L%d): %s", line_num, buffer);

    fprintf(stderr, "%s\n", analyzer->last_error_message);
}


// --- Symbol Table Implementation ---
static unsigned int hash_function(const char *key) {
    unsigned long hash = 5381;
    int c;
    while ((c = *key++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash % SYMBOL_TABLE_SIZE;
}

SymbolTable *symbol_table_new(BasicAstNode *program_lines_head) {
    SymbolTable *st = (SymbolTable *)calloc(1, sizeof(SymbolTable));
    if (!st) {
        perror("Failed to allocate SymbolTable");
        return NULL;
    }
    st->parent_scope = NULL;
    st->program_lines_head = program_lines_head;
    return st;
}

void symbol_table_free(SymbolTable *st) {
    if (!st) return;
    for (int i = 0; i < SYMBOL_TABLE_SIZE; ++i) {
        Symbol *current = st->table[i];
        while (current) {
            Symbol *next = current->next;
            free(current->name);
            free(current);
            current = next;
        }
    }
    free(st);
}

Symbol *symbol_table_put(SymbolTable *st, const char *name, BasicVariableType type, BasicAstNode *decl_node) {
    if (!name) return NULL;
    unsigned int index = hash_function(name);
    Symbol *current = st->table[index];
    while (current) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }

    Symbol *new_symbol = (Symbol *)malloc(sizeof(Symbol));
    if (!new_symbol) {
        perror("Failed to allocate Symbol");
        return NULL;
    }
    new_symbol->name = strdup(name);
    if (!new_symbol->name) {
        perror("Failed to strdup symbol name");
        free(new_symbol);
        return NULL;
    }
    new_symbol->type = type;
    new_symbol->declaration_node = decl_node;
    new_symbol->next = st->table[index];
    st->table[index] = new_symbol;
    return new_symbol;
}

Symbol *symbol_table_get(SymbolTable *st, const char *name) {
    if (!name) return NULL;
    unsigned int index = hash_function(name);
    Symbol *current = st->table[index];
    while (current) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

BasicAstNode *symbol_table_get_line_node(SymbolTable *st, int32_t line_number) {
    if (!st || !st->program_lines_head) return NULL;
    BasicAstNode *current_line = st->program_lines_head;
    while (current_line) {
        if (current_line->type == AST_NODE_TYPE_PROGRAM_LINE && current_line->line_number == line_number) {
            return current_line;
        }
        current_line = current_line->data.program_line.next_line;
    }
    return NULL;
}

// --- Semantic Analyzer Implementation ---
SemanticAnalyzer *semantic_analyzer_new(void) {
    SemanticAnalyzer *analyzer = (SemanticAnalyzer *)calloc(1, sizeof(SemanticAnalyzer));
    if (!analyzer) {
        perror("Failed to allocate SemanticAnalyzer");
        return NULL;
    }
    analyzer->symbol_table = NULL;
    analyzer->error_count = 0;
    analyzer->last_error_message[0] = '\0';
    analyzer->for_loop_stack_ptr = -1; // Initialize FOR loop stack
    return analyzer;
}

void semantic_analyzer_free(SemanticAnalyzer *analyzer) {
    if (analyzer) {
        if (analyzer->symbol_table) {
            symbol_table_free(analyzer->symbol_table);
        }
        free(analyzer);
    }
}

// --- Main Analysis Function ---
int semantic_analyzer_analyze(SemanticAnalyzer *analyzer, BasicAstNode *program_ast_root) {
    if (!analyzer || !program_ast_root || program_ast_root->type != AST_NODE_TYPE_PROGRAM) {
        if (analyzer) semantic_error(analyzer, program_ast_root, "Invalid program AST node for analysis.");
        return 1;
    }

    analyzer->current_program_ast = program_ast_root;
    analyzer->error_count = 0;
    analyzer->for_loop_stack_ptr = -1; // Reset stack for this run

    if (analyzer->symbol_table) {
        symbol_table_free(analyzer->symbol_table);
    }
    analyzer->symbol_table = symbol_table_new(program_ast_root->data.program.program_lines);
    if (!analyzer->symbol_table) {
        semantic_error(analyzer, program_ast_root, "Failed to create symbol table.");
        return 1;
    }

    visit_node(analyzer, program_ast_root);

    // After visiting all nodes, check if there are any unclosed FOR loops
    if (analyzer->for_loop_stack_ptr != -1) {
        for (int i = 0; i <= analyzer->for_loop_stack_ptr; ++i) {
            semantic_error(analyzer, analyzer->for_loop_stack[i],
                           "FOR statement without a matching NEXT statement (loop variable: %s).",
                           analyzer->for_loop_stack[i]->data.for_stmt.loop_variable->data.variable.name);
        }
    }

    return analyzer->error_count;
}

// --- AST Traversal (Visitor) Functions ---

void visit_node(SemanticAnalyzer *analyzer, BasicAstNode *node) {
    if (!node) return;

    switch (node->type) {
        case AST_NODE_TYPE_PROGRAM:         visit_program(analyzer, node); break;
        case AST_NODE_TYPE_PROGRAM_LINE:    visit_program_line(analyzer, node); break;

        case AST_NODE_TYPE_LET:             visit_let_statement(analyzer, node); break;
        case AST_NODE_TYPE_PRINT:           visit_print_statement(analyzer, node); break;
        case AST_NODE_TYPE_GOTO:            visit_goto_statement(analyzer, node); break;
        case AST_NODE_TYPE_REM:             break;
        case AST_NODE_TYPE_END:             break;
        case AST_NODE_TYPE_IF_THEN_ELSE:    visit_if_statement(analyzer, node); break;
        case AST_NODE_TYPE_FOR:             visit_for_statement(analyzer, node); break;
        case AST_NODE_TYPE_NEXT:            visit_next_statement(analyzer, node); break;

        case AST_NODE_TYPE_VARIABLE:        visit_variable_reference(analyzer, node, 0); break;
        case AST_NODE_TYPE_NUMBER_LITERAL:  break;
        case AST_NODE_TYPE_STRING_LITERAL:  break;
        case AST_NODE_TYPE_UNARY_EXPR:
        case AST_NODE_TYPE_BINARY_EXPR:     visit_expression(analyzer, node); break;

        default:
            semantic_error(analyzer, node, "Encountered unknown AST node type: %d", node->type);
            break;
    }
}

void visit_program(SemanticAnalyzer *analyzer, BasicAstNode *program_node) {
    BasicAstNode *current_line = program_node->data.program.program_lines;
    while (current_line) {
        visit_node(analyzer, current_line);
        current_line = current_line->data.program_line.next_line;
    }
}

void visit_program_line(SemanticAnalyzer *analyzer, BasicAstNode *line_node) {
    if (line_node->data.program_line.statement) {
        visit_node(analyzer, line_node->data.program_line.statement);
    }
}

// --- Statement Visitors ---
void visit_let_statement(SemanticAnalyzer *analyzer, BasicAstNode *let_node) {
    visit_variable_reference(analyzer, let_node->data.let_stmt.variable, 1);
    visit_expression(analyzer, let_node->data.let_stmt.expression);

    BasicVariableType lhs_type = get_expression_type(analyzer, let_node->data.let_stmt.variable);
    BasicVariableType rhs_type = get_expression_type(analyzer, let_node->data.let_stmt.expression);

    int lhs_is_numeric = (lhs_type != BASIC_VAR_TYPE_STRING);
    int rhs_is_numeric = (rhs_type != BASIC_VAR_TYPE_STRING);

    if (lhs_is_numeric && !rhs_is_numeric) {
        semantic_error(analyzer, let_node, "Type mismatch: Cannot assign STRING to numeric variable '%s'.",
                       let_node->data.let_stmt.variable->data.variable.name);
    } else if (!lhs_is_numeric && rhs_is_numeric) {
         semantic_error(analyzer, let_node, "Type mismatch: Cannot assign NUMERIC to string variable '%s'.",
                       let_node->data.let_stmt.variable->data.variable.name);
    }
}

void visit_print_statement(SemanticAnalyzer *analyzer, BasicAstNode *print_node) {
    BasicAstNode *current_item = print_node->data.print_stmt.print_items;
    while (current_item) {
        visit_node(analyzer, current_item);
        get_expression_type(analyzer, current_item);
        current_item = current_item->next;
    }
}

void visit_goto_statement(SemanticAnalyzer *analyzer, BasicAstNode *goto_node) {
    int32_t target_line = goto_node->data.goto_stmt.target_line_number;
    if (!symbol_table_get_line_node(analyzer->symbol_table, target_line)) {
        semantic_error(analyzer, goto_node, "Undefined line number %d in GOTO statement.", target_line);
    }
}

static void visit_if_statement(SemanticAnalyzer *analyzer, BasicAstNode *if_node) {
    if (!if_node || if_node->type != AST_NODE_TYPE_IF_THEN_ELSE) return;

    visit_node(analyzer, if_node->data.if_then_else_stmt.condition);
    BasicVariableType cond_type = get_expression_type(analyzer, if_node->data.if_then_else_stmt.condition);
    if (cond_type == BASIC_VAR_TYPE_STRING) {
        semantic_error(analyzer, if_node,
                       "IF condition cannot be a string type.");
    }

    BasicAstNode *then_branch = if_node->data.if_then_else_stmt.then_branch;
    if (then_branch) {
        if (then_branch->type == AST_NODE_TYPE_GOTO) {
            if (!symbol_table_get_line_node(analyzer->symbol_table, then_branch->data.goto_stmt.target_line_number)) {
                semantic_error(analyzer, then_branch, "Undefined line number %d in THEN clause of IF statement.",
                               then_branch->data.goto_stmt.target_line_number);
            }
        } else {
            visit_node(analyzer, then_branch);
        }
    } else {
         semantic_error(analyzer, if_node, "Missing THEN clause body in IF statement.");
    }

    BasicAstNode *else_branch = if_node->data.if_then_else_stmt.else_branch;
    if (else_branch) {
        if (else_branch->type == AST_NODE_TYPE_GOTO) {
            if (!symbol_table_get_line_node(analyzer->symbol_table, else_branch->data.goto_stmt.target_line_number)) {
                semantic_error(analyzer, else_branch, "Undefined line number %d in ELSE clause of IF statement.",
                               else_branch->data.goto_stmt.target_line_number);
            }
        } else {
            visit_node(analyzer, else_branch);
        }
    }
}

static void visit_for_statement(SemanticAnalyzer *analyzer, BasicAstNode *for_node) {
    if (!for_node || for_node->type != AST_NODE_TYPE_FOR) return;

    BasicAstNode *loop_var_node = for_node->data.for_stmt.loop_variable;
    visit_variable_reference(analyzer, loop_var_node, 1);
    BasicVariableType loop_var_type = get_expression_type(analyzer, loop_var_node);
    if (loop_var_type == BASIC_VAR_TYPE_STRING) {
        semantic_error(analyzer, loop_var_node, "FOR loop counter variable '%s' cannot be a string.", loop_var_node->data.variable.name);
    }
    loop_var_node->data.variable.var_type = loop_var_type;


    visit_node(analyzer, for_node->data.for_stmt.start_value);
    BasicVariableType start_type = get_expression_type(analyzer, for_node->data.for_stmt.start_value);
    if (start_type == BASIC_VAR_TYPE_STRING) {
        semantic_error(analyzer, for_node->data.for_stmt.start_value, "FOR loop START value cannot be a string.");
    }

    visit_node(analyzer, for_node->data.for_stmt.end_value);
    BasicVariableType end_type = get_expression_type(analyzer, for_node->data.for_stmt.end_value);
    if (end_type == BASIC_VAR_TYPE_STRING) {
        semantic_error(analyzer, for_node->data.for_stmt.end_value, "FOR loop TO value cannot be a string.");
    }

    if (for_node->data.for_stmt.step_value) {
        visit_node(analyzer, for_node->data.for_stmt.step_value);
        BasicVariableType step_type = get_expression_type(analyzer, for_node->data.for_stmt.step_value);
        if (step_type == BASIC_VAR_TYPE_STRING) {
            semantic_error(analyzer, for_node->data.for_stmt.step_value, "FOR loop STEP value cannot be a string.");
        }
    }

    if (analyzer->for_loop_stack_ptr >= MAX_LOOP_NESTING - 1) {
        semantic_error(analyzer, for_node, "FOR loop nesting too deep (max %d).", MAX_LOOP_NESTING);
    } else {
        analyzer->for_loop_stack_ptr++;
        analyzer->for_loop_stack[analyzer->for_loop_stack_ptr] = for_node;
    }
}

static void visit_next_statement(SemanticAnalyzer *analyzer, BasicAstNode *next_node) {
    if (!next_node || next_node->type != AST_NODE_TYPE_NEXT) return;

    if (analyzer->for_loop_stack_ptr < 0) {
        semantic_error(analyzer, next_node, "NEXT statement without a matching FOR statement.");
        return;
    }

    BasicAstNode *current_for_node = analyzer->for_loop_stack[analyzer->for_loop_stack_ptr];
    BasicAstNode *next_loop_var_node = next_node->data.next_stmt.loop_variable;

    if (next_loop_var_node) {
        visit_variable_reference(analyzer, next_loop_var_node, 0);
        const char *next_var_name = next_loop_var_node->data.variable.name;
        const char *for_var_name = current_for_node->data.for_stmt.loop_variable->data.variable.name;

        if (strcmp(next_var_name, for_var_name) != 0) {
            semantic_error(analyzer, next_loop_var_node, "NEXT variable '%s' does not match current FOR loop variable '%s'.",
                           next_var_name, for_var_name);
            return;
        }
    }

    next_node->data.next_stmt.for_node_ptr = current_for_node;
    current_for_node->data.for_stmt.next_node_ptr = next_node;

    analyzer->for_loop_stack[analyzer->for_loop_stack_ptr] = NULL;
    analyzer->for_loop_stack_ptr--;
}


// --- Expression and Variable Visitors ---
void visit_variable_reference(SemanticAnalyzer *analyzer, BasicAstNode *var_node, int is_lhs_in_let) {
    if (var_node->type != AST_NODE_TYPE_VARIABLE) return;

    const char *name = var_node->data.variable.name;
    Symbol *symbol = symbol_table_get(analyzer->symbol_table, name);

    BasicVariableType inferred_type = var_node->data.variable.var_type;

    if (!symbol) {
        if (inferred_type == BASIC_VAR_TYPE_DEFAULT) {
            inferred_type = BASIC_VAR_TYPE_SINGLE;
        }
        symbol = symbol_table_put(analyzer->symbol_table, name, inferred_type, var_node);
        if (!symbol) {
             semantic_error(analyzer, var_node, "Failed to add symbol '%s' to table.", name);
             return;
        }
    } else {
        int current_is_numeric = (inferred_type != BASIC_VAR_TYPE_STRING);
        int symbol_is_numeric = (symbol->type != BASIC_VAR_TYPE_STRING);

        if (current_is_numeric != symbol_is_numeric) {
             semantic_error(analyzer, var_node, "Variable '%s' used with conflicting types (e.g. numeric vs string). Original: %s, Current: %s",
                           name, variable_type_to_string(symbol->type), variable_type_to_string(inferred_type));
        }
        if (is_lhs_in_let) {
            if (symbol->type == BASIC_VAR_TYPE_DEFAULT || symbol->type == BASIC_VAR_TYPE_SINGLE || symbol->type == BASIC_VAR_TYPE_DOUBLE) {
                 if(inferred_type == BASIC_VAR_TYPE_INTEGER || inferred_type == BASIC_VAR_TYPE_SINGLE || inferred_type == BASIC_VAR_TYPE_DOUBLE){
                    symbol->type = inferred_type;
                 }
            }
        }
    }

    var_node->data.variable.var_type = symbol->type;

    if (var_node->data.variable.dimensions) {
        BasicAstNode *current_dim_expr = var_node->data.variable.dimensions;
        while(current_dim_expr) {
            visit_node(analyzer, current_dim_expr);
            BasicVariableType dim_type = get_expression_type(analyzer, current_dim_expr);
            if (dim_type == BASIC_VAR_TYPE_STRING) {
                 semantic_error(analyzer, current_dim_expr, "Array dimension expression cannot be a string.");
            }
            current_dim_expr = current_dim_expr->next;
        }
    }
}

void visit_expression(SemanticAnalyzer *analyzer, BasicAstNode *expr_node) {
    if (!expr_node) return;
    switch (expr_node->type) {
        case AST_NODE_TYPE_UNARY_EXPR:
            visit_node(analyzer, expr_node->data.unary_expr.operand);
            get_expression_type(analyzer, expr_node);
            break;
        case AST_NODE_TYPE_BINARY_EXPR:
            visit_node(analyzer, expr_node->data.binary_expr.left);
            visit_node(analyzer, expr_node->data.binary_expr.right);
            get_expression_type(analyzer, expr_node);
            break;
        case AST_NODE_TYPE_NUMBER_LITERAL:
        case AST_NODE_TYPE_STRING_LITERAL:
        case AST_NODE_TYPE_VARIABLE:
             get_expression_type(analyzer, expr_node);
            break;
        default:
            semantic_error(analyzer, expr_node, "Unknown node type in visit_expression: %d", expr_node->type);
            break;
    }
}


// --- Type System Helper ---
BasicVariableType get_expression_type(SemanticAnalyzer *analyzer, BasicAstNode *expr_node) {
    if (!expr_node) return BASIC_VAR_TYPE_DEFAULT;

    switch (expr_node->type) {
        case AST_NODE_TYPE_NUMBER_LITERAL:
            return expr_node->data.number_literal.num_type;
        case AST_NODE_TYPE_STRING_LITERAL:
            return BASIC_VAR_TYPE_STRING;
        case AST_NODE_TYPE_VARIABLE: {
            if (expr_node->data.variable.var_type == (BasicVariableType)0 && analyzer) {
                visit_variable_reference(analyzer, expr_node, 0);
            }
            Symbol* sym = NULL;
            if(analyzer) sym = symbol_table_get(analyzer->symbol_table, expr_node->data.variable.name);

            if (sym) return sym->type;

            BasicVariableType inferred = expr_node->data.variable.var_type;
            if (inferred == BASIC_VAR_TYPE_DEFAULT) inferred = BASIC_VAR_TYPE_SINGLE;
            if (analyzer && !sym) {
                symbol_table_put(analyzer->symbol_table, expr_node->data.variable.name, inferred, expr_node);
            }
            expr_node->data.variable.var_type = inferred;
            return inferred;
        }
        case AST_NODE_TYPE_UNARY_EXPR: {
            BasicVariableType operand_type = get_expression_type(analyzer, expr_node->data.unary_expr.operand);
            if (strcmp(expr_node->data.unary_expr.operator, "-") == 0 || strcmp(expr_node->data.unary_expr.operator, "+") == 0) {
                if (operand_type == BASIC_VAR_TYPE_STRING) {
                    if (analyzer) semantic_error(analyzer, expr_node, "Unary '%s' cannot be applied to a string.", expr_node->data.unary_expr.operator);
                    return BASIC_VAR_TYPE_SINGLE;
                }
                return (operand_type == BASIC_VAR_TYPE_DEFAULT) ? BASIC_VAR_TYPE_SINGLE : operand_type;
            } else if (strcasecmp(expr_node->data.unary_expr.operator, "NOT") == 0) {
                 if (operand_type == BASIC_VAR_TYPE_STRING) {
                    if (analyzer) semantic_error(analyzer, expr_node, "'NOT' operator cannot be applied to a string.");
                    return BASIC_VAR_TYPE_INTEGER;
                }
                return BASIC_VAR_TYPE_INTEGER;
            }
            if(analyzer) semantic_error(analyzer, expr_node, "Unknown unary operator '%s'.", expr_node->data.unary_expr.operator);
            return BASIC_VAR_TYPE_SINGLE;
        }
        case AST_NODE_TYPE_BINARY_EXPR: {
            BasicVariableType left_type = get_expression_type(analyzer, expr_node->data.binary_expr.left);
            BasicVariableType right_type = get_expression_type(analyzer, expr_node->data.binary_expr.right);
            const char* op = expr_node->data.binary_expr.operator;

            int left_is_numeric = (left_type != BASIC_VAR_TYPE_STRING);
            int right_is_numeric = (right_type != BASIC_VAR_TYPE_STRING);

            if (strcmp(op, "+") == 0) {
                if (left_type == BASIC_VAR_TYPE_STRING && right_type == BASIC_VAR_TYPE_STRING) return BASIC_VAR_TYPE_STRING;
                if (left_type == BASIC_VAR_TYPE_STRING || right_type == BASIC_VAR_TYPE_STRING) {
                     if(analyzer) semantic_error(analyzer, expr_node, "Type mismatch: Cannot use '+' with string and numeric types together.");
                    return BASIC_VAR_TYPE_STRING;
                }
            } else if (strcmp(op, "-") == 0 || strcmp(op, "*") == 0 || strcmp(op, "/") == 0 ||
                       strcmp(op, "\\") == 0 || strcmp(op, "^") == 0 || strcasecmp(op, "MOD") == 0) {
                if (!left_is_numeric || !right_is_numeric) {
                    if(analyzer) semantic_error(analyzer, expr_node, "Type mismatch: Operator '%s' requires numeric operands.", op);
                    return BASIC_VAR_TYPE_SINGLE;
                }
            } else if (strcmp(op, "=") == 0 || strcmp(op, "<>") == 0 || strcmp(op, "<") == 0 ||
                       strcmp(op, ">") == 0 || strcmp(op, "<=") == 0 || strcmp(op, ">=") == 0) {
                if (left_is_numeric != right_is_numeric) {
                    if(analyzer) semantic_error(analyzer, expr_node, "Type mismatch: Cannot compare string with numeric type using '%s'.", op);
                }
                return BASIC_VAR_TYPE_INTEGER;
            } else if (strcasecmp(op, "AND") == 0 || strcasecmp(op, "OR") == 0 ||
                       strcasecmp(op, "XOR") == 0 || strcasecmp(op, "IMP") == 0 || strcasecmp(op, "EQV") == 0) {
                if (!left_is_numeric || !right_is_numeric) {
                    if(analyzer) semantic_error(analyzer, expr_node, "Type mismatch: Logical operator '%s' requires numeric operands.", op);
                }
                return BASIC_VAR_TYPE_INTEGER;
            } else {
                 if(analyzer) semantic_error(analyzer, expr_node, "Unknown or unsupported binary operator '%s'.", op);
                 return BASIC_VAR_TYPE_SINGLE;
            }

            if (left_type == BASIC_VAR_TYPE_DOUBLE || right_type == BASIC_VAR_TYPE_DOUBLE) return BASIC_VAR_TYPE_DOUBLE;
            if (left_type == BASIC_VAR_TYPE_SINGLE || right_type == BASIC_VAR_TYPE_SINGLE) return BASIC_VAR_TYPE_SINGLE;
            if (left_type == BASIC_VAR_TYPE_INTEGER && right_type == BASIC_VAR_TYPE_INTEGER) return BASIC_VAR_TYPE_INTEGER;

            if (left_type == BASIC_VAR_TYPE_DEFAULT && right_is_numeric) return right_type;
            if (right_type == BASIC_VAR_TYPE_DEFAULT && left_is_numeric) return left_type;

            return BASIC_VAR_TYPE_SINGLE;
        }
        default:
            if(analyzer) semantic_error(analyzer, expr_node, "Cannot determine type for unknown expression node type %d.", expr_node->type);
            return BASIC_VAR_TYPE_SINGLE;
    }
}
