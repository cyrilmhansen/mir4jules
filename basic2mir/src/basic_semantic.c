#include "basic_semantic.h"
#include "basic_ast.h" // For BasicVariableType and node structures

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h> // For va_list in semantic_error

// --- Helper: Variable Type to String ---
static const char* variable_type_to_string(BasicVariableType type) {
    switch (type) {
        case BASIC_VAR_TYPE_DEFAULT: return "DEFAULT (FLOAT)"; // GW-BASIC default
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

    int line_num = node ? node->line_number : -1; // Get line number from AST node if available

    snprintf(analyzer->last_error_message, sizeof(analyzer->last_error_message),
             "Semantic Error (L%d): %s", line_num, buffer);

    fprintf(stderr, "%s\n", analyzer->last_error_message);
}


// --- Symbol Table Implementation ---
static unsigned int hash_function(const char *key) {
    unsigned long hash = 5381;
    int c;
    while ((c = *key++)) {
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }
    return hash % SYMBOL_TABLE_SIZE;
}

SymbolTable *symbol_table_new(BasicAstNode *program_lines_head) {
    SymbolTable *st = (SymbolTable *)calloc(1, sizeof(SymbolTable));
    if (!st) {
        perror("Failed to allocate SymbolTable");
        return NULL;
    }
    st->parent_scope = NULL; // Global scope initially
    st->program_lines_head = program_lines_head;
    // Table itself is already zeroed by calloc
    return st;
}

void symbol_table_free(SymbolTable *st) {
    if (!st) return;
    for (int i = 0; i < SYMBOL_TABLE_SIZE; ++i) {
        Symbol *current = st->table[i];
        while (current) {
            Symbol *next = current->next;
            free(current->name);
            // current->declaration_node is not freed here, AST is managed separately
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
            // Symbol already exists. GW-BASIC allows implicit re-typing to some extent,
            // but strict checking would report an error or warning.
            // For now, let's just return the existing symbol.
            // A more robust system might check if the type is compatible or update it.
            // semantic_error(analyzer_ptr, decl_node, "Symbol '%s' already declared.", name); // Needs analyzer ptr
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
    new_symbol->declaration_node = decl_node; // Store where it was first seen/declared
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
    return NULL; // Not found
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
    return NULL; // Line number not found
}

// --- Semantic Analyzer Implementation ---
SemanticAnalyzer *semantic_analyzer_new(void) {
    SemanticAnalyzer *analyzer = (SemanticAnalyzer *)calloc(1, sizeof(SemanticAnalyzer));
    if (!analyzer) {
        perror("Failed to allocate SemanticAnalyzer");
        return NULL;
    }
    analyzer->symbol_table = NULL; // Will be created in analyze()
    analyzer->error_count = 0;
    analyzer->last_error_message[0] = '\0';
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
        return 1; // Indicate error
    }

    analyzer->current_program_ast = program_ast_root;
    analyzer->error_count = 0; // Reset error count for this run

    // Create a new symbol table for this analysis run
    if (analyzer->symbol_table) { // Free old one if any (e.g. re-analysis)
        symbol_table_free(analyzer->symbol_table);
    }
    analyzer->symbol_table = symbol_table_new(program_ast_root->data.program.program_lines);
    if (!analyzer->symbol_table) {
        semantic_error(analyzer, program_ast_root, "Failed to create symbol table.");
        return 1;
    }

    visit_node(analyzer, program_ast_root);

    return analyzer->error_count;
}

// --- AST Traversal (Visitor) Functions ---

// Main dispatch
void visit_node(SemanticAnalyzer *analyzer, BasicAstNode *node) {
    if (!node) return;

    // Pre-visit actions (e.g., symbol table population for declarations) could go here

    switch (node->type) {
        case AST_NODE_TYPE_PROGRAM:         visit_program(analyzer, node); break;
        case AST_NODE_TYPE_PROGRAM_LINE:    visit_program_line(analyzer, node); break;

        // Statements
        case AST_NODE_TYPE_LET:             visit_let_statement(analyzer, node); break;
        case AST_NODE_TYPE_PRINT:           visit_print_statement(analyzer, node); break;
        case AST_NODE_TYPE_GOTO:            visit_goto_statement(analyzer, node); break;
        case AST_NODE_TYPE_REM:             /*visit_rem_statement(analyzer, node);*/ break; // No semantic actions usually
        case AST_NODE_TYPE_END:             /*visit_end_statement(analyzer, node);*/ break; // No specific semantic actions
        // TODO: Add cases for IF, FOR, NEXT, DIM, INPUT, GOSUB, RETURN etc.

        // Expressions & Literals (usually visited as part of statements)
        case AST_NODE_TYPE_VARIABLE:        visit_variable_reference(analyzer, node, 0); break; // 0 = not a declaration context by default
        case AST_NODE_TYPE_NUMBER_LITERAL:  /* visit_literal_node(analyzer, node); */ break; // Type is inherent
        case AST_NODE_TYPE_STRING_LITERAL:  /* visit_literal_node(analyzer, node); */ break; // Type is inherent
        case AST_NODE_TYPE_UNARY_EXPR:
        case AST_NODE_TYPE_BINARY_EXPR:     visit_expression(analyzer, node); break;

        default:
            semantic_error(analyzer, node, "Encountered unknown AST node type: %d", node->type);
            break;
    }
    // Post-visit actions could go here
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
    // TODO: Handle multiple statements on a line if parser supports it and links them
}

// --- Statement Visitors ---
void visit_let_statement(SemanticAnalyzer *analyzer, BasicAstNode *let_node) {
    // Visit LHS variable - this will add to symbol table if new, and annotate its type
    // Pass 1 for is_declaration if LET implies declaration/type setting on first encounter
    visit_variable_reference(analyzer, let_node->data.let_stmt.variable, 1);

    // Visit RHS expression to ensure its parts are valid (e.g., variables exist)
    visit_expression(analyzer, let_node->data.let_stmt.expression);

    // Type Checking
    BasicVariableType lhs_type = get_expression_type(analyzer, let_node->data.let_stmt.variable);
    BasicVariableType rhs_type = get_expression_type(analyzer, let_node->data.let_stmt.expression);

    // GW-BASIC Type Compatibility for LET A = B or A$ = B$
    // 1. Default/Numeric = Default/Numeric (any combination)
    // 2. String = String
    // All other combinations are type mismatches.
    int lhs_is_numeric = (lhs_type != BASIC_VAR_TYPE_STRING);
    int rhs_is_numeric = (rhs_type != BASIC_VAR_TYPE_STRING);

    if (lhs_is_numeric && !rhs_is_numeric) {
        semantic_error(analyzer, let_node, "Type mismatch: Cannot assign STRING to numeric variable '%s'.",
                       let_node->data.let_stmt.variable->data.variable.name);
    } else if (!lhs_is_numeric && rhs_is_numeric) {
         semantic_error(analyzer, let_node, "Type mismatch: Cannot assign NUMERIC to string variable '%s'.",
                       let_node->data.let_stmt.variable->data.variable.name);
    }
    // If both are numeric, it's generally fine. Conversion rules (e.g. float to int) apply at code gen/runtime.
    // If both are string, it's fine.
}

void visit_print_statement(SemanticAnalyzer *analyzer, BasicAstNode *print_node) {
    BasicAstNode *current_item = print_node->data.print_stmt.print_items;
    while (current_item) {
        visit_node(analyzer, current_item); // Visit each expression in the print list
        // Call get_expression_type to ensure types are valid, though PRINT can usually handle various types.
        // (We don't need to store its result unless there are specific checks for PRINT)
        get_expression_type(analyzer, current_item);
        current_item = current_item->next; // Assuming generic 'next' for print items list
    }
}

void visit_goto_statement(SemanticAnalyzer *analyzer, BasicAstNode *goto_node) {
    int32_t target_line = goto_node->data.goto_stmt.target_line_number;
    if (!symbol_table_get_line_node(analyzer->symbol_table, target_line)) {
        semantic_error(analyzer, goto_node, "Undefined line number %d in GOTO statement.", target_line);
    }
}

// --- Expression and Variable Visitors ---
void visit_variable_reference(SemanticAnalyzer *analyzer, BasicAstNode *var_node, int is_lhs_in_let) {
    if (var_node->type != AST_NODE_TYPE_VARIABLE) return; // Should not happen

    const char *name = var_node->data.variable.name;
    Symbol *symbol = symbol_table_get(analyzer->symbol_table, name);

    BasicVariableType inferred_type = var_node->data.variable.var_type; // From lexer (suffix)

    if (!symbol) { // First encounter
        // Infer type if not explicitly set by lexer (should be, but as fallback)
        if (inferred_type == BASIC_VAR_TYPE_DEFAULT) { // No suffix
            // GW-BASIC default variable type is single-precision float
            inferred_type = BASIC_VAR_TYPE_SINGLE;
        }
        symbol = symbol_table_put(analyzer->symbol_table, name, inferred_type, var_node);
        if (!symbol) {
             semantic_error(analyzer, var_node, "Failed to add symbol '%s' to table.", name);
             return;
        }
    } else {
        // Symbol exists. Check if type from suffix matches stored type.
        // GW-BASIC is somewhat flexible here. A = 1, then A$ = "s" is an error.
        // A = 1, then A% = 2 might be okay or change type.
        // For now, let's assume suffix always dictates intended type for this reference.
        // If existing symbol has a different fundamental kind (string vs numeric), that's an error.
        int current_is_numeric = (inferred_type != BASIC_VAR_TYPE_STRING);
        int symbol_is_numeric = (symbol->type != BASIC_VAR_TYPE_STRING);

        if (current_is_numeric != symbol_is_numeric) {
             semantic_error(analyzer, var_node, "Variable '%s' used with conflicting types (e.g. numeric vs string). Original: %s, Current: %s",
                           name, variable_type_to_string(symbol->type), variable_type_to_string(inferred_type));
        }
        // If this is LHS of LET, we might update the symbol's type if more specific (e.g. default -> integer)
        if (is_lhs_in_let) {
            // If symbol was default and current is specific, update.
            if (symbol->type == BASIC_VAR_TYPE_DEFAULT || symbol->type == BASIC_VAR_TYPE_SINGLE || symbol->type == BASIC_VAR_TYPE_DOUBLE) {
                 if(inferred_type == BASIC_VAR_TYPE_INTEGER || inferred_type == BASIC_VAR_TYPE_SINGLE || inferred_type == BASIC_VAR_TYPE_DOUBLE){
                    symbol->type = inferred_type; // Potentially refine type
                 }
            }
        }
    }

    // Annotate AST node with the determined (possibly refined) type from symbol table
    var_node->data.variable.var_type = symbol->type;

    // Visit dimension expressions if it's an array reference
    if (var_node->data.variable.dimensions) {
        // TODO: Semantic checks for array dimensions (e.g. number of dimensions, types of expressions)
        // This would also involve checking against a DIM statement if present.
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
            get_expression_type(analyzer, expr_node); // Check types
            break;
        case AST_NODE_TYPE_BINARY_EXPR:
            visit_node(analyzer, expr_node->data.binary_expr.left);
            visit_node(analyzer, expr_node->data.binary_expr.right);
            get_expression_type(analyzer, expr_node); // Check types
            break;
        // Literals and variables are visited directly by visit_node if they are standalone expressions
        case AST_NODE_TYPE_NUMBER_LITERAL:
        case AST_NODE_TYPE_STRING_LITERAL:
        case AST_NODE_TYPE_VARIABLE:
             get_expression_type(analyzer, expr_node); // Ensure variable is in symtab / gets type
            break;
        default:
            // This case should ideally not be reached if visit_node calls this appropriately
            semantic_error(analyzer, expr_node, "Unknown node type in visit_expression: %d", expr_node->type);
            break;
    }
}


// --- Type System Helper ---
BasicVariableType get_expression_type(SemanticAnalyzer *analyzer, BasicAstNode *expr_node) {
    if (!expr_node) return BASIC_VAR_TYPE_DEFAULT; // Or an error type

    switch (expr_node->type) {
        case AST_NODE_TYPE_NUMBER_LITERAL:
            return expr_node->data.number_literal.num_type;
        case AST_NODE_TYPE_STRING_LITERAL:
            return BASIC_VAR_TYPE_STRING;
        case AST_NODE_TYPE_VARIABLE: {
            // Ensure variable is visited to be in symbol table and have its type annotated
            if (expr_node->data.variable.var_type == (BasicVariableType)0 && analyzer) { // Check if not yet annotated (0 might be a valid enum but used as flag here)
                 // This call might be redundant if visit_variable_reference was already called during node traversal
                 // but ensures type is known. Pass 0 for is_declaration as this is a usage context.
                visit_variable_reference(analyzer, expr_node, 0);
            }
            Symbol* sym = NULL;
            if(analyzer) sym = symbol_table_get(analyzer->symbol_table, expr_node->data.variable.name);

            if (sym) return sym->type;

            // If symbol still not found after visit (shouldn't happen if visit_variable_reference works)
            // or if type is still default, infer again.
            BasicVariableType inferred = expr_node->data.variable.var_type; // From lexer suffix
            if (inferred == BASIC_VAR_TYPE_DEFAULT) inferred = BASIC_VAR_TYPE_SINGLE; // Default numeric type
            if (analyzer && !sym) { // Add to symbol table if somehow missed (e.g. direct call to get_expression_type)
                symbol_table_put(analyzer->symbol_table, expr_node->data.variable.name, inferred, expr_node);
            }
            expr_node->data.variable.var_type = inferred; // Annotate AST
            return inferred;
        }
        case AST_NODE_TYPE_UNARY_EXPR: {
            BasicVariableType operand_type = get_expression_type(analyzer, expr_node->data.unary_expr.operand);
            // Example: '-' operator. If operand is string, it's an error.
            if (strcmp(expr_node->data.unary_expr.operator, "-") == 0 || strcmp(expr_node->data.unary_expr.operator, "+") == 0) {
                if (operand_type == BASIC_VAR_TYPE_STRING) {
                    if (analyzer) semantic_error(analyzer, expr_node, "Unary '%s' cannot be applied to a string.", expr_node->data.unary_expr.operator);
                    return BASIC_VAR_TYPE_SINGLE; // Error recovery: assume numeric
                }
                return operand_type; // Result type is same as operand for unary +/-
            }
            // TODO: Handle NOT (logical operator, typically integer)
            return BASIC_VAR_TYPE_SINGLE; // Default for unknown unary ops
        }
        case AST_NODE_TYPE_BINARY_EXPR: {
            BasicVariableType left_type = get_expression_type(analyzer, expr_node->data.binary_expr.left);
            BasicVariableType right_type = get_expression_type(analyzer, expr_node->data.binary_expr.right);
            const char* op = expr_node->data.binary_expr.operator;

            int left_is_str = (left_type == BASIC_VAR_TYPE_STRING);
            int right_is_str = (right_type == BASIC_VAR_TYPE_STRING);

            if (strcmp(op, "+") == 0) {
                if (left_is_str && right_is_str) return BASIC_VAR_TYPE_STRING; // String concatenation
                if (left_is_str || right_is_str) { // Mixing string and number with +
                    if(analyzer) semantic_error(analyzer, expr_node, "Type mismatch: Cannot use '+' with string and numeric types together.");
                    return BASIC_VAR_TYPE_STRING; // Error recovery: assume string concatenation was intended
                }
            } else if (strcmp(op, "-") == 0 || strcmp(op, "*") == 0 || strcmp(op, "/") == 0 || strcmp(op, "\\") == 0 || strcmp(op, "^") == 0 || strcasecmp(op, "MOD") == 0) {
                if (left_is_str || right_is_str) {
                    if(analyzer) semantic_error(analyzer, expr_node, "Type mismatch: Operator '%s' cannot be used with string types.", op);
                    return BASIC_VAR_TYPE_SINGLE; // Error recovery
                }
            }
            // TODO: Handle relational operators (e.g. <, >, =). They can compare strings or numbers.
            // TODO: Handle logical operators (AND, OR, etc.). They operate on numeric types (integers).

            // Numeric type promotion rules (simplified):
            if (left_type == BASIC_VAR_TYPE_DOUBLE || right_type == BASIC_VAR_TYPE_DOUBLE) return BASIC_VAR_TYPE_DOUBLE;
            if (left_type == BASIC_VAR_TYPE_SINGLE || right_type == BASIC_VAR_TYPE_SINGLE) return BASIC_VAR_TYPE_SINGLE;
            if (left_type == BASIC_VAR_TYPE_INTEGER || right_type == BASIC_VAR_TYPE_INTEGER) return BASIC_VAR_TYPE_INTEGER;
            return BASIC_VAR_TYPE_SINGLE; // Default numeric result
        }
        default:
            if(analyzer) semantic_error(analyzer, expr_node, "Cannot determine type for unknown expression node type %d.", expr_node->type);
            return BASIC_VAR_TYPE_SINGLE; // Default error recovery
    }
}
