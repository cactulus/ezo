%{
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "parser.tab.h"

#include "ezo.h"
#include "sb.h"

extern int yylex(void);
extern void yyrestart(FILE *yyin);

void yyerror(const char *s);
void parser_init(struct cli_options *options);

static void push_scope(void);
static void pop_scope(void);
static struct symbol *find_function(char *name);

static void use_std_library(char *lib_name);

static void parser_error(const char *fmt, ...);
   
extern int yylineno;
extern FILE *yyin;

static struct symbol **functions;
static char **libraries = NULL;

static struct cli_options *compiler_options;

struct stmt **AST = NULL;
struct scope *scope;

static int scope_level = 0;
static int enum_field_counter = 0;

%}

%union {
	struct symbol **params;
	struct symbol *symbol;
	struct stmt **block;
	struct stmt *stmt;
	struct expr **args;
	struct expr *expr;
	struct ezo_type *type;
	char text[256];
	int int_val;
}

%token NUMBER
%token STRING_LIT
%token ID

%token CONTINUE
%token BREAK
%token NEW
%token NIL
%token SIZEOF
%token USE
%token ENUM
%token STRUCT
%token LET
%token CONST
%token IF
%token ELSE
%token FOR
%token IN
%token WHILE
%token EXTERN
%token RETURN

%token COLONCOLON DDOT TDOT
%token EXCL PP MM AND  
%token ADD SUB STAR DIV MOD
%token PLUS_EQ SUB_EQ MUL_EQ DIV_EQ MOD_EQ
%token AND_EQ BOR_EQ BXOR_EQ BLSH_EQ BRSH_EQ
%token LT GT EQEQ NOTEQ LE GE
%token BOR BRSH BLSH BXOR
%token OP CP AT ANDAND BARBAR

%type <stmt> declaration
%type <stmt> statement
%type <stmt> structdef
%type <stmt> funcdef
%type <stmt> extern
%type <stmt> vardef
%type <stmt> return
%type <stmt> funccall
%type <stmt> assign
%type <stmt> ifstmt
%type <stmt> elsestmt
%type <stmt> blockstmt
%type <stmt> while
%type <stmt> for
%type <stmt> enumdef

%type <block> block
%type <block> statements

%type <expr> expression
%type <expr> int_lit_expression
%type <expr> constant_expression
%type <expr> primary_expression
%type <expr> postfix_expression
%type <expr> unary_expression
%type <expr> cast_expression
%type <expr> additive_expression
%type <expr> multiplicative_expression
%type <expr> shift_expression
%type <expr> bitwise_expression
%type <expr> relational_expression
%type <expr> equality_expression
%type <expr> cond
%type <expr> logical_and_expression
%type <expr> logical_or_expression
%type <expr> initializer

%type <args> arglist
%type <args> init_list

%type <params> paramlist
%type <params> struct_fields

%type <symbol> varsig
%type <symbol> param
%type <symbol> struct_field

%type <type> type

%type <text> ID
%type <text> STRING_LIT;

%start program

%%

program
	: top_level_statement
	;

top_level_statement
	: USE STRING_LIT ';' { use_std_library($2); } top_level_statement
	| declarations
	;

declarations
	: /* empty */
	| declaration declarations { sbpush(AST, $1); }
	;

declaration
	: funcdef
	| enumdef
	| extern
	| structdef
	| vardef { $1->vd.glbl = 1; }
	;

structdef
    : ID COLONCOLON STRUCT '{' struct_fields '}'
                    { $$ = stmt(STMT_STRUCT_DEF);
                        struct symbol *sy = sym(strdup($1));
                        sy->kind = SYM_TYPE;  

                        struct ezo_type *sty = ezot_struct(sy);
                        sty->struct_fields = $5;

                        sy->type = sty;
                        $$->struct_type = sty;
                        scope_add(scope, sy); }
    ;

struct_fields
    : /* empty */ { $$ = NULL; }
    | struct_field { $$ = NULL; sbpush($$, $1); }
	| struct_fields ',' struct_field { if ($1 != NULL) {
                                    $$ = $1;
                                } else {
                                    $$ = NULL;
                                }
                                sbpush($$, $3); }
    ;

struct_field
    : ID ':' type { $$ = sym(strdup($1)); $$->type = $3; }
    ;

enumdef
	: ID COLONCOLON ENUM { enum_field_counter = 0; } '{' enum_fields '}' { $$ = stmt(STMT_ENUM_DEF); }
	;

enum_fields
	: /* empty */
	| enum_field
	| enum_fields ',' enum_field
	;

enum_field
	: ID { struct symbol *sy = sym(strdup($1));
		   sy->kind = SYM_CONST_VAL;
		   sy->constant = enum_field_counter++;
		   scope_add(scope, sy); }
	;

funcdef
	: ID COLONCOLON { push_scope(); } OP paramlist CP type 
	block { $$ = stmt(STMT_FUNC_DEF);
			$$->fd.sym = sym(strdup($1));
			$$->fd.sym->type = $7;
			$$->fd.stmts = $8;
			$$->fd.params = $5;
			$$->fd.vararg = 0;
			pop_scope();
			sbpush(functions, $$->fd.sym); }
	| ID COLONCOLON { push_scope(); } AT OP paramlist CP type 
	block { $$ = stmt(STMT_FUNC_DEF);
			$$->fd.sym = sym(strdup($1));
			$$->fd.sym->type = $8;
			$$->fd.stmts = $9;
			$$->fd.params = $6;
			$$->fd.vararg = 1;
			pop_scope();
			sbpush(functions, $$->fd.sym); }
	;

extern
	: EXTERN ID COLONCOLON  { push_scope(); } OP paramlist CP type ';'
		{ $$ = stmt(STMT_EXTERN);
			$$->fd.sym = sym(strdup($2));
			$$->fd.sym->type = $8;
			$$->fd.params = $6;
			$$->fd.vararg = 0;
			sbpush(functions, $$->fd.sym); }
	| EXTERN ID COLONCOLON  { push_scope(); } AT OP paramlist CP type ';'
		{ $$ = stmt(STMT_EXTERN);
			$$->fd.sym = sym(strdup($2));
			$$->fd.sym->type = $9;
			$$->fd.params = $7;
			$$->fd.vararg = 1;
			sbpush(functions, $$->fd.sym); }
	;

paramlist
	: /* empty */ { $$ = NULL; }
	| param { $$ = NULL; sbpush($$, $1); }
	| paramlist ',' param { if ($1 != NULL) {
								$$ = $1;
							} else {
								$$ = NULL;
							}
							sbpush($$, $3); }
	;

param
	: ID ':' type { $$ = sym(strdup($1));
					$$->kind = SYM_VAR;
					$$->type = $3;
					scope_add(scope, $$); }
	;

block
	: '{' statements '}' { $$ = $2; }
	;

statements
	: { $$ = NULL; }
	| statements statement { if ($1 != NULL) {
								$$ = $1;
							} else {
								$$ = NULL;
							}
							sbpush($$, $2); }
	;

statement
	: vardef { $1->vd.glbl = 0; }
	| return
	| assign
	| funccall
	| ifstmt
	| blockstmt
	| while
	| for
	| expression ';' { $$ = stmt(STMT_EXPR); $$->target = $1; }
	| BREAK ';' { $$ = stmt(STMT_BREAK); }
	| CONTINUE ';' { $$ = stmt(STMT_CONTINUE); }
	;

vardef
	: varsig ':' type '=' initializer ';' { $$ = stmt(STMT_VAR_DEF);
										$1->kind = SYM_VAR;
										$1->type = $3;
										if ($5->rtype == NULL) {
                                            /* do nothing */
                                        } else if ($5->rtype->base == TYPE_ARRAY && $3->base == TYPE_STRUCT) {
                                            int i;

                                            if (sblen($3->struct_fields) != $5->rtype->array_size)
										        parser_error("Type of value does not match declared variable type!");

                                            for (i = 0; i < sblen($3->struct_fields); ++i)
                                                if (!comp_types($3->struct_fields[i]->type, $5->init.init_list[i]->rtype))
										            parser_error("Type of value does not match declared variable type!");

                                            $5->init.is_struct = 1;
                                            $5->rtype = $3;

                                        } else if (!types_compatible(&$5, $3))
										    parser_error("Type of value does not match declared variable type!");


										$$->vd.sym = $1; 
										$$->vd.val = $5; }
	| varsig '=' initializer ';' { $$ = stmt(STMT_VAR_DEF);
								$1->kind = SYM_VAR;
								$1->type = $3->rtype;
								if ($3->rtype == NULL)
								    parser_error("Illegal value for type infered variable"); 
								$$->vd.sym = $1;
								$$->vd.val = $3; }
	| varsig ':' type ';' { $$ = stmt(STMT_VAR_DEF);
                    $1->kind = SYM_VAR;
                    $1->type = $3;
                    $$->vd.sym = $1;
                    $$->vd.val = NULL; }
	;

varsig
	: LET ID { $$ = sym(strdup(yylval.text)); scope_add(scope, $$); }
	| CONST ID { $$ = sym(strdup(yylval.text)); $$->constant = 1; scope_add(scope, $$); }
    ;

initializer
    : '{' init_list '}' { $$ = expr(EXPR_INIT);
                          $$->init.init_list = $2;
                          $$->init.is_struct = 0;

                          if ($2 == NULL)
                            $$->rtype = NULL;
                          else
                            $$->rtype = ezot_arr($2[0]->rtype, sblen($2)); }
    | expression { $$ = $1; }
	| NEW type { $$ = expr(EXPR_NEW);
				 $$->rtype = ezot_pt($2); }
    ;

init_list
    : /* empty */ { $$ = NULL; }
    | constant_expression { $$ = NULL; sbpush($$, $1); }
	| init_list ',' constant_expression { if ($1 != NULL) {
                                    $$ = $1;
                                } else {
                                    $$ = NULL;
                                }
                                sbpush($$, $3); }
    ;

return
	: RETURN expression ';' { $$ = stmt(STMT_RETURN); $$->ret_val = $2; }
	;

funccall
	: ID OP arglist CP ';' { $$ = stmt(STMT_FUNC_CALL);
							$$->fc.sym = find_function($1);
							$$->fc.args = $3; }
	;

assign
    : unary_expression '=' expression ';' { $$ = assign_stmt($1, $3); }
    | unary_expression PLUS_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_ADD)); }
    | unary_expression SUB_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_SUB)); }
    | unary_expression MUL_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_MUL)); }
    | unary_expression DIV_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_DIV)); }
    | unary_expression MOD_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_MOD)); }
    | unary_expression AND_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_AND)); }
    | unary_expression BOR_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_OR)); }
    | unary_expression BXOR_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_XOR)); }
    | unary_expression BLSH_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_LSH)); }
    | unary_expression BRSH_EQ expression ';' { $$ = assign_stmt($1, bin_expr($1, $3, BIN_RSH)); }
    ;

ifstmt
    : IF cond statement elsestmt { $$ = stmt(STMT_IF);
                     $$->ifs.cond = $2;
                     $$->ifs.then = $3;
                     $$->ifs.otherwise = $4; }
    ;

elsestmt
    : /* empty */ { $$ = NULL; }
    | ELSE statement { $$ = $2; }
    ;

while
    : WHILE cond statement { $$ = stmt(STMT_WHILE);
                             $$->ws.cond = $2;
                             $$->ws.then = $3; }
    ;

for
    : FOR { push_scope(); } ID { $<symbol>$ = sym(strdup($3)); $<symbol>$->kind = SYM_VAR; $<symbol>$->type = ezot(TYPE_INT32); scope_add(scope, $<symbol>$); }
        IN expression DDOT expression statement {
                $$ = stmt(STMT_FOR);

                $$->frs.var = $<symbol>4;
                $$->frs.from = $6;
                $$->frs.to = $8;
                $$->frs.then = $9;

                pop_scope(); }
    ;

blockstmt
    : { push_scope(); } block { $$ = stmt(STMT_BLOCK);
                                $$->block = $2;
                                pop_scope(); }
    ;

arglist
	: /* empty */ { $$ = NULL; }
	| expression { $$ = NULL; sbpush($$, $1); }
	| arglist ',' expression { if ($1 != NULL) {
								$$ = $1;
							} else {
								$$ = NULL;
							}
							sbpush($$, $3); }
	;

type
	: ID { struct symbol *sym = scope_find(scope, yylval.text);
			if (sym->kind != SYM_TYPE)
				parser_error("expected type got '%s'", sym->name);
			$$ = sym->type;
		   }
	| OP CP { $$ = scope_find(scope, "void")->type; }
	| STAR type { $$ = ezot_pt($2); }
	| '[' int_lit_expression ']' type { $$ = ezot_arr($4, $2->int_val); }
	;

int_lit_expression
	: NUMBER { $$ = expr(EXPR_NUMBER);
				$$->int_val = yylval.int_val;
				$$->rtype = ezot(TYPE_INT32); }
    ;

constant_expression
	: STRING_LIT { $$ = expr(EXPR_STRING);
	                $$->string_lit = strdup(yylval.text);
                    $$->rtype = ezot_pt(ezot(TYPE_INT8));
	                }
	| int_lit_expression
	| NIL { $$ = expr(EXPR_NIL);
			$$->rtype = ezot(TYPE_NIL); }
    ;

primary_expression
    : constant_expression
	| ID {
			struct symbol *sym = scope_find(scope, yylval.text);
			 
			if (sym->kind == SYM_CONST_VAL) {
				$$ = expr(EXPR_NUMBER);
				$$->int_val = sym->constant;
				$$->rtype = ezot(TYPE_INT64);
			} else {
				$$ = expr(EXPR_VARIABLE); 
				if (sym->kind != SYM_VAR)
					parser_error("expected a variable but got '%s'", sym->name);
 
				$$->var_sym = sym;
				if (sym->type->base == TYPE_ARRAY)
					$$->rtype = ezot_pt(sym->type->to);
				else
					$$->rtype = sym->type;
			} }
	| OP expression CP { $$ = $2; }
	| ID OP arglist CP { $$ = expr(EXPR_FUNC_CALL);
							$$->fc.sym = find_function($1);
							$$->rtype = $$->fc.sym->type;
							$$->fc.args = $3; }
	| SIZEOF type { $$ = expr(EXPR_SIZEOF);
					$$->rtype = ezot(TYPE_INT64);
					$$->so.soty = $2; }
	;

postfix_expression
    : primary_expression { $$ = $1; }
    | postfix_expression '.' ID { $$ = expr(EXPR_MEM_ACCESS);
                                  $$->ma.container = $1;

								  struct ezo_type *sty;

								  if ($1->rtype->base == TYPE_POINTER && $1->rtype->to->base == TYPE_STRUCT) {
									  sty = $1->rtype->to;
									  $$->ma.ispointer = 1;
								  } else if ($1->rtype->base != TYPE_STRUCT) {
                                      parser_error("Can't access member on non-struct type!");
								  } else {
									  sty = $1->rtype;
									  $$->ma.ispointer = 0;
								  }
                                  
                                  int found = 0;
                                  for (int i = 0; i < sblen(sty->struct_fields); ++i) {
                                      if (strcmp(sty->struct_fields[i]->name, $3) == 0) {
                                          $$->ma.member = i;
                                          $$->rtype = sty->struct_fields[i]->type;
                                          found = 1;
                                      }
                                  }
                                  if (!found)
                                      parser_error("Member '%s' not found in struct '%s'\n", $3, sty->struct_symbol->name); }
	| postfix_expression PP { $$ = unary_expr($1, UNARY_POST_PP); }
	| postfix_expression MM { $$ = unary_expr($1, UNARY_POST_MM); }
	| postfix_expression '[' expression ']' { $$ = expr(EXPR_ARRAY_ACCESS);
                                            if ($1->rtype->base != TYPE_POINTER && $1->rtype->base != TYPE_ARRAY)
                                                parser_error("Tried to index non-array variable");
                                            $$->arr.target = $1;
                                            $$->arr.index = $3;
                                            $$->rtype = $1->rtype->to; }
    ;

unary_expression
    : postfix_expression { $$ = $1; }
	| EXCL unary_expression { $$ = unary_expr($2, UNARY_NOT); }
	| SUB unary_expression { $$ = unary_expr($2, UNARY_NEG); }
	| PP unary_expression { $$ = unary_expr($2, UNARY_PRE_PP); }
	| MM unary_expression { $$ = unary_expr($2, UNARY_PRE_MM); }
    | STAR unary_expression { $$ = expr(EXPR_DEREF);
	                    $$->target = $2; 
	                    $$->rtype = $2->rtype->to;
                        if ($2->rtype->base != TYPE_POINTER && $2->rtype->base != TYPE_ARRAY)
                            parser_error("Can't deref non-pointer variable");
                        }
	| AND unary_expression { $$ = expr(EXPR_REF);
	                    $$->target = $2;
                        $$->rtype = ezot_pt($2->rtype);
                        if ($2->kind != EXPR_VARIABLE && $2->kind != EXPR_ARRAY_ACCESS)
                            parser_error("Can't reference non-variable expression");
                        }
    ;

cast_expression
    : unary_expression { $$ = $1; }
    | OP type CP cast_expression { $$ = expr(EXPR_CAST);
                                    if (!types_can_convert($2, $4->rtype))
                                        parser_error("Can't cast expression to type");
                                    $$->rtype = $2;
                                    $$->cst.target = $4;
                                    $$->cst.type_to = $2; }
    ;

multiplicative_expression
    : cast_expression { $$ = $1; }
	| multiplicative_expression STAR cast_expression { $$ = bin_expr($1, $3, BIN_MUL); }
	| multiplicative_expression DIV cast_expression { $$ = bin_expr($1, $3, BIN_DIV); }
	| multiplicative_expression MOD cast_expression { $$ = bin_expr($1, $3, BIN_MOD); }
    ;

additive_expression
    : multiplicative_expression { $$ = $1; }
	| additive_expression ADD multiplicative_expression { $$ = bin_expr($1, $3, BIN_ADD); }
	| additive_expression SUB multiplicative_expression { $$ = bin_expr($1, $3, BIN_SUB); }
    ;

shift_expression
    : additive_expression { $$ = $1; }
	| shift_expression BLSH additive_expression { $$ = bin_expr($1, $3, BIN_LSH); }
	| shift_expression BRSH additive_expression { $$ = bin_expr($1, $3, BIN_RSH); }
    ;

relational_expression
    : shift_expression { $$ = $1; }
	| relational_expression LT shift_expression { $$ = cond_expr($1, $3, COND_LT); }
	| relational_expression LE shift_expression { $$ = cond_expr($1, $3, COND_LE); }
	| relational_expression GT shift_expression { $$ = cond_expr($1, $3, COND_GT); }
	| relational_expression GE shift_expression { $$ = cond_expr($1, $3, COND_GE); }
    ;

equality_expression
    : relational_expression { $$ = $1; }
	| equality_expression EQEQ relational_expression { $$ = cond_expr($1, $3, COND_EQ); }
	| equality_expression NOTEQ relational_expression { $$ = cond_expr($1, $3, COND_NEQ); }
    ;

bitwise_expression
    : equality_expression { $$ = $1; }
    | bitwise_expression AND equality_expression { $$ = bin_expr($1, $3, BIN_AND); }
    | bitwise_expression BOR equality_expression { $$ = bin_expr($1, $3, BIN_OR); }
    | bitwise_expression BXOR equality_expression { $$ = bin_expr($1, $3, BIN_XOR); }
    ;
	
logical_and_expression
	: bitwise_expression { $$ = $1; }
	| cond ANDAND cond { $$ = expr(EXPR_LOGICAL);
						 $$->rtype = $1->rtype;
						 $$->lg.lhs = $1;
						 $$->lg.rhs = $3; 
						 $$->lg.and = 1; }
	;

logical_or_expression
	: logical_and_expression { $$ = $1; }
	| cond BARBAR cond { $$ = expr(EXPR_LOGICAL);
						 $$->rtype = $1->rtype;
						 $$->lg.lhs = $1;
						 $$->lg.rhs = $3; 
						 $$->lg.and = 0; }
	;

expression
    : logical_or_expression
    ;

cond
    : expression { if ($1->kind == EXPR_COND) {
                        $$ = $1;
                    } else {
                        struct expr *zero = expr(EXPR_NUMBER);
                        zero->int_val = 0;
                        zero->rtype = ezot(TYPE_INT32);
                        $$ = cond_expr($1, zero, COND_NEQ);
                    } }
    ;

%%

void yyerror(const char *s) {
	printf("Error: %s\n", s);
}

void parser_init(struct cli_options *options) {
	compiler_options = options;
	functions = NULL;

	scope = scope_create(NULL);

    char *type_names[] = { "bool", "i8", "i16",
                                "i32", "i64", "u8",
                                "u16", "u32", "u64",
                                "void" };
    int type_types[] = { TYPE_INT1, TYPE_INT8, TYPE_INT16,
                                TYPE_INT32, TYPE_INT64, TYPE_INTU8,
                                TYPE_INTU16, TYPE_INTU32, TYPE_INTU64,
                                TYPE_VOID };

    int type_count = sizeof(type_names) / sizeof(*type_names);

    for (int i = 0; i < type_count; ++i) {
        struct symbol *tys = sym(type_names[i]);
        tys->kind = SYM_TYPE;
        tys->type = ezot(type_types[i]);
        scope_add(scope, tys);
    }
}

void parser_free(void) {
	sbfree(scope->symbols);
	free(scope);
}

void push_scope(void) {
	scope = scope_create(scope);
}

void pop_scope(void) {
	struct scope *old = scope;
	scope = scope->parent;
	sbfree(old->symbols);
	free(old);
}

struct symbol *find_function(char *name) {
	int i;

	for (i = 0; i < sblen(functions); ++i)
		if (strcmp(functions[i]->name, name) == 0)
			return functions[i];

	parser_error("canno't find function '%s'\n", name);
	return NULL;
}

void use_std_library(char *lib_name) {
	char file_name[256];
	FILE *lib_src, *org_src;
	int i;

	for (i = 0; i < sblen(libraries); ++i)
		if (strcmp(libraries[i], lib_name) == 0)
			return;
	
	sbpush(libraries, lib_name);
	
	sprintf(file_name, "%s%s.ezo", STDLIB_PATH, lib_name);

	lib_src = fopen(file_name, "r");
	if (!lib_src)
		fatal("Standard library '%s' not found!", lib_name);

	yyrestart(lib_src);
	yyparse();

	fclose(lib_src);

	org_src = fopen(compiler_options->input_name, "r");
	
	yyrestart(org_src);
	yyparse();
}

void parser_error(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "Error in line %d:\n", yylineno);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
	exit(1);
}