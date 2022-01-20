enum ezo_base_type {
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_STRUCT,
	TYPE_INT1,
	TYPE_INT8,
	TYPE_INT16,
	TYPE_INT32,
	TYPE_INT64,
	TYPE_INTU8,
	TYPE_INTU16,
	TYPE_INTU32,
	TYPE_INTU64,
	TYPE_VOID,
	TYPE_NIL,
};

struct ezo_type {
	int base;

    union {
        struct {
            struct ezo_type *to;
            int array_size;
        };

        struct {
            struct symbol *struct_symbol;
            struct symbol **struct_fields;
        };
    };
};

enum symbol_kind {
	SYM_VAR,
	SYM_TYPE,
	SYM_CONST_VAL,
};

#ifndef LLVM_HEADER_
typedef void * LLVMValueRef;
#endif

struct symbol {
	struct ezo_type *type;
	LLVMValueRef llvm_ref;
	char *name;
	int kind;
	int constant;
};

struct scope {
	struct scope *parent;
	struct symbol **symbols;
};

struct scope *scope_create(struct scope *scope);
struct symbol *scope_find(struct scope *scope, char *name);
void scope_add(struct scope *scope, struct symbol *sym);

enum stmt_kind {
	STMT_VAR_DEF,
	STMT_STRUCT_DEF,
	STMT_ENUM_DEF,
	STMT_FUNC_DEF,
	STMT_EXTERN,
	STMT_RETURN,
	STMT_FUNC_CALL,
	STMT_ASSIGN,
	STMT_IF,
	STMT_WHILE,
	STMT_BLOCK,
	STMT_FOR,
	STMT_EXPR,
};

struct expr;

struct stmt {
	int kind;
	
	union {
	    struct ezo_type *struct_type;
		struct expr *ret_val;
		struct stmt **block;
		struct expr *target;

		struct {
			struct symbol *sym;			
			struct expr *val;
			int glbl;
		} vd;

		struct {
			struct symbol *sym;
			struct symbol **params;
			struct stmt **stmts;
			int vararg;
		} fd;

		struct {
			struct symbol *sym;
			struct expr **args;
		} fc;

		struct {
            struct expr *target;
            struct expr *value;
        } as;

		struct {
            struct expr *cond;
            struct stmt *then;
            struct stmt *otherwise;
        } ifs;

        struct {
            struct expr *cond;
            struct stmt *then;
        } ws;

        struct {
            struct symbol *var;
            struct expr *from;
            struct expr *to;
            struct stmt *then;
        } frs;
	};
};

enum expr_kind {
	EXPR_NUMBER,
	EXPR_STRING,
	EXPR_VARIABLE,
	EXPR_FUNC_CALL,
	EXPR_BIN,
	EXPR_REF,
	EXPR_DEREF,
	EXPR_COND,
	EXPR_UNARY,
	EXPR_INIT,
	EXPR_NEW,
	EXPR_ARRAY_ACCESS,
	EXPR_CAST,
	EXPR_MEM_ACCESS,
	EXPR_SIZEOF,
	EXPR_NIL,
	EXPR_LOGICAL
};

enum bin_opcode {
	BIN_ADD,
	BIN_SUB,
	BIN_MUL,
	BIN_DIV,
	BIN_MOD,
	BIN_AND,
	BIN_OR,
	BIN_XOR,
	BIN_LSH,
	BIN_RSH
};

enum cond_opcode {
	COND_LT,
	COND_GT,
	COND_LE,
	COND_GE,
	COND_EQ,
	COND_NEQ,
};

enum unary_op {
    UNARY_NOT, 
    UNARY_NEG, 
    UNARY_PRE_PP, 
    UNARY_POST_PP, 
    UNARY_PRE_MM, 
    UNARY_POST_MM, 
};

struct expr {
	struct ezo_type *rtype;
	int kind;
	
	union {
		struct symbol *var_sym;
		struct expr *target;
		char *string_lit;
		int int_val;

		struct {
			struct symbol *sym;
			struct expr **args;
		} fc;
		
		struct {
			struct expr *lhs;
			struct expr *rhs;
			int bin_op;
		} bin;
		
		struct {
			struct expr *lhs;
			struct expr *rhs;
			int cond_op;
		} cond;
		
		struct {
		    struct expr *target;
			int unary_op;
		} un;

		struct {
		    struct expr *target;
		    struct expr *index;
        } arr;

        struct {
            struct expr *target;
            struct ezo_type *type_to;
        } cst;
		
		struct {
		    struct expr **init_list;
		    int is_struct;
        } init;

        struct {
            struct expr *container;        
            int member;
			int ispointer;
        } ma;

		struct {
			struct ezo_type *soty;
		} so;

		struct {
			struct expr *lhs;
			struct expr *rhs;
			int and;
		} lg;
	};
};

struct expr *expr(int kind);
struct stmt *stmt(int kind);
struct symbol *sym(char *name);
struct ezo_type *ezot(int base);
struct ezo_type *ezot_pt(struct ezo_type *ty);
struct ezo_type *ezot_arr(struct ezo_type *ty, int size);
struct ezo_type *ezot_struct(struct symbol *name);

struct stmt *assign_stmt(struct expr *target, struct expr *value);
struct expr *bin_expr(struct expr *lhs, struct expr *rhs, int op);
struct expr *cond_expr(struct expr *lhs, struct expr *rhs, int op);
struct expr *unary_expr(struct expr *target, int op);

int type_is_int(struct ezo_type *ty);
int type_is_uint(struct ezo_type *ty);
int types_can_convert(struct ezo_type *ty1, struct ezo_type *ty2);
int types_compatible(struct expr **f, struct ezo_type *ty);
int types_compatible2(struct expr **f, struct expr **s);
int comp_types(struct ezo_type *f, struct ezo_type *s);

void fatal(const char *fmt, ...);

