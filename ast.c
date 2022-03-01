#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ezo.h"
#include "sb.h"

struct scope *scope_create(struct scope *parent) {
	struct scope *s = malloc(sizeof(struct scope));
	s->parent = parent;
	s->symbols = NULL;
	return s;
}

struct symbol *scope_find(struct scope *scope, char *name) {
	struct scope *cur = scope;
	int i;

	while (cur) {
		for (i = 0; i < sblen(cur->symbols); ++i)
			if (strcmp(cur->symbols[i]->name, name) == 0)
				return cur->symbols[i];
		cur = cur->parent;
	}

	fatal("symbol '%s' not found!", name);
	return NULL;
}

void scope_add(struct scope *scope, struct symbol *sym) {
	int i;

	for (i = 0; i < sblen(scope->symbols); ++i)
		if (strcmp(scope->symbols[i]->name, sym->name) == 0)
			fatal("symbol '%s'%d already exists!", sym->name, i);

	sbpush(scope->symbols, sym);
}

struct expr *expr(int kind) {
	struct expr *e = ezo_alloc_bytes(sizeof(struct expr));
	e->kind = kind;
	return e;
}

struct stmt *stmt(int kind) {
	struct stmt *s = ezo_alloc_bytes(sizeof(struct stmt));
	s->kind = kind;
	return s;
}

struct symbol *sym(char *name) {
	struct symbol *sym = ezo_alloc_bytes(sizeof(struct symbol));
	sym->name = name;
	sym->constant = 0;
	return sym;
}

struct ezo_type *ezot(int base) {
	struct ezo_type *ty = ezo_alloc_bytes(sizeof(struct ezo_type));
	ty->base = base;
	return ty;
}

struct ezo_type *ezot_pt(struct ezo_type *ty) {
	struct ezo_type *ptty = ezot(TYPE_POINTER);
	ptty->to = ty;
	return ptty;
}

struct ezo_type *ezot_arr(struct ezo_type *ty, int size) {
	struct ezo_type *arty = ezot(TYPE_ARRAY);
	arty->to = ty;
    arty->array_size = size;
	return arty;
}

struct ezo_type *ezot_struct(struct symbol *name) {
	struct ezo_type *sty = ezot(TYPE_STRUCT);
	sty->struct_symbol = name;
	sty->struct_fields = NULL;
	return sty;
}

struct stmt *assign_stmt(struct expr *target, struct expr *value) {
    struct stmt *s = stmt(STMT_ASSIGN);

    if (!types_compatible(&value, target->rtype))
        fatal("Tried to assign value to variable of other type!");

    s->as.target = target;
    s->as.value = value;
    return s;
}

struct expr *bin_expr(struct expr *lhs, struct expr *rhs, int op) {
	struct expr *be = expr(EXPR_BIN);
    
    if (lhs->rtype->base != TYPE_POINTER || !type_is_int(rhs->rtype)) {
        if (!types_compatible2(&lhs, &rhs))
            fatal("Binary operator can be applied to values of the same type only!");
    }

	be->rtype = lhs->rtype;
	be->bin.lhs = lhs;
	be->bin.rhs = rhs;
	be->bin.bin_op = op;
	return be;
}

struct expr *cond_expr(struct expr *lhs, struct expr *rhs, int op) {
	struct expr *be = expr(EXPR_COND);

    if (!types_compatible2(&lhs, &rhs))
        fatal("Tried to compare two types that are not compatible!");

	be->rtype = ezot(TYPE_INT1);
	be->cond.lhs = lhs;
	be->cond.rhs = rhs;
	be->cond.cond_op = op;
	return be;
}

struct expr *unary_expr(struct expr *target, int op) {
	struct expr *be = expr(EXPR_UNARY);
	be->rtype = target->rtype;
	be->un.target = target;
	be->un.unary_op = op;
	return be;
}

int type_is_uint(struct ezo_type *ty) {
    return ty->base >= TYPE_INTU8 && ty->base <= TYPE_INTU64;
}

int type_is_int(struct ezo_type *ty) {
    return ty->base >= TYPE_INT1 && ty->base <= TYPE_INTU64;
}

int types_can_convert(struct ezo_type *ty1, struct ezo_type *ty2) {
    if (comp_types(ty1, ty2))
        return 1;

    if (ty1->base == TYPE_POINTER && ty2->base == TYPE_POINTER)
        return 1;

    if (type_is_int(ty1) && type_is_int(ty2))
        return 1;

    return 0;
}

int types_compatible(struct expr **f, struct ezo_type *ty) {
    struct ezo_type *ft = (*f)->rtype;

    if (comp_types(ft, ty))
        return 1;

    int convertable = 0;

	if (ft->base == TYPE_NIL) {
		(*f)->rtype = ty;
		return 1;
	}

    if (!types_can_convert(ft, ty))
        return 0;
    
    struct expr *cast_expr = expr(EXPR_CAST);
    cast_expr->rtype = ty;
    cast_expr->cst.target = *f;
    cast_expr->cst.type_to = ty;
    
    *f = cast_expr;

    return 1;
}

int types_compatible2(struct expr **fr, struct expr **sr) {
    if (types_compatible(fr, (*sr)->rtype))
        return 1;

    if (types_compatible(sr, (*fr)->rtype))
        return 1;

    return 0;
}

int comp_types(struct ezo_type *f, struct ezo_type *s) {
    if (f->base == TYPE_ARRAY && s->base == TYPE_ARRAY) {
        return comp_types(f->to, s->to) && f->array_size == s->array_size;
    }
    if (f->base == TYPE_POINTER && s->base == TYPE_POINTER) {
        return comp_types(f->to, s->to);
    }
    if (f->base == TYPE_STRUCT && s->base == TYPE_STRUCT) {
        /* possible because type names are unique */
        return strcmp(f->struct_symbol->name, s->struct_symbol->name) == 0;
    }

    return f->base == s->base;
}

void fatal(const char *fmt, ...) {
    extern int yylineno;

	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "Error: ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
	exit(1);
}
