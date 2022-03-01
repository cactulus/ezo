#include <stdio.h>
#include <string.h>

#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>

#define LLVM_HEADER_
#include "ezo.h"
#include "sb.h"

struct string_constant {
    char *value;
    LLVMValueRef llvm_ref;
};

void init_llvm(struct cli_options *options);
void optimize(void);

static void gen_stmt(struct stmt *s);
static LLVMValueRef gen_expr(struct expr *e);
static LLVMValueRef gen_access(struct expr *e);
static LLVMValueRef gen_func_call(struct symbol *sym, struct expr **args);

static LLVMTypeRef convert_type(struct ezo_type *ty);
static int llvm_size_of(LLVMTypeRef ty);

static void push_continue_block(LLVMBasicBlockRef block);
static void push_break_block(LLVMBasicBlockRef block);
static LLVMBasicBlockRef pop_continue_block(void);
static LLVMBasicBlockRef pop_break_block(void);
static LLVMBasicBlockRef popif_continue_block(LLVMBasicBlockRef bb);
static LLVMBasicBlockRef popif_break_block(LLVMBasicBlockRef bb);

/* change file extension */
static char *cfext(char *src, char *ext);

static LLVMModuleRef mod;
static LLVMBuilderRef builder;
static LLVMValueRef current_func;
static LLVMTargetDataRef data_layout;
static LLVMTargetMachineRef target_machine;

static struct string_constant **string_constants = NULL;

#define MAX_LOOP_LEVEL 10
static LLVMBasicBlockRef continue_blocks[MAX_LOOP_LEVEL];
static LLVMBasicBlockRef break_blocks[MAX_LOOP_LEVEL];
static int cblocks_index = 0;
static int bblocks_index = 0;

void gen(struct stmt **AST, struct cli_options *options) {
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllAsmParsers();
    LLVMInitializeAllAsmPrinters();

	int i;
	char *error = NULL;

	mod = LLVMModuleCreateWithName("test");
	builder = LLVMCreateBuilder();

    init_llvm(options);

	/* register functions */
	for (i = 0; i < sblen(AST); ++i) {
		struct stmt *node = AST[i];
		if (node->kind != STMT_FUNC_DEF && node->kind != STMT_EXTERN) continue;

		int param_count = sblen(node->fd.params);
		int i;

		LLVMTypeRef *param_types = malloc(sizeof(LLVMTypeRef) * param_count);
		for (i = 0; i < param_count; ++i) {
			param_types[i] = convert_type(node->fd.params[i]->type);
		}

		LLVMTypeRef ftype = LLVMFunctionType(convert_type(node->fd.sym->type), param_types, param_count, node->fd.vararg);
		LLVMValueRef fn = LLVMAddFunction(mod, node->fd.sym->name, ftype);
		node->fd.sym->llvm_ref = fn;
	}

	for (i = 0; i < sblen(AST); ++i) {
		struct stmt *node = AST[i];
		if (node->kind == STMT_FUNC_DEF) continue;

		gen_stmt(node);
    }

	/* function body code generation */
	for (i = 0; i < sblen(AST); ++i) {
		struct stmt *node = AST[i];
		if (node->kind != STMT_FUNC_DEF) continue;

		gen_stmt(node);
    }

    if (options->flags & OPTIMIZE)
        optimize();

    if (options->flags & EMIT_IR)
        LLVMPrintModuleToFile(mod, cfext(options->input_name, ".ll"), &error);

    if (options->flags & EMIT_ASM) {
        char *asm_file = cfext(options->input_name, ".s");
        LLVMTargetMachineEmitToFile(target_machine, mod, asm_file, LLVMAssemblyFile, &error);
    }

    char *obj_file = cfext(options->input_name, ".o");
    LLVMTargetMachineEmitToFile(target_machine, mod, obj_file, LLVMObjectFile, &error);

    if ((options->flags & COMPILE_ONLY) == 0) {
        char *exec_file = cfext(options->input_name, ".out");
        char buf[256];
        sprintf(buf, "gcc -o %s %s", exec_file, obj_file);
        system(buf);
        remove(obj_file);
    }

    LLVMDumpModule(mod);

    for (i = 0; i < sblen(string_constants); ++i)
        free(string_constants[i]);
    sbfree(string_constants);
}

void init_llvm(struct cli_options *options) {
    char *errors = 0;

    char* target_triple = "x86_64-unknown-windows-cygnus";
   // char *target_triple = LLVMGetDefaultTargetTriple();

    int code_gen = (options->flags & OPTIMIZE) ? LLVMCodeGenLevelAggressive : LLVMCodeGenLevelNone;
    int reloc_model = LLVMRelocPIC;
    
    LLVMTargetRef target;
    LLVMGetTargetFromTriple(target_triple, &target, &errors);
    LLVMDisposeMessage(errors);
    target_machine = LLVMCreateTargetMachine(
                target,
                target_triple,
                "generic", LLVMGetHostCPUFeatures(),
                code_gen, reloc_model,
                LLVMCodeModelDefault
            );

    LLVMSetTarget(mod, target_triple);
    data_layout = LLVMCreateTargetDataLayout(target_machine);
    char *datalayout_str = LLVMCopyStringRepOfTargetData(data_layout);
    LLVMSetDataLayout(mod, datalayout_str);
}

void optimize(void) {
    LLVMPassManagerRef pass_manger = LLVMCreatePassManager();
    LLVMPassManagerBuilderRef pmbr = LLVMPassManagerBuilderCreate();
    LLVMPassManagerBuilderSetOptLevel(pmbr, 3);
    LLVMPassManagerBuilderPopulateModulePassManager(pmbr, pass_manger);

    LLVMRunPassManager(pass_manger, mod);
    LLVMDisposePassManager(pass_manger);
}

void gen_stmt(struct stmt *s) {
	switch (s->kind) {
		case STMT_FUNC_DEF: {
			int param_count = sblen(s->fd.params);
			int i;

			LLVMValueRef fn = LLVMGetNamedFunction(mod, s->fd.sym->name);
			current_func = fn;
			LLVMBasicBlockRef entry_block;

			s->fd.sym->llvm_ref = fn;

			entry_block = LLVMAppendBasicBlock(fn, "");
			LLVMPositionBuilderAtEnd(builder, entry_block);

			for (i = 0; i < param_count; ++i) {
				LLVMValueRef param = LLVMBuildAlloca(builder, convert_type(s->fd.params[i]->type), "");
				LLVMBuildStore(builder, LLVMGetParam(fn, i), param);
				s->fd.params[i]->llvm_ref = param;
			}

			for (i = 0; i < sblen(s->fd.stmts); ++i)
				gen_stmt(s->fd.stmts[i]);
		
		} break;
		case STMT_STRUCT_DEF: {
		    convert_type(s->struct_type);
        }
        case STMT_ENUM_DEF:
            break;
		case STMT_EXTERN: {
		} break;
		case STMT_VAR_DEF: {
			struct ezo_type *typ = s->vd.sym->type;
			LLVMTypeRef llvm_ty = convert_type(typ);
			
			LLVMValueRef var;
			if (s->vd.glbl) {
			    var = LLVMAddGlobal(mod, llvm_ty, "");
                LLVMSetGlobalConstant(var, s->vd.sym->constant);

			    s->vd.sym->llvm_ref = var;

                if (s->vd.val == NULL) break;

                struct expr *val_expr = s->vd.val;

                if (s->vd.val->kind != EXPR_INIT) {
                    LLVMSetInitializer(var, gen_expr(val_expr));
                } else if (s->vd.val->init.init_list != NULL) {
                    int count = val_expr->rtype->array_size, i;
                    LLVMValueRef *vals = malloc(sizeof(LLVMValueRef) * count);
                    LLVMTypeRef ty = convert_type(val_expr->rtype);

                    for (i = 0; i < count; ++i)
                        vals[i] = gen_expr(val_expr->init.init_list[i]);

                    LLVMSetInitializer(var, gen_expr(val_expr));
                }
            } else {
			    var = LLVMBuildAlloca(builder, llvm_ty, "");
			    s->vd.sym->llvm_ref = var;

                if (s->vd.val == NULL) break;
                
                struct expr *val_expr = s->vd.val;

                if (s->vd.val->kind != EXPR_INIT) {
                    LLVMValueRef val = gen_expr(val_expr);
                    LLVMBuildStore(builder, val, var);
                } else if (s->vd.val->init.init_list != NULL) {
                    LLVMValueRef val = gen_expr(val_expr);
                    int dst_align = LLVMGetAlignment(var);
                    int src_align = LLVMGetAlignment(val);

                    LLVMValueRef size;
                    int count;
                    if (val_expr->init.is_struct) {
                        int elm_size =  0;
                        for (int j = 0; j < sblen(typ->struct_fields); ++j)
                            elm_size += llvm_size_of(LLVMStructGetTypeAtIndex(llvm_ty, j));
                        size = LLVMConstInt(LLVMInt32Type(), elm_size, 0);
                    } else {
                        count = val_expr->rtype->array_size;
                        int elm_size = llvm_size_of(LLVMGetElementType(LLVMTypeOf(val)));
                        size = LLVMConstInt(LLVMInt32Type(), elm_size * count, 0);
                    }

                    LLVMBuildMemCpy(builder, var, dst_align, val, src_align, size);
                }
            }
		} break;
		case STMT_RETURN: {
			LLVMValueRef val = gen_expr(s->ret_val);
			LLVMBuildRet(builder, val);
		} break;
		case STMT_FUNC_CALL: {
			gen_func_call(s->fc.sym, s->fc.args);
		} break;
		case STMT_ASSIGN: {
            LLVMValueRef val = gen_expr(s->as.value);
            LLVMValueRef target = gen_access(s->as.target);
            LLVMBuildStore(builder, val, target);
        } break;
		case STMT_BLOCK: {
		    for (int i = 0; i < sblen(s->block); ++i)
		        gen_stmt(s->block[i]);
        } break;
		case STMT_IF: {
		    LLVMBasicBlockRef true_block = LLVMAppendBasicBlock(current_func, "");
		    LLVMBasicBlockRef false_block = NULL;
		    LLVMBasicBlockRef after_block = NULL;
		    if (s->ifs.otherwise) {
		        false_block = LLVMAppendBasicBlock(current_func, "");
		        after_block = LLVMAppendBasicBlock(current_func, "");
            } else {
		        false_block = LLVMAppendBasicBlock(current_func, "");
		        after_block = false_block;
            }
            LLVMValueRef cmp = gen_expr(s->ifs.cond);
            LLVMBuildCondBr(builder, cmp, true_block, false_block);
            LLVMPositionBuilderAtEnd(builder, true_block);
            gen_stmt(s->ifs.then);
            LLVMBuildBr(builder, s->ifs.otherwise ? after_block : false_block);
            LLVMPositionBuilderAtEnd(builder, false_block);
            if (s->ifs.otherwise) {
                gen_stmt(s->ifs.otherwise);
                LLVMBuildBr(builder, after_block);
                LLVMPositionBuilderAtEnd(builder, after_block);
            }
        } break;
        case STMT_WHILE: {
            LLVMBasicBlockRef cond_block = LLVMAppendBasicBlock(current_func, "");
            LLVMBasicBlockRef body_block = LLVMAppendBasicBlock(current_func, "");
            LLVMBasicBlockRef after_block = LLVMAppendBasicBlock(current_func, "");

            push_break_block(after_block);
            push_continue_block(cond_block);

            LLVMBuildBr(builder, cond_block);
            LLVMPositionBuilderAtEnd(builder, cond_block);
            LLVMValueRef cmp = gen_expr(s->ws.cond);

            LLVMBuildCondBr(builder, cmp, body_block, after_block);
            LLVMPositionBuilderAtEnd(builder, body_block);

            gen_stmt(s->ws.then);
            popif_break_block(after_block);
            popif_continue_block(cond_block);

            LLVMBuildBr(builder, cond_block);

            LLVMPositionBuilderAtEnd(builder, after_block);
        } break;
        case STMT_FOR: {
            LLVMValueRef from = gen_expr(s->frs.from);
            LLVMValueRef to = gen_expr(s->frs.to);

            LLVMBasicBlockRef cond_block = LLVMAppendBasicBlock(current_func, "");
            LLVMBasicBlockRef body_block = LLVMAppendBasicBlock(current_func, "");
            LLVMBasicBlockRef inc_block = LLVMAppendBasicBlock(current_func, "");
            LLVMBasicBlockRef after_block = LLVMAppendBasicBlock(current_func, "");

            push_break_block(after_block);
            push_continue_block(inc_block);

            LLVMValueRef inc_var = LLVMBuildAlloca(builder, LLVMInt32Type(), ""); 
            s->frs.var->llvm_ref = inc_var;

            LLVMBuildStore(builder, from, inc_var);
            LLVMBuildBr(builder, cond_block);
            LLVMPositionBuilderAtEnd(builder, cond_block);

            LLVMValueRef loaded_index = LLVMBuildLoad(builder, inc_var, "");
            LLVMValueRef cmp = LLVMBuildICmp(builder, LLVMIntSLT, loaded_index, to, "");
            LLVMBuildCondBr(builder, cmp, body_block, after_block);
    
            LLVMPositionBuilderAtEnd(builder, body_block);

            gen_stmt(s->frs.then);
            popif_break_block(after_block);
            popif_continue_block(inc_block);

            LLVMBuildBr(builder, inc_block);

            LLVMPositionBuilderAtEnd(builder, inc_block);
            loaded_index = LLVMBuildLoad(builder, inc_var, "");
            LLVMValueRef added = LLVMBuildAdd(builder, loaded_index, LLVMConstInt(LLVMInt32Type(), 1, 0), "");
            LLVMBuildStore(builder, added, inc_var);
            LLVMBuildBr(builder, cond_block);

            LLVMPositionBuilderAtEnd(builder, after_block);
        } break;
        case STMT_EXPR: {
            gen_expr(s->target);
            break;
        }
        case STMT_CONTINUE:
            LLVMBuildBr(builder, pop_continue_block());
            break;
        case STMT_BREAK:
            LLVMBuildBr(builder, pop_break_block());
            break;
		default:
			fatal("unexpected stmt");
	}
}

LLVMValueRef gen_expr(struct expr *e) {
	switch (e->kind) {
		case EXPR_NUMBER:
			return LLVMConstInt(LLVMInt32Type(), e->int_val, 0);
		case EXPR_STRING: {
            int i;

            for (i = 0; i < sblen(string_constants); ++i)
                if (strcmp(string_constants[i]->value, e->string_lit) == 0)
                    return string_constants[i]->llvm_ref;

            LLVMValueRef ref = LLVMBuildGlobalStringPtr(builder, e->string_lit, "");

            struct string_constant *sc = malloc(sizeof(struct string_constant));
            sc->llvm_ref = ref;
            sc->value = e->string_lit;
            sbpush(string_constants, sc);

		    return ref;
        }
		case EXPR_FUNC_CALL: {
			return gen_func_call(e->fc.sym, e->fc.args);
		}
		case EXPR_BIN: {
			LLVMValueRef lhs = gen_expr(e->bin.lhs);
			LLVMValueRef rhs = gen_expr(e->bin.rhs);

            if (e->rtype->base != TYPE_POINTER) {
                LLVMOpcode opcode;
                int unsignd = type_is_uint(e->rtype);
                switch (e->bin.bin_op) {
                    case BIN_ADD: opcode = LLVMAdd; break;
                    case BIN_SUB: opcode = LLVMSub; break;
                    case BIN_MUL: opcode = LLVMMul; break;
                    case BIN_DIV: opcode = unsignd ? LLVMUDiv : LLVMSDiv; break;
                    case BIN_MOD: opcode = unsignd ? LLVMURem : LLVMSRem; break;
                    case BIN_AND: opcode = LLVMAnd; break;
                    case BIN_OR: opcode = LLVMOr; break;
                    case BIN_XOR: opcode = LLVMXor; break;
                    case BIN_LSH: opcode = LLVMShl; break;
                    case BIN_RSH: opcode = LLVMAShr; break;
                }
                return LLVMBuildBinOp(builder, opcode, lhs, rhs, "");
            } else {
                switch (e->bin.bin_op) {
                    case BIN_ADD: {
                        return LLVMBuildInBoundsGEP(builder, lhs, &rhs, 1, "");
                    }
                    case BIN_SUB: {
                        LLVMValueRef neg = LLVMBuildSub(builder,
                                            LLVMConstInt(LLVMInt32Type(), 0, 0), rhs, "");
                        return LLVMBuildInBoundsGEP(builder, lhs, &neg, 1, "");
                    }
                    default:
                        fatal("illegal binary operator on pointer variable!");
                }
            }
		}
		case EXPR_COND: {
			LLVMValueRef lhs = gen_expr(e->cond.lhs);
			LLVMValueRef rhs = gen_expr(e->cond.rhs);

			LLVMIntPredicate opcode;
            int unsignd = type_is_uint(e->rtype);

			switch (e->cond.cond_op) {
                case COND_LT: opcode = unsignd ? LLVMIntULT : LLVMIntSLT; break;
                case COND_LE: opcode = unsignd ? LLVMIntULE : LLVMIntSLE; break;
                case COND_GT: opcode = unsignd ? LLVMIntUGT : LLVMIntSGT; break;
                case COND_GE: opcode = unsignd ? LLVMIntUGE : LLVMIntSGE; break;
				case COND_EQ: opcode = LLVMIntEQ; break;
				case COND_NEQ: opcode = LLVMIntNE; break;
			}
			return LLVMBuildICmp(builder, opcode, lhs, rhs, "");
		}
        case EXPR_REF: {
            return e->target->var_sym->llvm_ref;
        }
        case EXPR_UNARY: {

            switch (e->un.unary_op) {
                case UNARY_NEG: return LLVMBuildNeg(builder, gen_expr(e->un.target), "");
                case UNARY_NOT: return LLVMBuildNot(builder, gen_expr(e->un.target), "");
                case UNARY_PRE_PP:
                case UNARY_PRE_MM:
                case UNARY_POST_PP:
                case UNARY_POST_MM: {
                    LLVMValueRef target = gen_access(e->un.target);
                    LLVMValueRef loaded = LLVMBuildLoad(builder, target, "");
                    LLVMValueRef one;
                    if (e->un.unary_op == UNARY_PRE_PP || e->un.unary_op == UNARY_POST_PP) {
                        one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                    } else {
                        one = LLVMConstInt(LLVMInt32Type(), -1, 0);
                    }
                    LLVMValueRef val = LLVMBuildAdd(builder, loaded, one, "");
                    LLVMBuildStore(builder, val, target);
                    
                    if (e->un.unary_op == UNARY_PRE_PP || e->un.unary_op == UNARY_PRE_MM) {
                        return val;
                    } else {
                        return loaded;
                    }
                }
            }
        }
        case EXPR_INIT: {
            int count, i;
            if (e->init.is_struct)
                count = sblen(e->rtype->struct_fields);
            else
                count = e->rtype->array_size;

            LLVMValueRef *vals = malloc(sizeof(LLVMValueRef) * count);
            LLVMTypeRef ty = convert_type(e->rtype);

            for (i = 0; i < count; ++i)
                vals[i] = gen_expr(e->init.init_list[i]);

            LLVMValueRef const_val;
            if (e->init.is_struct)
                const_val = LLVMConstStruct(vals, count, 0);
            else
                const_val = LLVMConstArray(ty, vals, count);

            LLVMValueRef glob = LLVMAddGlobal(mod, ty, "");
            LLVMSetGlobalConstant(glob, 1);
            LLVMSetInitializer(glob, const_val);

            return glob;
        }
        case EXPR_NEW: {
            LLVMValueRef malloc_fn = LLVMGetNamedFunction(mod, "malloc");
            if (!malloc_fn) {
                LLVMTypeRef i64_ty = LLVMInt64Type();
                LLVMTypeRef malloc_fn_type = LLVMFunctionType(LLVMPointerType(LLVMVoidType(), 0), &i64_ty, 1, 0);
                malloc_fn = LLVMAddFunction(mod, "malloc", malloc_fn_type);
            }

            LLVMTypeRef target_type = convert_type(e->rtype);
            LLVMTypeRef type = convert_type(e->rtype->to);
            LLVMValueRef type_size = LLVMConstInt(LLVMInt64Type(), llvm_size_of(type), 0);

            LLVMValueRef mallocd = LLVMBuildCall(builder, malloc_fn, &type_size, 1, "");
            return LLVMBuildPointerCast(builder, mallocd, target_type, "");
        }
        case EXPR_VARIABLE: {
            LLVMValueRef target = gen_access(e);
            if (LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(target))) == LLVMArrayTypeKind) {
                LLVMValueRef indices[] = {
                    LLVMConstInt(LLVMInt32Type(), 0, 0),
                    LLVMConstInt(LLVMInt32Type(), 0, 0),
                };

                return LLVMBuildInBoundsGEP(builder, target, indices, 2, "");
            }
            return LLVMBuildLoad(builder, target, "");
        }
        case EXPR_ARRAY_ACCESS:
        case EXPR_MEM_ACCESS:
        case EXPR_DEREF:
            return LLVMBuildLoad(builder, gen_access(e), "");
        case EXPR_CAST: {
            struct expr *target = e->cst.target;
            struct ezo_type *from = target->rtype;
            struct ezo_type *to = e->cst.type_to;

            LLVMValueRef llvm_target = gen_expr(target);
            LLVMTypeRef llvm_to = convert_type(to);
            
            if (from->base == TYPE_POINTER && to->base == TYPE_POINTER)
                return LLVMBuildPointerCast(builder, llvm_target, llvm_to, "");


            if (type_is_int(from) && type_is_int(to)) {
                if (type_is_uint(from) && to->base > from->base) {
                    return LLVMBuildZExt(builder, llvm_target, llvm_to, "");
                }
                return LLVMBuildIntCast2(builder, llvm_target, llvm_to, type_is_uint(from), "");
            }
        }
        case EXPR_SIZEOF:
            return LLVMConstInt(LLVMInt64Type(), llvm_size_of(convert_type(e->so.soty)), 0);
        case EXPR_NIL:
            return LLVMConstNull(convert_type(e->rtype));
        case EXPR_LOGICAL: {
            LLVMBasicBlockRef rhs_block = LLVMAppendBasicBlock(current_func, "");
            LLVMBasicBlockRef merge_block = LLVMAppendBasicBlock(current_func, "");
            int isand = e->lg.and;

            LLVMValueRef lhs = gen_expr(e->lg.lhs);
            lhs = LLVMBuildIsNotNull(builder, lhs, "");

            if (isand) {
                LLVMBuildCondBr(builder, lhs, rhs_block, merge_block);
            } else {
                LLVMBuildCondBr(builder, lhs, merge_block, rhs_block);
            }

            LLVMBasicBlockRef lhs_block = LLVMGetInsertBlock(builder);

            LLVMPositionBuilderAtEnd(builder, rhs_block);
            LLVMValueRef rhs = gen_expr(e->lg.rhs);
            rhs = LLVMBuildIsNotNull(builder, rhs, "");

            LLVMBuildBr(builder, merge_block);
            LLVMPositionBuilderAtEnd(builder, merge_block);

            LLVMValueRef cmp = LLVMBuildPhi(builder, LLVMInt1Type(), "");
            LLVMAddIncoming(cmp, &lhs, &lhs_block, 1);
            LLVMAddIncoming(cmp, &rhs, &rhs_block, 1);

            return cmp;
        }
		default:
			fatal("unexpected expr");
			return NULL;
	}
}

LLVMValueRef gen_access(struct expr *e) {
    switch (e->kind) {
        case EXPR_VARIABLE:
            return e->var_sym->llvm_ref;
        case EXPR_DEREF: {
            LLVMValueRef target = gen_access(e->target);
            if (LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(target))) == LLVMArrayTypeKind) {
                LLVMValueRef indices[] = {
                    LLVMConstInt(LLVMInt32Type(), 0, 0),
                    LLVMConstInt(LLVMInt32Type(), 0, 0),
                };

                return LLVMBuildInBoundsGEP(builder, target, indices, 2, "");
            }
            if (LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(target))) == LLVMPointerTypeKind) {
                return LLVMBuildLoad(builder, target, "");
            }
            return target;
        }
        case EXPR_ARRAY_ACCESS: {
            LLVMValueRef target = gen_access(e->arr.target);
            LLVMValueRef indices[] = {
                LLVMConstInt(LLVMInt32Type(), 0, 0),
                gen_expr(e->arr.index)
            };

            return LLVMBuildInBoundsGEP(builder, target, indices, 2, "");
        }
        case EXPR_MEM_ACCESS: {
            LLVMValueRef target = gen_access(e->ma.container);
            if (e->ma.ispointer)
                target = LLVMBuildLoad(builder, target, "");
            LLVMValueRef indices[] = {
                LLVMConstInt(LLVMInt32Type(), 0, 0),
                LLVMConstInt(LLVMInt32Type(), e->ma.member, 0),
            };

            return LLVMBuildInBoundsGEP(builder, target, indices, 2, "");
        }
        default: {
            LLVMValueRef pt = gen_expr(e);
            return pt;
        }
    }
}

LLVMValueRef gen_func_call(struct symbol *sym, struct expr **args) {
	int arg_len = sblen(args), i;
	LLVMValueRef *llvm_args = malloc(sizeof(LLVMValueRef) * arg_len);

	for (i = 0; i < arg_len; ++i)
		llvm_args[i] = gen_expr(args[i]);
	
	return LLVMBuildCall(builder, sym->llvm_ref, llvm_args, arg_len, "");
}

LLVMTypeRef convert_type(struct ezo_type *ty) {
	switch (ty->base) {
        case TYPE_VOID: return LLVMVoidType();
        case TYPE_INT1: return LLVMInt1Type();
        case TYPE_INT8: return LLVMInt8Type();
        case TYPE_INT16: return LLVMInt16Type();
		case TYPE_INT32: return LLVMInt32Type();
        case TYPE_INT64: return LLVMInt64Type();
        case TYPE_INTU8: return LLVMIntType(8);
        case TYPE_INTU16: return LLVMIntType(16);
		case TYPE_INTU32: return LLVMIntType(32);
        case TYPE_INTU64: return LLVMIntType(64);
		case TYPE_POINTER: {
		    if (ty->to->base == TYPE_VOID) {
		        return LLVMPointerType(LLVMInt8Type(), 0);
            }
		    return LLVMPointerType(convert_type(ty->to), 0);
        }
		case TYPE_ARRAY:
		    return LLVMArrayType(convert_type(ty->to), ty->array_size);
		case TYPE_STRUCT: {
		    LLVMTypeRef llty = LLVMGetTypeByName(mod, ty->struct_symbol->name);
		    if (llty != NULL)
		        return llty;

            int fields = sblen(ty->struct_fields);
		    LLVMTypeRef *field_types = malloc(sizeof(LLVMTypeRef) * fields);
		    for (int i = 0; i < fields; ++i)
		        field_types[i] = convert_type(ty->struct_fields[i]->type);

            LLVMTypeRef sty = LLVMStructCreateNamed(LLVMGetGlobalContext(), ty->struct_symbol->name);
            LLVMStructSetBody(sty, field_types, fields, 0);
            return sty;
        }
	}
	fatal("unexpected type");
	return NULL;
}

int llvm_size_of(LLVMTypeRef ty) {
    return LLVMSizeOfTypeInBits(data_layout, ty) / 8;
}

void push_continue_block(LLVMBasicBlockRef block) {
    if (cblocks_index >= MAX_LOOP_LEVEL)
        fatal("Max loop level reached");

    continue_blocks[cblocks_index++] = block;
}

void push_break_block(LLVMBasicBlockRef block) {
    if (bblocks_index >= MAX_LOOP_LEVEL)
        fatal("Max loop level reached");

    break_blocks[bblocks_index++] = block;
}

LLVMBasicBlockRef pop_continue_block(void) {
    if (cblocks_index < 0)
        fatal("Illegal continue statement");

    return continue_blocks[cblocks_index--];
}

LLVMBasicBlockRef pop_break_block(void) {
    if (bblocks_index < 0)
        fatal("Illegal break statement");
    
    return break_blocks[bblocks_index--];
}

LLVMBasicBlockRef popif_continue_block(LLVMBasicBlockRef bb) {
    int i;

    for (i = 0; i < cblocks_index; ++i)
        if (continue_blocks[i] == bb)
            pop_continue_block();
}

LLVMBasicBlockRef popif_break_block(LLVMBasicBlockRef bb) {
    int i;

    for (i = 0; i < bblocks_index; ++i)
        if (break_blocks[i] == bb)
            pop_break_block();
}

char *cfext(char *src, char *ext) {
    size_t dot_index = 0;
    char *new;
    int n;
    for (n = strlen(src) - 1; n >= 0; n--) {
        if (src[n] == '.') {
            dot_index = n;
            break;
        }
    }
    if (dot_index == 0) {
        fatal("Illegal file extension of file '%s'.", src);
    }
    new = malloc(dot_index + strlen(ext) + 1);
    memcpy(new, src, dot_index);
    strcpy(new + dot_index, ext);
    new[dot_index + strlen(ext)] = '\0';
    return new;
}
