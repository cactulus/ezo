#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "cli.h"
#include "sb.h"

extern int yyparse(void);
extern void parser_init(struct cli_options *options);
extern void gen(struct stmt **AST, struct cli_options *options);

extern FILE *yyin;

extern struct stmt **AST;

static void parse_args(int argc, char *argv[], struct cli_options *options) {
    options->input = NULL;

    while (argc--) {
        char *arg = *argv++;

        if (strcmp(arg, "--release") == 0) {
            options->flags |= OPTIMIZE;
        } else if (strcmp(arg, "-c") == 0) {
            options->flags |= COMPILE_ONLY;
        } else if (strcmp(arg, "-S") == 0) {
            options->flags |= EMIT_ASM;
        } else if (strcmp(arg, "--emit-ir") == 0) {
            options->flags |= EMIT_IR;
        } else {
            options->input = fopen(arg, "r");
            options->input_name = arg;
        }
    }

    if (options->input == NULL)
        fatal("usage: ezo <FILE> [--release]");
}

int main(int argc, char *argv[]) {
    struct cli_options options;

    parse_args(argc - 1, argv + 1, &options);

	yyin = options.input;

	parser_init(&options);
	int ret = yyparse();

	gen(AST, &options);

	fclose(options.input);

	return 0;
}

char *escape_str_lit(char *text) {
    int sl = strlen(text), nl = sl - 2, i;
    char *etext;

    for (i = 1; i < sl - 2; ++i) {
        if (text[i] != '\\') continue;

        switch (text[i+1]) {
            case 'n':
            case 'r':
            case 't':
            case '0':
                nl--;
                break;
            default:
                break;
        }
    }

    etext = malloc(nl);
    for (i = 1; i < sl - 1; ++i) {
        if (text[i] != '\\') {
            etext[i-1] = text[i]; 
            continue;
        }

        switch (text[i+1]) {
            case 'n':
                etext[i-1] = '\n';
                break;
            case 'r':
                etext[i-1] = '\t';
                break;
            case 't':
                etext[i-1] = '\r';
                break;
            case '0':
                etext[i-1] = '\0';
                break;
            default:
                break;
        }
    }

    etext[nl] = '\0';
    return etext;
}
