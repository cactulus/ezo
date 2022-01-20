enum cli_flags {
    OPTIMIZE     = 1 << 0 ,
    EMIT_IR      = 1 << 1,
    EMIT_ASM     = 1 << 2,
    COMPILE_ONLY = 1 << 3,
};

struct cli_options {
    FILE *input;
    char *input_name;
    unsigned long int flags;
};
