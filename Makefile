EXEC := ezo.out

INSTALL_PATH := /usr/local/bin
STDLIB_PATH := $(INSTALL_PATH)/ezostd/
BIN := ezo

LLVM_CONFIG:=llvm-config
CFLAGS+=`$(LLVM_CONFIG) --cflags`
LIBS+=`$(LLVM_CONFIG) --libs --ldflags`

CFLAGS += -ggdb -D STDLIB_PATH="\"$(STDLIB_PATH)\""

$(BIN):
	bison -d parser.y
	flex lex.l
	gcc lex.yy.c parser.tab.c ast.c gen.c main.c alloc.c -o $(EXEC) $(CFLAGS) $(LIBS)

install: $(BIN)
	mkdir -p $(STDLIB_PATH)
	cp -f -r std/* $(STDLIB_PATH)
	cp -f $(EXEC) $(INSTALL_PATH)/$(BIN)
	chmod 755 $(INSTALL_PATH)/$(BIN)

uninstall:
	rm -f -r $(STDLIB_PATH)*
	rmdir $(STDLIB_PATH)
	rm -f $(INSTALL_PATH)/$(BIN)

clean:
	rm -f $(EXEC) lex.yy.c parser.tab.c parser.tab.h
