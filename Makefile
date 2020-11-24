SRC_DIR = src
LIB_DIR = includes
OBJ_DIR = obj
BIN_DIR = bin
MKDIR = mkdir

CC = ocamlc
FLAGS = 

all : directories

directories : $(OBJ_DIR) $(BIN_DIR)

$(OBJ_DIR) :
	$(MKDIR) $(OBJ_DIR)

$(BIN_DIR) :
	$(MKDIR) $(BIN_DIR)

clean :
	@echo "Cleaning obj dir and bin dir\n"
	rm -r $(OBJ_DIR)/
	rm -r $(BIN_DIR)/

