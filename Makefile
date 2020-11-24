SRC_DIR = src
LIB_DIR = includes
OBJ_DIR = obj
BIN_DIR = bin
MKDIR = mkdir

CC = ocamlc
FLAGS = -g

all : directories analyser

directories : $(OBJ_DIR) $(BIN_DIR)

$(OBJ_DIR) :
	$(MKDIR) $(OBJ_DIR)

$(BIN_DIR) :
	$(MKDIR) $(BIN_DIR)
	
analyser : $(OBJ_DIR)/config.cmo $(OBJ_DIR)/state.cmo $(OBJ_DIR)/analyseurSyntaxique.cmo
	@echo "Compiling analyser\n"
	$(CC) $^ -o $(BIN_DIR)/$@
	
$(OBJ_DIR)/config.cmi : $(LIB_DIR)/config.mli
	$(CC) -c $(FLAGS) $< -o $@
	
$(OBJ_DIR)/state.cmi : $(LIB_DIR)/state.mli
	$(CC) -c $(FLAGS) $< -o $@

$(OBJ_DIR)/analyseurSyntaxique.cmi : $(LIB_DIR)/analyseurSyntaxique.mli
	$(CC) -c $(FLAGS) $< -o $@
	
$(OBJ_DIR)/config.cmo : $(SRC_DIR)/config.ml $(OBJ_DIR)/config.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/config.cmi -o $@
	
$(OBJ_DIR)/state.cmo : $(SRC_DIR)/state.ml $(OBJ_DIR)/state.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/state.cmi -o $@

$(OBJ_DIR)/analyseurSyntaxique.cmo : $(SRC_DIR)/analyseurSyntaxique.ml $(OBJ_DIR)/analyseurSyntaxique.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/analyseurSyntaxique.cmi -o $@

clean :
	@echo "Cleaning obj dir and bin dir\n"
	rm -r $(OBJ_DIR)/
	rm -r $(BIN_DIR)/

