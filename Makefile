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
	
analyser : $(OBJ_DIR)/analyseurLexicale.cmo $(OBJ_DIR)/state.cmo $(OBJ_DIR)/arithExp.cmo $(OBJ_DIR)/boolExp.cmo $(OBJ_DIR)/analyseurSyntaxique.cmo $(OBJ_DIR)/config.cmo
	@echo "Compiling analyser\n"
	$(CC) $^ -o $(BIN_DIR)/$@
	
lexical : $(OBJ_DIR)/analyseurLexical.cmo
	@echo "Compiling lexical analyser\n"
	$(CC) $^ -o $(BIN_DIR)/$@
    
	
$(OBJ_DIR)/config.cmi : $(LIB_DIR)/config.mli $(OBJ_DIR)/state.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/state.cmi : $(LIB_DIR)/state.mli
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@

$(OBJ_DIR)/analyseurSyntaxique.cmi : $(LIB_DIR)/analyseurSyntaxique.mli
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/analyseurLexicale.cmi : $(LIB_DIR)/analyseurLexicale.mli
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/arithExp.cmi : $(LIB_DIR)/arithExp.mli
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/boolExp.cmi : $(LIB_DIR)/boolExp.mli
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/config.cmo : $(SRC_DIR)/config.ml $(OBJ_DIR)/config.cmi $(OBJ_DIR)/state.cmo
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/state.cmo : $(SRC_DIR)/state.ml $(OBJ_DIR)/state.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@

$(OBJ_DIR)/analyseurSyntaxique.cmo : $(SRC_DIR)/analyseurSyntaxique.ml $(OBJ_DIR)/analyseurSyntaxique.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@

$(OBJ_DIR)/analyseurLexicale.cmo : $(SRC_DIR)/analyseurLexicale.ml $(OBJ_DIR)/analyseurLexicale.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/arithExp.cmo : $(SRC_DIR)/arithExp.ml $(OBJ_DIR)/arithExp.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@
	
$(OBJ_DIR)/boolExp.cmo : $(SRC_DIR)/boolExp.ml $(OBJ_DIR)/boolExp.cmi
	$(CC) -c $(FLAGS) $< -I $(OBJ_DIR)/ -o $@

clean :
	@echo "Cleaning obj dir and bin dir\n"
	rm -r $(OBJ_DIR)/
	rm -r $(BIN_DIR)/

