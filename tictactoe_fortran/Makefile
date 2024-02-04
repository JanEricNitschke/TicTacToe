FC = gfortran
TARGET = tictactoe
BIN_DIR = bin

SRC_DIR = src
OBJ_DIR = obj

OBJ_FILES = $(OBJ_DIR)/main.o $(OBJ_DIR)/tictactoe_module.o

.PHONY: all clean

all: $(BIN_DIR)/$(TARGET)

$(BIN_DIR)/$(TARGET): $(OBJ_FILES)
	@mkdir -p $(BIN_DIR)
	$(FC) -o $@ $^

$(OBJ_DIR)/main.o: $(SRC_DIR)/main.f90 $(OBJ_DIR)/tictactoe_module.o
	@mkdir -p $(OBJ_DIR)
	$(FC) -c -o $@ $<

$(OBJ_DIR)/tictactoe_module.o: $(SRC_DIR)/tictactoe.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) -c -o $@ $<

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR) *.mod

test:
	@echo "Running tests..."
