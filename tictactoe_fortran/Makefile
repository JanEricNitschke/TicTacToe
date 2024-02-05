FC = gfortran
# Compiler flags
CFLAGS = -std=f2018 -Wall -Wextra -Wfloat-equal -Wundef -Wshadow -Wpointer-arith \
		  -Wcast-align -Wstrict-prototypes -Wstrict-overflow=5 -Wwrite-strings \
		  -Waggregate-return -Wcast-qual -Wswitch-default -Wswitch-enum \
		  -Wconversion -Wunreachable-code -Wformat=2 -Winit-self -Werror \
		  -pedantic
TARGET = tictactoe
BIN_DIR = bin

SRC_DIR = src
OBJ_DIR = obj

OBJ_FILES = $(OBJ_DIR)/main.o $(OBJ_DIR)/tictactoe_module.o

.PHONY: all clean

all: $(BIN_DIR)/$(TARGET)

$(BIN_DIR)/$(TARGET): $(OBJ_FILES)
	@mkdir -p $(BIN_DIR)
	$(FC) $(CFLAGS) -o $@ $^

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
