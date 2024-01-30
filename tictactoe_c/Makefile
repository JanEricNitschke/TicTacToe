CC = gcc
CFLAGS = -std=c17 -Wall -Wextra -Wfloat-equal -Wundef -Wshadow -Wpointer-arith \
		  -Wcast-align -Wstrict-prototypes -Wstrict-overflow=5 -Wwrite-strings \
		  -Waggregate-return -Wcast-qual -Wswitch-default -Wswitch-enum \
		  -Wconversion -Wunreachable-code -Wformat=2 -Winit-self -Werror

TARGET_DIR = bin
TARGET = $(TARGET_DIR)/tictactoe_c
SRC_DIR = src
INCLUDE_DIR = include
BUILD_DIR = build
OBJ = $(BUILD_DIR)/main.o $(BUILD_DIR)/tictactoe.o

PYTHON_CMD = python

ifeq ($(OS),Windows_NT)
    # Windows-specific setup
    PYTHON_CMD = python
    EXTENSION_CMD = $(PYTHON_CMD) setup.py build_ext --inplace --compiler=mingw32
else
    # Non-Windows setup
    EXTENSION_CMD = $(PYTHON_CMD) setup.py build_ext --inplace
endif

all: $(BUILD_DIR) $(TARGET_DIR) $(TARGET) extension

extension:
	$(EXTENSION_CMD)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(TARGET_DIR):
	mkdir -p $(TARGET_DIR)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -I$(INCLUDE_DIR) -c -o $@ $<

test:
	@echo "Running tests..."

run_python:
	$(PYTHON_CMD) tictactoe_c.py

run_c:
	./$(TARGET)

clean:
	rm -rf $(BUILD_DIR) $(TARGET_DIR) *.o *.a *.so *.pyd
