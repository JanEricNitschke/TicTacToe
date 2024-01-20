CC = gcc
CFLAGS = -std=c17 -Wall -Wextra -Wfloat-equal -Wundef -Wshadow -Wpointer-arith \
          -Wcast-align -Wstrict-prototypes -Wstrict-overflow=5 -Wwrite-strings \
          -Waggregate-return -Wcast-qual -Wswitch-default -Wswitch-enum \
          -Wconversion -Wunreachable-code -Wformat=2 -Winit-self

TARGET_DIR = bin
TARGET = $(TARGET_DIR)/tictactoe_c
SRC = main.c
BUILD_DIR = build
OBJ = $(BUILD_DIR)/$(SRC:.c=.o)

all: $(BUILD_DIR) $(TARGET_DIR) $(TARGET)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(TARGET_DIR):
	mkdir -p $(TARGET_DIR)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^

$(BUILD_DIR)/%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

test:
	@echo "Running tests..."

clean:
	rm -rf $(BUILD_DIR) $(TARGET_DIR)
