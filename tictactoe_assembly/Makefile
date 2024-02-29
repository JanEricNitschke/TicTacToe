# Define the compiler and flags
NASM = nasm
NASMFLAGS = -f elf64

# Define the linker and flags
LD = ld # gcc
LDFLAGS = -m elf_x86_64

# Define the source file
SRC = tictactoe.asm

# Define the output executable
OUT = tictactoe

# Rule to build the executable
build: $(SRC)
	$(NASM) $(NASMFLAGS) $(SRC) -o $(OUT).o
	$(LD) $(LDFLAGS) $(OUT).o -o $(OUT)

# Rule to run the executable
run: build
	./$(OUT)

# Default target
.DEFAULT_GOAL := run

test:
	@echo "Running tests..."

# Rule to clean the generated files
clean:
	rm -f $(OUT) $(OUT).o
