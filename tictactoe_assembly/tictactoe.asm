; ----------------------------------------------------------------------------------------
; Play tictactoe with 2 players. Runs on 64-bit Linux only.
; To assemble and run:
;
;     make
; ----------------------------------------------------------------------------------------

	section .text
	global _start
	global exit_function
	global write_function
	global read_character
	global clear_stdin
	global show_board
	global player_turn
	global game_won
	global game_draw

; Calling conventions:
; rdi, rsi, rdx, rcx, r8, r9 are caller-saved
; r15b always stores the active player

; Function to exit the program
; Arguments:
; rdi: Status code to return with
exit_function:
	mov rax, 60                  ; system call for exit
	syscall                      ; invoke operating system to exit
	ret

; Function to write to stdout
; Arguments:
; rsi: address of the string to output
; rdx: number of bytes to write
write_function:
	mov rax, 1                   ; system call for write
	mov rdi, 1                   ; file handle 1 is stdout
	syscall                      ; invoke operating system to write
	ret

; Function to read from stdin
; Returns:
; rax: character read, or - 1 on invalid input or error
read_character:
	mov rax, 0                   ; system call for read
	mov rdi, 0                   ; file handle 0 is stdin
	mov rsi, buffer              ; address of input buffer
	mov rdx, buffer_size         ; maximum number of bytes to read
	syscall                      ; invoke operating system to read

	cmp rax, - 1                 ; check if read syscall failed (return value - 1)
	je return

check_input:
	; Expected input is one character + newline
	cmp byte [buffer + 1], 10    ; Second value should be newline
	jne invalid_input            ; If not, jump to invalid_input
    movzx rax, byte [buffer]     ; Return the first character
	ret


invalid_input:
	; Invalid because only a newline was entered.
	; In that case there is nothing in stdin to clear.
	cmp byte [buffer], 10
	je skip_clear
	call clear_stdin
skip_clear:
	mov rsi, overflow_message     ; address of overflow message
	mov rdx, overflow_message_len ; number of bytes in overflow message
	call write_function
	mov rax, - 1                  ; return - 1 to indicate invalid input

return:
	ret

; Function to drain stdin
clear_stdin:
	mov rax, 0                   ; system call for read
	mov rdi, 0                   ; file handle 0 is stdin
	mov rsi, buffer              ; address of input buffer
	mov rdx, 1                   ; maximum number of bytes to read
	syscall                      ; invoke operating system to read
	cmp byte [buffer], 10        ; Read one element from stdin until newline
	jne clear_stdin
	ret

; Function to show the board
show_board:
	mov dil, [board + 0]
	mov [board_delim2 + 1], dil
	mov dil, [board + 1]
	mov [board_delim2 + 5], dil
	mov dil, [board + 2]
	mov [board_delim2 + 9], dil
	mov rsi, board_delim2
	mov rdx, delim_size
	call write_function
	mov rsi, board_delim1
	mov rdx, delim_size
	call write_function

	mov dil, [board + 3]
	mov [board_delim2 + 1], dil
	mov dil, [board + 4]
	mov [board_delim2 + 5], dil
	mov dil, [board + 5]
	mov [board_delim2 + 9], dil
	mov rsi, board_delim2
	mov rdx, delim_size
	call write_function
	mov rsi, board_delim1
	mov rdx, delim_size
	call write_function

	mov dil, [board + 6]
	mov [board_delim2 + 1], dil
	mov dil, [board + 7]
	mov [board_delim2 + 5], dil
	mov dil, [board + 8]
	mov [board_delim2 + 9], dil
	mov rsi, board_delim2
	mov rdx, delim_size
	call write_function
	ret

; Function to perform a player turn.
; The player is always stored in r15b
player_turn:
	; Show the message whose turn it is
	mov rsi, player_turn_msg
	mov rdx, player_turn_msg_len
	call write_function
	; Write the active player
	mov [buffer], r15b
	mov byte [buffer+1], 10
	mov rsi, buffer
	mov rdx, buffer_size
	call write_function
	call show_board
	; Show the message to ask for the next move
	mov rsi, next_mv_msg
	mov rdx, next_mv_msg_len
	call write_function
	; Read the next move
	call read_character
	; Validate that it is a spot between 0-8
	cmp rax, '0'
	jb invalid_char
	cmp rax, '8'
	ja invalid_char
	; Turn the char into the integer it represents
	mov r14, rax
	sub rax, '0'
	; If a position is empty, it contains its char
	cmp byte [board + rax], r14b
	jne pos_taken
	mov [board + rax], r15b
	ret
invalid_char:
	mov rsi, invalid_char_msg
	mov rdx, invalid_char_msg_len
	call write_function
	jmp player_turn
pos_taken:
	mov rsi, pos_taken_msg
	mov rdx, pos_taken_msg_len
	call write_function
	jmp player_turn

; Function to check if the current player has won
; The current plyer is always stored in r15b
; Returns:
; rax: 1 of the game is won, 0 otherwise.
game_won:
	mov rdi, 0 ; Tracks the condition being checked
loop_conditions:
	cmp rdi, n_conditions
	je no_win
	mov rsi, 0 ; Tracks the spot in the condition
	mov rdx, 1 ; Tracks whether the current condition can still be fulfilled
loop_condition:
	; Calculate the offset for the win condition
	; Second spot in third condition (6,7,8)
	; rdi = 2, rsi = 1
	; r8 = 2*3 + 1 = 7
	;    1      2     3
	;  1 2 3  1 2 3  1 2 3
	; (0,1,2|,3,4,5|,6,7,8|,0,3,6,1,4,7,2,5,8,0,4,8,2,4,6)[7] = 7
	; rcx = condition_size * rdi + rsi
	mov rcx, condition_size
	imul rcx, rdi
	add rcx, rsi
	add rcx, win_conditions      ; Grab the spot to check
	movzx rcx, byte [rcx]        ; Grab the actual spot from memory.
grabbed:
	cmp byte [board + rcx], r15b ; Check if it's the current player
	jne condition_failed         ; If the current spot is not occupied by the player
	                             ; we are done with this condition and can check the next.
	inc rsi                      ; If it is occupied, go to the next spot in the condition
	cmp rsi, condition_size      ; If we have check all spots and
	jne loop_condition           ; they are all occupied by the current player,
	mov rax, 1                   ; we have a winner.
	ret
condition_failed:
	inc rdi
	jmp loop_conditions
no_win:
	mov rax, 0
	ret

; Function to check if the board is full
; Used to check for draw after game_won
; Returns:
; rax: 1 of the board is full, 0 otherwise.
game_draw:
	mov rdi, 0
loop_board:
	mov rsi, rdi
	add rsi, '0'
	cmp byte [board + rdi], sil
	je open_spots
	cmp rdi, 8
	je full_board
	inc rdi
	jmp loop_board
open_spots:
	mov rax, 0
	ret
full_board:
	mov rax, 1
	ret

; Function to swap the active player
; Active player is always stored in r15b
swap_player:
	cmp r15b, 'X'
	je swap_to_o
	mov r15b, 'X'
	ret
swap_to_o:
	mov r15b, 'O'
	ret


_start:
	; r15b will hold the active player char the whole game
	mov r15b, 'X'
game_loop:
	call player_turn
	; Could probably set flags directly
	; in game_won and game_draw
	call game_won
	cmp rax, 1
	je game_over_won
	call game_draw
	cmp rax, 1
	je game_over_draw
	call swap_player
	jmp game_loop

game_over_won:
	mov rsi, win_msg
	mov rdx, win_msg_len
	call write_function
	mov [buffer], r15b
	mov byte [buffer+1], 10
	mov rsi, buffer
	mov rdx, buffer_size
	call write_function
	jmp game_over

game_over_draw:
	mov rsi, draw_msg
	mov rdx, draw_msg_len
	call write_function
	jmp game_over

game_over:
	call show_board
	; Exit with code 0
	xor rdi, rdi        ; exit code 0
	call exit_function


	section .data
board: db "012345678"
board_delim1: db "---+---+---",10
board_delim2: db " 0 | 0 | 0 ",10
delim_size equ $ - board_delim2
win_conditions:
	db  0,1,2, ; Rows
	db	3,4,5,
	db	6,7,8,
	db	0,3,6, ; Columns
	db	1,4,7,
	db	2,5,8,
	db	0,4,8, ; Diagonals
	db	2,4,6
condition_size equ 3
n_conditions equ 8
buffer: times 2 db 0 ; input buffer, also used for writing active player.
buffer_size equ $ - buffer
overflow_message: db "Invalid input! Expected exactly one character!", 10
overflow_message_len equ $ - overflow_message
player_turn_msg: db "Player turn: " ; No newline at the end!
player_turn_msg_len equ $ - player_turn_msg
next_mv_msg: db "Where to make your next move? [0-8]",10
next_mv_msg_len equ $ - next_mv_msg
invalid_char_msg: db "Invalid character! Please enter a number between 0 and 8",10
invalid_char_msg_len equ $ - invalid_char_msg
pos_taken_msg: db "Position already taken! Please enter a different position",10
pos_taken_msg_len equ $ - pos_taken_msg
win_msg: db "Player wins the game: " ; Also no newline!
win_msg_len equ $ - win_msg
draw_msg: db "Match drawn!",10
draw_msg_len equ $ - draw_msg
