#!/bin/bash

# # Initialize the board with empty spaces
board=("0" "1" "2" "3" "4" "5" "6" "7" "8")

# Possible win conditions
win_conditions=(
	"0 1 2"
	"3 4 5"
	"6 7 8"
	"0 3 6"
	"1 4 7"
	"2 5 8"
	"0 4 8"
	"2 4 6"
)

print_board() {
	echo " ${board[0]} | ${board[1]} | ${board[2]} "
	echo "---+---+---"
	echo " ${board[3]} | ${board[4]} | ${board[5]} "
	echo "---+---+---"
	echo " ${board[6]} | ${board[7]} | ${board[8]} "
}

check_win() {
	local current_player
	current_player=$1
	for condition in "${win_conditions[@]}"; do
		# Split the condition into indices
		read -r -a indices <<<"$condition"
		if [[ ${board[indices[0]]} == "$current_player" && ${board[indices[0]]} == "${board[indices[1]]}" && ${board[indices[1]]} == "${board[indices[2]]}" ]]; then
			return 0
		fi
	done
	return 1
}

check_draw() {
	for i in {0..8}; do
		if [[ ${board[i]} != "X" && ${board[i]} != "O" ]]; then
			return 1
		fi
	done
	return 0
}

player_turn() {
	while true; do
		echo "Player $player's turn. Enter position (0-8):"
		print_board
		read -r position

		if [[ ! $position =~ ^[0-8]$ ]]; then
			echo "Position outside of allowed range (0-8)."
			continue
		fi

		index=$((position))

		if [[ ${board[index]} == "X" || ${board[index]} == "O" ]]; then
			echo "Position already taken."
			continue
		fi
		board[index]=$player
		break
	done
}

ai_turn() {
	echo "AI turn as player $player with strength $1."
	print_board
	case $1 in
	1) random_move ;;
	2) win_move ;;
	3) win_block_move ;;
	*) minmax_move ;;
	esac
	sleep 1
}

random_move() {
	local empty_cells=()
	for ((i = 0; i < ${#board[@]}; i++)); do
		if [[ ${board[$i]} != "X" && ${board[$i]} != "O" ]]; then
			empty_cells+=("$i")
		fi
	done
	local random_index=$((RANDOM % ${#empty_cells[@]}))
	local random_cell=${empty_cells[$random_index]}
	board[random_cell]=$player
}

try_win_move() {
	local current_player
	current_player=$1
	for condition in "${win_conditions[@]}"; do
		read -r -a indices <<<"$condition"
		local count=0
		local empty_index=-1
		for index in "${indices[@]}"; do
			if [[ ${board[$index]} == "$current_player" ]]; then
				((count++))
			elif [[ ${board[$index]} != "X" && ${board[$index]} != "O" ]]; then
				empty_index=$index
			fi
		done
		if [[ $count -eq 2 && $empty_index -ne -1 ]]; then
			echo $empty_index
			return
		fi
	done
	echo 9
}

win_move() {
	local -i index
	index=$(try_win_move "$player")
	if [[ $index -ne 9 ]]; then
		board[index]=$player
		return
	fi
	random_move
}

win_block_move() {
	local -i index

	index=$(try_win_move "$player")
	if [[ $index -ne 9 ]]; then
		board[index]=$player
		return
	fi
	local opponent
	opponent=$([[ $player == "X" ]] && echo "O" || echo "X")
	index=$(try_win_move "$opponent")
	if [[ $index -ne 9 ]]; then
		board[index]=$player
		return
	fi
	random_move
}

get_best_move() {
	# 0 for loss, 1 for draw, 2 for win
	local current_player=$1
	local opponent_player
	if [[ $current_player == "X" ]]; then
		opponent_player="O"
	else
		opponent_player="X"
	fi

	if check_win "$current_player"; then
		echo 2
		return
	fi

	if check_win "$opponent_player"; then
		echo 0
		return
	fi

	local empty_cells=()
	for ((i = 0; i < ${#board[@]}; i++)); do
		if [[ ${board[$i]} != "X" && ${board[$i]} != "O" ]]; then
			empty_cells+=("$i")
		fi
	done

	if [[ ${#empty_cells[@]} -eq 0 ]]; then
		echo 1
		return
	fi

	if [[ ${#empty_cells[@]} -eq ${#board[@]} ]]; then
		echo $(((RANDOM % ${#empty_cells[@]}) * 10))
		return
	fi

	local -i best_score=-100
	local -i score
	local -i best_move=-1
	for cell in "${empty_cells[@]}"; do
		board[cell]=$current_player
		score=$(get_best_move "$opponent_player")
		score=$((score % 10))
		score=$((2 - score))

		if [[ $score -gt $best_score ]]; then
			best_score=$score
			best_move=$cell

		fi
		board[cell]=$cell
	done
	local -i move_score
	move_score=$((best_move * 10 + best_score))
	echo "$move_score"
}

minmax_move() {
	echo "Calculating best move for player $player..."
	move_score=$(get_best_move "$player")
	index=$((move_score / 10))
	board[index]=$player
}

player="X"
DEFAULT_X_STRENGTH="4"
DEFAULT_O_STRENGTH="4"
X_strength=${1:-$DEFAULT_X_STRENGTH}
O_strength=${2:-$DEFAULT_O_STRENGTH}
echo "Player X strength: $X_strength"
echo "Player O strength: $O_strength"
while true; do
	if [[ $player == "X" && $X_strength -gt 0 ]]; then
		ai_turn "$X_strength"
	elif [[ $player == "O" && $O_strength -gt 0 ]]; then
		ai_turn "$O_strength"
	else
		player_turn
	fi

	if check_win "$player"; then
		echo "Player $player wins!"
		break
	fi
	if check_draw; then
		echo "It's a draw!"
		break
	fi
	player=$([[ $player == "X" ]] && echo "O" || echo "X")
done
print_board
