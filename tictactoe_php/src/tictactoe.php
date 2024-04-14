<?php

declare(strict_types=1);

const WINNING_COMBINATIONS =  [
    [0, 1, 2], [3, 4, 5], [6, 7, 8],
    [0, 3, 6], [1, 4, 7], [2, 5, 8],
    [0, 4, 8], [2, 4, 6]
];

class Move
{
    public int $index;
    public int $score;

    public function __construct(int $index, int $score)
    {
        $this->index = $index;
        $this->score = $score;
    }
}


function show_board(array $board): void
{
    echo " $board[0] | $board[1] | $board[2] \n";
    echo "---+---+---\n";
    echo " $board[3] | $board[4] | $board[5] \n";
    echo "---+---+---\n";
    echo " $board[6] | $board[7] | $board[8] \n";
}

function swap_player(string &$player): string
{
    return $player === "X" ? "O" : "X";
}

function flush_stdin(): void
{
    while (fgets(STDIN)) {
    }
}

function player_turn(array &$board, string $player): void
{
    echo "Player $player, enter your move (0-8): \n";
    show_board($board);
    $position = -1;
    while (true) {
        $position = preg_replace('/\s+/', '', fgets(STDIN));
        if (ctype_digit($position)) {
            $position = intval($position);
        } else {
            echo "Invalid input. Please enter a number between 0 and 8!\n";
            continue;
        }
        if ($position < 0 || $position > 8) {
            echo "Input out of range. Please enter a number between 0 and 8!\n";
            continue;
        }
        if ($board[$position] === "X" || $board[$position] === "O") {
            echo "Cell already taken. Please choose another one!\n";
            continue;
        }
        $board[$position] = $player;
        break;
    }
}

function is_winner(array $board, string $player): bool
{
    foreach (WINNING_COMBINATIONS as $combination) {
        if (
            $board[$combination[0]] === $player &&
            $board[$combination[1]] === $player &&
            $board[$combination[2]] === $player
        ) {
            return true;
        }
    }
    return false;
}

function is_board_full(array $board): bool
{
    foreach ($board as &$cell) {
        if ($cell !== "X" && $cell !== "O") {
            return false;
        }
    }
    return true;
}

function get_empty_cells(array $board): array
{
    $empty_cells = [];
    foreach ($board as $index => $cell) {
        if ($cell !== "X" && $cell !== "O") {
            $empty_cells[] = $index;
        }
    }
    return $empty_cells;
}

function random_value(array $array): int
{
    return $array[array_rand($array)];
}

function random_move(array $board): int
{
    $empty_cells = get_empty_cells($board);
    return random_value($empty_cells);
}

function get_winning_move(array $board, string $player): ?int
{
    foreach (WINNING_COMBINATIONS as $combination) {
        $open_cells = [];
        $done_cells = 0;
        foreach ($combination as $cell) {
            if ($board[$cell] === $player) {
                $done_cells++;
            } elseif ($board[$cell] !== swap_player($player)) {
                $open_cells[] = $cell;
            }
        }
        if ($done_cells === 2 && count($open_cells) === 1) {
            return $open_cells[0];
        }
    }
    return null;
}

function winning_move(array $board, string $player): int
{
    $move = get_winning_move($board, $player);
    return is_null($move) ? random_move($board) : $move;
}

function winning_or_blocking_move(array $board, string $player): int
{
    $move = get_winning_move($board, $player);
    if (!is_null($move)) {
        return $move;
    }
    $opponent = $player === "X" ? "O" : "X";
    $move = get_winning_move($board, $opponent);
    return is_null($move) ? random_move($board) : $move;
}

function get_optimal_move(array $board, string $player): Move
{
    $best_move = new Move(0, -1000);
    if (is_winner($board, $player)) {
        $best_move->score = 1;
        return $best_move;
    }
    if (is_winner($board, swap_player($player))) {
        $best_move->score = -1;
        return $best_move;
    }
    $empty_cells = get_empty_cells($board);

    if (!$empty_cells) {
        $best_move->score = 0;
        return $best_move;
    }

    if (count($empty_cells) === 9) {
        $best_move->index = random_value($empty_cells);
        return $best_move;
    }

    foreach ($empty_cells as $index) {
        $board[$index] = $player;
        $score = -get_optimal_move($board, swap_player($player))->score;
        $board[$index] = strval($index);
        if ($score > $best_move->score) {
            $best_move->score = $score;
            $best_move->index = $index;
        }
    }
    return $best_move;
}

function minmax(array $board, string $player): int
{
    return get_optimal_move($board, $player)->index;
}



function ai_turn(array &$board, string $player, int $strength): void
{
    echo "AI turn as player $player with strength $strength.\n";
    show_board($board);
    $move = match ($strength) {
        1 => random_move($board),
        2 => winning_move($board, $player),
        3 => winning_or_blocking_move($board, $player),
        default => minmax($board, $player),
    };
    $board[$move] = $player;
    sleep(1);
}

function play_game(?int $X_strength, ?int $O_strength): void
{
    $board = ["0", "1", "2", "3", "4", "5", "6", "7", "8"];
    $player = "X";

    while (true) {
        if ($player === "X" && !is_null($X_strength)) {
            ai_turn($board, $player, $X_strength);
        } elseif ($player === "O" && !is_null($O_strength)) {
            ai_turn($board, $player, $O_strength);
        } else {
            player_turn($board, $player);
        }
        if (is_winner($board, $player)) {
            echo "Player $player wins!\n";
            break;
        }
        if (is_board_full($board)) {
            echo "It's a draw!\n";
            break;
        }
        $player = swap_player($player);
    }

    show_board($board);
}
