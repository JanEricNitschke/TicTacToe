<?php

declare(strict_types=1);

include 'src/tictactoe.php';

use PHPUnit\Framework\TestCase;

final class TictactoeTest extends TestCase
{
    public function testShowBoard(): void
    {
        $board = ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X'];
        $this->expectOutputString(" X | O | X \n---+---+---\n O | X | O \n---+---+---\n X | O | X \n");
        show_board($board);
    }

    public function testSwapPlayer(): void
    {
        $player = 'X';
        $this->assertSame('O', swap_player($player));
    }

    public function testMinmax(): void
    {
        $board = ['X', '1', '2', '3', '4', '5', '6', '7', '8'];
        $player = 'O';
        $move = get_optimal_move($board, $player);
        $this->assertSame(4, $move->index);
        $this->assertSame(0, $move->score);
    }
}
