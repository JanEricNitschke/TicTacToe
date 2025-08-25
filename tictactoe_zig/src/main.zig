//! Module for playing command line tictactoe.

const std = @import("std");
const tictactoe = @import("tictactoe.zig");
const io = std.io;

test {
    std.testing.refAllDecls(@This());
}

pub fn main() !void {
    // We only need a maximum of 72 bytes of dynamic memory.
    // Our only dynamic memory are ArrayLists for win condition check
    // and getting empty cells.
    // A win condition is an array list of usize (max 8 atm) with max 3 slots.
    // The empty cell are max 9 (board spots) of usize (8 bytes atm).
    var buffer: [@max(9 * 8, 3 * 8)]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const fixed_allocator = fba.allocator();

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const stdin = &stdin_reader.interface;

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var player = tictactoe.Player.X;

    var ai_marker = tictactoe.Player.X;
    var ai_strength = tictactoe.AIStrength.Easy;
    const single_player = try tictactoe.getSinglePlayer(stdin, stdout);
    if (single_player) {
        ai_marker = try tictactoe.getAIStart(stdin, stdout);
        ai_strength = try tictactoe.getAIStrength(stdin, stdout);
    }

    var board = tictactoe.GameBoard{
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
    };

    while (true) {
        if (single_player and player == ai_marker) {
            try tictactoe.aiTurn(
                player,
                &board,
                ai_strength,
                stdout,
                fixed_allocator,
            );
        } else {
            try tictactoe.playerTurn(
                player,
                &board,
                stdin,
                stdout,
            );
        }

        if (try tictactoe.isGameOver(
            player,
            &board,
            stdout,
            fixed_allocator,
        )) {
            break;
        }

        player = player.swap();
    }
    try tictactoe.showBoard(&board, stdout);
    try stdout.flush();
}
