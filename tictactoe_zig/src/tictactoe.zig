const std = @import("std");
const ArrayList = std.ArrayList;

/// Error for when the user input is too big for the buffer.
const TooMuchInputError = error{TooMuchInput};
/// Error for when a user tries to make a move on an occupied spot.
const SpotOccupiedError = error{SpotOccupied};
/// Error for when a user tries to make a move out of bonds of the board.
const OutOfBoundsError = error{OutOfBounds};

/// List of winconditions.
/// In order to win a player has to occupy all (3) spots listed
/// in one of these conditions.
const win_conditions = [_][3]usize{
    // Rows
    [_]usize{ 0, 1, 2 },
    [_]usize{ 3, 4, 5 },
    [_]usize{ 6, 7, 8 },
    // Cols
    [_]usize{ 0, 3, 6 },
    [_]usize{ 1, 4, 7 },
    [_]usize{ 2, 5, 8 },
    // Diags
    [_]usize{ 0, 4, 8 },
    [_]usize{ 2, 4, 6 },
};

/// Struct for tracking the result of checking a win condition for a player.
/// open contains the spots that neither player occupies.
/// done lists the number of spots already occupied by th player
const ConditionResult = struct { open: []const usize, done: u8 };

const EndState = enum {
    Loss,
    Draw,
    Win,

    pub fn reverse(self: EndState) EndState {
        return switch (self) {
            EndState.Loss => EndState.Win,
            EndState.Draw => EndState.Draw,
            EndState.Win => EndState.Loss,
        };
    }
};

test "reverse end state" {
    try std.testing.expectEqual(EndState.Loss.reverse(), EndState.Win);
    try std.testing.expectEqual(EndState.Draw.reverse(), EndState.Draw);
    try std.testing.expectEqual(EndState.Win.reverse(), EndState.Loss);
}

const Move = struct { spot: usize, end_state: EndState };

pub const AIStrength = enum { Easy, Medium, Hard, Impossible };

/// Enum for the open spots of the board.
const OpenSpot = enum { Zero, One, Two, Three, Four, Five, Six, Seven, Eight };

/// Enum for the two players of the game.
pub const Player = enum {
    X,
    O,

    /// Swap function to change player at the end of a turn.
    pub fn swap(self: Player) Player {
        return switch (self) {
            Player.X => Player.O,
            Player.O => Player.X,
        };
    }

    pub fn format(
        self: Player,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}", .{@tagName(self)});
    }
};

test "Swap player" {
    try std.testing.expectEqual(Player.X.swap(), Player.O);
    try std.testing.expectEqual(Player.O.swap(), Player.X);
}

pub const GameBoard = [9]?Player;

/// Pretty printer for the game board.
pub fn showBoard(board: *GameBoard, writer: anytype) !void {
    const line_separator = "---------------";
    try writer.print("{s}\n", .{line_separator});
    const side_length = 3;
    for (0..3) |row| {
        for (0..3) |col| {
            const index = (row * side_length) + col;
            if (board[index]) |player| {
                try writer.print("| {s} |", .{player});
            } else {
                try writer.print("| {d} |", .{index});
            }
        }
        try writer.print("\n", .{});
        try writer.print("{s}\n", .{line_separator});
    }
}

/// Flush the provided reader.
/// Needed when the user enters more input than what fits in the buffer.
/// In that case the reader has to be flushed to make sure it does not
/// read the overflowing values on the next loop iteration.
fn flush(reader: anytype) !void {
    var scratch: [64]u8 = undefined;
    while (true) {
        if (reader.read(&scratch)) |val| {
            if (val != 0) break;
        } else |err| return err;
    }
}

test "flush empties" {
    var fis = std.io.fixedBufferStream("foobar");
    const reader = fis.reader();

    try flush(reader);

    try std.testing.expectError(error.EndOfStream, reader.readByte());
}

/// Gets any u8 from user input.
fn getUserInputNumber(reader: anytype, writer: anytype) !usize {
    var buf: [5]u8 = undefined;

    const amt = try reader.read(buf[0..]);
    if (amt == buf.len) {
        try writer.print("ERROR: Input too long.\n", .{});
        return TooMuchInputError.TooMuchInput;
    }
    const line = std.mem.trimRight(u8, buf[0..amt], "\r\n");
    const parsed = std.fmt.parseUnsigned(usize, line, 10) catch |err| switch (err) {
        error.Overflow => {
            try writer.print("ERROR: Input number too large!\n", .{});
            return err;
        },
        error.InvalidCharacter => {
            try writer.print("ERROR: Input must be a valid positive integer!\n", .{});
            return err;
        },
    };
    return parsed;
}

/// Ask the user where on the `board` they want to make their move as `player`.
/// Invalid characters, out of bounds values and trying to make a move
/// on an occupied spot return errors (on top of the usual ones that can happen
/// with input and output).
fn fixSpot(player: Player, board: *GameBoard, reader: anytype, writer: anytype) !void {
    try writer.print("Where to make your next move? [0-8]\n", .{});
    const parsed = try getUserInputNumber(reader, writer);

    if (parsed < 0 or board.len <= parsed) {
        try writer.print("ERROR: Spot has to be in range [0-8] but was {d}!\n", .{parsed});
        return OutOfBoundsError.OutOfBounds;
    }

    if (board[parsed]) |_| {
        try writer.print("ERROR: Spot {d} is already occupied!\n", .{parsed});
        return SpotOccupiedError.SpotOccupied;
    } else {
        board[parsed] = player;
    }
}

test "fixSpot invalid character" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("a");
    const reader = fis.reader();
    var board = GameBoard{
        null,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
    };

    try std.testing.expectError(error.InvalidCharacter, fixSpot(Player.X, &board, reader, stderr));
}

test "fixSpot outofbounds" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("9");
    const reader = fis.reader();
    var board = GameBoard{
        null,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
    };
    try std.testing.expectError(error.OutOfBounds, fixSpot(Player.X, &board, reader, stderr));
}

test "fixSpot spot occupied" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("1");
    const reader = fis.reader();
    var board = GameBoard{
        null,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
    };
    try std.testing.expectError(error.SpotOccupied, fixSpot(Player.X, &board, reader, stderr));
}

test "fixSpot input too long" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("1111111");
    const reader = fis.reader();
    var board = GameBoard{
        null,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
    };
    try std.testing.expectError(error.TooMuchInput, fixSpot(Player.X, &board, reader, stderr));
}

test "fixSpot valid" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("2");
    const reader = fis.reader();
    var board = GameBoard{
        null,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
    };
    try fixSpot(Player.X, &board, reader, stderr);
    try std.testing.expectEqual(Player.X, board[2].?);
}

/// Perform a (human) player turn.
/// Provides context of the current game state and then asks the user for
/// input where they want to make their next move until they enter
/// a valid value.
pub fn playerTurn(player: Player, board: *GameBoard, reader: anytype, writer: anytype) !void {
    try writer.print("Player {s} turn.\n", .{player});
    try showBoard(board, writer);

    while (true) {
        fixSpot(player, board, reader, writer) catch |err| switch (err) {
            error.OutOfBounds, error.SpotOccupied, error.InvalidCharacter => {
                continue;
            },
            error.TooMuchInput => {
                try flush(reader);
                continue;
            },
            else => return err,
        };
        break;
    }
}

test "player turn" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("2");
    const reader = fis.reader();
    var board = GameBoard{
        null,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
    };
    try playerTurn(Player.X, &board, reader, stderr);
    try std.testing.expectEqual(Player.X, board[2].?);
}

/// Check if `player` has fulfilled the `condition` on `board`.
/// Return how many spots are done and which spots are still open.
fn checkWinCondition(player: Player, board: *GameBoard, condition: [3]usize, allocator: std.mem.Allocator) !ConditionResult {
    // Initialize the ArrayList with its max size of 3 as each condition has size 3.
    var done: u8 = 0;
    var open = try ArrayList(usize).initCapacity(allocator, 3);
    for (condition) |spot| {
        if (board[spot]) |occupying_player| {
            if (player == occupying_player) {
                done += 1;
            }
        } else {
            open.appendAssumeCapacity(spot);
        }
    }
    return .{ .open = try open.toOwnedSlice(), .done = done };
}

test "checkWinCondition filled" {
    var board = GameBoard{
        Player.X,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
        Player.X,
    };

    const expected: ConditionResult = .{ .open = &[_]usize{}, .done = 3 };
    const actual = try checkWinCondition(Player.X, &board, [_]usize{ 0, 1, 8 }, std.testing.allocator);
    defer std.testing.allocator.free(actual.open);

    try std.testing.expectEqualDeep(expected, actual);
}

test "checkWinCondition open" {
    var board = GameBoard{
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        Player.X,
    };

    var expected: ConditionResult = .{ .open = &[_]usize{ 0, 1, 2 }, .done = 0 };
    const actual = try checkWinCondition(Player.X, &board, [_]usize{ 0, 1, 2 }, std.testing.allocator);
    defer std.testing.allocator.free(actual.open);

    try std.testing.expectEqualDeep(expected, actual);
}

test "checkWinCondition partial" {
    var board = GameBoard{
        Player.O,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        Player.X,
    };

    var expected: ConditionResult = .{ .open = &[_]usize{4}, .done = 1 };
    const actual = try checkWinCondition(Player.O, &board, [_]usize{ 0, 4, 8 }, std.testing.allocator);
    defer std.testing.allocator.free(actual.open);

    try std.testing.expectEqualDeep(expected, actual);
}

/// Test if `player` has won the game on `board`.
/// For that check if any of the win conditions have been completely fulfilled.
fn isPlayerWin(player: Player, board: *GameBoard, allocator: std.mem.Allocator) !bool {
    for (win_conditions) |condition| {
        const result = try checkWinCondition(player, board, condition, allocator);
        defer allocator.free(result.open);
        if (result.done == 3) {
            return true;
        }
    }
    return false;
}

test "player win row" {
    var board = GameBoard{
        Player.X,
        Player.X,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
    };

    try std.testing.expect(try isPlayerWin(Player.X, &board, std.testing.allocator));
    try std.testing.expect(!try isPlayerWin(Player.O, &board, std.testing.allocator));

    board = GameBoard{
        null,
        null,
        null,
        Player.O,
        Player.O,
        Player.O,
        null,
        null,
        null,
    };

    try std.testing.expect(try isPlayerWin(Player.O, &board, std.testing.allocator));
    try std.testing.expect(!try isPlayerWin(Player.X, &board, std.testing.allocator));

    board = GameBoard{
        null,
        null,
        null,
        null,
        null,
        null,
        Player.X,
        Player.X,
        Player.X,
    };

    try std.testing.expect(try isPlayerWin(Player.X, &board, std.testing.allocator));
    try std.testing.expect(!try isPlayerWin(Player.O, &board, std.testing.allocator));
}

test "player win col" {
    var board = GameBoard{
        Player.X,
        null,
        null,
        Player.X,
        null,
        null,
        Player.X,
        null,
        null,
    };

    try std.testing.expect(try isPlayerWin(Player.X, &board, std.testing.allocator));
    try std.testing.expect(!try isPlayerWin(Player.O, &board, std.testing.allocator));

    board = GameBoard{
        null,
        Player.O,
        null,
        null,
        Player.O,
        null,
        null,
        Player.O,
        null,
    };

    try std.testing.expect(try isPlayerWin(Player.O, &board, std.testing.allocator));
    try std.testing.expect(!try isPlayerWin(Player.X, &board, std.testing.allocator));

    board = GameBoard{
        null,
        null,
        Player.X,
        null,
        null,
        Player.X,
        null,
        null,
        Player.X,
    };

    try std.testing.expect(try isPlayerWin(Player.X, &board, std.testing.allocator));
    try std.testing.expect(!try isPlayerWin(Player.O, &board, std.testing.allocator));
}

test "player win diag" {
    var board = GameBoard{
        Player.X,
        null,
        null,
        null,
        Player.X,
        null,
        null,
        null,
        Player.X,
    };

    try std.testing.expect(try isPlayerWin(Player.X, &board, std.testing.allocator));
    try std.testing.expect(!try isPlayerWin(Player.O, &board, std.testing.allocator));

    board = GameBoard{
        null,
        null,
        Player.O,
        null,
        Player.O,
        null,
        Player.O,
        null,
        null,
    };

    try std.testing.expect(try isPlayerWin(Player.O, &board, std.testing.allocator));
    try std.testing.expect(!try isPlayerWin(Player.X, &board, std.testing.allocator));
}

/// Check if the full board is occupied by players.
/// If this check is done after a possible win check, then
/// it indicates a draw.
fn isBoardFilled(board: *GameBoard) bool {
    for (board) |value| {
        if (value) |_| {
            continue;
        } else {
            return false;
        }
    }
    return true;
}

test "board not filled" {
    var board = GameBoard{
        null,
        null,
        Player.O,
        null,
        Player.O,
        null,
        Player.O,
        null,
        null,
    };

    try std.testing.expect(!isBoardFilled(&board));
}

test "board filled" {
    var board = GameBoard{
        Player.O,
        Player.X,
        Player.O,
        Player.X,
        Player.X,
        Player.O,
        Player.O,
        Player.X,
        Player.O,
    };

    try std.testing.expect(isBoardFilled(&board));
}

/// Check if the game is over.
/// This can either happen because the player who made the last move
/// has won the game. Or if that is not the case, because the game is drawn.
pub fn isGameOver(player: Player, board: *GameBoard, writer: anytype, allocator: std.mem.Allocator) !bool {
    if (try isPlayerWin(player, board, allocator)) {
        try writer.print("Player {s} wins the game!\n", .{player});
        return true;
    }

    if (isBoardFilled(board)) {
        try writer.print("Match Drawn!\n", .{});
        return true;
    }
    return false;
}

test "game still going" {
    const stderr = std.io.getStdErr().writer();
    var board = GameBoard{
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

    try std.testing.expect(!try isGameOver(Player.X, &board, stderr, std.testing.allocator));
}

test "game drawn" {
    const stderr = std.io.getStdErr().writer();
    var board = GameBoard{
        Player.X,
        Player.O,
        Player.X,
        Player.X,
        Player.O,
        Player.O,
        Player.O,
        Player.X,
        Player.X,
    };

    try std.testing.expect(try isGameOver(Player.X, &board, stderr, std.testing.allocator));
    try std.testing.expect(try isGameOver(Player.O, &board, stderr, std.testing.allocator));
}

test "game won" {
    const stderr = std.io.getStdErr().writer();
    var board = GameBoard{
        Player.X,
        Player.X,
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
    };

    try std.testing.expect(try isGameOver(Player.X, &board, stderr, std.testing.allocator));
    try std.testing.expect(!try isGameOver(Player.O, &board, stderr, std.testing.allocator));
}

/// Ask the player for a yes/no answer via stdin.
fn getPlayerYesNo(question: []const u8, reader: anytype, writer: anytype) !bool {
    try writer.print("{s} [y/n]\n", .{question});

    while (true) {
        var buf: [3]u8 = undefined;
        const amt = try reader.read(buf[0..]);

        if (amt == buf.len) {
            try writer.print("ERROR: Input too long.\n", .{});
            try flush(reader);
            continue;
        }

        switch (buf[0]) {
            'Y', 'y' => return true,
            'N', 'n' => return false,
            else => {
                try writer.print("Please answer with 'y' or 'n'.\n", .{});
                continue;
            },
        }
    }
}

test "player yes" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("y");
    var reader = fis.reader();
    try std.testing.expect(try getPlayerYesNo("Whatever?", reader, stderr));

    fis = std.io.fixedBufferStream("Y");
    reader = fis.reader();
    try std.testing.expect(try getPlayerYesNo("Whatever?", reader, stderr));
}

test "player no" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("n");
    var reader = fis.reader();
    try std.testing.expect(!try getPlayerYesNo("Whatever?", reader, stderr));

    fis = std.io.fixedBufferStream("N");
    reader = fis.reader();
    try std.testing.expect(!try getPlayerYesNo("Whatever?", reader, stderr));
}

/// Ask the player if they want to play alone vs AI.
pub fn getSinglePlayer(reader: anytype, writer: anytype) !bool {
    return getPlayerYesNo("Play alone vs AI?", reader, writer);
}

/// Ask the player if the AI should make the first move.
/// In that case the AI plays as 'X' as 'X' makes the first move.
pub fn getAIStart(reader: anytype, writer: anytype) !Player {
    if (try getPlayerYesNo("Should the AI make the first move?", reader, writer)) {
        return Player.X;
    }
    return Player.O;
}

test "ai start yes" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("y");
    const reader = fis.reader();
    try std.testing.expectEqual(try getAIStart(reader, stderr), Player.X);
}

test "ai start no" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("N");
    const reader = fis.reader();
    try std.testing.expectEqual(try getAIStart(reader, stderr), Player.O);
}

/// Ask the player which strength the AI should play with.
pub fn getAIStrength(reader: anytype, writer: anytype) !AIStrength {
    try writer.print("AI strength settings:\n", .{});
    try writer.print("1: Easy\n", .{});
    try writer.print("2: Medium\n", .{});
    try writer.print("3: Hard\n", .{});
    try writer.print("4: Impossible\n", .{});
    while (true) {
        try writer.print("How strong should the AI be? [1 - 4]\n", .{});
        const user_input = getUserInputNumber(reader, writer) catch |err| switch (err) {
            error.TooMuchInput => {
                try flush(reader);
                continue;
            },
            error.InvalidCharacter => {
                continue;
            },
            else => return err,
        };
        switch (user_input) {
            1...4 => return @enumFromInt(user_input - 1),
            else => {
                try writer.print("ERROR: AIStrength has to be in range [1-4] but was {d}!\n", .{user_input});
            },
        }
    }
}

test "ai strength valid" {
    const stderr = std.io.getStdErr().writer();
    var fis = std.io.fixedBufferStream("3");
    const reader = fis.reader();
    try std.testing.expectEqual(try getAIStrength(reader, stderr), AIStrength.Hard);
}

fn getNEmptyCells(board: *GameBoard) u8 {
    var n_empty: u8 = 0;
    for (board) |value| {
        if (value == null) {
            n_empty += 1;
        }
    }
    return n_empty;
}

test "get N empty cells none" {
    var board = GameBoard{
        Player.X,
        Player.O,
        Player.X,
        Player.X,
        Player.O,
        Player.O,
        Player.O,
        Player.X,
        Player.X,
    };
    const expected: u8 = 0;
    const actual: u8 = getNEmptyCells(&board);

    try std.testing.expectEqual(expected, actual);
}

test "get N empty cells some" {
    var board = GameBoard{
        Player.X,
        Player.O,
        null,
        Player.O,
        null,
        Player.X,
        null,
        Player.X,
        Player.X,
    };
    const expected: u8 = 3;
    const actual: u8 = getNEmptyCells(&board);

    try std.testing.expectEqual(expected, actual);
}

/// Get all the empty cells on the board.
/// Useful for getting a random valid spot or exhaustively checking them
/// for the minmax algorithm.
fn getEmptyCells(board: *GameBoard, allocator: std.mem.Allocator) ![]const (usize) {
    // Max size of empty cells is the size of the board -> 9
    var empty_cells = try ArrayList(usize).initCapacity(allocator, 9);
    for (board, 0..) |value, spot| {
        if (value == null) {
            empty_cells.appendAssumeCapacity(spot);
        }
    }
    return empty_cells.toOwnedSlice();
}

test "get empty cells none" {
    var board = GameBoard{
        Player.X,
        Player.O,
        Player.X,
        Player.X,
        Player.O,
        Player.O,
        Player.O,
        Player.X,
        Player.X,
    };

    var expected: []const usize = &[_]usize{};

    const actual = try getEmptyCells(&board, std.testing.allocator);
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualDeep(expected, actual);
}

test "get empty cells some" {
    var board = GameBoard{
        Player.X,
        Player.O,
        null,
        Player.O,
        null,
        Player.X,
        null,
        Player.X,
        Player.X,
    };

    var expected: []const usize = &[_]usize{ 2, 4, 6 };

    const actual = try getEmptyCells(&board, std.testing.allocator);
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualDeep(expected, actual);
}

/// Perform a move on a random open spot.
/// The EndState is not relevant for random moves.
/// It is only relevant for minmax moves.
fn randomMove(board: *GameBoard, allocator: std.mem.Allocator) !Move {
    const empty_cells = try getEmptyCells(board, allocator);
    defer allocator.free(empty_cells);
    const random_index = std.crypto.random.intRangeLessThan(usize, 0, empty_cells.len);
    const random_element = empty_cells[random_index];
    return .{ .spot = random_element, .end_state = EndState.Loss };
}

test "random move is valid" {
    var board = GameBoard{
        Player.X,
        Player.O,
        null,
        Player.O,
        null,
        Player.X,
        null,
        Player.X,
        Player.X,
    };

    for (0..100) |_| {
        const actual = try randomMove(&board, std.testing.allocator);
        const in_expectation = switch (actual.spot) {
            2, 4, 6 => true,
            else => false,
        };
        try std.testing.expect(in_expectation);
    }
}

/// Try to find a winning move for `player` on `board`.
/// Returns null if none can be found.
/// end_state value has no functional purpose as it is only used by the
/// minmax algorithm.
fn getWinningMove(player: Player, board: *GameBoard, allocator: std.mem.Allocator) !?Move {
    for (win_conditions) |condition| {
        const result = try checkWinCondition(player, board, condition, allocator);
        defer allocator.free(result.open);
        if (result.done == 2 and result.open.len == 1) {
            return .{ .spot = result.open[0], .end_state = EndState.Win };
        }
    }
    return null;
}

test "winning move finds row" {
    var board = GameBoard{
        Player.O,
        Player.X,
        Player.O,
        Player.X,
        Player.O,
        null,
        Player.X,
        Player.X,
        null,
    };
    const expected: Move = .{ .spot = 8, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, (try getWinningMove(Player.X, &board, std.testing.allocator)).?);
}

test "winning move finds col" {
    var board = GameBoard{
        Player.O,
        Player.X,
        null,
        null,
        null,
        null,
        Player.O,
        Player.X,
        Player.X,
    };
    const expected: Move = .{ .spot = 3, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, (try getWinningMove(Player.O, &board, std.testing.allocator)).?);
}

test "winning move finds diagonal" {
    var board = GameBoard{
        Player.O,
        null,
        null,
        null,
        Player.O,
        null,
        null,
        null,
        null,
    };
    const expected: Move = .{ .spot = 8, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, (try getWinningMove(Player.O, &board, std.testing.allocator)).?);
}

test "winning move finds antidiagonal" {
    var board = GameBoard{
        null,
        null,
        null,
        null,
        Player.X,
        null,
        Player.X,
        null,
        null,
    };
    const expected: Move = .{ .spot = 2, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, (try getWinningMove(Player.X, &board, std.testing.allocator)).?);
}

test "winning move finds null" {
    var board = GameBoard{
        Player.X,
        Player.O,
        Player.X,
        null,
        Player.O,
        Player.O,
        Player.O,
        Player.X,
        Player.X,
    };
    const is_null = if (try getWinningMove(Player.X, &board, std.testing.allocator)) |_| false else true;
    try std.testing.expect(is_null);
}

/// Get a winning move. If none can be found return a random one.
fn winMove(player: Player, board: *GameBoard, allocator: std.mem.Allocator) !Move {
    const winning_move = try getWinningMove(player, board, allocator);
    if (winning_move) |move| {
        return move;
    } else {
        return randomMove(board, allocator);
    }
}

test "winMove finds row" {
    var board = GameBoard{
        Player.O,
        Player.X,
        Player.O,
        Player.X,
        Player.O,
        null,
        Player.X,
        Player.X,
        null,
    };
    const expected: Move = .{ .spot = 8, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try winMove(Player.X, &board, std.testing.allocator));
}

test "winMove finds col" {
    var board = GameBoard{
        Player.O,
        Player.X,
        null,
        null,
        null,
        null,
        Player.O,
        Player.X,
        Player.X,
    };
    const expected: Move = .{ .spot = 3, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try winMove(Player.O, &board, std.testing.allocator));
}

test "winMove finds diagonal" {
    var board = GameBoard{
        Player.O,
        null,
        null,
        null,
        Player.O,
        null,
        null,
        null,
        null,
    };
    const expected: Move = .{ .spot = 8, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try winMove(Player.O, &board, std.testing.allocator));
}

test "winMove finds antidiagonal" {
    var board = GameBoard{
        null,
        null,
        null,
        null,
        Player.X,
        null,
        Player.X,
        null,
        null,
    };
    const expected: Move = .{ .spot = 2, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try winMove(Player.X, &board, std.testing.allocator));
}

test "winMove finds null" {
    var board = GameBoard{
        Player.X,
        Player.O,
        Player.X,
        null,
        Player.O,
        Player.O,
        Player.O,
        Player.X,
        Player.X,
    };
    const expected: Move = .{ .spot = 3, .end_state = EndState.Loss };
    try std.testing.expectEqualDeep(expected, try winMove(Player.X, &board, std.testing.allocator));
}

/// Get a winning, blocking or random move, in that order of availability.
fn winBlockMove(player: Player, board: *GameBoard, allocator: std.mem.Allocator) !Move {
    const winning_move = try getWinningMove(player, board, allocator);
    if (winning_move) |move| {
        return move;
    }
    const blocking_move = try getWinningMove(player.swap(), board, allocator);
    if (blocking_move) |move| {
        return move;
    }
    return randomMove(board, allocator);
}

test "winBlockMove wins" {
    var board = GameBoard{
        Player.X,
        Player.X,
        null,
        Player.O,
        Player.O,
        null,
        null,
        null,
        null,
    };
    var expected: Move = .{ .spot = 2, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try winBlockMove(Player.X, &board, std.testing.allocator));
    expected = .{ .spot = 5, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try winBlockMove(Player.O, &board, std.testing.allocator));
}

test "winBlockMove blocks" {
    var board = GameBoard{
        Player.X,
        null,
        null,
        Player.O,
        Player.O,
        null,
        null,
        null,
        null,
    };
    const expected: Move = .{ .spot = 5, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try winBlockMove(Player.X, &board, std.testing.allocator));
}

test "winBlockMove finds null" {
    var board = GameBoard{
        Player.O,
        Player.X,
        Player.X,
        Player.X,
        Player.O,
        Player.O,
        null,
        Player.X,
        Player.O,
    };
    const expected: Move = .{ .spot = 6, .end_state = EndState.Loss };
    try std.testing.expectEqualDeep(expected, try winBlockMove(Player.X, &board, std.testing.allocator));
}

/// Get an optimal move.
fn minMax(player: Player, board: *GameBoard, allocator: std.mem.Allocator) !Move {
    // Game already won.
    if (try isPlayerWin(player, board, allocator)) {
        return .{ .spot = 0, .end_state = EndState.Win };
    }
    //Game already lost.
    if (try isPlayerWin(player.swap(), board, allocator)) {
        return .{ .spot = 0, .end_state = EndState.Loss };
    }

    const n_empty_cells = getNEmptyCells(board);

    // Game already drawn.
    if (n_empty_cells == 0) {
        return .{ .spot = 0, .end_state = EndState.Draw };
    }

    // New game
    if (n_empty_cells == board.len) {
        return randomMove(board, allocator);
    }

    // Recursive step
    var best_move: Move = .{ .spot = 0, .end_state = EndState.Loss };
    for (board, 0..) |value, spot| {
        if (value != null) continue;
        board[spot] = player;
        const current_move = try minMax(player.swap(), board, allocator);
        if (@intFromEnum(current_move.end_state.reverse()) >= @intFromEnum(best_move.end_state)) {
            best_move = .{ .spot = spot, .end_state = current_move.end_state.reverse() };
        }
        board[spot] = null;
    }
    return best_move;
}

test "minMax takes open spot" {
    var board = GameBoard{
        Player.X,
        Player.X,
        null,
        Player.O,
        Player.X,
        Player.O,
        Player.X,
        Player.O,
        Player.O,
    };
    const expected: Move = .{ .spot = 2, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try minMax(Player.X, &board, std.testing.allocator));
}

test "minMax blocks win" {
    var board = GameBoard{
        Player.O,
        Player.O,
        Player.X,
        Player.X,
        null,
        Player.O,
        null,
        Player.O,
        Player.X,
    };
    const expected: Move = .{ .spot = 4, .end_state = EndState.Draw };
    try std.testing.expectEqualDeep(expected, try minMax(Player.X, &board, std.testing.allocator));
}

test "minMax takes win" {
    var board = GameBoard{
        Player.O,
        Player.O,
        Player.X,
        Player.X,
        null,
        null,
        null,
        Player.O,
        Player.X,
    };
    const expected: Move = .{ .spot = 4, .end_state = EndState.Win };
    try std.testing.expectEqualDeep(expected, try minMax(Player.O, &board, std.testing.allocator));
}

test "minMax finds best" {
    var board = GameBoard{
        Player.X,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
    };
    const expected: Move = .{ .spot = 4, .end_state = EndState.Draw };
    try std.testing.expectEqualDeep(expected, try minMax(Player.O, &board, std.testing.allocator));
}

/// Have the AI perform a turn.
/// The used algorithm is determined by `ai_strength`.
pub fn aiTurn(player: Player, board: *GameBoard, ai_strength: AIStrength, writer: anytype, allocator: std.mem.Allocator) !void {
    try writer.print("Player {s} turn.\n", .{player});
    try showBoard(board, writer);

    const ai_move = try switch (ai_strength) {
        AIStrength.Easy => randomMove(board, allocator),
        AIStrength.Medium => winMove(player, board, allocator),
        AIStrength.Hard => winBlockMove(player, board, allocator),
        AIStrength.Impossible => minMax(player, board, allocator),
    };
    board[ai_move.spot] = player;
    std.time.sleep(1 * std.time.ns_per_s);
}
