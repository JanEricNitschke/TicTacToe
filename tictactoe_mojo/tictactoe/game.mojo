# =========================================================================
# Future optimization ideas
# =========================================================================
#
# --- Move ordering ---
# Alpha-beta prunes more when good moves are tried first.  Trying center
# and corner cells before edges would improve the effective branching
# factor.  A static priority table indexed by cell position costs almost
# nothing and can dramatically reduce nodes visited on larger boards.
#
# --- Iterative deepening ---
# Search depth 1, then 2, etc.  Each shallow pass populates the TT, so
# deeper passes benefit from cached results AND better move ordering
# (try the best move from the previous depth first).  The overhead of
# re-searching shallow depths is small because those trees are tiny.
#
# --- Symmetry reduction ---
# A 4x4 board has 8 symmetries (4 rotations x 2 reflections).  Before
# storing a TT entry, canonicalize the board to its lexicographically
# smallest symmetric equivalent.  This reduces unique states by up to
# 8x, shrinking the TT and increasing cache hits.  Trade-off: computing
# the canonical form adds cost per node.
#
# --- Heuristic evaluation for larger boards ---
# For boards where full solving is infeasible (6x6+), replace terminal-
# only scoring (-1/0/+1) with a heuristic at a depth cutoff:
#   - Count "open" lines (where the opponent has no pieces).
#   - Weight lines by pieces already placed (3-of-4 >> 1-of-4).
#   - Penalize positions where the opponent has a fork (two threats).
# This turns the engine from an exact solver into a depth-limited AI,
# similar to how chess engines work.
#
# --- Zobrist hashing for TT keys ---
# The current TT key packs both bitmasks + player bit into a KeyInt,
# limiting board size to what fits.  Zobrist hashing assigns a random
# number to each (cell, player) pair and XORs them together.  The hash
# is incrementally updated per move (one XOR), making it O(1) regardless
# of board size.  Collisions are possible but rare with 64-bit hashes.
# This would decouple board size from key type width entirely.
#
# --- Comptime win mask array ---
# The win mask helpers are called inside comptime-for loops, so the
# compiler likely folds them already.  Making this explicit by storing
# all masks in a comptime InlineArray would guarantee zero runtime
# overhead and make the generated code inspectable.
#
# --- SIMD-parallel win checking ---
# Instead of checking win patterns sequentially (even if unrolled), pack
# all masks into a SIMD register and check them in parallel.  For a 4x4
# board, a SIMD[DType.uint16, 16] could hold all 10 masks and do a
# single SIMD AND + compare.  Whether this beats the unrolled scalar
# version depends on register pressure and reduction cost.
# =========================================================================

from std.bit import bit_width
from std.collections import Dict
from std.random import seed, random_ui64
from std.time import sleep

# Board, TT key, TT entry, and player types — change these to widen
# the board.  All constraints derive from these types automatically.
# For 4x4 (16 cells): BoardInt needs 16 bits, KeyInt needs 2*16+1=33
# bits, EntryInt needs 10 bits, PlayerInt needs 2 bits.
comptime BoardInt = UInt16  # Bitboard bitmask (one bit per cell)
comptime KeyInt = UInt64  # TT key (two bitmasks + player bit)
comptime EntryInt = UInt16  # TT entry (packed score/spot/flag)
comptime PlayerInt = UInt8  # Player identifier (OPEN=0, X=1, O=2)

comptime OPEN: PlayerInt = 0
comptime X: PlayerInt = 1
comptime O: PlayerInt = 2
comptime BOARD_BITS: Int = Int(bit_width(BoardInt.MAX))
comptime KEY_BITS: Int = Int(bit_width(KeyInt.MAX))
comptime ENTRY_BITS: Int = Int(bit_width(EntryInt.MAX))


@fieldwise_init
struct Move(ImplicitlyCopyable):
    var spot: Int
    var score: Int


# TT entry bit layout — derived from the board and field widths.
comptime _TT_SCORE_BITS: Int = 3  # Scores {-1,0,+1} → +2 → {1,2,3}
comptime _TT_SPOT_BITS: Int = Int(bit_width(BOARD_BITS))  # Spots {-1..max_cell} → +1 → {0..BOARD_BITS}
comptime _TT_FLAG_BITS: Int = 2  # {EXACT, LOWER, UPPER}
comptime _TT_TOTAL_BITS: Int = _TT_SCORE_BITS + _TT_SPOT_BITS + _TT_FLAG_BITS


@always_inline("nodebug")
def _check_size[size: Int]():
    comptime assert size > 0, "board size must be > 0"
    # The board is stored as two BoardInt bitmasks (one bit per cell).
    comptime assert size * size <= BOARD_BITS, (
        "board too large for BoardInt bitboard"
    )
    # The TT key packs x_bits (cells bits), o_bits (cells bits), and
    # a player-discriminator bit into a single KeyInt.
    comptime assert 2 * size * size + 1 <= KEY_BITS, (
        "board too large for TT key in KeyInt"
    )
    # The TT entry spot field must be wide enough for every board position.
    comptime assert size * size < (1 << _TT_SPOT_BITS), (
        "board too large for TT spot field"
    )
    # The max packed TT entry (2^_TT_TOTAL_BITS - 1) must fit in EntryInt.
    # bit_width(MAX) >= N  ⟺  MAX >= 2^N - 1.
    comptime assert _TT_TOTAL_BITS <= ENTRY_BITS, (
        "EntryInt can't hold max TT entry value"
    )


def _swap_player(player: PlayerInt) -> PlayerInt:
    return X + O - player


# --- Bitboard helpers ---
# The board is stored as two BoardInt bitmasks: one for X, one for O.
# Bit i is set if position i is occupied by that player.
# Bit 0 (rightmost) is the top-left cell; bit 15 is the bottom-right.
#
# 4x4 board layout (bit positions):
#
#     col0 col1 col2 col3
#      0    1    2    3     row 0
#      4    5    6    7     row 1
#      8    9   10   11     row 2
#     12   13   14   15     row 3
#
# Example board:
#
#   X | O | - | -        X at positions 0, 5, 8
#   - | X | - | -        O at positions 1, 9, 13
#   X | O | - | -
#   - | O | - | -
#
#   x_bits = 0b0000_0001_0010_0001  (bits 0, 5, 8)
#                 15..12  11..8  7..4  3..0
#                  0000   0001   0010  0001
#
#   o_bits = 0b0010_0010_0000_0010  (bits 1, 9, 13)
#                 15..12  11..8  7..4  3..0
#                  0010   0010   0000  0010


def _row_mask[size: Int, row: Int]() -> BoardInt:
    """Bitmask with all cells in the given row set.

    Builds a run of `size` ones and shifts it to the correct row.

    Example (size=4):
      _row_mask[4, 0]()  ->  0b0000_0000_0000_1111  (bits 0-3)
      _row_mask[4, 2]()  ->  0b0000_1111_0000_0000  (bits 8-11)
    """
    return ((BoardInt(1) << BoardInt(size)) - 1) << BoardInt(row * size)


def _col_mask[size: Int, col: Int]() -> BoardInt:
    """Bitmask with all cells in the given column set.

    Sets one bit per row, spaced `size` apart.

    Example (size=4):
      _col_mask[4, 0]()  ->  bits {0, 4, 8, 12}  = 0b0001_0001_0001_0001
      _col_mask[4, 3]()  ->  bits {3, 7, 11, 15} = 0b1000_1000_1000_1000
    """
    var mask: BoardInt = 0
    for r in range(size):
        mask |= BoardInt(1) << BoardInt(r * size + col)
    return mask


def _diag_mask[size: Int]() -> BoardInt:
    """Bitmask for the main diagonal (top-left to bottom-right).

    Bit positions increase by size+1 each step.

    Example (size=4):
      bits {0, 5, 10, 15}  = 0b1000_0100_0010_0001
    """
    var mask: BoardInt = 0
    for i in range(size):
        mask |= BoardInt(1) << BoardInt(i * size + i)
    return mask


def _anti_diag_mask[size: Int]() -> BoardInt:
    """Bitmask for the anti-diagonal (top-right to bottom-left).

    Bit positions increase by size-1 each step.

    Example (size=4):
      bits {3, 6, 9, 12}  = 0b0001_0010_0100_1000
    """
    var mask: BoardInt = 0
    for i in range(size):
        mask |= BoardInt(1) << BoardInt(i * size + (size - 1 - i))
    return mask


def _full_mask[size: Int]() -> BoardInt:
    """Bitmask with all board positions set.

    Uses right-shift of all-ones instead of (1 << N) - 1 to avoid
    overflow when size*size equals the type width.

    Example (size=4, BoardInt=UInt16):
      ~UInt16(0) >> (16 - 16) = 0xFFFF >> 0 = 0b1111_1111_1111_1111
    """
    return ~BoardInt(0) >> BoardInt(BOARD_BITS - size * size)


def _player_at(x_bits: BoardInt, o_bits: BoardInt, pos: Int) -> PlayerInt:
    """Returns which player occupies the given position.

    Shifts the bitmask right by `pos` and checks the lowest bit.

    Example (x_bits has bit 5 set, o_bits has bit 3 set):
      _player_at(0b100000, 0b001000, 5)  ->  X
      _player_at(0b100000, 0b001000, 3)  ->  O
      _player_at(0b100000, 0b001000, 0)  ->  OPEN
    """
    if (x_bits >> BoardInt(pos)) & 1:
        return X
    if (o_bits >> BoardInt(pos)) & 1:
        return O
    return OPEN


def _is_full[size: Int](x_bits: BoardInt, o_bits: BoardInt) -> Bool:
    """Returns True if every cell is occupied.

    ORs both masks together and checks against the full-board mask.

    Example (size=4, all 16 cells filled):
      x_bits = 0b1010_1010_1010_1010
      o_bits = 0b0101_0101_0101_0101
      x_bits | o_bits == 0xFFFF  ->  True
    """
    return (x_bits | o_bits) == _full_mask[size]()


# --- Win checking ---
# Each win pattern is a compile-time constant bitmask.  The check for
# one pattern is just (bits & mask) == mask — a single AND + compare.
# All 2*size+2 patterns are unrolled via comptime for.


def _is_winner_param[size: Int, player: PlayerInt](
    x_bits: BoardInt, o_bits: BoardInt,
) -> Bool:
    """Check whether `player` has a complete line (compile-time specialised).

    Selects the player's bitmask, then tests it against every win pattern
    using (bits & mask) == mask.

    Example (size=4, X occupies the first column: bits 0, 4, 8, 12):
      x_bits = 0b0001_0001_0001_0001
      _col_mask[4, 0]() produces the same pattern
      (x_bits & mask) == mask  ->  True  ->  X wins
    """
    var bits: BoardInt
    comptime if player == X:
        bits = x_bits
    else:
        bits = o_bits

    comptime for row in range(size):
        comptime mask = _row_mask[size, row]()
        if (bits & mask) == mask:
            return True

    comptime for col in range(size):
        comptime mask = _col_mask[size, col]()
        if (bits & mask) == mask:
            return True

    comptime diag = _diag_mask[size]()
    if (bits & diag) == diag:
        return True

    comptime anti = _anti_diag_mask[size]()
    if (bits & anti) == anti:
        return True

    return False


def _is_winner[size: Int](
    x_bits: BoardInt, o_bits: BoardInt, player: PlayerInt,
) -> Bool:
    """Runtime dispatch to the compile-time specialised winner check."""
    if player == X:
        return _is_winner_param[size, X](x_bits, o_bits)
    return _is_winner_param[size, O](x_bits, o_bits)


# --- Transposition table ---
#
# "Transposition" = same board reached through different move orders.
# E.g., X(0,1)→O(1,1)→X(2,2)→O(2,1) and X(2,2)→O(2,1)→X(0,1)→O(1,1)
# produce the same board.  The table caches results so we don't
# re-evaluate positions we've already seen.
#
# =========================================================================
# Why we need bound-type flags (EXACT / LOWER / UPPER)
# =========================================================================
#
# Position P (O to move):
#
#   X | X | -
#   O | O | -         Empty: (0,2), (1,2)
#   X | O | X
#
# O's two moves at P:
#   (0,2) → no winner → board fills → Draw. Score = 0 for O.
#   (1,2) → row 1: (1,0),(1,1),(1,2) → O wins! Score = +1 for O.
#
# True minimax value of P: +1 (O plays (1,2) and wins).
#
# P can appear in multiple branches of the search tree (transposition).
# The alpha-beta window at P depends on what the parent explored first.
#
# Since our TT is shared across all turns in a game, the two passes
# can happen in different AI turns.  Here's how:
#
# --- Turn 1: P cached with a tight window ---
#
# The board before P's parent move (X to move, 3 empty):
#
#   X | X | -
#   O | O | -         Empty: (0,2), (1,2), (2,0)
#   - | O | X
#
# X searches positions in order: (0,2), (1,2), (2,0).
#
#   X tries (0,2):  row 0 complete → X wins!  cur_alpha raised to 1.
#   X tries (1,2):  ...eventually some score ≤ 1.
#   X tries (2,0):  this gives us P (O to move).
#     Because cur_alpha = 1, O's window is [-2, -1].
#     (O's beta = -cur_alpha = -1 — tight!)
#
# X picks (0,2) and wins.  But during the search, P was visited
# and cached with a tight window.
#
# --- Turn 3: P looked up with a wide window ---
#
# Two moves later the game has progressed differently (perhaps the
# first move was random).  Now a different board reaches P:
#
#   X | - | -
#   O | O | -         Empty: (0,1), (0,2), (1,2)
#   X | O | X
#
# X searches positions in order: (0,1), (0,2), (1,2).
#
#   X tries (0,1):  this gives us P (O to move) — the FIRST move
#     X explores, so cur_alpha is still -2.
#     O's window is [-2, 2] — wide open!
#
# P is the same board state, same TT key.  The stale cache entry
# from turn 1 is found.  This is where the flags matter:
#
# -------------------------------------------------------------------------
# Pass 1 (turn 1): P reached with tight window (beta = -1)
# -------------------------------------------------------------------------
#
# O's window: alpha=-2, beta=-1.
#
#   O tries (0,2):                O's cur_alpha=-2  beta=-1
#       X | X | O
#       O | O | -      No winner.  X plays the last cell (1,2).
#       X | O | X
#
#       X | X | O
#       O | O | X      No winner.  Board full → Draw.  Score = 0.
#       X | O | X
#
#       Negated for O: 0.  O's cur_alpha: -2 → 0.
#
#       0 >= beta (-1)?  YES → cutoff!
#       ✂ O never tries (1,2) — the winning move!
#
#   Result cached:  P → score 0.
#
# -------------------------------------------------------------------------
# Pass 2 (turn 3): P reached with a wide window
# -------------------------------------------------------------------------
#
# X plays (0,1) as its first move, reaching P before exploring any
# siblings.  cur_alpha is still -2, so O gets alpha=-2, beta=2.
#
#   Cache lookup finds P → score 0.
#
# ---- WITHOUT flags (score treated as exact): ----------------------------
#
#   Return 0.  P's parent (X) thinks this branch is a draw.
#   X may choose this branch, believing it's safe.
#   But O plays (1,2) and wins:
#
#       X | X | -       X | X | -
#       O | O | -  →    O | O | O    ← O wins!  Row 1 complete.
#       X | O | X       X | O | X
#
#   The game ends with an O victory that perfect play should have
#   avoided — X walked into a loss because the cache lied.
#
# ---- WITH LOWER flag: ---------------------------------------------------
#
#   Cache says: LOWER bound, score 0.  (True value >= 0, could be higher.)
#   Is 0 >= beta (2)?  No — cache not usable.
#
#   Re-search P with the wider window [-2, 2]:
#       O tries (0,2) → 0.
#       O tries (1,2) → +1.  ← found this time!
#       O returns +1.
#
#   X now correctly knows this branch is an O win and avoids it.
#   The game ends in a draw (or X win) as it should.
#
# =========================================================================
#
# Each entry packs score, spot, and bound flag into one EntryInt:
#
#   With BoardInt=UInt16: BOARD_BITS=16, _TT_SPOT_BITS=bit_width(16)=5,
#   _TT_SCORE_BITS=3, _TT_FLAG_BITS=2, _TT_TOTAL_BITS=10.
#
#   Bit layout:    [9 8 | 7 6 5 4 3 | 2 1 0]
#                   flag    spot+1    score+2
#
#   score + 2  (3 bits):  score -1 → 0b001,  0 → 0b010,  +1 → 0b011
#   spot  + 1  (5 bits):  spot   0 → 0b00001 ... spot 15 → 0b10000
#   flag       (2 bits):  EXACT    → 0b00, LOWER → 0b01, UPPER → 0b10

comptime TT_EXACT: EntryInt = 0  # Exact minimax value
comptime TT_LOWER: EntryInt = 1  # Real value >= stored (beta cutoff)
comptime TT_UPPER: EntryInt = 2  # Real value <= stored (alpha not raised)


@always_inline
def _tt_pack(score: Int, spot: Int, flag: EntryInt) -> EntryInt:
    """Pack score, spot, and flag into a single EntryInt.

    With BoardInt=UInt16: BOARD_BITS=16, _TT_SPOT_BITS=bit_width(16)=5,
    _TT_SCORE_BITS=3, _TT_FLAG_BITS=2, _TT_TOTAL_BITS=10.

    Layout:  flag << (_TT_SCORE_BITS + _TT_SPOT_BITS)
                       | (spot+1) << _TT_SCORE_BITS
                                    | (score+2)
           = flag << 8 | (spot+1) << 3 | (score+2)

    Bit ranges (after shifting into position):
      score + 2  (bits 0-2):  -1 → 0b00_00000_001, +1 → 0b00_00000_011
      spot  + 1  (bits 3-7):   0 → 0b00_00001_000, 15 → 0b00_10000_000
      flag       (bits 8-9):   1 → 0b01_00000_000,  2 → 0b10_00000_000

    Example: _tt_pack(score=1, spot=12, flag=TT_EXACT)
      score + 2  =  3  = 0b011         -> bits 0-2
      spot  + 1  = 13  = 0b01101 << 3  -> bits 3-7
      flag       =  0  = 0b00    << 8  -> bits 8-9
      = EntryInt((0 << 8) | (13 << 3) | 3) = 0b00_01101_011 = 107
    """
    return EntryInt(
        (Int(flag) << (_TT_SCORE_BITS + _TT_SPOT_BITS))
        | ((spot + 1) << _TT_SCORE_BITS)
        | (score + 2)
    )


@always_inline
def _tt_score(entry: EntryInt) -> Int:
    """Extract score from bits 0-2 of a packed entry.

    With BoardInt=UInt16: _TT_SCORE_BITS=3,
    mask = (1 << _TT_SCORE_BITS) - 1 = 0b111.
    Return: Int(entry & 0b111) - 2

    Example: _tt_score(0b00_01101_011) -> 0b011 = 3, minus 2 = 1
    """
    return Int(entry & EntryInt((1 << _TT_SCORE_BITS) - 1)) - 2


@always_inline
def _tt_spot(entry: EntryInt) -> Int:
    """Extract spot from bits 3-7 of a packed entry.

    With BoardInt=UInt16: _TT_SCORE_BITS=3,
    _TT_SPOT_BITS=bit_width(16)=5,
    mask = (1 << _TT_SPOT_BITS) - 1 = 0b11111.
    Return: Int((entry >> 3) & 0b11111) - 1

    Example: _tt_spot(0b00_01101_011) -> 0b01101 = 13, minus 1 = 12
    """
    return Int(
        (entry >> EntryInt(_TT_SCORE_BITS))
        & EntryInt((1 << _TT_SPOT_BITS) - 1)
    ) - 1


@always_inline
def _tt_flag(entry: EntryInt) -> EntryInt:
    """Extract flag from bits 8-9 of a packed entry.

    With BoardInt=UInt16: _TT_SCORE_BITS=3, _TT_SPOT_BITS=5,
    shift = _TT_SCORE_BITS + _TT_SPOT_BITS = 8,
    mask = (1 << _TT_FLAG_BITS) - 1 = 0b11.
    Return: (entry >> 8) & 0b11

    Example: _tt_flag(0b00_01101_011) -> 0b00 = 0  (TT_EXACT)
    """
    return (
        (entry >> EntryInt(_TT_SCORE_BITS + _TT_SPOT_BITS))
        & EntryInt((1 << _TT_FLAG_BITS) - 1)
    )


# --- Minimax (negamax + alpha-beta + transposition table) ---
#
# Negamax is a simplification of minimax where both players use the same
# maximising logic.  The trick: the opponent's score is the negation of
# ours, so we always maximise and negate the recursive result.
#
# Alpha-beta narrows the search window [alpha, beta]:
#   alpha = best score we can guarantee so far
#   beta  = best score the opponent can guarantee
# When alpha >= beta the opponent would never allow this position,
# so we prune the remaining moves (beta cutoff).
#
# The initial window [-2, 2] is wider than any possible game score
# (-1, 0, 1), so the first call always produces an exact result.


def _minmax[player: PlayerInt, size: Int](
    x_bits: BoardInt, o_bits: BoardInt,
) -> Move:
    """Entry point — creates a fresh transposition table and searches.

    The initial window [-1, 1] matches the exact range of game scores
    (loss, draw, win).  This means a winning move (score +1) immediately
    triggers a beta cutoff (1 >= 1) so we stop searching — there's no
    better score to find.
    """
    var tt = Dict[KeyInt, EntryInt]()
    return _minmax_impl[player, size](x_bits, o_bits, -1, 1, tt)


# =========================================================================
# Worked example — alpha-beta pruning on a 3x3 board
# =========================================================================
#
# Scores:  +1 = win for the current player
#           0 = draw
#          -1 = loss for the current player
#
# At each node we track a search window [alpha, beta]:
#   alpha = best score the current player can guarantee so far
#   beta  = best score the opponent can guarantee
#
# We start with alpha=-1, beta=1 — the exact range of game scores.
# This means a winning move (score +1) immediately triggers a beta
# cutoff (1 >= 1), so we stop searching as soon as we find a win.
#
# When alpha >= beta the opponent already has a better option elsewhere
# and would never allow this position, so we stop (prune).
#
# In negamax the child's window is the parent's negated and flipped:
# child gets [-beta, -cur_alpha].
#
# Starting board (X to move, 3 empty cells):
#
#   X | O | -         (0,0) | (0,1) | (0,2)
#   ---------         ------+-------+------
#   - | O | -         (1,0) | (1,1) | (1,2)
#   ---------         ------+-------+------
#   O | X | X         (2,0) | (2,1) | (2,2)
#
# Empty: (0,2), (1,0), (1,2)
#
# -------------------------------------------------------------------------
# STEP 1: X tries (0,2)               X's cur_alpha=-1  beta=1
# -------------------------------------------------------------------------
#
#   X | O | X
#   - | O | -      No winner yet.  O to move.
#   O | X | X      O's window: [-beta, -cur_alpha] = [-1, 1]
#
#   1a) O tries (1,0):                O's cur_alpha=-1  beta=1
#       X | O | X
#       O | O | -      No winner.  X to move.  Only (1,2) left.
#       O | X | X      X's window: [-1, 1]
#
#       X plays (1,2):
#       X | O | X
#       O | O | X      Col 2 complete → X wins! Score = +1.
#       O | X | X
#
#       Negated for O: -1.
#       O records: best = ((1,0), -1).  cur_alpha: -1 → -1.
#       -1 >= beta (1)?  No, keep searching.
#
#   1b) O tries (1,2):                O's cur_alpha=-1  beta=1
#       X | O | X
#       - | O | O      No winner.  X to move.  Only (1,0) left.
#       O | X | X      X's window: [-1, 1]
#
#       X plays (1,0):
#       X | O | X
#       X | O | O      Board full, no winner → Draw. Score = 0.
#       O | X | X
#
#       Negated for O: 0.  0 > -1 → O updates best = ((1,2), 0).
#       O's cur_alpha: -1 → 0.  0 >= beta (1)?  No.
#
#   O returns ((1,2), 0).  O picks the draw over letting X win.
#   Negated for X: score = 0.
#
#   X records: best = ((0,2), 0).  cur_alpha: -1 → 0.
#   0 >= beta (1)?  No, keep searching.
#
# -------------------------------------------------------------------------
# STEP 2: X tries (1,2)               X's cur_alpha=0  beta=1
# -------------------------------------------------------------------------
#                                      ^^^^^^^^^^^^^^^^
#   X | O | -      X already knows it can get a draw from step 1.
#   - | O | X      O's window: [-beta, -cur_alpha] = [-1, 0]
#   O | X | X                                             ^
#                   O's beta is now 0 — tighter than before!
#
#   2a) O tries (0,2):                O's cur_alpha=-1  beta=0
#       X | O | O
#       - | O | X      Anti-diagonal (0,2),(1,1),(2,0) → O wins!
#       O | X | X
#
#       Score = +1 for O.  O's cur_alpha: -1 → 1.
#
#       *** PRUNING: O's cur_alpha (1) >= O's beta (0) → cutoff! ***
#
#       X already has a guaranteed draw.  O winning here means X
#       would never choose (1,2), so there's no point checking
#       O's other response.
#
#       ✂ O does NOT try (1,0) — entire subtree skipped.
#
#   O returns ((0,2), +1).
#   Negated for X: score = -1.  -1 < 0 → doesn't beat X's best.
#
# -------------------------------------------------------------------------
# STEP 3: X tries (1,0)               X's cur_alpha=0  beta=1
# -------------------------------------------------------------------------
#
#   X | O | -
#   X | O | -      Same narrowed window as step 2.
#   O | X | X      O's window: [-1, 0]
#
#   3a) O tries (1,2):                O's cur_alpha=-1  beta=0
#       X | O | -
#       X | O | O      No winner.  X to move.  Only (0,2) left.
#       O | X | X      X's window: [0, 1]
#
#       X plays (0,2):
#       X | O | X
#       X | O | O      Board full, no winner → Draw. Score = 0.
#       O | X | X
#
#       Negated for O: 0.
#       O records: best = ((1,2), 0).  cur_alpha: -1 → 0.
#
#       *** PRUNING: O's cur_alpha (0) >= O's beta (0) → cutoff! ***
#
#       A mere DRAW was enough to trigger the cutoff — the pruning
#       is driven by the tight window, not by the score being high.
#       O's remaining move (0,2) would actually have been an O win,
#       but it doesn't matter.  X already has a draw from step 1
#       and would never pick (1,0) anyway.
#
#       ✂ O does NOT try (0,2) — O's own winning move is pruned!
#
#   O returns ((1,2), 0).
#   Negated for X: score = 0.  Doesn't beat X's best (0).
#
# =========================================================================
# Result: X plays (0,2) → Draw.
# =========================================================================
#
#   X | O | -       X | O | X       X | O | X       X | O | X
#   - | O | -  →    - | O | -  →    - | O | O  →    X | O | O
#   O | X | X       O | X | X       O | X | X       O | X | X
#    (start)       X at (0,2)      O at (1,2)      X at (1,0) → Draw
#
# -------------------------------------------------------------------------
# Full tree comparison.  ✂ = pruned, never explored.
# -------------------------------------------------------------------------
#
# Without pruning (plain minimax):
#
# [X wins]        [Draw]           [O wins]      [Draw]         [O wins]      [X wins]
#     |              |                 |             |               |             |
# O at (1,0)    O at (1,2)       O at (0,2)    O at (1,2)     O at (0,2)    O at (1,0)
#      \            /                  \            /                \            /
#    O picks (1,2)→Draw            O picks (0,2)→O wins          O picks (0,2)→O wins
#            |                             |                             |
#      X at (0,2)→0                  X at (1,0)→-1                 X at (1,2)→-1
#             \                            |                            /
#              ·───────────────── X picks (0,2) → Draw ────────────────·
#
#
# With alpha-beta (a=-1,b=1 at root):
#
#                         a=alpha  b=beta  for the current player
#
# [X wins]        [Draw]           [O wins]    ✂[X wins]        [Draw]   ✂[O wins]
# a=-1,b=1       a=-1,b=1         a=-1,b=0                    a=0,b=1
#     |              |                 |                           |
# O(1,0)         O(1,2)           O(0,2)     (1,0) pruned    O(1,2)    (0,2) pruned
# a=-1,b=1       a=-1,b=1         a=-1,b=0                   a=-1,b=0
#      \            /                  |                          |
#    O picks (1,2)→0               O stops                     O stops
#    a=-1→0, b=1                   a→1>=b=0                    a→0>=b=0
#            |                      (cutoff)                   (cutoff)
#      X at (0,2)→0               X at (1,2)→-1             X at (1,0)→0
#      a=-1→0, b=1                a=0, b=1                   a=0, b=1
#             \                        |                        /
#              ·──────────────── X picks (0,2) → Draw ─────────·
#                                a=-1, b=1
#
# Step 2 prunes via an O win — the obvious case.  Step 3 prunes O's
# own WINNING move because a mere draw was enough to trigger the
# cutoff.  Both demonstrate that pruning is about the window, not
# the score.
#
# 2 of 6 leaf nodes are never evaluated.  On a 4x4 board with 16
# cells, this effect compounds at every level, turning hours into ms.


def _minmax_impl[player: PlayerInt, size: Int](
    x_bits: BoardInt, o_bits: BoardInt, alpha: Int, beta: Int,
    mut tt: Dict[KeyInt, EntryInt],
) -> Move:
    """Negamax search with alpha-beta pruning and transposition table.

    Board state is passed by value (two BoardInt copies) so no make/undo
    is needed.  The transposition table is shared across the full search
    tree via mutable reference; it caches results with bound-type flags
    so that positions reached through different move orders are never
    recomputed.
    """
    # --- Terminal states (no caching needed — these are instant) ---

    # Did the current player already win?  (Can happen when called right
    # after our own move in the negamax recursion.)
    if _is_winner_param[size, player](x_bits, o_bits):
        return Move(-1, 1)  # +1 = win for current player

    # Did the opponent win?
    comptime if player == X:
        if _is_winner_param[size, O](x_bits, o_bits):
            return Move(-1, -1)  # -1 = loss for current player
    else:
        if _is_winner_param[size, X](x_bits, o_bits):
            return Move(-1, -1)

    # No moves left and nobody won — draw.
    if _is_full[size](x_bits, o_bits):
        return Move(-1, 0)

    # --- Transposition table probe ---
    # Key packs both bitmasks and the current player into one KeyInt:
    #   bits 0            .. cells-1:      o_bits
    #   bits cells        .. 2*cells-1:    x_bits
    #   bit  2*cells:                      1 if player == O
    #
    # Example (size=4, X at pos 0, O at pos 5, X to move):
    #   x_bits = 0b01, o_bits = 0b100000
    #   key = (0b01 << 16) | 0b100000 = 0x10020
    var key: KeyInt
    comptime if player == X:
        key = (KeyInt(x_bits) << KeyInt(size * size)) | KeyInt(o_bits)
    else:
        key = (KeyInt(x_bits) << KeyInt(size * size)) | KeyInt(o_bits) | (
            KeyInt(1) << KeyInt(2 * size * size)
        )

    # Check if we've already evaluated this exact board state.
    if (cached := tt.get(key)):
        var v = cached.value()
        var flag = _tt_flag(v)
        var score = _tt_score(v)
        var spot = _tt_spot(v)
        # EXACT: we know the true minimax value — use it directly.
        if flag == TT_EXACT:
            return Move(spot, score)
        # LOWER: the real value is >= score.  If that already causes
        # a beta cutoff in the caller's window, we can return early.
        elif flag == TT_LOWER and score >= beta:
            return Move(spot, score)
        # UPPER: the real value is <= score.  If that's below alpha
        # (the caller already has a better option), skip this state.
        elif flag == TT_UPPER and score <= alpha:
            return Move(spot, score)

    # --- Search all empty positions ---
    var best = Move(-1, -2)  # -2 is below any real score, so any move beats it
    var cur_alpha = alpha  # Mutable copy; `alpha` itself is the original for TT classification
    var occupied = x_bits | o_bits

    for pos in range(size * size):
        if (occupied >> BoardInt(pos)) & 1:
            continue
        # Place the current player's piece at `pos` by setting the bit.
        # Because x_bits/o_bits are owned copies, the caller's state
        # is untouched — no undo step needed.
        var bit = BoardInt(1) << BoardInt(pos)
        var score: Int
        # Recurse as the opponent with a negated, flipped window.
        # Their +1 is our -1, so we negate the returned score.
        comptime if player == X:
            score = -_minmax_impl[O, size](
                x_bits | bit, o_bits, -beta, -cur_alpha, tt
            ).score
        else:
            score = -_minmax_impl[X, size](
                x_bits, o_bits | bit, -beta, -cur_alpha, tt
            ).score

        # Update best move if this score is higher.
        if score > best.score:
            best = Move(pos, score)
            if score > cur_alpha:
                cur_alpha = score  # Raise our guaranteed lower bound

        # Beta cutoff: the opponent already has a better option
        # elsewhere, so they'd never let us reach this position.
        # No need to search remaining moves.
        if cur_alpha >= beta:
            break


    # --- Store result in transposition table ---

    # LOWER: We found a move good enough to cause a cutoff,
    # but we didn't try all moves — there might be an even
    # better one we never looked at.  So the true value is
    # at LEAST best.score, possibly higher.
    #
    # We searched ALL moves (no cutoff).  Two cases:
    #
    # EXACT: best.score > alpha (original).  Some move improved our
    # position beyond what the caller already had.  Since we searched
    # everything, this is the true minimax value.
    #
    # UPPER: best.score <= alpha (original).  No move beat what the
    # caller already has from a sibling.  Even though we searched all
    # children, they were searched with windows tightened by our alpha.
    # Those children may have had their own cutoffs, returning inflated
    # LOWER bounds that we negated into our scores.  So our best.score
    # might be higher than the true value.
    #
    # Example: P searched with alpha=0.  Child C gets beta=0 (tight).
    # C finds a draw (score 0), cutoff → LOWER bound (true value >= 0,
    # maybe +1).  Negated at P: 0.  But C's true value might be +1,
    # which negated is -1.  P's true value could be -1, not 0.
    #
    # If cached as EXACT and later looked up with alpha=-1: returns 0.
    # Parent O gets -0=0, thinks it's a draw.  But true value is -1
    # for X = +1 for O — O misses a win.  UPPER prevents this by
    # forcing a re-search when 0 > alpha(-1).
    var flag: EntryInt
    if best.score >= beta:
        flag = TT_LOWER
    elif best.score <= alpha:
        flag = TT_UPPER
    else:
        flag = TT_EXACT
    tt[key] = _tt_pack(best.score, best.spot, flag)
    return best


# --- Display ---


def _player_str(player: PlayerInt) -> String:
    if player == X:
        return "X"
    elif player == O:
        return "O"
    else:
        return "-"


def _show_board[size: Int](x_bits: BoardInt, o_bits: BoardInt):
    comptime SEP: String = "-"
    comptime SEPARATOR: String = SEP * (5 * size)
    print(SEPARATOR)
    for i in range(size):
        for j in range(size):
            print(
                "| ",
                _player_str(_player_at(x_bits, o_bits, i * size + j)),
                " |",
                end="",
                sep="",
            )
        print()
        print(SEPARATOR)


# --- Game loop ---


def _ai_turn[size: Int, player: PlayerInt](
    x_bits: BoardInt, o_bits: BoardInt, strength: UInt8,
    mut tt: Dict[KeyInt, EntryInt], first_move_random: Bool,
) -> Int:
    print(
        "AI turn as ",
        _player_str(player),
        " with strength ",
        strength,
        ".",
        sep="",
    )
    _show_board[size](x_bits, o_bits)
    var spot: Int
    if first_move_random and (x_bits | o_bits) == 0:
        # Pick a random opening move for variety
        spot = Int(random_ui64(0, UInt64(size * size - 1)))
    else:
        # Reuse the transposition table across turns — positions evaluated
        # in earlier turns are still valid and make later turns near-instant.
        spot = _minmax_impl[player, size](x_bits, o_bits, -1, 1, tt).spot
    sleep(UInt(1))
    return spot


def _player_turn[size: Int](
    x_bits: BoardInt, o_bits: BoardInt, player: PlayerInt,
) raises -> Int:
    var player_input: String
    var move: Int
    while True:
        print(
            "Player ",
            _player_str(player),
            " enter your move (0-",
            (size * size) - 1,
            ").",
            sep="",
        )
        _show_board[size](x_bits, o_bits)
        player_input = input()
        try:
            move = Int(player_input)
            if (
                move >= 0
                and move < size * size
                and ((x_bits | o_bits) >> BoardInt(move)) & 1 == 0
            ):
                break
            print("Enter a valid number and try again.")
        except ValueError:
            print("Enter a number and try again.")
            continue
    return move


def _play[size: Int](
    mut x_bits: BoardInt,
    mut o_bits: BoardInt,
    player: PlayerInt,
    x_strength: UInt8,
    o_strength: UInt8,
    mut tt: Dict[KeyInt, EntryInt],
    first_move_random: Bool,
) raises:
    """Execute one turn (AI or human) and update the board."""
    var move: Int
    if player == X and x_strength > 0:
        move = _ai_turn[size, X](
            x_bits, o_bits, x_strength, tt, first_move_random
        )
    elif player == O and o_strength > 0:
        move = _ai_turn[size, O](
            x_bits, o_bits, o_strength, tt, first_move_random
        )
    else:
        move = _player_turn[size](x_bits, o_bits, player)
    if player == X:
        x_bits |= BoardInt(1) << BoardInt(move)
    else:
        o_bits |= BoardInt(1) << BoardInt(move)


def play[size: Int](
    x_strength: UInt8, o_strength: UInt8, first_move_random: Bool
) raises:
    _check_size[size]()

    seed()
    var x_bits: BoardInt = 0
    var o_bits: BoardInt = 0
    var player: PlayerInt = X
    # One TT for the entire game — turn 1 warms the cache, later turns
    # are near-instant because most positions are already solved.
    var tt = Dict[KeyInt, EntryInt]()

    while True:
        _play[size](
            x_bits, o_bits, player, x_strength, o_strength, tt,
            first_move_random,
        )
        if _is_winner[size](x_bits, o_bits, player):
            print("Player ", _player_str(player), " wins!", sep="")
            break
        if _is_full[size](x_bits, o_bits):
            print("It's a draw!")
            break
        player = _swap_player(player)

    _show_board[size](x_bits, o_bits)
