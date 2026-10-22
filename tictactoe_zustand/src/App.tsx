import type { ReactElement } from 'react';
import { combine } from 'zustand/middleware';
import { create } from 'zustand';

type Player = 'X' | 'O';
type SquareValue = Player | null;
type Board = SquareValue[];
type History = Board[];

type WinningLine = readonly [
  firstIndex: number,
  secondIndex: number,
  thirdIndex: number,
];

interface GameState {
  history: History;
  currentMove: number;
}

interface GameActions {
  setHistory: (nextHistory: History | ((history: History) => History)) => void;
  setCurrentMove: (
    nextCurrentMove: number | ((nextCurrentMove: number) => number),
  ) => void;
}

interface SquareProps {
  value: SquareValue;
  onSquareClick: () => void;
}

interface BoardProps {
  xIsNext: boolean;
  board: Board;
  onPlay: (board: Board) => void;
}

const useGameStore = create(
  combine<GameState, GameActions>(
    {
      history: [Array.from({ length: 9 }, (): SquareValue => null)],
      currentMove: 0,
    },
    (set) => ({
      setHistory: (nextHistory) => {
        set((state) => ({
          history:
            typeof nextHistory === 'function'
              ? nextHistory(state.history)
              : nextHistory,
        }));
      },
      setCurrentMove: (nextCurrentMove) => {
        set((state) => ({
          currentMove:
            typeof nextCurrentMove === 'function'
              ? nextCurrentMove(state.currentMove)
              : nextCurrentMove,
        }));
      },
    }),
  ),
);

function calculateWinner(squares: Board): SquareValue {
  const lines: WinningLine[] = [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6],
  ];

  for (const [firstIndex, secondIndex, thirdIndex] of lines) {
    const firstValue = squares[firstIndex];
    const secondValue = squares[secondIndex];
    const thirdValue = squares[thirdIndex];

    if (firstValue && firstValue === secondValue && firstValue === thirdValue) {
      return firstValue;
    }
  }

  return null;
}

function calculateTurns(board: Board): number {
  return board.filter((square) => !square).length;
}

function calculateStatus(
  winner: SquareValue,
  turns: number,
  player: Player,
): string {
  if (!winner && !turns) {
    return 'Draw';
  }
  if (winner) {
    return `Winner ${winner}`;
  }
  return `Next player: ${player}`;
}

function Square({ value, onSquareClick }: SquareProps): ReactElement {
  return (
    <button
      style={{
        display: 'inline-flex',
        alignItems: 'center',
        justifyContent: 'center',
        padding: 0,
        backgroundColor: '#fff',
        border: '1px solid #999',
        outline: 0,
        borderRadius: 0,
        fontSize: '1rem',
        fontWeight: 'bold',
      }}
      type="button"
      onClick={onSquareClick}
    >
      {value}
    </button>
  );
}

function GameBoard({ xIsNext, board, onPlay }: BoardProps): ReactElement {
  const player: Player = xIsNext ? 'X' : 'O';
  const winner = calculateWinner(board);
  const turns = calculateTurns(board);
  const status = calculateStatus(winner, turns, player);

  function handleClick(squareIndex: number): void {
    if (board[squareIndex] || winner) {
      return;
    }
    const nextBoard = [...board];
    nextBoard[squareIndex] = player;
    onPlay(nextBoard);
  }

  return (
    <>
      <div style={{ marginBottom: '0.5rem' }}>{status}</div>
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(3, 1fr)',
          gridTemplateRows: 'repeat(3, 1fr)',
          width: 'calc(3 * 2.5rem)',
          height: 'calc(3 * 2.5rem)',
          border: '1px solid #999',
        }}
      >
        {board.map((square, squareIndex) => (
          <Square
            key={squareIndex}
            value={square}
            onSquareClick={() => handleClick(squareIndex)}
          />
        ))}
      </div>
    </>
  );
}

function Game(): ReactElement {
  const history = useGameStore((state) => state.history);
  const setHistory = useGameStore((state) => state.setHistory);
  const currentMove = useGameStore((state) => state.currentMove);
  const setCurrentMove = useGameStore((state) => state.setCurrentMove);
  const currentBoard = history.at(currentMove);

  if (!currentBoard) {
    throw new Error(`No board found at move ${currentMove}`);
  }
  const xIsNext = currentMove % 2 === 0;

  function handlePlay(nextBoard: Board): void {
    const nextHistory = [...history.slice(0, currentMove + 1), nextBoard];
    setHistory(nextHistory);
    setCurrentMove(nextHistory.length - 1);
  }

  function jumpTo(nextMove: number): void {
    setCurrentMove(nextMove);
  }

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        fontFamily: 'monospace',
      }}
    >
      <div>
        <GameBoard xIsNext={xIsNext} board={currentBoard} onPlay={handlePlay} />
      </div>
      <div style={{ marginLeft: '1rem' }}>
        <ol>
          {history.map((_board, historyIndex) => {
            const description =
              historyIndex > 0
                ? `Go to move #${historyIndex}`
                : 'Go to game start';

            return (
              <li key={historyIndex}>
                <button type="button" onClick={() => jumpTo(historyIndex)}>
                  {description}
                </button>
              </li>
            );
          })}
        </ol>
      </div>
    </div>
  );
}

function App(): ReactElement {
  return <Game />;
}

export default App;
