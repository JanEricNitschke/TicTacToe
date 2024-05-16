using Xunit;
using Tictactoe.Lib;

namespace Tictactoe.Tests
{
    public class Tictactoe_BestMoveShould
    {
        [Fact]
        public void Minmax_FindBestSpot()
        {
            var game = new Game();
            game.board = ['X', '1', '2', '3', '4', '5', '6', '7', '8'];
            var result = game.BestMove('O');
            var expected = new Move(4, Score.TIE);
            Assert.Equal(expected, result);
        }
    }
}
