using Xunit;
using Tictactoe.Lib;

namespace Tictactoe.Tests
{
    public class Tictactoe_SwapPlayerShould
    {
        [Fact]
        public void SwapPlayer_InputIsX_ReturnO()
        {
            var result = Game.SwapPlayer('X');
            Assert.Equal('O', result);
        }

        [Fact]
        public void SwapPlayer_InputIsO_ReturnX()
        {
            var result = Game.SwapPlayer('O');
            Assert.Equal('X', result);
        }
    }
}
