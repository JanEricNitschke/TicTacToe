module tictactoe_module
   implicit none

   ! Define the size of the Tic-Tac-Toe board
   integer, parameter :: board_size = 3

   ! Declare the Tic-Tac-Toe board as a 2D array
   integer :: board(board_size, board_size)

contains

   ! Function to initialize the Tic-Tac-Toe board
   subroutine initialize_board()
      integer :: i, j

      do i = 1, board_size
         do j = 1, board_size
            board(i, j) = 0
         end do
      end do
   end subroutine initialize_board

end module tictactoe_module
