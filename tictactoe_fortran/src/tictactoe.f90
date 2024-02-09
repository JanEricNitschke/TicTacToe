module tictactoe_module
   implicit none

   private
   public play_tictactoe


   ! Define the size of the Tic-Tac-Toe board
   integer, parameter :: board_size = 3

   ! Declare the Tic-Tac-Toe board as a 2D array
   character :: board(board_size, board_size) = &
      transpose(reshape(['-','-','-','-','-','-','-','-','-'], [board_size, board_size]))

   type :: coordinate
      integer :: x
      integer :: y
   end type

   integer, parameter :: win_length = 3  ! Number of consecutive cells a player must have to win
   type(coordinate), parameter :: win_conditions(8, win_length) = &
      transpose(reshape([&
      coordinate(1,1), coordinate(1,2), coordinate(1,3), & ! Rows
      coordinate(2,1), coordinate(2,2), coordinate(2,3), & ! Rows
      coordinate(3,1), coordinate(3,2), coordinate(3,3), & ! Rows
      coordinate(1,1), coordinate(2,1), coordinate(3,1), & ! Columns
      coordinate(1,2), coordinate(2,2), coordinate(3,2), & ! Columns
      coordinate(1,3), coordinate(2,3), coordinate(3,3), & ! Columns
      coordinate(1,1), coordinate(2,2), coordinate(3,3), & ! Diagonals
      coordinate(1,3), coordinate(2,2), coordinate(3,1)], & ! Diagonals
      [win_length, 8]))  ! Winning conditions


contains

   subroutine player_turn(player)
      character, intent(in) :: player
      integer :: x, y
      do
         write(*, *) 'Player ', player, ' turn'
         call show_board()
         write(*, *) 'Enter the x and y coordinates of the cell'
         read(*, *) x, y
         if (x < 1 .or. x > board_size .or. y < 1 .or. y > board_size) then
            write(*, *) 'Invalid coordinates. Try again'
            cycle
         else if (board(x,y) /= 'X' .and. board(x,y) /= 'O') then
            board(x,y) = player
            exit
         else
            write(*, *) 'Cell already occupied. Try again'
            cycle
         end if
      end do
   end subroutine player_turn

   subroutine show_board()
      integer :: x, y
      write(*, '(A)') '  1 2 3 y'
      do x = 1, board_size
         write(*, '(I1)', advance='no') x
         write(*, '(A)', advance='no') '|'
         do y = 1, board_size
            write(*, '(A)', advance='no') board(x,y)
            write(*, '(A)', advance='no') '|'
         end do
         write(*, *)
      end do
      write(*, '(A)') 'x'
   end subroutine show_board

   function is_winner(player) result(winner)
      character, intent(in) :: player
      logical :: winner
      integer :: i, j, k
      winner = .false.
      do i = 1, 8
         k = 0
         do j = 1, win_length
            if (board(win_conditions(i,j)%x, win_conditions(i,j)%y) == player) then
               k = k + 1
            end if
         end do
         if (k == win_length) then
            winner = .true.
            exit
         end if
      end do
   end function is_winner

   function is_draw() result(draw)
      logical :: draw
      integer :: x, y
      draw = .true.
      do x = 1, board_size
         do y = 1, board_size
            if (board(x,y) /= 'X' .and. board(x,y) /= 'O') then
               draw = .false.
               exit
            end if
         end do
      end do
   end function is_draw

   function swap_player(player) result(opponent)
      character, intent(in) :: player
      character :: opponent
      if (player == 'X') then
         opponent = 'O'
      else
         opponent = 'X'
      end if
   end function swap_player

   ! Function to play the game
   subroutine play_tictactoe()
      character :: player

      player = 'X'

      do
         call player_turn(player)
         if (is_winner(player)) then
            write(*, *) 'Player ', player, '  wins the game!'
            exit
         end if
         if (is_draw()) then
            write(*, *) 'Match Drawn!'
            exit
         end if
         player = swap_player(player)
      end do

      call show_board()
   end subroutine play_tictactoe

end module tictactoe_module
