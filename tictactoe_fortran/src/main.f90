program main
   use tictactoe_module
   implicit none

   integer :: i, j
   ! Your main program and argument parsing go here
   write(*,*) 'Hello, Tic-Tac-Toe!'

   ! Call the initialize_board function
   call initialize_board()

   ! You can now access the initialized board in the tictactoe_module
   ! For example, print the board
   write(*,*) 'Initialized Board:'
   do i = 1, board_size
      write(*,*) (board(i, j), j = 1, board_size)
   end do

end program main
