with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Tictactoe_Ada is
   type Array_Entry is ('X', 'O', '1', '2', '3', '4', '5', '6', '7', '8', '9');
   subtype Player_Char is Array_Entry range 'X' .. 'O';
   --  subtype Open_Spot is Array_Entries range '1' .. '9';
   subtype Board_Index is Integer range 1 .. 9;
   subtype Rows is Board_Index range 1 .. 3;
   subtype Columns is Board_Index range 1 .. 3;
   type Board is array (Board_Index) of Array_Entry;

   Game_Board : Board := ('1', '2', '3', '4', '5', '6', '7', '8', '9');

   procedure Show_Board (Game_Board : Board) is
      Line_Seperator : constant String := "---------------------";
   begin
      Put_Line (Line_Seperator);
      for Row in Rows loop
         for Column in Columns loop
            Put
              ("| " &
               Array_Entry'Image (Game_Board (((Row - 1) * 3) + Column)) &
               " |");
         end loop;
         New_Line;
         Put_Line (Line_Seperator);
      end loop;
   end Show_Board;

   Player : Player_Char := 'X';

   function Swap_Player (Player : Player_Char) return Player_Char is
   begin
      if Player = 'X' then
         return 'O';
      end if;
      return 'X';
   end Swap_Player;

   Spot : Board_Index;
   Spot_Filled_Except : exception;

   procedure Fix_Spot
     (Player : Player_Char; Spot : Board_Index; Game_Board : in out Board)
   is
   begin
      if Game_Board (Spot) in Player_Char then
         raise Spot_Filled_Except;
      end if;
      Game_Board (Spot) := Player;
   end Fix_Spot;

   procedure Player_Turn (Player : Player_Char; Game_Board : in out Board) is
   begin
      loop
         Put_Line ("Player " & Player_Char'Image (Player) & "turn.");
         Show_Board (Game_Board);
         begin
            Get (Spot);
            Fix_Spot (Player, Spot, Game_Board);
            exit;
         exception
            when Spot_Filled_Except =>
               Put_Line ("Spot already filled");
            when Data_Error         =>
               Put_Line ("Spot has to be a valid integer in range 1..9");
               Skip_Line;
            when Constraint_Error   =>
               Put_Line ("Spot has to be a valid integer in range 1..9");
         end;
      end loop;
   end Player_Turn;

begin
   Ada.Text_IO.Put_Line ("Hello Ada World!");
   Ada.Text_IO.Put_Line (Player_Char'Image (Player));
   Ada.Text_IO.Put_Line (Player_Char'Image (Swap_Player (Player)));
   loop
      Player_Turn (Player, Game_Board);
      Player := Swap_Player (Player);
   end loop;
end Tictactoe_Ada;
