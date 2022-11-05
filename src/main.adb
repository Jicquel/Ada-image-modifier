with Images;                 use Images;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   I : Image;
begin
   I.Init (4, 4);
   Put ("Image size: ");
   Put (I.Get_Area);
   Put_Line (" pixels.");
   I.Print_Pixels;
end Main;
