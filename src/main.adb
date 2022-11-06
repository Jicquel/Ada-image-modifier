with Images;            use Images;
with ImageModifiers;    use ImageModifiers;
with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Strings.Bounded;

procedure Main is
   PPM_Modif : PPMImageModifier;

   Max_String_Length : constant Integer := 100;
   package B_Str is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Max_String_Length);
   use B_Str;
   Input_File_Name  : Bounded_String := To_Bounded_String ("image.ppm");
   Output_File_Name : Bounded_String :=
     To_Bounded_String ("rotated_image.ppm");
   Print_Mode : Boolean := False;

   procedure Print_Help is
   begin
      Put_Line
        ("Rotate colors of each image pixels. Only PPM ASCII (mode P3) is currently supported.");
      Put_Line ("Options:");
      Put_Line ("-v: Verbose mode. Print RGB values of each pixel.");
      Put_Line
        ("-i <Input file name>: Input file to transform. Default: image.ppm");
      Put_Line
        ("-o <Output file name>: Output file where result is stored. Default: rotated_image.ppm");
      Put_Line ("-h: Print help.");
   end Print_Help;
begin

   loop
      case Getopt ("v i: o: h") is
         when 'v' =>
            Print_Mode := True;
         when 'i' =>
            Input_File_Name := To_Bounded_String (Parameter);
         when 'o' =>
            Output_File_Name := To_Bounded_String (Parameter);
         when 'h' =>
            Print_Help;
            return;
         when others =>
            exit;
      end case;
   end loop;

   PPM_Modif.Init (To_String (Input_File_Name));
   if Print_Mode then
      Put_Line ("");
      Put_Line ("---- Initial image pixels ----");
      PPM_Modif.Img.Print_Pixels;
   end if;

   PPM_Modif.Img.Rotate_Pixels_Colors;

   if Print_Mode then

      Put_Line ("");
      Put_Line ("---- Rotated image pixels ----");
      PPM_Modif.Img.Print_Pixels;
   end if;

   PPM_Modif.Store_Image (To_String (Output_File_Name));
   PPM_Modif.Destroy;

end Main;
