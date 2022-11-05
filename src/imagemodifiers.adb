with Images;            use Images;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO;

package body ImageModifiers is

   -- Note: 'Class allows to declare the procedure as class-wide.
   -- Otherwise, abstract procedures / functions could not be called
   -- as they are not defined for type ImageModfier.

   ----------
   -- Init --
   ----------
   procedure Init (Self : in out ImageModifier'Class; File_Name : in String) is
   begin

      Self.File_Name := To_Bounded_String (File_Name);
      Self.Load_Image;
   end Init;

   -------------
   -- Destroy --
   -------------
   procedure Destroy (Self : in out ImageModifier'Class) is
   begin
      Self.Img.Destroy;
   end Destroy;

   ------------------
   -- PPM modifier --
   ------------------

   type Word_Slice is record
      First : Integer;
      Last  : Integer;
   end record;
   -- Indexes of a word in a String

   type Word_Slice_Array is array (Positive range <>) of Word_Slice;
   -- Indexes of all words in a String

   ----------------------
   -- Get_Words_Slices --
   ----------------------
   function Get_Words_Slices (S : String) return Word_Slice_Array is
      Fi         : Positive;
      La         : Natural;
      I          : Natural                := 1;
      Whitespace : constant Character_Set := To_Set (' ');
      WordIdx    : Positive               := 1;

      function Get_Words_Number (S : String) return Integer is
         Count : Integer := 0;
         J     : Natural := 1;
      begin

         while J in S'Range loop
            Find_Token
              (Source => S, Set => Whitespace, From => J, Test => Outside,
               First  => Fi, Last => La);

            exit when La = 0;

            Count := Count + 1;

            J := La + 1;
         end loop;
         J := 1;
         return Count;
      end Get_Words_Number;

      -- Initialize array with correct size.
      WordSlices : Word_Slice_Array (1 .. Get_Words_Number (S));
   begin
      while I in S'Range loop
         Find_Token
           (Source => S, Set => Whitespace, From => I, Test => Outside,
            First  => Fi, Last => La);

         exit when La = 0;
         WordSlices (WordIdx).First := Fi;
         WordSlices (WordIdx).Last  := La;
         WordIdx                    := WordIdx + 1;

         I := La + 1;
      end loop;
      return WordSlices;
   end Get_Words_Slices;

   --------------------------
   -- Get_Uncommented_Line --
   --------------------------
   function Get_Uncommented_Line (F : File_Type) return String is
      S : Bounded_String;

   begin
      while not End_Of_File (F) loop
         S := To_Bounded_String (Get_Line (F));

         if Length (S) > 0 then
            if To_String (S) (1) /= '#' then
               return To_String (S);
            end if;
         end if;
      end loop;

      raise Data_Error;

   end Get_Uncommented_Line;

   ----------------
   -- Load_Image --
   ----------------
   overriding procedure Load_Image (Self : in out PPMImageModifier) is
      F       : File_Type;
      Width   : Integer;
      Height  : Integer;
      Pix_Idx : Pixel_Index := 1;

   begin
      Ada.Text_IO.Put_Line ("Opening file " & To_String (Self.File_Name));
      Open (F, In_File, To_String (Self.File_Name));

      if Get_Uncommented_Line (F) /= "P3" then
         raise Data_Error;
      end if;

      declare
         S : String           := Get_Uncommented_Line (F);
         W : Word_Slice_Array := Get_Words_Slices (S);
      begin
         -- PPM format ensures that first value is columns number and second is
         -- lines number.
         Width  := Integer'Value (S (W (1).First .. W (1).Last));
         Height := Integer'Value (S (W (2).First .. W (2).Last));
      end;

      -- Skip maximum color value number
      declare
         S : String := Get_Uncommented_Line (F);
      begin
         null;
      end;

      Self.Img.Init (Width => Width, Height => Height);
      while not End_Of_File (F) loop

         declare
            S   : String           := Get_Uncommented_Line (F);
            W   : Word_Slice_Array := Get_Words_Slices (S);
            Pix : Pixel;
         begin
            -- We expect that RGB colors are not split betwen lines
            if W'Length mod 3 /= 0 then
               raise Data_Error;
            end if;
            for Idx in W'Range loop
               if Idx mod 3 = 1 then
                  Pix.Red :=
                    Pixel_Color
                      (Integer'Value (S (W (Idx).First .. W (Idx).Last)));
                  Pix.Green :=
                    Pixel_Color
                      (Integer'Value
                         (S (W (Idx + 1).First .. W (Idx + 1).Last)));
                  Pix.Blue :=
                    Pixel_Color
                      (Integer'Value
                         (S (W (Idx + 2).First .. W (Idx + 2).Last)));

                  Self.Img.Set_Pixel (Pix => Pix, Index => Pix_Idx);
                  Pix_Idx := Pix_Idx + 1;
               end if;
            end loop;
         end;

      end loop;
      Close (F);

   end Load_Image;

   -----------------
   -- Store_Image --
   -----------------
   overriding procedure Store_Image
     (Self : in PPMImageModifier; Output_File_Name : in String)
   is
      Input_File  : File_Type;
      Output_File : File_Type;
   begin
      Ada.Text_IO.Put_Line ("Creating file " & Output_File_Name);
      Create (Output_File, Out_File, Output_File_Name);

      -- Open initial file to copy headers (without comments).
      Open (Input_File, In_File, To_String (Self.File_Name));

      for i in 1 .. 3 loop
         Put_Line (Output_File, Get_Uncommented_Line (Input_File));
      end loop;

      declare
         Image_Area : Integer := Self.Img.Get_Area;
         Pix        : Pixel;
      begin
         for i in 1 .. Image_Area loop
            Pix := Self.Img.Get_Pixel (Pixel_Index (i));
            Put_Line
              (Output_File,
               Integer'Image (Integer (Pix.Red)) & " " &
               Integer'Image (Integer (Pix.Green)) & " " &
               Integer'Image (Integer (Pix.Blue)));
         end loop;
      end;

      Close (Output_File);
      Close (Input_File);

      Put_Line ("Image successfully stored");
   end Store_Image;

end ImageModifiers;
