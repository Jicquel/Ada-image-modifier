with Ada.Text_IO; use Ada.Text_IO;

package body Images is

   ----------
   -- Init --
   ----------
   procedure Init
     (Self : in out Image; Width : in Integer; Height : in Integer)
   is

   begin

      Self.Width  := Width;
      Self.Height := Height;
      Self.Pixels := new Pixel_Array (1 .. Pixel_Index (Width * Height));
   end Init;

   -------------
   -- Destroy --
   -------------
   procedure Destroy (Self : in out Image) is
   begin
      null;
      -- FIXME: Free Pixel_Array
   end Destroy;

   --------------
   -- Get_Area --
   --------------
   function Get_Area (Self : Image) return Integer is
   begin
      return Self.Height * Self.Width;
   end Get_Area;

   -----------------
   -- Print_Pixel --
   -----------------
   procedure Print_Pixel (P : in Pixel) is
   begin
      Put
        ("[" & Integer'Image (Integer (P.Red)) & ", " &
         Integer'Image (Integer (P.Green)) & ", " &
         Integer'Image (Integer (P.Blue)) & "]");
   end Print_Pixel;

   ------------------
   -- Print_Pixels --
   ------------------
   procedure Print_Pixels (Self : in Image) is
   begin
      for Idx in Self.Pixels'Range loop
         -- Display pixels line by line
         if (Integer (Idx) mod Self.Width = 1) and Idx /= 1 then
            Put_Line ("");
         end if;

         Print_Pixel (Self.Pixels (Idx));
      end loop;
      Put_Line ("");
   end Print_Pixels;

   --------------------------
   -- Rotate_Pixels_Colors --
   --------------------------
   procedure Rotate_Pixels_Colors (Self : in out Image) is
      Red   : Pixel_Color;
      Green : Pixel_Color;
      Blue  : Pixel_Color;
   begin

      for Idx in Self.Pixels'Range loop
         Red   := Self.Pixels (Idx).Red;
         Green := Self.Pixels (Idx).Green;
         Blue  := Self.Pixels (Idx).Blue;

         Self.Pixels (Idx).Red   := Green;
         Self.Pixels (Idx).Green := Blue;
         Self.Pixels (Idx).Blue  := Red;

      end loop;
   end Rotate_Pixels_Colors;

   ---------------
   -- Set_Pixel --
   ---------------
   procedure Set_Pixel
     (Self : in out Image; Pix : in Pixel; Index : in Pixel_Index)
   is
   begin
      if Integer (Index) > Self.Get_Area then
         raise Constraint_Error;
      end if;

      Self.Pixels (Index) := Pix;
   end Set_Pixel;

   ---------------
   -- Get_Pixel --
   ---------------
   function Get_Pixel (Self : in Image; Index : in Pixel_Index) return Pixel is
   begin
      if Integer (Index) > Self.Get_Area then
         raise Constraint_Error;
      end if;

      return Self.Pixels (Index);
   end Get_Pixel;

end Images;
