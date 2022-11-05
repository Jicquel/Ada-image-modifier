package Images is

   type Image is tagged private;

   procedure Init
     (Self : in out Image; Width : in Integer; Height : in Integer);
   -- Initialize the image, including its array of pixels.

   procedure Destroy (Self : in out Image);
   -- Destroy the image.

   function Get_Area (Self : Image) return Integer;
   -- Compute number of pixels contained in the image.

   procedure Print_Pixels (Self : in Image);
   -- Print image pixels in ASCII (RGB format).

   procedure Rotate_Pixels_Colors (Self : in out Image);
   -- Rotate image pixels colors.

   type Pixel_Color is range 0 .. 255;
   -- Red, Green or Blue element of a Pixel

   type Pixel is record
      Red   : Pixel_Color;
      Green : Pixel_Color;
      Blue  : Pixel_Color;
   end record;
   -- RGB pixel

   Max_Height : constant Integer := 2_000;
   -- Maximum image height supported

   Max_Width : constant Integer := 2_000;
   -- Maximum image width supported

   type Pixel_Index is range 1 .. ((Max_Height * Max_Width));
   -- A maximum of Max_Height * Max_Width pixels can exist.
   -- This index ensures that this constraint is respected.

   procedure Set_Pixel
     (Self : in out Image; Pix : in Pixel; Index : in Pixel_Index);
   -- Set an image pixel

   function Get_Pixel (Self : in Image; Index : in Pixel_Index) return Pixel;
   -- Get an image pixel

private

   type Pixel_Array is array (Pixel_Index range <>) of Pixel;
   -- Array of pixels. Used to represent Image pixels
   -- Note: range <> is for unconstrained arrays.

   procedure Print_Pixel (P : in Pixel);
   -- Print one pixel in ASCII (RGB format).

   type Image is tagged record
      Height : Integer range 1 .. Max_Height;
      Width  : Integer range 1 .. Max_Width;
      Pixels : access Pixel_Array;
   end record;
   -- Image basically containing pixels.

end Images;
