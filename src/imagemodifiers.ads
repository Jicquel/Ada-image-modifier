with Images;      use Images;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded;

package ImageModifiers is

   Max_String_Length : constant Integer := 100;
   package B_Str is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Max_String_Length);
   use B_Str;
   -- Required to use Bounded String.

   type ImageModifier is abstract tagged limited record
      File_Name : Bounded_String;
      Img       : Image;
   end record;
   -- ImageModifier can serve for several image Modifier, for example PPM
   -- or PNG formats. Each format needs to define abstract methods.

   procedure Init (Self : in out ImageModifier'Class; File_Name : in String);
   -- Initialize the Image Modifier.
   
   procedure Destroy (Self : in out ImageModifier'Class);
   -- Destroy the Image Modifier.

   ---------------------------------------
   -- Abstract functions and procedures --
   ---------------------------------------
   
   procedure Load_Image (Self : in out ImageModifier) is abstract;
   -- Load an image in memory from a file.

   procedure Store_Image
     (Self : in ImageModifier; Output_File_Name : in String) is abstract;
   -- Create an image from current image state.

   ------------------
   -- PPM modifier --
   ------------------

   type PPMImageModifier is new ImageModifier with null record;
   -- PPM format image modifier.

   overriding procedure Load_Image (Self : in out PPMImageModifier);
   -- Load image from file.

   overriding procedure Store_Image
     (Self : in PPMImageModifier; Output_File_Name : in String);
   -- Store current image state in a file.

end ImageModifiers;
