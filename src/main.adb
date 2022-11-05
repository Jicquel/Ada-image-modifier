with Images;              use Images;
with ImageModifiers;      use ImageModifiers;
with Ada.Text_IO;         use Ada.Text_IO;

procedure Main is
   PPM_Modif : PPMImageModifier;
begin
   PPM_Modif.Init ("./image.ppm");
   Put_Line("");
   Put_Line("---- Initial image pixels ----");
   PPM_Modif.Img.Print_Pixels;
   PPM_Modif.Img.Rotate_Pixels_Colors;
   Put_Line("");
   Put_Line("---- Rotated image pixels ----");
   PPM_Modif.Img.Print_Pixels;
   PPM_Modif.Store_Image("./rotated_image.ppm");
   PPM_Modif.Destroy;

end Main;
