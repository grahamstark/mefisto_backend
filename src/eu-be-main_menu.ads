with Ada.Strings.Unbounded;
with EU.BE.I81N;

package EU.BE.Main_Menu is
  
  use Ada.Strings.Unbounded;
  use EU.BE.I81N;
  
  type Section_Type is ( home_page, light_page, parameters_page, output_page );
  
  function Get_Main_Menu( 
        which_page      : Section_Type;
        output_disabled : Boolean;
        lang            : Languages ) return Unbounded_String; 

end EU.BE.Main_Menu; 
