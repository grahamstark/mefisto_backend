with Ada.Strings.Unbounded;

package EU.BE.Results.IO is

   use Ada.Strings.Unbounded;
   
   function Read_Summary( line : Unbounded_String ) return Summary_Record;
   function Read_Detailed( line : Unbounded_String ) return Detailed_Record;
   
   procedure Read_To_List( 
      filename : String; 
      personal_level_list : out Detailed_Results_List; 
      hh_level_list : out Detailed_Results_List );

end EU.BE.Results.IO;
