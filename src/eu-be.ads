with Format_Utils;

package EU.BE is

  package BE_Format_Utils is new 
      Format_Utils( 
         Counter_Type => Counter_Type, 
         Float_Type => Rate,
         Default_Separator => ',',
         Default_Radix_Mark => '.',
         Amount_Picture => 
            Ada.Text_IO.Editing.To_Picture( "-ZZZ_ZZZ_ZZZ_ZZZ_ZZZ_ZZ9.99") ); -- comma seperator

   use BE_Format_Utils;

   
end EU.BE;
