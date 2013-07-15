with AUnit.Test_Cases;
with AUnit;

package body AUT is

 
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( "Parameter_System.Input_Buffer.Tests" );
   end Name;
   
end AUT;
