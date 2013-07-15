with AUnit.Test_Cases;
with AUnit;

package AUT is
   use AUnit.Test_Cases;
   use AUnit;

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   
   function Name (T : Test_Case) return Message_String;

  
   
end AUT;
