with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AUnit.Test_Cases;
with AUnit;
with AUT;

generic

package Parameter_System.Input_Buffer.Tests is

   use AUnit.Test_Cases;
   use AUnit;
   -- use AUT;
    
   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run
   
   
   --  Override if needed. Default empty implementations provided:
   
   --  Preparation performed before each routine:
   procedure Set_Up (T : in out Test_Case);


end Parameter_System.Input_Buffer.Tests;
