with AUnit.Test_Suites;
with Ada.Text_IO;

-- with BE_Base_Model_Types;
-- with DOM_And_Parameter_Tests;
-- with EU.BE.Household.IO.Tests;
with EU.BE.Output.Tests;
-- with EU.BE.Results.IO.Tests;
-- with EU.BE.Model.Runner.Tests;
-- with EU_Tests;
-- with Web_Tests;
with EU.BE.Globals;

function EU_Suite return AUnit.Test_Suites.Access_Test_Suite is

   use AUnit.Test_Suites;
   use Ada.Text_IO;

   result : Access_Test_Suite := new Test_Suite;
begin 
   EU.BE.Globals.Initialise_Settings_And_Datasets( 
      include_base_results => True,
      load_household_data  => True );
   Put_Line( "SUITE STARTING " );
   -- Add_Test( result, new EU_Tests.Test_Case );   
   -- Add_Test( result, new DOM_And_Parameter_Tests.Test_Case );
   -- Add_Test( result, new EU.BE.Household.IO.Tests.Test_Case );   
   Add_Test( result, new EU.BE.Output.Tests.Test_Case );
   -- Add_Test( result, new EU.BE.Results.IO.Tests.Test_Case );
   -- Add_Test( result, new Web_Tests.Test_Case );
   -- Add_Test( result, new EU.BE.Model.Runner.Tests.Test_Case ); 
   return result;      
end EU_Suite;
