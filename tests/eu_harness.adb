with EU_Suite;

with AUnit.Run;
with AUnit.Reporter.Text;
with Utils;




procedure EU_Harness is

   use AUnit.Run;
   use AUnit.Reporter.Text;

   procedure Run is new AUnit.Run.Test_Runner( EU_Suite );
   reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Run( reporter );        
end EU_Harness;
