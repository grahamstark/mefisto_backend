pragma License( Modified_GPL );

with Ada.Strings.Unbounded;
with AWS.Session;
with Ada.Containers.Vectors;
with EU.BE.Model.Settings;
with EU.BE.I81N;
with EU.BE.Users;

package EU.BE.Model.Runner.Web is

   use Ada.Strings.Unbounded;
   use EU.BE.Model.Settings;
   use EU.BE.I81N;
   use EU.BE.Users;


   function Get_State_Of_Run_As_HTML( 
      run_state : State_Rec;
      lang      : Languages ) return Unbounded_String;
      
   procedure Submit_Run( session_id : AWS.Session.Id; 
                         run_settings : Model_Settings );

private                         
   --
   -- for testing only
   --
   function Monitor_To_HTML( 
      state : State_Rec; 
      lang : Languages ) return Unbounded_String;
                         
end EU.BE.Model.Runner.Web;
