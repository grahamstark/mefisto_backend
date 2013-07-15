with Ada.Strings.Unbounded;
with Ada.Calendar;
with EU.BE.Model.Settings;
with EU.BE.Output;
with Ada.Containers.Vectors;

package EU.BE.Model.Runner is
   
   use EU.BE.Model.Settings;
   use EU.BE.Output;
   use Ada.Strings.Unbounded;

   type Run_Results_Record is record
      date       : Ada.Calendar.Time;
      success    : Boolean;
      directory  : Unbounded_String;
      state      : State_Rec;
      run_number : Unbounded_String;
   end record;
   function To_String( r : Run_Results_Record ) return String;
   
   
   package Run_Results_Package is new Ada.Containers.Vectors( Index_Type=>Positive, Element_Type=>Run_Results_Record );
   subtype Run_Results_List is Run_Results_Package.Vector;
   NO_RUN_RESULTS : constant Run_Results_List := Run_Results_Package.Empty_Vector;
   
   function Get_New_Run_Id return String;
   
   function Get_Previous_Runs( ctl : in Model_Settings ) return Run_Results_List;
   function Last_Successful_Run( run_results : Run_Results_List ) return Natural;
   
   procedure Run_Direct( ctl : in Model_Settings; output : in out Outputs_Rec; state : out State_Rec ); 
   procedure Monitor( ctl : in Model_Settings; state : in out State_Rec );
   procedure Read_EM_Monitor_File( file_name : String; state : in out State_Rec );
   procedure Write_EM_Monitor_File( file_name : String; state : State_Rec );
   procedure Load_Output( ctl : in Model_Settings; output : in out Outputs_Rec );
   
end EU.BE.Model.Runner;
