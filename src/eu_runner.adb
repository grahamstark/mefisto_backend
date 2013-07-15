with EU.BE.Model.Settings;
with EU.BE.Output;
with EU.BE.Users.IO;
with EU.BE.Users;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with BE_Base_Model_Types;
with EU.BE.Household.IO;
with EU.BE.Household;
with EU.BE.Model.Results;
with EU.BE.Model.Runner;
with EU.BE.Model.Settings;
with Text_Utils;
with EU.BE.Globals;

procedure EU_Runner is
   
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use BE_Base_Model_Types;
   use EU.BE.Household.IO;
	use EU.BE.Output;
   use EU.BE.Household;
   use EU.BE.Model.Results;
   use EU.BE.Model.Runner;
   use EU.BE.Model.Settings;
	use EU.BE.Users.IO;
	use EU.BE.Users;
	use EU.BE.Globals;
   use Text_Utils;

	sep        : constant String := EU.Web.Settings.Dir_Separator;
	username   : constant Unbounded_String := TuS( "test_user" ) & nstr( 2 .. nstr'Length );
	run_number_str : constant String := Get_New_Run_Id;
	output_dir : Unbounded_String;
	model_sett_2 : ms.Model_Settings := model_sett.Copy;
	state : State_Rec;
	output : Outputs_Rec;
begin
	model_sett_2.Set_Run_Id( username & SEP & run_number_str & SEP );
	output_dir := Create_Directories_For_Run( model_sett_2.Working_Root, SEP, user, run_number_str );
	Put_Line( "starting create; username " & TS( username ) & "| run number |" & run_number_str );
	globals.Create_Parameter_Files( model_sett_2, param_buffer );
	Run_Direct( model_sett_2, output, state ); 
	Assert( state.error_code = 1, "run shpuld return 1 was " & Integer'Image( state.error_code ));
end EU_Runner;
