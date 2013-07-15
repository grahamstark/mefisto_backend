with Ada.Strings.Unbounded;
with Text_Utils;
with Ada.Calendar.Formatting;

package EU.BE.Model.Settings is

   use Text_Utils;
   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   
   -- FIXME: these are common (apart from years) between at least UK and EU 
   -- and need to be put somewhere 
   -- common
   --                         1         2          3            4               5             6               7               8            9     
   type Phase_Type is ( not_started, starting, currency, households, non_households, eu_output, eu_run_complete, mefisto_output, mefisto_complete );
   type Health_Type is ( normal, aborted, in_error, queued );
   
   subtype Running_Phase is Phase_Type range starting .. eu_output;

   DEFAULT_TIME : constant Time := Time_Of( Year_Number'First, Month_Number'First, Day_Number'First, 0.0 );

   type State_Rec is record
      queued        : Boolean := False; -- FIXME duplicates information in queued
      household     : Natural := 0;
      module        : Natural := 0;        
      other_counter : Natural := 0; -- num tables generated, or whatever
      year          : Positive; -- FIXME Data_Constants.Data_Years := Data_Constants.Data_Years'First;
      phase         : Phase_Type  := not_started;
      health        : Health_Type := normal;
      error_code    : Integer := 0;
      message       : Unbounded_String := Null_Unbounded_String;
      pid           : Integer := 0;
      exit_state    : Integer := 0;
      execution_time : Duration := 0.0;
      start_time     : Time := DEFAULT_TIME;
      monitor_read_error : Boolean := False;
   end record;
   
    NULL_STATE_REC : constant State_Rec := (
      queued             => False,
      household          => 0,
      module             => 0,        
      other_counter      => 0, -- num tables generated, or whatever
      year               => Positive'First,
      -- wave          => Waves'First,
      phase              => not_started,
      health             => normal,
      error_code         => 0,
      monitor_read_error => False,
      message            => TuS( "" ),
      pid                => -1,
      exit_state         => 0,
      execution_time     => 0.0,
      start_time         => DEFAULT_TIME
   );
   
   function State_To_Parameters( state : State_Rec ) return Unbounded_String;
   function Load_State_From_URL( full_url_string : Unbounded_String ) return State_Rec;

   type Model_Settings is tagged private;
   
   function Get_Null_Model_Settings return Model_Settings;
   
   function Read_Model_Settings( filename : String ) return Model_Settings;
   
   procedure Set_Run_Id( settings : in out Model_Settings; run_id : in Unbounded_String );
   procedure Set_Run_Id( settings : in out Model_Settings; run_id : in String );
   procedure Set_Users_Directory( settings : in out Model_Settings; dir : in String );
   procedure Set_Users_Directory( settings : in out Model_Settings; dir : in Unbounded_String );
   procedure Set_Dir_Separator( settings : in out Model_Settings; sep : Character );
   
   function Create_Zip_File( this : in Model_Settings ) return Boolean; 
   
   function Year( this : in Model_Settings ) return Positive; 
   
   function Run_Id( this : in Model_Settings ) return String; 
   
   function Qualified_Run_Directory( this : in Model_Settings ) return String;
   
   function Model_Root( this : in Model_Settings ) return String; 
   
   function Working_Root( this : in Model_Settings ) return String; 
   
   function Invocation_Script( this : in Model_Settings ) return String; 
   
   function Datafile_Directory( this : in Model_Settings ) return String; 
   
   function Base_Log_File_Name( this : in Model_Settings ) return String;
   function Qualified_Log_File_Name( this : in Model_Settings ) return String; 
   
   function Base_Monitor_File_Name( this : in Model_Settings ) return String;
   function Qualified_Monitor_File_Name( this : in Model_Settings ) return String; 
   
   function Base_Output_Directory( this : in Model_Settings ) return String; 
   function Qualified_Output_Directory( this : in Model_Settings ) return String; 

   function Base_Output_File_Name( this : in Model_Settings ) return String; 
   function Qualified_Output_File_Name( this : in Model_Settings ) return String; 
   
   function Base_Users_Directory( this : in Model_Settings ) return String; 
   function Qualified_Users_Directory( this : in Model_Settings ) return String; 
   
   function Qualified_Main_Configuration_File_Name( this : in Model_Settings ) return String; 

   function Log_Warnings( this : in Model_Settings ) return Boolean; 
   
   function Log_Runtime( this : in Model_Settings ) return Boolean; 
   
   function Dataset_Name( this : in Model_Settings ) return String; 
   
   function Spine_File_Name( this : in Model_Settings ) return String; 
   
   function System_Name( this : in Model_Settings ) return String; 

   function Euromod_Version( this : in Model_Settings ) return String; 
   
   function Qualified_Parameter_Directory( this : in Model_Settings ) return String; 
   
   function Control_File_Name( this : in Model_Settings ) return String;
   
   function Poverty_Line_Per_Month_Per_Person( this : in Model_Settings ) return Amount;
   
   function Copy( settings : in Model_Settings ) return Model_Settings;
   
   procedure Write_EU_Config_File( settings : in Model_Settings );
   
private   
   --
   type Model_Settings is tagged record
      
      is_null_settings           : Boolean := False; -- for session storage in AWS
   
      t_control_file_name     : Unbounded_String := To_Unbounded_String("");
      t_run_id                   : Unbounded_String := To_Unbounded_String("");
      t_year                     : Positive := 2006; -- Data_Constants.Data_Years := Data_Constants.Data_Years'First;
      
      t_model_root               : Unbounded_String := To_Unbounded_String("");
      t_working_root             : Unbounded_String := To_Unbounded_String("");
      t_invocation_script        : Unbounded_String := To_Unbounded_String("");
      
      t_log_warnings             : Boolean := True;
      t_log_runtime              : Boolean := False;
      
      t_poverty_line_per_month_per_person : Amount := 777.0;
      
      t_datafile_directory       : Unbounded_String := To_Unbounded_String("");
      t_dataset_name             : Unbounded_String := To_Unbounded_String("");
      t_spine_file_name          : Unbounded_String := To_Unbounded_String("");
      t_system_name              : Unbounded_String := To_Unbounded_String("");
      t_parameter_directory      : Unbounded_String := To_Unbounded_String("");
      t_output_directory         : Unbounded_String := To_Unbounded_String("");
      t_euromod_version          : Unbounded_String := To_Unbounded_String("");
      
      t_log_file_name            : Unbounded_String := To_Unbounded_String("");
      t_main_configuration_file_name      : Unbounded_String := To_Unbounded_String("");
      t_monitor_file_name        : Unbounded_String := To_Unbounded_String("");
      t_output_file_name         : Unbounded_String := To_Unbounded_String("");
      t_users_directory          : Unbounded_String := To_Unbounded_String("");
      t_dir_separator            : Character;
      t_create_zip_file          : Boolean;
   end record;
 

end EU.BE.Model.Settings;
