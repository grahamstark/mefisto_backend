with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Templates_Parser; 
with Keyed_Text_Buffer;
with AWS.URL;

package body EU.BE.Model.Settings is

   function Get_Null_Model_Settings return Model_Settings is
      m : Model_Settings;
    begin
       m.is_null_settings := True;
       return m;
    end Get_Null_Model_Settings;
      
   
   function Yes_No( b : Boolean ) return String is
   begin
      if( b ) then return "yes"; else return "no"; end if;
   end Yes_No;
   
   function Now_As_String return String is
   begin
      return Ada.Calendar.Formatting.Image(
         Date => Ada.Calendar.Clock,
         Include_Time_Fraction => True
       );
   end Now_As_String;
   

   function Read_Model_Settings( filename : String ) return Model_Settings is
      use EU_Key_Value_IO;
      use Ada.Text_IO;
      file : Ada.Text_IO.File_Type;  
      settings : Model_Settings;
   begin
      Open( file, In_File, filename );

      settings.t_model_root := Read( file, "model_root" );
      settings.t_working_root := Read( file, "working_root" );
      settings.t_invocation_script := Read( file, "invocation_script" );
      settings.t_log_warnings := Read( file, "log_warnings" );
      settings.t_log_runtime := Read( file, "log_runtime" );
      settings.t_datafile_directory := Read( file, "datafile_directory" );
      settings.t_dataset_name := Read( file, "dataset_name" );
      settings.t_spine_file_name := Read( file, "spine_file_name" );
      settings.t_system_name := Read( file, "system_name" );
      settings.t_parameter_directory := Read( file, "parameter_directory" );
      settings.t_output_directory := Read( file, "output_directory" );
      settings.t_control_file_name := Read( file, "control_file_name" );
      settings.t_euromod_version := Read( file, "euromod_version" );
      settings.t_Log_File_Name := Read( file, "log_file_name" );
      settings.t_main_configuration_file_name := Read( file, "main_configuration_file_name" );
      settings.t_monitor_file_name := Read( file, "monitor_file_name" );
      settings.t_poverty_line_per_month_per_person := Read( file, "poverty_line_per_month_per_person" );
      settings.t_output_file_name := Read( file, "output_file_name" );
      settings.t_create_zip_file := Read( file, "create_zip_file" );
      Close( file );
      return settings;
   end Read_Model_Settings;
   
   function Copy(  settings : in Model_Settings ) return Model_Settings is
      c : Model_Settings := settings;
   begin
      return c;   
   end Copy;
   
   function Create_Zip_File( this : in Model_Settings ) return Boolean is
   begin
      return this.t_create_zip_file;
   end Create_Zip_File;
   
   procedure Write_EU_Config_File( settings : in Model_Settings ) is
   use Templates_Parser;
   use Ada.Text_IO;
      translations : Translate_Set;
      output_str : Unbounded_String;
      file : Ada.Text_IO.File_Type;  
      template_name : constant String := To_String( settings.t_model_root & "etc/templates/control.tmpl" );
   begin
      Put_Line( "opening " & template_name );
      Insert( translations, Assoc( "ERRLOG_FILE", settings.Qualified_Log_File_Name ));
      Insert( translations, Assoc( "LOG_WARNINGS", Yes_No( settings.Log_Warnings )));
      Insert( translations, Assoc( "EMVERSION", settings.Euromod_Version ));
      Insert( translations, Assoc( "PARAMPATH", settings.Qualified_Parameter_Directory ));
      Insert( translations, Assoc( "OUTPUTPATH", settings.Qualified_Output_Directory ));
      Insert( translations, Assoc( "DATAPATH", settings.Datafile_Directory ));
      Insert( translations, Assoc( "DATASETNAME", settings.Dataset_Name ));
      Insert( translations, Assoc( "HEADER_DATE", Now_As_String ));
      Insert( translations, Assoc( "OUTFILE_DATE", "-" ));
      Insert( translations, Assoc( "LOG_RUNTIME", Yes_No( settings.t_Log_Runtime )));
      Insert( translations, Assoc( "LAST_RUN", "yes" )); -- FIXME
      Insert( translations, Assoc( "DECSIGN_PARAM","XX" ));
      Insert( translations, Assoc( "CONTROL", settings.t_Control_File_Name ));
      Insert( translations, Assoc( "SPINE", settings.t_Spine_File_Name ));
      Insert( translations, Assoc( "SYSTEM", settings.t_System_Name ));
      output_str := Parse( template_name, translations );
      Create( file, Out_File, settings.control_file_name );
      Put_Line( "writing to: " & settings.control_file_name );
      Put_Line( file, To_String( output_str ));
      Close( file );      
   end Write_EU_Config_File;

   procedure Set_Users_Directory( settings : in out Model_Settings; dir : in String ) is
   begin
      settings.t_users_directory := TuS( dir );
   end Set_Users_Directory;      
   
   procedure Set_Users_Directory( settings : in out Model_Settings; dir : in Unbounded_String ) is
     begin
      settings.t_users_directory := dir;
   end Set_Users_Directory;      

   
   function Poverty_Line_Per_Month_Per_Person( this : in Model_Settings ) return Amount is
   begin
      return this.t_poverty_line_per_month_per_person;
   end Poverty_Line_Per_Month_Per_Person;
   
   function Log_Warnings( this : in Model_Settings ) return Boolean is 
   begin 
      return this.t_log_warnings; 
   end Log_Warnings;
   
   function Log_Runtime( this : in Model_Settings ) return Boolean is 
   begin 
      return this.t_log_runtime; 
   end Log_Runtime;
   
   function Dataset_Name_L( this : in Model_Settings ) return Unbounded_String is 
   begin 
      return this.t_dataset_name; 
   end Dataset_Name_L;
   
   function Dataset_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_dataset_name ); 
   end Dataset_Name;

   function Spine_File_Name_L( this : in Model_Settings ) return Unbounded_String is 
   begin 
      return this.t_spine_file_name; 
   end spine_file_name_L;
   
   function Spine_File_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_spine_file_name ); 
   end spine_file_name;

   function System_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_system_name ); 
   end System_Name;
   
   function Qualified_Output_Directory( this : in Model_Settings ) return String is 
   begin 
      return TS( this.Qualified_Run_Directory & this.t_output_directory & this.t_dir_separator ); 
   end Qualified_Output_Directory;
    
   function Base_Output_Directory( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_output_directory ); 
   end Base_Output_Directory;

   function Qualified_Users_Directory( this : in Model_Settings ) return String is 
   begin 
      return TS( this.t_working_root & this.t_dir_separator & this.t_users_directory & this.t_dir_separator ); 
   end Qualified_Users_Directory;
   
   function Base_Users_Directory( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_users_directory ); 
   end Base_Users_Directory;


   function Control_File_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_control_file_name ); 
   end Control_File_Name;
   
   function Euromod_Version( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_euromod_version ); 
   end Euromod_Version;
   
   
   procedure Set_Dir_Separator( settings : in out Model_Settings; sep : Character ) is
   begin
      settings.t_dir_separator := sep;
   end Set_Dir_Separator;
   
   procedure Set_Run_Id( settings : in out Model_Settings; run_id : in String ) is
   begin
      settings.t_run_id := To_Unbounded_String( run_id );
   end Set_Run_Id;
   
   procedure Set_Run_Id( settings : in out Model_Settings; run_id : in Unbounded_String ) is
   begin
      settings.t_run_id := run_id;
   end Set_Run_Id;
   
   function Year( this : in Model_Settings ) return Positive is 
   begin 
      return this.t_year; 
   end Year;
   
   function Run_Id( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_run_id ); 
   end Run_Id;
   
   function Working_Root( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_working_root ); 
   end Working_Root;

   function Model_Root( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_model_root ); 
   end Model_Root;

   function Invocation_Script( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_model_root & this.t_invocation_script ); 
   end Invocation_Script;

   function Datafile_Directory( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_model_root & this.t_datafile_directory ); 
   end Datafile_Directory;
   
   function Qualified_Run_Directory( this : in Model_Settings ) return String is
   begin
      return To_String( this.Qualified_Users_Directory & this.t_run_id & this.t_dir_separator );
   end Qualified_Run_Directory;

   function Qualified_Parameter_Directory( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.Qualified_Run_Directory & this.t_parameter_directory & this.t_dir_separator ); 
   end Qualified_Parameter_Directory;
   
   function Base_Log_File_Name( this : in Model_Settings ) return String is 
   begin 
      return TS( this.t_Log_File_Name );
   end Base_Log_File_Name;

   function Qualified_Log_File_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.Qualified_Run_Directory & "log" & this.t_dir_separator & this.t_Log_File_Name ); 
   end Qualified_Log_File_Name;
   
   function Qualified_Main_Configuration_File_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.Qualified_Parameter_directory & this.t_dir_separator & this.t_main_configuration_file_name ); 
   end Qualified_Main_Configuration_File_Name;
   
   function Qualified_Monitor_File_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.Qualified_Output_Directory  & this.t_monitor_file_name ); 
   end Qualified_Monitor_File_Name;
   
   function Base_Monitor_File_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_monitor_file_name ); 
   end Base_Monitor_File_Name;
   

   function Qualified_Output_File_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.Qualified_Output_Directory  & this.t_dir_separator & this.t_Output_file_name ); 
   end Qualified_Output_File_Name;

   function Base_Output_File_Name( this : in Model_Settings ) return String is 
   begin 
      return To_String( this.t_Output_file_name ); 
   end Base_Output_File_Name;
   
   
   --
   -- Fixme maybe move these 2 to child package to remove AWS dependency?
   --
   
   function Load_State_From_URL( full_url_string : Unbounded_String ) return State_Rec is
      use Keyed_Text_Buffer;
      use Ada.Text_IO;
      url : Keyed_Text_Buffer.Text_Buffer;
      state : State_Rec;
   begin
      Put_Line( "Load_State_From_URL entered" );
      url := Parse( full_url_string, '&' );
      Put_Line( "Load_State_From_URL parsed OK" );
      state.queued := Boolean'Value( Keyed_Text_Buffer.Get( url, "queued" ));
      state.household := Natural'Value( Keyed_Text_Buffer.Get( url, "household" ));
      state.module := Natural'Value( Keyed_Text_Buffer.Get( url, "module" ));
      state.other_counter := Natural'Value( Keyed_Text_Buffer.Get( url, "other_counter" ));
      state.year := Year_Number'Value( Keyed_Text_Buffer.Get( url, "year" ));
      state.phase := Phase_Type'Value( Keyed_Text_Buffer.Get( url, "phase" ));
      state.health := Health_Type'Value( Keyed_Text_Buffer.Get( url, "health" ));
      state.error_code := Integer'Value( Keyed_Text_Buffer.Get( url, "error_code" ));
      Put_Line( "Load_State_From_URL go" );
               --state.message := XX.Value( Keyed_Text_Buffer.Get( url, "message" ));
      state.pid := Integer'Value( Keyed_Text_Buffer.Get( url, "pid" ));
      state.exit_state := Integer'Value( Keyed_Text_Buffer.Get( url, "exit_state" ));
      state.monitor_read_error := Boolean'Value( Keyed_Text_Buffer.Get( url, "monitor_read_error" ));
      state.start_time := Ada.Calendar.Formatting.Value( AWS.URL.Decode( Keyed_Text_Buffer.Get( url, "start_time" )));
      state.execution_time := Duration'Value( Keyed_Text_Buffer.Get( url, "execution_time" ));
      return state;      
   end Load_State_From_URL;
   
   function State_To_Parameters( state : State_Rec ) return Unbounded_String is
   use Ada.Text_IO;
         s : Unbounded_String := Null_Unbounded_String;
   begin
      Put_Line( "State_To_Parameters entered" );
      s := s &       
         "queued=" & Trim( state.queued'Img ) & "&" &
         "household=" & Trim( state.household'Img ) & "&" &
         "module=" & Trim( state.module'Img ) & "&" &
         "other_counter=" & Trim( state.other_counter'Img ) & "&" &
         "year=" & Trim( state.year'Img ) & "&" &
         "phase=" & Trim( state.phase'Img ) & "&" &
         "health=" & Trim( state.health'Img ) & "&" &
         "error_code=" & Trim( state.error_code'Img ) & "&";
      Put_Line( "State_To_Parameters 1/2 way" );
      s := s &       
         -- "message=" & Trim( state.message'Img ) & "&" &
         "pid=" & Trim( state.pid'Img ) & "&" &
         "exit_state=" & Trim( state.exit_state'Img ) & "&" &
         "monitor_read_error=" & Trim( state.monitor_read_error'Img ) & "&";
      Put_Line( "State_To_Parameters 2/3s way" );
      s := s &       
         "start_time=" & AWS.URL.Encode( Trim( Ada.Calendar.Formatting.Image( state.start_time )))  & "&" &
         "execution_time=" & Trim( state.execution_time'Img );
      Put_Line( "State_To_Parameters full way" );
      return s;
   end State_To_Parameters;

   
end EU.BE.Model.Settings;
