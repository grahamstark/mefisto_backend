with Ada.Command_Line;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;

with EU.BE.Household.IO;
with EU.BE.Results.IO;
with EU.BE.Users.IO;
with EU.BE.Model.Settings;
with EU.BE.Model.Runner;
with EU_Logger;

with Templates_Parser;
with Web_Utils;
with XML_Utils;
with Time_Format;

package body EU.BE.Globals is
  
   use Ada.Text_IO;
   use Text_Utils;
   
   package ms renames EU.BE.Model.Settings;
   
   BE_PARAMETERS                : BE_Parameter_System.Parameter_System_Rec;
   DEFAULT_PARAMS               : Keyed_Text_Buffer.Text_Buffer;
   SEP                          : Character;
   default_sett                 : ms.Model_Settings;
   hh_level_list_pre            : Detailed_Results_List;
   households                   : EU_Data_Set;
   personal_level_results_pre   : Detailed_Results_List; 
   text_params_loaded           : Boolean := False; 
   xml_params_loaded            : Boolean := False;
   users                        : EU.BE.Users.User_Maps.Map;
    
   procedure Create_Parameter_Files( ctl : in Model_Settings;  buffer : in BE_Buffer ) is
      use Ada.Directories;
      use Templates_Parser;
      use Text_Utils;

      translations : Translate_Set := BE_Renderer.Create_Euromod_Parameter_Mappings( buffer );

      procedure Create_Param_File( directory_entry : Directory_Entry_Type ) is
         s : Unbounded_String;
         filename    : constant String := Full_Name( directory_entry );
         directory   : constant String := Containing_Directory( filename );
         basename    : constant String := Base_Name( filename );
         ext         : constant String := Extension( filename );
         output_file : constant String := ctl.Qualified_Parameter_Directory & SEP & basename & ".txt";
         output      : constant String := Replace_In_String( Parse( filename, translations ), ",", TAB );
         
      begin
         Put_Line( "file name |" & filename & " directory " & directory & " base name " & basename & " ext " & ext & " output file = " & output_file & " output = |" & output & "|" );
         s := TuS( output );
         Write_Whole_File( output_file, s ); 
      exception
         when Ada.IO_Exceptions.Name_Error => null;
      end Create_Param_File;
      
      state : State_Rec;
      PARAM_TEMPLATE_DIR_NAME : constant String := EU.Web.Settings.Physical_Root & SEP & "etc" & SEP & "parameters" & SEP;
   begin
      Insert( translations, Assoc( "LOG-FILE-NAME", ctl.Qualified_Log_File_Name ));
      Insert( translations, Assoc( "LOG-WARNINGS", ctl.Log_Warnings ));
      Insert( translations, Assoc( "EUROMOD-VERSION", ctl.Euromod_Version ));
      Insert( translations, Assoc( "OUTPUT-DIRECTORY", ctl.Qualified_Output_Directory ));
      
      Insert( translations, Assoc( "PARAMETER-DIRECTORY", ctl.Qualified_Parameter_Directory ));
      Insert( translations, Assoc( "DATAFILE-DIRECTORY", ctl.Datafile_Directory ));
      Insert( translations, Assoc( "DATASET-NAME", ctl.Dataset_Name ));
      Insert( translations, Assoc( "HEADER-DATE", Censor_String( Ada.Calendar.Formatting.Image( Ada.Calendar.Clock )))); 
      -- hack: the censor string above gets rid of stray characters in the date string that 
      if( ctl.Log_Runtime )then
         Insert( translations, Assoc( "LOG-RUNTIME" , "yes" ));
      else
         Insert( translations, Assoc( "LOG-RUNTIME" , "no" ));
      end if;
      
      if( ctl.Log_Warnings )then
         Insert( translations, Assoc( "LOG-WARNINGS", "yes" ));
      else
         Insert( translations, Assoc( "LOG-WARNINGS", "no" ));
      end if;
      Insert( translations, Assoc( "CONTROL-FILE-NAME", ctl.control_file_name ));
      Insert( translations, Assoc( "SPINE-FILE-NAME", ctl.Spine_File_Name ));
      Insert( translations, Assoc( "SYSTEM-NAME", ctl.System_Name ));
      Web_Utils.Dump_Translations( translations );
      state.phase := starting;
      state.health := queued;
      state.queued := True;
      EU.BE.Model.Runner.Write_EM_Monitor_File( ctl.Qualified_Monitor_File_Name, state  );
      Put_Line( "Create_Parameter_Files; Looking in |" & PARAM_TEMPLATE_DIR_NAME & "| for csv files " );
      Search( PARAM_TEMPLATE_DIR_NAME, "*.csv", ( Ordinary_File=>True, others => False ), Create_Param_File'Access );     
   end Create_Parameter_Files;

   
   function Get_Personal_Level_Results_Pre return Detailed_Results_List is
   begin
      if( Natural( hh_level_list_pre.Length ) = 0 )then
         EU.BE.Results.IO.Read_To_List( 
            default_sett.Qualified_Output_File_Name, 
            personal_level_results_pre, 
            hh_level_list_pre );
      end if;
      return personal_level_results_pre;
   end Get_Personal_Level_Results_Pre;
   
   function Get_HH_Level_List_Pre return Detailed_Results_List is
   begin
      if( Natural( hh_level_list_pre.Length ) = 0 )then
         EU.BE.Results.IO.Read_To_List( 
            default_sett.Qualified_Output_File_Name, 
            personal_level_results_pre, 
            hh_level_list_pre );
      end if;
      return hh_level_list_pre;       
   end Get_HH_Level_List_Pre;
   
   function Get_Households return EU_Data_Set is
   begin
      if( households.Number_Of_Households = 0 )then
         households := EU.BE.Household.IO.Load_Binary( 
            default_sett.Datafile_Directory & SEP & default_sett.Dataset_Name );      
      end if;
      return households;
   end Get_Households;
   
   function Get_Default_Model_Settings return Model_Settings is
   begin
      return default_sett;
   end Get_Default_Model_Settings;
   
   function Get_BE_Parameter_System return BE_Parameter_System.Parameter_System_Rec is
      use XML_Utils; -- domc
   begin
      if( not xml_params_loaded )then
         declare
            doc : domc.Document; 
            filename : constant String := EU.Web.Settings.XML_File;
         begin
            Put_Line( "Get_BE_Parameter_System: opening " & filename );
            doc := XML_Utils.Get_Doc( filename, False );
            BE_PARAMETERS := BE_Parameter_System_XML.Load( doc );
            xml_params_loaded := True;
         end;
      end if;
      return BE_PARAMETERS;
   end Get_BE_Parameter_System;
   
   function Get_Default_Params return Keyed_Text_Buffer.Text_Buffer is
      use Keyed_Text_Buffer;  
      filename : String := EU.Web.Settings.Physical_Root & SEP & "etc" & SEP & DEFAULT_PARAMETERS;
   begin
      if( not text_params_loaded )then
         Put_Line( "Get_Default_Paramss: opening " & filename );
         DEFAULT_PARAMS := Load( filename, single_line_delimited );
         text_params_loaded := True;   
      end if;
      return DEFAULT_PARAMS;
   end Get_Default_Params;
      
   function Get_Loaded_Input_Buffer( lang : Languages ) return BE_Buffer is
      use Text_Utils;
      
      params   : Keyed_Text_Buffer.Text_Buffer := Get_Default_Params;  
      sys_desc : BE_Parameter_System.Parameter_System_Rec := Get_BE_Parameter_System;
      buff     : BE_Buffer;
   begin
      buff := BE_Parameter_System_IO.Init( lang, sys_desc, params, params );
      buff.Set_Handler(
         TuS( "be.benefits.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.more_than_16_months_unemployed_20_years.payment_type" ),
         BE_Renderer.Switch_Payment_For_UB_1'Access,
         BE_Parameter_System_IO.on_change );
         
      return buff;
   end Get_Loaded_Input_Buffer;

   function Validate( 
      username : Unbounded_String; 
      password : Unbounded_String ) return EU.BE.Users.User_Type is
   begin
      return EU.BE.Users.Validate( users, username, password );
   end Validate;
   
   procedure Update_User( user : in out EU.BE.Users.User_Type ) is
   use Text_Utils;
   begin
      if( users.Contains( user.username ))then
         users.Replace( user.username, user );
      else
         users.Insert( user.username, user );
      end if;
      EU.BE.Users.IO.Create_User_Files(
         default_sett.Working_Root,
         EU.Web.Settings.Dir_Separator,
         user );
   end Update_User;
   
   procedure Initialise_Settings_And_Datasets( 
      include_base_results : Boolean;
      load_household_data  : Boolean ) is
   begin
      Put_Line( "Add_All_Targets globals entered" );
      if( Ada.Command_Line.Argument_Count > 1 ) then
         default_sett := Read_Model_Settings( Ada.Command_Line.Argument( 1 ));
         EU.Web.Settings.Read_Web_Settings( Ada.Command_Line.Argument( 2 ) );
      else
         default_sett := Read_Model_Settings( "etc/be_settings.txt" );
         EU.Web.Settings.Read_Web_Settings( "etc/web_settings.txt" );
      end if;
      Put_Line( "reading parameters" );
      BE_Parameter_System.Set_Delimiter( DELIMITER );
         
      sep := EU.Web.Settings.Dir_Separator(1);
      EU.BE.I81N.Load_Translations;
      default_sett.Set_Users_Directory( DEFAULT_USER );
      default_sett.Set_Run_Id( DEFAULT_RUN );
      default_sett.Set_Dir_Separator( EU.Web.Settings.Dir_Separator( 1 ));
      EU.BE.Users.IO.Load_Users( default_sett.Working_Root, users ); 
      
      Put_Line( "EU_Logger.Set_Output" );
      EU_Logger.Set_Output( 
         EU.Web.Settings.Log_File_Dir & 
         EU.Web.Settings.Dir_Separator & 
         "mefisto_server_log" );
      EU_Logger.Add_All_Targets;
      Put_Line( "Add_All_Targets to logger" );
      
      if( include_base_results )then
         Put_Line( "including base results " );
         EU.BE.Results.IO.Read_To_List( 
            default_sett.Qualified_Output_File_Name, 
            personal_level_results_pre, 
            hh_level_list_pre );
      end if;
      Put_Line( "loading household data" );
      if load_household_data then
         households := EU.BE.Household.IO.Load_Binary( 
            default_sett.Datafile_Directory & SEP & default_sett.Dataset_Name );
      end if;
      Put_Line( "initialisation complete" );
   end Initialise_Settings_And_Datasets;

end EU.BE.Globals;
