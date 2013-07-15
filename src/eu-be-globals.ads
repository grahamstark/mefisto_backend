with AWS.Session;
with AWS.Log;

with Ada.Strings.Unbounded;

with EU.BE.Household.IO;
with EU.BE.Model.Runner;
with EU.BE.I81N;
with EU.BE.Output;
with EU.BE.Results;
with EU.BE.Users;
with EU.BE.Light_Parameters;
with EU.BE.Model.Settings;
with EU.Web.Settings;

with BE_Base_Model_Types;
with HTML_Utils;
with Keyed_Text_Buffer;

with Parameter_System.Input_Buffer.EU_Renderer;
with Parameter_System.Input_Buffer.Utils;
with Parameter_System.Input_Buffer;
with Parameter_System.XML;
with Parameter_System;
with EU.BE.Parameter_System_Declarations;

-- 
-- 
-- This contains session variable definitions and code to retrieve default versions of
-- parameter systems for Mefisto.
-- 
package EU.BE.Globals is
   
   use EU.BE.Household.IO;
   use EU.BE.I81N;
   use EU.BE.Results;
   use EU.BE.Model.Settings;
   use Ada.Strings.Unbounded;
   use EU.BE.Parameter_System_Declarations;
   
   DELIMITER : constant Character := '.';
   
   DEFAULT_USER         : constant String := "default";
   DEFAULT_RUN          : constant String := "default";
   
   DEFAULT_PARAMETERS   : constant String := "be_mefisto.prm";
   
   -- mime type for svg see: /etc/mime.types, and AWS.Mime.ads
   MIME_TYPE_IMAGE_SVG : constant String := "image/svg+xml";
   MIME_TYPE_TEXT_CSV  : constant String := "text/comma-separated-values";

   --
   -- constants for identfiying stuff bound to a session
   --
   SESSION_USER             : constant String := "session-user";
   SESSION_LIGHT            : constant String := "session-light";
   SESSION_PARAMETER_BUFFER : constant String := "session-param-buffer";
   SESSION_RUN_STATE        : constant String := "session-run-state";
   SESSION_RUN_SETTINGS     : constant String := "session-run-settings";
   SESSION_OUTPUTS          : constant String := "session-outputs";
   SESSION_USER_ID          : constant String := "session-user-id";
   SESSION_PREV_RUNS        : constant String := "session-prev-runs";
   SESSION_ABORTING         : constant String := "session-aborting";
   
   AUTHENTICATION_DOMAIN    : constant String := "Mefisto/Motiff Models";
   
   package Output_Session_Data is new AWS.Session.Generic_Data(
      EU.BE.Output.Outputs_Rec,
      EU.BE.Output.Get_Null_Data
   );

   package BE_HTML_Utils is new HTML_Utils(
      Rate         => Rate, 
      Counter_Type => Counter_Type
   );
   --
   -- Bindings for AWS sessions
   --   
   package Buffer_Session_Package is new AWS.Session.Generic_Data(
      BE_Buffer,
      BE_Parameter_System_IO.Get_Null_Buffer );
      
   package Run_Settings_Session_Package is new AWS.Session.Generic_Data(
      EU.BE.Model.Settings.Model_Settings,
      EU.BE.Model.Settings.Get_Null_Model_Settings );
      
   package Run_State_Session_Package is new AWS.Session.Generic_Data(
      EU.BE.Model.Settings.State_Rec,
      EU.BE.Model.Settings.NULL_STATE_REC );
      
   package Output_Session_Package is new AWS.Session.Generic_Data(
      EU.BE.Output.Outputs_Rec,
      EU.BE.Output.Get_Null_Data );  
   
   package Previous_Runs_Session_Package is new AWS.Session.Generic_Data(
      EU.BE.Model.Runner.Run_Results_List,
      EU.BE.Model.Runner.NO_RUN_RESULTS );
   
   package Light_Params_Session_Package is new AWS.Session.Generic_Data(
      EU.BE.Light_Parameters.Light_Params,
      EU.BE.Light_Parameters.MISSING_PARAMS );
   
   --
   -- Default data and results 
   --
   function Get_Default_Model_Settings return Model_Settings;
   
   function Get_HH_Level_List_Pre return Detailed_Results_List;
   
   function Get_Personal_Level_Results_Pre return Detailed_Results_List;
   
   function Get_Households return EU_Data_Set;
   
   function Validate( 
      username : Unbounded_String; 
      password : Unbounded_String ) return EU.BE.Users.User_Type;
   
   procedure Update_User( user : in out EU.BE.Users.User_Type );
      
   function Get_Default_Params return Keyed_Text_Buffer.Text_Buffer;
   
   function Get_Loaded_Input_Buffer( lang : Languages ) return BE_Buffer;
   
   function Get_BE_Parameter_System return BE_Parameter_System.Parameter_System_Rec;

   procedure Create_Parameter_Files( 
      ctl : in Model_Settings;
      buffer : in BE_Buffer ); 
   
   procedure Initialise_Settings_And_Datasets( 
      include_base_results : Boolean;
      load_household_data  : Boolean );
   --
   -- shared global logger object
   --
   -- logger : AWS.Log.Object;
   


end EU.BE.Globals;
