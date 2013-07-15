------------------------------------------------------------------------------
--                                                                          --
--  Handlers for each of OSCR's callbacks, plus some support functions      --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
--                                                                          --
with Ada.Containers;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Text_IO;

with AWS.Config;
-- with AWS.Log;
with AWS.Messages;
with AWS.Mime;
with AWS.Resources;
with AWS.Resources;
with AWS.Response.Set;
with AWS.Response;
with AWS.Server;
with AWS.Cookie;

with EU.BE.Model.Runner.Web;
with EU.Web.Settings;
with EU.BE.Model.Runner;

with GNAT.Regexp;

with BE_Base_Model_Types;
with Html_Utils;
with T_Utils.Web_IO;
with Tabulator_Commons;
with Text_Utils;
with Utils;
with Web_Utils;


pragma Elaborate_All( EU.BE.Model.Runner.Web );

package body Callbacks is

   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Strings.Unbounded;
   
   
   procedure Log( s : String ) is
   begin
      EU_Logger.Log( EU_Logger.callbacks, s );
   end Log;
   
   -- log :  AWS.Log.Object        renames globals.logger;
   
   function Serve_Static_Resource( Request : in AWS.Status.Data ) return AWS.Response.Data is
      use Ada.Strings;
      WWW_Root : constant String := AWS.Config.WWW_Root( AWS.Server.Config( AWS.Server.Get_Current.all ));
      URI      : constant String := AWS.Status.URI( Request );
      root     : constant String := EU.Web.Settings.Mefisto_Root;      
      filename : constant String := WWW_Root & "mefisto" & URI ( root'Last .. URI'Last);
      -- filename :  constant String := WWW_Root & root & URI;
   begin
      Log(  "serving |" & filename & "| URI was |" & URI & "| root |" & root & "| WWW_Root |" & WWW_Root & "|" );
      if AWS.Resources.Is_Regular_File( filename ) then
         return AWS.Response.File( 
            Content_Type => AWS.MIME.Content_Type( filename ),
            Filename     => filename );
      else          
         return AWS.Response.Acknowledge
              (AWS.Messages.S404,
               "<p>The page '"
               --  Replace HTML control characters to the HTML inactive symbols
               --  to avoid correct HTML pages initiated from the client side.
               --  See http://www.securityfocus.com/bid/7596
               & Fixed.Translate (URI, Maps.To_Mapping ("<>&", "{}@"))
               & "' was not found.");
      end if;         
   end Serve_Static_Resource;
   
   function Supports_Cookies( request : in AWS.Status.Data ) return Boolean is
   begin
      return True;
   end Supports_Cookies;
   
   
   function Cookie_Test( request : in AWS.Status.Data )  return AWS.Response.Data is
      d : AWS.Response.Data;
   begin
      if( AWS.Cookie.Exists( request, "tc01", False ))then
         return AWS.Response.URL( Location => EU.Web.Settings.Mefisto_Root & "?cookie=True" );
      else 
         return AWS.Response.URL( Location => EU.Web.Settings.Mefisto_Root & "?cookie=False" );
      end if;      
   end Cookie_Test;


   function Handle_Login( request : in AWS.Status.Data ) return users.Login_Result is
      use EU.BE.Users;
      use EU.BE.Users.IO;
      use AWS.Session;
      package cookies renames AWS.Cookie;
      session_id : AWS.Session.Id := AWS.Status.Session( request );
      this_user  : users.User_Type;
      result     : users.Login_Result;
      root       : constant String := EU.Web.Settings.Mefisto_Root;
      params     : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      cookie_string : constant String := AWS.Parameters.Get( params, "cookie" );
   begin
      if( not AWS.Cookie.Exists( request, "tc01", False ))then
         Log( "cookie not found" );
         Log( "cookie string is " & cookie_string );
         if( cookie_string'Last > 0 )then
            if( Boolean'Value( cookie_string ))then
               result.cookie_support := yes;
            else
               result.cookie_support := no;
            end if;
         else
            result.cookie_support := unknown;
         end if;  
      else
         Log( "cookie found" );
         result.cookie_support := yes;
      end if;
      if( result.cookie_support = yes )then
   
         if( not Exist( session_id ) or ( session_id = No_Session ))then
            session_id := Create;
            Log( "creating new session " & Image( session_id ));
         end if;
         
         --
         -- cookie handling
         --
         if( cookies.Exists( request, "authentication" ))then
            declare
               auth_content : constant String := cookies.Get( request, "authentication" );
            begin
               Log( "got auth content as " & auth_content );
            end;
         end if;
         
         this_user := users.User_Session_Data.Get( session_id, globals.SESSION_USER_ID );
         if( this_user.utype = invalid )then
            --
            --  no user in the session; put the user record in the session
            --
            this_user.username := Censor_String( "tmp_user_" & TuS( Image( session_id )));
            this_user.description := TuS( "Temporary User with ID " &  Image( session_id ) & " Created on " ) & Utils.Now_As_String;
            this_user.title := TuS( "Temporary User" );
            this_user.utype := Users.anon;
            users.User_Session_Data.Set( session_id, globals.SESSION_USER_ID, this_user );
            declare
               run_state     : runsett.State_Rec := runsett.NULL_STATE_REC;
               run_settings  : runsett.Model_Settings := globals.Get_Default_Model_Settings.Copy;
               output        : outp.Outputs_Rec;
               param_buffer  : Param_Buff := globals.Get_Loaded_Input_Buffer( result.user.lang );
               prev_runs     : EU.BE.Model.Runner.Run_Results_List;
            begin
               Create_User_Files( run_settings.Working_Root, EU.Web.Settings.Dir_Separator, this_user );
               run_settings.Set_Users_Directory( this_user.Username );
               run_settings.Set_Dir_Separator( EU.Web.Settings.Dir_Separator( 1 ));
               prev_runs     := EU.BE.Model.Runner.Get_Previous_Runs( run_settings );
   
               Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, run_state );
               Run_Settings_Session.Set( session_id, globals.SESSION_RUN_SETTINGS, run_settings );
               Output_Session.Set( session_id, globals.SESSION_OUTPUTS, output );
               Buffer_Session.Set( session_id, globals.SESSION_PARAMETER_BUFFER, param_buffer );
               Previous_Runs_Session.Set( session_id, globals.SESSION_PREV_RUNS, prev_runs );
            end;
            result.new_session := True;
            result.response := AWS.Response.URL( Location => root );
         end if;
         users.User_Session_Data.Set( session_id, globals.SESSION_USER_ID, this_user );
         result.user := this_user;
         result.validated := True;
      end if;
      return result;
   end Handle_Login;

   
   function Handle_Login_With_Authentication( request : in AWS.Status.Data ) return users.Login_Result is
      use type users.User_Type;

      username   : constant Unbounded_String := TuS( AWS.Status.Authorization_Name( request ));
      password   : constant Unbounded_String := TuS( AWS.Status.Authorization_Password( request ));
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
      this_user  : users.User_Type;
      result     : users.Login_Result;
      root       : constant String := EU.Web.Settings.Mefisto_Root;

   begin
      if( username = TuS( "" ))then
          result.response := AWS.Response.Authenticate( globals.AUTHENTICATION_DOMAIN, AWS.Response.Basic );
      end if;
      this_user := globals.Validate( username, password );
      if( this_user = users.INVALID_USER )then
          result.response := AWS.Response.Authenticate( globals.AUTHENTICATION_DOMAIN, AWS.Response.Basic );
      else
         result.user := this_user;
         result.validated := True;
         if( users.User_Session_Data.Get( session_id, globals.SESSION_USER_ID ) = users.INVALID_USER )then
            --
            --  no user in the session; put the user record in the session
            --
            users.User_Session_Data.Set( session_id, globals.SESSION_USER_ID, this_user );
            declare
               run_state     : runsett.State_Rec := runsett.NULL_STATE_REC;
               run_settings  : runsett.Model_Settings := globals.Get_Default_Model_Settings.Copy;
               output        : outp.Outputs_Rec;
               param_buffer  : Param_Buff := globals.Get_Loaded_Input_Buffer( result.user.lang );
               prev_runs     : EU.BE.Model.Runner.Run_Results_List;
            begin
               run_settings.Set_Users_Directory( result.user.Username );
               run_settings.Set_Dir_Separator( EU.Web.Settings.Dir_Separator( 1 ));
               Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, run_state );
               Run_Settings_Session.Set( session_id, globals.SESSION_RUN_SETTINGS, run_settings );
               Output_Session.Set( session_id, globals.SESSION_OUTPUTS, output );
               Buffer_Session.Set( session_id, globals.SESSION_PARAMETER_BUFFER, param_buffer );
               prev_runs     := EU.BE.Model.Runner.Get_Previous_Runs( run_settings );
               Previous_Runs_Session.Set( session_id, globals.SESSION_PREV_RUNS, prev_runs );
            end;
            result.new_session := True;
            result.response := AWS.Response.URL( Location => root );
         end if;
      end if;
      return result;
   end Handle_Login_With_Authentication;
   
   function Is_Job_Running( request : in AWS.Status.Data ) return Boolean is
   use runsett;
      session_id     : constant AWS.Session.Id := AWS.Status.Session( request );
      run_state      : runsett.State_Rec;
   begin
      run_state := Run_State_Session.Get(
         session_id,
         globals.SESSION_RUN_STATE );
      Log( "Is_Job_Running: got phase as " & 
           run_state.phase'Img & " queued as " & run_state.queued'Img & 
           "session id |" & AWS.Session.Image( session_id ) & "|" );
      if( run_state = NULL_STATE_REC )then
         return False;
      end if;
      if( run_state.queued )then
         return True;
      end if;
      if( run_state.phase = not_started or run_state.phase = mefisto_complete )then
         return False;
      end if;
      return True;
   end Is_Job_Running;
   
   function Get_Std_Translations( request : in AWS.Status.Data; user : users.User_Type ) return Templates_Parser.Translate_Set is
   use Templates_Parser;
   use EU.BE.I81N;
      URI    : constant String := AWS.Status.URI( Request );
      translations : Translate_Set;
   begin
      Insert( translations, Assoc( "LANG", EU.BE.I81N.Lang_Str( user.lang )));
      Insert( translations, Assoc( "TEMPLATE_ROOT",  EU.Web.Settings.template_components_path ));
      Insert( translations, Assoc( "SEP", EU.Web.Settings.Dir_Separator ));
      Insert( translations, Assoc( "ROOT", EU.Web.Settings.Mefisto_Root ));
      Insert( translations, Assoc( "USERNAME", user.title ));
      Insert( translations, Assoc( "USERID", user.username ));
      
      Insert( translations, Assoc( "RANDOM_STRING", Utils.Random_String ));
      Insert( translations, Assoc( "URI", URI ));
      Insert( translations, Assoc( "PAGE-TITLE", "" ));
      
      if( user.lang = en )then
      	 Insert( translations, Assoc( "LANG-CHOICE", "EN&nbsp;|&nbsp;<a href='lang/nl'>NL</a>" ));
      else
      	 Insert( translations, Assoc( "LANG-CHOICE", "<a href='lang/en'>EN</a>&nbsp;|&nbsp;NL" ));
         --  "<a href='lang/en'>EN</a>&nbsp;|&nbsp;NL"
      end if;
      
      -- if( user.lang = en )then
      	 -- Insert( translations, Assoc( "LANG-CHOICE", "EN&nbsp;|&nbsp;<a href='#' onclick='changeLanguageFromMyEnd( ""nl"");'>NL</a>" ));
      -- else
      	 -- Insert( translations, Assoc( "LANG-CHOICE", "<a href='#' onclick='changeLanguageFromMyEnd( ""en"");'>EN</a>&nbsp;|&nbsp;NL" ));
         -- --  "<a href='lang/en'>EN</a>&nbsp;|&nbsp;NL"
      -- end if;
      return translations;
   end Get_Std_Translations;

 
   function Logout_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
      d : AWS.Response.Data;
   begin
      AWS.Response.Set.Clear_Session( d );
      AWS.Session.Delete( session_id );
      return AWS.Response.URL( Location => EU.Web.Settings.Mefisto_Root);
   end Logout_Callback;

   function Popup_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use EU.BE.Users;
      logresult        : constant users.Login_Result := Handle_Login( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      translations : Translate_Set;
      params       : constant AWS.Parameters.List := AWS.Status.Parameters( Request );
      root         : constant String := EU.Web.Settings.Mefisto_Root;
   begin
      --
      -- fixme: this is overkill
      --
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      translations := Get_Std_Translations( request, logresult.user );
      return Web_Utils.Build_Input_Page(
         EU.Web.Settings.template_components_path & "popup",
         translations );

   end Popup_Callback;

   -- function Serve_File_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   -- use EU.BE.Users;
      -- filename : Unbounded_String;
      -- logresult : constant users.Login_Result := Handle_Login( request );
      -- session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      -- params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      -- extension        : constant String := AWS.Parameters.Get( params, "type", 1 ); -- FIXME: unused
      -- user : User_Type := User_Session_Data.Get( session_id, globals.SESSION_USER_ID ); 
   -- begin
      -- if(( not logresult.validated ) or ( logresult.new_session )) then
         -- return logresult.response;
      -- end if;
      -- if( user = INVALID_USER ) then
         -- return AWS.Response.URL( Location => EU.Web.Settings.Mefisto_Root );
      -- end if;
      -- return AWS.Response.File( AWS.Mime.Image_Png, TS( filename ));
   -- end Serve_File_Callback;
   
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      logresult        : users.Login_Result := Handle_Login( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
   
   use Templates_Parser;  
      URI                : constant String := AWS.Status.URI( Request );
      params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      translations : Translate_Set;
      path               : Unbounded_String_List := Split( URI, '/' );
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      Handle_Language( path, logresult.user, session_id );
      translations := Get_Std_Translations( request, logresult.user );
      return Web_Utils.Build_Input_Page(
         EU.Web.Settings.template_components_path & "input_run_settings",
         translations );
   end Run_Settings_Callback;

   procedure Handle_Language( 
      path       : in out  Unbounded_String_List; 
      user       : in out  users.User_Type;
      session_id : in AWS.Session.Id ) is
   use EU.BE.I81N;
   use Ada.Containers;
      Path_Length  : constant Natural := Natural( path.Length );
      n            : Natural;
   begin
      if( path_length > 2 )then
         if( path.Element( path_length - 1 ) = TuS( "lang" ))then
            if( path.Element( path_length ) = TuS( "en" ))then
               user.lang := en;
            elsif( path.Element( path_length ) = TuS( "nl" ))then
               user.lang := nl;
            end if;
            users.User_Session_Data.Set( session_id, globals.SESSION_USER_ID, user );
            -- if you change > 1 time on the same page there can be multiple
            -- occurrencies of lang/en|nl, so delete then all
            for i in 1 .. Path_Length loop
               if( path.Element( i ) = TuS( "lang" ))then
                  n := i;
                  exit;
               end if;
            end loop;
            globals.Update_User( user );
            path.Delete( n, Count_Type( Path_Length - n + 1 )); -- clean out the language request(s)
         end if;
      end if;
   end Handle_Language;


end Callbacks;
