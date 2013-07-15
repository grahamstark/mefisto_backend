--
--
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Config.Set; 
with AWS.Config; 
with AWS.Dispatchers.Callback;
with AWS.Mime;
with AWS.Server.Log;
with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Page_Server;
with AWS.Services;

with Callbacks.Mefisto;
with Callbacks;
with EU.BE.Globals;
with EU.BE.Parameter_System_Declarations;

with EU.BE.Model.Settings;
with EU.Web.Settings;
with EU_Logger;

with Text_Utils;
with Web_Utils;

procedure Mefisto_Server is
   
   use AWS.Services;
   use AWS.Config;
   use AWS.Config.Set;
   use Ada.Text_IO;
   package globals renames EU.BE.Globals;
   
   my_server     : AWS.Server.HTTP;
   my_dispatcher : AWS.Services.Dispatchers.URI.Handler;
   my_config     : AWS.Config.Object := AWS.Config.Get_Current;
   
   package awsc renames AWS.Config; 
   
   SERVER_SCALE : constant := 2; -- this halfs all the server sizes compared to the default
   
   use Text_Utils;
   use Ada.Strings.Unbounded;
    
   default_handler : AWS.Dispatchers.Callback.Handler;

   STATIC_FILE_REGEP : constant String :=  
                                   ".*\.css|" & 
                                   ".*\.js|" &
                                   ".*\.png|" & 
                                   ".*\.html|" & 
                                   ".*\.gif|" &
                                   ".*\.pdf|" &
                                   ".*\.zip";
begin
   Put_Line( "server entered; EU.BE.Globals.Initialise_Settings_And_Datasets" );
   EU.BE.Globals.Initialise_Settings_And_Datasets(
      include_base_results => True,
      load_household_data  => True ); 
   declare
      MEFISTO_ROOT : constant String := EU.Web.Settings.Mefisto_Root;
      SEP : constant String := EU.Web.Settings.Dir_Separator;
   begin      
      -- add a mime type for SVG
      Put_Line( "main section; MEFISTO_ROOT |" & MEFISTO_ROOT );
      AWS.Mime.Add_Extension( "svg", globals.MIME_TYPE_IMAGE_SVG );
   
      default_handler := AWS.Dispatchers.Callback.Create( Callbacks.Mefisto.Index_Page_Callback'Access );
      Dispatchers.URI.Register_Default_Callback( my_dispatcher, default_handler );
      
      Ada.Text_IO.Put_Line
         ("Call me on port" &
            Positive'Image( EU.Web.Settings.port ) & "; serving web root |" & MEFISTO_ROOT &
            "| press ctl-break to stop me ...");
      awsc.Set.Server_Name( my_config, "Mefisto/Motiff Server" );
      awsc.Set.Server_Port( my_config, EU.Web.Settings.port );
      awsc.Set.WWW_Root( my_config, EU.Web.Settings.Physical_Root & "web" );
      awsc.Set.Session( my_config, True );
      awsc.Set.Session_Lifetime( Duration( 720_000 / SERVER_SCALE ));
      awsc.Set.Max_Connection( my_config,  100 / SERVER_SCALE );
      awsc.Set.Accept_Queue_Size( my_config, 120 / SERVER_SCALE );
      awsc.Set.Free_Slots_Keep_Alive_Limit(my_config,  80 / SERVER_SCALE );
      
      -- Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT, Callbacks.Mefisto.Index_Page_Callback'Access );
      
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "progress.*",      Callbacks.Mefisto.Run_Progress_Callback'Access );   
       -- Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "save_file.*",     Callbacks.Serve_File_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "logout.*",        Callbacks.Logout_Callback'Access );
      
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "download_run",    Callbacks.Mefisto.Download_Run_Callback'Access );
      
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "output_page.*",   Callbacks.Mefisto.Output_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "light_page.*",    Callbacks.Mefisto.Light_Page_Callback'Access );
      
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "run_settings.*",  Callbacks.Mefisto.Run_Settings_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "parameters_page.*",  Callbacks.Mefisto.Parameter_Page_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "array_update.*",  Callbacks.Mefisto.Array_Update_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "example.*",       Callbacks.Mefisto.Example_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, MEFISTO_ROOT & "cookietest.*",    Callbacks.Cookie_Test'Access );
   
      Dispatchers.URI.Register_Regexp( my_dispatcher, STATIC_FILE_REGEP,
                                      Callbacks.Serve_Static_Resource'Access );
      
   end;
   Put_Line( "starting logger" ); 
   AWS.Server.Log.Start( 
      Web_Server => my_server,
      Auto_Flush => True );
   Put_Line( "starting server" );
   AWS.Server.Start( 
      my_server,
      Dispatcher => my_dispatcher,
      Config     => my_config );
   Put_Line( "server waiting" );  
   AWS.Server.Wait( AWS.Server.forever );
   
   AWS.Server.Shutdown( my_server );
   
end Mefisto_Server;
