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
with Ada.Characters.Handling;
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

with Templates_Parser;

with GNAT.Regexp;

with BE_Base_Model_Types;
with Html_Utils;
with T_Utils.Web_IO;
with Tabulator_Commons;
with Text_Utils;
with Utils;
with Web_Utils;
with Parameter_System;

with EU.BE.Globals;
with EU.BE.Parameter_System_Declarations;
with EU.BE.Main_Menu;
with EU.BE.Output;
with EU.BE.Output.Web_IO;
with EU.BE.Users.IO;
with EU.BE.Users;
with EU.BE.Model.Settings;
with EU.BE.Model.Runner;
with EU.BE.Model.Runner.Web;
with EU.BE.Results;
with EU.BE.Results.Web_IO;
with EU.BE.Household;
with EU.BE.Household.Web_IO;
with EU.BE.Light_Parameters;
with EU.Web.Settings;
with EU.BE.I81N;

with EU_Logger;

pragma Elaborate_All( EU.BE.Model.Runner.Web );

package body Callbacks.Mefisto is

   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Strings.Unbounded;
   
   function Get_Help( request : in AWS.Status.Data ) return AWS.Response.Data is
      html : Unbounded_String;
   begin     
      return AWS.Response.Build( "text/html", html );   
   end Get_Help;
   
      
   function Get_And_Save_Runs_List( request : in AWS.Status.Data ) return EU.BE.Model.Runner.Run_Results_List is
   use EU.BE.Model.Runner;
      session_id     : constant AWS.Session.Id := AWS.Status.Session( request );
      run_settings  : constant runsett.Model_Settings := Run_Settings_Session.Get( session_id, globals.SESSION_RUN_SETTINGS );
      prev_runs     : Run_Results_List;
   begin
      prev_runs := EU.BE.Model.Runner.Get_Previous_Runs( run_settings );
      Previous_Runs_Session.Set( session_id, globals.SESSION_PREV_RUNS, prev_runs );
      return prev_runs;
   end Get_And_Save_Runs_List;
      
   
   function Dont_Show_Output( request : in AWS.Status.Data; user : users.User_Type ) return Boolean is
   use EU.BE.Users.IO;
   use EU.BE.Model.Runner;
      job_is_running  : constant Boolean := Is_Job_Running( request );
      session_id      : constant AWS.Session.Id := AWS.Status.Session( request );
      run_settings    : constant runsett.Model_Settings := Run_Settings_Session.Get( session_id, globals.SESSION_RUN_SETTINGS );
      SEP             : constant String := euws.Dir_Separator;
      prev_runs       : constant Run_Results_List := Get_And_Save_Runs_List( request );
      -- FIXME the above is VERY inefficient - 
      -- better to check the output in the session and the saved runlist, if any
      -- Previous_Runs_Session.Get( session_id, globals.SESSION_PREV_RUNS );
      run_count       : constant Natural := Natural( prev_runs.Length );
   begin
      Log( "Dont_Show_Output; job_is_running = " & 
         Boolean'Image( job_is_running ) & 
         " run_count " & Positive'Image( run_count ));
      return job_is_running or run_count = 0; -- i.e. no previous run if = 1   
   end Dont_Show_Output;
 
   function Run_Progress_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use runsett;
   use EU.BE.Model.Runner;
      session_id   : constant AWS.Session.Id := AWS.Status.Session( request );
      state_string : Unbounded_String;
      logresult    : users.Login_Result := Handle_Login( request );
      run_state    : State_Rec := Run_State_Session.Get( 
         session_id, 
         globals.SESSION_RUN_STATE );  
      run_settings : constant Model_Settings := Run_Settings_Session.Get( 
         session_id, 
         globals.SESSION_RUN_SETTINGS );
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      if( run_state.phase < mefisto_complete )then
         Log( "getting monitor" );
         Monitor( run_settings, run_state );
         -- FIXME is this needed?
         if( run_state.monitor_read_error ) then
             delay 0.1;
             Monitor( run_settings, run_state );            
         end if;
      end if;
      Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, run_state );  
      state_string := web_runner.Get_State_Of_Run_As_HTML( run_state, logresult.user.lang );
      return AWS.Response.Build( "text/html", state_string );
   end Run_Progress_Callback;
   
   
   function Index_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use EU.BE.Main_Menu;
   use EU.BE.I81N.Translations;
      --
      -- FIXME not actually used
      -- copied from OSCR
      --
      function Extract_Action( params : AWS.Parameters.List ) return String is
      use globals.BE_Html_Utils;
      begin
         if( Contains_Value( params, "Edit" ) ) then 
            return "Edit";
         end if;
         if( Contains_Value( params, "Copy" ) ) then 
            return "Copy";
         end if;
         if( Contains_Value( params, "Delete" ) ) then 
            return "Delete";
         end if;
         return "";
      end Extract_Action;
      
      URI            : constant String := AWS.Status.URI( Request );
      params         : constant AWS.Parameters.List := AWS.Status.Parameters( request );
      translations   : Translate_Set;
      logresult      : users.Login_Result := Handle_Login( request );
      session_id     : constant AWS.Session.Id := AWS.Status.Session( request );
      root           : constant String := euws.Mefisto_Root;
      path           : Unbounded_String_List := Split( URI, '/' );
      disable_output : Boolean;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      disable_output := Dont_Show_Output( request, logresult.user ); 
      Handle_Language( path, logresult.user, session_id );
      translations := Get_Std_Translations( request, logresult.user ); 
      Insert( translations, Assoc( "INTRO-TEXT", Lookup( "front_page", logresult.user.lang )));
      Insert( translations, Assoc( "MODEL-MENU", Get_Main_Menu( home_page, disable_output, logresult.user.lang )));
      return Web_Utils.Build_Input_Page(
         euws.template_components_path & "index",
         translations );

   end Index_Page_Callback;
   
   function Download_Run_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      session_id         : constant AWS.Session.Id := AWS.Status.Session( request );
      run_settings       : runsett.Model_Settings:= Run_Settings_Session.Get( session_id, globals.SESSION_RUN_SETTINGS );
      zip_name           : String := run_settings.Qualified_Output_Directory & "run_output.zip";
   begin
      Log( "Download_Run_Callback found; serving file |" & zip_name & "| " );
      return AWS.Response.File( 
            Content_Type  => AWS.MIME.Application_Zip,
            Cache_Control => AWS.Messages.Prevent_Cache,
            Filename      => zip_name,
            User_Filename => "run_output.zip",
            Disposition   => AWS.Response.Attachment );
   end Download_Run_Callback;
   
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      logresult        : users.Login_Result := Handle_Login( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
   
   use Templates_Parser;  
      params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      translations : Translate_Set;
   begin
      -- Handle_Language( path, logresult.user , session_id);
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      translations := Get_Std_Translations( request, logresult.user );
      return Web_Utils.Build_Input_Page(
         euws.template_components_path & "input_run_settings",
         translations );
   end Run_Settings_Callback;

   function Example_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use EU.BE.Results;
   use EU.BE.Results.Web_IO;
   use EU.BE.Household;
   use EU.BE.Household.Web_IO;
   use EU.BE.Output;
   use EU.BE.I81N.Translations;
      URI              : constant String := AWS.Status.URI( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      logresult        : users.Login_Result := Handle_Login( request );
      hh               : Household_Rec;
      res1             : Detailed_Record;
      res2             : Detailed_Record;
      diff             : Detailed_Record;
      translations     : Translate_Set;
      path             : Unbounded_String_List := Split( URI, '/' );
      hhref            : Natural;
      output           : Outputs_Rec := Output_Session.Get( session_id, globals.SESSION_OUTPUTS );
      pre_trans        : Translate_Set;
      post_trans       : Translate_Set;
      diff_trans       : Translate_Set;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      translations := Get_Std_Translations( request, logresult.user );
      path.Delete( 1, 2 );
      hhref := Natural'Value( TS( path.Element( 1 )));
      hh := globals.Get_Households.Read_Household( hhref );
      res1 := globals.Get_HH_Level_List_Pre.Element( hhref );
      res2 := output.Get_Detailed_Output( hhref );
      diff := Difference( res1, res2 );

      Insert( translations, Assoc( "IS-EXAMPLE-PAGE", True ));
      
      pre_trans := To_Translate_Set( res1, logresult.user.lang, "-PRE" );
      post_trans := To_Translate_Set( res2, logresult.user.lang, "-POST" );
      diff_trans := To_Translate_Set( diff, logresult.user.lang, "-DIFF" );
      Insert( pre_trans, post_trans );
      Insert( pre_trans, diff_trans );
      
      Insert( pre_trans, Assoc( "NETINCOME", Lookup( "Net Income", logresult.user.lang )));
      Insert( pre_trans, Assoc( "INCOME", Lookup( "Incomes", logresult.user.lang )));
      Insert( pre_trans, Assoc( "PENSION", Lookup( "Pensions", logresult.user.lang )));
      Insert( pre_trans, Assoc( "BENEFITS", Lookup( "Benefits", logresult.user.lang )));
      Insert( pre_trans, Assoc( "TAX", Lookup( "Tax", logresult.user.lang )));
      Insert( pre_trans, Assoc( "EXPENDITURE", Lookup( "Expenditures", logresult.user.lang )));
      Insert( pre_trans, Assoc( "LABOURMARKET", Lookup( "Labour Market", logresult.user.lang )));
      Insert( pre_trans, Assoc( "IDENTIFIERS", Lookup( "Identifiers", logresult.user.lang )));
      Insert( pre_trans, Assoc( "DEMOGRAPHICS", Lookup( "Demographics", logresult.user.lang )));
      
      Insert( translations, Assoc( "HOUSEHOLD", To_HTML( hh, logresult.user.lang )));
      Insert( translations, Assoc( "RESULTS",
        Web_Utils.Parse_Template( 
         euws.template_components_path & "detailed_comparative_output", pre_trans )));
       
      return Web_Utils.Build_Input_Page(
         euws.template_components_path & "example_page",
         translations );
      
   end Example_Callback;
   
   function Output_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use EU.BE.Output.Web_IO;
   use EU.BE.Output;
   use EU.BE.Results;
   use globals.BE_HTML_Utils;
   use Tabulator_Commons;
   use EU.BE.I81N.Translations;
   
   type Popup_Page_Type is ( no_popup, gallery_popup, single_chart_popup );
   
      function Get_Which_Popup( path : Unbounded_String_List ) return Popup_Page_Type is
         popup : Popup_Page_Type := no_popup;
         ps   : Unbounded_String;
         path_length : constant Integer := Integer( path.Length );
      begin
         -- FIXME this is a horrible mess
         if( path_length >= 2 )then
            ps := path.Element( 2 );
            Log( "got path element 2 as " & TS( ps ));
            if( ps = TuS( "gallery_popup" ))then
               popup := gallery_popup;
            elsif( ps = TuS( "single_chart_popup" ))then
                   popup := single_chart_popup;
            end if;
            -- might be "gallery_popip/single_chart_popup"
            if( path_length >= 3 )then
                ps := path.Element( 3 );
                Log( "got path element 3 as " & TS( ps ));
                if( ps = TuS( "single_chart_popup" ))then
                   popup := single_chart_popup;
               end if;
            end if;
         end if;
         Log( "got popup as " & Popup_Page_Type'Image( popup ));
         return popup;
      end Get_Which_Popup;

      function Get_Which_Page( path : Unbounded_String_List ) return Output_Page_Type is
         page : Output_Page_Type := summary_page;
         ps   : Unbounded_String;
      begin
         if( Integer( path.Length ) < 1 )then
            page := summary_page;
         else
            ps := path.Element( 1 );
            if( ps = TuS( "summary_page" ))then
               page := summary_page;
            elsif( ps = TuS( "gain_lose_page" ))then
               page := gain_lose_page;
            elsif( ps = TuS( "budget_page" ))then
               page := budget_page;
            elsif( ps = TuS( "inequality_page" ))then
               page := inequality_page;   
            elsif( ps = TuS( "poverty_page" ))then
               page := poverty_page;   
            end if; 
         end if;
         return page;
      end Get_Which_Page;
   
      URI              : constant String := AWS.Status.URI( request );
      params           : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      logresult        : users.Login_Result := Handle_Login( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      run_settings     : constant runsett.Model_Settings := Run_Settings_Session.Get( session_id, globals.SESSION_RUN_SETTINGS );
      filename         : Unbounded_String;
      translations     : Translate_Set;
      output           : Outputs_Rec;
      table            : Unbounded_String;
      gallery          : Unbounded_String;
      controls         : Unbounded_String;
      html             : Unbounded_String := TuS( "NOT IMPLEMENTED YET" );
      which_page       : Output_Page_Type;
      path             : Unbounded_String_List := Split( URI, '/' );
      which_popup      : Popup_Page_Type;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      
      output := Output_Session.Get( session_id, globals.SESSION_OUTPUTS );
      
      if( not output.Is_Initialised )then
         declare
            use EU.BE.Model.Runner;
            run_list : Run_Results_List := Get_And_Save_Runs_List( request );
            last_run_id : Natural := Last_Successful_Run( run_list );
            last_run_details : Run_Results_Record;
            local_run_settings : runsett.Model_Settings := run_settings.Copy;
         begin
            if( last_run_id > 0 )then
               last_run_details := run_list.Element( last_run_id );
               local_run_settings.Set_Run_Id( last_run_details.run_number );
               output :=  Load_Output( local_run_settings );
               Output_Session.Set( session_id, globals.SESSION_OUTPUTS, output );
            end if;
         end;
      end if;
      
      Handle_Language( path, logresult.user, session_id );
      translations := Get_Std_Translations( request, logresult.user );
      path.Delete( 1, 2 ); -- strip "mefisto" and "output_page" from the url
      
      which_page := Get_Which_Page( path );
      which_popup := Get_Which_Popup( path );
      if( output = EU.BE.Output.Get_Null_Data )then -- FIXME: any completed run
         html := Get_Output_Page(      
                     translations    => translations,
                     which_page      => which_page,
                     control_section => Null_Unbounded_String,
                     gallery         => Null_Unbounded_String,
                     content         => TuS( Lookup( "You have no completed model runs as yet", logresult.user.lang )),
                     lang            => logresult.user.lang );
      else
         case which_page is
         when summary_page =>
            gallery := Null_Unbounded_String;
            controls :=  Null_Unbounded_String;
            table :=  Get_Summary_Table( output, logresult.user.lang );
            html := Get_Output_Page(      
                        translations    => translations,
                        which_page      => which_page,
                        control_section => controls,
                        gallery         => gallery,
                        content         => table,
                        lang            => logresult.user.lang );
         when gain_lose_page  =>
            declare
               breakdown    : Breakdown_Target   := by_decile;
               comp_cell    : Compare_Cell       := current_cell;
               cell_op      : Cell_Compare_Type  := counter;
               value_To_Use : Summary_Items_Type := disposable_income;
            begin
               if( Contains_Key( params, "Redraw" ))then 
                  breakdown  := Breakdown_Target'Value( AWS.Parameters.Get( params, "breakdown", 1 ));
                  comp_cell  := Compare_Cell'Value(AWS.Parameters.Get( params, "comp_cell", 1 ));               
                  -- cell_op    := Cell_Compare_Type'Value(AWS.Parameters.Get( params, "cell_op", 1 ))               
               end if;
               table    := Get_Gain_Lose_Table( 
                  outputs   => output,
                  comp_cell => comp_cell,
                  breakdown => breakdown,
                  lang      => logresult.user.lang ); 
               controls := Get_Gain_Lose_Control_Section(
                  comp_cell => comp_cell,
                  breakdown => breakdown,
                  lang      => logresult.user.lang ); 
            end;
            html := Get_Output_Page(  
                     translations    => translations,
                     which_page      => which_page,
                     control_section => controls,
                     gallery         => gallery,
                     content         => table,
                     lang            => logresult.user.lang );
   
         when budget_page     => 
           declare
               print_counts   : Boolean := False;
               do_differences : Boolean := False;
               sysno          : Integer := 1;
               breakdown      : Breakdown_Target := by_decile;
               col            : All_Taxes_And_Benefits_Type;
           begin
              case which_popup is
                 when no_popup =>
                     if( Contains_Key( params, "Redraw" ))then 
                        print_counts   := AWS.Parameters.Get( params, "print_counts", 1 ) = "on";
                        do_differences := AWS.Parameters.Get( params, "do_differences", 1 ) = "on";
                        sysno          := Integer'Value( AWS.Parameters.Get( params, "sysno", 1 ));
                        breakdown      := Breakdown_Target'Value( AWS.Parameters.Get( params, "breakdown", 1 ));
                     end if;
                     table    := Get_Budget_Table(
                        outputs        => output, 
                        sysno          => sysno,
                        breakdown      => breakdown,
                        print_counts   => print_counts,
                        do_differences => do_differences,
                        lang           => logresult.user.lang ); 
                     controls := Get_Budget_Control_Section(
                        sysno          => sysno,
                        breakdown      => breakdown,
                        print_counts   => print_counts,
                        do_differences => do_differences,
                        lang           => logresult.user.lang );
                     gallery  := Get_Budget_Chart_Popup_Link( 
                        outputs   => output,
                        breakdown => breakdown,
                        lang      => logresult.user.lang );
                     html := Get_Output_Page(      
                              translations    => translations,
                              which_page      => which_page,
                              control_section => controls,
                              gallery         => gallery,
                              content         => table,
                              lang            => logresult.user.lang );
                 when gallery_popup =>
                     html := Get_Budget_Chart_Grid( output, logresult.user.lang );
                 when single_chart_popup =>
                     print_counts   := AWS.Parameters.Get( params, "print_counts", 1 ) = "on";
                     breakdown      := Breakdown_Target'Value( AWS.Parameters.Get( params, "breakdown", 1 ));
                     col            := All_Taxes_And_Benefits_Type'Value( AWS.Parameters.Get( params, "col", 1 ));
                     html := Get_Large_Budget_Chart_Page(
                        outputs      => output,
                        breakdown    => breakdown,
                        col          => col,
                        print_counts => print_counts,
                        lang         => logresult.user.lang );
              end case;
           end;
         when inequality_page =>
               case which_popup is
                  when no_popup =>
                     table    := TuS( "" ); -- Get_Inequality_Table( output, breakdown, logresult.user.lang ); 
                     controls := TuS( "" ); -- Get_Inequality_Control_Section( breakdown,logresult.user.lang );
                     gallery  := Get_Inequality_Chart_Grid( output, logresult.user.lang );
                     html := Get_Output_Page(      
                                 translations    => translations,
                                 which_page      => which_page,
                                 control_section => controls,
                                 gallery         => gallery,
                                 content         => table,
                                 lang            => logresult.user.lang );
                  when single_chart_popup =>
                     declare
                        breakdown    : Breakdown_Target :=  Breakdown_Target'Value( AWS.Parameters.Get( params, "breakdown", 1 ));
                        col            : String := AWS.Parameters.Get( params, "col", 1 );
                     begin
                        html := Get_Large_Inequality_Chart_Page(
                           outputs      => output,
                           breakdown    => breakdown,
                           col          => col,
                           lang         => logresult.user.lang );
                     end;
                  when others => html := Null_Unbounded_String;
               end case;
         when poverty_page    => 
            declare
               do_differences : Boolean := False;
               sysno          : Integer := 1;
               breakdown      : Breakdown_Target := by_decile;
               col            : Poverty_Elements;
            begin
              case which_popup is
                 when no_popup =>
                  if( Contains_Key( params, "Redraw" ))then 
                     do_differences := AWS.Parameters.Get( params, "do_differences", 1 ) = "on";
                     sysno          := Integer'Value( AWS.Parameters.Get( params, "sysno", 1 ));
                     breakdown      := Breakdown_Target'Value( AWS.Parameters.Get( params, "breakdown", 1 ));
                  end if;
                  table    := Get_Poverty_Table( 
                     outputs        => output, 
                     sysno          => sysno,
                     breakdown      => breakdown,
                     do_differences => do_differences,
                     lang           => logresult.user.lang ); 
                  controls := Get_Poverty_Control_Section(
                     sysno          => sysno,
                     breakdown      => breakdown,
                     do_differences => do_differences,
                     lang           => logresult.user.lang );
                  gallery  := Get_Poverty_Chart_Popup_Link(
                     outputs        => output, 
                     breakdown      => breakdown,
                     lang           => logresult.user.lang );
                  html := Get_Output_Page(      
                                    translations    => translations,
                                    which_page      => which_page,
                                    control_section => controls,
                                    gallery         => gallery,
                                    content         => table,
                                    lang            => logresult.user.lang );
                 when gallery_popup =>
                     html := Get_Poverty_Chart_Grid( output, logresult.user.lang );
                 when single_chart_popup =>
                     breakdown      := Breakdown_Target'Value( AWS.Parameters.Get( params, "breakdown", 1 ));
                     col            := Poverty_Elements'Value( AWS.Parameters.Get( params, "col", 1 ));
                     html := Get_Large_Poverty_Chart_Page(
                        outputs      => output,
                        breakdown    => breakdown,
                        col          => col,
                        lang         => logresult.user.lang );
               end case; 
            end;
         end case;
       end if;
       return AWS.Response.Build( "text/html", html );
   end Output_Callback;
   
   function Array_Update_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      --
      -- FIXME we've wired 'be' in here as a prefix in a couple of places
      -- 
   use Templates_Parser;
   use Ada.Characters.Handling;
      params         : constant AWS.Parameters.List := AWS.Status.Parameters( request );
      
      type Actions is ( insert_below, insert_above, delete, no_action );
      
      function Extract_Action( action_string : String ) return Actions is
         action : Actions := no_action;
      begin
         if( action_string = "insert_below" )then 
            action := insert_below;
         elsif( action_string = "insert_above" )then 
            action := insert_above;
         elsif( action_string = "delete" )then 
            action := delete;
         end if;
         return action;
      end Extract_Action;
      
      GLOBAL_PARAM_SYS   : constant be_params.BE_Parameter_System.Parameter_System_Rec := globals.Get_BE_Parameter_System;
      session_id         : constant AWS.Session.Id := AWS.Status.Session( request );
      outs               : Unbounded_String;
      buff               : Param_Buff := Buffer_Session.Get( session_id, globals.SESSION_PARAMETER_BUFFER );
      array_target       : constant Unbounded_String := TuS( AWS.Parameters.Get( params, "target_params" ));
      row_string         : constant String := AWS.Parameters.Get( params, "row" );
      action_string      : constant String := AWS.Parameters.Get( params, "action" );
      action             : constant Actions := Extract_Action( action_string );
      ajax_target_key    : constant Unbounded_String := TuS( AWS.Parameters.Get( params, "ajax_target_key" ));
      row                : Natural := Natural'Value( row_string );
      nb                 : constant Natural := buff.Get_Current_Collection_Size( array_target );
      max_len            : constant Natural := buff.Maximum_Collection_Size( array_target );
   begin
      Log(  "action is " & Actions'Image( action ));
      Log(  "max len " & Natural'Image( max_len ));
      Log(  "nb " & Natural'Image( nb ));
      Log(  "row " & Natural'Image( row ));
      Log(  "array_target " & TS( array_target ));
      case action is
      when insert_below => 
         if ( nb < max_len )then
            buff.Add( array_target, row+1 );
         end if;
      when insert_above =>
         if( nb < max_len )then
            buff.Add( array_target, row );
         end if;
      when delete       =>
         if( nb > 1 )then
            buff.Delete( array_target, row );
         end if;
      when no_action    =>
         Log( "unrecognised action " & action_string );
      end case;
      Buffer_Session.Set( session_id, globals.SESSION_PARAMETER_BUFFER, buff );
      outs := be_params.BE_Renderer.Make_Indexed_Block(
         GLOBAL_PARAM_SYS,
         array_target,
         buff, 
         ajax_target_key );
      Log( "created outs as " & TS( outs ));
      return AWS.Response.Build( "text/html", outs );
   end Array_Update_Callback;
   
   function Build_Help_Page( 
      path               : Unbounded_String_List; 
      lang : EU.BE.I81N.Languages ) return Unbounded_String is
   use Templates_Parser;
   use EU.BE.I81N.Translations;
   use EU.BE.I81N;
      GLOBAL_PARAM_SYS   : constant be_params.BE_Parameter_System.Parameter_System_Rec := globals.Get_BE_Parameter_System;
      SEP                : constant String := euws.Dir_Separator;
      TOP_LEVEL_PREFIX   : constant Unbounded_String := GLOBAL_PARAM_SYS.instance_name;
      help_sys     : be_params.BE_Parameter_System.Parameter_System_Rec;
      desc         : Unbounded_String;
      translations : Translate_Set;
      help_list    : Vector_Tag;
      link_list    : Vector_Tag;
      title_list   : Vector_Tag;      
      path_str     : Unbounded_String;
      title        : Unbounded_String;
      text         : Unbounded_String;
      link         : Unbounded_String;
      path_copy    : Unbounded_String_List := path;
      path_length  : Natural := Natural( path_copy.length );
   begin
      Log( "Build_Help_Page; path_length = " & Natural'Image( path_length ));
      for i in reverse 1 .. path_length loop
         
         path_str :=  TOP_LEVEL_PREFIX & TuS( Join( path_copy, '.' ));
         Log( "getting system " & TS( path_str ));
         help_sys := GLOBAL_PARAM_SYS.Get( TS( path_str ));
         text := TuS( help_sys.Description( be_params.BE_Parameter_System.description, lang ));
         title := TuS( help_sys.Description( be_params.BE_Parameter_System.label, lang ));
         link := path_str;
         
         help_list := help_list & text;
         link_list := link_list & link;
         title_list := title_list & title;
         
         path_copy.Delete( i );
      end loop;
      Insert( translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( translations, Assoc( "LANG", Lang_Str( lang )));
      Insert( translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( translations, Assoc( "SEP", euws.Dir_Separator ));
      Insert( translations, Assoc( "PAGE-TITLE", "Mefisto&nbsp;" & Lookup( "Help", lang )));
      Insert( translations, Assoc( "CONTENTS", help_list ));
      Insert( translations, Assoc( "LINKS", link_list ));
      Insert( translations, Assoc( "TITLES", title_list ));
      return TuS( Web_Utils.Parse_Template( 
         euws.template_components_path & 
         euws.dir_separator & "help_popup", 
         translations ));      
   end Build_Help_Page;

   type Actions is ( save, run, uprate, reset, abort_it, no_action );

   function Extract_Action( params : AWS.Parameters.List ) return Actions is
   use globals.BE_HTML_Utils;
   begin
      if( Contains_Key( params, "save" ))then 
         return save;
      end if;
      if( Contains_Key( params, "run" ))then 
         return run;
      end if;
      if( Contains_Key( params, "uprate" ))then 
         return uprate;
      end if;
      if( Contains_Key( params, "reset" ))then 
         return reset;
      end if;
      if( Contains_Key( params, "abort" ))then 
         return abort_it;
      end if;
      return no_action;
   end Extract_Action;
   
   
   function Light_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use EU.BE.Main_Menu;
   use EU.BE.I81N.Translations;
   use EU.BE.I81N;
   use EU.BE.Light_Parameters;
   use Ada.Containers;
   use BE_Base_Model_Types;
   
      lang : Languages;
      
      generic
         type T is (<>);
      function From_Amount( a : Amount ) return T;

      function From_Amount( a : Amount ) return T is
         im : constant String := Integer'Image( Integer( a ));
         s  : constant String := "V" & im( 2 .. im'Length );
      begin
         Put_Line( "s |" & s & "|" );
         return T'Value( s );
      end From_Amount;

      generic
         type T is (<>);
      function To_Amount( tt : T ) return Amount;
      
      function To_Amount( tt : T ) return Amount is
         s : String := T'Image( tt )( 2 .. T'Image( tt )'Length ) & ".0";
      begin
         return Amount'Value( s );
      end To_Amount;
      
      generic
         type T is (<>);
      function Pretty_Print( tt : T ) return String;
      
      function Pretty_Print( tt : T ) return String is
         m : Amount;
         s : String := T'Image( tt )( 2 .. T'Image( tt )'Length ) & ".0";
      begin
         m := Amount'Value( s );
         return EU.BE.I81N.Web_Format( m, lang );
      end Pretty_Print;
      
      type Rates_Type is ( v0, v1, v5, v6, v7, v8, v9, v10, v11, v12, v14, v15, v16, v18, v20, v25, v30, v40, v50 );
      type Bands_Type is ( v10000, v12000, v12500, v13000, v15000, v30000, v40000, v50000 );
      type RTC_Type is ( v100, v200, v300, v500, v800, v1000, v1100, v1200, v1500, v2000 );
      type MIP_Type is ( v100, v200, v800, v1000,  v1200, v1500, v1725, v2000, v2200, v2400, v3000 );
      type Child_Care_Type is ( v50, v100, v150, v200, v300, v500, v800, v1000 );
      
   
      function Rates_Print is new Pretty_Print( Rates_Type );
      function Rates_Convert_To is new To_Amount( Rates_Type );
      function Rates_Convert_From is new From_Amount( Rates_Type );
      package Rates_Package is new T_Utils( Rates_Type, Rate, Amount, Counter_Type );
      package HTML_Rates is new Rates_Package.Web_IO;

      function Bands_Print is new Pretty_Print( Bands_Type );
      function Bands_Convert_To is new To_Amount( Bands_Type );
      function Bands_Convert_From is new From_Amount( Bands_Type );
      package Bands_Package is new T_Utils( Bands_Type, Rate, Amount, Counter_Type );
      package HTML_Bands is new Bands_Package.Web_IO;
      
      function RTC_Print is new Pretty_Print( RTC_Type );
      function RTC_Convert_To is new To_Amount( RTC_Type );
      function RTC_Convert_From is new From_Amount( RTC_Type );
      package RTC_Package is new T_Utils( RTC_Type, Rate, Amount, Counter_Type );
      package HTML_RTC is new RTC_Package.Web_IO;

      function MIP_Print is new Pretty_Print( MIP_Type );
      function MIP_Convert_To is new To_Amount( MIP_Type );
      function MIP_Convert_From is new From_Amount( MIP_Type );
      package MIP_Package is new T_Utils( MIP_Type, Rate, Amount, Counter_Type );
      package HTML_MIP is new MIP_Package.Web_IO;
      
      function Child_Care_Print is new Pretty_Print( Child_Care_Type );
      function Child_Care_Convert_To is new To_Amount( Child_Care_Type );
      function Child_Care_Convert_From is new From_Amount( Child_Care_Type );
      package Child_Care_Package is new T_Utils( Child_Care_Type, Rate, Amount, Counter_Type );
      package HTML_Child_Care is new Child_Care_Package.Web_IO;

      GLOBAL_PARAM_SYS   : constant be_params.BE_Parameter_System.Parameter_System_Rec := globals.Get_BE_Parameter_System;
      SEP                : constant String := euws.Dir_Separator;
      TOP_LEVEL_PREFIX   : constant Unbounded_String := GLOBAL_PARAM_SYS.instance_name;
      URI                : constant String := AWS.Status.URI( Request );
      logresult          : users.Login_Result := Handle_Login( request );
      params             : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      session_id         : constant AWS.Session.Id := AWS.Status.Session( request );
      which_page         : constant String := AWS.Parameters.Get( params, "which_page", 1 );
      run_settings       : runsett.Model_Settings;
      path               : Unbounded_String_List := Split( URI, '/' );
      action             : Actions := Extract_Action( params );
      run_state          : runsett.State_Rec := Run_State_Session.Get(
         session_id,
         globals.SESSION_RUN_STATE );
      output_disabled    : Boolean;
      translations : Translate_Set;
      input_page : Unbounded_String;
      param_buffer       : Param_Buff;
      job_is_running     : Boolean := Is_Job_Running( request );
      num_errors         : Natural;
      disable_output     : Boolean;
      light_pars         : Light_Params := Light_Params_Session.Get( session_id, globals.SESSION_LIGHT );
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      output_disabled := Dont_Show_Output( request, logresult.user );
      Handle_Language( path, logresult.user, session_id );
      lang := logresult.user.lang;
      disable_output := Dont_Show_Output( request, logresult.user ); 
      Log( AWS.Parameters.URI_Format( params ));
      run_settings := Run_Settings_Session.Get( session_id, globals.SESSION_RUN_SETTINGS );      
      param_buffer := Buffer_Session.Get( session_id, globals.SESSION_PARAMETER_BUFFER );
      param_buffer.Set_Language( logresult.user.lang );     
      num_errors := param_buffer.Get_Num_Errors;
      translations := Get_Std_Translations( request, logresult.user );
      if action = run or action = save then
         declare
            which_package_str : constant String := AWS.Parameters.Get( params, "which_package", 1 );
            which_package_v     : constant Reform_Packages := Reform_Packages'Value( which_package_str );
         begin
            case which_package_v is
               when flat_tax =>
                  declare
                     r : Rates_Type := Rates_Type'Value( AWS.Parameters.Get( params, "base_rate", 1 ));
                     e : Bands_Type := Bands_Type'Value( AWS.Parameters.Get( params, "exemption", 1 ));
                  begin
                     light_pars.which_package := flat_tax;
                     light_pars.base_rate     := Rates_Convert_To( r ); 
                     light_pars.exemption     := Bands_Convert_To( e );
                  end;
               when child_care =>
                  declare 
                     c : Child_Care_Type := Child_Care_Type'Value( AWS.Parameters.Get( params, "basic_amount", 1 ));
                  begin
                     light_pars.which_package := child_care;
                     light_pars.basic_amount  := Child_Care_Convert_To( c );
                  end;
               when regional_tax_credit => 
                  declare 
                     c : RTC_Type := RTC_Type'Value( AWS.Parameters.Get( params, "basic_tax_credit", 1 ));
                  begin
                     light_pars.which_package := regional_tax_credit;
                     light_pars.basic_tax_credit := RTC_Convert_To( c );
                  end;
               when minimum_income_protection => 
                  declare 
                     m : MIP_Type := MIP_Type'Value( AWS.Parameters.Get( params, "income_support_per_single_person", 1 ));
                  begin
                     light_pars.which_package := minimum_income_protection;
                     light_pars.income_support_per_single_person := MIP_Convert_To( m );
                  end;
            end case;
            param_buffer := globals.Get_Loaded_Input_Buffer( lang ); -- reset everything back to defaults
            EU.BE.Parameter_System_Declarations.Map_To_Parameters( light_pars, param_buffer );          
            Buffer_Session.Set( session_id, globals.SESSION_PARAMETER_BUFFER, param_buffer );
            Light_Params_Session.Set( session_id, globals.SESSION_LIGHT, light_pars );
            if action = run then
               Log( "run entered; num_errors=" & Natural'Image( num_errors ) & " job_is_running " & Boolean'Image( job_is_running ));
               if( not job_is_running ) and num_errors = 0 then
                  run_state := runsett.NULL_STATE_REC;
                  run_state.queued := True;
                  Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, run_state );
                  declare
                     use EU.BE.Users.IO;
                     run_number_str : constant String := EU.BE.Model.Runner.Get_New_Run_Id;
                     output_dir : Unbounded_String;
                  begin
                     run_state.queued := True;
                     run_settings.Set_Run_Id( run_number_str  );
                     Log( "quickies: creating run; run_settings.Working_Root=" & run_settings.Working_Root & " SEP |" & SEP & "| run_number_str | " & run_number_str );
                     output_dir := Create_Directories_For_Run( 
                        run_settings.Working_Root, 
                        SEP, 
                        logresult.user, 
                        run_number_str );
                     globals.Create_Parameter_Files( 
                        run_settings, 
                        param_buffer );
                     web_runner.Submit_Run( 
                        session_id, 
                        run_settings );
                     job_is_running := True;
                  end;
               end if;
            end if; -- run request
            Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, run_state );
            Run_Settings_Session.Set( session_id, globals.SESSION_RUN_SETTINGS, run_settings );
         end; -- declare_packages
      end if; -- run or save
      if( job_is_running or ( num_errors > 0 ))then
         Insert( translations, Assoc( "DISABLE-RUN", " disabled='disabled' " ));    
      end if;
      Insert( translations, Assoc( "JOB-IS-RUNNING", job_is_running ));
      Insert( translations, Assoc( "IS-INPUT-PAGE", True ));
      Insert( translations, Assoc( "RATE-INPUT-LABEL", Lookup( "Unique Tax Rate", lang )));
      Insert( translations, Assoc( "RATE-INPUT", HTML_Rates.Make_Select( "base_rate", Rates_Convert_From( light_pars.base_rate ), Rates_Print'Access )));
      Insert( translations, Assoc( "EXEMPTION-INPUT-LABEL", Lookup( "Exemption", lang )));
      Insert( translations, Assoc( "EXEMPTION-INPUT", HTML_Bands.Make_Select( "exemption", Bands_Convert_From( light_pars.exemption ), Bands_Print'Access )));
      Insert( translations, Assoc( "MODEL-MENU", Get_Main_Menu( light_page, disable_output, logresult.user.lang )));
      Insert( translations, Assoc( "LIGHT-PAGE", Lookup( "light_page", lang )));
      Insert( translations, Assoc( "SAVE", Lookup( "Save", lang )));
      Insert( translations, Assoc( "RUN", Lookup( "Run", lang )));
      
      Insert( translations, Assoc( "FLAT-RATE-LABEL", Lookup( "Flat tax proposal", lang )));
      Insert( translations, Assoc( "FLAT-RATE-LINK", Lookup( "Help Link here", lang )));
      
      Insert( translations, Assoc( "RTC-INPUT-LABEL", Lookup( "Regional Tax Credit", lang )));
      Insert( translations, Assoc( "RTC-LABEL", Lookup( "Basic Tax Credit", lang )));
      Insert( translations, Assoc( "RTC-LINK", Lookup( "Help Link here", lang )));
      Insert( translations, Assoc( "RTC-INPUT", HTML_RTC.Make_Select( "basic_tax_credit", RTC_Convert_From( light_pars.basic_tax_credit ), RTC_Print'Access )));
      
      Insert( translations, Assoc( "CHILD-CARE-LABEL", Lookup( "Child Care", lang )));
      Insert( translations, Assoc( "CHILD-CARE-INPUT-LABEL", Lookup( "Base amount of child care", lang )));
      Insert( translations, Assoc( "CHILD-CARE-LINK", Lookup( "Help Link here", lang )));
      Insert( translations, Assoc( "CHILD-CARE-INPUT", HTML_Child_Care.Make_Select( "basic_amount", Child_Care_Convert_From( light_pars.basic_amount ), Child_Care_Print'Access )));
      
      Insert( translations, Assoc( "MIP-INPUT-LABEL", Lookup( "Base Amount", lang )));
      Insert( translations, Assoc( "MIP-LABEL", Lookup( "Minimum income protection", lang )));
      Insert( translations, Assoc( "MIP-LINK", Lookup( "Help Link here", lang )));
      Insert( translations, Assoc( "MIP-INPUT", HTML_MIP.Make_Select( "income_support_per_single_person", MIP_Convert_From( light_pars.income_support_per_single_person ), MIP_Print'Access )));

      return Web_Utils.Build_Input_Page(
         euws.template_components_path & "input_light",
         translations );
   end Light_Page_Callback;
   
   

   function Parameter_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use EU.BE.Main_Menu;
   use EU.BE.I81N.Translations;
   use Ada.Containers;
   

      GLOBAL_PARAM_SYS   : constant be_params.BE_Parameter_System.Parameter_System_Rec := globals.Get_BE_Parameter_System;
      SEP                : constant String := euws.Dir_Separator;
      TOP_LEVEL_PREFIX   : constant Unbounded_String := GLOBAL_PARAM_SYS.instance_name;
      URI                : constant String := AWS.Status.URI( Request );
      logresult          : users.Login_Result := Handle_Login( request );
      params             : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      session_id         : constant AWS.Session.Id := AWS.Status.Session( request );
      which_page         : constant String := AWS.Parameters.Get( params, "which_page", 1 );
      
      action             : Actions := Extract_Action( params );
      depth              : Natural := 0;
      error_message      : Unbounded_String;
      extra_translations : Translate_Set;
      input_page         : Unbounded_String;
      job_is_running     : Boolean := Is_Job_Running( request );
      main_error_message : Unbounded_String;
      model_menu         : Unbounded_String;
      num_errors         : Natural := 0;
      param_buffer       : Param_Buff;
      path               : Unbounded_String_List := Split( URI, '/' );
      path_str           : Unbounded_String;
      prefix             : Unbounded_String := TuS( "/mefisto/parameters_page/" );
      run_settings       : runsett.Model_Settings;
      run_state          : runsett.State_Rec := Run_State_Session.Get(
         session_id,
         globals.SESSION_RUN_STATE );
      target_sys         : be_params.BE_Parameter_System.Parameter_System_Rec;
      output_disabled    : Boolean;
      in_help_mode       : Boolean := False;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      output_disabled := Dont_Show_Output( request, logresult.user );
      Handle_Language( path, logresult.user, session_id );
      Log( AWS.Parameters.URI_Format( params ));
      run_settings := Run_Settings_Session.Get( session_id, globals.SESSION_RUN_SETTINGS );      
      param_buffer := Buffer_Session.Get( session_id, globals.SESSION_PARAMETER_BUFFER );
      param_buffer.Set_Language( logresult.user.lang );     
      extra_translations := Get_Std_Translations( request, logresult.user );
      
      path.Delete( 1, 2 ); -- strip "mefisto" and "parameters_page" from the url
      if( path.Length > 0 ) and then (path.Element( Natural( path.Length )) = TuS( "help" ))then
          path.Delete( Natural( path.Length ));
          in_help_mode := True;
      end if;
      -- the path this page represents in the menu, e.g. the 1st tax page and so on
      GLOBAL_PARAM_SYS.Complete_Path_To_Left( path, depth );
      -- use this to complete the top menu    
      model_menu := model_menu & Get_Main_Menu( parameters_page, output_disabled, logresult.user.lang );
      model_menu := model_menu & LINE_BREAK;
      model_menu := model_menu & be_params.BE_Renderer.Make_Parameter_Menu( 
         GLOBAL_PARAM_SYS, 
         path, 
         prefix, 
         logresult.user.lang, 
         logresult.user );      
      model_menu := model_menu & LINE_BREAK;
      --
      -- this gets the xml parameters system description of the component we're on
      --
      path_str := TOP_LEVEL_PREFIX & TuS( Join( path, '.' ));
      target_sys := GLOBAL_PARAM_SYS.Get( TS( path_str ));
      
      Log(  "path_str " & TS( path_str ));
      if( in_help_mode )then
         return AWS.Response.Build( "text/html", Build_Help_Page( path, logresult.user.lang ));
      end if;
      Log(  "Parameter_Page_Callback entered" );
      Log(  "URI = " & URI );
      Log(  "action = " & Actions'Image( action ));
      -- initial error check - so we don't allow running on a new page 
      -- if there are errors elsewhere
      num_errors := param_buffer.Get_Num_Errors;
      case action is
      when run =>
         param_buffer.Load( params );
         num_errors := param_buffer.Get_Num_Errors;
         Log( "run entered; num_errors=" & Natural'Image( num_errors ) & " job_is_running " & Boolean'Image( job_is_running ));
         if( not job_is_running ) and num_errors = 0 then
            run_state := runsett.NULL_STATE_REC;
            run_state.queued := True;
            Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, run_state );
            declare
               use EU.BE.Users.IO;
               run_number_str : constant String := EU.BE.Model.Runner.Get_New_Run_Id;
               output_dir : Unbounded_String;
            begin
               run_state.queued := True;
               
               run_settings.Set_Run_Id( run_number_str  );
               Log( "creating run; run_settings.Working_Root=" & run_settings.Working_Root & " SEP |" & SEP & "| run_number_str | " & run_number_str );
               output_dir := Create_Directories_For_Run( 
                  run_settings.Working_Root, 
                  SEP, 
                  logresult.user, 
                  run_number_str );
               globals.Create_Parameter_Files( 
                  run_settings, 
                  param_buffer );
               web_runner.Submit_Run( 
                  session_id, 
                  run_settings );
               job_is_running := True;
            end;
         end if;
      when reset =>
         Log( "reset entered" );
         declare
            reset_all : Boolean := AWS.Parameters.Get( params, "resetselect", 1 ) = "reset_allpages";
         begin
            if( reset_all )then
                 be_params.BE_Parameter_System_IO.Load( 
                      param_buffer,
                      GLOBAL_PARAM_SYS,
                      GLOBAL_PARAM_SYS,
                      globals.Get_Default_Params,
                      TOP_LEVEL_PREFIX     
                      );
            else
                 be_params.BE_Parameter_System_IO.Load( 
                      param_buffer,
                      GLOBAL_PARAM_SYS,
                      target_sys,
                      globals.Get_Default_Params,
                      path_str 
                      );
            end if;
         end;
      when save =>
         Log( "saving entered" );
         param_buffer.Load( params );
      when abort_it =>
         Log( "aborting" );
         -- FIXME we have no way of actually interruping euromod at present: maybe use an external queue?
         AWS.Session.Set( session_id, globals.SESSION_ABORTING, True );
      when uprate => 
         Log( "uprate entered" );
         declare
            use BE_Base_Model_Types;
            use Globals;
            use Utils;
            ms : constant String := AWS.Parameters.Get( params, "uprate_amount", 1 );
            m  : Rate;
            message : Unbounded_String;
            error : Utils.Error_Type;
            uprate_all : Boolean;
            uprate_all_str : constant String := AWS.Parameters.Get( params, "uprateselect", 1 );
         begin
            uprate_all := uprate_all_str = "uprate_allpages";
            Log( " ms = |" & ms & "uprate_all_str |" & uprate_all_str & "| uprate_all = " & Boolean'Image( uprate_all ));
            -- load so we save any other changes the user made before pressing uprate
            param_buffer.Load( params );
            EU.BE.I81N.Web_Validate(
               ms,
               logresult.user.lang,
               m,
               message,
               error,
               -1_000.0,
               1_000.0 );
            if( error = No_Error )then
               m := 1.0 + ( m / 100.0 );
               if( uprate_all )then
                  be_params.BE_Parameter_System_IO.Operate( param_buffer, Null_Unbounded_String, m );
               else
                  be_params.BE_Parameter_System_IO.Operate( param_buffer, path_str, m );
               end if;                     
               Insert( extra_translations, Assoc( "UPRATE-ERROR", "" ));
            else
               message := TuS( LINE_BREAK & "<br/><span class='input_error_message'>" ) & message & "</span>" & LINE_BREAK;
               Insert( extra_translations, Assoc( "UPRATE-ERROR", message ));
               Insert( extra_translations, Assoc( "UPRATE-FIELD-CLASS", " class='input_error' " ));
            end if;   
         end;
      when no_action => null;   
      end case;
      -- new error count on the way out ...
      num_errors := param_buffer.Get_Num_Errors;
      Log( "got error count as " & Natural'Image( num_errors ));
      if( num_errors /= 0 ) then
         main_error_message := TuS( "<div class='error_section'>" ) &
               Lookup( "submit_error_message", logresult.user.lang ) & 
               "</div>";
      else
         main_error_message := TuS( "" );
      end if;
      Insert( extra_translations, Assoc( "MAIN-ERROR-MESSAGE", main_error_message ));
      if( job_is_running or ( num_errors > 0 ))then
         Insert( extra_translations, Assoc( "DISABLE-RUN", " disabled='disabled' " ));    
      end if;
      Insert( extra_translations, Assoc( "JOB-IS-RUNNING", job_is_running ));
      Insert( extra_translations, Assoc( "IS-INPUT-PAGE", True ));
      
      input_page := be_params.BE_Renderer.Create_Input_Page(
         buff                => param_buffer,
         model_menu          => model_menu,
         base_sys            => GLOBAL_PARAM_SYS, 
         sys                 => target_sys, 
         parameter_prefix    => path_str,
         main_error_message  => error_message,
         job_is_running      => job_is_running,
         user                => logresult.user,
         extra_translations  => extra_translations );
      Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, run_state );
      Run_Settings_Session.Set( session_id, globals.SESSION_RUN_SETTINGS, run_settings );
      Buffer_Session.Set( session_id, globals.SESSION_PARAMETER_BUFFER, param_buffer );
      return AWS.Response.Build( "text/html", input_page );
   end Parameter_Page_Callback;


end Callbacks.Mefisto;
