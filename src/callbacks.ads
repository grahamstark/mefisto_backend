------------------------------------------------------------------------------
--                                                                          --
--  Handlers for each of Mefisto's callbacks, plus some support functions   --
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
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

with Text_Utils;

with AWS.Session;
with AWS.Response;
with AWS.Parameters;
with AWS.Status;

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
with EU.BE.I81N;
with EU.Web.Settings;

with EU_Logger;
with Templates_Parser;

--
-- Callbacks common to Mefisto and the Example Household model, plus
-- some private support routines for handling logins, language changes, etc.
-- Main model handlers should be child packages of this.
--
package Callbacks is
   
   use Ada.Strings.Unbounded;
   use Text_Utils;
   
   function Serve_Static_Resource( Request : in AWS.Status.Data ) return AWS.Response.Data;
   -- function Serve_File_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Logout_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Cookie_Test( request : in AWS.Status.Data )  return AWS.Response.Data;
   
private  
   
   package chars                 renames Ada.Characters.Handling;
   package users                 renames EU.BE.Users;
   package globals               renames EU.BE.Globals;
   package be_params             renames EU.BE.Parameter_System_Declarations;
   package runsett               renames EU.BE.Model.Settings;
   package outp                  renames EU.BE.Output;
   package web_runner            renames EU.BE.Model.Runner.Web;
   package euws                  renames EU.Web.Settings;
   package Output_Session        renames globals.Output_Session_Package;
   package Run_State_Session     renames globals.Run_State_Session_Package;
   package Run_Settings_Session  renames globals.Run_Settings_Session_Package;
   package Buffer_Session        renames globals.Buffer_Session_Package;
   package Previous_Runs_Session renames globals.Previous_Runs_Session_Package;
   package Light_Params_Session  renames globals.Light_Params_Session_Package;
   
   subtype Param_Buff is be_params.BE_Parameter_System_IO.Buffer;

   
   procedure Log( s : String );
   function Handle_Login( request : in AWS.Status.Data ) return users.Login_Result;
   function Is_Job_Running( request : in AWS.Status.Data ) return Boolean;
   function Get_Std_Translations( request : in AWS.Status.Data; user : users.User_Type ) return Templates_Parser.Translate_Set;
   procedure Handle_Language( 
      path : in out Unbounded_String_List; 
      user : in out  users.User_Type;
      session_id : in AWS.Session.Id );

end Callbacks;
