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
with Text_Utils;

with AWS.Session;
with AWS.Response;
with AWS.Parameters;
with AWS.Status;

--
-- These are the callbacks specific to the Mefisto model. Essentially, one per page
-- of the main menu, plus chart and example popups & download handlers.
--
package Callbacks.Mefisto is
   
   use Ada.Strings.Unbounded;
   use Text_Utils;

   function Index_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Light_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Run_Progress_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Parameter_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Output_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Array_Update_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Download_Run_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Example_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;

end Callbacks.Mefisto;
