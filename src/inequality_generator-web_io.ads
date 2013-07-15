--
-- copyright(c) 2009 Graham Stark/ Virtual Worlds (graham.stark@virtual-worlds.biz)
--
-- ////////////////////////////////
--
-- This is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
-- 
-- It is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this software; see the file docs/gpl_v3.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street,
-- Boston, MA 02110-1301, USA.
-- 
-- /////////////////////////////
pragma License( Modified_GPL );
--
-- FIXME : Generic on Language somehow.
--

with Ada.Strings.Unbounded;
with Text_Utils;
with Ada.Text_IO;
with EU.BE.I81N;
with Google_Chart_Constants;
with General_Chart_Constants;

generic
   
package Inequality_Generator.Web_IO is
   
   use Text_Utils;
   use Ada.Strings.Unbounded;
   use EU.BE.I81N;     
   use General_Chart_Constants;
    
   function Make_Standard_Chart(
      plotter_url   : String;
      pre           : Quantile_List;
      post          : Quantile_List;      
      title         : String;
      size          : Chart_Size;
      lang          : Languages ) return Unbounded_String;
  
   function Make_Chart(
      pre           : Quantile_List;
      pre_measures  : Inequality_Array;
      post          : Quantile_List;      
      post_measures : Inequality_Array;      
      title         : String;
      is_thumbnail  : Boolean;
      lang          : Languages ) return Unbounded_String;
   
   function To_String(
         pre          : Inequality_Array;
         post         : Inequality_Array;      
         label        : String;
         lang         : Languages ) return Unbounded_String;
         
   function To_String(
       pre_lorenz   : Quantile_List;
       pre_ineq     : Inequality_Array;
       post_lorenz  : Quantile_List;  
       post_ineq    : Inequality_Array;
       label         : String;
       lang          : Languages ) return Unbounded_String;
          


end Inequality_Generator.Web_IO;
