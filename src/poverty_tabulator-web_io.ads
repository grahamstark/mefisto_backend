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
with General_Chart_Constants;

generic

package Poverty_Tabulator.Web_IO is
   
   use Text_Utils;
   use Ada.Strings.Unbounded;
   use EU.BE.I81N;     

   function Make_Chart(
      pre_tab      : Table_Type;
      post_tab     : Table_Type;
      title        : String;
      subtitle     : String;
      col          : Poverty_Elements;
      chart_type   : General_Chart_Constants.Chart_Type;
      chart_size   : General_Chart_Constants.chart_size;
      x_axis_title : String;
      y_axis_title : String;
      lang         : Languages ) return Unbounded_String;

   
   function To_String( 
         tab          : Table_Type;
         poverty_line : Data_Type;
         name         : String;
         description  : String; 
         lang         : Languages ) return Unbounded_String;


end Poverty_Tabulator.Web_IO;
