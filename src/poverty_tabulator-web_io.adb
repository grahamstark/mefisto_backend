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
with IO_Commons;
with Text_Utils;
with BE_Base_Model_Types;
with T_Utils;
with T_Utils.Standard_Chart_Generator;
with EU.Web.Settings;

package body Poverty_Tabulator.Web_IO is
   
   use IO_Commons;
   use Text_Utils;
   use Translations;
   use BE_Base_Model_Types;
   
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
      lang         : Languages ) return Unbounded_String is

      package Pov_T is new T_Utils( Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type, T => Breakdown_Range );
      package Pov_Standard_Charts is new Pov_T.Standard_Chart_Generator;
      
      function Pretty_Print( v : Breakdown_Range ) return String is
      begin
         return Lookup( Censor_String( Breakdown_Range'Image( v )), lang );
      end  Pretty_Print;
         
    use General_Chart_Constants;
    use Text_Utils;
    use Pov_T;  
      url       : Unbounded_String;
      utitle    : Unbounded_String;
      pre_data  : Pov_T.Amount_Array;
      post_data : Pov_T.Amount_Array;   
   begin
      for b in Breakdown_Range loop
         pre_data( b ) := Amount( pre_tab.cells( b )( col ));
         post_data( b ) := Amount( post_tab.cells( b )( col ));
      end loop;
      url := Pov_Standard_Charts.Make_Univariate_Chart(
         plotter_url  => EU.Web.Settings.Charts_URL,         
         title        => Lookup( title, lang ),
         subtitle     => Lookup( subtitle, lang ),
         data1        => pre_data,
         data2        => post_data,
         printer      => Pretty_Print'Access,
         x_axis_label => Lookup( x_axis_title, lang ),
         y_axis_label => Lookup( y_axis_title, lang ),
         ctype        => chart_type,
         system       => both,
         size         => chart_size );
      return url;
   end Make_Chart;

   
   function To_String( 
         tab          : Table_Type;
         poverty_line : Data_Type;
         name         : String;
         description  : String; 
         lang         : Languages
        ) return Unbounded_String is
            
      function Fmt( a : Data_Type; cells_as_ints : Boolean ) return String is
      begin
         if( cells_as_ints )then
            return Web_Format( Integer( a ), lang );
         else
            return Web_Format( Amount( a ), lang );
         end if;
      end Fmt;
            
      t : Unbounded_String;
      format_as_ints : Boolean;
   begin
      t := t & "<span class='smallText'>" & description & "</span><br/>" & LINE_BREAK;
      t := t & "<span class='important'>" & Lookup( "Poverty line (&euro;s per month per person)", lang ) & Fmt( poverty_line, False ) & "</span>" & LINE_BREAK;
      t := t & "<table class='datatable' cellpadding='8' >" & LINE_BREAK;
      t := t & "   <caption>" & Lookup( name, lang ) & "</caption>" & LINE_BREAK;
      t := t & "   <thead>" & LINE_BREAK;
      t := t & "      <tr class='headerRow'>" & LINE_BREAK;
      t := t & "          <th width='20%'></th>";
      for v in Poverty_Elements loop 
          t := t & "<th>" & Lookup( Censor_String( Poverty_Elements'Image( v )), lang ) & "</th>";
      end loop;
      t := t & "      </tr>" & LINE_BREAK;
      t := t & "   </thead>" &  LINE_BREAK;
      t := t & "   <tbody>" & LINE_BREAK;
      
      for b in Breakdown_Range loop
         t := t & "      <tr>" & LINE_BREAK;
         t := t & "         <th>" & Lookup( Censor_String( Breakdown_Range'Image( b )), lang ) & "</th>";
         for v in Poverty_Elements loop 
            format_as_ints := ( v = population ) or ( v = people_in_poverty );
            t := t & "<td>" & Fmt( tab.cells( b )( v ), format_as_ints ) & "</td>"; 
         end loop;
         t := t & "          </tr>" & LINE_BREAK;
      end loop;
      t := t & "   <tr class='tableRowTotals'>" & LINE_BREAK;
      t := t & "      <th width='20%'>" & Lookup( "totals", lang ) & "</th>";
      for v in Poverty_Elements loop
         format_as_ints := ( v = population ) or ( v = people_in_poverty );
         t := t & "<td class='totals'>" & Fmt( tab.totals( v ), format_as_ints ) & "</td>"; 
      end loop;
      t := t & "      </tr>" & LINE_BREAK;
      t := t & "   </tbody>" & LINE_BREAK;
      t := t & "</table>" & LINE_BREAK;
      return t;
   end To_String;

end Poverty_Tabulator.Web_IO;
