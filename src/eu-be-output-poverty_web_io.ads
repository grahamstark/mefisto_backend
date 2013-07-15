with Ada.Strings.Unbounded;
with Text_Utils;
with Tabulator_Commons;
with IO_Commons;
with EU.BE.I81N;
with T_Utils;
with T_Utils.Web_IO;
with HTML_Utils;
with General_Chart_Constants;

package EU.BE.Output.Poverty_Web_IO is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Tabulator_Commons;
   use EU.BE.I81N;
   use IO_Commons;
   use General_Chart_Constants;
   
   function Get_Summary(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String;  
   
   function Get_Poverty_Chart_Grid(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String;

   function Get_Poverty_Table( 
      outputs        : Outputs_Rec; 
      sysno          : System_Number    := 1;
      breakdown      : Disaggregated_Breakdown_Target := by_decile;
      do_differences : Boolean          := False;
      lang           : Languages        := Languages'First ) return Unbounded_String;
      
  function Get_Large_Poverty_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Disaggregated_Breakdown_Target   := by_decile;
      col            : Poverty_Elements;
      lang           : Languages          := Languages'First ) return Unbounded_String; 
      
  function Get_Poverty_Chart_Popup_Link(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target;
      lang          : Languages := Languages'First ) return Unbounded_String;    
end  EU.BE.Output.Poverty_Web_IO;
