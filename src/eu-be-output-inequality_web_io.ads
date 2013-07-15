with Ada.Strings.Unbounded;

with EU.BE.I81N;
with General_Chart_Constants;
with HTML_Utils;
with IO_Commons;
with T_Utils.Web_IO;
with T_Utils;
with Tabulator_Commons;
with Text_Utils;

package EU.BE.Output.Inequality_Web_IO is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Tabulator_Commons;
   use EU.BE.I81N;
   use IO_Commons;
   use General_Chart_Constants;
 
    function Get_Summary(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String;  
  
   function Get_Inequality_Chart_Grid(
      outputs      : Outputs_Rec; 
      lang         : Languages          := Languages'First ) return Unbounded_String; 
   
   function Get_Inequality_Table(
      outputs        : Outputs_Rec; 
      breakdown      : Breakdown_Target := no_breakdown;
      lang           : Languages        := Languages'First ) return Unbounded_String;
      
   function Create_Chart(
      outputs      : Outputs_Rec; 
      breakdown    : Breakdown_Target   := no_breakdown;
      col          : String;
      size         : Chart_Size;
      lang         : Languages := Languages'First) return Unbounded_String;
      
   function Get_Large_Inequality_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Breakdown_Target;
      col            : String;
      lang           : Languages          := Languages'First ) return Unbounded_String; 
  
end  EU.BE.Output.Inequality_Web_IO;
