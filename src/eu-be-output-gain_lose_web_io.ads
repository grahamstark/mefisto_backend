with Ada.Strings.Unbounded;
with Text_Utils;
with Tabulator_Commons;
with IO_Commons;
with EU.BE.I81N;
with T_Utils;
with T_Utils.Web_IO;
with HTML_Utils;

package EU.BE.Output.Gain_Lose_Web_IO is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Tabulator_Commons;
   use EU.BE.I81N;
   use IO_Commons;
   
   function Get_Summary(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String;  
   
   function Get_Gain_Lose_Chart_Grid(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target   := no_breakdown;
      lang          : Languages          := Languages'First ) return Unbounded_String;  
 
  function Get_Gain_Lose_Table( 
      outputs      : Outputs_Rec; 
      breakdown    : Disaggregated_Breakdown_Target   := by_decile;
      comp_Cell    : Compare_Cell       := current_cell;
      cell_op      : Cell_Compare_Type  := counter;
      value_To_Use : Summary_Items_Type := disposable_income;
      lang         : Languages := Languages'First ) return Unbounded_String;
   
end  EU.BE.Output.Gain_Lose_Web_IO;
