with Ada.Strings.Unbounded;
with Text_Utils;
with Tabulator_Commons;
with IO_Commons;
with EU.BE.I81N;
with T_Utils;
with T_Utils.Web_IO;
with HTML_Utils;
with EU.BE.Output.Inequality_Web_IO;
with EU.BE.Output.Poverty_Web_IO;
with EU.BE.Output.Budget_Web_IO;
with EU.BE.Output.Gain_Lose_Web_IO;
with Templates_Parser;

package EU.BE.Output.Web_IO is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Tabulator_Commons;
   use EU.BE.I81N;
   use IO_Commons;
   use Templates_Parser;
   
   function Get_Summary_Table( 
      outputs        : Outputs_Rec;
      lang           : Languages        := Languages'First ) return Unbounded_String;
   
    -- no longer needed --      
   function Get_Inequality_Control_Section(
      breakdown    : Breakdown_Target   := no_breakdown;
      lang         : Languages          := Languages'First ) return Unbounded_String; 
   
   function Get_Inequality_Chart_Grid(
      outputs      : Outputs_Rec; 
      lang         : Languages          := Languages'First ) return Unbounded_String 
         renames EU.BE.Output.Inequality_Web_IO.Get_Inequality_Chart_Grid; 
   
       
  function Get_Large_Inequality_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Breakdown_Target;
      col            : String;
      lang           : Languages          := Languages'First ) return Unbounded_String renames  EU.BE.Output.Inequality_Web_IO.Get_Large_Inequality_Chart_Page;    
      

   function Get_Poverty_Control_Section( 
      sysno          : System_Number                  := 1;
      breakdown      : Disaggregated_Breakdown_Target := by_decile;
      do_differences : Boolean                        := False;
      lang           : Languages := Languages'First ) return Unbounded_String;
   
   function Get_Poverty_Table( 
      outputs        : Outputs_Rec; 
      sysno          : System_Number                  := 1;
      breakdown      : Disaggregated_Breakdown_Target := by_decile;
      do_differences : Boolean                        := False;
      lang           : Languages                      := Languages'First ) return Unbounded_String 
      renames EU.BE.Output.Poverty_Web_IO.Get_Poverty_Table;
      
   function Get_Poverty_Chart_Grid(
      outputs      : Outputs_Rec; 
      lang         : Languages          := Languages'First ) return Unbounded_String
      renames EU.BE.Output.Poverty_Web_IO.Get_Poverty_Chart_Grid;
      
   function Get_Large_Poverty_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Disaggregated_Breakdown_Target   := by_decile;
      col            : Poverty_Elements;
      lang           : Languages          := Languages'First ) return Unbounded_String 
      renames EU.BE.Output.Poverty_Web_IO.Get_Large_Poverty_Chart_Page;
      
   function Get_Poverty_Chart_Popup_Link(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target;
      lang          : Languages := Languages'First ) return Unbounded_String 
      renames  EU.BE.Output.Poverty_Web_IO.Get_Poverty_Chart_Popup_Link; 
    
         
   function Get_Budget_Chart_Grid(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String
      renames EU.BE.Output.Budget_Web_IO.Get_Budget_Chart_Grid;
   
   function Get_Budget_Table( 
      outputs        : Outputs_Rec; 
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := by_decile;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First) return Unbounded_String
      renames EU.BE.Output.Budget_Web_IO.Get_Budget_Table;
 
   function Get_Budget_Chart_Popup_Link(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target;
      lang          : Languages := Languages'First ) return Unbounded_String 
      renames EU.BE.Output.Budget_Web_IO.Get_Budget_Chart_Popup_Link;
      
  function Get_Budget_Control_Section(
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := by_decile;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First) return Unbounded_String;
      
  function Get_Large_Budget_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Disaggregated_Breakdown_Target   := by_decile;
      col            : All_Taxes_And_Benefits_Type;
      print_counts   : Boolean            := False;
      lang           : Languages          := Languages'First ) return Unbounded_String renames
   EU.BE.Output.Budget_Web_IO.Get_Large_Budget_Chart_Page;  
      
  function Get_Gain_Lose_Chart_Grid(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target   := no_breakdown;
      lang          : Languages          := Languages'First ) return Unbounded_String 
  renames EU.BE.Output.Gain_Lose_Web_IO.Get_Gain_Lose_Chart_Grid;
      
   function Get_Gain_Lose_Table( 
      outputs      : Outputs_Rec; 
      breakdown    : Disaggregated_Breakdown_Target   := by_decile;
      comp_Cell    : Compare_Cell       := current_cell;
      cell_op      : Cell_Compare_Type  := counter;
      value_To_Use : Summary_Items_Type := disposable_income;
      lang         : Languages          := Languages'First ) return Unbounded_String 
   renames EU.BE.Output.Gain_Lose_Web_IO.Get_Gain_Lose_Table;
      
   function Get_Gain_Lose_Control_Section(
      breakdown        : Disaggregated_Breakdown_Target   := by_decile;
      comp_cell        : Compare_Cell       := current_cell;
      cell_op          : Cell_Compare_Type  := counter;
      value_To_Use     : Summary_Items_Type := disposable_income;
      lang             : Languages := Languages'First;
      advanced_version : Boolean := False ) return Unbounded_String;
      
   type Output_Page_Type is ( summary_page, gain_lose_page, budget_page, inequality_page, poverty_page );  
   
   function Get_Output_Menu( 
        which_page : Output_Page_Type;
        lang       : Languages ) return Unbounded_String;
   
   function Get_Output_Page( translations    : Translate_Set;
                             which_page      : Output_Page_Type;
                             control_section : Unbounded_String;
                             gallery         : Unbounded_String;
                             content         : Unbounded_String;
                             lang            : Languages ) return Unbounded_String;
                              
   -- package Summary_Items_Type_T is new T_Utils( T => Summary_Items_Type, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
   -- package Summary_Items_Type_T_Web_IO is new Summary_Items_Type_T.Web_IO;
   -- 
   -- package Cell_Compare_Type_T is new T_Utils( T => Cell_Compare_Type, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
   -- package Cell_Compare_Type_T_Web_IO is new Cell_Compare_Type_T.Web_IO;
   -- 
   -- package Breakdown_Target_T is new T_Utils( T => Breakdown_Target, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
   -- package Breakdown_Target_T_Web_IO is new Breakdown_Target_T.Web_IO;
   -- 
   -- package Compare_Cell_T is new T_Utils( T => Compare_Cell, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
   -- package Compare_Cell_T_Web_IO is new Compare_Cell_T.Web_IO;
  -- 
   package BE_HTML is new HTML_Utils( Rate=>Rate, Counter_Type=>Counter_Type );
   
end  EU.BE.Output.Web_IO;
