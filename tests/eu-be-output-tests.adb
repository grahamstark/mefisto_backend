--
--  $Author: graham_s $
--  $Date: 2010-02-15 13:40:48 +0000 (Mon, 15 Feb 2010) $
--  $Revision: 8644 $
--


with Ada.Text_IO;

with AUnit.Assertions;
with AUnit.Test_Cases;

with EU;
with EU.BE.Globals;
with EU.BE.Parameter_System_Declarations;

with EU.BE.Household.Web_IO;
with EU.BE.I81N;
with EU.BE.Output.Web_IO;
with EU.BE.Results.IO;
with EU.BE.Results;
with EU.BE;
with EU.BE.Model.Runner;
with EU.BE.Model.Settings;
with EU.BE.Model;
with EU.Web.Settings;

with Google_Chart_Constants;
with Inequality_Generator;
with Poverty_Tabulator.Web_IO;
with Text_Buffer;
with Text_Utils;
with T_Utils.Standard_Chart_Generator;
with Templates_Parser;  

package body EU.BE.Output.Tests is

   use Text_Utils;
   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   package ms renames EU.BE.Model.Settings;
   
   model_sett   : ms.Model_Settings;
   
   personal_level_results_pre  : Detailed_Results_List; 
   hh_level_list_pre           : Detailed_Results_List;
      
   personal_level_results_post : Detailed_Results_List; 
   hh_level_list_post          : Detailed_Results_List;
   
   households : EU_Data_Set;

   procedure Set_Up (T : in out Test_Case) is
      use EU.BE.Results.IO;
          username : constant String := "default";
          run_name : constant String := "default";
   begin
      Put_Line( "read OK" );
      EU.BE.I81N.Load_Translations;
      model_sett :=  EU.BE.Globals.Get_Default_Model_Settings;
      personal_level_results_pre := EU.BE.Globals.Get_Personal_Level_Results_Pre; 
      hh_level_list_pre          := EU.BE.Globals.Get_HH_Level_List_Pre;
      households := EU.BE.Globals.Get_Households;
      declare
         sep : constant String := EU.Web.Settings.Dir_Separator;
      begin   
         model_sett.Set_Run_Id( run_name );
         Read_To_List( model_sett.Qualified_Output_File_Name, personal_level_results_post, hh_level_list_post );
      end;
   end Set_Up;
   
   procedure Test_Dump_Outputs(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      null; 
   end Test_Dump_Outputs;
   
   procedure Test_Inequality( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      package iqg is new Inequality_Generator( Rate=>Rate, Amount=>Amount );
      quant : iqg.Quantile;
      quant_list : iqg.Quantile_List;
      ineq  : iqg.Inequality_Array;
      bins  :  iqg.Quantile_List;
      n     : Positive;
   begin
      quant := ( index=>1, population=>1_145_008.0,income=>2954.35);
      iqg.Quantile_Package.append(quant_list, quant );
      iqg.Quantile_Package.append(quant_list, ( index=>1, population=>1274868.0, income=>9680.00 ));
      iqg.Quantile_Package.append(quant_list, ( index=>2, population=>1489169.0, income=>18586.47 ));
      iqg.Quantile_Package.append(quant_list, ( index=>3, population=>1309984.0, income=>22810.09 ));
      iqg.Quantile_Package.append(quant_list, ( index=>4, population=>1227877.0, income=>27624.27 ));
      iqg.Quantile_Package.append(quant_list, ( index=>5, population=>1333681.0, income=>3671.367 ));
      iqg.Quantile_Package.append(quant_list, ( index=>6, population=>3136635.0, income=>110401.13 ));
      iqg.Quantile_Package.append(quant_list, ( index=>7, population=>3619401.0, income=>162869.10 ));
      iqg.Quantile_Package.append(quant_list, ( index=>8, population=>3105688.0, income=>170061.28 ));
      iqg.Quantile_Package.append(quant_list, ( index=>9, population=>3252768.0, income=>217926.70 ));
      iqg.Quantile_Package.append(quant_list, ( index=>10, population=>3383398.0, income=>291369.02 ));
      iqg.Quantile_Package.append(quant_list, ( index=>11, population=>3126897.0, income=>420418.97 ));
      iqg.Quantile_Package.append(quant_list, ( index=>12, population=>207672.0, income=>68629.44 ));
      iqg.Quantile_Package.append(quant_list, ( index=>13, population=>49031.0, income=>32752.28 ));
      iqg.Quantile_Package.append(quant_list, ( index=>14, population=>13820.0, income=>18659.36 ));
      iqg.Quantile_Package.append(quant_list, ( index=>15, population=>5249.0, income=>15461.40 ));
      iqg.Quantile_Package.append(quant_list, ( index=>16, population=>1247.0, income=>8458.45 ));
      iqg.Quantile_Package.append(quant_list, ( index=>17, population=>686.0, income=>14801.38 ));
      new_line;put( "Inequality measures; germany 1999" );new_line;
      ineq := iqg.generate( quant_list );
      put( iqg.To_String( ineq ) );
      
      new_line;
      new_line;
      iqg.Quantile_Package.clear( quant_list );
      for i in 1..36 loop
         iqg.Quantile_Package.append(quant_list, ( index=>i, population=>1.0, income=>1.0 ));
      end loop;
      iqg.Quantile_Package.append( quant_list, ( index=>37, population=>1.0, income=>1.0E+08 ));
      ineq := iqg.generate( quant_list );
      put( iqg.To_String( ineq ) );
      
      iqg.Sort_By_Income( quant_list ); 
      
      bins := iqg.Binify( quant_list, 3 );
      n :=  Positive(iqg.Quantile_Package.Length( bins ));
      for p in 1 .. n loop
          put( Positive'Image(p) & " = " & iqg.To_String( iqg.Quantile_Package.element( bins, p ) ));
          new_line;
      end loop;
   end Test_Inequality;

   
   procedure Test_Load_Output( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use EU.BE.Output.Web_IO;
   use Poverty_By_Tenure_Package;
   use EU.BE.I81N;
   use Ada.Text_IO;
   use Templates_Parser;  
   
      package pio is new Poverty_By_Tenure_Package.Web_IO;
      outputs : Outputs_Rec;
      hh : Household_Rec;
      html : Unbounded_String;
      poverty_table   : Unbounded_String;
      poverty_gallery : Unbounded_String;
      poverty_html    : Unbounded_String;
      
      table    : Unbounded_String;
      gallery  : Unbounded_String;
      controls : Unbounded_String;
      popup_link : Unbounded_String;
      f               : File_Type;
      translations    : Translate_Set;
   begin
      Initialise_Outputs( outputs );
      -- FIXME
      -- Assert( households.Number_Of_Households = 3125, " load of ds didn't return 3125 households was: " & households.Number_Of_Households'Img );
      -- Assert( households.Number_Of_People = 7719, " load of ds didn't return 7719 people was: " & households.Number_Of_People'Img );
      hh := households.Read_Household( 1 );
      -- Assert( abs( hh.people( 1 ).demographic_weight - 516.61 ) < 0.01, " 1st household p1 gf should be 516.61 was " & Amount'Image( hh.people( 1 ).demographic_weight ));
      hh := households.Read_Household( 3125 );
      -- Assert( abs( hh.people( 3 ).demographic_weight - 558.24 ) < 0.01, " last household p3 gf should be  516.61 was " & Amount'Image( hh.people( 1 ).demographic_weight ));
      
      Create_Outputs( 
         households,
         hh_level_list_pre, 
         hh_level_list_post, 
         personal_level_results_pre,
         personal_level_results_pre,
         model_sett,
         outputs );
         
       controls := Get_Inequality_Control_Section; 
       -- table := Get_Inequality_Table( outputs );
       gallery :=  Get_Inequality_Chart_Grid( outputs );
       html := Get_Output_Page(      
                              translations    => translations,
                              which_page      => inequality_page,
                              control_section => controls,
                              gallery         => gallery,
                              content         => table,
                              lang            => en );
       Create( f, Out_File, "web/inequality_overall.html" );
       Put_Line( f, TS( html ));
       Close( f );
       
       controls := Get_Inequality_Control_Section; 
       -- table := Get_Inequality_Table( outputs, by_tenure );
       gallery :=  Get_Inequality_Chart_Grid( outputs );
       html := Get_Output_Page(      
                              translations    => translations,
                              which_page      => inequality_page,
                              control_section => controls,
                              gallery         => gallery,
                              content         => table,
                              lang            => en );
       Create( f, Out_File, "web/inequality_by_tenure.html" );
       Put_Line( f, TS( html ));
       Close( f );
       
       controls := Get_Budget_Control_Section; 
       table := Get_Budget_Table( outputs, 1, by_tenure );
       gallery :=  Get_Budget_Chart_Grid( outputs );
       
       popup_link := Get_Budget_Chart_Popup_Link(
         outputs, by_tenure, en );
       
       html := Get_Output_Page(      
                              translations    => translations,
                              which_page      => inequality_page,
                              control_section => controls,
                              gallery         => gallery,
                              content         => table,
                              lang            => en );
       Create( f, Out_File, "web/budget_by_tenure.html" );
       Put_Line( f, TS( html ));
       Close( f );

       Create( f, Out_File, "web/budget_popups.html" );
       Put_Line( f, TS( gallery ));
       Close( f );

       Create( f, Out_File, "web/budget_popup_link.html" );
       Put_Line( f, TS( popup_link ));
       Close( f );

       controls := Get_Gain_Lose_Control_Section; 
       table := Get_Gain_Lose_Table( outputs, by_tenure );
       gallery :=  Get_Gain_Lose_Chart_Grid( outputs, by_tenure );
       html := Get_Output_Page(      
                              translations    => translations,
                              which_page      => inequality_page,
                              control_section => controls,
                              gallery         => gallery,
                              content         => table,
                              lang            => en );
       Create( f, Out_File, "web/gain_lose_by_tenure.html" );
       Put_Line( f, TS( html ));
       Close( f );
       
       
       for t in Tenure_Type loop
          Put_Line( t'Img & " (1) : " & Be_Inequality.To_String( outputs.ineq_by_tenure(1)( t ).inequality_measures ));
          Put_Line( t'Img & " (2) : " & Be_Inequality.To_String( outputs.ineq_by_tenure(2)( t ).inequality_measures ));
       end loop;
       Put_Line( "Population (1) : " & Be_Inequality.To_String( outputs.ineq(1).inequality_measures ));
       Put_Line( "Population (2) : " & Be_Inequality.To_String( outputs.ineq(2).inequality_measures ));
 
       poverty_gallery := Get_Poverty_Chart_Grid( outputs, en );
       Put_Line( "=========  Google chart for poverty sys 1 ======= " & LINE_BREAK & TS( poverty_gallery ));
       poverty_table :=  Get_Poverty_Table( outputs, 2, by_tenure, True );
       Put_Line( "=========  Inequality by Tenure sys 1/2 differences (with 0 pl) ======= " & LINE_BREAK & TS( poverty_table ));
       
       controls := Get_Poverty_Control_Section; 
       poverty_html := Get_Output_Page(
                              translations    => translations,
                              which_page      => poverty_page,
                              control_section => controls,
                              gallery         => poverty_gallery,
                              content         => poverty_table,
                              lang            => en );

       Put_Line( "=========  Google chart for poverty sys 1 ======= " & LINE_BREAK & TS( poverty_html ));
       Create( f, Out_File, "web/poverty.html" );
       Put_Line( f, TS( poverty_html ));
       Close( f );
    end Test_Load_Output;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests( t : in out Test_Case ) is
   begin 
      -- Register_Routine( T, Test_Inequality'Access, "Test_Inequality" );
      Register_Routine( T, Test_Load_Output'Access, "Test_Load_Output" );
   end Register_Tests;
   

   ----------
   -- Name --
   ----------
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( "EU.BE.Results.IO.Tests" );
   end Name;

end EU.BE.Output.Tests;
