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

with Inequality_Generator;
with Ada.Text_IO;
with Tabulator.Text_IO;
with Costs_Tabulator.Text_IO;
with Poverty_Tabulator.Text_IO;
with IO_Commons;
with Text_Utils;
with Ada.Assertions;
with EU_Logger;

package body EU.BE.Output is

   use Ada.Text_IO;
   use IO_Commons;
   use Text_Utils;
   
   procedure Log( s : String ) is
   begin
      EU_Logger.Log( EU_Logger.output, s );
   end Log;

   
   function Is_Initialised( outputs : Outputs_Rec ) return Boolean is
   begin
      return outputs.initialised;
   end Is_Initialised;
   
   function Get_Null_Data return Outputs_Rec is
      d :  Outputs_Rec;
   begin
      d.initialised := False;
      return d;
   end Get_Null_Data;
   
   function Pretty_Print( t : Breakdown_Target ) return String is
   begin
      return Breakdown_Target'Image( t );
   end Pretty_Print;
   
   procedure Clear( lg : in out Lorenz_And_Gini ) is
   begin
      lg.gini  := 0.0;
      lg.inequality_measures := ( others => 0.0 );
      lg.lorenz.Clear;
   end Clear;
    
   
   function Create_Lorenz_And_Gini( 
      all_points               : Inequality_List; 
      target_type              : Breakdown_Target;
      which_tenure             : Tenure_Type := Tenure_Type'First;
      which_decile             : Deciles := Deciles'First;
      which_age_of_head        : Adult_Age_Band;
      which_occupation_of_head : Occupation_Isco_1_Digit;
      num_bins                 : Natural := LORENZ_BIN_SIZE ) return Lorenz_And_Gini is
      
      points_for_output : be_inequality.Quantile_List;
      
      procedure Add_To_Disaggregated( c : Inequality_Package.Cursor ) is
         iq : Inequality_Record := Inequality_Package.Element( c );
         include_this_point : Boolean := False;
         quant : be_inequality.Quantile;
      begin
         -- if( iq.income > 0.0 ) then
            case target_type is
               when no_breakdown          => include_this_point := True;
               when by_tenure             => include_this_point := iq.tenure = which_tenure;
               when by_decile             => include_this_point := iq.decile = which_decile;
               when by_age_of_head        => include_this_point := iq.age_of_head = which_age_of_head;
               when by_occupation_of_head => include_this_point := iq.occupation_of_head = which_occupation_of_head;
            end case;
            if( include_this_point ) then
               quant.income := iq.income;
               quant.population := iq.population;
               be_inequality.Quantile_Package.Append( points_for_output, quant );
            end if;
         -- end if;
      end Add_To_Disaggregated;
      
      
      use Ada.Containers;
      lg : Lorenz_And_Gini;
      inequality_measures : be_inequality.Inequality_Array;
      lorenz        : be_inequality.Quantile_List;
   begin
      Clear( lg );
      Inequality_Package.Iterate( all_points, Add_To_Disaggregated'Access );
      if( be_inequality.Quantile_Package.Length( points_for_output ) > 0 ) then
         be_inequality.Sort_By_Income( points_for_output );
         inequality_measures := be_inequality.Generate( points_for_output );
         lg.gini := inequality_measures( be_inequality.gini ) * 100.0;
         lg.inequality_measures := inequality_measures;
         lg.lorenz := be_inequality.Binify( points_for_output, num_bins );
      end if;
      return lg;
   end Create_Lorenz_And_Gini;

   procedure Print_Gini( 
      f           : File_Type; 
      target_type : Breakdown_Target;
      breakdown   : String;
      sys_no      : Positive; 
      lg          : Lorenz_And_Gini ) is
      use be_inequality;
      use Quantile_Package;
      quant : Quantile;
      top_quant : Quantile;
      num_bins    : constant Natural := Natural(Length( lg.lorenz ));
   begin
      Put( f, """lorenz""," );
      Put( f, """" & Pretty_Print( target_type ) & """," );
      Put( f, """" & breakdown & """," );
      Put( f, Rate'Image( lg.gini ) & "," );
      Put( f, "2009," );
      Put( f, Positive'Image( sys_no ) & "," );
      Put_Line( f, Natural'Image( num_bins + 1 ) ); -- 1 extra for zero starting point
      top_quant := Element( lg.lorenz, num_bins );
      Put_Line( f, "0.0,0.0" ); -- starting point      
      for i in 1 .. num_bins loop
         quant := Element( lg.lorenz, i );
         Put_Line( 
            f, 
            Rate'Image( quant.population/top_quant.population ) & "," &
            Rate'Image( quant.income/top_quant.income ));
      end loop;
   end Print_Gini;
   
   --
   -- FIXME 
   --
   procedure Print_All_Ginis( 
      f               : File_Type; 
      sys_no          : Positive; 
      all_net_incomes : Inequality_List ) is
      lg : Lorenz_And_Gini;
   begin
      Print_Gini( 
         f, 
         no_breakdown, 
         "no_breakdown", 
         sys_no, 
         lg );
      for tenure in Tenure_Type loop -- skip all the junk ones
         Print_Gini( 
            f, 
            by_tenure, 
            Censor_String( Tenure_Type'Image( tenure )), 
            sys_no, 
            lg );
      end loop;

   end Print_All_Ginis;
   
   package Gain_Lose_By_Tenure_Printer is new 
      Gain_Lose_By_Tenure.Text_IO;
      
   package Poverty_By_Tenure_Printer is new 
      Poverty_By_Tenure_Package.Text_IO;
   
   package Household_Costs_By_Tenure_Printer is new 
      Household_Costs_By_Tenure.Text_IO;
      
   procedure Print_Outputs( 
      filename : String; 
      outputs  : in out Outputs_Rec ) is
      f : File_Type;
   begin
      Put_Line( "Output written to |" & filename & "|" );
      Create( f, Out_File, filename );
      
      Household_Costs_By_Tenure_Printer.Print(
         f,
         "budget_table_sys_1",
         outputs.costs_by_tenure( 1 ),
         2,
         "tenure",
         "taxes",
         2009,
         1
         );
      Household_Costs_By_Tenure_Printer.Print(
         f,
         "budget_table_sys_2",
         outputs.costs_by_tenure( 2 ),
         7,
         "tenure",
         "taxes",
         2009,
         2
         );
      --
      -- missing columns in poverty tables
      -- 
      Poverty_By_Tenure_Package.Complete_Table( outputs.poverty_by_Tenure( 1 ));
      Poverty_By_Tenure_Package.Complete_Table( outputs.poverty_by_Tenure( 2 ));
      
      --
      -- poverty sys 1
      --
      Poverty_By_Tenure_Printer.Print(
         f,
         "poverty_status_sys_1",
         outputs.poverty_by_tenure( 1 ),
         12,
         "tenure",
         2009,
         1
         );
      Poverty_By_Tenure_Printer.Print(
         f,
         "poverty_status_sys_2",
         outputs.poverty_by_tenure( 2 ),
         16,
         "tenure",
         2009,
         2
         );
      --
      -- Gain/Lose tables
      -- 
         
      Gain_Lose_By_Tenure_Printer.Print( 
         f, 
         "gains_by_tenure",
         outputs.gains_by_tenure, 
         NO_CHANGE_COL, 
         "change_income", 
         "tenure", 
         2009, 
         21 );
      -- FIXME TODO
      -- Print_All_Ginis( f, 1, outputs.all_net_incomes_1 );
      -- Print_All_Ginis( f, 2, outputs.all_net_incomes_2 );
      Close( f );
   end Print_Outputs;
   
   procedure Initialise_Outputs( outputs : in out Outputs_Rec ) is
   begin
     null;
   end Initialise_Outputs;

   function Calculate_Poverty_Gap_Per_Person( hh : Household_Rec; poverty_level : Amount; result : Detailed_Record ) return Amount is
   begin
      return  poverty_level - (result.standard_disposable_income/hh.Get_Equivalence_Scale );
   end Calculate_Poverty_Gap_Per_Person;      
   
   procedure Create_Summary_Statistics( outputs  : in out Outputs_Rec ) is
   use Tabulator_Commons;
      -- express in millions pa & reverse the sign for total cost
      gain : Amount := outputs.summary_statistics( average_gain ) * 12.0/1000.0;
   begin
      if( outputs.summary_statistics( hh_count )) > 0.0 then
         outputs.summary_statistics( average_gain ) := 
            outputs.summary_statistics( average_gain ) / outputs.summary_statistics( hh_count ); 
         outputs.summary_statistics( percent_gaining ) := 
            100.0 * outputs.summary_statistics( percent_gaining ) / outputs.summary_statistics( hh_count );
         outputs.summary_statistics( percent_losing ) := 
            100.0 * outputs.summary_statistics( percent_losing ) / outputs.summary_statistics( hh_count );
         outputs.summary_statistics( total_cost ) := gain;
         outputs.summary_statistics( change_in_poverty_rate ) := 
            outputs.poverty_by_decile( 2 ).totals( people_in_poverty ) - 
            outputs.poverty_by_decile( 1 ).totals( people_in_poverty );
         outputs.summary_statistics( change_in_gini ) := 
            outputs.ineq( 2 ).inequality_measures( be_inequality.Gini ) - 
            outputs.ineq( 1 ).inequality_measures( be_inequality.Gini );
      end if;
   end Create_Summary_Statistics;
   
   procedure Add_To_Outputs( 
      hh_ref   : Positive;
      hh       : Household_Rec; 
      result   : in out Detailed_Record_Array; 
      outputs  : in out Outputs_Rec ) is
      
      breakdown            : Cell_Breakdown;
      column               : Scale_Range;
      exmpl                : Example;
      all_tbs              : All_Taxes_And_Benefits_Array_By_Sys;
      summary              : Summary_Items_Array_By_Sys;
      age_b                : Adult_Age_Band;
      change_in_net_income : Amount;
      weighted_people      : constant Amount := 
               hh.people(1).demographic_weight * Amount( hh.num_people );
   begin
      Log( "age is " & hh.people( 1 ).demographic_age'Img );
      age_b := Age_Band'Max( Adult_Age_Band'First, Get_Age_Band( hh.people( 1 ).demographic_age )); -- lock off under 16s
      breakdown.tenure := hh.assets_main_residence_tenure;
      
      for sysno in 1 .. 2 loop
         all_tbs( sysno ) := Map( result( sysno ));
         summary( sysno ) := Map( result( sysno ));
         for i in  All_Taxes_And_Benefits_Type loop
             -- costs to annual thousands
            all_tbs( sysno )( i ) := all_tbs( sysno )( i ) * 12.0/1_000.0;
         end loop;
      end loop;
     
      --
      -- since all tables at household level, no other fields needed in example
      --
      exmpl.hh_ref := hh_ref;
      --
      -- Costs table
      --
      for sysno in 1 .. 2 loop
         Household_Costs_By_Tenure.Add_Observation( 
                outputs.costs_by_tenure( sysno ),
                hh.assets_main_residence_tenure, 
                hh.people( 1 ).demographic_weight, 
                all_tbs( sysno ));
         Household_Costs_By_Decile.Add_Observation( 
                outputs.costs_by_decile( sysno ),
                hh.equivalent_income_decile, 
                hh.people( 1 ).demographic_weight, 
                all_tbs( sysno ));
         Household_Costs_By_Age_Band.Add_Observation( 
                outputs.costs_by_age_band( sysno ),
                age_b, 
                hh.people( 1 ).demographic_weight,
                all_tbs( sysno ));
         Household_Costs_By_Occupation.Add_Observation( 
                outputs.costs_by_occupation( sysno ),
                hh.people( 1 ).labour_market_occupation_isco_1_digit, 
                hh.people( 1 ).demographic_weight, 
                all_tbs( sysno ));
      end loop;
      
      change_in_net_income := result( 2 ).standard_disposable_income -
        result( 1 ).standard_disposable_income;
      outputs.summary_statistics( average_gain ) :=
          outputs.summary_statistics( average_gain ) + 
            ( change_in_net_income * hh.people(1).demographic_weight );     
      outputs.summary_statistics( hh_count ) :=
          outputs.summary_statistics( hh_count ) + 
            hh.people(1).demographic_weight;     
      outputs.summary_statistics( pers_count ) :=
          outputs.summary_statistics( pers_count ) + 
          weighted_people;
          
      if( change_in_net_income > 0.01 )then
          outputs.summary_statistics( percent_gaining ) :=
                  outputs.summary_statistics( percent_gaining ) + hh.people(1).demographic_weight;     
      elsif( change_in_net_income < -0.01 )then
          outputs.summary_statistics( percent_losing ) :=
                  outputs.summary_statistics( percent_losing ) + hh.people(1).demographic_weight;     
      end if;
      column := Gain_Lose_By_Tenure.Get_Col_Range(
            result( 1 ).standard_disposable_income,
            result( 2 ).standard_disposable_income,
            Gain_Lose_Scale,
            pct_change );

      Gain_Lose_By_Tenure.Add_Observation( 
            outputs.gains_by_tenure, 
            hh.assets_main_residence_tenure, 
            column, 
            hh.people( 1 ).demographic_weight, 
            summary( 1 ),
            summary( 2 ),
            breakdown,
            exmpl );
      Gain_Lose_By_Age_Band.Add_Observation( 
            outputs.gains_by_age_band, 
            age_b, 
            column, 
            hh.people( 1 ).demographic_weight, 
            summary( 1 ),
            summary( 2 ),
            breakdown,
            exmpl );
      Gain_Lose_By_Occupation.Add_Observation( 
            outputs.gains_by_occupation, 
            hh.people( 1 ).labour_market_occupation_isco_1_digit, 
            column, 
            hh.people( 1 ).demographic_weight, 
            summary( 1 ),
            summary( 2 ),
            breakdown,
            exmpl );
      Gain_Lose_By_Decile.Add_Observation( 
            outputs.gains_by_decile, 
            hh.equivalent_income_decile, 
            column, 
            hh.people( 1 ).demographic_weight, 
            summary( 1 ),
            summary( 2 ),
            breakdown,
            exmpl );
      --
      --
      --
      result( 1 ).poverty_gap := Calculate_Poverty_Gap_Per_Person( hh, outputs.poverty_line_per_person( 1 ), result( 1 ) ); -- note we use the base poverty line all the time       
      result( 2 ).poverty_gap := Calculate_Poverty_Gap_Per_Person( hh, outputs.poverty_line_per_person( 1 ), result( 2 ) ); -- even though we calculate the reform one
      
      for sysno in 1 .. 2 loop
         Poverty_By_Decile_Package.Add_Observation( 
                outputs.poverty_by_decile( sysno ),
                result( sysno ).poverty_gap,
                hh.num_people,
                hh.people( 1 ).equivalent_income_decile, 
                hh.people( 1 ).demographic_weight );
         Poverty_By_Tenure_Package.Add_Observation( 
                outputs.poverty_by_tenure( sysno ),
                result( sysno ).poverty_gap,
                hh.num_people,
                hh.assets_main_residence_tenure, 
                hh.people( 1 ).demographic_weight );
         Poverty_By_Age_Band_Package.Add_Observation( 
                outputs.poverty_by_age_band( sysno ),
                result( sysno ).poverty_gap,
                hh.num_people,
                age_b, 
                hh.people( 1 ).demographic_weight );
         Poverty_By_Occupation_Package.Add_Observation( 
                outputs.poverty_by_occupation( sysno ),
                result( sysno ).poverty_gap,
                hh.num_people,
                hh.people( 1 ).labour_market_occupation_isco_1_digit, 
                hh.people( 1 ).demographic_weight );
      end loop;
   end Add_To_Outputs;
   
   function Get_Detailed_Output( outputs : Outputs_Rec; hhref : Positive ) return Detailed_Record is
   	d : Detailed_Record;
   begin
   	return d;
      -- return outputs.hh_level_results_post.Element( hhref );
   end Get_Detailed_Output;
   
   function Get_Head_Of_Household( 
      hh      : Household_Rec;
      results : Detailed_Record ) return Person_Range is
   begin
      for pno in 1 .. hh.num_people loop
         if( hh.people( pno ).identifier_person = results.head_of_household_indentifier )then
            return pno;
         end if;
      end loop;
      return 1;
   end Get_Head_Of_Household;

   procedure Create_Outputs(
      households : EU_Data_Set;
      hh_level_results_1   : Detailed_Results_List;
      hh_level_results_2   : Detailed_Results_List;
      pers_level_results_1 : Detailed_Results_List;
      pers_level_results_2 : Detailed_Results_List;
      settings             : Model_Settings;
      outputs              : in out Outputs_Rec ) is
      hh : Household_Rec;   
   use Ada.Assertions;
   use be_inequality;
      all_net_incomes_1           : Inequality_List;
      all_net_incomes_2           : Inequality_List;
      all_equiv_incomes_1         : Quantile_List;
      all_equiv_incomes_2         : Quantile_List;
      ir_1                        : Inequality_Record;
      ir_2                        : Inequality_Record;      
      pov_1                       : Quantile;
      pov_2                       : Quantile;      
      result                      : Detailed_Record_Array;       
      equiv_inequality_measures_1 : Inequality_Array;
      equiv_inequality_measures_2 : be_inequality.Inequality_Array;      
      decile                      : Deciles := Deciles'First;
      age_of_head                 : Adult_Age_Band;
      occupation_of_head          : Occupation_Isco_1_Digit;      
      head_id                     : Person_Range;
   begin
      Log( "Create_Outputs entered" );
      outputs.initialised := True;
      
      -- outputs.hh_level_results_post := hh_level_results_2;
      for hhref in 1 .. households.Number_Of_Households loop
         hh := households.Read_Household( hhref );
         
         result( 1 ) := hh_level_results_1.Element( hhref );
         result( 2 ) := hh_level_results_2.Element( hhref );

         head_id := Get_Head_Of_Household( hh, result( 1 ));
         age_of_head := Get_Age_Band( hh.people( head_id ).demographic_age );

         occupation_of_head := hh.people( 1 ).labour_market_occupation_isco_1_digit;
         Assert( result( 1 ).identifier_household = hh.people( 1 ).identifier_household, 
            "result( 1 ).identifier_household /= hh.identifier_household; hhref=" & Integer'Image( hhref ) &
            "result( 1 ).identifier_household = " & Integer'Image( result( 1 ).identifier_household ) & 
            "hh.identifier_household = " & Integer'Image( hh.people( 1 ).identifier_household ));
         Assert( result( 2 ).identifier_household = hh.people( 1 ).identifier_household, 
            "result( 2 ).identifier_household /= hh.identifier_household; hhref=" & Integer'Image( hhref ) &
            "result( 2 ).identifier_household = " & Integer'Image( result( 2 ).identifier_household ) & 
            "hh.identifier_household = " & Integer'Image( hh.people( 1 ).identifier_household ));
            
         ir_1.occupation_of_head := occupation_of_head;
         ir_2.occupation_of_head := occupation_of_head;
         ir_1.age_of_head := age_of_head;
         ir_2.age_of_head := age_of_head;
         ir_1.tenure := hh.assets_main_residence_tenure;
         ir_2.tenure := hh.assets_main_residence_tenure;
         ir_1.decile := hh.people( 1 ).equivalent_income_decile;
         ir_2.decile := hh.people( 1 ).equivalent_income_decile;
          
         ir_1.income := Amount( hh.num_people ) * result( 1 ).standard_disposable_income * hh.people( 1 ).demographic_weight / hh.Get_Equivalence_Scale;
         ir_2.income := Amount( hh.num_people ) * result( 2 ).standard_disposable_income * hh.people( 1 ).demographic_weight / hh.Get_Equivalence_Scale;
         -- ir_1.population := hh.people( 1 ).demographic_weight * Amount( hh.num_people );
         -- ir_2.population := hh.people( 1 ).demographic_weight * Amount( hh.num_people );
         ir_1.population := hh.people( 1 ).demographic_weight * Amount( hh.num_people );
         ir_2.population := hh.people( 1 ).demographic_weight * Amount( hh.num_people );

         Assert( ir_1.population > 0.0, " ir1.population should be > 0; was" & Amount'Image( ir_1.population ) & " for hh " & hhref'Img );
         Assert( ir_2.population > 0.0, " ir2.population should be > 0; was" & Amount'Image( ir_2.population ) & " for hh " &hhref'Img );
         
         all_net_incomes_1.Append( ir_1 );
         all_net_incomes_2.Append( ir_2 );
         
         pov_1.income := result( 1 ).standard_disposable_income * hh.people( 1 ).demographic_weight / hh.Get_Equivalence_Scale;
         pov_2.income := result( 2 ).standard_disposable_income * hh.people( 1 ).demographic_weight / hh.Get_Equivalence_Scale;
         pov_1.population := hh.people( 1 ).demographic_weight;
         pov_2.population := hh.people( 1 ).demographic_weight;
         for pno in 1 .. hh.num_people loop
            if( pov_1.income <= 0.0 )then
               pov_1.income := 0.01;
            end if;
            all_equiv_incomes_1.Append( pov_1 );
            if( pov_2.income <= 0.0 )then
               pov_2.income := 0.01;
            end if;
            all_equiv_incomes_2.Append( pov_2 );
         end loop;
      end loop;
      Log( "Main Loop Completed" );
      be_inequality.Sort_By_Income( all_equiv_incomes_1 );
      be_inequality.Sort_By_Income( all_equiv_incomes_2 );
     
      equiv_inequality_measures_1 := be_inequality.Generate( all_equiv_incomes_1 );
      equiv_inequality_measures_2 := be_inequality.Generate( all_equiv_incomes_2 );
      
      outputs.poverty_line_per_person( 1 ) := equiv_inequality_measures_1( MEDIAN ) * 0.60;
      outputs.poverty_line_per_person( 2 ) := equiv_inequality_measures_2( MEDIAN ) * 0.60;
      
      outputs.ineq( 1 ) := Create_Lorenz_And_Gini( 
         all_net_incomes_1, 
         no_breakdown,
         Tenure_Type'First,
         Deciles'First,
         Adult_Age_Band'First,
         Occupation_Isco_1_Digit'First,
         LORENZ_BIN_SIZE );
      outputs.ineq( 2 ) := Create_Lorenz_And_Gini( 
         all_net_incomes_2, 
         no_breakdown,
         Tenure_Type'First,
         Deciles'First,
         Adult_Age_Band'First,
         Occupation_Isco_1_Digit'First,
         LORENZ_BIN_SIZE );
      Log( "Main Inequality Completed" );
      for tenure in Tenure_Type loop -- skip all the junk ones
         outputs.ineq_by_tenure( 1 )( tenure ) := Create_Lorenz_And_Gini( 
            all_net_incomes_1, 
            by_tenure,
            tenure,
            Deciles'First,
            Adult_Age_Band'First,
            Occupation_Isco_1_Digit'First,
            LORENZ_BIN_SIZE );
         outputs.ineq_by_tenure( 2 )( tenure ) := Create_Lorenz_And_Gini( 
            all_net_incomes_2, 
            by_tenure,
            tenure,
            Deciles'First,
            Adult_Age_Band'First,
            Occupation_Isco_1_Digit'First,
            LORENZ_BIN_SIZE );
      end loop;
      Log( "Tenure Loop Completed" );
      for decile in Deciles loop -- skip all the junk ones
         outputs.ineq_by_decile( 1 )( decile ) := Create_Lorenz_And_Gini( 
            all_net_incomes_1, 
            by_decile,
            Tenure_Type'First,
            decile,
            Adult_Age_Band'First,
            Occupation_Isco_1_Digit'First,
            LORENZ_BIN_SIZE );
         outputs.ineq_by_decile( 2 )( decile ) := Create_Lorenz_And_Gini( 
            all_net_incomes_2, 
            by_decile,
            Tenure_Type'First,
            decile,
            Adult_Age_Band'First,
            Occupation_Isco_1_Digit'First,
            LORENZ_BIN_SIZE );
      end loop;
      Log( "Decile Loop Completed" );
      
      for age_band in Adult_Age_Band loop -- skip all the junk ones
         outputs.ineq_by_age_band( 1 )( age_band ) := Create_Lorenz_And_Gini( 
            all_net_incomes_1, 
            by_age_of_head,
            Tenure_Type'First,
            Deciles'First,
            age_band,
            Occupation_Isco_1_Digit'First,
            LORENZ_BIN_SIZE );
         outputs.ineq_by_age_band( 2 )( age_band ) := Create_Lorenz_And_Gini( 
            all_net_incomes_2, 
            by_age_of_head,
            Tenure_Type'First,
            Deciles'First,
            age_band,
            Occupation_Isco_1_Digit'First,
            LORENZ_BIN_SIZE );
      end loop;
      Log( "Age_Band Loop Completed" );
      for occ in Occupation_Isco_1_Digit loop -- skip all the junk ones
         outputs.ineq_by_occupation( 1 )( occ ) := Create_Lorenz_And_Gini( 
            all_net_incomes_1, 
            by_occupation_of_head,
            Tenure_Type'First,
            Deciles'First,
            Adult_Age_Band'First,
            occ,
            LORENZ_BIN_SIZE );
         outputs.ineq_by_occupation( 2 )( occ ) := Create_Lorenz_And_Gini( 
            all_net_incomes_2, 
            by_occupation_of_head,
            Tenure_Type'First,
            Deciles'First,
            Adult_Age_Band'First,
            occ,
            LORENZ_BIN_SIZE );
      end loop;
      Log( "Occupation Loop Completed" );

      for hhref in 1 .. households.Number_Of_Households loop
         hh := households.Read_Household( hhref );
         result( 1 ) := hh_level_results_1.Element( hhref );
         result( 2 ) := hh_level_results_2.Element( hhref );
         Add_To_Outputs( hhref, hh, result, outputs );
      end loop;
      Log( "result(1); result(2) accumulated" );

      Poverty_By_Tenure_Package.Complete_Table( outputs.poverty_by_tenure( 1 ));
      Poverty_By_Tenure_Package.Complete_Table( outputs.poverty_by_tenure( 2 ));
      Poverty_By_Age_Band_Package.Complete_Table( outputs.poverty_by_age_band( 1 ));
      Poverty_By_Age_Band_Package.Complete_Table( outputs.poverty_by_age_band( 2 ));
      Poverty_By_Occupation_Package.Complete_Table( outputs.poverty_by_occupation( 1 ));
      Poverty_By_Occupation_Package.Complete_Table( outputs.poverty_by_occupation( 2 ));
      Poverty_By_Decile_Package.Complete_Table( outputs.poverty_by_decile( 1 ));
      Poverty_By_Decile_Package.Complete_Table( outputs.poverty_by_decile( 2 ));
      Log( "Poverty Completed" );
      Create_Summary_Statistics( outputs );
      Log( "Create_Summary_Statistics completed" );
      
      Print_Inequality_List( settings.Qualified_Output_Directory & "inequality_data_sys_1.txt", all_net_incomes_1 );
      Print_Inequality_List( settings.Qualified_Output_Directory & "inequality_data_sys_2.txt", all_net_incomes_2 );
      Log( "Inequality Printed" );
      Dump_All_Quantiles( settings.Qualified_Output_Directory & "equivalent_incomes_sys_1.txt", all_equiv_incomes_1 );
      Dump_All_Quantiles( settings.Qualified_Output_Directory & "equivalent_incomes_sys_2.txt", all_equiv_incomes_1 );
      Log( "Quantiles Dumped; returning from Create_Outputs" );
   end Create_Outputs;


end EU.BE.Output;
