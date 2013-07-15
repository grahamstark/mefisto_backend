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

with Ada.Containers.Vectors;

with Costs_Tabulator;
with Inequality_Generator;
with Tabulator;
with Tabulator_Commons;
with T_Utils;
with Poverty_Tabulator;
with EU.BE.Household;
with EU.BE.Household.IO;
with EU.BE.Results;
with EU_Types;
with BE_Types;
with EU.BE.Commons;
with EU.BE.Gain_Lose;
with EU.BE.Inequality;
with EU.BE.Budget;
with EU.BE.Poverty_Tables;
with EU.BE.Model.Settings;

package EU.BE.Output is

   use EU.BE.Household;
   use EU.BE.Household.IO;
   use EU.BE.Results;
   use EU_Types;
   use BE_Types;
   use EU.BE.Gain_Lose;
   use EU.BE.Inequality;
   use EU.BE.Budget;
   use EU.BE.Poverty_Tables;
   use EU.BE.Model.Settings;
   
   
   type Breakdown_Target is ( no_breakdown, by_decile, by_age_of_head, by_occupation_of_head,  by_tenure );
   subtype Disaggregated_Breakdown_Target is Breakdown_Target range by_decile .. Breakdown_Target'Last;
   
   type Summary_Statistics_Type is (
      average_gain,
      percent_gaining,
      percent_losing,
      total_cost,
      change_in_poverty_rate,
      change_in_gini,
      hh_count,
      pers_count );
      
   type Summary_Statistics_Array is array( Summary_Statistics_Type ) of Amount;
   
   function Pretty_Print( t : Breakdown_Target ) return String;
   
   type Outputs_Rec is tagged private;
   
   procedure Print_Outputs( filename : String; outputs : in out Outputs_Rec );
   
   procedure Initialise_Outputs( outputs : in out Outputs_Rec );

   procedure Create_Outputs(
      households : EU_Data_Set;
      hh_level_results_1   : Detailed_Results_List;
      hh_level_results_2   : Detailed_Results_List;
      pers_level_results_1 : Detailed_Results_List;
      pers_level_results_2 : Detailed_Results_List;
      settings             : Model_Settings;
      outputs              : in out Outputs_Rec );
   
   function Get_Null_Data return  Outputs_Rec;
   function Is_Initialised( outputs : Outputs_Rec ) return Boolean;
   function Get_Detailed_Output( outputs : Outputs_Rec; hhref : Positive ) return Detailed_Record;
   
   
private      

   use Tabulator_Commons;
   

   type Amount_Array is array( System_Number'Range ) of Amount;
     
   type Outputs_Rec is tagged record
      
      initialised             : Boolean := False;
      
      summary_statistics      : Summary_Statistics_Array := ( Others => 0.0 );

      poverty_line_per_person : Amount_Array;
      
      ineq                    : Lorenz_And_Gini_By_Sys_Array;
      
      gains_by_decile         : Gain_Lose_By_Decile.Table_Type;
      gains_by_tenure         : Gain_Lose_By_Tenure.Table_Type;
      gains_by_age_band       : Gain_Lose_By_Age_Band.Table_Type;
      gains_by_occupation     : Gain_Lose_By_Occupation.Table_Type;
      
      ineq_by_decile          : Lorenz_And_Gini_By_Decile_And_Sys_Array;
      ineq_by_tenure          : Lorenz_And_Gini_By_Tenure_And_Sys_Array;
      ineq_by_age_band        : Lorenz_And_Gini_By_Age_Band_And_Sys_Array;
      ineq_by_occupation      : Lorenz_And_Gini_By_Occupation_And_Sys_Array;
      
      costs_by_decile         : Household_Costs_By_Decile_Array;
      costs_by_tenure         : Household_Costs_By_Tenure_Array;
      costs_by_age_band       : Household_Costs_By_Age_Band_Array;
      costs_by_occupation     : Household_Costs_By_Occupation_Array;
      
      poverty_by_decile       : Poverty_By_Decile_Array;
      poverty_by_tenure       : Poverty_By_Tenure_Array;
      poverty_by_age_band     : Poverty_By_Age_Band_Array;
      poverty_by_occupation   : Poverty_By_Occupation_Array;
      -- commented out as test of memory hh_level_results_post   : Detailed_Results_List;
   end record;
   
   function Calculate_Poverty_Gap_Per_Person( hh : Household_Rec; poverty_level : Amount; result : Detailed_Record ) return Amount;
   
end  EU.BE.Output;
