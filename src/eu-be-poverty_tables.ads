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
with T_Utils;
with EU.BE.Household;
with EU.BE.Household.IO;
with EU.BE.Results;
with EU_Types;
with BE_Types;
with EU.BE.Commons;
with Costs_Tabulator;
with Poverty_Tabulator;

package EU.BE.Poverty_Tables is
   
   use EU.BE.Household;
   use EU.BE.Results;
   use EU_Types;
   use BE_Types;
   use EU.BE.Commons;

   package Poverty_By_Tenure_Package is new Poverty_Tabulator(
      Data_Type => Amount,
      Breakdown_Range => Tenure_Type,
      People_Count => Person_Count
   );
   package Poverty_By_Decile_Package is new Poverty_Tabulator(
      Data_Type => Amount,
      Breakdown_Range => Deciles,
      People_Count => Person_Count
   );
   package Poverty_By_Occupation_Package is new Poverty_Tabulator(
      Data_Type => Amount,
      Breakdown_Range => Occupation_Isco_1_Digit,
      People_Count => Person_Count
   );
   package Poverty_By_Age_Band_Package is new Poverty_Tabulator(
      Data_Type => Amount,
      Breakdown_Range => Adult_Age_Band,
      People_Count => Person_Count
   );
   
   
   type Poverty_By_Tenure_Array is array( System_Number'Range ) of Poverty_By_Tenure_Package.Table_Type;
   type Poverty_By_Decile_Array is array( System_Number'Range ) of Poverty_By_Decile_Package.Table_Type;
   type Poverty_By_Age_Band_Array is array( System_Number'Range ) of Poverty_By_Age_Band_Package.Table_Type;
   type Poverty_By_Occupation_Array is array( System_Number'Range ) of Poverty_By_Occupation_Package.Table_Type;
 

end EU.BE.Poverty_Tables;
