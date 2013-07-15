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

with EU_Types;
with BE_Types;

package EU.BE.Commons is

   use EU_Types;
   use BE_Types;
   
   type Tenure_Array is array( Tenure_Type ) of Amount;
   type Marital_status_type_Array is array( Marital_status_type ) of Amount;
   type Citizenship_type_Array is array( Citizenship_type ) of Amount;
   type Gender_type_Array is array( Gender_type ) of Amount;
   type Education_highest_status_type_Array is array( Education_highest_status_type ) of Amount;
   type Deciles_Array is array( Deciles ) of Amount;
   type Age_Band_Array is array( Age_Band ) of Amount;
   type Occupation_Isco_1_Digit_Array is array(  Occupation_Isco_1_Digit ) of Amount;

end EU.BE.Commons;
