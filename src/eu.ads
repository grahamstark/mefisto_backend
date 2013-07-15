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
--
-- This is the top-level package for the EU model. It does very little: just
-- imports some standard types and generic formatting and calculation routines, so these can  
-- be used easily in the model itself, which is (almost) all declared in child packages.
-- 
-- Putting anything else in here may cause nasty cross-dependencies.
--
pragma License( Modified_GPL );

with Ada.Text_IO.Editing;

with BE_Base_Model_Types;
with EU_Types;
with Text_Utils;
with Key_Value_IO;

--
-- FIXME note that BE_Base_Model_Types should be in EU.BE or made more general
--
package EU is

   use BE_Base_Model_Types;
   use EU_Types;
   
   package EU_Key_Value_IO is new Key_Value_IO( Real_Type=>Amount,  Counter_Type=>Counter_Type );
   
   
end EU;
