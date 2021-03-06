--
--  $Author: graham_s $
--  $Date: 2011-04-09 11:36:52 +0100 (Sat, 09 Apr 2011) $
--  $Revision: 11387 $
--
--  basic types for our models;
--  originally UK-centric, but now moved out since really
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Bounded; use Ada.Strings.Bounded;

package body BE_Base_Model_Types is

   function Standard_Ratio_Between( old_period : Periods; new_period : Periods ) return Rate is
      r : Rate;
   begin
      case old_period is
      when day =>
         case new_period is
            when day => r := 1.0;
            when week => r := 7.0;
            when month => r := 30.0;
            when year => r := 365.0;
         end case;
      when week =>
         case new_period is
            when day => r := 1.0/7.0;
            when week => r := 1.0;
            when month => r := 4.0;
            when year => r := 52.0;
         end case;
      when month =>
         case new_period is
            when day => r := 1.0/30.0;
            when week => r := 1.0/4.0;
            when month => r := 1.0;
            when year => r := 12.0;
         end case;
      when year =>
         case new_period is
            when day => r := 1.0/365.0;
            when week => r := 1.0/52.0;
            when month => r := 1.0/12.0;
            when year => r := 1.0;
         end case;
      end case;
      return r;
   end Standard_Ratio_Between;
   
   function Ratio_Between( old_period : Periods; new_period : Periods ) return Rate is
   begin
      return Rate( DAYS_PER_Period( old_period ))/Rate( DAYS_PER_Period( new_period ));
   end Ratio_Between;


   function Safe_Mult( a, b : Real ) return Real is
      as : Real := a;
      bs : Real := b;
   begin
      if (a = MISS_R) then
         as := 0.0;
      end if;
      if (b = MISS_R) then
         bs := 0.0;
      end if;
      return Amount (as * bs);
   end Safe_Mult;

   function Safe_Add( a, b : Real; c, d, e, f, g, h : Real := 0.0) return Amount is
      as : Rate := a;
      bs : Rate := b;
      cs : Rate := c;
      ds : Rate := d;
      es : Rate := e;
      fs : Rate := f;
      gs : Rate := g;
      hs : Rate := h;
   begin
      if (as = MISS_R) then
         as := 0.0;
      end if;
      if (bs = MISS_R) then
         bs := 0.0;
      end if;
      if (cs = MISS_R) then
         cs := 0.0;
      end if;
      if (ds = MISS_R) then
         ds := 0.0;
      end if;
      if (es = MISS_R) then
         es := 0.0;
      end if;
      if (fs = MISS_R) then
         fs := 0.0;
      end if;
      if (gs = MISS_R) then
         gs := 0.0;
      end if;
      if (hs = MISS_R) then
         hs := 0.0;
      end if;
      return Amount (as + bs + cs + ds + es + fs + gs + hs);

   end Safe_Add;

   function Safe_Add( a, b : Integer) return Integer is
      as : Integer := a;
      bs : Integer := b;
   begin
      if (as = MISS) then
         as := 0;
      end if;
      if (bs = MISS) then
         bs := 0;
      end if;
      return as + bs;
   end Safe_Add;

   function Safe_Add( a, b : Real) return Real is
      as : Real := a;
      bs : Real := b;
   begin
      if (as = MISS_R) then
         as := 0.0;
      end if;
      if (bs = MISS_R) then
         bs := 0.0;
      end if;
      return as + bs;
   end Safe_Add;


   function Safe_Assign( r : Real ) return Real is
   begin
      if (r = MISS_R) then
         return 0.0;
      end if;
      return r;
   end Safe_Assign;

   function Safe_Real_To_Int( r : Real ) return Integer is
   begin
      if (r = MISS_R) then
         return 0;
      end if;
      return Integer(r);
   end Safe_Real_To_Int;
   
   
   function Safe_Int_To_Real( r : Integer ) return Real is
   begin
      if (r = MISS) then
         return 0.0;
      end if;
      return Real(r);
   end Safe_Int_To_Real;

   function Zero_Or_Missing (r : Real) return Boolean is
   begin
      return ((r = 0.0) or (r = MISS_R));
   end Zero_Or_Missing;

  
   function Annual_To_Weekly( m : Amount ) return Amount is
   begin
      return m / 52.0;
   end Annual_To_Weekly;
   
   function Weekly_To_Annual( m : Amount ) return Amount is
   begin
      return m * 52.0;
   end Weekly_To_Annual;
   
   function Multiply( r : Rate_Array; m : Rate ) return Rate_Array is
      x : Rate_Array( r'Range );
   begin
      for i in r'Range loop
         x(i) := r(i) * m;
      end loop;      
      return x;
   end Multiply;
   
   function Multiply( a : Amount_Array; m : Rate ) return Amount_Array is
      x : Amount_Array( a'Range );
   begin
      for i in a'Range loop
         x(i) := a(i) * m;
      end loop; 
      return x;
   end Multiply;
   
   function Sum( r : Amount_Array ) return Amount is
      sm : Amount := 0.0;
   begin
      for i in r'Range loop
         sm := sm + r(i);
      end loop; 
      return sm;
   end Sum;
   
   function To_Percent( r : Amount_Array ) return Amount_Array is
      sm  :  constant Amount := Sum( r );
      x   : Amount_Array( r'Range ) := ( Others=>0.0);
   begin
      if( sm /= 0.0 ) then
         for i in r'Range loop
            x(i) := 100.0 * r(i)/sm;
         end loop;
      end if;
      return x;
   end To_Percent;

   function Differs_By( a, b : Real; tol : Real := 0.0001 ) return Boolean is
   begin
      return Abs( a - b ) < tol;
   end Differs_By;
   
   function Nearly_Equal( a, b : Real; tol : Real := 0.0001 ) return Boolean is
   begin
      return Abs( a - b ) < tol;
   end Nearly_Equal;
    
end BE_Base_Model_Types;
