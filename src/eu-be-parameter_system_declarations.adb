with Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded;
with EU_Logger;
with Text_Utils;

package body EU.BE.Parameter_System_Declarations is

   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Strings.Unbounded;
   
   procedure Log_Params( s : String ) is
   begin
      EU_Logger.Log( EU_Logger.parameter_handlers, s );
   end Log_Params;
   
 
   
   procedure Uprate( 
               c    : in out Amount; 
               mult : Amount; 
               rec  : BE_Parameter_System.Parameter_Rec; 
               which_operation : Integer := 0 ) is
   use BE_Parameter_System;
   begin
      case rec.logical_type is
         -- need a rooker wise thing here
         when any_kind | tax_allowance |
              tax_band |  benefit | poverty_line =>
                 if( Long_Float( c * mult ) < Long_Float( Amount'Last ))then
                    c := c * mult;
                 end if;
         when others => null;
      end case;
   end Uprate;

   
  procedure Map_To_Parameters( light : Light_Params; params : in out BE_Buffer ) is
  begin
     case light.which_package is
     when  flat_tax => 
        declare
           RB_TARGET : constant Unbounded_String := TuS( "be.income_tax.rates_and_bands.rate_bands" );
           NUM_BANDS : constant Natural := params.Get_Current_Collection_Size( RB_TARGET );
        begin
           for t in 2 .. NUM_BANDS loop
              params.Delete( RB_TARGET, 2 );
           end loop;
           params.Set( RB_TARGET, light.base_rate, 1, TuS( "rate" ));  
           params.Set( RB_TARGET, 999999999.9, 1, TuS( "band" ));
           params.Set( TuS("be.income_tax.allowances.base_tax_allowance" ), light.exemption );
        end;
     when child_care                => 
        params.Set( TuS( "be.child_benefits.base_amounts.first_child" ), light.basic_amount );  
        params.Set( TuS( "be.child_benefits.base_amounts.second_child" ), light.basic_amount );  
        params.Set( TuS( "be.child_benefits.base_amounts.third_child" ), light.basic_amount );  
     when regional_tax_credit       => 
        params.Set( TuS( "be.income_tax.jobkorting.basic_yearly_amount" ), light.basic_tax_credit );  
     when minimum_income_protection => 
        params.Set( TuS( "be.basic_minimum_income.single_person" ), light.income_support_per_single_person ); 
        params.Set( TuS( "be.basic_minimum_income.family_without_children" ), light.income_support_per_single_person * 483.0/725.0 );
        params.Set( TuS( "be.basic_minimum_income.family_with_children" ), light.income_support_per_single_person * 2.0 );
     end case;
  end Map_To_Parameters;     
   
   
end EU.BE.Parameter_System_Declarations;
