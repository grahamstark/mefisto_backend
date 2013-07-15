with Ada.Text_IO;

package body EU.BE.Results is
   
   MAP_SIMULATED_VALUES : Boolean := FALSE;
   
   use Ada.Text_IO;
   
   function Map( results : Detailed_Record ) return All_Taxes_And_Benefits_Array is
      a :  All_Taxes_And_Benefits_Array := (others=>0.0);
   begin
      a( personal_income_tax ) := results.tax_income_tax_national_government_simulated +
                                  results.tax_income_tax_capital_taxation_simulated + 
                                  results.tax_income_tax_municipal_government_simulated;
                                  
      a( social_security_contributions ) := 
         results.employee_social_insurance_contributions_paid_by_the_person +
         results.employer_social_insurance_contributions_paid_for_the_person_not_included_in_ils_dispy;
      
      a( child_benefits ) := 
          results.benefit_child_simulated +
          results.benefit_child_birth_or_adoption_simulated;
      if( MAP_SIMULATED_VALUES )then   
         a( unemployment_benefits ) := 
            results.benefit_unemployment_simulated; -- FIXME not 
      else
         a( unemployment_benefits ) := 
            results.benefit_unemployment + results.benefit_early_retirement; -- FIXME not 
      end if;
      
      a( mip ) := results.benefit_social_assistance_simulated + 
         results.benefit_social_assistance_old_age_simulated;
      a( taxes_minus_benefits ) := results.sum_of_all_taxes_paid_by_the_person - results.sum_of_all_benefits_received_by_the_person;
      return a;
   end Map;
   
   function Map( results : Detailed_Record ) return Summary_Items_Array is
      a : Summary_Items_Array := (others=>0.0);
   begin
      a( direct_taxes ) := results.sum_of_all_taxes_paid_by_the_person;
      a( benefits ) := results.sum_of_all_benefits_received_by_the_person;
      a( disposable_income ) := results.standard_disposable_income;
      return a;
   end Map;
        
   procedure Accumulate( to_res :  in out Detailed_Record; from_res : Detailed_Record ) is
   begin
      to_res.tax_income_tax_tax_allowance_tax_credit_simulated := to_res.tax_income_tax_tax_allowance_tax_credit_simulated + from_res.tax_income_tax_tax_allowance_tax_credit_simulated;
      to_res.tax_income_tax_tax_credit_replacement_income_simulated := to_res.tax_income_tax_tax_credit_replacement_income_simulated + from_res.tax_income_tax_tax_credit_replacement_income_simulated;
      to_res.tax_income_tax_tax_credit_child_simulated := to_res.tax_income_tax_tax_credit_child_simulated + from_res.tax_income_tax_tax_credit_child_simulated;
      to_res.tax_income_tax_tax_credit_low_pay_or_income_simulated := to_res.tax_income_tax_tax_credit_low_pay_or_income_simulated + from_res.tax_income_tax_tax_credit_low_pay_or_income_simulated;
      to_res.tax_income_tax_schedule_simulated := to_res.tax_income_tax_schedule_simulated + from_res.tax_income_tax_schedule_simulated;
      to_res.income_employment := to_res.income_employment + from_res.income_employment;
      to_res.income_investment := to_res.income_investment + from_res.income_investment;
      to_res.income_other := to_res.income_other + from_res.income_other;
      to_res.income_property := to_res.income_property + from_res.income_property;
      to_res.income_private_pension := to_res.income_private_pension + from_res.income_private_pension;
      to_res.income_private_transfers := to_res.income_private_transfers + from_res.income_private_transfers;
      to_res.income_self_employment := to_res.income_self_employment + from_res.income_self_employment;
      to_res.expenditure_maintenance_payment := to_res.expenditure_maintenance_payment + from_res.expenditure_maintenance_payment;
      to_res.pension_old_age := to_res.pension_old_age + from_res.pension_old_age;
      to_res.pension_survivors := to_res.pension_survivors + from_res.pension_survivors;
      to_res.pension_disability := to_res.pension_disability + from_res.pension_disability;
      to_res.pension_health := to_res.pension_health + from_res.pension_health;
      to_res.benefit_early_retirement := to_res.benefit_early_retirement + from_res.benefit_early_retirement;
      to_res.benefit_social_assistance_simulated := to_res.benefit_social_assistance_simulated + from_res.benefit_social_assistance_simulated;
      to_res.benefit_social_assistance_old_age_simulated := to_res.benefit_social_assistance_old_age_simulated + from_res.benefit_social_assistance_old_age_simulated;
      to_res.benefit_education := to_res.benefit_education + from_res.benefit_education;
      to_res.benefit_housing := to_res.benefit_housing + from_res.benefit_housing;
      to_res.benefit_child_simulated := to_res.benefit_child_simulated + from_res.benefit_child_simulated;
      to_res.benefit_child_birth_or_adoption_simulated := to_res.benefit_child_birth_or_adoption_simulated + from_res.benefit_child_birth_or_adoption_simulated;
      to_res.benefit_unemployment := to_res.benefit_unemployment + from_res.benefit_unemployment;
      to_res.benefit_unemployment_simulated := to_res.benefit_unemployment_simulated + from_res.benefit_unemployment_simulated;
      to_res.benefit_family_parental_leave := to_res.benefit_family_parental_leave + from_res.benefit_family_parental_leave;
      to_res.benefit_maternity := to_res.benefit_maternity + from_res.benefit_maternity;
      to_res.tax_income_tax_national_government_simulated := to_res.tax_income_tax_national_government_simulated + from_res.tax_income_tax_national_government_simulated;
      to_res.tax_income_tax_capital_taxation_simulated := to_res.tax_income_tax_capital_taxation_simulated + from_res.tax_income_tax_capital_taxation_simulated;
      to_res.tax_income_tax_municipal_government_simulated := to_res.tax_income_tax_municipal_government_simulated + from_res.tax_income_tax_municipal_government_simulated;
      to_res.tax_sic_employee_simulated := to_res.tax_sic_employee_simulated + from_res.tax_sic_employee_simulated;
      to_res.tax_sic_pensioner_simulated := to_res.tax_sic_pensioner_simulated + from_res.tax_sic_pensioner_simulated;
      to_res.tax_sic_disabled_simulated := to_res.tax_sic_disabled_simulated + from_res.tax_sic_disabled_simulated;
      to_res.tax_sic_employee_reduction_simulated := to_res.tax_sic_employee_reduction_simulated + from_res.tax_sic_employee_reduction_simulated;
      to_res.tax_sic_employee_spouse_simulated := to_res.tax_sic_employee_spouse_simulated + from_res.tax_sic_employee_spouse_simulated;
      to_res.tax_sic_self_employed_simulated := to_res.tax_sic_self_employed_simulated + from_res.tax_sic_self_employed_simulated;
      to_res.standard_disposable_income := to_res.standard_disposable_income + from_res.standard_disposable_income;
      to_res.persons_monthly_original_income := to_res.persons_monthly_original_income + from_res.persons_monthly_original_income;
      to_res.sum_of_all_benefits_received_by_the_person := to_res.sum_of_all_benefits_received_by_the_person + from_res.sum_of_all_benefits_received_by_the_person;
      to_res.sum_of_all_taxes_paid_by_the_person := to_res.sum_of_all_taxes_paid_by_the_person + from_res.sum_of_all_taxes_paid_by_the_person;
      to_res.employee_social_insurance_contributions_paid_by_the_person := to_res.employee_social_insurance_contributions_paid_by_the_person + from_res.employee_social_insurance_contributions_paid_by_the_person;
      to_res.employer_social_insurance_contributions_paid_for_the_person_not_included_in_ils_dispy := to_res.employer_social_insurance_contributions_paid_for_the_person_not_included_in_ils_dispy + from_res.employer_social_insurance_contributions_paid_for_the_person_not_included_in_ils_dispy;
      to_res.self_employed_social_insurance_contributions_paid_by_the_person := to_res.self_employed_social_insurance_contributions_paid_by_the_person + from_res.self_employed_social_insurance_contributions_paid_by_the_person;
      to_res.pension := to_res.pension + from_res.pension;
      to_res.taxable_income := to_res.taxable_income + from_res.taxable_income;
      to_res.taxable_income_something_mq := to_res.taxable_income_something_mq + from_res.taxable_income_something_mq;
      to_res.bennt := to_res.bennt + from_res.bennt;
      to_res.ils_benmt := to_res.ils_benmt + from_res.ils_benmt;
      to_res.bensim := to_res.bensim + from_res.bensim;
      to_res.taxsim := to_res.taxsim + from_res.taxsim;
      to_res.benefit_social_assistance_entitlement_simulated := to_res.benefit_social_assistance_entitlement_simulated + from_res.benefit_social_assistance_entitlement_simulated;
   end Accumulate;

   function Difference( sys1 : Detailed_Record; sys2 : Detailed_Record ) return Detailed_Record is
      diff : Detailed_Record;
   begin
      diff := sys1;
      diff.tax_income_tax_tax_allowance_tax_credit_simulated := sys2.tax_income_tax_tax_allowance_tax_credit_simulated - sys1.tax_income_tax_tax_allowance_tax_credit_simulated;
      diff.tax_income_tax_tax_credit_replacement_income_simulated := sys2.tax_income_tax_tax_credit_replacement_income_simulated - sys1.tax_income_tax_tax_credit_replacement_income_simulated;
      diff.tax_income_tax_tax_credit_child_simulated := sys2.tax_income_tax_tax_credit_child_simulated - sys1.tax_income_tax_tax_credit_child_simulated;
      diff.tax_income_tax_tax_credit_low_pay_or_income_simulated := sys2.tax_income_tax_tax_credit_low_pay_or_income_simulated - sys1.tax_income_tax_tax_credit_low_pay_or_income_simulated;
      diff.tax_income_tax_schedule_simulated := sys2.tax_income_tax_schedule_simulated - sys1.tax_income_tax_schedule_simulated;
      diff.income_employment := sys2.income_employment - sys1.income_employment;
      diff.income_investment := sys2.income_investment - sys1.income_investment;
      diff.income_other := sys2.income_other - sys1.income_other;
      diff.income_property := sys2.income_property - sys1.income_property;
      diff.income_private_pension := sys2.income_private_pension - sys1.income_private_pension;
      diff.income_private_transfers := sys2.income_private_transfers - sys1.income_private_transfers;
      diff.income_self_employment := sys2.income_self_employment - sys1.income_self_employment;
      diff.expenditure_maintenance_payment := sys2.expenditure_maintenance_payment - sys1.expenditure_maintenance_payment;
      diff.pension_old_age := sys2.pension_old_age - sys1.pension_old_age;
      diff.pension_survivors := sys2.pension_survivors - sys1.pension_survivors;
      diff.pension_disability := sys2.pension_disability - sys1.pension_disability;
      diff.pension_health := sys2.pension_health - sys1.pension_health;
      diff.benefit_early_retirement := sys2.benefit_early_retirement - sys1.benefit_early_retirement;
      diff.benefit_social_assistance_simulated := sys2.benefit_social_assistance_simulated - sys1.benefit_social_assistance_simulated;
      diff.benefit_social_assistance_old_age_simulated := sys2.benefit_social_assistance_old_age_simulated - sys1.benefit_social_assistance_old_age_simulated;
      diff.benefit_education := sys2.benefit_education - sys1.benefit_education;
      diff.benefit_housing := sys2.benefit_housing - sys1.benefit_housing;
      diff.benefit_child_simulated := sys2.benefit_child_simulated - sys1.benefit_child_simulated;
      diff.benefit_child_birth_or_adoption_simulated := sys2.benefit_child_birth_or_adoption_simulated - sys1.benefit_child_birth_or_adoption_simulated;
      diff.benefit_unemployment := sys2.benefit_unemployment - sys1.benefit_unemployment;
      diff.benefit_unemployment_simulated := sys2.benefit_unemployment_simulated - sys1.benefit_unemployment_simulated;
      diff.benefit_family_parental_leave := sys2.benefit_family_parental_leave - sys1.benefit_family_parental_leave;
      diff.benefit_maternity := sys2.benefit_maternity - sys1.benefit_maternity;
      diff.tax_income_tax_national_government_simulated := sys2.tax_income_tax_national_government_simulated - sys1.tax_income_tax_national_government_simulated;
      diff.tax_income_tax_capital_taxation_simulated := sys2.tax_income_tax_capital_taxation_simulated - sys1.tax_income_tax_capital_taxation_simulated;
      diff.tax_income_tax_municipal_government_simulated := sys2.tax_income_tax_municipal_government_simulated - sys1.tax_income_tax_municipal_government_simulated;
      diff.tax_sic_employee_simulated := sys2.tax_sic_employee_simulated - sys1.tax_sic_employee_simulated;
      diff.tax_sic_pensioner_simulated := sys2.tax_sic_pensioner_simulated - sys1.tax_sic_pensioner_simulated;
      diff.tax_sic_disabled_simulated := sys2.tax_sic_disabled_simulated - sys1.tax_sic_disabled_simulated;
      diff.tax_sic_employee_reduction_simulated := sys2.tax_sic_employee_reduction_simulated - sys1.tax_sic_employee_reduction_simulated;
      diff.tax_sic_employee_spouse_simulated := sys2.tax_sic_employee_spouse_simulated - sys1.tax_sic_employee_spouse_simulated;
      diff.tax_sic_self_employed_simulated := sys2.tax_sic_self_employed_simulated - sys1.tax_sic_self_employed_simulated;
      diff.standard_disposable_income := sys2.standard_disposable_income - sys1.standard_disposable_income;
      diff.persons_monthly_original_income := sys2.persons_monthly_original_income - sys1.persons_monthly_original_income;
      diff.sum_of_all_benefits_received_by_the_person := sys2.sum_of_all_benefits_received_by_the_person - sys1.sum_of_all_benefits_received_by_the_person;
      diff.sum_of_all_taxes_paid_by_the_person := sys2.sum_of_all_taxes_paid_by_the_person - sys1.sum_of_all_taxes_paid_by_the_person;
      diff.employee_social_insurance_contributions_paid_by_the_person := sys2.employee_social_insurance_contributions_paid_by_the_person - sys1.employee_social_insurance_contributions_paid_by_the_person;
      diff.employer_social_insurance_contributions_paid_for_the_person_not_included_in_ils_dispy := sys2.employer_social_insurance_contributions_paid_for_the_person_not_included_in_ils_dispy - sys1.employer_social_insurance_contributions_paid_for_the_person_not_included_in_ils_dispy;
      diff.self_employed_social_insurance_contributions_paid_by_the_person := sys2.self_employed_social_insurance_contributions_paid_by_the_person - sys1.self_employed_social_insurance_contributions_paid_by_the_person;
      diff.pension := sys2.pension - sys1.pension;
      diff.taxable_income := sys2.taxable_income - sys1.taxable_income;
      diff.taxable_income_something_mq := sys2.taxable_income_something_mq - sys1.taxable_income_something_mq;
      diff.bennt := sys2.bennt - sys1.bennt;
      diff.ils_benmt := sys2.ils_benmt - sys1.ils_benmt;
      diff.bensim := sys2.bensim - sys1.bensim;
      diff.taxsim := sys2.taxsim - sys1.taxsim;
      diff.benefit_social_assistance_entitlement_simulated := sys2.benefit_social_assistance_entitlement_simulated - sys1.benefit_social_assistance_entitlement_simulated;
      diff.poverty_gap := sys2.poverty_gap - sys1.poverty_gap;
      return diff;
   end Difference;

   procedure Add_To_List( list : in out Detailed_Results_List; result : Detailed_Record; accumulate_to_hh_level : Boolean := False ) is
   use Detailed_Results_Package;
   begin
      if( accumulate_to_hh_level )then
         declare
            p : Natural := Natural( list.Length );
            current : Detailed_Record;
         begin
            if( p > 0 )then
               current := list.Element( p ); 
            end if;
            if( result.identifier_household = current.identifier_household ) then
               Accumulate( current, result );
               list.Replace_Element( p, current );
            else
               Append( list, result );
            end if;
         end;
      else
         Append( list, result );
      end if;
   end Add_To_List;
    
end EU.BE.Results;

