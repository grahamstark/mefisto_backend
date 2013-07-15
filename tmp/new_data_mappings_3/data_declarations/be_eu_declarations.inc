   type Person_Rec is record
      identifier_household : Integer; -- (idhh) : Household
      identifier_person : Integer; -- (idperson) : Person
      identifier_father : Integer; -- (idfather) : Father
      identifier_mother : Integer; -- (idmother) : Mother
      identifier_partner : Integer; -- (idpartner) : Partner
      idorighh : Integer; -- (idorighh) : 
      idorigperson : Integer; -- (idorigperson) : 
      demographic_age : Integer; -- (dag) : Age
      demographic_gender : Gender_Type; -- (dgn) : Gender
      demographic_marital_status : Marital_Status_Type; -- (dms) : Marital status
      demographic_citizenship : Citizenship_Type; -- (dcz) : Citizenship
      demographic_disability : Integer; -- (ddi) : Disability
      demographic_date_of_interview : Integer; -- (ddt) : Date of interview
      demographic_weight : Amount; -- (dwt) : Weight
      demographic_sample_units_main_or_basic : Integer; -- (dsu00) : Sample units : main/basic
      demographic_sample_units_01 : Integer; -- (dsu01) : Sample units : 01
      demographic_sample_units_02 : Integer; -- (dsu02) : Sample units : 02
      demographic_country : Integer; -- (dct) : Country
      demographic_education_current_status : Education_Current_Status_Type; -- (dec) : Education - current status
      demographic_education_highest_status : Education_Highest_Status_Type; -- (deh) : Education - highest status
      demographic_education_number_of_years : Integer; -- (dey) : Education - number of years
      demographic_education_when_achieved_highest_status : Integer; -- (dew) : Education - when achieved highest status
      demographic_region_nuts_level_1 : Nuts_Level_1_Type; -- (drgn1) : Region : nuts level 1
      demographic_consensual_union : Consensual_Union_Type; -- (dcu) : Consensual union
      labour_market_economic_status : Integer; -- (les) : Economic status
      labour_market_civil_servant : Boolean; -- (lcs) : Civil servant
      labour_market_firm_size : Integer; -- (lfs) : Firm size
      labour_market_industry_nace : Integer; -- (lin) : Industry (nace)
      labour_market_industry_nace_detailed_industry : Integer; -- (lindi) : Industry (nace) : detailed industry
      labour_market_in_work_months_per_year : Integer; -- (liwmy) : In work : months per year
      labour_market_hours_worked_per_week : Integer; -- (lhw) : Hours worked per week
      labour_market_in_work_work_history_length_of_time_in_months : Integer; -- (liwwh) : In work : work history (length of time in months)
      labour_market_occupation_isco_1_digit : Occupation_Isco_1_Digit; -- (loc) : Occupation (isco 1-digit)
      labour_market_pensioner_months_per_year : Integer; -- (lpemy) : Pensioner : months per year
      labour_market_self_employed : Integer; -- (lse) : Self employed
      labour_market_unemployed_months_per_year : Integer; -- (lunmy) : Unemployed : months per year
      labour_market_out_of_work_actively_seeking : Integer; -- (lowas) : Out of work : actively seeking
      income_imputed_value_wage_or_salary : Amount; -- (yivwg) : Imputed value : wage/salary
      income_employment : Amount; -- (yem) : Employment
      in_kind_fringe_benefit : Amount; -- (kfb) : Fringe benefit
      income_private_pension : Amount; -- (ypp) : Private pension
      income_self_employment : Amount; -- (yse) : Self employment
      benefit_education : Amount; -- (bed) : Education
      benefit_unemployment : Amount; -- (bun) : Unemployment
      pension_health : Amount; -- (phl) : Health
      pension_disability : Amount; -- (pdi) : Disability
      pension_old_age : Amount; -- (poa) : Old age
      pension_survivors : Amount; -- (psu) : Survivors
      in_kind_imputed_value_housing : Amount; -- (kivho) : Imputed value : housing
      income_investment : Amount; -- (yiy) : Investment
      income_property_rent : Amount; -- (yprrt) : Property : rent
      income_property : Amount; -- (ypr) : Property
      income_private_transfers : Amount; -- (ypt) : Private transfers
      benefit_social_assistance : Amount; -- (bsa) : Social assistance
      benefit_housing : Amount; -- (bho) : Housing
      tax_property_tax : Amount; -- (tpr) : Property tax
      income_other : Amount; -- (yot) : Other
      income_disposable : Amount; -- (yds) : Disposable
      tax_repayments_or_receipts : Amount; -- (tad) : Repayments/receipts
      benefit_child_main_or_basic : Amount; -- (bch00) : Child : main/basic
      benefit_child_birth_or_adoption : Amount; -- (bchba) : Child : birth/adoption
      benefit_child : Amount; -- (bch) : Child
      benefit_maternity : Amount; -- (bma) : Maternity
      benefit_family_parental_leave : Amount; -- (bfapl) : Family : parental leave
      in_kind_housing_owned_occupied : Amount; -- (khooo) : Housing : owned-occupied
      income_employment_months_per_year : Integer; -- (yemmy) : Employment : months per year
      in_kind_fringe_benefit_months_per_year : Amount; -- (kfbmy) : Fringe benefit : months per year
      income_self_employment_months_per_year : Integer; -- (ysemy) : Self employment : months per year
      benefit_unemployment_months_per_year : Amount; -- (bunmy) : Unemployment : months per year
      pension_disability_months_per_year : Amount; -- (pdimy) : Disability : months per year
      pension_health_months_per_year : Amount; -- (phlmy) : Health : months per year
      pension_old_age_months_per_year : Amount; -- (poamy) : Old age : months per year
      pension_survivors_months_per_year : Amount; -- (psumy) : Survivors : months per year
      tax_income_tax_and_sics : Amount; -- (tis) : Income tax and sics
      benefit_early_retirement : Amount; -- (byr) : Early retirement
      benefit_early_retirement	early_retirement_pension_months_per_year : Amount; -- (byrmy) : Early retirement	early retirement pension - months per year
      assets_main_residence_number_of_rooms : Integer; -- (amrrm) : Main residence : number of rooms
      assets_main_residence_tenure : Tenure_Type; -- (amrtn) : Main residence : tenure
      assets_financial_capital : Amount; -- (afc) : Financial capital
      expenditure_maintenance_payment : Amount; -- (xmp) : Maintenance payment
      expenditure_private_pension_voluntary : Amount; -- (xpp) : Private pension (voluntary)
      expenditure_housing_cost : Amount; -- (xhc) : Housing cost
      expenditure_housing_cost_rent : Amount; -- (xhcrt) : Housing cost : rent
      expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest : Amount; -- (xhcmomi) : Housing cost : mortgage payment (interest+capital) : mortgage interest
      expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_capital : Amount; -- (xhcmomc) : Housing cost : mortgage payment (interest+capital) : mortgage capital
      assets_mortgage_year_of_loan : Amount; -- (amoyl) : Mortgage : year of loan
      assets_mortgage_loan_value : Amount; -- (amolv) : Mortgage : loan value
      expenditure_housing_cost_other : Amount; -- (xhcot) : Housing cost : other
   end record


   type Household_Rec is record
   end record
