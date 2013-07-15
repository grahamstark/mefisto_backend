   type Person_Rec is record
      identifier_household : Integer; -- (idhh) : Household
      identifier_person : Integer; -- (idperson) : Person
      identifier_partner : Integer; -- (idpartner) : Partner
      identifier_mother : Integer; -- (idmother) : Mother
      identifier_father : Integer; -- (idfather) : Father
      demographic_country : Integer; -- (dct) : Country
      demographic_weight : Integer; -- (dwt) : Weight
      demographic_age : Integer; -- (dag) : Age
      demographic_gender : Gender_Type; -- (dgn) : Gender
      demographic_marital_status : Marital_Status_Type; -- (dms) : Marital status
      demographic_education_current_status : Education_Current_Status_Type; -- (dec) : Education - current status
      labour_market_economic_status : Integer; -- (les) : Economic status
      labour_market_civil_servant : Boolean; -- (lcs) : Civil servant
      demographic_consensual_union : Consensual_Union_Type; -- (dcu) : Consensual union
      demographic_region_nuts_level_1 : Nuts_Level_1_Type; -- (drgn1) : Region : nuts level 1
      tax_income_tax_tax_allowance_tax_credit_simulated : Amount; -- (tintatc_s) : Income tax : tax allowance : tax credit : simulated
      tax_income_tax_tax_credit_replacement_income_simulated : Amount; -- (tintcri_s) : Income tax : tax credit : replacement income : simulated
      tax_income_tax_tax_credit_child_simulated : Amount; -- (tintcch_s) : Income tax : tax credit : child : simulated
      tax_income_tax_tax_credit_low_pay_or_income_simulated : Amount; -- (tintcly_s) : Income tax : tax credit : low pay/income : simulated
      tax_income_tax_schedule_simulated : Amount; -- (tints_s) : Income tax : schedule : simulated
      income_disposable : Amount; -- (yds) : Disposable
      tax_income_tax_national_government_simulated : Amount; -- (tinna_s) : Income tax : national government : simulated
      assets_main_residence_tenure : Tenure_Type; -- (amrtn) : Main residence : tenure
      labour_market_occupation_isco_1_digit : Occupation_Isco_1_Digit; -- (loc) : Occupation (isco 1-digit)
      income_employment : Amount; -- (yem) : Employment
      income_investment : Integer; -- (yiy) : Investment
      income_other : Amount; -- (yot) : Other
      income_property : Amount; -- (ypr) : Property
      income_private_pension : Amount; -- (ypp) : Private pension
      income_private_transfers : Amount; -- (ypt) : Private transfers
      income_self_employment : Amount; -- (yse) : Self employment
      expenditure_maintenance_payment : Amount; -- (xmp) : Maintenance payment
      pension_old_age : Amount; -- (poa) : Old age
      pension_survivors : Amount; -- (psu) : Survivors
      pension_disability : Amount; -- (pdi) : Disability
      pension_health : Amount; -- (phl) : Health
      benefit_early_retirement : Amount; -- (byr) : Early retirement
      benefit_social_assistance_simulated : Amount; -- (bsa_s) : Social assistance : simulated
      benefit_social_assistance_old_age_simulated : Amount; -- (bsaoa_s) : Social assistance : old age : simulated
      benefit_education : Amount; -- (bed) : Education
      benefit_housing : Amount; -- (bho) : Housing
      benefit_child_simulated : Amount; -- (bch_s) : Child : simulated
      benefit_child_birth_or_adoption_simulated : Amount; -- (bchba_s) : Child : birth/adoption : simulated
      benefit_unemployment : Amount; -- (bun) : Unemployment
      benefit_unemployment_simulated : Amount; -- (bun_s) : Unemployment : simulated
      benefit_family_parental_leave : Amount; -- (bfapl) : Family : parental leave
      benefit_maternity : Amount; -- (bma) : Maternity
      tax_income_tax_capital_taxation_simulated : Amount; -- (tinkt_s) : Income tax : capital taxation : simulated
      tax_income_tax_municipal_government_simulated : Amount; -- (tinmu_s) : Income tax : municipal government : simulated
      tax_sic_employee_simulated : Amount; -- (tscee_s) : Sic : employee : simulated
      tax_sic_pensioner_simulated : Amount; -- (tscpe_s) : Sic : pensioner : simulated
      tax_sic_disabled_simulated : Amount; -- (tscdb_s) : Sic : disabled : simulated
      tax_sic_employee_reduction_simulated : Amount; -- (tsceerd_s) : Sic : employee : reduction : simulated
      tax_sic_employee_spouse_simulated : Amount; -- (tsceesp_s) : Sic : employee : spouse : simulated
      tax_sic_self_employed_simulated : Amount; -- (tscse_s) : Sic : self-employed : simulated
      standard_disposable_income : Amount; -- (ils_dispy) : Standard disposable income
      persons_monthly_original_income : Amount; -- (ils_origy) : Person’s monthly original income
      sum_of_all_benefits_received_by_the_person : Amount; -- (ils_ben) : Sum of all benefits received by the person
      sum_of_all_taxes_paid_by_the_person : Amount; -- (ils_tax) : Sum of all taxes paid by the person
      employee_social_insurance_contributions_paid_by_the_person : Amount; -- (ils_sicee) : Employee social insurance contributions paid by the person
      employer_social_insurance_contributions_paid_for_the_person_not_included_in_ils_dispy : Amount; -- (ils_sicer) : Employer social insurance contributions paid for the person (not included in ils_dispy)
      self_employed_social_insurance_contributions_paid_by_the_person : Amount; -- (ils_sicse) : Self-employed social insurance contributions paid by the person
      pension : Amount; -- (ils_pen) : Pension
      taxable_income : Amount; -- (il_taxabley) : Taxable income
      taxable_income_something_mq : Amount; -- (il_taxabley_bf_mq) : Taxable income (something mq)
      bennt : Amount; -- (ils_bennt) : Bennt
      ils_benmt : Amount; -- (ils_benmt) : Ils_benmt
      bensim : Amount; -- (ils_bensim) : Bensim
      taxsim : Amount; -- (ils_taxsim) : Taxsim
      ils_tinty : Amount; -- (ils_tinty) : Ils_tinty
      tu_family_be_headid : Amount; -- (tu_family_be_headid) : Tu_family_be_headid
      head_of_household_indentifier : Amount; -- (tu_household_be_headid) : Head of household indentifier
   end record


   type Household_Rec is record
   end record
