
* Calculating age based on the year of birth
generate age = 2019 - K62
*----------------------------------------------------------------------------
*Recoding all questions with Yes/No options into 0 for No and 1 for Yes
* I Type: Originally 1 for Yes and 2 for No
local varlist_I K03 K1001-K1003 K1801-K1809 K2401-K2410 K3901-K3907 K4301-K4306 K5701-K5705 K5901-K5912
foreach var of varlist `varlist_I' {
    recode `var' (1=1) (2=0)
}
* II Type: Originally 1 for No and 2 for Yes
local varlist_II K04 K40 K44 K49 K4902 K52
foreach var of varlist `varlist_II' {
    recode `var' (1=0) (2=1)
}
*----------------------------------------------------------------------------
*Investigating multiple choice variables

*Multiple choice K60 into 1 if there's an answer and 0 if there's no answer
foreach var in K6001 K6002 K6003 K6004 K6005 K6006 K6007 K6008 K6009 {
    gen `var'_new = 0
    replace `var'_new = 1 if `var' == 1
}
summarize K6001_new K6002_new K6003_new K6004_new K6005_new K6006_new K6007_new K6008_new K6009_new
*Multiple choice K66 into 1 if there's an answer and 0 if there's no answer
foreach var in K6601 K6602 K6603 K6604 K6605 K6606 K6607 K6608 {
    gen `var'_new = 0
    replace `var'_new = 1 if `var' == 1
}
summarize K6601_new K6602_new K6603_new K6604_new K6605_new K6606_new K6607_new K6608_new
*Multiple choice K69 into 1 if there's an answer and 0 if there's no answer
foreach var in K6901 K6902 K6903 K6904 K6905 K6906 K6907 K6908 K6909 {
    gen `var'_new = 0
    replace `var'_new = 1 if `var' == 1
}
summarize K6901_new K6902_new K6903_new K6904_new K6905_new K6906_new K6907_new K6908_new K6909_new
* a more detailed view needed: transforming the 1 values to specified numeric values
foreach i of numlist 1/9 {
    local var K690`i'
    tostring `var', replace force
    replace `var' = "`i'" if `var' == "1"
    replace `var' = "0" if missing(`var')
}
gen str9 K69_aggreg_string = ""
foreach i of numlist 1/9 {
    local var K690`i'
    replace K69_aggreg_string = K69_aggreg_string + `var'
}
*It seems some people have selected more than 1 option. I will transform them in the following way: "Unemployed or temporarily laid off" AND "Studying" as "Unemployed"; "Unemployed or temporarily laid off" AND "Providing care" as "Providing care"; "Entrepreneur" AND "In pension on the basis of age" as "Entrepreneur"; "Entrepreneur" AND "Unemployed or temporarily laid off" as "Entrepreneur"; "Part-time working" AND "Studying" as "Part-time working"; "Full-time working" AND "Entrepreneur" as "Entrepreneur"
gen str60 K69_aggreg = ""
replace K69_aggreg = "" if K69_aggreg_string == "........."
replace K69_aggreg = "Studying (including professional education at the workplace)" if substr(K69_aggreg_string, 9, 1) == "9"
replace K69_aggreg = "Providing care for relatives, children or other family members" if substr(K69_aggreg_string, 8, 1) == "8"
replace K69_aggreg = "In pension on the basis of age" if substr(K69_aggreg_string, 7, 1) == "7"
replace K69_aggreg = "In disability pension or in rehabilitation" if substr(K69_aggreg_string, 6, 1) == "6"
replace K69_aggreg = "In a long-term sick leave (more than 6 months)" if substr(K69_aggreg_string, 5, 1) == "5"
replace K69_aggreg = "Unemployed or temporarily laid off" if substr(K69_aggreg_string, 4, 1) == "4"
replace K69_aggreg = "Entrepreneur" if substr(K69_aggreg_string, 3, 1) == "3"
replace K69_aggreg = "Part-time working" if substr(K69_aggreg_string, 2, 1) == "2"
replace K69_aggreg = "Full-time working" if substr(K69_aggreg_string, 1, 1) == "1"
replace K69_aggreg = "" if trim(K69_aggreg) == ""
*INVESTIGATING K66 variables
gen K6601_test = K6601 * 1
gen K6602_test = K6602 * 2
gen K6603_test = K6603 * 3
gen K6604_test = K6604 * 4
gen K6605_test = K6605 * 5
gen K6606_test = K6606 * 6
gen K6607_test = K6607 * 7
gen K6608_test = K6608 * 8
tostring K6601_test K6602_test K6603_test K6604_test K6605_test K6606_test K6607_test K6608_test, replace
gen K66_test = K6601_test + K6602_test + K6603_test + K6604_test + K6605_test + K6606_test + K6607_test + K6608_test
gen K66_aggreg = ""
* Assign "higher" if K6607 or K6603 have value 1
replace K66_aggreg = "higher" if K6607 == 1 | K6603 == 1
* Assign "professional" if K6607 and K6603 are 0 or missing, but K6602 or K6606 have value 1
replace K66_aggreg = "professional" if K66_aggreg == "" & (K6602 == 1 | K6606 == 1)
* Assign "less than professional" if K6607, K6603, K6602, and K6606 are 0 or missing, but K6601, K6604, or K6605 have 1
replace K66_aggreg = "less than professional" if K66_aggreg == "" & (K6601 == 1 | K6604 == 1 | K6605 == 1)

* Recoding K66_aggreg (string to numeric)
gen K66_aggreg_num = .
replace K66_aggreg_num = 1 if K66_aggreg == "less than professional"
replace K66_aggreg_num = 2 if K66_aggreg == "professional"
replace K66_aggreg_num = 3 if K66_aggreg == "higher"
*Assigning value labels
label define K66_lbl 1 "less than professional" 2 "professional" 3 "higher"
label values K66_aggreg_num K66_lbl

*----------------------------------------------------------------------------

* Recoding K14, K2001-K2007, K25, K2901-K2904, K3001-K3003, K3005, K3007-K3008 so that the smallest value is to biggest exclusion and the smallest to the biggest inclusion
local varlist K14 K2001 K2002 K2003 K2004 K2005 K2006 K2007 K25 K2901 K2902 K2903 K2904 K3001 K3002 K3003 K3005 K3007 K3008
foreach var of local varlist {
    recode `var' (1=4) (2=3) (3=2) (4=1)
}
*Recoding K1901-K1904, K23, K58
local varlist K1901 K1902 K1903 K1904 K23 K58
foreach var of local varlist {
    recode `var' (1=5) (2=4) (4=2) (5=1)
}
*Recoding K1601-K1602
local varlist K1601 K1602
foreach var of local varlist {
    recode `var' (1=3) (3=1)
}
* Recording K10, K39, K43 and K49
local varlist K1001 K1002 K1003 K3901 K3902 K3903 K3904 K3905 K3906 K4301 K4302 K4303 K4304 K4305 K4306 K49
foreach var of local varlist {
    recode `var' (0=1) (1=0)
}
*----------------------------------------------------------------------------
*Converting Paavo variables from string to numeric
local varlist Inhabintantslowestincome2019HR Averageage2019HE Averagehouseholdssize2019TE Averageincome2019HR Householdaverageincome2019TR Householdmedianincome2019TR Householdpurchasingpower2019TR Householdshighestincome2019TR Householdslowestincome2019TR Householdsmiddleincome2019TR Householdsotherdwellings2019TE Householdsownerdwellings2019TE Householdsrenteddwellings2019TE Householdsschoolchildren2019TE Householdssmallchildren2019TE Inhabitants2019PT Inhabitantshighestincome2019HR Inhabitantsmiddleincome2019HR Inhabitantspurchasingpower2019HR Medianincome2019HR aged18orover_2019KO Basiclevelstudies2019KO Matriculationexamination2019KO Witheducationtotal2019KO Vocationaldiploma2019KO Academicdegree_higherlevel2019KO Academicdegree_lowerlevel2019KO Eikotimaankieliset Employed2019PT Pensioners2019PT Students2019PT Females2019HE Occupancyrate2019TE Onepersonhouseholds2019TE Pensionerhouseholds2019TE Unemployed2019PT
*converting to numeric
foreach var of local varlist {
    destring `var', replace force
}
*Creating ratio variables
gen HOD2019TE_ratio = Householdsownerdwellings2019TE / Householdstotal2019TE
gen HRD2019TE_ratio = Householdsrenteddwellings2019TE / Householdstotal2019TE
gen HSchC2019TE_ratio = Householdsschoolchildren2019TE / Householdstotal2019TE
gen HSmC2019TE_ratio = Householdssmallchildren2019TE / Householdstotal2019TE
gen OPH2019TE_ratio = Onepersonhouseholds2019TE / Householdstotal2019TE
gen PH2019TE_ratio = Pensionerhouseholds2019TE / Householdstotal2019TE
* Create ratio variables for employment and population
gen Employed2019PT_ratio = Employed2019PT / Inhabitants2019PT
gen Pensioners2019PT_ratio = Pensioners2019PT / Inhabitants2019PT
gen Students2019PT_ratio = Students2019PT / Inhabitants2019PT
gen Unemployed2019PT_ratio = Unemployed2019PT / Inhabitants2019PT
gen Eikotimaankieliset_ratio = Eikotimaankieliset / Inhabitants2019PT
gen BLS2019KO_ratio = Basiclevelstudies2019KO / aged18orover_2019KO
gen ME2019KO_ratio = Matriculationexamination2019KO / aged18orover_2019KO
gen WET2019KO_ratio = Witheducationtotal2019KO / aged18orover_2019KO
gen VD2019KO_ratio = Vocationaldiploma2019KO / aged18orover_2019KO
gen ADHL2019KO_ratio = Academicdegree_higherlevel2019KO / aged18orover_2019KO
gen ADLL2019KO_ratio = Academicdegree_lowerlevel2019KO / aged18orover_2019KO
*Inspecting NAs in the Paavo variables
egen missing_count = rowmiss(ADHL2019KO_ratio ADLL2019KO_ratio aged18orover_2019KO Averageage2019HE Averagehouseholdssize2019TE Averageincome2019HR BLS2019KO_ratio Eikotimaankieliset_ratio Employed2019PT_ratio Females2019HE Householdaverageincome2019TR Householdmedianincome2019TR Householdpurchasingpower2019TR Householdshighestincome2019TR Householdslowestincome2019TR Householdsmiddleincome2019TR HOD2019TE_ratio HRD2019TE_ratio HSchC2019TE_ratio HSmC2019TE_ratio Inhabintantslowestincome2019HR Inhabitants2019PT Inhabitantshighestincome2019HR Inhabitantsmiddleincome2019HR Inhabitantspurchasingpower2019HR ME2019KO_ratio Medianincome2019HR Occupancyrate2019TE OPH2019TE_ratio PH2019TE_ratio Pensioners2019PT_ratio Students2019PT_ratio Unemployed2019PT_ratio VD2019KO_ratio WET2019KO_ratio Kuntaryhmännimi Maakunnannimi Suuralueennimi)
*It came out there are 3 rows with missing data on these variables. I will remove them to add these predictors into multiple imputation and increase its quality
drop if missing_count > 1
log close
save "C:\Users\sierp\OneDrive\CHARM\Data\processed_data_05_08_2025.dta"
exit, clear
