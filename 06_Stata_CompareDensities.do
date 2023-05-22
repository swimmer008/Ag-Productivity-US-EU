* ------------------------------------- *
*                                       *
* This program runs the kernel-analysis * 
* procedure by Combes et al. (2012)     *
*                                       *
* ------------------------------------- *

use "H:\02 Forschung\02 Replication study\Submission\Revision II\Estimation\R_output\densities.dta", clear

*Convert factor variable into string variable
decode variable, generate(variable_string)

*Display indexes
tab variable

*TFP_lowe_vrs vs TFP_lowe_crs: Not different                                                 
preserve
keep if variable_string == "TFP_lowe_vrs" | variable_string == "TFP_lowe_crs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_vrs vs. TFP_ADEA_vrs: Different ***
preserve
keep if variable_string == "TFP_lowe_vrs" | variable_string == "TFP_ADEA_vrs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_vrs vs. TFP_ADEA_crs: Different ***
preserve
keep if variable_string == "TFP_lowe_vrs" | variable_string == "TFP_ADEA_crs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_vrs vs. TFP_MSFA_vrs: Different ***
preserve
keep if variable_string == "TFP_lowe_vrs" | variable_string == "TFP_MSFA_vrs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_vrs vs. TFP_MSFA_crs: Different ***
preserve
keep if variable_string == "TFP_lowe_vrs" | variable_string == "TFP_MSFA_crs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_vrs vs. TFP_GlobMalm: Different ***
preserve
keep if variable_string == "TFP_lowe_vrs" | variable_string == "TFP_GlobMalm"
estquant value, cat(variable) sh di
restore

*****

* TFP_lowe_crs vs. TFP_ADEA_vrs: Different ***
preserve
keep if variable_string == "TFP_lowe_crs" | variable_string == "TFP_ADEA_vrs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_crs vs. TFP_ADEA_crs: Different ***
preserve
keep if variable_string == "TFP_lowe_crs" | variable_string == "TFP_ADEA_crs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_crs vs. TFP_MSFA_vrs: Different ***
preserve
keep if variable_string == "TFP_lowe_crs" | variable_string == "TFP_MSFA_vrs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_crs vs. TFP_MSFA_crs: Different ***
preserve
keep if variable_string == "TFP_lowe_crs" | variable_string == "TFP_MSFA_crs"
estquant value, cat(variable) sh di
restore

* TFP_lowe_crs vs. TFP_GlobMalm: Different ***
preserve
keep if variable_string == "TFP_lowe_crs" | variable_string == "TFP_GlobMalm"
estquant value, cat(variable) sh di
restore

*****

* TFP_ADEA_vrs vs. TFP_ADEA_crs: Different ***
preserve
keep if variable_string == "TFP_ADEA_vrs" | variable_string == "TFP_ADEA_crs"
estquant value, cat(variable) sh di
restore

* TFP_ADEA_vrs vs. TFP_MSFA_vrs: Not different                                                
preserve
keep if variable_string == "TFP_ADEA_vrs" | variable_string == "TFP_MSFA_vrs"
estquant value, cat(variable) sh di
restore

* TFP_ADEA_vrs vs. TFP_MSFA_crs: Different ***
preserve
keep if variable_string == "TFP_ADEA_vrs" | variable_string == "TFP_MSFA_crs"
estquant value, cat(variable) sh di
restore

* TFP_ADEA_vrs vs. TFP_GlobMalm: Shift is different *** but dilation is not                   
preserve
keep if variable_string == "TFP_ADEA_vrs" | variable_string == "TFP_GlobMalm"
estquant value, cat(variable) sh di
restore

*****

* TFP_ADEA_crs vs. TFP_MSFA_vrs: Different ***
preserve
keep if variable_string == "TFP_ADEA_crs" | variable_string == "TFP_MSFA_vrs"
estquant value, cat(variable) sh di
restore

* TFP_ADEA_crs vs. TFP_MSFA_crs: Different ***
preserve
keep if variable_string == "TFP_ADEA_crs" | variable_string == "TFP_MSFA_crs"
estquant value, cat(variable) sh di
restore

* TFP_ADEA_crs vs. TFP_GlobMalm: Different ***
preserve
keep if variable_string == "TFP_ADEA_crs" | variable_string == "TFP_GlobMalm"
estquant value, cat(variable) sh di
restore

*****

* TFP_MSFA_vrs vs. TFP_MSFA_crs: Different ***
preserve
keep if variable_string == "TFP_MSFA_vrs" | variable_string == "TFP_MSFA_crs"
estquant value, cat(variable) sh di
restore

* TFP_MSFA_vrs vs. TFP_GlobMalm: Shift is different ** but dilation is not                    
preserve
keep if variable_string == "TFP_MSFA_vrs" | variable_string == "TFP_GlobMalm"
estquant value, cat(variable) sh di
restore

*****

*TFP_MSFA_crs vs. TFP_GlobMalm: Shift is not different but dilation is different ***          
preserve
keep if variable_string == "TFP_MSFA_crs" | variable_string == "TFP_GlobMalm"
estquant value, cat(variable) sh di
restore