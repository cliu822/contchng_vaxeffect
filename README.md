# Summary
This repository contains the data and code used to characterize change in contact rates and estiamte the effect of vaccination on changes in contact rate. The data for this study comes from [COVIDVU](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7417272/), a longitudinal survey of a diverse and geographically representative cohort of individuals sampled from households in the United States. Our findings are described in this preprint entitled ["Use-case for monitoring longitudinal contact rates during epidemics: The effect of COVID-19 vaccination on change in contacts and on transmission in a US cohort"]().

# Code description
The code for the main analysis can be found in [contchng_vaxeffect](https://github.com/cliu822/contchng_vaxeffect). The relevant data, including survey tools and codebook are stored in [0_data](0_data). Figure outputs are in [0_plot](0_plot) and table outputs are in [0_table](0_tab).
 
### Description of main analysis code


| File                   | Description |Category|
| ---------------------- | ------------- |------------- |
| [0_model/0_model_code_sero](0_model/0_model_code_sero.R)           |Model code function using serology to trigger vaccinations| Serology-triggered models|
| [0_model/0_model_setup](0_model/0_model_setup.R)        | Setup model with seroprevalence vax trigger | Sero-trigger models|
| [0_model/0_model_setup_hiescape](0_model/0_model_setup_hiescape.R) |Setup model with seroprevalence vax trigger & high immune escape| Sero-trigger models|
| [0_model/0_model_setup_randtime](0_model/0_model_setup_randtime.R) |Setup model with seroprevalence vax trigger & randomly-timed epidemics| Sero-trigger models|
| [0_model/1_model_code_int](0_model/1_model_code_int.R)           |Model code function with fixed-time vaccinations| Fixed-time models|
| [0_model/1_model_setup](0_model/1_model_setup.R)        | Setup model with fixed-time trigger | Fixed-time models|
| [0_model/1_model_setup_hiescape](0_model/1_model_setup_hiescape.R) |Setup model with fixed-time vax trigger& high immune escape|Fixed-time models|
| [0_model/0_model_setup_randtime](0_model/0_model_setup_randtime.R) |Setup model with fixed-time vax trigger & randomly-timed epidemics|Fixed-time models|
| [9_last_Rrand](0_model/9_last_Rrand.RDS)      | Distribution of compartments at end of calibration|Model input|
| [9_mixing_matrix_gmix](0_model/9_mixing_matrix_gmix.R)      | Social mixing matrix input|Model input|
| [9_spec_humid](0_model/9_spec_humid.csv)      | Specific humidity over calendar year|Model input||
[0_postprocess/0_case_sero_death_timeseries](0_postprocess/0_case_sero_death_timeseries.R)      | Takes raw outputs from models and summarize into time-series|Compile results &summarise| 
| [0_postprocess/0_imm_timeseries](0_postprocess/0_imm_timeseries.R)      | Takes raw outputs from simulations and summarize into time-series (immune landscape) |Compile results &summarise|
| [0_postprocess/0_nnt_sero](0_postprocess/0_nnt_sero.R)      | Takes raw outputs from simulations and summarize results for NNV |Compile results &summarise|

### Sample folder structure for model input/output and result generation for a single scenario
| File                   | Description |
| ---------------------- | ------------- |
| [0_plot](1_main/0_plot)|Plots and figures from scenario|
| [0_res](1_main/0_res)|Summarized results from scripts in [0_post_process](0_postprocess)|
| [0_sweep_sero](1_main/0_sweep_imm4_foisp0.7.RDS)|Data frame of model parameter inputs for serology-triggered vax scenarios|
| [0_sweep_int](1_main/0_sweep_imm4_foisp0.7_int.RDS)|Data frame of model parameter inputs for fixed-time vax scenarios|
| [2_combine_res_vax](1_main/2_combine_res_vax.R)|Combine model runs and summarise with scripts in 0_postprocess[0_postprocess]|
| [3_plots](1_main/3_plots)|Time series plots and tables|
| [3_plot_nnt](1_main/3_plot_nnt)|Plots and tables for NNV|
| [3_plots_corr](1_main/3_plots_corr)|Plots and tables for correlation|

## Sensitivity analysis
The structure of code for the sensitivity analysis repicate the same structure as the main analysis. The following sensitiivty analysis were conducted

* [Varying rate of waning anitbody](2_kappasweep)
* [Varying decreased susceptibility among seropositive](3_foisweep)
* [Randomly-timed annual epidemics as opposed to seasonal epidemics](4_randtime)
* [High immune escape where future waves are primarily driven by increasingly transmissible variants](5_hiescape)
* [Varying rate of waning immunity](6_waneimmune)
## Model calibration
The code used for model calibration implemented using Approximate Bayesian Approach can be found in [0_calibration](0_calibration)
