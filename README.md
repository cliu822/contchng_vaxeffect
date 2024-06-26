# Summary
This repository contains the data and code used to characterize change in contact rates and estiamte the effect of vaccination on changes in contact rate. The data for this study comes from [COVIDVU](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7417272/), a longitudinal survey of a diverse and geographically representative cohort of individuals sampled from households in the United States. Our findings are described in this preprint entitled ["Use-case for monitoring longitudinal contact rates during epidemics: The effect of COVID-19 vaccination on change in contacts and on transmission in a US cohort"]().

# Repo summary
The code for the main analysis can be found in [contchng_vaxeffect](https://github.com/cliu822/contchng_vaxeffect). The relevant data, including survey tools and codebook are stored in [0_data](0_data). Figure outputs are in [0_plot](0_plot) and table outputs are in [0_table](0_tab).

### Description of data
| File                   | Description |
| ---------------------- | ------------- |
| [0_studymaterials](0_data/0_studymaterials)           |Survey tools and codebook|
| [0_baseline_characteristics](0_data/0_baseline_characteristics.RDS)           |Participant characteristics collected at baseline|
| [0_covid_vax_wk_co](0_data/0_covid_vax_wk_co.RDS)           |County-level vax coverage by week|
| [0_covid_vax_wk_us](0_data/0_covid_vax_wk_us.RDS)           |US vax coverage by week|
| [0_lca_class](0_data/0_data/0_lca_vlass.RDS)           |LCA classifications for each participant at baseline from [1_lca_baseline](1_lca_baseline.Rmd)|
| [0_surveydates](0_data/0_surveydates_r1r2r3r4.RDS)           |Participant survey dates for each round|
| [1_contact_long](0_data/1_contact_long_r1r2r3r4.RDS)           |Contact data linelisted by participant, survey round, location and physical vs non-physical|
| [2_behavior](0_data/2_behavior_r1r2r3r4.RDS)           |Behavior data for all four rounds linelisted by participant|
| [3_cont_long_cov](0_data/3_cont_long_cov.RDS)           |Contact data (all locations & attributed) linelisted by participant and survey round, joined with participant characteristics|
| [3_cont_long_cov](0_data/3_cont_long_cov.RDS)           |Joined vaccination and contact data, linelisted by participant, survey round and truncation choice of contact|

### Description of main analysis code

| File                   | Description |
| ---------------------- | ------------- |
| [1_lca_baseline](1_lca_baseline.Rmd)           |Latent class analysis of baseline|
| [2_desc_figs](2_desc_figs.Rmd)        | Code to generate descriptive figures|
| [2_desc_tables](2_desc_tables.Rmd)        | Code to generate descriptive tables|
| [3_model_vax_contchng](3_model_vax_contchng.R)        |Statistical model code estimating effect of vaccination on changes in contact|
| [4_r0calc](4_r0calc.R)        |Estimating R0 over time through Next Generation Matrices|
| [5_modelres_figtab](5_modelres_figtab.R)        |Code for figures and tables from model and R0 estimations|

