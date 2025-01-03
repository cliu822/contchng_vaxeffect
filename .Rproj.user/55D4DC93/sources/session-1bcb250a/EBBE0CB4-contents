##Back selection nobase

library(lme4)

# Full model
mod_allcov <- lmer(cont_chng ~ vax_cat2 + Series_Complete_Pop_Pct + age_cat1 + gender_i + 
                     race_eth_i + hh_cat + pol_cat + emp_cat + income + comorb_cat + 
                     lca4_full + base_cont + variant_worry_cat + StringencyIndex + (1 | p_id), 
                   data = vax_cont_list[[13]])

mod_nobase <- lmer(cont_chng ~ vax_cat2 + Series_Complete_Pop_Pct + age_cat1 + gender_i + 
                     race_eth_i + hh_cat + pol_cat + emp_cat + income + comorb_cat + 
                     lca4_full +  variant_worry_cat + StringencyIndex + (1 | p_id), 
                   data = vax_cont_list[[13]])

# List of covariates to consider removing (excluding main exposure and random effects)
covariates <- c("Series_Complete_Pop_Pct",  "gender_i", "race_eth_i", 
                "pol_cat", "emp_cat", "income", "comorb_cat", 
                "lca4_full", "variant_worry_cat")

# Initialize results storage
res_list <- list()

# Iterate through covariates, removing one at a time
for (cov in covariates) {
  # Create formula without the current covariate
  formula <- as.formula(paste("cont_chng ~ vax_cat2 +age_cat1+hh_cat +StringencyIndex+",
                              paste(setdiff(covariates, cov), collapse = " + "),
                              "+ (1 | p_id)"))
  
  form_text <- paste("cont_chng ~ vax_cat2 +age_cat1+hh_cat +StringencyIndex+",
                     paste(setdiff(covariates, cov), collapse = " + "),
                     "+ (1 | p_id)")
  
  # Fit the model
  model <- lmer(formula, data = vax_cont_list[[13]])
  
  # Extract vax_cat2 effect estimate and standard error
  res_list[[cov]] <- get_maineff2(mod_res(model))%>%
    mutate(formula=cov)
  
  
}

res_list_df <- do.call(bind_rows,res_list)
res_list_df<- res_list_df%>%
  mutate(formula = c(rep(covariates[1],each=3),rep(covariates[2:9], each=4)))%>%
  filter(vaxcat=="a2_full_new")

#######################################################
####Remove comorb cat, effect estimate at 1.98#########
#######################################################

# List of covariates to consider removing (excluding main exposure and random effects)
covariates <- c("Series_Complete_Pop_Pct",  "gender_i", "race_eth_i", 
                "pol_cat", "emp_cat", "income",
                "lca4_full", "variant_worry_cat")

# Initialize results storage
res_list <- list()

# Iterate through covariates, removing one at a time
for (cov in covariates) {
  # Create formula without the current covariate
  formula <- as.formula(paste("cont_chng ~ vax_cat2 +age_cat1+hh_cat +StringencyIndex+",
                              paste(setdiff(covariates, cov), collapse = " + "),
                              "+ (1 | p_id)"))
  
  form_text <- paste("cont_chng ~ vax_cat2 +age_cat1+hh_cat +StringencyIndex+",
                     paste(setdiff(covariates, cov), collapse = " + "),
                     "+ (1 | p_id)")
  
  # Fit the model
  model <- lmer(formula, data = vax_cont_list[[13]])
  
  # Extract vax_cat2 effect estimate and standard error
  res_list[[cov]] <- get_maineff2(mod_res(model))%>%
    mutate(formula=cov)
  
  
}

res_list_df2 <- do.call(bind_rows,res_list)
res_list_df2<- res_list_df2%>%
  mutate(formula = c(rep(covariates[1],each=3),rep(covariates[2:8], each=4)))

res_list_df2%>%
  filter(vaxcat=="a2_full_new")

#################################################
####Remove gender_i, effect estimate at 1.97######
#################################################

# List of covariates to consider removing (excluding main exposure and random effects)
covariates <- c("Series_Complete_Pop_Pct", "race_eth_i", 
                "pol_cat", "emp_cat", "income",
                "lca4_full", "variant_worry_cat")

# Initialize results storage
res_list <- list()

# Iterate through covariates, removing one at a time
for (cov in covariates) {
  # Create formula without the current covariate
  formula <- as.formula(paste("cont_chng ~ vax_cat2 +age_cat1+hh_cat +StringencyIndex+",
                              paste(setdiff(covariates, cov), collapse = " + "),
                              "+ (1 | p_id)"))
  
  
  # Fit the model
  model <- lmer(formula, data = vax_cont_list[[13]])
  
  # Extract vax_cat2 effect estimate and standard error
  res_list[[cov]] <- get_maineff2(mod_res(model))%>%
    mutate(formula=cov)
  
  
}

res_list_df3 <- do.call(bind_rows,res_list)
res_list_df3<- res_list_df3%>%
  mutate(formula = c(rep(covariates[1],each=3),rep(covariates[2:7], each=4)))%>%
  filter(vaxcat=="a2_full_new")


#################################################
####Remove income, effect estimate at 1.97######
#################################################

# List of covariates to consider removing (excluding main exposure and random effects)
covariates <- c("Series_Complete_Pop_Pct", "race_eth_i", 
                "pol_cat", "emp_cat", 
                "lca4_full", "variant_worry_cat")

# Initialize results storage
res_list <- list()

# Iterate through covariates, removing one at a time
for (cov in covariates) {
  # Create formula without the current covariate
  formula <- as.formula(paste("cont_chng ~ vax_cat2 +age_cat1+hh_cat +StringencyIndex+",
                              paste(setdiff(covariates, cov), collapse = " + "),
                              "+ (1 | p_id)"))
  
  
  # Fit the model
  model <- lmer(formula, data = vax_cont_list[[13]])
  
  # Extract vax_cat2 effect estimate and standard error
  res_list[[cov]] <- get_maineff2(mod_res(model))%>%
    mutate(formula=cov)
  
  
}

res_list_df4 <- do.call(bind_rows,res_list)
res_list_df4<- res_list_df4%>%
  mutate(formula = c(rep(covariates[1],each=3),rep(covariates[2:6], each=4)))%>%
  filter(vaxcat=="a2_full_new")

#################################################
####Remove emp cat, effect estimate at 1.96######
#################################################

# List of covariates to consider removing (excluding main exposure and random effects)
covariates <- c("Series_Complete_Pop_Pct", "race_eth_i",
                "pol_cat",
                "lca4_full",  "variant_worry_cat")

# Initialize results storage
res_list <- list()

# Iterate through covariates, removing one at a time
for (cov in covariates) {
  # Create formula without the current covariate
  formula <- as.formula(paste("cont_chng ~ vax_cat2 +age_cat1+hh_cat +StringencyIndex+",
                              paste(setdiff(covariates, cov), collapse = " + "),
                              "+ (1 | p_id)"))
  
  
  # Fit the model
  model <- lmer(formula, data = vax_cont_list[[13]])
  
  # Extract vax_cat2 effect estimate and standard error
  res_list[[cov]] <- get_maineff2(mod_res(model))%>%
    mutate(formula=cov)
  
  
}

res_list_df5 <- do.call(bind_rows,res_list)
res_list_df5<- res_list_df5%>%
  mutate(formula = c(rep(covariates[1],each=3),rep(covariates[2:5], each=4)))%>%
  filter(vaxcat=="a2_full_new")


#################################################
####Remove emp cat, effect estimate at 1.96######
#################################################

# List of covariates to consider removing (excluding main exposure and random effects)
covariates <- c("Series_Complete_Pop_Pct", "race_eth_i",
                "lca4_full",  "variant_worry_cat")

# Initialize results storage
res_list <- list()

# Iterate through covariates, removing one at a time
for (cov in covariates) {
  # Create formula without the current covariate
  formula <- as.formula(paste("cont_chng ~ vax_cat2 +age_cat1+hh_cat +StringencyIndex+",
                              paste(setdiff(covariates, cov), collapse = " + "),
                              "+ (1 | p_id)"))
  
  
  # Fit the model
  model <- lmer(formula, data = vax_cont_list[[13]])
  
  # Extract vax_cat2 effect estimate and standard error
  res_list[[cov]] <- get_maineff2(mod_res(model))%>%
    mutate(formula=cov)
  
  
}

res_list_df6 <- do.call(bind_rows,res_list)
res_list_df6<- res_list_df6%>%
  mutate(formula = c(rep(covariates[1],each=3),rep(covariates[2:4], each=4)))%>%
  filter(vaxcat=="a2_full_new")