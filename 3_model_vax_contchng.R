library(openxlsx)
library(lme4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(stringr)

##Input: long form of a joined vaccine exposure and contact change outcome data frame with various outlier criteria
##Computes:
          #1) Univariate associations between covariate and outcome using linear mixed regression implemented with "lme4" package
          #2) Multivariate linear mixed regression using covariates determined apriori or selected through 
#             model selection for all contacts 99th percentile
##Visualizes:
          #1) Main effect estimate on all, work, other and home contacts with primary truncation method (99th percentile)
          #2) Sensitivity analysis of Effect estimate on all, work, other and home contacts for all other truncation methods

##Output: Excel sheet with full model effect estimates for outcomes of: 
                #1) all contacts; 
                #2) work contacts; 
                #3) other contacts;
                #4) home contacts
##          with the following types of location-specific right censoring to exclude extreme outlier responses unlikely to be accurate:
                #1)Censoring at 100 contact (CoMix);
                #2)Censoring at 99th percentile;
                #3)Censoring at 97.5th percentile;
                #4)Censoring at 95th percentile
            

vax_cont_long_full <- readRDS("0_data/3_vax_cont_long_full_new.RDS")%>%arrange(outcome)
vax_cont_long_full <- vax_cont_long_full%>%
                      mutate(Series_Complete_Pop_Pct = Series_Complete_Pop_Pct/20)

vax_cont_long_full <- vax_cont_long_full%>%
                      filter(!outcome%in%c("orig_all","orig_home","orig_work","orig_other","orig_school",
                      #                     "val100_home","val99_home","val975_home","val95_home",
                                           "val100_school","val99_school","val975_school","val95_school"))

#vax_cont_long_full<- vax_cont_long_full%>%filter(outcome%in%c("val99_all","val99_home","val99_other","val99_work"))
vax_cont_long_full<- vax_cont_long_full%>%
                      mutate(vax_comm_cat_all = as.factor(vax_comm_cat_all))%>%
                      mutate(variant_worry_cat =factor(variant_worry_cat,
                                                       levels = c("Decreased greatly","Decreased slightly","No change","Increased slightly","Increased greatly")))

vax_cont_list <- split(vax_cont_long_full,vax_cont_long_full$outcome)


##Function to get coefficients, standard error, tvalue and confidence interval from the lmer model output reslt
mod_res <- function(x){
  d1 <- as.data.frame(summary(x)$coefficients)
  d2 <- as.data.frame(confint(x))
  colnames(d2)<-c("lo_2.5","hi_97.5")
  d1$cat <- row.names(d1)
  d2$cat <- row.names(d2)
  d1 <- d1%>%left_join(d2, keep=F)
  d1<- d1%>%mutate_if(is.numeric, round,2)%>%
    mutate(est = paste(Estimate,"(",lo_2.5,"-",hi_97.5,")",sep=""))
  print(d1)
}

## Function that computes univariate and multivariate model and combines them
compute_uni_full <- function(x){
                    ### Univariate
                    res_list <- list()
                    res_list[[1]] <- lmer(cont_chng~vax_cat2+(1|p_id),data = x)
                    res_list[[2]]<-lmer(cont_chng~Series_Complete_Pop_Pct+(1|p_id),data=x)
                    res_list[[3]] <- lmer(cont_chng ~ age_cat1+(1|p_id), data = x)
                    res_list[[4]] <- lmer(cont_chng~ gender_i+(1|p_id),data = x)                      
                    res_list[[5]] <- lmer(cont_chng~ race_eth_i+(1|p_id),data = x)
                    res_list[[6]] <- lmer(cont_chng~ hh_cat+(1|p_id),data = x)                    
                    res_list[[7]] <- lmer(cont_chng~ pol_cat+(1|p_id),data = x)
                    res_list[[8]] <- lmer(cont_chng~ emp_cat+(1|p_id),data = x) 
                    res_list[[9]] <- lmer(cont_chng~income_cat +(1|p_id), data=x)
                    res_list[[10]] <- lmer(cont_chng~ comorb_cat+(1|p_id),data = x)
                    res_list[[11]]<-lmer(cont_chng~lca4_full+(1|p_id), data=x)
                    res_list[[12]]<- lmer(cont_chng~ base_cont+(1|p_id), data = x)
                    res_list[[13]]<-lmer(cont_chng~variant_worry_cat+(1|p_id), data = x)
                    res_list[[14]]<-lmer(cont_chng~StringencyIndex+(1|p_id), data=x)
                    #res_list[[13]]<-lmer(cont_chng~vax_comm_cat_all+(1|p_id),data=x)
         

                    res_uni<- lapply(res_list,mod_res)   ##Get the confidence intervals
                    tab_uni <-do.call(rbind, res_uni)%>% ##Compile list of univariate results into one giant dataframe
                               mutate(cat=ifelse(cat=="(Intercept)","Intercept",cat))
                    
                    ##Multivariate model
                    #mod_full4<- lmer(cont_chng ~ vax_cat2+StringencyIndex+variant_worry_cat+age_cat1+pol_cat+hh_cat+income_cat+emp_cat+base_cont+lca4_full+vax_comm_cat_all+(1|p_id), data = x)
                    mod_full4<- lmer(cont_chng ~ vax_cat2+StringencyIndex+variant_worry_cat+age_cat1+pol_cat+hh_cat+emp_cat+lca4_full+Series_Complete_Pop_Pct+(1|p_id), data = x)
                    full_res4 <- mod_res(mod_full4)
                    
                    tab_all <-tab_uni%>% 
                                  full_join(full_res4, by = c("cat"="cat"))

                    return(tab_all)
                      }

##Function to extract the uni and multi effect of vaccine on contact (main effect of interest)
get_maineff <- function(x){
              
               main_eff <- x%>%filter(cat %in% c("vax_cata1_first_new",
                                     "vax_cata2_both_new",
                                     "vax_cata2_second_new",
                                     "vax_cata3_full_vax"))%>%
                    select(cat,Estimate.x, lo_2.5.x,hi_97.5.x,Estimate.y,lo_2.5.y,hi_97.5.y)%>%
                    mutate(cat = gsub("vax_cat","",cat))
               colnames(main_eff)<- c("vaxcat","uni_est","uni_lo","uni_hi","multi_est","multi_lo","multi_hi")
               return(main_eff)
}

get_maineff1 <- function(x){
  
  main_eff <- x%>%filter(cat %in% c("vax_cat2a1_first_new",
                                    "vax_cat2a2_full_new",
                                    "vax_cat2a3_full_vax",
                                    "Series_Complete_Pop_Pct"))%>%
    select(cat,Estimate.x, lo_2.5.x,hi_97.5.x,Estimate.y,lo_2.5.y,hi_97.5.y)%>%
    mutate(cat = gsub("vax_cat2","",cat))
  colnames(main_eff)<- c("vaxcat","uni_est","uni_lo","uni_hi","multi_est","multi_lo","multi_hi")
  return(main_eff)
}

get_maineff2 <- function(x){
  
  main_eff <- x%>%filter(cat %in% c("vax_cat2a1_first_new",
                                    "vax_cat2a2_full_new",
                                    "vax_cat2a3_full_vax",
                                    "Series_Complete_Pop_Pct"))%>%
    select(cat,est, lo_2.5,hi_97.5)%>%
    mutate(cat = gsub("vax_cat2","",cat))
  colnames(main_eff)<- c("vaxcat","multi_est","multi_lo","multi_hi")
  return(main_eff)
}



##Function to take raw output and simplify into presentable table
get_tab <- function(x){
                  res_tab <-x%>%
                                select(cat, est.x, est.y)%>%
                                rename("Univariate"="est.x")%>%
                                rename("Multivariate"="est.y")
                  return(res_tab)
                    }


##Compute univariate and multivariate results
res_full_list <- lapply(vax_cont_list,compute_uni_full)
names(res_full_list)<- names(vax_cont_list)     ##Set list names

saveRDS(res_full_list, "0_data/4_res_full_list.RDS")
#####

##Extract the modeled effect estimates without concern for new variants
mod_full4<- lmer(cont_chng ~ vax_cat2+StringencyIndex+age_cat1+pol_cat+hh_cat+emp_cat+lca4_full+Series_Complete_Pop_Pct+variant_worry_cat+(1|p_id), data = vax_cont_list[[13]])
full_res4 <- mod_res(mod_full4)
write.csv(full_res4, "0_tab/res_maineffect_noconcern.csv")

lmer(cont_chng~vax_cat2+(1|p_id)+base_cont,data = vax_cont_list[[13]])
lm(cont_chng~vax_cat2+base_cont, data = vax_cont_list[[13]])
df <- vax_cont_list[[13]]
df%>%group_by(vax_cat2,vax_round)%>%dplyr::summarise(n=n(),cont_chng=mean(cont_chng))
lmer(cont_chng~vax_cat2+(1|p_id),data = df)
lm(cont_chng~vax_cat2, data = df)

mod_full4<- lmer(cont_chng ~ vax_cat2+StringencyIndex+age_cat1+pol_cat+hh_cat+emp_cat+lca4_full+variant_worry_cat+base_cont+Series_Complete_Pop_Pct+(1|p_id), data = vax_cont_list[[13]])
full_res4 <- mod_res(mod_full4)
write.csv(full_res4,"0_tab/res_maineffect.csv")

mod_full4<- lmer(cont_chng ~ vax_cat2+StringencyIndex+age_cat1+pol_cat+hh_cat+emp_cat+lca4_full+variant_worry_cat+Series_Complete_Pop_Pct+(1|p_id), data = vax_cont_list[[13]])
full_res4 <- mod_res(mod_full4)
write.csv(full_res4,"0_tab/res_maineffect_nobasecont.csv")


vax_cont_list[[13]]%>%
ggplot()+
  geom_point(aes(x=base_cont, y=cont_chng))



df <- vax_cont_list[[13]]
df<- df%>%
      mutate(variant_worry_cat =factor(variant_worry_cat,
                                       levels = c("Decreased greatly","Decreased slightly","No change","Increased slightly","Increased greatly")))
m1 <- lmer(cont_chng ~ vax_cat2+Series_Complete_Pop_Pct+variant_worry_cat+age_cat1+pol_cat+hh_cat+emp_cat+lca4_full+StringencyIndex+(1|p_id), data = df)
mod_res(m1)

mod_allcov <- lmer(cont_chng ~ vax_cat2+Series_Complete_Pop_Pct+age_cat1+gender_i+race_eth_i+hh_cat+pol_cat+emp_cat+income+comorb_cat+lca4_full+base_cont+variant_worry_cat+StringencyIndex+(1|p_id), data = vax_cont_list[[13]])
mod_allcovres <- mod_res(mod_allcov)


res_list <- list()
res_list[[1]] <- lmer(cont_chng~vax_cat2+(1|p_id),data = x)
res_list[[2]]<-lmer(cont_chng~Series_Complete_Pop_Pct+(1|p_id),data=x)
res_list[[3]] <- lmer(cont_chng ~ age_cat1+(1|p_id), data = x)
res_list[[4]] <- lmer(cont_chng~ gender_i+(1|p_id),data = x)                      
res_list[[5]] <- lmer(cont_chng~ race_eth_i+(1|p_id),data = x)
res_list[[6]] <- lmer(cont_chng~ hh_cat+(1|p_id),data = x)                    
res_list[[7]] <- lmer(cont_chng~ pol_cat+(1|p_id),data = x)
res_list[[8]] <- lmer(cont_chng~ emp_cat+(1|p_id),data = x) 
res_list[[9]] <- lmer(cont_chng~income_cat +(1|p_id), data=x)
res_list[[10]] <- lmer(cont_chng~ comorb_cat+(1|p_id),data = x)
res_list[[11]]<-lmer(cont_chng~lca4_full+(1|p_id), data=x)
res_list[[12]]<- lmer(cont_chng~ base_cont+(1|p_id), data = x)
res_list[[13]]<-lmer(cont_chng~variant_worry_cat+(1|p_id), data = x)
res_list[[14]]<-lmer(cont_chng~StringencyIndex+(1|p_id), data=x)
#res_list[[13]]<-lmer(cont_chng~vax_comm_cat_all+(1|p_id),data=x)

