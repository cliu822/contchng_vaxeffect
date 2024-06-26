##Read in main results from model
res_full_list <- readRDS("0_data/4_res_full_list.RDS")
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

##Function to take raw output and simplify into presentable table
get_tab <- function(x){
  res_tab <-x%>%
    select(cat, est.x, est.y)%>%
    rename("Univariate"="est.x")%>%
    rename("Multivariate"="est.y")
  return(res_tab)
}


##Get the main effect
res_main_list<- lapply(res_full_list, get_maineff1)
res_main<- dplyr::bind_rows(res_main_list, .id = 'model')

##Get tables for export
res_tab_list <- lapply(res_full_list,get_tab)
write.xlsx(res_tab_list, "0_tab/res_maineffect_full.xlsx")


###Visualizations for main effect
empty <- data.frame(model ="val99_all",vaxcat = "",mod=rep(c("Multivariate","Univariate"),each=4),truncate="val99",outcome=c("all","other","work","home"),
                    est = NA, lo=NA, hi=NA)

##Wrangle for all truncations
res_main<- res_main%>%
  pivot_longer(cols=uni_est:multi_hi, names_to="var",values_to="values")%>%
  #mutate(values = ifelse(vaxcat=="Series_Complete_Pop_Pct",values*20,values))%>%
  mutate(mod= sub("_.*", "", var),         ##Extract pattern before underscore, uni or multi model
         var= sub(".*_", "", var),         ##Extract pattern after underscore, estimate type, either estimate, lo or hi bounds
         truncate =sub("_.*", "", model),  ##Type of truncation (100 contacts, 99th, 97.5th or 95th percentile)
         outcome = sub(".*_", "", model)   ##Type of outcome (all, other, work or home)
  )%>%   
  pivot_wider(names_from = "var",values_from = "values")%>%
  
  
  bind_rows(empty)%>%
  ##Fixing labels
  mutate(vaxcat = case_when(
    vaxcat == ""~"",
    vaxcat =="a1_first_new"~"First dose new",
    #vaxcat == "a2_both_new"~"Both dose new",
    vaxcat=="a2_full_new"~"Newly completed series",
    vaxcat=="a3_full_vax"~"Already fully vaxed",
    vaxcat=="Series_Complete_Pop_Pct"~"Every 20% increase in vax coverage"),
    vaxcat=factor(vaxcat, levels = c("Every 20% increase in vax coverage","","First dose new","Both dose new","Newly completed series","Already fully vaxed")),
    
    truncate = case_when(
      truncate=="val100"~"100 contacts",
      truncate=="val99" ~"99th percentile",
      truncate=="val975"~"97.5th percentile",
      truncate=="val95"~"95th percentile"),
    
    mod = ifelse(mod=="uni","Univariate","Multivariate"),
    outcome = factor(outcome, levels = c("all","other","work","home")))%>%
  
  arrange(truncate,outcome,mod,vaxcat)

###Plot figures
theme <-  theme(legend.position = c(0.87, 0.32), 
        legend.text = element_text(size=14),
        legend.title=element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title = element_text(size=14),
        plot.title = element_text(size=16),
        strip.text = element_text(size=14))
## Main effect estimates using 99th percentile truncation for all locations
p1_main_loc <-res_main %>%
                  filter(truncate=="99th percentile")%>%
                  mutate(outcome = case_when(
                    outcome=="all"~"All",
                    outcome=="other"~"Other",
                    outcome=="work"~"Work",
                    outcome=="home"~"Home"
                  ),
                  outcome = factor(outcome, levels= c("All","Work","Other","Home")))%>%
                  ggplot(aes(x=vaxcat, color=mod, group=mod))+
                  geom_point(aes(y=est),size=5,position=position_dodge(width=0.5))+
                  geom_errorbar(aes(ymin = lo, ymax=hi),width=0.2, size=1, position=position_dodge(width=0.5))+
                  geom_hline(yintercept=0, linetype="dashed")+
                  scale_color_brewer(palette = "YlGnBu",direction=-1, name="Model")+
                  scale_x_discrete(labels=function(x) str_wrap(x,width=22))+
                  theme_bw()+
                  ggtitle("A. Model results")+ylab("Difference in contact change compared to contact change in unvaccinated")+
                  xlab("Vaccination status")+
                  coord_flip()+
                  facet_wrap(~outcome, ncol=2)+theme


png("0_plot/p1_main_loc_v2.png",width=10,height=6, units="in",res=200)
p1_main_loc
dev.off()

###Main effect for all contacts
p1_main <- res_main%>%
              filter(truncate=="99th percentile")%>%
              filter(outcome=="all")%>%
              filter(vaxcat!="Every 20% increase in vax coverage")%>%
              mutate(outcome = case_when(
                outcome=="all"~"All"))%>%
  
            ggplot(aes(x=vaxcat, color=mod, group=mod))+
                  geom_point(aes(y=est),size=5,position=position_dodge(width=0.5))+
                  geom_errorbar(aes(ymin = lo, ymax=hi),width=0.2, size=1, position=position_dodge(width=0.5))+
                  geom_hline(yintercept=0, linetype="dashed")+
                  scale_color_brewer(palette = "YlGnBu",direction=-1, name="Model")+
                  scale_x_discrete(labels=function(x) str_wrap(x,width=12))+
                  theme_bw()+
                  ggtitle("Effect of vaccination on change in contact rates")+ylab("Difference in contact change compared to contact change \namong unvaccinated")+
                  xlab("Vaccination exposure")+
                  coord_flip()+theme(legend.position = c(0.83, 0.15), 
                                     legend.text = element_text(size=13),
                                     legend.title=element_text(size=12),
                                     axis.text.y = element_text(size=12),
                                     axis.text.x = element_text(size=12),
                                     axis.title = element_text(size=12),
                                     plot.title = element_text(size=14))

png("0_plot/p1_main_single_nocommvax.png",width=6,height=6, units="in",res=400)
p1_main
dev.off()

###Images 
df1 <- readRDS("0_data/4_r0_varyQcov.RDS")
us_cov<- data.frame(us_cov = c(0.115,0.498,0.659),
                    round = c("Round 2","Round 3","Round 4"))

df1a <- df1%>%
  mutate(r0_rel = r0/3)%>%
  filter(cov!=0 &cov!=1)%>%
  mutate(Q=round(Q,digits=1),
         round = case_when(
           round=="round2"~"Round 2",
           round=="round3"~"Round 3",
           round=="round4"~"Round 4"
         ))%>%
  group_by(cov,Q,round)%>%
  summarise(r0_rel = mean(r0_rel))

p4 <- 
  ggplot()+
  geom_raster(data=df1a,aes(x=Q, y=cov, z=r0_rel, fill=r0_rel))+
  geom_contour(data=df1a,aes(x=Q,y=cov,z=r0_rel),color="black",alpha=0.7)+
  geom_hline(data=us_cov,aes(yintercept=us_cov), linetype="dashed", color = "red",size=1)+
  geom_text(data=us_cov,aes(y=us_cov,x=0.5,label = "US coverage",vjust=-0.3), color="red", size=5)+
  scale_fill_viridis_c(breaks = c(0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1), "Ratio \nof Rt/R0")+
  facet_wrap(~round)+theme_bw()+xlab("Q (Assortativity index)")+ylab("Vaccine coverage")+
  ggtitle("B. Relative transmissibility based on measured contact in vaxed and unvaxed for each round")+
  theme(legend.position = "left",
        legend.text = element_text(size=13),
        legend.title=element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=16),
        axis.title = element_text(size=16),
        plot.title = element_text(size=20),
        strip.text = element_text(size=16),
        plot.margin=unit(c(0.7,0.2,1,0.1),"cm"))

png("0_plot/1_fig2_v5.png",width=14,height=12, units="in",res=400)
gridExtra::grid.arrange(p1_main_loc,p4,ncol=1, nrow=2, 
                        layout_matrix=rbind(1,2))
dev.off()



png("0_plot/p1_main.png",width=12,height=6, units="in",res=400)
p1_main_loc
dev.off()

##Supplementary for different truncations
p1_supp <- res_main%>%
  ggplot(aes(x=vaxcat, color=mod, group=mod))+
  geom_point(aes(y=est),size=5,position=position_dodge(width=0.7))+
  geom_errorbar(aes(ymin = lo, ymax=hi),width=0.4, size=1.0, position=position_dodge(width=0.7))+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_color_brewer(palette = "YlGnBu",direction=-1, name="Model")+
  theme_bw()+
  ggtitle("Model results")+ylab("Difference in contact change compared to contact change among unvaccinated")+
  xlab("Vaccination status")+
  coord_flip()+
  facet_grid(rows = vars(truncate),cols=vars(outcome))+
  theme(legend.position = "bottom",
        axis.text = element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size=16),
        strip.text = element_text(size=16),
        legend.text = element_text(size=16))

png("0_plot/1_modres_supp_v5.png",width=12,height=12, units="in",res=500)
p1_supp
dev.off()


##Supplementary varying across VEs for rel transmission
df <- readRDS("0_data/4_r0_ve_varyQ.RDS")

p2 <- df%>%
  mutate(rel_r0 = r0/3,
         ve_s = as.factor(ve_s))%>%
  filter(ve_s %in% c(0, 0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80))%>%
  filter(est_cat=="est")%>%
  mutate(ve_s = factor(ve_s, levels = c(0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0)),
         round = case_when(
           round=="round2"~"Round 2",
           round=="round3"~"Round 3",
           round=="round4"~"Round 4"
         ))%>%
  ggplot(aes(x=Q, y=rel_r0,group=ve_s))+
  scale_color_brewer(palette="BuPu",direction=1,"Round")+
  #geom_point(aes(size=ve_s, color=ve_s),alpha=1)+
  scale_size_manual(values=c(0.6,0.9,1.1,1.1,1.2,1.2,1.3,1.3,1.4))+
  guides(size="none")+
  geom_line(alpha=1, size=0.8, aes(color=ve_s))+
  facet_wrap(~round)+theme_bw()+
  xlab("Q (Assortativity coefficient)")+
  ylab("Relative transmissibility (Ratio of Rt/R0)")+
  ggtitle("Relative transmissibility based on measured contact & vax coverage for each round")+
  theme(legend.position = "left",
        legend.text = element_text(size=13),
        legend.title=element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=16),
        axis.title = element_text(size=16),
        plot.title = element_text(size=20),
        strip.text = element_text(size=16),
        plot.margin=unit(c(2,0.2,1,0.1),"cm"))

png("0_plot/3_supp10.png", width=12, height=6, units="in",res=400)
p2
dev.off()


