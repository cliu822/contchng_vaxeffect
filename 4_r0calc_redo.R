library("dplyr")
library("ggplot2")
library("RColorBrewer")
library("viridis")

surveydt <- readRDS("0_data/0_surveydates_r1r2r3r4.RDS")
vax_coverage<- read.csv("0_data/0_covidvax_coverage.csv")

vax_coverage <- vax_coverage%>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y"))%>%
  filter(Location=="US")

r2_meddt <- median(surveydt$m3_date_completed__r2)
r3_meddt <- median(surveydt$m6_date_completed__r3)
r4_meddt <- median(surveydt$m18_dt_complete__r4)

vax_cov <- vax_coverage%>%
  filter(Date%in%c(r2_meddt,r3_meddt,r4_meddt))%>%
  select(Date, Series_Complete_Pop_Pct)

vax_cov$round <- c("round4","round3","round2") 


getr0<- function(x){
  r0_init = x["r0_init"]
  cont_pre =x["cont_pre"]
  
  #r0_init=3
  #cont_pre=16
  cont_pre_red = 5.333
  dur = 7
  beta_orig = r0_init/cont_pre/7
  
  cont_vv = x["cont_vv"]
  cont_vu = x["cont_vu"]
  cont_uv =x["cont_uv"]
  cont_uu =x["cont_uu"]
  
  ve_s = x["ve_s"]
  ve_i = x["ve_i"]
  
  df99 <- data.frame(inf = c("inf_v","inf_v","inf_u","inf_u"),
                     sus = c("sus_v", "sus_u", "sus_v", "sus_u"),
                     cont = c(cont_vv, cont_vu, cont_uv, cont_uu)) %>%
    mutate(beta = c(beta_orig*(1-ve_s), beta_orig, 
                    beta_orig*(1-ve_s),beta_orig),
           
           R = cont*beta*dur)
  
  mat<- matrix(df99$R, nrow=2, ncol=2)
  
  max(eigen(mat)$values)
}

###
survey_cont <- data.frame(cont_v = c(8.6, 11.2,14.7,7.6,10.4,13.6,9.5,12.1,15.8),
                          cont_u = c(10.8,15.2,15.9,9.5,11.7,11.9,12,18.6,20),
                          round = rep(c("round2","round3","round4"),3),
                          est_cat=rep(c("est","lo","hi"),each=3))

df5 <- expand.grid(ve_s=seq(from=0, to=0.95, by=0.05),
                   round = c("round2","round3","round4"),
                   est_cat= c("est","lo","hi"),
                   gamma=seq(from = 1, to =100, by=1))

df5 <- df5%>%left_join(vax_cov,by = c("round"))%>%
  rename("cov"="Series_Complete_Pop_Pct")%>%
  mutate(cov=cov/100)

df5 <- df5 %>%
  left_join(survey_cont, by =c("round","est_cat"))%>%
  mutate(cont_vv = cont_v*cov,
         cont_vu = cont_v*(1-cov),
         cont_uv = cont_u*cov,
         cont_uu =cont_u*(1-cov),
         r0_init =3, 
         cont_pre = 16,
         ve_i=0)

df5 <- df5%>%
  mutate(cont_vv = cont_vv+cont_vu*0.01*gamma,
         cont_vv = ifelse(cont_vv<=cont_v, cont_vv, cont_v),
         cont_vu = cont_v-cont_vv,
         cont_uu = cont_uu+cont_uv*0.01*gamma,
         cont_uu = ifelse(cont_uu<=cont_u, cont_uu, cont_u),
         cont_uv = cont_u-cont_uu,
         
         Mvv = cont_vv/(cont_vv+cont_uv),
         Muu = cont_uu/(cont_uu+cont_vu),
         Q = Mvv+Muu-1)

df_add <- df5%>%select(cont_v,cont_u,round,cov,gamma,round,ve_i, Mvv,Muu,Q,est_cat)
df5 <- df5%>%select(-cont_v,-cont_u,-round,-cov,-gamma,-round, -ve_i, -Mvv, -Muu, -Q, -Date,-est_cat)

df5$r0 <- apply(df5,1,getr0)

df5 <-cbind(df5,df_add)

###Pick just 
df5_sub <- df5%>%
           mutate(Q_cat=case_when(
                         Q<0.23&Q>0.17~"Proportional",
                         Q<0.53&Q>0.47~"Medium",
                         Q<0.83&Q>0.77~"Assortative"
                         #Q<0.23&Q>0.17~0.2,
                         #Q<0.53&Q>0.47~0.5,
                         #Q<0.93&Q>0.87~0.9
                       ))%>%
           filter(ve_s%in% c(0.1,0.55,0.9)&(!is.na(Q_cat)))

df_med <- df5_sub%>%group_by(round,est_cat,ve_s,Q_cat)%>%
        summarise(mean =mean(r0))%>%
        pivot_wider(names_from = "est_cat",values_from = "mean")%>%
        mutate(Q_cat = factor(Q_cat, levels = c("Proportional","Medium","Assortative")),
               ve_s = paste0("VE=",ve_s*100,"%"))
        

pal <- brewer.pal(n=9, "RdPu")

p1<- df_med%>%
  ggplot()+
  geom_point(aes(x=round,y=est, color=Q_cat),size=2,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=round,y=est, color=Q_cat,ymin=lo, ymax=hi), width=.1,position=position_dodge(width=0.5))+
               ylim(1,4)+
  geom_hline(yintercept =1.575, colour="orange1", linetype="dashed", size=1.0)+
  geom_hline(yintercept= 3, colour="lightgoldenrod1",linetype="dashed",size=1.0)+
  #geom_text(aes(y=3.2,x=2),label="No distancing no vaccine",size=2)+
  scale_color_manual("Assumed mixing",values=pal[c(4,6,8)])+
  facet_wrap(~ve_s)+theme_bw()+ylab("Estimated R")+
  ggtitle("Estimated R accounting for vaccination and contact change")+
  xlab("Vaccine effectiveness against infection")

png("0_plot/2_R_round_ve_Q.png",width=10,height=5, units="in",res=300)
p1
dev.off()



p2 <- df_med%>%
  filter(Q_cat=="Medium")%>%
  ggplot()+
  geom_point(aes(x=ve_s,y=est, color=round),size=2,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=ve_s,y=est, color=round,ymin=lo, ymax=hi), width=.1,position=position_dodge(width=0.5))+
  ylim(1,4)+
  geom_hline(yintercept =1.575, colour="orange1", linetype="dashed", size=1.0)+
  geom_hline(yintercept= 3, colour="lightgoldenrod1",linetype="dashed",size=1.0)+
  scale_color_manual(values=pal[c(4,6,8)])+
  theme_bw()+ylab("Estimated R")+
  ggtitle("Estimated R accounting for vaccination and contact change")+
  xlab("Vaccine effectiveness against infection")

png("0_plot/2_R_round_ve.png",width=8,height=5, units="in",res=300)
p2
dev.off()


p3<- df_med%>%
  filter(ve_s=="VE=55%")%>%
  ggplot()+
  geom_point(aes(x=Q_cat,y=est, color=round),size=4,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=Q_cat,y=est, color=round,ymin=lo, ymax=hi), 
                size=1.5,width=.2,position=position_dodge(width=0.5))+
  ylim(1,3.5)+
  geom_hline(yintercept =1.575, colour="orange1", linetype="dashed", size=1.5)+
  geom_hline(yintercept= 3, colour="lightgoldenrod1",linetype="dashed",size=1.5)+
  #geom_text(aes(y=3.2,x=2),label="No distancing no vaccine",size=2)+
  scale_color_manual("Round",values=pal[c(4,6,8)])+
  theme_bw()+theme(axis.text = element_text(size=13),
                   axis.title= element_text(size=13),
                   legend.text=element_text(size=13),
                   legend.title=element_text(size=13),
                   legend.position = "bottom",
                   plot.title= element_text(size=16))+
  ylab("Estimated R")+
  ggtitle("B. Estimated R accounting for vaccination and contact change")+
  xlab("Assumed mixing between vaxed and unvaxed")

png("0_plot/2_R_round_Q.png",width=10,height=6, units="in",res=200)
p3
dev.off()

##Try a plot with 



