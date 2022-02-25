rm(list=ls())
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(MASS)
library(broom)
library(plotly)
library(hrbrthemes)
library(gridExtra)
library(msm)
library(ggpubr)

select<-dplyr::select
load("data/final_data.rdata")
load("data/weekly.rdata")
load("data/elections.rdata")
head(final_nola_geo)
head(final_deaths)
head(final_tract)

#descriptives
#see summaries in data_management for # of cases/tests/positives by geo
#number of non-matched tests determined by # of tests total (on website)- tests in CT dataset

#Figure 1

#switch order of geographies
weekly<-weekly%>%
  mutate(nola_geo=fct_rev(nola_geo))

figure1a<-ggplot(weekly, aes(x=date_endweek1, y=cases, fill=nola_geo))+
  geom_area()+
  scale_x_date(limits=c(as_date("2020-03-04"), max(weekly$date_endweek1)), 
               breaks="1 month", 
               date_labels ="%b%y", 
               expand=expansion(mult=0))+
         scale_y_continuous(expand=expansion(mult=0)) +
    scale_fill_brewer(type="qual", palette=2, direction=-1)+
  labs(x="", y="New Case", title="Weekly Cases in Louisiana")+
  guides(fill=F)+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        legend.position = "bottom",
        legend.title=element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank(), 
        legend.text=element_text(color="black", size=14), 
        axis.title = element_text(color="black", size=12))
figure1a
ggsave(figure1a, file="Results/Figure1a.pdf", width=15, height=6)


figure1b<-ggplot(weekly, aes(x=date_endweek1, y=cases, fill=nola_geo))+
  geom_area(position="fill")+
  scale_x_date(limits=c(as_date("2020-03-04"), max(weekly$date_endweek1)), 
               breaks="1 month", 
               date_labels ="%b%y", 
               expand=expansion(mult=0))+ 
  scale_y_continuous(expand=expansion(mult=c(0, 0)),labels = scales::percent)+
  scale_fill_brewer(type="qual", palette=2, direction=-1)+  
    labs(x="", y="Proportion of Cases")+
  theme_bw()+
    theme(axis.text=element_text(color="black", size=14),
          legend.position = "bottom",
          legend.title=element_blank(), 
          panel.background = element_blank(),
          plot.background = element_blank(), 
          legend.text=element_text(color="black", size=14), 
          axis.title = element_text(color="black", size=12))

figure1b
ggsave(figure1b, file="Results/Figure1b.pdf", width=15, height=6)


#combine into one plot 

figure1<-grid.arrange(figure1a,figure1b,
             ncol = 1, nrow = 2)
g <- arrangeGrob(figure1a,figure1b,  nrow=2) #generates g
ggsave(g, file="Results/figure1.pdf", width=15, height=10) #saves g

# figure 1: pol version
figure1a<-ggplot(weekly_elect, aes(x=date_endweek1, y=cases, fill=color))+
  geom_area()+
  scale_x_date(limits=c(as_date("2020-03-04"), max(weekly$date_endweek1)), 
               breaks="1 month", 
               date_labels ="%b%y", 
               expand=expansion(mult=0))+
  scale_y_continuous(expand=expansion(mult=0)) +
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  labs(x="", y="New Case", title="Weekly Cases in Louisiana")+
  guides(fill=F)+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        legend.position = "bottom",
        legend.title=element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank(), 
        legend.text=element_text(color="black", size=14), 
        axis.title = element_text(color="black", size=12))
figure1a
ggsave(figure1a, file="Results/Figure1a_elect.pdf", width=15, height=6)


figure1b<-ggplot(weekly_elect, aes(x=date_endweek1, y=cases, fill=color))+
  geom_area(position="fill")+
  scale_x_date(limits=c(as_date("2020-03-04"), max(weekly$date_endweek1)), 
               breaks="1 month", 
               date_labels ="%b%y", 
               expand=expansion(mult=0))+ 
  scale_y_continuous(expand=expansion(mult=c(0, 0)),labels = scales::percent)+
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  labs(x="", y="Proportion of Cases")+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        legend.position = "bottom",
        legend.title=element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank(), 
        legend.text=element_text(color="black", size=14), 
        axis.title = element_text(color="black", size=12))

figure1b
ggsave(figure1b, file="Results/Figure1b_elect.pdf", width=15, height=6)

#combine into one plot 

figure1<-grid.arrange(figure1a,figure1b,
                      ncol = 1, nrow = 2)
g <- arrangeGrob(figure1a,figure1b,  nrow=2) #generates g
ggsave(g, file="Results/figure1_elect.pdf", width=10, height=8) #saves g


# figure 2:
dta_f1b<-final_nola_geo %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                         month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                         month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
           group_by(nola_geo, wave) %>%
  summarise(months=as.numeric(case_when(wave=="First"~4, 
                             wave=="Second"~4,
                             wave=="Third" ~5, 
                             wave=="Vaccine Rollout"~3, 
                             wave=="Fourth"~4)),
            cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>% 
  mutate(testing_rate=(tests/months)/pop*10000,
         incidence_rate=(cases/months)/pop*10000,
         positivity_ratio=(positives/months)/(tests/months)*100) %>% 
  pivot_longer(cols = c(incidence_rate, testing_rate, positivity_ratio)) %>%
  select(nola_geo, wave, name, value)

dta_f1c<-final_deaths_geo %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6)  ~"First", 
                         month %in% c(7, 8, 9 , 10) ~"Second", 
                         month %in% c(11, 12, 13, 14, 15) ~"Third", 
                         month %in% c( 16, 17, 18) ~"Vaccine Rollout", 
                         month %in% c(19, 20, 21, 22) ~"Fourth")) %>%
  group_by(nola_geo,wave) %>% 
  summarise(death=sum(death_geo),
            pop=sum(geo_pop), 
            months=as.numeric(max(month)-min(month)+1, units="months")) %>% 
  mutate( value= (death/months)/pop*10000,
          name="mortality")%>%
  select(nola_geo, wave, name, value)

dta_figure2_final<-bind_rows(dta_f1b, dta_f1c) %>% 
  filter(name!="testing_rate") %>% 
  mutate(name=factor(name, levels=c( "positivity_ratio",
                                    "incidence_rate", 
                                    "mortality"),
                     labels = c("Positivity", "Incidence", "Mortality"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Vaccine Rollout", 
                               "Fourth"))) 
figure2b<-ggplot(dta_figure2_final, aes(x=wave, y=value)) +
  geom_line(aes(color=nola_geo, group=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=4)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="Wave", y="Rate per 10,000 or positivity ratio (%)")+
  facet_wrap(~name, scales = "free_y") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=10),
        axis.title=element_text(color="black", size=10, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure2b
ggsave(figure2b, file="Results/Figure2.pdf", width=17, height=6)

ggplotly(figure2b)


# figure 2 by pol
dta_f1b<-final_color %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                         month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                         month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  group_by(color, wave) %>% 
  summarise(months=as.numeric(case_when(wave=="First"~4, 
                                        wave=="Second"~4,
                                        wave=="Third" ~5, 
                                        wave=="Vaccine Rollout"~3, 
                                        wave=="Fourth"~4)),
            cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>% 
  mutate(testing_rate=(tests/months)/pop*10000,
         incidence_rate=(cases/months)/pop*10000,
         positivity_ratio=(positives/months)/(tests/months)*100) %>% 
  pivot_longer(cols = c(incidence_rate, testing_rate, positivity_ratio)) %>% 
  select(color, wave, name, value)
dta_f1c<-final_deaths_color %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6)  ~"First", 
                         month %in% c(7, 8, 9 , 10) ~"Second", 
                         month %in% c(11, 12, 13, 14, 15) ~"Third", 
                         month %in% c(16, 17, 18) ~"Vaccine Rollout", 
                         month %in% c(19, 20, 21, 22) ~"Fourth")) %>%
  group_by(color, wave) %>% 
  summarise(death=sum(death_color),
            pop=sum(geo_pop), 
            months=as.numeric(max(month)-min(month)+1, units="months")) %>% 
  mutate( value=(death/months)/pop*10000,
          name="mortality") %>% 
  select(color, wave, name, value)
dta_figure2_final<-bind_rows(dta_f1b, dta_f1c) %>% 
  filter(name!="testing_rate") %>% 
  mutate(name=factor(name, levels=c( "positivity_ratio",
                                     "incidence_rate", 
                                     "mortality"),
                     labels = c("Positivity", "Incidence", "Mortality"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Vaccine Rollout", 
                               "Fourth"))) %>% 
  mutate(color=factor(color, levels=c("blue", "purple", "red"),
                      labels=c("Democrat", "Mixed", "Republican")))
figure2b_elect<-ggplot(dta_figure2_final, aes(x=wave, y=value)) +
  geom_line(aes(color=color, group=color)) +
  geom_point(aes(fill=color), color="black", pch=21,size=4)+
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  scale_color_manual(values=c("blue", "purple", "red"), name="")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="Wave", y="Rate per 10,000 or positivity ratio (%)")+
  facet_wrap(~name, scales = "free_y") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=10),
        axis.title=element_text(color="black", size=10, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure2b_elect
ggsave(figure2b_elect, file="Results/Figure2_elect.pdf", width=17, height=6)

ggplotly(figure2b_elect)
 # combine both
figure2<-arrangeGrob(grobs=list(figure2b, figure2b_elect), ncol=1)
ggsave(figure2, file="Results/Figure2_new.pdf", width=17, height=12)



# figure 3  by elections

quintiles<-final_tract %>% 
  full_join(elect) %>% 
  # getting a unique value for each census tract
  filter(month==12) %>% 
  group_by(color) %>% 
  mutate(svi_q=cut(RPL_THEMES, 
                   breaks = quantile(RPL_THEMES, probs=seq(0, 1, by=0.2), na.rm=T), 
                   include.lowest = T) %>% as.numeric) %>%
  select(color, tract_fips, svi_q, svi=RPL_THEMES)

quintiles<-full_join(final_tract, quintiles) %>% 
  # test
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                         month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout", 
                         month %in% c(7, 8, 9, 10) & year==2021~"Fourth"))%>%
  group_by(color, wave, svi_q) %>%
  summarise(months=case_when(wave=="First"~4, 
                      wave=="Second"~4,
                      wave=="Third" ~5, 
                      wave=="Vaccine Rollout"~3, 
                      wave=="Fourth"~4),
            cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018))%>%
  mutate(testing_rate=(tests/months)/pop*10000,
         incidence_rate=(cases/months)/pop*10000,
         positivity_ratio=(positives/months)/(tests/months)*100)%>% 
  select(color, wave, svi_q, testing_rate, incidence_rate, positivity_ratio) %>%
  pivot_longer(cols=c(testing_rate, positivity_ratio, incidence_rate))%>%
  mutate(name=factor(name, levels=c("testing_rate",
                                    "incidence_rate", 
                                    "positivity_ratio"),
                     labels = c("Testing", "Incidence", "Positivity"))) %>%
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Vaccine Rollout", 
                               "Fourth"))) %>% 
  mutate(color=factor(color, levels=c("blue", "purple", "red"),
                      labels=c("Democrat", "Mixed", "Republican"))) %>% 
  filter(name!="Testing")

ggplot(final_tract, aes(x=pct_hsplus, y=cases)) + geom_point()
ggplot(final_tract, aes(x=pct_service, y=cases)) + geom_point()
ggplot(final_tract, aes(x=pct_crowded, y=cases)) + geom_point()
ggplot(final_tract, aes(x=mhi, y=cases)) + geom_point()
ggplot(final_tract, aes(x=svi, y=cases)) +geom_point()

  # Figure 3

#recode exposure measures so that they're rescaled 0-1 and mhi, pct_hs, pct service, and pct crowded are coded as quintiles, 
#and all coded as higher = more vulnerable so that RII >1 is a disparity 
dispar_exp<-final_tract %>%ungroup() %>% 
  filter(!duplicated(tract_fips)) %>% 
  mutate(
    #svi doesn't need to be cut into quartiles bc linear relationship between SVI and cases
    svi=as.numeric((RPL_THEMES-min(RPL_THEMES, na.rm=T))/(max(RPL_THEMES, na.rm=T)-min(RPL_THEMES, na.rm=T))), 
    svi_theme1=as.numeric((RPL_THEME1-min(RPL_THEME1, na.rm=T))/(max(RPL_THEME1, na.rm=T)-min(RPL_THEME1, na.rm=T))), 
    #create quartiles for mhi
    mhi=as.numeric(cut(mhi, 
                            breaks = quantile(mhi, probs=seq(0, 1, by=0.2), na.rm=T), 
                            include.lowest = T)), 
    #convert to 0-1 
    mhi=as.numeric((mhi-min(mhi, na.rm=T))/(max(mhi, na.rm=T)-min(mhi, na.rm=T))),  
    #invert so that lower MHI is higher ranked
    mhi=1-mhi, 
    #create quartiles
    pct_crowded=as.numeric(cut(pct_crowded, 
                             breaks = quantile(pct_crowded, probs=seq(0, 1, by=0.2), na.rm=T), 
                             include.lowest = T)),
    pct_crowded=as.numeric((pct_crowded-min(pct_crowded, na.rm=T))/(max(pct_crowded, na.rm=T)-min(pct_crowded, na.rm=T))), 
    #pct service 
    pct_service=as.numeric(cut(pct_service, 
                               breaks = quantile(pct_service, probs=seq(0, 1, by=0.2), na.rm=T), 
                               include.lowest = T)),
    pct_service=as.numeric((pct_service-min(pct_service, na.rm=T))/(max(pct_service, na.rm=T)-min(pct_service, na.rm=T))),
    #create PCT less than HS
    pct_lesshs=100-pct_hsplus, 
    pct_lesshs=as.numeric(cut(pct_lesshs, 
                               breaks = quantile(pct_lesshs, probs=seq(0, 1, by=0.2), na.rm=T), 
                               include.lowest = T)),
    pct_lesshs=as.numeric((pct_lesshs-min(pct_lesshs, na.rm=T))/(max(pct_lesshs, na.rm=T)-min(pct_lesshs, na.rm=T))))%>%
  select(tract_fips, svi, svi_theme1, mhi, pct_crowded, pct_service, pct_lesshs) 

res_nola_geo<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                         month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                         month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  group_by(tract_fips,nola_geo, wave) %>% 
  summarise(months=case_when(wave=="First"~4, 
                      wave=="Second"~4,
                      wave=="Third" ~5, 
                      wave=="Vaccine Rollout"~3, 
                      wave=="Fourth"~4),
            cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(nola_geo,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(nola_geo=="Other Urban", wave=="Fourth")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~svi+offset(log(pop)), data=.x)
    mhi_incid<-glm.nb(cases~mhi+offset(log(pop)), data=.x)
    crowd_incid<-glm.nb(cases~pct_crowded+offset(log(pop)), data=.x)
    service_incid<-glm.nb(cases~pct_service+offset(log(pop)), data=.x)
    hs_incid<-glm.nb(cases~pct_lesshs+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~svi+offset(log(tests)), data=.x %>% filter(tests>0))
    mhi_posit<-glm.nb(positives~mhi+offset(log(tests)), data=.x %>% filter(tests>0))
    crowd_posit<-glm.nb(positives~pct_crowded+offset(log(tests)), data=.x %>% filter(tests>0))
    service_posit<-glm.nb(positives~pct_service+offset(log(tests)), data=.x %>% filter(tests>0))
    hs_posit<-glm.nb(positives~pct_lesshs+offset(log(tests)), data=.x %>% filter(tests>0))
    #coefficient and se for svi 
    logRII_incid<-summary(m_incid)$coefficients["svi",1]
    selogrii_incid<-summary(m_incid)$coefficients["svi",2]
    serii_incid<-deltamethod(~exp(x1),logRII_incid,selogrii_incid^2)
    logRII_posit<-summary(m_posit)$coefficients["svi",1]
    selogrii_posit<-summary(m_posit)$coefficients["svi",2]
    serii_posit<-deltamethod(~exp(x1),logRII_posit,selogrii_posit^2)
    #coefficient and se for mhi 
    mhi_logRII_incid<-summary(mhi_incid)$coefficients["mhi",1]
    mhi_selogrii_incid<-summary(mhi_incid)$coefficients["mhi",2]
    mhi_serii_incid<-deltamethod(~exp(x1),mhi_logRII_incid,mhi_selogrii_incid^2)
    mhi_logRII_posit<-summary(mhi_posit)$coefficients["mhi",1]
    mhi_selogrii_posit<-summary(mhi_posit)$coefficients["mhi",2]
    mhi_serii_posit<-deltamethod(~exp(x1),mhi_logRII_posit,mhi_selogrii_posit^2)
    
    
    # compile
    bind_rows(data.frame(est=logRII_incid,
                         se=selogrii_incid) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="incid"),
              data.frame(est=logRII_posit,
                         se=selogrii_posit) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="posit"))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Vaccine Rollout", 
                               "Fourth"))) 
res_color<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                         month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                         month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  left_join(elect %>% select(county_fips, color)) %>% 
  group_by(tract_fips,color, wave) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(svi) %>% 
  group_by(color,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(color=="Other Urban", month=="First")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~svi+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~svi+offset(log(tests)), data=.x %>% filter(tests>0))
    bind_rows(tidy(m_incid) %>% filter(term=="svi") %>% mutate(outcome="incid"),
              tidy(m_posit) %>% filter(term=="svi") %>% mutate(outcome="posit")) %>% 
      mutate(est=exp(estimate),
             lci=exp(estimate-1.96*std.error),
             uci=exp(estimate+1.96*std.error))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Vaccine Rollout", 
                               "Fourth")),
         color=factor(color, levels=c("blue", "purple", "red"),
                      labels=c("Democrat", "Mixed", "Republican")))

ylim<-c(
  min(c(min(res_nola_geo$lci), min(res_color$lci))),
  max(c(max(res_nola_geo$uci), max(res_color$uci)))
)

f3a<-ggplot(res_nola_geo, aes(x=wave, y=est, group=nola_geo)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=nola_geo, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=nola_geo, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=nola_geo), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for the\nSocial Vulnerability Index (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))

f3a
f3b<-ggplot(res_color, aes(x=wave, y=est, group=color)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=color, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=color, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=color), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=color), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_manual(values=c("blue", "purple", "red"), name="")+
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for the\nSocial Vulnerability Index (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
f3b
figure3<-arrangeGrob(grobs=list(f3a, f3b), ncol=1)
ggsave("Results/Figure3_new.pdf", figure3, width=15, height=12.5)
figure3


#Appendix Figure 
#repeat figure3 with MHI 

#Run RII for MHI by geography
res_nola_geo_mhi<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                        month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                        (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                        month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                        month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  group_by(tract_fips,nola_geo, wave) %>% 
  summarise(months=case_when(wave=="First"~4, 
                             wave=="Second"~4,
                             wave=="Third" ~5, 
                             wave=="Vaccine Rollout"~3, 
                             wave=="Fourth"~4),
            cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(nola_geo,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(nola_geo=="Other Urban", wave=="Fourth")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~mhi+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~mhi+offset(log(tests)), data=.x %>% filter(tests>0))
    #coefficient and se for mhi 
    logRII_incid<-summary(m_incid)$coefficients["mhi",1]
    selogrii_incid<-summary(m_incid)$coefficients["mhi",2]
    serii_incid<-deltamethod(~exp(x1),logRII_incid,selogrii_incid^2)
    logRII_posit<-summary(m_posit)$coefficients["mhi",1]
    selogrii_posit<-summary(m_posit)$coefficients["mhi",2]
    serii_posit<-deltamethod(~exp(x1),logRII_posit,selogrii_posit^2)
    # compile
    bind_rows(data.frame(est=logRII_incid,
                         se=selogrii_incid) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="incid"),
              data.frame(est=logRII_posit,
                         se=selogrii_posit) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="posit"))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                     labels=c("First",
                              "Second",
                              "Third", 
                              "Vaccine Rollout", 
                              "Fourth"))) 


#run RII for MHI with political party (color)
res_color_mhi<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                        month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                        (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                        month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                        month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  left_join(elect %>% select(county_fips, color)) %>% 
  group_by(tract_fips,color, wave) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(color,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(color=="Other Urban", month=="First")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~mhi+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~mhi+offset(log(tests)), data=.x %>% filter(tests>0))
    bind_rows(tidy(m_incid) %>% filter(term=="mhi") %>% mutate(outcome="incid"),
              tidy(m_posit) %>% filter(term=="mhi") %>% mutate(outcome="posit")) %>% 
      mutate(est=exp(estimate),
             lci=exp(estimate-1.96*std.error),
             uci=exp(estimate+1.96*std.error))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                     labels=c("First",
                              "Second",
                              "Third", 
                              "Vaccine Rollout", 
                              "Fourth")),
         color=factor(color, levels=c("blue", "purple", "red"),
                      labels=c("Democrat", "Mixed", "Republican")))



#set boundaries
ylim<-c(
  min(c(min(res_nola_geo$lci), min(res_color$lci))),
  max(c(max(res_nola_geo$uci), max(res_color$uci)))
)

#figure3 with MHI
f3a_mhi<-ggplot(res_nola_geo_mhi, aes(x=wave, y=est, group=nola_geo)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=nola_geo, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=nola_geo, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=nola_geo), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for \n Median Household Income (inverted) (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))

f3a_mhi

f3b_mhi<-ggplot(res_color_mhi, aes(x=wave, y=est, group=color)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=color, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=color, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=color), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=color), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_manual(values=c("blue", "purple", "red"), name="")+
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for \n Median Household Income (Inverted) (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
f3b_mhi
figure3_mhi<-arrangeGrob(grobs=list(f3a_mhi, f3b_mhi), ncol=1)
ggsave("Results/Figure3_new_mhi.pdf", figure3_mhi, width=15, height=12.5)
figure3_mhi

#######Repeat Figure 3 with pct_crowded

res_nola_geo_pct_crowded<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                        month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                        (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                        month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                        month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  group_by(tract_fips,nola_geo, wave) %>% 
  summarise(months=case_when(wave=="First"~4, 
                             wave=="Second"~4,
                             wave=="Third" ~5, 
                             wave=="Vaccine Rollout"~3, 
                             wave=="Fourth"~4),
            cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(nola_geo,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(nola_geo=="Other Urban", wave=="Fourth")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~pct_crowded+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~pct_crowded+offset(log(tests)), data=.x %>% filter(tests>0))
    #coefficient and se for pct_crowded 
    logRII_incid<-summary(m_incid)$coefficients["pct_crowded",1]
    selogrii_incid<-summary(m_incid)$coefficients["pct_crowded",2]
    serii_incid<-deltamethod(~exp(x1),logRII_incid,selogrii_incid^2)
    logRII_posit<-summary(m_posit)$coefficients["pct_crowded",1]
    selogrii_posit<-summary(m_posit)$coefficients["pct_crowded",2]
    serii_posit<-deltamethod(~exp(x1),logRII_posit,selogrii_posit^2)
    # compile
    bind_rows(data.frame(est=logRII_incid,
                         se=selogrii_incid) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="incid"),
              data.frame(est=logRII_posit,
                         se=selogrii_posit) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="posit"))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                     labels=c("First",
                              "Second",
                              "Third", 
                              "Vaccine Rollout", 
                              "Fourth"))) 


#run RII for pct_crowded with political party (color)
res_color_pct_crowded<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                        month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                        (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                        month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                        month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  left_join(elect %>% select(county_fips, color)) %>% 
  group_by(tract_fips,color, wave) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(color,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(color=="Other Urban", month=="First")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~pct_crowded+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~pct_crowded+offset(log(tests)), data=.x %>% filter(tests>0))
    bind_rows(tidy(m_incid) %>% filter(term=="pct_crowded") %>% mutate(outcome="incid"),
              tidy(m_posit) %>% filter(term=="pct_crowded") %>% mutate(outcome="posit")) %>% 
      mutate(est=exp(estimate),
             lci=exp(estimate-1.96*std.error),
             uci=exp(estimate+1.96*std.error))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                     labels=c("First",
                              "Second",
                              "Third", 
                              "Vaccine Rollout", 
                              "Fourth")),
         color=factor(color, levels=c("blue", "purple", "red"),
                      labels=c("Democrat", "Mixed", "Republican")))



#set boundaries
ylim<-c(
  min(c(min(res_nola_geo$lci), min(res_color$lci))),
  max(c(max(res_nola_geo$uci), max(res_color$uci)))
)

#figure3 with pct_crowded
f3a_pct_crowded<-ggplot(res_nola_geo_pct_crowded, aes(x=wave, y=est, group=nola_geo)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=nola_geo, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=nola_geo, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=nola_geo), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for\n Percent Crowded (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))

f3a_pct_crowded

f3b_pct_crowded<-ggplot(res_color_pct_crowded, aes(x=wave, y=est, group=color)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=color, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=color, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=color), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=color), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_manual(values=c("blue", "purple", "red"), name="")+
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for\n Percent Crowded  (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
f3b_pct_crowded
figure3_pct_crowded<-arrangeGrob(grobs=list(f3a_pct_crowded, f3b_pct_crowded), ncol=1)
ggsave("Results/Figure3_new_pct_crowded.pdf", figure3_pct_crowded, width=15, height=12.5)
figure3_pct_crowded

#Repeat FIgure 3 with Pct service

res_nola_geo_pct_service<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                        month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                        (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                        month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                        month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  group_by(tract_fips,nola_geo, wave) %>% 
  summarise(months=case_when(wave=="First"~4, 
                             wave=="Second"~4,
                             wave=="Third" ~5, 
                             wave=="Vaccine Rollout"~3, 
                             wave=="Fourth"~4),
            cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(nola_geo,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(nola_geo=="Other Urban", wave=="Fourth")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~pct_service+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~pct_service+offset(log(tests)), data=.x %>% filter(tests>0))
    #coefficient and se for pct_service 
    logRII_incid<-summary(m_incid)$coefficients["pct_service",1]
    selogrii_incid<-summary(m_incid)$coefficients["pct_service",2]
    serii_incid<-deltamethod(~exp(x1),logRII_incid,selogrii_incid^2)
    logRII_posit<-summary(m_posit)$coefficients["pct_service",1]
    selogrii_posit<-summary(m_posit)$coefficients["pct_service",2]
    serii_posit<-deltamethod(~exp(x1),logRII_posit,selogrii_posit^2)
    # compile
    bind_rows(data.frame(est=logRII_incid,
                         se=selogrii_incid) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="incid"),
              data.frame(est=logRII_posit,
                         se=selogrii_posit) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="posit"))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                     labels=c("First",
                              "Second",
                              "Third", 
                              "Vaccine Rollout", 
                              "Fourth"))) 


#run RII for pct_service with political party (color)
res_color_pct_service<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                        month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                        (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                        month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                        month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  left_join(elect %>% select(county_fips, color)) %>% 
  group_by(tract_fips,color, wave) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(color,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(color=="Other Urban", month=="First")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~pct_service+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~pct_service+offset(log(tests)), data=.x %>% filter(tests>0))
    bind_rows(tidy(m_incid) %>% filter(term=="pct_service") %>% mutate(outcome="incid"),
              tidy(m_posit) %>% filter(term=="pct_service") %>% mutate(outcome="posit")) %>% 
      mutate(est=exp(estimate),
             lci=exp(estimate-1.96*std.error),
             uci=exp(estimate+1.96*std.error))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                     labels=c("First",
                              "Second",
                              "Third", 
                              "Vaccine Rollout", 
                              "Fourth")),
         color=factor(color, levels=c("blue", "purple", "red"),
                      labels=c("Democrat", "Mixed", "Republican")))



#set boundaries
ylim<-c(
  min(c(min(res_nola_geo$lci), min(res_color$lci))),
  max(c(max(res_nola_geo$uci), max(res_color$uci)))
)

#figure3 with pct_service
f3a_pct_service<-ggplot(res_nola_geo_pct_service, aes(x=wave, y=est, group=nola_geo)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=nola_geo, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=nola_geo, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=nola_geo), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for\n Percent Service (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))

f3a_pct_service

f3b_pct_service<-ggplot(res_color_pct_service, aes(x=wave, y=est, group=color)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=color, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=color, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=color), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=color), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_manual(values=c("blue", "purple", "red"), name="")+
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for\n Percent Service  (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
f3b_pct_service
figure3_pct_service<-arrangeGrob(grobs=list(f3a_pct_service, f3b_pct_service), ncol=1)
ggsave("Results/Figure3_new_pct_service.pdf", figure3_pct_service, width=15, height=12.5)
figure3_pct_service

####Repeat Figure 3 with Pct Less than HIgh school

res_nola_geo_pct_lesshs<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                        month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                        (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                        month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                        month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  group_by(tract_fips,nola_geo, wave) %>% 
  summarise(months=case_when(wave=="First"~4, 
                             wave=="Second"~4,
                             wave=="Third" ~5, 
                             wave=="Vaccine Rollout"~3, 
                             wave=="Fourth"~4),
            cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(nola_geo,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(nola_geo=="Other Urban", wave=="Fourth")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~pct_lesshs+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~pct_lesshs+offset(log(tests)), data=.x %>% filter(tests>0))
    #coefficient and se for pct_lesshs 
    logRII_incid<-summary(m_incid)$coefficients["pct_lesshs",1]
    selogrii_incid<-summary(m_incid)$coefficients["pct_lesshs",2]
    serii_incid<-deltamethod(~exp(x1),logRII_incid,selogrii_incid^2)
    logRII_posit<-summary(m_posit)$coefficients["pct_lesshs",1]
    selogrii_posit<-summary(m_posit)$coefficients["pct_lesshs",2]
    serii_posit<-deltamethod(~exp(x1),logRII_posit,selogrii_posit^2)
    # compile
    bind_rows(data.frame(est=logRII_incid,
                         se=selogrii_incid) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="incid"),
              data.frame(est=logRII_posit,
                         se=selogrii_posit) %>% 
                mutate(lci=exp(est-1.96*se),
                       uci=exp(est+1.96*se),
                       est=exp(est)) %>% 
                select(est, lci, uci) %>% 
                mutate(outcome="posit"))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                     labels=c("First",
                              "Second",
                              "Third", 
                              "Vaccine Rollout", 
                              "Fourth"))) 


#run RII for pct_lesshs with political party (color)
res_color_pct_lesshs<-final_tract %>% 
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                        month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                        (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                        month %in% c(4, 5, 6) & year==2021 ~"Vaccine Rollout",
                        month%in% c(7,8, 9, 10) & year==2021~"Fourth"))%>%
  left_join(elect %>% select(county_fips, color)) %>% 
  group_by(tract_fips,color, wave) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(tract_pop_2018)) %>% 
  left_join(dispar_exp) %>% 
  group_by(color,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(color=="Other Urban", month=="First")
    #print(.y$color)
    # incid
    m_incid<-glm.nb(cases~pct_lesshs+offset(log(pop)), data=.x)
    # posit
    m_posit<-glm.nb(positives~pct_lesshs+offset(log(tests)), data=.x %>% filter(tests>0))
    bind_rows(tidy(m_incid) %>% filter(term=="pct_lesshs") %>% mutate(outcome="incid"),
              tidy(m_posit) %>% filter(term=="pct_lesshs") %>% mutate(outcome="posit")) %>% 
      mutate(est=exp(estimate),
             lci=exp(estimate-1.96*std.error),
             uci=exp(estimate+1.96*std.error))
  }) %>% mutate(outcome=factor(outcome, levels=c( "posit",
                                                  "incid"),
                               labels = c("Positivity", "Incidence"))) %>% 
  mutate(wave=factor(wave, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                     labels=c("First",
                              "Second",
                              "Third", 
                              "Vaccine Rollout", 
                              "Fourth")),
         color=factor(color, levels=c("blue", "purple", "red"),
                      labels=c("Democrat", "Mixed", "Republican")))



#set boundaries
ylim<-c(
  min(c(min(res_nola_geo$lci), min(res_color$lci))),
  max(c(max(res_nola_geo$uci), max(res_color$uci)))
)

#figure3 with pct_lesshs
f3a_pct_lesshs<-ggplot(res_nola_geo_pct_lesshs, aes(x=wave, y=est, group=nola_geo)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=nola_geo, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=nola_geo, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=nola_geo), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for\n Percent lesshs (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))

f3a_pct_lesshs

f3b_pct_lesshs<-ggplot(res_color_pct_lesshs, aes(x=wave, y=est, group=color)) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_ribbon(aes(fill=color, ymin=lci, ymax=uci), alpha=0.3)+
  geom_linerange(aes(color=color, ymin=lci, ymax=uci), position=position_dodge(width=0.2))+
  geom_line(aes(color=color), position=position_dodge(width=0.2)) +
  geom_point(aes(fill=color), color="black", pch=21,size=4, position=position_dodge(width=0.2))+
  scale_color_manual(values=c("blue", "purple", "red"), name="")+
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  scale_y_continuous(trans="log", breaks=2^(-1:4), limits=ylim) +
  labs(x="Wave", y="Relative Index of Inequality for\n Percent lesshs  (95% CI)") +
  guides(fill="none")+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
f3b_pct_lesshs
figure3_pct_lesshs<-arrangeGrob(grobs=list(f3a_pct_lesshs, f3b_pct_lesshs), ncol=1)
ggsave("Results/Figure3_new_pct_lesshs.pdf", figure3_pct_lesshs, width=15, height=12.5)
figure3_pct_lesshs
