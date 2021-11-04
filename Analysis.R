rm(list=ls())
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(MASS)
library(broom)
library(plotly)
library(hrbrthemes)
library(gridExtra)
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
               date_labels ="%b", 
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
        axis.title = element_text(color="black", size=14))
figure1a
ggsave(figure1a, file="Results/Figure1a.pdf", width=15, height=6)


figure1b<-ggplot(weekly, aes(x=date_endweek1, y=cases, fill=nola_geo))+
  geom_area(position="fill")+
  scale_x_date(limits=c(as_date("2020-03-04"), max(weekly$date_endweek1)), 
               breaks="1 month", 
               date_labels ="%b", 
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
          axis.title = element_text(color="black", size=14))

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
               date_labels ="%b", 
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
        axis.title = element_text(color="black", size=14))
figure1a
ggsave(figure1a, file="Results/Figure1a_elect.pdf", width=15, height=6)


figure1b<-ggplot(weekly_elect, aes(x=date_endweek1, y=cases, fill=color))+
  geom_area(position="fill")+
  scale_x_date(limits=c(as_date("2020-03-04"), max(weekly$date_endweek1)), 
               breaks="1 month", 
               date_labels ="%b", 
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
        axis.title = element_text(color="black", size=14))

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
  labs(x="Wave", y="Monthly Rate per 10,000 or positivity ratio (%)")+
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
  labs(x="Wave", y="Monthly Rate per 10,000 or positivity ratio (%)")+
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

# figure 3
# urbanicity-specific quintiles
quintiles<-final_tract %>% 
  # getting a unique value for each census tract
  filter(month==12) %>% 
  group_by(nola_geo) %>% 
  mutate(svi_q=cut(RPL_THEMES, 
                   breaks = quantile(RPL_THEMES, probs=seq(0, 1, by=0.2), na.rm=T), 
                   include.lowest = T) %>% as.numeric) %>%
  select(nola_geo, tract_fips, svi_q, svi=RPL_THEMES)

quintiles<-full_join(final_tract, quintiles) %>% 
  # test
  mutate(wave=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2, 3)) ~"Third", 
                         month %in% c( 4, 5, 6) & year==2021 ~"Vaccine Rollout", 
                         month %in% c(7,8, 9 , 10) & year==2021~"Fourth"))%>%
  group_by(nola_geo, wave, svi_q) %>%
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
  select(nola_geo, wave, svi_q, testing_rate, incidence_rate, positivity_ratio) %>%
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
  filter(name!="Testing")

figure3<-ggplot(quintiles %>% filter(!is.na(wave)), aes(x=svi_q, y=value)) +
  geom_line(aes(color=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21, size=4)+
  facet_grid(name~wave, scales="free_y")+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(limits=c(0, NA))+
  labs(x="Social Vulnerability Index Quintile (1=lowest, 5=highest vulnerability)",
       y="Monthly Incidence rate per 10,000 or positivity ratio (%)")+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure3
ggsave(figure3, file="Results/Figure3.pdf", width=20, height=10)

ggplotly(figure3)



# figure 3  by elections
# election-specific quintiles?

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

figure4<-ggplot(quintiles %>% filter(!is.na(wave)), aes(x=svi_q, y=value)) +
  geom_line(aes(color=color)) +
  geom_point(aes(fill=color), color="black", pch=21, size=4)+
  facet_grid(name~wave, scales="free_y")+
  scale_color_manual(values=c("blue", "purple", "red"), name="")+
  scale_fill_manual(values=c("blue", "purple", "red"), name="")+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(limits=c(0, NA))+
  labs(x="Social Vulnerability Index Quintile (1=lowest, 5=highest vulnerability)",
       y="Monthly Incidence rate per 10,000 or positivity ratio (%)")+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure4
ggsave(figure4, file="Results/Figure4.pdf", width=20, height=10)


# NEw Figure 3



svi<-final_tract %>%ungroup() %>% 
  filter(!duplicated(tract_fips)) %>% 
  mutate(svi=as.numeric(RPL_THEMES-min(RPL_THEMES, na.rm=T)),
         svi=svi/max(svi, na.rm = T)) %>% 
  select(tract_fips, svi) 

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
  left_join(svi) %>% 
  group_by(nola_geo,wave) %>% 
  group_modify(~{
    #.x<-dta %>% filter(nola_geo=="Other Urban", month=="First")
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
            pop=sum(estimate_tract_pop_2018)) %>% 
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
figure3<-arrangeGrob(grobs=list(f3a, f3b), ncol=1)
ggsave("Results/Figure3_new.pdf", figure3, width=15, height=12.5)
figure3

