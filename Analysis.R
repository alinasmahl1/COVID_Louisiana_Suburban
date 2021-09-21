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

# figure 2:
dta_f1b<-final_nola_geo %>% 
  mutate(month=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2)) ~"Third", 
                         month %in% c(3, 4, 5, 6) & year==2021 ~"Vaccine Rollout",
                         month%in% c(7,8, 9) & year==2021~"Fourth"))%>%
           group_by(nola_geo, month) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>% 
  mutate(testing_rate=tests/pop*10000,
         incidence_rate=cases/pop*10000,
         positivity_ratio=positives/tests*100) %>% 
  pivot_longer(cols = c(incidence_rate, testing_rate, positivity_ratio)) %>% 
  select(nola_geo, month, name, value)
dta_f1c<-final_deaths_geo %>% 
  mutate(month=case_when(month %in% c(3, 4, 5, 6)  ~"First", 
                         month %in% c(7, 8, 9 , 10) ~"Second", 
                         month %in% c(11, 12, 13, 14) ~"Third", 
                         month %in% c(15, 16, 17, 18) ~"Vaccine Rollout", 
                         month %in% c(19, 20, 21) ~"Fourth")) %>%
  group_by(nola_geo, month) %>% 
  summarise(death=sum(death_geo),
            pop=sum(geo_pop)) %>% 
  mutate( value=death/pop*10000,
          name="mortality") %>% 
  select(nola_geo, month, name, value)
dta_figure2_final<-bind_rows(dta_f1b, dta_f1c) %>% 
  filter(name!="testing_rate") %>% 
  mutate(name=factor(name, levels=c( "positivity_ratio",
                                    "incidence_rate", 
                                    "mortality"),
                     labels = c("Positivity", "Incidence", "Mortality"))) %>% 
  mutate(month=factor(month, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Vaccine Rollout", 
                               "Fourth"))) 
figure2b<-ggplot(dta_figure2_final, aes(x=month, y=value)) +
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
  mutate(month=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2)) ~"Third", 
                         month %in% c(3, 4, 5, 6) & year==2021 ~"Vaccine Rollout", 
                         month %in% c(7, 8) & year==2021~"Fourth"))%>%
  group_by(nola_geo, month, svi_q) %>%
    summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>%
  mutate(testing_rate=tests/pop*10000,
         incidence_rate=cases/pop*10000,
         positivity_ratio=positives/tests*100) %>%
  select(nola_geo, month, svi_q, testing_rate, incidence_rate, positivity_ratio) %>%
  pivot_longer(cols=c(testing_rate, positivity_ratio, incidence_rate))%>%
  mutate(name=factor(name, levels=c("testing_rate",
                                    "incidence_rate", 
                                    "positivity_ratio"),
                     labels = c("Testing", "Incidence", "Positivity"))) %>%
  mutate(month=factor(month, levels=c("First", "Second", "Third", "Vaccine Rollout", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Vaccine Rollout", 
                               "Fourth"))) %>% 
  filter(name!="Testing")

figure3<-ggplot(quintiles %>% filter(!is.na(month)), aes(x=svi_q, y=value)) +
  geom_line(aes(color=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21, size=4)+
  facet_grid(name~month, scales="free_y")+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(limits=c(0, NA))+
  labs(x="Social Vulnerability Index Quintile (1=lowest, 5=highest vulnerability)",
       y="Incidence rate per 10,000 or positivity ratio (%)")+
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


