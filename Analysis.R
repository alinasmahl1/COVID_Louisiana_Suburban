rm(list=ls())
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(MASS)
library(broom)
library(plotly)
select<-dplyr::select
load("final_data.rdata")
head(final_nola_geo)
head(final_deaths)
head(final_tract)

# figure 1:
# make long
dta_f1<-final_nola_geo %>% 
  select(nola_geo, month, incidence_rate, testing_rate, positivity_ratio) %>% 
  mutate(positivity_ratio=positivity_ratio*100,
         month=mdy(paste0(month, "-01-2020"))) %>% 
  pivot_longer(cols = c(incidence_rate, testing_rate, positivity_ratio)) %>% 
  mutate(name=factor(name, levels=c("testing_rate",
                                    "positivity_ratio",
                                    "incidence_rate"),
                     labels = c("Testing", "Positivity", "Incidence"))) %>% 
  filter(name!="Testing")
figure1<-ggplot(dta_f1, aes(x=month, y=value)) +
  geom_line(aes(color=nola_geo)) +
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_x_date(breaks="1 month", date_labels ="%b")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="", y="Testing/incidence rate per 10,000\nor positivity ratio (%)")+
  facet_wrap(~name, scales = "free_y") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure1
ggsave(figure1, file="Results/Figure1.pdf", width=10, height=5)
#figure 1 repeated for mortality 

dta_f1<-final_deaths_geo %>% 
  select(nola_geo, month, death_rate) %>%
  mutate(month=factor(month), 
         month=mdy(paste0(month, "-01-2020")))
figure3<-ggplot(dta_f1, aes(x=month, y=death_rate)) +
  geom_line(aes(color=nola_geo)) +
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_x_date(breaks="1 month", date_labels ="%b")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="", y="death rate per 100,000")+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure3
ggsave(figure3, file="Results/Figure3.pdf", width=10, height=5)
ggplotly(figure3)

# final Figure 1
dta_f1b<-final_nola_geo %>% 
  mutate(month=case_when(month%in%(3:4)~"First", 
                         month%in%(5:6)~"Valley",
                         month%in%(7:9)~"Second")) %>% 
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
  mutate(month=case_when(month%in%(3:4)~"First", 
                          month%in%(5:6)~"Valley",
                          month%in%(7:9)~"Second")) %>%
  group_by(nola_geo, month) %>% 
  summarise(death=sum(death_geo),
            pop=sum(geo_pop)) %>% 
  mutate( value=death/pop*100000,
          name="mortality") %>% 
  select(nola_geo, month, name, value)
dta_figure1_final<-bind_rows(dta_f1b, dta_f1c) %>% 
  filter(name!="testing_rate") %>% 
  mutate(name=factor(name, levels=c("incidence_rate", 
                                    "positivity_ratio",
                                    "mortality"),
                     labels = c("Incidence", "Positivity", "Mortality"))) %>% 
  mutate(month=factor(month, levels=c("First", "Valley", "Second"),
                      labels=c("First peak\n(March-April)",
                               "Re-opening\n(May-June)",
                               "Second peak\n(July-September)"))) 
figure1b<-ggplot(dta_figure1_final, aes(x=month, y=value)) +
  geom_line(aes(color=nola_geo, group=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=2)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="", y="Testing/incidence rate per 10,000\nor positivity ratio (%)",
       title="Figure 1: COVID-19 outcomes by geography in Lousiana")+
  labs(x="", y="Incidence rate per 10,000 or positivity ratio (%)")+
  facet_wrap(~name, scales = "free_y") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure1b
ggsave(figure1b, file="Results/Figure1b.pdf", width=15, height=6)



# figure 2: EDA
#scatter plots 
#scatter plots SVI 
final_tract %>%
  ggplot(aes(x=RPL_THEMES, y=testing_rate, color=nola_geo)) + 
  geom_point()+
  geom_smooth(method="loess", se=FALSE, fullrange=TRUE) +
  scale_y_log10()+annotation_logticks(sides="l")+
  facet_wrap(.~month, scales="free_y")
final_tract %>%
  ggplot(aes(x=RPL_THEMES, y=positivity_ratio, color=nola_geo)) + 
  geom_point()+
  geom_smooth(method="loess", se=FALSE, fullrange=TRUE) +
  facet_wrap(.~month, scales="free_y")
final_tract %>%
  ggplot(aes(x=RPL_THEMES, y=incidence_rate, color=nola_geo)) + 
  geom_point()+
  geom_smooth(method="loess", se=FALSE, fullrange=TRUE) +
  scale_y_log10()+annotation_logticks(sides="l")+
  facet_wrap(.~month, scales="free_y")
final_deaths_county %>%
  ggplot(aes(x=RPL_THEMES, y=death_rate, color=nola_geo)) + 
  geom_point()+
  geom_smooth(method="loess", se=FALSE, fullrange=TRUE) +
  scale_y_log10()+annotation_logticks(sides="l")+
  facet_wrap(.~month, scales="free_y")

# urbanicity-specific quintiles
quintiles<-final_tract %>% 
  # getting a unique value for each census tract
  filter(month==5) %>% 
  group_by(nola_geo) %>% 
  mutate(svi_q=cut(RPL_THEMES, 
                   breaks = quantile(RPL_THEMES, probs=seq(0, 1, by=0.2), na.rm=T), 
                   include.lowest = T) %>% as.numeric) %>% 
  select(nola_geo, tract_fips, svi_q)
quintiles<-full_join(final_tract, quintiles) %>% 
  # test
  mutate(month=case_when(month%in%(3:4)~"First", 
                         month%in%(5:6)~"Valley",
                         month%in%(7:9)~"Second")) %>% 
  group_by(nola_geo, month, svi_q) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>% 
  mutate(testing_rate=tests/pop*10000,
         incidence_rate=cases/pop*10000,
         positivity_ratio=positives/tests*100) %>% 
  select(nola_geo, month, svi_q, testing_rate, incidence_rate, positivity_ratio) %>% 
  pivot_longer(cols=c(testing_rate, positivity_ratio, incidence_rate)) %>% 
  mutate(name=factor(name, levels=c("testing_rate",
                                    "incidence_rate", 
                                    "positivity_ratio"),
                     labels = c("Testing", "Incidence", "Positivity"))) %>% 
  mutate(month=factor(month, levels=c("First", "Valley", "Second"),
                      labels=c("First peak\n(March-April)",
                               "Re-opening\n(May-June)",
                               "Second peak\n(July-September)"))) %>% 
  filter(name!="Testing")

figure2<-ggplot(quintiles %>% filter(!is.na(month)), aes(x=svi_q, y=value)) +
  geom_line(aes(color=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21, size=2)+
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
figure2
ggsave(figure2, file="Results/Figure2.pdf", width=12, height=10)

ggplotly(figure2)



# NOT USED BELOW


# figure 3
dta_f3<-final_tract %>% group_by(month, nola_geo) %>% 
  group_modify(~{
    #.x<-final_tract %>% filter(month==3, nola_geo=="Suburban")
    # exclude 1 tract with 0 tests in a month
    .x<-.x %>% filter(tests>0)
    testing<-glm.nb(tests~RPL_THEMES+offset(log(estimate_tract_pop_2018)), data=.x) %>% 
      tidy %>% 
      filter(grepl("RPL_THEME", term)) %>% 
      mutate(outcome="testing") %>% 
      select(outcome, estimate, std.error)
    positivity<-glm.nb(positives~RPL_THEMES+offset(log(tests)), data=.x) %>% 
      tidy %>% 
      filter(grepl("RPL_THEME", term)) %>% 
      mutate(outcome="positivity") %>% 
      select(outcome, estimate, std.error)
    incidence<-glm.nb(cases~RPL_THEMES+offset(log(estimate_tract_pop_2018)), data=.x) %>% 
      tidy %>% 
      filter(grepl("RPL_THEME", term)) %>% 
      mutate(outcome="incidence") %>% 
      select(outcome, estimate, std.error)
    bind_rows(testing, positivity, incidence)
  }) %>% 
  mutate(est=exp(estimate),
         lci=exp(estimate-1.96*std.error),
         uci=exp(estimate+1.96*std.error),
         month=mdy(paste0(month, "-01-2020")),
         outcome=factor(outcome, levels=c("testing", "incidence", "positivity"),
                        labels=c("Testing", "Incidence", "Positivity"))) %>% 
  filter(outcome!="Testing")
max<-max(c(max(dta_f3$uci), 1/min(dta_f3$lci)))
figure3<-ggplot(dta_f3, aes(x=month, y=est)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(aes(ymin=lci, ymax=uci, fill=nola_geo),
              alpha=0.3)+
  geom_line(aes(color=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_x_date(breaks="1 month", date_labels ="%b")+
  scale_y_continuous(trans="log", breaks=c(0.33, 0.5,0.66, 1, 1.5, 2, 3 ),
                     limits=c(1/max, max))+
  labs(x="", y="RR (95% CI) per 1-SD increase in SVI")+
  guides(fill=F)+
  facet_wrap(~outcome) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure3
ggsave(figure3, file="Results/Figure3.pdf", width=12, height=5)

# figure 3 by wave
dta_f3b<-final_tract %>% 
  mutate(month=case_when(month%in%(3:4)~"First", 
                         month%in%(5:6)~"Valley",
                         month%in%(7:9)~"Second")) %>% 
  group_by(month, nola_geo) %>% 
  group_modify(~{
    #.x<-final_tract %>% filter(month==3, nola_geo=="Suburban")
    # exclude 1 tract with 0 tests in a month
    .x<-.x %>% filter(tests>0)
    testing<-glm.nb(tests~RPL_THEMES+offset(log(estimate_tract_pop_2018)), data=.x) %>% 
      tidy %>% 
      filter(grepl("RPL_THEME", term)) %>% 
      mutate(outcome="testing") %>% 
      select(outcome, estimate, std.error)
    positivity<-glm.nb(positives~RPL_THEMES+offset(log(tests)), data=.x) %>% 
      tidy %>% 
      filter(grepl("RPL_THEME", term)) %>% 
      mutate(outcome="positivity") %>% 
      select(outcome, estimate, std.error)
    incidence<-glm.nb(cases~RPL_THEMES+offset(log(estimate_tract_pop_2018)), data=.x) %>% 
      tidy %>% 
      filter(grepl("RPL_THEME", term)) %>% 
      mutate(outcome="incidence") %>% 
      select(outcome, estimate, std.error)
    bind_rows(testing, positivity, incidence)
  }) %>% 
  mutate(est=exp(estimate),
         lci=exp(estimate-1.96*std.error),
         uci=exp(estimate+1.96*std.error),
         outcome=factor(outcome, levels=c("testing", "incidence", "positivity"),
                        labels=c("Testing", "Incidence", "Positivity"))) %>% 
  filter(outcome!="Testing") %>% 
  mutate(month=factor(month, levels=c("First", "Valley", "Second"),
                      labels=c("First peak\n(March-April)",
                               "Re-opening\n(May-June)",
                               "Second peak\n(July-September)")))
max<-max(c(max(dta_f3b$uci), 1/min(dta_f3b$lci)))
figure3b<-ggplot(dta_f3b, aes(x=month, y=est, group=nola_geo)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_linerange(aes(ymin=lci, ymax=uci, fill=nola_geo, color=nola_geo))+
  geom_line(aes(color=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21, size=4)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(trans="log", breaks=c(0.33, 0.5,0.66, 1, 1.5, 2, 3 ),
                     limits=c(1/max, max))+
  labs(x="", y="RR (95% CI) per 1-SD increase in SVI")+
  guides(fill=F, color=F)+
  facet_grid(outcome~nola_geo) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure3b
ggsave(figure3b, file="Results/Figure3b.pdf", width=20, height=10)

#Figure not included in Article 
#group urban and nola, since only 1 county for nola
levels(final_deaths_county$nola_geo)
final_deaths_county$geo <- factor(final_deaths_county$nola_geo)
levels(final_deaths_county$geo) <- list(
  AllUrban= c("New Orleans", "Other urban"),
  Suburban=("Suburban"), 
  Rural=("Rural"))

#test
quintiles<-full_join(final_deaths_county, final_deaths_county %>% filter(month==5) %>%
                       group_by(geo) %>% 
                       mutate(svi_q=cut(RPL_THEMES, 
                                        breaks = quantile(RPL_THEMES, probs=seq(0, 1, by=0.2), na.rm=T), 
                                        include.lowest = T) %>% as.numeric) %>% 
                       select(geo, county_fips, svi_q))%>%
  # test
  mutate(month=factor(month), 
         month=case_when(month%in%(3:4)~"First", 
                         month%in%(5:6)~"Valley",
                         month%in%(7:9)~"Second")) %>% 
  group_by(geo, month, svi_q) %>%
  summarise(deaths=sum(deaths),
            pop=sum(county_pop_2018)) %>% 
  mutate(deaths_rate=deaths/pop*100000) %>% 
  select(geo, month, svi_q, deaths_rate) %>% 
  pivot_longer(cols=c(deaths_rate)) %>% 
  mutate(month=factor(month, levels=c("First", "Valley", "Second"),
                      labels=c("First peak\n(March-April)",
                               "Re-opening\n(May-June)",
                               "Second peak\n(July-September)"))) 
figure3mort<-ggplot(quintiles %>% filter(!is.na(month)), aes(x=svi_q, y=value)) +
  geom_line(aes(color=geo)) +
  geom_point(aes(fill=geo), color="black", pch=21, size=2)+
  facet_grid(name~month, scales="free_y")+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(limits=c(0, NA))+
  labs(x="Social Vulnerability Index Quintile (1=lowest, 5=highest vulnerability)",
       y="Death rate per 100,000 ")+
  theme_bw()+
  theme(
  #  axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        legend.position = "bottom", 
        legend.text=element_text(color="black", size=14))
figure3mort
ggsave(figure3mort, file="Results/Figure3mort.pdf", width=12, height=10)







