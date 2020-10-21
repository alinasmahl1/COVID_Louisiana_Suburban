rm(list=ls())
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(MASS)
library(broom)
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
# figure 1 v2
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
  mutate(name=factor(name, levels=c("testing_rate",
                                    "incidence_rate", 
                                    "positivity_ratio"),
                     labels = c("Testing", "Incidence", "Positivity"))) %>% 
  mutate(month=factor(month, levels=c("First", "Valley", "Second"),
                      labels=c("First peak\n(March-April)",
                               "Re-opening\n(May-June)",
                               "Second peak\n(July-September)"))) %>% 
  filter(name!="Testing")
figure1b<-ggplot(dta_f1b, aes(x=month, y=value)) +
  geom_line(aes(color=nola_geo, group=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=2)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
<<<<<<< HEAD
  labs(x="", y="Testing/incidence rate per 10,000\or positivity ratio (%)",
       title="Figure 1: COVID-19 outcomes by geography in Lousiana")+
=======
  labs(x="", y="Incidence rate per 10,000 or positivity ratio (%)")+
>>>>>>> 79d8e5e2093aee04968ee814e5cd43533813ae3a
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

#final figure 
dta_f1c<-final_nola_geo %>% 
  mutate(month=case_when(month%in%(3:4)~"First", 
                         montsh%in%(5:6)~"Valley",
                         month%in%(7:9)~"Second")) %>% 
  group_by(nola_geo, month) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>% 
  mutate( incidence_rate=cases/pop*10000,
         positivity_ratio=positives/tests*100) %>% 
  pivot_longer(cols = c(incidence_rate, positivity_ratio)) %>% 
  mutate(name=factor(name, levels=c("incidence_rate", 
                                    "positivity_ratio"),
                     labels = c( "Incidence", "Positivity"))) %>% 
  mutate(month=factor(month, levels=c("First", "Valley", "Second"),
                      labels=c("First wave\n(March-April)",
                               "Re-opening\n(May-June)",
                               "Second wave\n(July-September)")))
figure1c<-ggplot(dta_f1c, aes(x=month, y=value)) +
  geom_line(aes(color=nola_geo, group=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=2)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="", y="Incidence rate per 10,000 or positivity ratio (%)",
       title="Figure 1: COVID-19 outcomes by geography in Lousiana")+
  facet_wrap(~name, scales = "free_y") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure1c
ggsave(figure1c, file="/Results/Figure1c.pdf", width=15, height=6)

#
ggplotly(figure1c)

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

# urbanicity-specific quintiles
quintiles<-full_join(final_tract, final_tract %>% filter(month==5) %>% 
  group_by(nola_geo) %>% 
  mutate(svi_q=cut(RPL_THEMES, 
                      breaks = quantile(RPL_THEMES, probs=seq(0, 1, by=0.2), na.rm=T), 
                      include.lowest = T) %>% as.numeric) %>% 
  select(nola_geo, tract_fips, svi_q)) %>% 
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
<<<<<<< HEAD
ggplotly(figure2)

=======

# NOT USED BELOW
>>>>>>> 79d8e5e2093aee04968ee814e5cd43533813ae3a
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




#********************************  Data for sample characteristics summary  *********************************** 
#total positives, cases, tests
totals<-final_covid%>%
  ungroup()%>%
  summarise(total_pos=sum(positives), 
            total_case=sum(cases), 
            total_test=sum(tests))

#total deaths by geo 
total_deaths<-deaths1 %>% filter(parish!="Parish Under Investigation") %>% 
  left_join(rucc_LA1) %>% 
  group_by(nola_geo1) %>% 
  summarise(deaths_black=sum(deaths_black, na.rm=T),
            deaths_white=sum(deaths_white, na.rm=T),
            deaths_unknown=sum(deaths_unknown,na.rm=T),
            deaths_other=sum(deaths_other, na.rm=T), 
            total_deaths=deaths_black+deaths_white+deaths_other+deaths_unknown, na.rm=T)
#********************************  Table 1  *********************************** 
#unadusted RR and RD 
table1<-final_deaths %>% 
  mutate(rd=rate_black-rate_white,
         rr=rate_black/rate_white) %>% 
  select(nola_geo1, rate_black, rate_white, rd, rr)
fwrite(table1, file="results/table1.csv")

<<<<<<< HEAD
#see code Ind_age_adj_movingcovid.R for indirectly adjusted data
=======
# tract race/ethnicity?
# brief test
final_tract<-final_tract %>% 
  mutate(race_cat=case_when(
    #pct_nonwhite<0.4 ~ "white",
    #pct_nonwhite>=0.4 & pct_nonwhite<0.6 ~ "mixed",
    #pct_nonwhite>=0.6 ~ "non-white",
    pct_nonwhite>=0.5 ~ "non-white",
    pct_nonwhite<0.5 ~ "white",
    T ~ ""
  ))
gq<-fread("Data/R12640627_SL140.csv") %>% 
  mutate(tract_fips=as.numeric(Geo_FIPS), 
         gq=SE_A19001_002, 
         pct_gq=SE_A19001_002/SE_A19001_001) %>% 
  select(tract_fips, gq, pct_gq) %>% arrange(desc(pct_gq)) %>% as_tibble
college<-fread("Data/DECENNIALSF12010.P42_2020-10-07T113524/DECENNIALSF12010.P42_data_with_overlays_2020-10-07T113521.csv") %>% 
  as_tibble %>% 
  slice(-1) %>% 
  rename(college_n=P042008) %>% 
  mutate(tract_fips=as.numeric(sub("1400000US", "", GEO_ID)),
         college_n=as.numeric(college_n)) %>% 
  select(tract_fips, college_n) %>% arrange(desc(college_n))


test<-final_tract %>% 
  #filter(tract_fips!=22071012102) %>% 
  filter(!tract_fips%in%(gq %>% filter(pct_gq>0.1) %>% pull(tract_fips))) %>% 
  mutate(month=case_when(month%in%(3:4)~"First", 
                         month%in%(5:6)~"Valley",
                         month%in%(7:9)~"Second")) %>% 
  group_by(nola_geo, month, race_cat) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>% 
  mutate(testing_rate=tests/pop*10000,
         incidence_rate=cases/pop*10000,
         positivity_ratio=positives/tests*100) %>% 
  mutate(month=factor(month, levels=c("First", "Valley", "Second"),
                      labels=c("First wave\n(March-April)",
                               "Re-opening\n(May-June)",
                               "Second wave\n(July-September)")))
ggplot(test %>% rename(value=incidence_rate), aes(x=month, y=value)) +
  geom_line(aes(color=nola_geo, group=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=2)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="", y="Testing/incidence rate per 10,000\nor positivity ratio (%)",
       title="Figure 1: COVID-19 outcomes by urbanicity in Lousiana")+
  facet_wrap(~race_cat, scales = "free_y") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
ggplot(test %>% rename(value=incidence_rate) %>% filter(!is.na(nola_geo)), aes(x=month, y=value)) +
  geom_line(aes(color=race_cat, group=race_cat)) +
  geom_point(aes(fill=race_cat), color="black", pch=21,size=2)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="", y="Testing/incidence rate per 10,000\nor positivity ratio (%)",
       title="Figure 1: COVID-19 outcomes by urbanicity in Lousiana")+
  facet_wrap(~nola_geo) +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))

testir<-test %>% select(nola_geo, month, race_cat, incidence_rate) %>% 
  pivot_wider(id_cols = c(nola_geo, month), names_from = race_cat, 
              values_from = incidence_rate) %>% 
  mutate(rr=`non-white`/white,
         rd=`non-white`-white) %>% 
  arrange(nola_geo, month)
testposit<-test %>% select(nola_geo, month, race_cat, positivity_ratio) %>% 
  pivot_wider(id_cols = c(nola_geo, month), names_from = race_cat, 
              values_from = positivity_ratio) %>% 
  mutate(rr=`non-white`/white,
         rd=`non-white`-white) %>% 
  arrange(nola_geo, month)
ggplot(testposit, aes(x=month, y=rd)) +
  geom_point(aes(color=nola_geo)) +
  geom_line(aes(group=nola_geo, color=nola_geo))



#####################plots repeated for SVI #####################
### incidence (log of cases)
ggplot(data=corr_nola_svi, aes(x=month, y=svi_inc, group=nola_geo)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(colour=nola_geo)) +
  geom_point(aes(colour=nola_geo)) +
  ylim(-0.7, 0.7) +
  scale_color_brewer(type="qual", palette=2) +
  theme(legend.position="none") +
  labs(title="Incidence rate", x="Month", y = "Pearson Correlation Coefficient with SVI")
# Testing (log of test)
ggplot(data=corr_nola_svi, aes(x=month, y=svi_test, group=nola_geo)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(colour=nola_geo)) +
  geom_point(aes(colour=nola_geo)) +
  ylim(-0.7, 0.7) +
  scale_color_brewer(type="qual", palette=2) +
  theme(legend.position="none", axis.title.y=element_blank()) + 
  labs(title="Testing Rate", x="Month", y = "Pearson Correlation Coefficient with SVI")

#Positivity 
corr_nola_svi<-county_tract_covid3 %>%
  subset(tract_fips!=22071012102)%>%
  mutate(loginc=log(1+incidence_rate_month), 
         logtest=log(1+test_ratio_month))%>%
  group_by(month, nola_geo)%>%
  summarise(svi_inc = cor(RPL_THEMES, loginc,
                          method="pearson", use="complete.obs"), 
            svi_test=cor(RPL_THEMES,  logtest, 
                         method="pearson", use="complete.obs"),
            svi_pos=cor(RPL_THEMES, posit_ratio_month, 
                        method="pearson", use="complete.obs"))%>%    
  ungroup()

ggplot(data=corr_nola_svi, aes(x=month, y=svi_pos, group=nola_geo)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(colour=nola_geo)) +
  geom_point(aes(colour=nola_geo)) +
  ylim(-0.7, 0.7) +
  scale_color_brewer(type="qual", palette=2) +
  theme(axis.title.y=element_blank()) + 
  labs(title="Positivity Ratio", x="Month", y = "Pearson Correlation Coefficient with SVI", colour="Geography")

ggplot(data=corr_nola_svi, aes(x=month, y=svi_test, group=nola_geo)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(colour=nola_geo)) +
  geom_point(aes(colour=nola_geo)) +
  ylim(-0.7, 0.7) +
  scale_color_brewer(type="qual", palette=2) +
  theme(axis.title.y=element_blank()) + 
  labs(title="Positivity Ratio", x="Month", y = "Pearson Correlation Coefficient with SVI", colour="Geography")

ggplot(data=corr_nola_svi, aes(x=month, y=svi_inc, group=nola_geo)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(colour=nola_geo)) +
  geom_point(aes(colour=nola_geo)) +
  ylim(-0.7, 0.7) +
  scale_color_brewer(type="qual", palette=2) +
  theme(axis.title.y=element_blank()) + 
  labs(title="Positivity Ratio", x="Month", y = "Pearson Correlation Coefficient with SVI", colour="Geography")


>>>>>>> 79d8e5e2093aee04968ee814e5cd43533813ae3a

###################################APPENDIX ###################################
## repeated for figure 2 
deciles1<-full_join(final_tract, final_tract %>% filter(month==5) %>% filter(tract_fips!=22071012102) %>% 
                     group_by(nola_geo) %>% 
                     mutate(svi_d=cut(RPL_THEMES, 
                                      breaks = quantile(RPL_THEMES, probs=seq(0, 1, by=0.2), na.rm=T), 
                                      include.lowest = T) %>% as.numeric) %>% 
                     select(nola_geo, tract_fips, svi_d)) %>% 
  # test
  mutate(month=case_when(month%in%(3:4)~"First", 
                         month%in%(5:6)~"Valley",
                         month%in%(7:9)~"Second")) %>% 
  group_by(nola_geo, month, svi_d) %>% 
  summarise(cases=sum(cases),
            tests=sum(tests),
            positives=sum(positives),
            pop=sum(estimate_tract_pop_2018)) %>% 
  mutate(testing_rate=tests/pop*10000,
         incidence_rate=cases/pop*10000,
         positivity_ratio=positives/tests*100) %>% 
  select(nola_geo, month, svi_d, testing_rate, incidence_rate, positivity_ratio) %>% 
  pivot_longer(cols=c(testing_rate, positivity_ratio, incidence_rate)) %>% 
  mutate(name=factor(name, levels=c("testing_rate",
                                    "positivity_ratio",
                                    "incidence_rate"),
                     labels = c("Testing", "Positivity", "Incidence"))) %>% 
  mutate(month=factor(month, levels=c("First", "Valley", "Second"),
                      labels=c("First wave (March-April)",
                               "Re-opening (May-June)",
                               "Second wave (July-September)")))
#month=factor(month, levels=3:9,
#              labels=month.name[3:9]))

figure2<-ggplot(deciles1 %>% filter(!is.na(month)), aes(x=svi_d, y=value)) +
  geom_line(aes(color=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21)+
  facet_grid(name~month, scales="free_y")+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_x_continuous(breaks=1:10)+
  labs(x="Social Vulnerability Index Quintile (1=lowest, 5=highest vulnerability)",
       y="Testing/incidence rate per 10,000 or positivity ratio (%)")+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure2



