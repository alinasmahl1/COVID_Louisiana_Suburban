rm(list=ls())
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(MASS)
library(broom)
library(plotly)
library(hrbrthemes)

select<-dplyr::select
load("data/final_data.rdata")
head(final_nola_geo)
head(final_deaths)
head(final_tract)

# figure 1
#find codes for colors in other graphs 
brewer.pal(n=4, name="Set2")
#calculate total cases by geo and create 2 peaks 1 before June 1, and 1 after 

summary<-final_nola_geo %>%
  mutate(
    peaks=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~1, 
                    month %in% c(7, 8, 9 , 10) & year==2020 ~2, 
                    (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2)) ~3, 
                    month %in% c(3, 4, 5, 6) & year==2021 ~4))%>%
  group_by(peaks, nola_geo)%>%
  summarize(case_peaks=sum(cases))

summary1<-final_nola_geo%>%
  mutate(peaks=case_when(year==2020 ~1, 
                         year==2021~2))%>%
  group_by(peaks,nola_geo)%>%
  summarize(case_peaks=sum(cases))

smart_round <- function(x, digits = 0) { # somewhere on SO
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}
waffleize <- function(xdf) {
  data_frame(
    peaks = rep(xdf$peaks, xdf$pct),
    nola_geo = rep(xdf$nola_geo, xdf$pct)
  )
}

#maybe make this just 2020 and 2021
final<-summary1 %>% 
  group_by(peaks) %>% 
  mutate(pct=case_peaks/sum(case_peaks)*100) %>% 
  #mutate(pct = (smart_round(pct, 1) * 100L) %>%  as.integer()) %>% 
  mutate(pct=round(pct, digits=0)) %>% 
  select(-case_peaks) %>% 
  ungroup() %>% 
  mutate(nola_geo = as.character(nola_geo))  %>% 
  mutate(peaks = case_when(peaks==1~"2020",
                           peaks==2~"2021"))%>%
   #from first set of code- delete if above works        ifelse(peaks==1, "First half", "Second half")) 
  select(peaks, nola_geo, pct) %>%
  rowwise() %>% 
  do(waffleize(.)) %>% 
  ungroup() %>% 
  slice(-201) %>% 
  mutate(nola_geo=factor(nola_geo, levels=c("New Orleans",
                                            "Other Urban",
                                            "Suburban",
                                            "Rural"))) %>% 
  arrange(peaks, nola_geo) %>%
  bind_cols(
    map_df(seq_len(length(unique(.$peaks))), ~expand.grid(y = 1:10, x = 1:10))
  )

plot<-ggplot(final, aes(x, y)) + 
  geom_tile(aes(fill=nola_geo), color=c("white"), size=0.5) +
  facet_wrap(~peaks) +
  scale_fill_manual(values=c("#66C2A5", "#FC8D62", "#8DA0CB" ,"#E78AC3")) +
  coord_equal() +
  labs(x=NULL, y = NULL) +
  theme_void()+
  theme(axis.text=element_blank(), 
        strip.text=element_text(face="bold", size=14),
        legend.title = element_blank()) 
plot
ggsave(plot, file="Results/Figure1.pdf", width=10, height=5)


# figure 2:
dta_f1b<-final_nola_geo %>% 
  mutate(month=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2)) ~"Third", 
                         month %in% c(3, 4, 5, 6) & year==2021 ~"Fourth"))%>%
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
                         (month %in% c(11, 12, 13, 14)) ~"Third", 
                         month %in% c(15, 16, 17) ~"Fourth")) %>%
  group_by(nola_geo, month) %>% 
  summarise(death=sum(death_geo),
            pop=sum(geo_pop)) %>% 
  mutate( value=death/pop*10000,
          name="mortality") %>% 
  select(nola_geo, month, name, value)
dta_figure1_final<-bind_rows(dta_f1b, dta_f1c) %>% 
  filter(name!="testing_rate") %>% 
  mutate(name=factor(name, levels=c( "positivity_ratio",
                                    "incidence_rate", 
                                    "mortality"),
                     labels = c("Positivity", "Incidence", "Mortality"))) %>% 
  mutate(month=factor(month, levels=c("First", "Second", "Third", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Fourth"))) 
figure1b<-ggplot(dta_figure1_final, aes(x=month, y=value)) +
  geom_line(aes(color=nola_geo, group=nola_geo)) +
  geom_point(aes(fill=nola_geo), color="black", pch=21,size=4)+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_y_continuous(limits=c(0, NA))+#, sec.axis = dup_axis())+
  labs(x="Peak", y="Rate per 10,000 or positivity ratio (%)")+
  facet_wrap(~name, scales = "free_y") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=10),
        axis.title=element_text(color="black", size=10, face="bold"),
        plot.title=element_text(color="black", size=20, face="bold"),
        strip.background = element_blank(),
        strip.text =element_text(color="black", size=16, face="bold"),
        legend.position = "bottom",
        legend.text=element_text(color="black", size=14))
figure1b
ggsave(figure1b, file="Results/Figure2.pdf", width=17, height=6)


# figure 3
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
  mutate(month=case_when(month %in% c(3, 4, 5, 6) & year==2020 ~"First", 
                         month %in% c(7, 8, 9 , 10) & year==2020 ~"Second", 
                         (month %in% c(11, 12)& year==2020)|(month %in% c(1, 2)) ~"Third", 
                         month %in% c(3, 4, 5, 6) & year==2021 ~"Fourth"))%>%
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
  mutate(month=factor(month, levels=c("First", "Second", "Third", "Fourth"),
                      labels=c("First",
                               "Second",
                               "Third", 
                               "Fourth"))) %>% 
  filter(name!="Testing")

figure2<-ggplot(quintiles %>% filter(!is.na(month)), aes(x=svi_q, y=value)) +
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
figure2
ggsave(figure2, file="Results/Figure3.pdf", width=20, height=10)

ggplotly(figure2)


