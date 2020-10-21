#Code to indirectly age standardize race & age specific covid death rates
library(data.table)
library(tidyverse)
library(stringr) 


setwd("/Users/2/OneDrive - Drexel University/Alina Research/Research/Mapping Covid in Louisianna/Data")
#---------------------------------------------------------------------------#
#import the datasets
#import CDC data for Observed rate 
age_race_spec=read.csv("LA_deaths_race_age.csv", header=TRUE, stringsAsFactors = FALSE)


#import the age* race specific estimates, for the county
pop_est<-read.csv("cc-est2019-alldata-22.csv", header=TRUE, stringsAsFactors = FALSE)
str(pop_est)

#import the deaths data 
load("final_deaths1.Rdata")

#add Parish so we can join on CTYNAME 
final_deaths1$CTYNAME <- paste(final_deaths1$parish, sep=" ", "Parish")

#subset county deaths data to just the RUCC and nola_geo data (for joining w/ the pop est)
nola_geo<-final_deaths1 %>%
        select(CTYNAME, nola_geo1)
#---------------------------------------------------------------------------#
#clean the race/age data (cdc state data)
#data has 11 deaths among Asian population aged 50-64 years, but no deaths in 45-54 or 55-64. Unclear why 
#those deaths are only deaths among population that we'll categorize as "another race", so b/c we use the 10 yr age grouping 
#to allow for conversion to the pop estimates, we have zero deaths in "another race" category
# 
age_race_spec1<-age_race_spec %>%
        select(Age.group, Race.and.Hispanic.Origin.Group, COVID.19.Deaths)%>%
        rename(race_ethnicity=Race.and.Hispanic.Origin.Group, 
               deaths=COVID.19.Deaths) %>%
#create age categories that can be calculated from both datasets 
        mutate(
                new_agecat=case_when(Age.group %in% c("Under 1 year","1-4 years","5-14 years") ~ 1,
                                     Age.group=="15-24 years" ~2, 
                                     Age.group=="25-34 years" ~3, 
                                     Age.group=="35-44 years" ~4, 
                                     Age.group=="45-54 years" ~5, 
                                     Age.group=="55-64 years" ~6,
                                     Age.group=="65-74 years" ~7,
                                     Age.group=="75-84 years" ~8,
                                     Age.group=="85 years and over" ~9), 
#combine hispanic and non-hispanic white because no ethnicity in county data 
                race=case_when(race_ethnicity %in% c("Non-Hispanic White", "Hispanic")~1, 
                               race_ethnicity=="Non-Hispanic Black"~2,
                               race_ethnicity %in% c("Unknown", "Non-Hispanic More than one race", 
                                                     "Non-Hispanic Native Hawaiian or Other Pacific Islander", 
                                                     "Non-Hispanic Asian", "Non-Hispanic American Indian or Alaska Native") ~3
                              ), 
                race=factor(race, labels=c("White", "Black", "Another_Race")),
                new_agecat=factor(new_agecat, labels=c("age_0_14", "age_15_24", 
                                                       "age_25_34", "age_35_44", 
                                                       "age_45-54","age_55_64",
                                                       "age_65_74", "age_75_84",
                                                       "age_85plus" ))) %>%
        filter(! Age.group %in% c("0-17 years","18-29 years", "30-49 years","50-64 years")) %>%
        replace_na(list(deaths = 0)) #replace NA with O

#add together the non hispanic + hispanic populations (because no Hispanic in county death data, 
# and because ~5% of the state is hispanic and _ % of that population identifies white as race
state_deaths<-age_race_spec1 %>%
        group_by(new_agecat, race) %>%
        summarize(deaths1=sum(deaths))%>%
ungroup() %>%
#shape to wide 
pivot_wider(id_cols=new_agecat, names_from = race, values_from = deaths1)%>%
        mutate(total_deaths=White+Black+Another_Race)

#---------------------------------------------------------------------------#
#clean the pop est data (for geogrpahy population estimates)
#---------------------------------------------------------------------------#
pop_est1<- pop_est%>%
        subset(YEAR==12)%>%
        mutate(
        new_agecat=case_when(AGEGRP %in% c(1: 3)~1, 
                             AGEGRP %in% c(4, 5)~2, 
                             AGEGRP %in% c(6, 7)~3,
                             AGEGRP %in% c(8, 9)~4, 
                             AGEGRP %in% c(10, 11)~5, 
                             AGEGRP %in% c(12, 13)~6, 
                             AGEGRP %in% c(14, 15)~7, 
                             AGEGRP %in% c(16, 17) ~8,
                             AGEGRP==18~9),
        new_agecat=factor(new_agecat, labels=c("age_0_14", "age_15_24", 
                                               "age_25_34", "age_35_44", 
                                               "age_45-54","age_55_64",
                                               "age_65_74", "age_75_84",
                                               "age_85plus" ))) %>%
        select(c(COUNTY, CTYNAME, AGEGRP, WA_MALE,
                 WA_FEMALE, BA_MALE, BA_FEMALE, new_agecat, TOT_POP))
                
#calculate the total pop estimates for state by age and race 
state_race_age<-pop_est1 %>%
        subset(AGEGRP!=0) %>%
        mutate( white =WA_MALE +WA_FEMALE, 
                black=BA_MALE +BA_FEMALE) %>%
        group_by(new_agecat) %>%
        summarize(pop_white=sum(white), 
                  pop_black=sum(black), 
                  tot_pop=sum(TOT_POP)) %>%
        ungroup()

#create the final county level pop estimates (by race)
       county_race_age<-pop_est1 %>%
                subset(AGEGRP!=0) %>%
       mutate( white =WA_MALE +WA_FEMALE, 
                        black=BA_MALE +BA_FEMALE)%>%
               group_by(new_agecat, CTYNAME)%>%
       summarize(pop_est_white=sum(white), 
                 pop_est_black=sum(black),
                 pop_est_total=sum(TOT_POP)) 
         
county_race_age<-county_race_age[c(2,1,3,4,5)]

#join in the nola_geo to get the geographic identifier, and then aggregate 
#  the county estimates to the 4 geographies 
#need to rename the LaSalle parish written as 2 words in the nola _geo. 

nola_geo1<-nola_geo %>%
        mutate(CTYNAME= case_when(
                str_detect(CTYNAME, "La Salle Parish") ~ "LaSalle Parish",
                TRUE ~ CTYNAME))

county_pop<- county_race_age %>%
        left_join(nola_geo1) %>%
group_by(nola_geo1, new_agecat)%>%
        summarize(geo_est_white=sum(pop_est_white), 
                  geo_est_black=sum(pop_est_black), 
                  geo_est_total=sum(pop_est_total))
#----------------------------------------------------------------------        
#Join the state estimates by race/age & popu estimates by race & age

state_adj<-state_race_age %>%
        left_join(state_deaths) %>%
        rename(white_death=White, 
               black_death=Black)%>%
        mutate(white_rate=white_death/pop_white, 
               black_rate=black_death/pop_black,
               total_rate=total_deaths/tot_pop)

####calculate total crude rates by race (for observed rate)#######
total_state_rates<-state_adj%>%
        summarize(tot_pop_white=sum(pop_white, na.rm=T),
                  tot_pop_black=sum(pop_black, na.rm=T),
                  tot_death_white=sum(white_death,na.rm=T),
                  tot_death_black=sum(black_death, na.rm=T), 
                  tot_pop=sum(tot_pop, na.rm=T), 
                  tot_death=sum(total_deaths, na.rm=T)) %>%
        #this calculates the crude death rates (total and by race)
        mutate(cdr_white=tot_death_white/tot_pop_white*1000, 
               cdr_black=tot_death_black/tot_pop_black*1000, 
               cdr_tot=tot_death/tot_pop*1000) 

###create final state_adj for joining with population estimates
final_state_adj<-state_adj %>%
        select(new_agecat, white_rate, black_rate, total_rate)

#---------------------------------------------------------------------------
#CALCULATION FOR EXPECTED 
#----------------------------------------------------------------------
#multiply the race specific rates * Rate specific death counts for "expected"

expected_calcs<-county_pop%>%
            left_join(final_state_adj)%>%
               mutate(expected_white_cat=geo_est_white*white_rate, 
                        expected_black_cat=geo_est_black*black_rate, 
                      expected_total_cat=geo_est_total*total_rate)
   final_expect<-expected_calcs %>%
               group_by(nola_geo1) %>%
               summarize(tot_expec_white=sum(expected_white_cat, na.rm = T),  
                         tot_expec_black=sum(expected_black_cat, na.rm=T), 
                         expec_total=sum(expected_total_cat, na.rm=T))


#--------------------------------------------------------------------------   
#CALCULATION FOR OBSERVED  (geography specific )
 #--------------------------------------------------------------------------   
#find the observed deaths by geography
  #take final_deaths 1 and limit to columns we need, then create var of total deaths
   final_deaths2<-final_deaths1%>%
           select(deaths_black, deaths_white, deaths_other, deaths_unknown, nola_geo1)%>% 
       mutate(total_deaths=deaths_black+deaths_white+deaths_unknown+ deaths_other)
               
   final_observed <-final_deaths2 %>%
  group_by(nola_geo1) %>%
 summarize(obser_white=sum(deaths_white,na.rm = T),
            obser_black=sum(deaths_black, na.rm=T), 
           obser_total=sum(total_deaths, na.rm=T))%>%
      ungroup()     
   
   view(final_observed)
 #calculate the SMR (uses final_expect and state_observed)
 smr<-final_observed%>%
         left_join(final_expect) 

 smr_calc<-smr%>%
         group_by(nola_geo1) %>%
 summarize(smr_white= obser_white/tot_expec_white*100,
                   smr_black=obser_black/tot_expec_black*100, 
           smr_total=obser_total/expec_total*100)  
#calculate IAR = smr*cdr (race specific) (using total_state_rates)
 
 iar_white<-smr_calc$smr_white*total_state_rates$cdr_white
 iar_black<-smr_calc$smr_black*total_state_rates$cdr_black
 iar_total<-smr_calc$smr_total*total_state_rates$cdr_tot
 view(iar_white)
 view(iar_black)
 view(iar_total)
 
 RD<-iar_black-iar_white
 RR<-iar_black/iar_white
 
view(RD)
view(RR)
