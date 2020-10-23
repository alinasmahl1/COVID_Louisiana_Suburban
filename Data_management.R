##***********************Import Code_Moving Covid**************************

#*Version 1 started Sept 8, 2020 by Alina Schnake-Mahl
#*Imports, cleans, & merges COVID data + RUCA/RUCC + ACS data on Urban/Rural/Suburban designations
#*Version 2 Oct 7th, 2020 by Usama Bilal
#*Minor edits to code

#Import all libraries 
library(data.table)
library(tidyverse)
library(tidycensus)
library(lubridate)
library(rgdal)
library(broom)
library(sf)
library(spdep)
library(stringr)
getwd()
setwd("/Users/2/OneDrive - Drexel University/Alina Research/Research/Mapping Covid in Louisianna/Data")
setwd("/Users/usamabilal/OneDrive - Drexel University/COVID Quick Analyses/COVID_Louisiana_Suburban/data")

library(forcats)
library(rmapshaper)
library(maps)
setwd("/Users/2/OneDrive - Drexel University/Alina Research/Research/Mapping Covid in Louisianna/Data")
setwd("/Users/usamabilal/OneDrive - Drexel University/COVID Quick Analyses/COVID_Louisiana_Suburban")
#---------------------------------------------------------------------------#
#import RUCA data 
# have already downloaded data and converted to CSV
ruca<-read.csv("ruca2010revised.csv",  header=TRUE, stringsAsFactors = FALSE)
str(ruca)
head(ruca)

#clean ruca data 
#rename vars so easier to use
ruca1<- ruca %>% rename(county_fips=State.County.FIPS.Code, state=Select.State, county=Select.County, 
                        tract_fips=State.County.Tract.FIPS.Code..lookup.by.address.at.http...www.ffiec.gov.Geocode.., 
                        primary_ruca=Primary.RUCA.Code.2010, secondary_ruca=Secondary.RUCA.Code..2010..see.errata., 
                        tract_pop=Tract.Population..2010, land_area=Land.Area..square.miles...2010, pop_density=Population.Density..per.square.mile...2010)


#---------------------------------------------------------------------------#
#import RUCC data 
# have already downloaded data and converted to CSV
rucc_county<-read.csv("ruralurbancodes2013.csv",  header=TRUE, stringsAsFactors = FALSE)
rucc_county
str(rucc_county)
head(rucc_county)

#clean/relabel rucc data 
rucc_county1<-rucc_county %>% rename(county_fips=FIPS, state=State, county=County_Name, pop_2010=Population_2010, code_rucc=RUCC_2013)

#---------------------------------------------------------------------------#
#import Louisiana covid data 
# have already downloaded data and converted to CSV

la_covid<-read.csv("LA_COVID_TESTBYWEEK_TRACT_PUBLICUSE (2).csv",  header=TRUE, stringsAsFactors = FALSE)
la_covid
str(la_covid)
head(la_covid)

#relabel for easier use 

la_covid1<- la_covid %>% 
  rename(parish=Parish, tract_fips=Tract, week=Week..Thur.Wed., date_startweek=Date.for.start.of.week,
         date_endweek=Date.for.end.of.week, week_test_count=Weekly.Test.Count, week_neg_test_count=Weekly.Negative.Test.Count, 
         week_pos_test_count=Weekly.Positive.Test.Count, week_case_count=Weekly.Case.Count) %>% 
  mutate(date_startweek1= mdy(date_startweek), #convert to date format
         date_endweek1=mdy(date_endweek), 
         month=month(date_endweek1)) 

#create monthly counts  
la_covid2<- la_covid1 %>%           
  group_by(tract_fips, month) %>%  
  summarise(
    test_month=sum(week_test_count),
    positive_month=sum(week_pos_test_count), 
    negative_month=sum(week_neg_test_count), 
    case_month=sum(week_case_count))
head(la_covid2)   


#Make sure each CT is in the data set every week, if not set to zero 
checkfull<-la_covid2 %>% 
  group_by(tract_fips)%>% 
  summarise(number = n())
table(checkfull$number)
#all ct's have 7 months so no need to set to zero.

#----------------------------------------------------------------------------
#import covid parish level cases + deaths from Johns Hopkins 

deaths_jh<-fread("time_series_covid19_deaths_US.csv", header=TRUE) %>%
  subset(Province_State=="Louisiana")%>%
  #only keep vars we'll need, drop obs after oct. 
  select(c(FIPS, Admin2, "3/31/20", "4/30/20", "5/31/20", "6/30/20", "7/31/20", "8/31/20", "9/30/20"))%>%
#remove observation w/ unassigned 
   subset(Admin2!="Out of LA")

deaths_jh1<- deaths_jh %>%
  rename(parish=Admin2, county_fips=FIPS, march="3/31/20", april="4/30/20", may="5/31/20", june="6/30/20",
                       july="7/31/20", august="8/31/20", september="9/30/20")%>%
         group_by(parish)%>%
           mutate(deaths_3=march,
                  deaths_4=april-march, 
                  deaths_5=may-april, 
                  deaths_6=june-may, 
                  deaths_7=july-june, 
                  deaths_8=august-july, 
                  deaths_9=september-august)%>%
  select(c(parish, county_fips,  deaths_3:deaths_9)) %>%
#remove row with data from unassigned county (n=190)
  subset(parish!="Unassigned")

deaths1<-deaths_jh1%>%
  pivot_longer(!c(county_fips, parish), names_to="date",  
                          values_to="deaths")%>%
  #not efficient way to recode names in deaths to month numbers...
  mutate(month=case_when(date=="deaths_3" ~3, 
                         date=="deaths_4"~4,
                         date=="deaths_5"~5,
                         date=="deaths_6"~6,
                         date=="deaths_7"~7,
                         date=="deaths_8"~8,
                         date=="deaths_9"~9)) %>%
  select(-date) 


#---------------------------------------------------------------------------#

#import the census data
#my census API key if needed
# census_api_key("xxxx", install=TRUE)

#view all variables
#v17 <- load_variables(2018, "acs5", cache = TRUE)
#View(v17)

#pull selected variables (poverty, total pop, race, median income)
acs_data<-get_acs(geography="tract",
                  variables=c(tract_pop_2018 = "B01003_001", white_alone="B02001_002", 
                              race_total="B02001_001", medincome = "B19013_001", 
                              ratio_poverty="B05010_001", below_poverty="B05010_010"),
                  state="LA",
                  year=2018)

acs_data_county<-get_acs(geography="county",
                         variables=c(county_pop_2018 = "B01003_001", 
                                     white_alone="B02001_002", 
                                     black_alone="B02001_003",
                                     race_total="B02001_001"),
                         state="LA",
                         year=2018)

#pivot wider so data has 1 row/ census tract 
acs_data1<-acs_data %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from=variable,  
              values_from=c(estimate,moe)) %>%
  rename(tract_fips=GEOID) %>%
  mutate(tract_fips=as.numeric(tract_fips),
         nonhisp_white=estimate_race_total-estimate_white_alone, 
         pct_nonwhite=nonhisp_white/estimate_race_total, 
         pct_poverty=estimate_below_poverty/estimate_ratio_poverty)

str(acs_data1)
head(acs_data1)

#
acs_data_county1<-acs_data_county %>% 
  select(-moe) %>% 
  pivot_wider(id_cols = c(GEOID, NAME), names_from=variable,  
              values_from=c(estimate)) %>%
  rename(county_fips=GEOID) %>% 
  mutate(county_fips=as.numeric(county_fips))%>%
  select(c(county_fips, county_pop_2018 ))

#---------------------------------------------------------------------------------
#import the CDC SVI: 
#---------------------------------------------------------------------------------
SVI<-read.csv("Louisiana.csv", header=TRUE)

#limit dataset to only the vars we care about 
SVI1<-SVI %>%
  subset(select=c(FIPS,RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES )) %>%
  rename(tract_fips=FIPS) %>% 
  # deal with missing values
  mutate(RPL_THEMES=ifelse(RPL_THEMES==-999, NA, RPL_THEMES),
         RPL_THEME1=ifelse(RPL_THEME1==-999, NA, RPL_THEME1),
         RPL_THEME2=ifelse(RPL_THEME2==-999, NA, RPL_THEME2),
         RPL_THEME3=ifelse(RPL_THEME3==-999, NA, RPL_THEME3),
         RPL_THEME4=ifelse(RPL_THEME4==-999, NA, RPL_THEME4)) %>% 
  mutate_at(-1, scale)
head(SVI1)

#import the county level SVI
svi_county<-fread("Louisiana_COUNTY.csv")

svi_county1<-svi_county %>%
  subset(select=c(FIPS,RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES )) %>%
  rename(county_fips=FIPS) %>% 
  # deal with missing values
  mutate(RPL_THEMES=ifelse(RPL_THEMES==-999, NA, RPL_THEMES),
         RPL_THEME1=ifelse(RPL_THEME1==-999, NA, RPL_THEME1),
         RPL_THEME2=ifelse(RPL_THEME2==-999, NA, RPL_THEME2),
         RPL_THEME3=ifelse(RPL_THEME3==-999, NA, RPL_THEME3),
         RPL_THEME4=ifelse(RPL_THEME4==-999, NA, RPL_THEME4)) %>% 
  mutate_at(-1, scale)
head(svi_county1)
#---------------------------------------------------------------------------------
#data cleaning/preparing for join on RUCA analysis (may be useful for subsequent analysis)
#---------------------------------------------------------------------------------

#limit RUCC & RUCC dataset to just LA 
ruca_LA<- ruca1 %>% 
  filter(state== "LA") %>% 
  select(-land_area)
head(ruca_LA)

rucc_LA<- rucc_county1 %>% 
  filter(state== "LA")  
head(rucc_LA)

########## create Urban/Suburban/Rural designation For RUCA ############
#can run over the full geography (All US by just substituting ruca_LA for ruca full file)
#geography Definition 1 -- 
#urban = 1  
#suburban = 2, 3, 4, 5, 6, 
#rural= 7, 8 , 9, 10 

ruca_LA1<- ruca_LA %>%
  mutate(primary_ruca=factor(primary_ruca), 
         geo_def_1= case_when(primary_ruca==1 ~1, 
                              primary_ruca%in%c(2:6) ~ 2,
                              primary_ruca%in%c(7:10) ~ 3,
                              TRUE ~99),
         geo_def_1=factor(geo_def_1, labels=c("urban", "suburban", "rural", "missing")), 
         #Geography Definition 2 (TBD)
         #urban = 1, 2, 3  
         #suburban = 4, 5, 6  
         #rural= 7, 8, 9, 10 
         geo_def_2= case_when(primary_ruca%in%c(1,7)~1, 
                              primary_ruca %in%c(2,3,4,5,6,8,9) ~2,
                              primary_ruca==10 ~3,
                              TRUE ~99),
         geo_def_2=factor( geo_def_2, labels=c("urban", "suburban", "rural", "missing")), 
         # Geography Definition 3 
         #urban = 1,  
         #inner suburb = 2, 3
         #outer suburb = 4, 5, 6 
         # rural= 7, 8, 9, 10 
         geo_def_3= case_when(primary_ruca==1 ~1, 
                              primary_ruca%in%c(2, 3) ~2,
                              primary_ruca%in%c(4:6) ~3, 
                              primary_ruca%in%c(7:10) ~4,
                              TRUE ~99),
         geo_def_3=factor( geo_def_3, labels=c("urban", "inner suburb", "outer suburb", "rural",  "missing")), 
         #create var for NOLA MSA
         nola_msa= case_when(county %in% c("Jefferson Parish","Orleans Parish","Plaquemines Parish", 
                                           "St. Bernard Parish","St. Charles Parish","St. John the Baptist Parish",
                                           "St. Tammany Parish","St. James Parish") ~ 1, 
                             TRUE ~ 0),
         nola_msa=factor(nola_msa),
         #create NOLA CBSA which adds Tangipahoa parish and Washington River, but excludes Pearl River County in Mississippi 
         nola_cbsa=case_when(county %in% c("Jefferson Parish","Orleans Parish","Plaquemines Parish", 
                                           "St. Bernard Parish","St. Charles Parish","St. John the Baptist Parish",
                                           "St. Tammany Parish", "St. James Parish",
                                           "Tangipahoa Parish","Washington Parish")~1,  
                             TRUE ~ 0),
         nola_cbsa=factor(nola_cbsa),
         #create NOLA geography (nola as it's own)
         nola_geo=case_when(county=="Orleans Parish" & primary_ruca==1  ~1,
                            primary_ruca==1 ~2, 
                            primary_ruca %in% c(2:6)~3,
                            primary_ruca %in%c(7:10) ~4,
                            TRUE ~99),
         nola_geo=factor(nola_geo, labels=c("New Orleans", "Other Urban", "Suburban", "Rural", "Missing")))
head(ruca_LA1)
table(ruca_LA1$primary_ruca, ruca_LA1$geo_def_1)
table(ruca_LA1$primary_ruca, ruca_LA1$geo_def_2)
table(ruca_LA1$primary_ruca, ruca_LA1$geo_def_3)
table(ruca_LA1$primary_ruca, ruca_LA1$nola_geo)
########  Create Urban/suburban rural designation for RUCC (county)     ##########
#county definition 1
#urban = 1, 2  
#suburban = 3,4,5, 6,  
#rural= 7,8,9
table(rucc_county$RUCC_2013)

rucc_LA1<- rucc_LA %>%
  mutate(county_geo_1= case_when(code_rucc%in% c(1, 2)~1, 
                                 code_rucc%in% c(3,4,5,6) ~2, 
                                 code_rucc%in% c(7, 8, 9) ~3, 
                                 TRUE~ 99),
         county_geo_1=factor(county_geo_1, labels=c("urban", "suburban", "rural"))) %>%
  
  mutate(nola_geo1=case_when(county=="Orleans Parish" & code_rucc==1 ~1,
                     code_rucc %in% c(1, 2) ~2,
                     code_rucc %in% c(3, 4, 5, 6) ~3, 
                     code_rucc %in% c(7,8, 9) ~4,
                     TRUE~ 99),
         nola_geo1=factor(nola_geo1, labels=c("New Orleans", "Other urban", "Suburban", "Rural")))

table(rucc_LA1$code_rucc, rucc_LA1$county_geo_1)
table(rucc_LA1$code_rucc, rucc_LA1$nola_geo1)

# create a monthly geo_def_1 dataset
final_covid<-la_covid2 %>% left_join(ruca_LA1) %>% 
  group_by(nola_geo, month) %>% 
  summarise(cases=sum(case_month),
            positives=sum(positive_month),
            tests=sum(test_month))
final_acs<-acs_data1 %>% left_join(ruca_LA1) %>% 
  group_by(nola_geo) %>% 
  summarise(estimate_tract_pop_2018=sum(estimate_tract_pop_2018))
final_nola_geo<-full_join(final_covid, final_acs) %>% 
  mutate(incidence_rate=cases/estimate_tract_pop_2018*10000,
         testing_rate=tests/estimate_tract_pop_2018*10000,
         positivity_ratio=positives/tests) %>% 
  filter(!is.na(month))
# create a monthly tract dataset, with nola_geo1 and SVI 
final_tract<-la_covid2 %>% 
  group_by(tract_fips, month) %>% 
  summarise(cases=sum(case_month),
            positives=sum(positive_month),
            tests=sum(test_month)) %>% 
  left_join(acs_data1) %>% 
  mutate(incidence_rate=cases/estimate_tract_pop_2018*10000,
         testing_rate=tests/estimate_tract_pop_2018*10000,
         positivity_ratio=positives/tests) %>% 
  left_join(ruca_LA1) %>% 
  left_join(SVI1)
# create a monthly death dataset w/ population estimates + SVI
final_acs_county<-acs_data_county1 %>%
  left_join(rucc_LA1)%>%
  select(county_fips, county_pop_2018, nola_geo1)
final_deaths_county<-deaths1 %>% 
  left_join(final_acs_county)%>% 
  mutate(death_rate=deaths/county_pop_2018*100000)%>%
  left_join(svi_county1) %>%
    select(parish, county_fips, deaths, month, death_rate, nola_geo1, county_pop_2018, RPL_THEME1:RPL_THEMES)

final_deaths_geo<-final_deaths_county%>%
  group_by(month, nola_geo1)%>%
  summarize(death_geo=sum(deaths),
            geo_pop=sum(county_pop_2018))%>%
  mutate(death_rate=death_geo/geo_pop*100000)


summary(final_nola_geo)
summary(final_tract)
summary(final_deaths_county)
summary(final_deaths_geo)

save(final_nola_geo,
     final_tract,
     final_deaths_county,
     final_deaths_geo, file="final_data.rdata")

#save df w/ total deaths by geo
sum_county<-final_acs_county%>%
  group_by(nola_geo1)%>%
  summarise(geo_pop=sum(county_pop_2018))

total_deaths_county<-deaths1 %>% 
  left_join(rucc_LA1)%>% 
   group_by(nola_geo1) %>%
  summarise(deaths_total=sum(deaths, na.rm=T)) %>%
left_join(sum_county) %>%
mutate(death_rate_total=deaths_total/geo_pop*100000)
summary(total_deaths_county)
save(total_deaths_county, file="total_deaths_county.Rdata")
