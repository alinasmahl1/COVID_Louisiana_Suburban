
##***********************RUCA and RUCC recoding **************************
#*Version 1 started Jan 20, 2022 by Alina Schnake-Mahl
#*Imports and recodes  RUCA/RUCC for Urban/Rural/Suburban designations


#import RUCA and RUCC 2010 data 
ruca<-read.csv("Data/ruca2010revised.csv",  header=TRUE, stringsAsFactors = FALSE)
str(ruca)
head(ruca)
#rename vars so easier to use
ruca1<- ruca %>% rename(county_fips=ï..State.County.FIPS.Code, state=Select.State, county=Select.County, 
                        tract_fips=State.County.Tract.FIPS.Code..lookup.by.address.at.http...www.ffiec.gov.Geocode.., 
                        primary_ruca=Primary.RUCA.Code.2010, secondary_ruca=Secondary.RUCA.Code..2010..see.errata., 
                        tract_pop=Tract.Population..2010, land_area=Land.Area..square.miles...2010, pop_density=Population.Density..per.square.mile...2010)

str(ruca)

#import RUCC data 
rucc_county<-read.csv("data/ruralurbancodes2013.csv",  header=TRUE, stringsAsFactors = FALSE)
rucc_county
str(rucc_county)
head(rucc_county)

rucc_county1<-rucc_county %>% 
  rename(county_fips=ï..FIPS, state=State, county=County_Name, pop_2010=Population_2010, code_rucc=RUCC_2013)



#limit RUCC & RUCC dataset to just  (as an example) 
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
         geo_def_3=factor( geo_def_3, labels=c("urban", "inner suburb", "outer suburb", "rural",  "missing"))) 
        
head(ruca_LA1)
table(ruca_LA1$primary_ruca, ruca_LA1$geo_def_1)
table(ruca_LA1$primary_ruca, ruca_LA1$geo_def_2)
table(ruca_LA1$primary_ruca, ruca_LA1$geo_def_3)


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
         county_geo_1=factor(county_geo_1, labels=c("urban", "suburban", "rural"))) 

table(rucc_LA1$code_rucc, rucc_LA1$county_geo_1)
