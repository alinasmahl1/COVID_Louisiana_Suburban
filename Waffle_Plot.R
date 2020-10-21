#figuring out waffle plot 
library(waffle)
library(ggwaffle)
library(hrbrthemes)
library(RColorBrewer)
library(tidyverse)
library(ggplot2)

load("final_data.rdata")
head(final_nola_geo)

#find codes for colors in other graphs 
brewer.pal(n=4, name="Set2")

#calculate total cases by geo and create 2 peaks 1 before June 1, and 1 after 
summary<-final_nola_geo %>%
  mutate(
    peaks=case_when(month %in%c(3, 4, 5, 6) ~1, 
                         month %in% c(7, 8, 9 , 10) ~2)) %>%
    group_by(peaks, nola_geo) %>%
  summarize(case_peaks=sum(cases))

waffle(waffle$case_peaks)

#create vector w/ info from waffle 
peak1<-c("New Orleans"= 6833, "Other Urban"=29728, "Suburban"=8825, "Rural"=3563)
peak2<-c("New Orleans"= 4292, "Other Urban"=56373, "Suburban"=24094, "Rural"=7901) 

p1<-
    waffle(peak1/1000, 
           colors = c("#66C2A5", "#FC8D62", "#8DA0CB",  
                      "#E78AC3"),
           rows=8, 
           size=0.5, 
           title="March to June",  
           xlab="1 square = 1000 cases")+ 
              theme(
                       plot.title=element_text(color="black", size=20, face="bold"),
                       strip.background = element_blank(),
                       strip.text =element_text(color="black", size=16, face="bold"),
                       legend.position = "none")


p1
p2<-    
    waffle(peak2/1000, rows=8, size=0.5, 
             colors= c("#66C2A5", "#FC8D62", "#8DA0CB",  
                        "#E78AC3"),
             title="June-October",  
             xlab="1 square = 1000 cases") +
          theme(
               plot.title=element_text(color="black", size=20, face="bold"),
               strip.background = element_blank(),
               strip.text =element_text(color="black", size=16, face="bold"),
               legend.text=element_text(color="black", size=14))

  p2
    
#alternative
waffle_chart(
  waffle,
  fill=case_peaks,
  value = "nola_geo",
  facet = peaks,
  composition = TRUE,
  max_value = NULL,
  digits = 3,
  fill_colors = NULL,
  fill_title = NULL,
  ncol = NULL,
  base_size = 12,
  line_size = LS(1),
  legend.position = "bottom"
  )
  
  
#but can't get geom_waffle to run... 
waffle%>%
  count(nola_geo, wt = case_peaks) %>%
  ggplot(aes(fill = nola_geo, values = case_peaks)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() -> waf
  
waf +
  geom_waffle(
    n_rows = 20, size = 0.33, colour = "white", flip = TRUE
  )

  summary %>%
    ggplot(aes(fill = nola_geo, values = case_peaks)) +
    expand_limits(x=c(0,0), y=c(0,0)) +
    coord_equal() +
    labs(fill = NULL, colour = NULL) +
    theme_ipsum_rc(grid="")  -> buf
  buf +
    geom_waffle(
      color = "white", size = 0.33
    ) +
    facet_wrap(~peaks) +
    theme(strip.text.x = element_text(hjust = 0.5))
  
  waffle<-ggplot(summary, aes(fill=nola_geo, values=case_peaks/100)) +
    geom_waffle(color = “white”, size=1.125, n_rows = 8) +
    labs(x=“1 square = 100 cases”) +
    coord_equal() +
    scale_fill_discrete(name=“”) +
    facet_wrap(~peaks) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.text=element_blank(), axis.title.y=element_blank())
 