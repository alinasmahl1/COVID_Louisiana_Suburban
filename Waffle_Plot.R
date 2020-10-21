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
  library(hrbrthemes)
  final<-summary %>% 
    group_by(peaks) %>% 
    mutate(pct=case_peaks/sum(case_peaks)*100) %>% 
    #mutate(pct = (smart_round(pct, 1) * 100L) %>%  as.integer()) %>% 
    mutate(pct=round(pct, digits=0)) %>% 
    select(-case_peaks) %>% 
    ungroup() %>% 
    mutate(nola_geo = as.character(nola_geo))  %>% 
    mutate(peaks = ifelse(peaks==1, "First", "Second")) %>% 
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
          legend.title = element_blank()) 
  plot
  