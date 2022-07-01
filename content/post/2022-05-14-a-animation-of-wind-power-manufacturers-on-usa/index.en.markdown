---
title: A animation of wind power manufacturers on USA
author: Erika Borelli
date: '2022-05-14'
slug: []
categories:
  - R
tags:
  - minianalysis
  - ggplot2
Description: ''
Tags: []
Categories: []
DisableComments: no
---


![Green Moutain Wind Farm 2004](images/GreenMountainWindFarm_Fluvanna_2004.jpg)

The United States are increasingly investing in and encouraging the creation of wind farms. With this analysis we can see how many turbines were built between 1981 and 2021 in the US. 


### Dataset

The U.S Wind Turbine Database is a repository of over 70k turbines across the United States. The data provides a view into a number of descriptor variables for each individual turbine with varying degrees of coverage, detail and accuracy.



After cleaning and transforming the data, the dataset presented 58536 turbines to be analyzed with full confidence in the data.

### Animation


```r
manu_accumulated <- uswt_clean %>% 
  group_by(t_manu, p_year) %>% 
  summarise(quant = n()) %>% 
  pivot_wider(names_from = p_year, values_from = quant, values_fill = 0) %>% 
  pivot_longer(!t_manu, names_to = "year",
               values_to = "quant") %>%
  arrange(t_manu, year) %>% 
  mutate(quant_cum = cumsum(quant)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(rank = rank(-quant_cum) #%>%  floor(),
         ,Value_rel = quant_cum/quant_cum[rank==1]) %>% 
  group_by(t_manu) %>% 
  filter(rank <=10) %>%  
  ungroup()
```

```
## `summarise()` has grouped output by 't_manu'. You can override using the `.groups` argument.
```

```r
# Building Static Plots

staticplot = ggplot(manu_accumulated,
                    aes(rank, group = t_manu, 
                        fill = as.factor(t_manu),
                        color = as.factor(t_manu))) + 
  geom_tile(aes(y = quant_cum/2, height = quant_cum
                , width = 0.9),
            alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(t_manu, " ")),
            vjust = 0.2, hjust = 1, size = 7) +
  geom_text(aes(y=quant_cum,label = quant_cum, hjust=0)
            ,size = 7) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
         axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1,
                                           color="grey" ),
        panel.grid.minor.x = element_line( size=.1,
                                           color="grey" ),
        plot.title=element_text(size=25, hjust=0.5,
                                face="bold", colour="grey",
                                vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5,
                                   face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5,
                                   face="italic", color="grey"),
        plot.background=element_blank(),
       plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(year, transition_length = 9, state_length = 1, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Number of turbines per Year : {closest_state}',  
       subtitle  =  "Top 10 Turbine manufacturer",
       caption  = " Data Source: USWTDB ")
```

`animate(anim, 200, fps = 25,  width = 1300, height = 1000, duration =  30,
         renderer = gifski_renderer("turbinemanufactor.gif"))`
         
![Top 10 Turbine Manufactor per year](images/turbinemanufactor.gif) 

It shows us that the manufacturer that has the largest number of turbines used in the USA today is GE Wind with 44.7% of all turbines in the country, conquering its leadership from 2007. The second manufacturer with the largest quantity of turbines is Vestas, with 24.4% of the turbines in the country, it remained in the lead during the years 1983 to 2006.
