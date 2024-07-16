### Generate a map of resistance by gender 

## Needed libraries
library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library(giscoR)

# World map data 
world_coordinates <- map_data("world") 
worldMap <- map_data("world") 

# geom_map() function takes world coordinates as input  
# to plot world map color parameter determines the  
# color of borders in map fill parameter determines  
# the color of fill in map size determines the thickness 
# of border in map 

## Our data
data <- read_csv("data/atlas_data_cleaned.csv")

## What proportion in women? and then calculate a ratio
data_gender_props <- data %>% group_by(country, age, species, gender, antibiotic) %>%
  summarise(n_isolates = n(),
            prop_r = sum(mic_label)/n_isolates) %>% 
  select(-n_isolates) %>% 
  pivot_wider(names_from = gender, values_from = prop_r) %>%
  group_by(country, age, species, antibiotic) %>%
  summarise(ratio = log(f / m)) %>%
  rename(region = country)

## Add in missing countries into the data 
data_cntry <- unique(data_gender_props$region)
data_map <- unique(worldMap$region)
missing_countries <- setdiff(data_map, data_cntry)

distinct_rows <- data_gender_props %>% ungroup() %>% select(-c(ratio, region)) %>% distinct() %>%
  expand(age, species, antibiotic)
# generate new data with all combinations but NA for ratio
big_data <- data_gender_props
for(i in missing_countries){
  d <- distinct_rows %>% mutate(region = i, ratio = NA)
  big_data <- rbind(big_data, d)
}

## Cycle through all the bug-drug combinations in this data
drugs <- unique(data_gender_props$antibiotic)
bugs <- unique(data_gender_props$species)
ages <- unique(data_gender_props$age)

# Where save the plots? 
if(!file.exists("plots")){dir.create(file.path("plots"))}
if(!file.exists("plots/maps")){dir.create(file.path("plots/maps"))}

# Order ages
big_data$age <- factor(big_data$age, 
                                   levels = c("0 to 2 Years", "3 to 12 Years", "13 to 18 Years", "19 to 64 Years", 
  "65 to 84 Years", "85 and Over", "Unknown"))

# Generate the maps
for(i in drugs){
  for(j in bugs){
    
    data_grab <- big_data %>% filter(antibiotic == i,
                                     species == j) 
    
    if(dim(data_grab %>% filter(!is.na(ratio)))[1] > 10){ # if data from at least 10 sub-populations
      
      ggplot(data_grab, 
             aes(map_id = region)) + 
        geom_map(aes(fill = ratio), map = worldMap, 
                 color='grey66', size=0.3) + 
        expand_limits(x = worldMap$long, y = worldMap$lat) +
        facet_wrap(~age) + 
        theme_few()+
        ggtitle(paste0(i, " resistance in ", j)) + 
        #ggtitle(expression(i~" resistance in "~italic(j))) + 
        theme(legend.position = "bottom",
              axis.ticks = element_blank(), 
              axis.title = element_blank(), 
              axis.text =  element_blank()) +
        scale_fill_gradient2(low="blue", high="red", mid = "yellow", 
                             midpoint = 0, 
                             name="Relative proportion (log) in women vs men",
                             na.value="grey88") +
        guides(fill = guide_colorbar(barwidth = 10, barheight = .5))
      ggsave(paste0("plots/maps/map_",i,"_",j,".pdf"))
    }
  }    
}


