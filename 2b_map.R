####### August 2024 ######################################################################
## Authors: Naomi Waterlow , Alastair Clements, Chaelin Kim, Simon Procter, Gwen Knight ##
##########################################################################################

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
theme_set(theme_bw())

### Remove those with < 1000
### Remove those with unknown age 

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
  filter(!age == "Unknown") %>% 
  summarise(n_isolates = n(),
            prop_r = sum(mic_label)/n_isolates) %>% 
  select(-n_isolates) %>% 
  pivot_wider(names_from = gender, values_from = prop_r) %>%
  #group_by(country, age, species, antibiotic) %>%
  group_by(country, species, antibiotic) %>% # ignore age for now
  summarise(ratio = log(f / m)) %>%
  rename(region = country)

## Add in missing countries into the data 
data_cntry <- unique(data_gender_props$region)
data_map <- unique(worldMap$region)
missing_countries <- setdiff(data_map, data_cntry)

distinct_rows <- data_gender_props %>% ungroup() %>% select(-c(ratio, region)) %>% distinct() %>%
  #expand(age, species, antibiotic)
  expand(species, antibiotic)
# generate new data with all combinations but NA for ratio
big_data <- data_gender_props
for(i in missing_countries){
  d <- distinct_rows %>% mutate(region = i, ratio = NA)
  big_data <- rbind(big_data, d)
}

## Cycle through all the bug-drug combinations in this data
drugs <- unique(data_gender_props$antibiotic)
bugs <- unique(data_gender_props$species)
#ages <- unique(data_gender_props$age)

# Where save the plots? 
if(!file.exists("plots")){dir.create(file.path("plots"))}
if(!file.exists("plots/maps")){dir.create(file.path("plots/maps"))}

# Order ages
#big_data$age <- factor(big_data$age, 
#                                   levels = c("0 to 2 Years", "3 to 12 Years", "13 to 18 Years", "19 to 64 Years", 
#  "65 to 84 Years", "85 and Over", "Unknown"))
map_counter <- 0
# Generate the maps
for(i in drugs){
  for(j in bugs){
    
    data_grab <- big_data %>% filter(antibiotic == i,
                                     species == j) 
    
    if(dim(data_grab %>% filter(!is.na(ratio)))[1] > 10){ # if data from at least 10 sub-populations
      
      
      
      
    PLOT_TEMP <-   ggplot(data_grab, 
             aes(map_id = region)) + 
        geom_map(aes(fill = ratio), map = worldMap, 
                 color='grey66', size=0.3) + 
        expand_limits(x = worldMap$long, y = worldMap$lat) +
        theme_few()+
      ggtitle(bquote(.(i) ~ "resistance in" ~ italic(.(j)))) + 
        theme(legend.position = "bottom",
              axis.ticks = element_blank(), 
              axis.title = element_blank(), 
              axis.text =  element_blank()) +
        scale_fill_gradient2(low="blue", high="red", mid = "yellow", 
                             midpoint = 0, 
                             name="Relative proportion (log) in women vs men",
                             na.value="grey88") +
        guides(fill = guide_colorbar(barwidth = 10, barheight = .5))
    
    map_counter <- map_counter +1
    assign(paste0("MAP_", map_counter), PLOT_TEMP)
      ggsave(paste0("plots/maps/map_",i,"_",j,".pdf"))
    }
  }    
}

leg <- get_legend(MAP_1)
COMBINED_MAPS <- grid.arrange(MAP_1 + theme(legend.position = "none"),
             MAP_2 + theme(legend.position = "none"),
             MAP_3 + theme(legend.position = "none"),
             MAP_4 + theme(legend.position = "none"), 
             leg, 
             layout_matrix = rbind(c(1,1,1,2,2,2),
                                   c(1,1,1,2,2,2),
                                   c(1,1,1,2,2,2),
                                   c(1,1,1,2,2,2),
                                   c(1,1,1,2,2,2),
                                   c(3,3,3,4,4,4),
                                   c(3,3,3,4,4,4),
                                   c(3,3,3,4,4,4),
                                   c(3,3,3,4,4,4),
                                   c(3,3,3,4,4,4),
                                   c(5,5,5,5,5,5)))

ggsave(paste0("plots/maps/combined_map.pdf"), plot = COMBINED_MAPS, 
       width = 10, height = 6)

## Add in income group 
key <- read.csv("data/country_maps.csv") 
# Check all in there: data_gender_props %>% filter(!region %in% unique(key$country))

data_gender_props_in <- left_join(data_gender_props, key)

## How does it vary by country? 
# ggplot(data_gender_props %>% filter(species == "Staphylococcus aureus", !is.na(ratio)), aes(x=interaction(age, region), y = ratio)) + 
#   geom_point(aes(colour = age)) + 
#   facet_grid(age ~ antibiotic)

ggplot(data_gender_props %>% filter(!is.na(ratio)), 
       aes(x=antibiotic, y = ratio)) + 
  geom_boxplot(aes(colour = antibiotic)) + 
#  geom_boxplot(aes(colour = age)) + 
  facet_grid(~ species) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

### BY regions 
ggplot(data_gender_props_in %>% filter(!is.na(ratio)), 
       aes(x=antibiotic, y = ratio)) + 
  geom_boxplot(aes(colour = antibiotic)) + 
  facet_grid(species ~ income_group) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("plots/ratio_by_species_antibiotic_income_group_boxplot.pdf")

ggplot(data_gender_props_in %>% filter(!is.na(ratio)), 
       aes(x=antibiotic, y = ratio)) + 
  geom_boxplot(aes(colour = antibiotic)) + 
  facet_grid(species  ~ region_nat) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("plots/ratio_by_species_antibiotic_region_boxplot.pdf")


ggplot(data_gender_props_in %>% filter(!is.na(ratio)), 
       aes(x=antibiotic, y = ratio, group = region_nat)) + 
  geom_point(aes(col = antibiotic)) + 
  geom_smooth(aes(col = species)) + 
  facet_grid(species + antibiotic ~ region_nat) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("plots/ratio_by_species_antibiotic_region_dots.pdf")

