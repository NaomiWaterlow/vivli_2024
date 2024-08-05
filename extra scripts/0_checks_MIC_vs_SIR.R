### Checking for MIC vs SIR values: do the top bug-drug combinations have any missing SIR values? 
## Do we need to convert MIC to SIR? no - all the top bug-drugs have this

### Libraries
library(tidyverse)
library(AMR)

### CHECK on 2023 data and raw ATLAS data 
data_23 <- read_csv("data/full_data.csv")
atlas <- read_csv("../vivliamr_submitted/data/2023_06_15 atlas_antibiotics.csv")
cleaned_data <- data_23 %>% filter(data == "atls")

### Extract top 3 bacteria from all data 
cleaned_data %>% group_by(organism) %>% 
  summarise(number_isolates = n()) %>% arrange(-number_isolates) %>% slice(1:3)

bugs <- cleaned_data %>% group_by(organism) %>% 
  summarise(number_isolates = n()) %>% arrange(-number_isolates) %>% slice(1:3) %>%
  select(organism) %>% c()

bugs <- atlas_clean %>% group_by(Species) %>% 
  summarise(number_isolates = n()) %>% arrange(-number_isolates) %>% slice(1:5) %>%
  select(Species) %>% c()

### Extract top 3 drugs for each of the top 3 bugs
cleaned_data %>% filter(organism %in% bugs$organism) %>% group_by(organism, antibiotic) %>% 
  summarise(number_isolates = n()) %>% 
  ungroup() %>% 
  group_by(organism) %>% 
  arrange(-number_isolates, .by_group = TRUE) %>% print(n=Inf)

cleaned_data %>% filter(organism %in% bugs$organism) %>% group_by(organism, antibiotic) %>% 
  summarise(number_isolates = n()) %>% 
  ungroup() %>% 
  group_by(organism) %>% 
  arrange(-number_isolates, .by_group = TRUE) %>% slice(1:8) %>% print(n=Inf)

## Several drugs have the same number of isolates 
## => look at number with MIC vs isolate numbers 
drugs <- cleaned_data %>% filter(organism %in% bugs$Species) %>% group_by(organism, antibiotic) %>% 
  summarise(number_isolates = n()) %>% 
  ungroup() %>% 
  group_by(organism) %>% 
  arrange(-number_isolates, .by_group = TRUE) %>% slice(1:8) %>% print(n=Inf)

### 
###### (1) ATLAS
atlas$`Quinupristin dalfopristin` <- as.character(atlas$`Quinupristin dalfopristin`)

# Pivot longer to explore ranges in MIC
# Ignores the specified drugs, melts the rest, removes NAs and adds a colum for data source
# NOTE! This melts in both the mic value, but also the categorisation (S/I/R etc). So many records in twice. Drops out later. 
atlas_clean <- atlas %>% select(-c("Gatifloxacin", "Gatifloxacin_I","Tetracycline")) %>% 
  pivot_longer(cols = `Amikacin`:`GIM`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic)) %>% mutate(data = "atls")

# Check distribution of SIR vs MIC for the ones we want 
atlas_clean$antibiotic <- tolower(atlas_clean$antibiotic)
aa <- atlas_clean %>% filter(Year > 2014, Year < 2020,
                             Species %in% bugs$Species) %>% mutate(with_i = grepl("_i",antibiotic)) %>% 
  mutate(drug = ifelse(with_i, str_extract(antibiotic, "[^_]+"),antibiotic)) %>% 
  group_by(`Isolate Id`, drug, Species) %>% summarise(number = n()) 

aa %>% filter(drug %in% drugs$antibiotic) %>% group_by(Species, drug) %>% summarise(mean(number))

# Check how many drugs (of those that we want) have 2 results i.e. an MIC and an SIR value
aad <- aa %>% filter(drug %in% drugs$antibiotic) %>% group_by(Species, drug, number) %>% tally()
aad

### Suggests that out top bug-drug combinations have two entries 
### => both MIC + SIR so fine to use

### e.g. 
l1 <- atlas_clean %>% filter(Species == "Staphylococcus aureus", antibiotic == "levofloxacin")
l2 <- atlas_clean %>% filter(Species == "Staphylococcus aureus", antibiotic == "levofloxacin_i")

dim(l1)
dim(l2)


#### Intermediate keep? Not many... 
ai <- atlas_clean %>% filter(mic %in% c("Susceptible","Intermediate","Resistant"))
table(ai$mic)
