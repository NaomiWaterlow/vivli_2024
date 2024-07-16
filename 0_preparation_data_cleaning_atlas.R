### Data explore and clean 
# Cleaning specific to ATLAS dataset
# To use the tool for other data, own prep of data must be done

## Libraries
library(tidyverse); library(readxl); library(data.table); library(countrycode)

## Read in data 
# Data inputs need to be in folder called "data"
list.files("data") # Should be 6 files

## ATLAS
suppressWarnings(atlas <- read_csv("data/gender_/ATLAS_Antibiotics/2024_05_28 atlas_antibiotics.csv"))
# Warnings about column types, can be ignored
colnames(atlas)
# Check age groups
unique(atlas$`Age Group`)
# 7 age-groups: 0-2/3-12/13-18/19-64/65-84/85+ # And Unknown (gets removed later)
unique(atlas$Gender)
# Gender has NAs - remove these later
# Explore antibiotic data 
unique(atlas$Amikacin)
# Some data are NAs/logicals: remove
unique(atlas$Gatifloxacin)
unique(atlas$Tetracycline)
# Some are all NA: remove
unique(atlas$Gatifloxacin_I)
# Some are doubles: make characters for now
unique(atlas$`Quinupristin dalfopristin`)
atlas$`Quinupristin dalfopristin` <- as.character(atlas$`Quinupristin dalfopristin`)

# Pivot longer to explore MICs
# Ignores the specified drugs, melts the rest, removes NAs and adds a column for data source
# NOTE! This melts in both the mic value, but also the categorisation (S/I/R etc). So many records in twice. Drops out later. 
atlas_clean <- atlas %>% 
  select(-c("Gatifloxacin", "Gatifloxacin_I","Tetracycline")) %>% 
  pivot_longer(cols = `Amikacin`:`GIM`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic)) %>% mutate(data = "atls")

# Cleaning variable names
unique(atlas_clean$mic)
colnames(atlas_clean) <- tolower(colnames(atlas_clean))
atlas_clean <- rename(atlas_clean, "organism" = "species")
atlas_clean <- rename(atlas_clean, "age" = "age group")
# Select labels (not values, could use as.sir package on values later)
atlas_clean_mic <- atlas_clean %>% 
  filter(mic %in% c("Susceptible","Intermediate","Resistant"))

## Variables to use in analyses
col_use <- c("age","gender","mic","year", "country","organism","antibiotic","data")

## Extract variables and remove NAs
atlas_clean_nona <- atlas_clean_mic[,col_use] %>% 
  filter(!is.na(age), !is.na(gender), !gender == "N") %>% 
  mutate(organism_clean = "")

## Clean gender
unique(atlas_clean_nona$gender)
atlas_clean_nona$gender <- tolower(atlas_clean_nona$gender)
atlas_clean_nona$gender <- substr(atlas_clean_nona$gender, 1, 1)     
unique(atlas_clean_nona$gender)

## Clean year? No need
unique(atlas_clean_nona$year)

## Clean organism for top 3 bugs
u <- unique(atlas_clean_nona$organism)
table(atlas_clean_nona$organism) %>% as.data.frame() %>% arrange(desc(Freq))

# S aureus
u[str_which(u, "aureus")] # yes
u[str_which(u, "Staph")] # too many: think above captures it 
atlas_clean_nona[which(atlas_clean_nona$organism == u[str_which(u, "aureus")]),"organism_clean"] <- "Staphylococcus aureus"

# E coli 
u[str_which(u, "coli")] # no too many others 
u[str_which(u, "E coli")] # none
u[str_which(u, "E. coli")] # none
u[str_which(u, "Escherichia")] # too many
atlas_clean_nona[which(atlas_clean_nona$organism == u[str_which(u, "Escherichia coli")]),"organism_clean"] <- "Escherichia coli"

# K pneumoniae 
u[str_which(u, "Kleb")] # no too many others 
u[str_which(u, "kleb")] # none
u[str_which(u, "Klebsiella")] # lots
u[str_which(u, "pneumoniae")] # also includes strep
atlas_clean_nona[which(atlas_clean_nona$organism == u[str_which(u,  "Klebsiella pneumoniae")]),"organism_clean"] <- "Klebsiella pneumoniae"

# Focus on top 3 bugs found
atlas_clean_focus_b <- atlas_clean_nona %>% 
  filter(organism_clean %in% c("Escherichia coli",
                               "Staphylococcus aureus",
                               "Klebsiella pneumoniae"))

## Clean antibiotic names
atlas_clean_focus_b <- atlas_clean_focus_b %>% 
  mutate(antibiotic = str_extract(antibiotic, "[^_]+"))
atlas_clean_focus_b$antibiotic <- tolower(atlas_clean_focus_b$antibiotic)
abx <- unique(atlas_clean_focus_b$antibiotic) 
# No weird ones to rename

# Look at top antibiotics overall
table(atlas_clean_focus_b$antibiotic) %>% as.data.frame() %>% arrange(desc(Freq))
# 1) Tigecycline = 848,612 / 2) Levofloxacin = 832,186 / 3) Ampicillin = 789,534


# Now look at top antibiotics by bug AND drug
atlas_clean_focus_bd <- atlas_clean_focus_b %>% 
  group_by(organism_clean, antibiotic) %>%
  summarise(number_isolates = n()) %>%
  ungroup() %>%
  group_by(organism_clean) %>% 
  arrange(desc(number_isolates))


# Top antibiotics for S aureus
head(atlas_clean_focus_bd %>% filter(organism_clean=="Staphylococcus aureus"))
# =1) Levofloxacin, Vancomycin = 146470 / 3) Linezolid = 146470 (but also Tigecycline!)

# Now look at top antibiotics by bug AND drug and resistance level 
atlas_clean_focus_bd_r <- atlas_clean_focus_b %>% 
  group_by(organism_clean, antibiotic,mic) %>%
  summarise(nn = n()) %>% 
  pivot_wider(names_from = mic, values_from = nn) %>%
  summarise(total = sum(Intermediate, Resistant, Susceptible, na.rm = TRUE), 
            prop_r = Resistant / total)  %>% 
  ungroup() %>%
  group_by(organism_clean) %>% 
  arrange(desc(organism_clean), desc(total))

atlas_clean_focus_bd_r %>% filter(total > 100000)


# If choosing top 3
atlas_clean_focus_sad <- atlas_clean_focus_b %>% 
  filter(organism_clean=="Staphylococcus aureus",
         antibiotic %in% c("levofloxacin",
                          "vancomycin", # Exclude as resistance < 5%
                           "erythromycin"))
# If choosing 2
atlas_clean_focus_sad_pre <- atlas_clean_focus_b %>% 
  filter(organism_clean=="Staphylococcus aureus",
         antibiotic %in% c("levofloxacin",
                           "erythromycin"))

# Top antibiotics for E coli
head(atlas_clean_focus_bd %>% filter(organism_clean=="Escherichia coli"))
# =1) Amikacin, Amoxycillin clavulanate, Ampicillin, Cefepime, Levofloxacin = 105628
# If choosing top 3
atlas_clean_focus_ecd <- atlas_clean_focus_b %>% 
  filter(organism_clean=="Escherichia coli",
         antibiotic %in% c("amikacin",
                           "amoxycillin clavulanate",
                           "ampicillin"))
# If choosing 2
atlas_clean_focus_ecd_pre <- atlas_clean_focus_b %>% 
  filter(organism_clean=="Escherichia coli",
         antibiotic %in% c("levofloxacin",
                           "ampicillin")) # both have > 30% resistance

# Top antibiotics for K pneumoniae
head(atlas_clean_focus_bd %>% filter(organism_clean=="Klebsiella pneumoniae"))
# =1) Amikacin, Amoxycillin clavulanate, Cefepime = 87902
# If choosing top 3
atlas_clean_focus_kpd <- atlas_clean_focus_b %>% 
  filter(organism_clean=="Klebsiella pneumoniae",
         antibiotic %in% c("amikacin",
                           "amoxycillin clavulanate",
                           "cefepime"))

# Dataset focussing on 3 antibiotics for each of 3 focus bacteria
atlas_clean_focus_both <- rbind(atlas_clean_focus_sad,
                                atlas_clean_focus_ecd,
                                atlas_clean_focus_kpd)

# BUT for preliminary analysis just focus on 2 drugs for each of 2 bugs
atlas_clean_focus_saec <- rbind(atlas_clean_focus_sad_pre,
                                atlas_clean_focus_ecd_pre)

# Clean age
unique(atlas_clean_focus_saec$age)
# Unknown ages, need to clean
atlas_clean_focus_saec_age <- atlas_clean_focus_saec %>% 
  filter(age!="Unknown")
# Check
unique(atlas_clean_focus_saec_age$age)
# Factor age variable
atlas_clean_focus_saec_age$age <- factor(atlas_clean_focus_saec_age$age, 
                                               levels = c("0 to 2 Years","3 to 12 Years", "13 to 18 Years",
                                                          "19 to 64 Years", "65 to 84 Years", "85 and Over"))

# For preliminary analysis table, look at how many intermediate
table(atlas_clean_focus_saec$mic)
# Not many relative to sus/res so ASSUME Int -> Sus
# Prepare data for analysis
atlas_clean_focus_pre <- atlas_clean_focus_saec %>% 
  mutate(mic_label = ifelse(mic=="Resistant",1,0),
         organism_clean = as.character(organism_clean),
         antibiotic = as.character(antibiotic),
         country = as.character(country)) %>% 
  select(-c("mic","organism")) %>% 
  rename(species = organism_clean)
# NOTE: Struggled to change country to codes - change later during analysis

## Save final cleaned dataset
write.csv(atlas_clean_focus_pre, "data/atlas_data_cleaned.csv")

## Clean environment
rm(list = c("atlas",
            "atlas_clean",
            "atlas_clean_mic",
            "atlas_clean_nona",
            "atlas_clean_focus_b",
            "atlas_clean_focus_bd",
            "atlas_clean_focus_sad",
            "atlas_clean_focus_sad_pre",
            "atlas_clean_focus_ecd",
            "atlas_clean_focus_ecd_pre",
            "atlas_clean_focus_kpd",
            "atlas_clean_focus_both",
            "atlas_clean_focus_saec",
            "atlas_clean_focus_saec_age",
            "atlas_clean_focus_pre"))
