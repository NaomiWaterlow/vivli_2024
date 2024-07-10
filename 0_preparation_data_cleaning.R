##### DATA explore and clean 

#This script is specific for the MICAG analysis. 
# to use the tool for other data, own prep of data must be done

## Libraries
library(tidyverse); library(readxl); library(data.table)

## Read in data 
# Data inputs need to be in file called "data"
list.files("data") # should be 6 files

#####******************* Look at datasets - decide which can use ******************#################
###### (1) ATLAS
suppressWarnings(atlas <- read_csv("data/2023_06_15 atlas_antibiotics.csv"))
#warnings about column types, can be ignored
colnames(atlas) # metadata and antibiotic MIC eg age gender, source, country, in/out patient
unique(atlas$Year) # latest data from 2021 

table(atlas$Speciality)
table(atlas$Study)

### Explore antibiotic data 
unique(atlas$Amikacin)
# Some data are logicals: no MIC, remove
unique(atlas$Gatifloxacin)
unique(atlas$Tetracycline)
# Some are all NA: remove
unique(atlas$Gatifloxacin_I)
# Some are doubles: make characters for now
unique(atlas$`Quinupristin dalfopristin`)
atlas$`Quinupristin dalfopristin` <- as.character(atlas$`Quinupristin dalfopristin`)

# Pivot longer to explore ranges in MIC
# Ignores the specified drugs, melts the rest, removes NAs and adds a colum for data source
# NOTE! This melts in both the mic value, but also the categorisation (S/I/R etc). So many records in twice. Drops out later. 
atlas_clean <- atlas %>% select(-c("Gatifloxacin", "Gatifloxacin_I","Tetracycline")) %>% 
  pivot_longer(cols = `Amikacin`:`GIM`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic)) %>% mutate(data = "atls")

unique(atlas_clean$mic)
colnames(atlas_clean) <- tolower(colnames(atlas_clean))
atlas_clean <- rename(atlas_clean, "organism" = "species")
atlas_clean <- rename(atlas_clean, "age" = "age group")

###### (2) DREAM: Mtb 
dream <- readxl::read_excel("data/BEDAQUILINE DREAM DATASET FOR VIVLI - 06-06-2022.xlsx")
colnames(dream) # has MIC data and country, specimen metadata but not age / gender
dim(dream) # 5928
table(dream$Specimen) # vast majority (89%) sputum so would be hard to do sub analysis 
table(dream$SubType) # Resistance classification
## Due to lack of sub groupings EXCLUDE

##### (3) GSK
gsk <- read_csv("data/gsk_201818_published.csv")
colnames(gsk) # has age and gender, body location 
dim(gsk) # small: only 2413? 
table(gsk$COUNTRY) # Eastern Europe focus? 
table(gsk$ORGANISMNAME) # only Haemophilus influenzae and Strep pneumo but does have MIC 
table(gsk$BODYLOCATION) # lots sputum 
table(gsk$AGE)
table(gsk$GENDER)

# Pivot longer to explore ranges in MIC
gsk_clean <- gsk %>% #select(-c("Gatifloxacin", "Gatifloxacin_I","Tetracycline")) %>% 
  pivot_longer(cols = `AMOXICILLIN`:`TRIMETHOPRIM_SULFA`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic)) %>% mutate(data = "gsk8")

unique(gsk_clean$mic)
colnames(gsk_clean) <- tolower(colnames(gsk_clean))
gsk_clean <- rename(gsk_clean, "source" = "bodylocation")
gsk_clean <- rename(gsk_clean, "year" = "yearcollected")
gsk_clean <- rename(gsk_clean, "organism" = "organismname")

###### (4) Omadacycline
suppressWarnings(oma <- readxl::read_excel("data/Omadacycline_2014_to_2022_Surveillance_data.xlsx"))
# warning about column types, can supress as dealt with later
colnames(oma) # Age and Gender in there
head(oma)
dim(oma) # big: 83209
table(oma$Gender)
table(oma$Age) # good range but there are ages above 248? removed later
table(oma$`CF Patient`) # info on ~4000: 3738 CF patients
table(oma$Country) # Global
table(oma$Organism) # lots

### Explore antibiotic data 
unique(oma$Amikacin)
# Some data are logicals: no MIC, remove: 17! 
unique(oma$Oxacillin)
unique(oma$Ceftaroline)
unique(oma$Ceftriaxone)
unique(oma[,12])
unique(oma$Ampicillin)
unique(oma$Penicillin)

# Pivot longer to explore ranges in MIC
oma_clean <- oma %>% select(-c("Oxacillin","Ceftaroline","Ceftriaxone","Amoxicillin-\r\nclavulanic acid","Erythromycin","Clindamycin","Linezolid","Daptomycin",
                               "Vancomycin","Teicoplanin","Ampicillin","Azithromycin","Aztreonam","Ceftazidime","Colistin","Moxifloxacin","Penicillin")) %>% 
  pivot_longer(cols = `Omadacycline`:`Trimethoprim-sulfamethoxazole`, values_to = "mic", names_to = "antibiotic") %>%
  filter(!is.na(mic)) %>% mutate(data = "omad") %>%
  filter(Age < 120) # some odd year entries = birth date? 


unique(oma_clean$mic)
colnames(oma_clean) <- tolower(colnames(oma_clean))
oma_clean <- rename(oma_clean, "source" = "specimen type")
oma_clean <- rename(oma_clean, "year" = "study year")


###### (5) SIDERO
suppressWarnings(sidero <- readxl::read_excel("data/Updated_Shionogi Five year SIDERO-WT Surveillance data(without strain number)_Vivli_220409.xlsx"))
colnames(sidero) # No age and gender. Country, body location. 
head(sidero)
dim(sidero) # big: 47615 
table(sidero$Country) # Global
table(sidero$`Organism Name`) # lots

### Explore antibiotic data 
unique(sidero$Cefiderocol)
sidero$Cefiderocol <- as.character(sidero$Cefiderocol) # make characters to harmonise for now
sidero$Meropenem <- as.character(sidero$Meropenem) # make characters to harmonise for now
sidero$Ciprofloxacin <- as.character(sidero$Ciprofloxacin) # make characters to harmonise for now
sidero$Colistin <- as.character(sidero$Colistin) # make characters to harmonise for now
sidero$`Ceftazidime/ Avibactam` <- as.character(sidero$`Ceftazidime/ Avibactam`) # make characters to harmonise for now
sidero$`Ceftolozane/ Tazobactam`<- as.character(sidero$`Ceftolozane/ Tazobactam`) # make characters to harmonise for now
sidero$Cefepime<- as.character(sidero$Cefepime) # make characters to harmonise for now

# Pivot longer to explore ranges in MIC
sidero_clean <- sidero %>% pivot_longer(cols = `Cefiderocol`:`Imipenem/ Relebactam`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic), !mic == "NULL") %>% mutate(data = "sdro", age = NA, gender = NA) # add mock data for age and gender 

colnames(sidero_clean) <- tolower(colnames(sidero_clean))
sidero_clean <- rename(sidero_clean, "source" = "body location")
sidero_clean <- rename(sidero_clean, "year" = "year collected")
sidero_clean <- rename(sidero_clean, "organism" = "organism name")


###### (6) Venatorx
suppressWarnings(vena <- readxl::read_excel("data/Venatorx surveillance data for Vivli 27Feb2023.xlsx"))
colnames(vena) # Age and gender. Country, bodysite, facility
head(vena)
table(vena$BodySite)
table(vena$Facility)
dim(vena) # big: 24782
table(vena$Country) # Global
table(vena$Organism) # lots

### Explore antibiotic data 
unique(vena$CAZ_MIC)
unique(vena$CIP_MIC)
vena$CAZ_MIC <- as.character(vena$CAZ_MIC) # make characters to harmonise for now
vena$FEP_MIC <- as.character(vena$FEP_MIC) # make characters to harmonise for now
vena$GM_MIC <- as.character(vena$GM_MIC) # make characters to harmonise for now
vena$MEM_MIC <- as.character(vena$MEM_MIC) # make characters to harmonise for now
vena$TZP_MIC <- as.character(vena$TZP_MIC) # make characters to harmonise for now

# Pivot longer to explore ranges in MIC
vena_clean <- vena %>% pivot_longer(cols = `CAZ_MIC`:`TZP_MIC`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic), !mic == "-") %>% mutate(data = "vena")

unique(vena_clean$mic)
colnames(vena_clean) <- tolower(colnames(vena_clean))
vena_clean <- rename(vena_clean, "source" = "bodysite")


#### Combine data: only explore age / gender / country / body location 
col_use <- c("age","gender","source","year", "country","organism","antibiotic","mic","data")

########## Combine the datasets ########
full_data <- rbind(atlas_clean[,col_use],gsk_clean[,col_use], 
       vena_clean[,col_use],oma_clean[,col_use], sidero_clean[,col_use]) %>% 
   filter(!is.na(mic), !is.na(age), !is.na(gender), !gender == "N") %>% 
   mutate(organism_clean = "")

dim(full_data) # 24,523,575   

w_missing_data <- as.data.table(rbind(atlas_clean[,col_use],gsk_clean[,col_use], 
                   vena_clean[,col_use],oma_clean[,col_use]) )
nrow(w_missing_data[is.na(age)])/nrow(w_missing_data) *100
nrow(w_missing_data[is.na(gender)])/nrow(w_missing_data) *100

# Clean gender
unique(full_data$gender)
full_data$gender <- tolower(full_data$gender)
full_data$gender <- substr(full_data$gender, 1, 1)     
unique(full_data$gender)

# Clean year: no need
unique(full_data$year)

# Clean mic
unique(full_data$mic)
full_data$mic <- gsub('<', '', full_data$mic)
full_data$mic <- gsub('>', '', full_data$mic)
full_data$mic <- gsub('=', '', full_data$mic)
full_data$mic <- gsub('≤', '', full_data$mic)
full_data$mic <- gsub('<=', '', full_data$mic)
unique(full_data$mic)
# still many alphanumeric: as convert to as.numeric and then filter out NAs this should work? 
suppressWarnings(full_data$mic <- as.numeric(full_data$mic))
# suppressing warning as expect NAs!
full_data_cl <- full_data %>% filter(!is.na(mic)) 
100*dim(full_data_cl)[1] / dim(full_data)[1] # 45% of rows removed by filtering for numeric MIC

full_data <- full_data_cl

#### Clean organism for 4 top bugs for now
u <- unique(full_data$organism)
table(full_data$organism) %>% as.data.frame() %>% 
  arrange(desc(Freq)) 
## 4 have > 1.4M. Rest <<< 750K. 


# S. aureus
u[str_which(u, "aureus")] # yes
u[str_which(u, "Staph")] # too many: think above captures it 
full_data[which(full_data$organism == u[str_which(u, "aureus")]),"organism_clean"] <- "Staphylococcus aureus"

# E coli 
u[str_which(u, "coli")] # no too many others 
u[str_which(u, "E coli")] # none
u[str_which(u, "E. coli")] # none
u[str_which(u, "Escherichia")] # too many
full_data[which(full_data$organism == u[str_which(u, "Escherichia coli")]),"organism_clean"] <- "Escherichia coli"

# Klebsiella 
u[str_which(u, "Kleb")] # no too many others 
u[str_which(u, "kleb")] # none
u[str_which(u, "Klebsiella")] # lots
u[str_which(u, "pneumoniae")] # lots
full_data[which(full_data$organism == u[str_which(u,  "Klebsiella pneumoniae")]),"organism_clean"] <- "Klebsiella pneumoniae"

# P aeruginosa
u[str_which(u, "pseud")] # no too many others 
u[str_which(u, "aeru")] # none
u[str_which(u, "Pseud")] # lots
u[str_which(u, "P.")] # lots
full_data[which(full_data$organism == u[str_which(u,  "Pseudomonas aeruginosa")]),"organism_clean"] <- "Pseudomonas aeruginosa"


dim(full_data)
head(full_data)


### Clean age
full_data <- data.table(full_data)

# ATLAS data already in age_groups: move this over
full_data[, age_group := age]
# Make all ages numeric (this will make nas for atls but fine as already moved to age_group)
suppressWarnings(full_data[, age := as.numeric(age)] )
unique(full_data$age)
full_data[ !is.na(age), age_group := "0 to 2 Years"]
full_data[ age > 2, age_group := "3 to 12 Years"]
full_data[ age > 12, age_group := "13 to 18 Years"]
full_data[ age > 18, age_group := "19 to 64 Years"]
full_data[ age > 64, age_group := "65 to 84 Years"]
full_data[ age > 84, age_group := "85 and Over"]
full_data[age_group == "Unknown", age_group := NA]
full_data <- full_data[!is.na(age_group)]
unique(full_data$age_group)
full_data$age_group <- factor(full_data$age_group, 
                                 levels = c("0 to 2 Years","3 to 12 Years", "13 to 18 Years",
                                   "19 to 64 Years", "65 to 84 Years", "85 and Over"))

#### Source cleaning 
full_data <- full_data %>% mutate(key_source = "") # add new column for cleaned source data
full_data$source <- tolower(full_data$source)

### What is in there?  see source_cleaning.R for analysis
# u <- unique(full_data$source)
# tt <- table(full_data$source) %>% as.data.frame() %>% 
#   arrange(desc(Freq))  # easier to manipulate shorter dataframe for exploration of terms
# colnames(tt) <- c("source","freq")

### Key sources: 
## Urine / blood / respiratory / wound / gastro
# Could add reproduction / head (ear / eys) / heart 

# urine
full_data[str_which(full_data$source, "urine"),"key_source"] <- "urine"
full_data[str_which(full_data$source, "urinary"),"key_source"] <- "urine"
full_data[str_which(full_data$source, "urethra"),"key_source"] <- "urine"
full_data[which(full_data$source == "bladder"), "key_source"] <- "urine"
full_data[which(full_data$source == "ureter"), "key_source"] <- "urine"

# blood
full_data[str_which(full_data$source, "blood"),"key_source"] <- "blood"

# respiratory
full_data[str_which(full_data$source, "respiratory"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "lung"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "sputum"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "aspirate"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "sinus"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "trache"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "lavage"),"key_source"] <- "respiratory"
full_data[which(full_data$source == "bronchus"),"key_source"] <- "respiratory"
full_data[which(full_data$source == "pleural fluid"),"key_source"] <- "respiratory"
full_data[which(full_data$source == "bronchiole"),"key_source"] <- "respiratory"

# wound
full_data[str_which(full_data$source, "wound"),"key_source"] <- "wound"
full_data[str_which(full_data$source, "burn"),"key_source"] <- "wound"
full_data[str_which(full_data$source, "skin"),"key_source"] <- "wound"
full_data[str_which(full_data$source, "pus"),"key_source"] <- "wound"
full_data[str_which(full_data$source, "cellulitis"),"key_source"] <- "wound"
full_data[which(full_data$source == "abscess"),"key_source"] <- "wound"

### Gastrointestinal track 
full_data[str_which(full_data$source, "gi:"),"key_source"] <- "gastro"
full_data[str_which(full_data$source, "bowel"),"key_source"] <- "gastro"
full_data[str_which(full_data$source, "intestinal"),"key_source"] <- "gastro"
full_data[str_which(full_data$source, "gastric abscess"),"key_source"] <- "gastro"
full_data[str_which(full_data$source, "colon"),"key_source"] <- "gastro"


### Antibiotics tidy
full_data$antibiotic <- tolower(full_data$antibiotic)
abx <- unique(full_data$antibiotic) 
# rename the vena antibiotics (with _mic)
full_data[antibiotic == "caz_mic", antibiotic := "ceftazidime"]
full_data[antibiotic == "c_mic", antibiotic := "chloramphenicol"]
full_data[antibiotic == "cip_mic", antibiotic := "ciprofloxacin"]
full_data[antibiotic == "cl_mic", antibiotic := "colistin"]
full_data[antibiotic == "fep_mic", antibiotic := "cefepime"]
full_data[antibiotic == "gm_mic", antibiotic := "gentamicin"]
full_data[antibiotic == "ipm_mic", antibiotic := "imipenem"]
full_data[antibiotic == "lvx_mic", antibiotic := "levofloxacin"]
full_data[antibiotic == "mem_mic", antibiotic := "meropenem"]
full_data[antibiotic == "mi_mic", antibiotic := "minocycline"]
full_data[antibiotic == "sxt_mic", antibiotic := "trimethoprim sulfamethoxazole"]
full_data[antibiotic == "tim_mic", antibiotic := "ticarcillin-clavulanic acid"]
full_data[antibiotic == "tzp_mic", antibiotic := "piperacillin tazobactam"]
# check them
unique(full_data$antibiotic)
# rename some other weird ones
full_data[antibiotic == "piperacillin-\r\ntazobactam", antibiotic := "piperacillin tazobactam"]
full_data[antibiotic == "piperacillin-tazobactam", antibiotic := "piperacillin tazobactam"]
full_data[antibiotic == "ceftazidime/ avibactam", antibiotic := "ceftazidime avibactam"]
full_data[antibiotic == "ceftolozane/ tazobactam", antibiotic := "ceftolozane tazobactam"]
full_data[antibiotic == "imipenem/ relebactam", antibiotic := "imipenem relebactam"]
full_data[antibiotic == "ampicillin/ sulbactam", antibiotic := "ampicillin sulbactam"]
full_data[antibiotic == "aztreonam/ avibactam", antibiotic := "aztreonam avibactam"]
full_data[antibiotic == "meropenem/ vaborbactam at 8", antibiotic := "meropenem vaborbactam"]
full_data[antibiotic == "imipenem/ relebactam", antibiotic := "imipenem relebactam"]
full_data[antibiotic == "trimethoprim_sulfa", antibiotic := "trimethoprim sulfa"]
full_data[antibiotic == "trimethoprim-sulfamethoxazole", antibiotic := "trimethoprim sulfa"]
full_data[antibiotic == "trimethoprim/ sulfamethoxazole", antibiotic := "trimethoprim sulfa"]

### Focus
dim(full_data)
dim(full_data %>% filter(!organism_clean == ""))

# add income groups (world bank) and who regions
# NOTE: venezuela is unclassified on income group, have asssigned umic
income_grps <- as.data.table(read_csv("income.csv"))
who_regions <- as.data.table(read_csv("who-regions.csv"))
# match into full data
full_data[income_grps, on = "country", income_grp := income]
full_data[who_regions, on = "country", who_region := i.who_region]

#### output
write.csv(full_data %>% select(-age), "data/full_data.csv")


## some stats for the initial paper results
bacteria_to_use <- c("Staphylococcus aureus", "Escherichia coli", "Klebsiella pneumoniae", "Pseudomonas aeruginosa")
temp <- full_data[organism %in% bacteria_to_use]

data_props <- table(temp$data)
data_props[["atls"]] / sum(data_props)

region_props <- table(temp$who_region)
region_props/ sum(region_props)

gender_props <- table(temp$gender)
gender_props/ sum(gender_props)

age_props <- table(temp$age_group)
age_props/ sum(age_props)

year_props <- table(temp$year)
year_props/ sum(year_props)*100


props_plot <- table(temp[,c("age_group", "gender")])
props_plot <- data.table(props_plot)
props_plot$age_group <- factor(props_plot$age_group, levels = c(
  "0 to 2 Years", "3 to 12 Years", "13 to 18 Years", "19 to 64 Years", 
  "65 to 84 Years", "85 and Over"
)) 

props_plot$width <- rep(c(2,10,6, 45,20,15),2)

ggplot(props_plot, aes(x = age_group, y = N/width, fill = gender)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x = "Age group", y = "Number of samples by age-year", fill = "Sex") + 
  theme_linedraw() 
