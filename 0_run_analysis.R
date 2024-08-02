# Main workflow

#libraries needed
library(data.table)
library(ggplot2)
library(lme4)
library(boot)
library(gridExtra)

#### LOAD AND CLEAN ####

#Atlas data cleaning
source("1a_preparation_data_cleaning_atlas.R")
#load in covariate data
source("1b_other_data.R")
# combine covariate and resistance data
source("1c_Combine_datasets.R")

###### DESCRIPTIVE #####

# run descriptive stats
source("2a_DescriptiveStat.R")
# create maps
source("2b_map.R")

###### MODELLING #####

# calculate the intraclass correlation coefficients
source("3a_calculate_ICC.R")

### RUN THE MODELS - NOTE, This takes a long time!! 
# However the pre-run models are in the github, so the subsequent scripts should work anyway.
#source("3b_regressions_brms.R")

# model output
source("3c_investigating_brms_output.R")
# compare across bug-drugs
source("3d_comparison_bug_drug.R")
