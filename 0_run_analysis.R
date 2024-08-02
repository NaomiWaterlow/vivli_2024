############ Master script ###############################################################
####### August 2024 ######################################################################
## Authors: Naomi Waterlow , Alastair Clements, Chaelin Kim, Simon Procter, Gwen Knight ##
##########################################################################################


# Main workflow

#libraries needed
library(data.table)
library(ggplot2)
library(brms)
library(boot)
library(sjstats)
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
# this is run for each model (1-4) individually
model_to_run <- 1
#this script also requires 1c_Combine_datasets.R" to have been run previously
source("3c_investigating_brms_output.R")
# compare across bug-drugs
source("3d_comparison_bug_drug.R")
