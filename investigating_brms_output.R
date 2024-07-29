# load packages
library(data.table)
library(ggplot2)
library(brms)
library(boot)

# Read in models a


# load packages
library(data.table)
library(ggplot2)
library(lme4)
library(boot)

# Read in models and data
model_list <- readRDS("BRMS_Models.RDS")
input_data <- data.table(read.csv("data/combined_atlas.csv"))

# check format
input_data$age <- factor(input_data$age, levels = c("0 to 2 Years" , 
                                                    "3 to 12 Years", 
                                                    "13 to 18 Years",
                                                    "19 to 64 Years", 
                                                    "65 to 84 Years",
                                                    "85 and Over"))
input_data$c_section <- as.numeric(input_data$c_section)

# extract models (numbering matches the models un run_regressions.R)
models_together <- model_list$`Escherichia coli - ampicillin`
models_together <- model_list$`Staphylococcus aureus - levofloxacin`

Model_0 <- models_together[[1]]
Model_1 <- models_together[[2]]
Model_2 <- models_together[[3]]
Model_3 <- models_together[[4]]
#Model_4 <- models_together[[5]]
#Model_5 <- models_together[[6]]

#specify data subset
drug_specific <- "levofloxacin" # "ampicillin"
bug_specific <- "Staphylococcus aureus" # "Escherichia coli"
data_subset <- input_data[species == bug_specific & 
                            antibiotic == drug_specific]

#only include those countries with 1000 or more samples
# and remove Unknown age
data_subset[, country_total := .N, by = "country"]
data_subset <- data_subset[country_total>1000 & age != "Unknown",]
















