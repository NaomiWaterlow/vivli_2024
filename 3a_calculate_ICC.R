####### August 2024 ######################################################################
## Authors: Naomi Waterlow , Alastair Clements, Chaelin Kim, Simon Procter, Gwen Knight ##
##########################################################################################


# calculate ICC# run regressions
library(lme4)
library(data.table)

# load data
input_data <- data.table(read.csv("data/combined_atlas.csv"))

#specify subset
unique_bugs <- unique(input_data$species)
unique_drugs <- unique(input_data$antibiotic)
countries <- unique(input_data$country)

# formatting
input_data$age <- factor(input_data$age, levels = c("0 to 2 Years" , 
                                                    "3 to 12 Years", 
                                                    "13 to 18 Years",
                                                    "19 to 64 Years", 
                                                    "65 to 84 Years",
                                                    "85 and Over"))

input_data$c_section <- as.numeric(input_data$c_section)

# loop over bugs 
for(bug_specific in unique_bugs){
  
  # subset to bug data
  data_subset_top <- input_data[species ==bug_specific, ]
  
  # loop over drugs
  for(drug_specific in unique(data_subset_top$antibiotic)){
    
    # print for tracking
    print(paste0("model running is: ", bug_specific, " , ", drug_specific))
    
    #subset to drug data
    data_subset <- data_subset_top[antibiotic ==drug_specific, ]
    
    #only include those countries with 1000 or more samples
    # and remove uknown age
    data_subset[, country_total := .N, by = "country"]
    data_subset <- data_subset[country_total>1000 & age != "Unknown",]
    
    
    # Model 0
    Model_0 <-  glmer(mic_label ~ 1 + (1|country),
                      data = data_subset, 
                      family = "binomial")
    
    
    # Compute and display the VPC=ICC statistic (i.e. how much variation within and between countries)
    rpm1 <- as.data.frame(VarCorr(Model_0))
    # extracts the estimated country variance
    rpm1
    var0 <- rpm1$vcov[rpm1$grp == "country"] / (rpm1$vcov[rpm1$grp == "country"] + pi^2/3)
    # Country variation alone explains 0.1657993 % of the total variation in the sample 
    print(paste0("For ", bug_specific, ", ", drug_specific," " ,var0*100, "% of total variation in the sample is explained by country level variation" ))
    
    
  }}
