#what about brms version so we can quantify uncertainty?
library(brms)
library(data.table)


input_data <- data.table(read.csv("data/combined_atlas.csv"))


unique_bugs <- unique(input_data$species)
unique_drugs <- unique(input_data$antibiotic)
countries <- unique(input_data$country)

model_list <- list()

input_data$age <- factor(input_data$age, levels = c("0 to 2 Years" , 
                                                    "3 to 12 Years", 
                                                    "13 to 18 Years",
                                                    "19 to 64 Years", 
                                                    "65 to 84 Years",
                                                    "85 and Over"))

input_data$c_section <- as.numeric(input_data$c_section)
bug_specific <- unique_bugs[1]
drug_specific <- unique_drugs[1]

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
    
    
    # Model 1
    Model_0 <-  brm(mic_label ~ 1 + (1|country),
                      data = data_subset, 
                      family = "binomial")
    
    print("Model 0 complete")
    
    # Model 1
    Model_1 <-  brm(mic_label ~ 1 +age + gender  + (1|country),
                      data = data_subset, 
                      family = "binomial")
    
    print("Model 1 complete")
    # Model 2
    Model_2 <- brm(mic_label ~ age + gender + (1 + gender|country) ,
                     data = data_subset,
                     family = "binomial")
    
    # In this sample, adjusting for the impact of age, the odds of gender per country is.
    # We can calculate a country specific odds ratio of gender, but not an overall one. 
    # Want an uncertainty around it. 
    
    print("Model 2 complete")
    
    
    # Model 3 # need to add country level factors! 
    Model_3 <- brm(mic_label ~ age + gender  + c_section + birth_rate  + (1  + gender|country) ,
                     data = data_subset,
                     family = "binomial")
    # e.g. adjusting for birth rate as well, does  country still have an impact?
    
    print("Model 3 complete")
    
    # Model 4 - want the effect of gender to be able to be different with age - interaction?
    # Model_4 <- glmer(mic_label ~ age + gender + age*gender + c_section + birth_rate  + (1  + gender|country) ,
    #                  data = data_subset,
    #                  family = "binomial")
    
    # Model 5 - only age:gender interaction for reproductive age
    # 
    # this gives warning that it's rank deficient. I thin because reproductive
    # and age are subsets of one another?
    # 
    # data_subset[age %in% c("65 to 84 Years", "85 and Over", 
    #                        "13 to 18 Years", "3 to 12 Years", "0 to 2 Years"), 
    #             reproductive := 0]
    # data_subset[age %in% c("19 to 64 Years"), reproductive := 1]
    # 
    # Model_5 <- glmer(mic_label ~ age + gender + reproductive:gender + c_section + birth_rate  + (1  + gender|country) ,
    #                  data = data_subset,
    #                  family = "binomial")
    # 
    
    name_use <- paste0(bug_specific, " - ", drug_specific)
    
    model_list[[name_use]] <- list(Model_0, Model_1, Model_2, Model_3) 
    
    saveRDS(model_list, file = paste0("BRMS_Models_",drug_specific,"_",bug_specific,".RDS"))
  }
}


saveRDS(model_list, file = "BRMS_Models.RDS")
