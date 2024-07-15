# run regressions
library(lme4)
library(data.table)


input_data <- data.table(read.csv("data/combined_atlas.csv"))


unique_bugs <- unique(input_data$species)
unique_drugs <- unique(input_data$antibiotic)
countries <- unique(input_data$country)

model_list <- list()

# loop over bugs 
for(bug_specific in unique_bugs){
   
   # subset to bug data
   data_subset <- input_data[species ==bug_specific, ]
   
   # loop over drugs
  for(drug_specific in unique(data_subset$antibiotic)){
    
     # print for tracking
    print(paste0("model running is: ", bug_specific, " , ", drug_specific))
     
     #subset to drug data
     data_subset <- data_subset[antibiotic ==drug_specific, ]

     # Model 1
     Model_0 <-  glmer(mic_label ~ 1 + (1|country),
                       data = data_subset, 
                       family = "binomial")
     
     print("Model 0 complete")
         
   # Model 1
   Model_1 <-  glmer(mic_label ~ 1 +age + gender + (1|country),
          data = data_subset, 
          family = "binomial")
   
   print("Model 1 complete")
   # Model 2
   Model_2 <- glmer(mic_label ~ age + gender + gender*country + (1|country),
          data = data_subset,
          family = "binomial")
   
   print("Model 2 complete")
   # Model 3 # need to add country level factors! 
   Model_3 <- glmer(mic_label ~ age + gender + gender*country + (1 + birth_rate + c_section + GDP + primary_completion_female_over_male|country) ,
                    data = data_subset,
                    family = "binomial")
   print("Model 3 complete")
   
   name_use <- paste0(bug_specific, " - ", drug_specific)
   
   model_list[[name_use]] <- list(Model_0, Model_1, Model_2, Model_3) 
   
   
  }
}


saveRDS(model_list, file = "Models.RDS")
