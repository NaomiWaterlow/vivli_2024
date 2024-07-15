# run regressions
library(lme4)
library(data.table)

#TODO
# check that the model extract the random effects as well as the fixed ones for model 2
# add model 3

unique_bugs <- unique(dummy_data$bug)
unique_drugs <- unique(dummy_data$drug)
countries <- unique(dummy_data$country)

# loop over bugs and drugs
for(bug_specific in unique_bugs){
  for(drug_specfic in unique_drugs){
    
    print(paste0("model running is: ", bug_specific, " , ", drug_specific))
    
    # subset to drug and bug data
    data_subset <- dummy_data[drug == drug_specific & bug ==bug_specific, ]
    
    # Model 1
   Model_1 <-  glmer(resistance ~ age_group + sex + (1|country),
          data = data_subset, 
          family = "binomial")
   
   # extract data
   model_output_estimate <- unname(c(bug_specific, drug_specific, 1, "estimate", coef(summary(Model_1))[,"Estimate"]))
   model_output_sd <- unname(c(bug_specific, drug_specific, 1, "s_d", coef(summary(Model_1))[,"Std. Error"]))
   model_temp <- rbind(model_output_estimate, model_output_sd)
   if(drug_specific== unique_drugs[1] & bug_specific== unique_bugs[1]){
     model_1_output <- model_temp
     colnames(model_1_output) <- c("bug", "drug", "model", "output",names(coef(summary(Model_1))[,"Estimate"]) )} else{ 
     model_1_output <- rbind(model_1_output, model_temp)}
    
   #  # Model 2
   Model_2 <- glmer(resistance ~ age_group + sex + sex*country + (1|country),
          data = data_subset,
          family = "binomial")
   # extract data
   model_output_estimate <- unname(c(bug_specific, drug_specific, 2, "estimate", coef(summary(Model_2))[,"Estimate"]))
   model_output_sd <- unname(c(bug_specific, drug_specific, 2, "s_d", coef(summary(Model_2))[,"Std. Error"]))
   model_temp <- rbind(model_output_estimate, model_output_sd)
   
   if(drug_specific== unique_drugs[1] & bug_specific== unique_bugs[1]){
     model_2_output <- model_temp
     colnames(model_2_output) <- c("bug", "drug", "model", "output",names(coef(summary(Model_2))[,"Estimate"]) )} else{ 
       model_2_output <- rbind(model_2_output, model_temp)}
    
    # # Model 3 
    # glmer(resistance ~ age_group + sex + sex*country + (births + 1|country), 
    #       data = data_subset, 
    #       family = "binomial")
   
  }
}


model_1_output
model_2_output