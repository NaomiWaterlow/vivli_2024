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
input_data$gender <- as.factor(input_data$gender)
bug_specific <- unique_bugs[2]
drug_specific <- unique_drugs[1]



#  Need to put in the number of trials, so will calculate

# # loop over bugs 
# for(bug_specific in unique_bugs){
  
  # subset to bug data
  data_subset_top <- input_data[species ==bug_specific, ]
  
  # loop over drugs
  # for(drug_specific in unique(data_subset_top$antibiotic)){
    
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
                    #family = binomial(link = "logit"),
                   family = bernoulli(link="logit"),
                    file = paste0("fits/",bug_specific,"_",drug_specific, "_Model0"), 
                  #  sample_prior = "only",
                    file_refit = getOption("brms.file_refit", "always"),
                    iter = 5000, save_pars = save_pars(all = TRUE))
    
    Model_0 <- add_criterion(Model_0, criterion = "loo_subsample", 
                  file = paste0("fits/",bug_specific,"_",drug_specific, "_Model0"))
    
    print("Model 0 complete")
    
    # Model 1
    Model_1 <-  brm(mic_label ~ 1 +age + gender  + (1|country),
                      data = data_subset, 
                    family = bernoulli(link="logit"), 
                    file = paste0("fits/",bug_specific,"_",drug_specific, "_Model1"), 
                    file_refit = getOption("brms.file_refit", "always"),
                    iter = 5000, save_pars = save_pars(all = TRUE))
    
    Model_1 <- add_criterion(Model_1, criterion = "loo_subsample", 
                  file = paste0("fits/",bug_specific,"_",drug_specific, "_Model1"))
    
    print("Model 1 complete")
    # Model 2
    Model_2 <- brm(mic_label ~ age + gender + (1 + gender|country) ,
                     data = data_subset,
                   family = bernoulli(link="logit"), 
                   file = paste0("fits/",bug_specific,"_",drug_specific, "_Model2"), 
                   file_refit = getOption("brms.file_refit", "always"),
                   iter = 5000, save_pars = save_pars(all = TRUE))
    
    Model_2 <- add_criterion(Model2, criterion = "loo_subsample", 
                             file = paste0("fits/",bug_specific,"_",drug_specific, "_Model2"))
    # In this sample, adjusting for the impact of age, the odds of gender per country is.
    # We can calculate a country specific odds ratio of gender, but not an overall one. 
    # Want an uncertainty around it. 
    
    print("Model 2 complete")
    
    
    # Model 3 # need to add country level factors! 
    Model_3 <- brm(mic_label ~ age + gender + c_section*gender + birth_rate*gender  + (1  + gender|country) ,
                   data = data_subset,
                   family = bernoulli(link="logit"), 
                   file = paste0("fits/",bug_specific,"_",drug_specific, "_Model3"), 
                   file_refit = getOption("brms.file_refit", "always"),
                   iter = 5000, save_pars = save_pars(all = TRUE))
    
    Model_3 <- add_criterion(Model_3, criterion = "loo_subsample", 
                             file = paste0("fits/",bug_specific,"_",drug_specific, "_Model3"))
# e.g. adjusting for birth rate as well, does  country still have an impact?
    
    print("Model 3 complete")
    
    name_use <- paste0(bug_specific, " - ", drug_specific)
    
    model_list[[name_use]] <- list(Model_0, Model_1, Model_2, Model_3) 
    
    saveRDS(model_list, file = paste0("BRMS_Models_",drug_specific,"_",bug_specific,".RDS"))
#   }
# }


# saveRDS(model_list, file = "BRMS_Models.RDS")
