

# I've written my thoughts on the analysis in comments on this script. 
# Please check carefully as I'm not certain on it. 
# When I'm particularly uncertain I've added "???" on the line before. 
# I've also put the slide number of the multi-level modelling course in relevant bits, in case helpuful

##### Setup #####

# load packages
library(data.table)
library(ggplot2)
library(lme4)
library(boot)

# Read in models and data
model_list <- readRDS("Models.RDS")
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

#specify data subset
drug_specific <- "levofloxacin"
bug_specific <- "Staphylococcus aureus"
data_subset <- input_data[species == bug_specific & 
                            antibiotic == drug_specific]

#only include those countries with 1000 or more samples
# and remove Unknown age
data_subset[, country_total := .N, by = "country"]
data_subset <- data_subset[country_total>1000 & age != "Unknown",]


##### Use model 0 to look at the basic variation ####

# Compute and display the VPC=ICC statistic (i.e. how much variation within and between countries)
rpm1 <- as.data.frame(VarCorr(Model_0))
# extracts the estimated country variance
rpm1
rpm1$vcov[rpm1$grp == "country"] / (rpm1$vcov[rpm1$grp == "country"] + pi^2/3)
# Country variation alone explains 0.1657993 % of the total variation in the sample 
# ???
# Note that pi^2/3  is the residual variance in the latent response formulation of the model..... I THINK! 
#(i.e. because this is a logisitic model)
# slide number 289, page 73

# extract the random effects
u0 <- as.data.frame(ranef(Model_2))
head(u0)
str(u0)
# The variable condval contains the estimated random intercepts, while the
# variable condsd contains the estimated standard errors for the random
# intercepts.

# Standardise the country effects (i.e., generate the z-scores for the country effects).
u0$condvalstd <- scale(u0$condval)
head(u0)

# Plot a histogram of the standardised country effects and superimpose an
# appropriately scaled normal density in order to assess the normality
# assumption
ggplot(u0, aes(x = condvalstd)) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(u0$condvalstd), sd = sd(u0$condvalstd)))
# Okay


###### Plot a caterpillar plot of the country effects (not accounting for anything as still using model 0) ####

# calculate the range
u0$lower<-u0$condval - 1.96*u0$condsd
u0$upper<-u0$condval + 1.96*u0$condsd
head(u0)

# then rank them (for good plot)
u0$rank = rank(u0$condval)
head(u0)
#
# Generate caterpillar plot - shows the standardised variation by country. 
ggplot(u0, aes(x = rank, y = condval, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point() + 
  geom_text(aes(label = grp), vjust=5, hjust = -0.1)
# this shows the variation (in intercept?) by country

##### Ajusting for age and gender (Model 1) ##### 

# Adjusting for age, what is the impact of gender by country
# Gender acts both in the intercept and in slope by country.

# approximation of confidence intervals for fixed effects. 
se <- sqrt(diag(vcov(Model_1)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(Model_1), LL = fixef(Model_1) - 1.96 * se,
              UL = fixef(Model_1) + 1.96 *se)
# but this on logit scale and we want as odds ratios
exp(tab)
# clear differences by age, with increasing odds of resistance with age
# also effect of gender, with higher odds of resistance in males

deviance(Model_0) - deviance(Model_1)
# big reduction in deviance implies the model is better
# run Anova (correct?) to check that its statstically bigger, given mroe parameters
anova(Model_0, Model_1)
# Yes, two is better. So need to adjust for age and gender
 

##### Model 2 - does gender slope vary by country? ####

#lets take a look at teh randome ffects
# (this function seems to do pretty much the same as the catepillar plot)
lattice::dotplot(ranef(Model_2, whichel = "country", condVar = T), 
                 scales = list(y=list(alternating = 0)))


# can't consider the fixed effects alone for gender, as it's also a covariate in the random effects
# but age should still be meaninful

se <- sqrt(diag(vcov(Model_2)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(Model_2), LL = fixef(Model_2) - 1.96 * se,
             UL = fixef(Model_2) + 1.96 *se)
exp(tab)
 # so gender shouldnt really be considered like this... as shows the average intercept,
# but the slope varies by country. But age is still an odds ration

# check if this model is better (same logic as above)
deviance(Model_1) - deviance(Model_2)
anova(Model_1, Model_2) #??? I assume you can do this with random effect slopes?

# Want to have a look at how gender 'slope' varies by country

# Extract predicted log odds
data_subset$m2_logodds <- predict(Model_2)
# And the predicted probability
data_subset$m2_probpred <- inv.logit(predict(Model_2))

# This shows the predicted probability 'slope' by age group
# each lines represents a country
ggplot(data_subset, aes(x = age, y = m2_probpred,  group = interaction(country,gender), colour = country)) + 
  geom_line() + #theme(legend.position = "None") + 
  facet_grid(.~gender, scales ="free_y")

# each lines represents a country
# this version is slightly odd because it's a 'slope' between f and m
# but it shows how the slope differs by country. 
ggplot(data_subset, aes(x = gender, y = m2_probpred,  group = interaction(country,age), colour = country)) + 
  geom_line() +# theme(legend.position = "None") + 
  facet_grid(.~age, scales ="free_y")
# it's very varied -> mostly the gender slope incrase from f-> m in countries. 
# i.e. being male makes you more likely to test for resistance
# however in some countries it's the opposite, i.e. in those countries with negative slope
# being female makes you more likely to be resistant

###### Model 3 - is any of this difference in slope explained by the country level factors?


#lets take a look at teh randome ffects
lattice::dotplot(ranef(Model_3, whichel = "country", condVar = T), 
                 scales = list(y=list(alternating = 0)))


# can't consider the fixed effects alone for gender, as it's also a covariate in the random effects
# but age should still be meaninful

se <- sqrt(diag(vcov(Model_3)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(Model_3), LL = fixef(Model_3) - 1.96 * se,
             UL = fixef(Model_3) + 1.96 *se)
exp(tab)

# these are the odds ratios for ages, and the country level variables
# looks like c_section rate has no significant effect, but birth rate does

# check if this model is better (same logic as above)
# this doesn't work, as the dataset is different length
# I assume it must drop ones with NA
# which are these countries 
#"Greece"         "United Kingdom" "Kuwait"         "South Africa"   "Taiwan"  
#anova(Model_2, Model_3) 

 #For now, drop the records for these countries for further analysis 
# (Maybe we want to just exclude them from all the models in the long run)

data_subset <- data_subset[!(country %in% c("Greece","United Kingdom", "Kuwait","South Africa","Taiwan")),]

# Want to have a look at how gender 'slope' varies by country

# Extract predicted log odds
data_subset$m3_logodds <- predict(Model_3)
# And the predicted probability
data_subset$m3_probpred <- inv.logit(predict(Model_3))

# each lines represents a country
# this version is slightly odd because it's a 'slope' between f and m
# but it shows how the slope differs by country. 
ggplot(data_subset, aes(x = gender, y = m3_probpred,  group = interaction(country,age), colour = country)) + 
  geom_line() +# theme(legend.position = "None") + 
  facet_grid(.~age, scales ="free_y")
# oooh. From first glance it looks like fewer negative slopes... (although these may be the missing countries)
# want to be able to compare slopes from Model2 vs Model3
data_subset[, c("antibiotic", "data", "species", "GDP", "primary_completion_female_over_male", "country_total") := NULL]
comparison_data <- melt.data.table(data_subset, id.vars = c("age", "gender", "year", "country", "mic_label", 
                                         "birth_rate", "c_section"))
 
comparison_data[, model:=  tstrsplit(variable, "_", fixed = TRUE)[1]]
comparison_data[, type:=  tstrsplit(variable, "_", fixed = TRUE)[2]]

# Can I plot the same but from both models in a useful way? 
ggplot(comparison_data[type == "probpred"], aes(x = gender, y = value, 
                                              colour = country, linetype = model, 
                                              group = interaction(country, model))) + 
  geom_line() +# theme(legend.position = "None") + 
  facet_grid(.~age, scales ="free_y")
#this shows that in some countries there is a small impact of including these variables. 
# but little. 

# ideally want a metric of how much of the variation it explains... 

