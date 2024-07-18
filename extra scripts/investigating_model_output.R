# testing model output

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


# Use model generated from other script
staph_levo <- model_list$`Staphylococcus aureus - levofloxacin`
Model_0 <- staph_levo[[1]]
Model_1 <- staph_levo[[2]]
Model_2 <- staph_levo[[3]]
Model_3 <- staph_levo[[4]]

drug_specific <- "levofloxacin"
bug_specific <- "Staphylococcus aureus"
data_subset <- input_data[species == bug_specific & 
                            antibiotic == drug_specific]

#only include those countries with 1000 or more samples
# and remove uknown age
data_subset[, country_total := .N, by = "country"]
data_subset <- data_subset[country_total>1000 & age != "Unknown",]


##### Use model 0 to look at the basic variation ####

# Country alone explains what % of the variation
# Compute and display the VPC=ICC statistic
rpm1 <- as.data.frame(VarCorr(Model_0))
# extracts the estimated country variance
rpm1
rpm1$vcov[rpm1$grp == "country"] / (rpm1$vcov[rpm1$grp == "country"] + pi^2/3)
# Note that pi^2/3  is the residual variance in the latent response
# formulation of the model..... I THINK! (i.e. because this is a logisitic model)
# ---> X% of the variation is explained by the country level random effects...

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

# QQ plot 
ggplot(u0, aes(sample = condvalstd)) + 
  stat_qq() + 
  stat_qq_line()

###### Plot a caterpillar plot of the country effects (not accounting for anything as still using model 0)

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


##### Now model 2 ##### 
# Adjusting for age, what is the impact of gender by country
# Both in intercept and in slope by country.


fixef(Model_2) 
exp(fixef(Model_2)[1])# exp to convert from log odds to odds... SIMON CHECK
# the average intercept across countries is 0.13
fixef(Model_2)["genderm"]
# the slope in every country is 0.129 for males
# and age slope is also the same in every country



# Plot predicted country lines
# Extract predicted probabilities
data_subset$m2_logodds <- predict(Model_2)
data_subset[, m2_odds := exp(m2_logodds)]
data_subset$m2_probpred <- inv.logit(predict(Model_2))
# The predict() function stores the fitted values on the log-odds scale. The
# inv.logit() function tranlates these to the probability scale.


# ggplot(data_subset[gender == "f"], aes(x = age, y = m2_odds, colour = country)) + 
#   geom_point() + theme(legend.position = "None")

ggplot(data_subset, aes(x = gender, y = m2_odds, colour = age)) + 
  geom_point() + theme(legend.position = "None") + 
  facet_grid(.~country)


##### Model 3 ####

#check deviance to see if Model is better htna model 2/
deviance(Model_2) - deviance(Model_3) # (but more parameters so need to look at degrees of freedom). 
# 2 extra added parameters... 
#so has to be bigger than 5.99... way bigger!
# i.e. model 3 is much better than model 2

# Display confidence intervals for the model parameters
confint(Model_3)

lrt(Model_3, Model_2)
u01m6 <- data.frame(ranef(Model_3), condVar = TRUE)
u01m6


# look at model3 too - why does this have a different number of rows?
# The model must have dropped some
data_subset$m3_logodds <- predict(Model_3)
data_subset[, m3_odds := exp(m3_logodds)]
data_subset$m3_probpred <- inv.logit(predict(Model_3))



# calculate the proportion of country variance that is explained by birth weight
rpm2 <- VarCorr(Model_2)
rpm3 <- VarCorr(Model_3)




## Question is, does including birth rate etc. in Model 3 reduce the country differences??
# But this I guess would look the same on the plots - as predicting based on the birth rates etc.
# so have to look at the parameter values
# I think, does it reduce the random effect variance? <--- SIMON CHECK 



m2_ranefs <- ranef(Model_2)$country
m2_ranefs$Model <- "Model 2"
m2_ranefs$Country <- rownames(m2_ranefs)
m3_ranefs <- ranef(Model_3)$country
m3_ranefs$Model <- "Model 3"
m3_ranefs$Country <- rownames(m3_ranefs)

ranef_comp <- rbind(m2_ranefs, m3_ranefs)
colnames(ranef_comp) <- c("Intercept", "GenderM", "Model", "Country")

ranef_comp <- data.table(ranef_comp)
ranef_comp_m <- melt.data.table(ranef_comp, id.vars = c("Model", "Country"))

ggplot(ranef_comp_m, aes(x = Country, y = abs(value), colour = Model)) + 
  geom_point() + 
  facet_grid(variable~., scales = "free_y") + 
  theme_bw() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# For this example, looks like Model 3 mostly reduces the absolute values

fixef(Model_2)
fixef(Model_3)



