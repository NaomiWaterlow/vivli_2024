# testing model output

Model_1 <-  glmer(mic_label ~ 1  + (1|country),
                  data = data_subset, 
                  family = "binomial")




# Use model generated from other script
Model_1

# look at summary
summary(Model_1)


# Compute and display the VPC=ICC statistic
rpm1 <- as.data.frame(VarCorr(Model_1))
rpm1
# extracts the estimated country variance
rpm1
rpm1$vcov[rpm1$grp == "country"] / (rpm1$vcov[rpm1$grp == "country"] + pi^2/3)
# Note that pi^2/3  is the residual variance in the latent response
# formulation of the model..... I THINK!
# ---> 21% of the variation is explained by the country level random effects...

# eztract the random effects
u0 <- as.data.frame(ranef(Model_1))
head(u0)
str(u0)
# u0 is a new data frame with one row per country
#
# The variable condval contains the estimated random intercepts, while the
# variable condsd contains the estimated standard errors for the random
# intercepts.

# Standardise the country effects (i.e., generate the z-scores for the country
# effects).
u0$condvalstd <- scale(u0$condval)
head(u0)
# The scale function rescales u0$condval to have mean 0 and SD 1.

# Plot a histogram of the standardised country effects and superimpose an
# appropriately scaled normal density in order to assess the normality
# assumption
ggplot(u0, aes(x = condvalstd)) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(u0$condvalstd), sd = sd(u0$condvalstd)))

# QQ plot 
ggplot(u0, aes(sample = condvalstd)) + 
  stat_qq() + 
  stat_qq_line()


###### Plot a caterpillar plot of the country effects
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
  geom_point()


# Model 2 
Model_2 <-  glmer(mic_label ~ 1 +age + gender + (1|country),
                  data = data_subset, 
                  family = "binomial")


# Plot predicted state lines

# Extract predicted probabilities
data_subset$m2probpred <- inv.logit(predict(Model_2))
# The predict() function stores the fitted values on the log-odds scale. The
# inv.logit() function tranlates these to the probability scale.

ggplot(data_subset[gender == "f"], aes(x = age, y = m2probpred, colour = country)) + 
  geom_point() + theme(legend.position = "None")

ggplot(data_subset[gender == "f"], aes(x = country, y = m2probpred, colour = age)) + 
  geom_point() + theme(legend.position = "None")




