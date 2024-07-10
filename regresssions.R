library(data.table)
library(lmtest)
library(lme4)

dummy_data <- data.table(
  resistance = c(0,1,0,1,1,1), 
  sex = c("f","f","f","f","f","m"), 
  age_group= c(1,2,3,4,5,5), 
  bug = c("a", "a", "b", "c", "c", "c"),
  drug = c("x", "y", "z", "x", "y", "z"), 
  country = c("alpha", "beta","alpha", "beta","alpha", "beta")
)

dummy_data

# Model 1
# assumes a global effect of sex, not for it to vary by country (intercept varies by country)
model1 <- glm(resistance~sex+age_group+bug+drug+country, data = dummy_data, 
             family = "binomial")

# Model 2
# allows effect of sex to vary by country (i.e. intercept and slope)
model2 <- glm(resistance~sex+age_group+bug+drug+country+(sex*country), data = dummy_data, 
             family = "binomial")

# Is the model with interaction better than the one without
# I.e. is there stat evidence for interaction
lrtest(model1,model2)


# but actually we want it to be multi level 

# Model 1
glmer(resistance ~ age_group + sex + (1|country),
      data = dummy_data, 
      family = "binomial")

# Model 2
glmer(resistance ~ age_group + sex + sex*country + (1|country),
      data = dummy_data, 
      family = "binomial")

# Model 3 
glmer(resistance ~ age_group + sex + sex*country + (births + 1|country), 
      data = dummy_data, 
      family = "binomial")





