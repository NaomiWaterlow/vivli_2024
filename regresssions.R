library(data.table)

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
model <- glm(resistance~sex+age_group+bug+drug+country, data = dummy_data, 
             family = "binomial")

# Model 2
# allows effect of sex to vary by country (i.e. intercept and slope)
model <- glm(resistance~sex+age_group+bug+drug+country+(sex*country), data = dummy_data, 
             family = "binomial")





