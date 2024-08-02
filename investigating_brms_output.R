# load packages
library(data.table)
library(ggplot2)
library(brms)
library(boot)
library(sjstats)
library(gridExtra)

model_to_run <- 2

# Read in models and data
model_files <- list.files(pattern = "BRMS_Models")
model_list <- readRDS(model_files[model_to_run])
input_data <- data.table(read.csv("data/combined_atlas.csv"))
name_to_run <- names(model_list)

#specify data subset
bug_specific <- strsplit(name_to_run, "-")[[1]][1]
drug_specific <- strsplit(name_to_run, "-")[[1]][2]
bug_specific <- trimws(bug_specific)
drug_specific <- trimws(drug_specific)


# check format
input_data$age <- factor(input_data$age, levels = c("0 to 2 Years" , 
                                                    "3 to 12 Years", 
                                                    "13 to 18 Years",
                                                    "19 to 64 Years", 
                                                    "65 to 84 Years",
                                                    "85 and Over"))
input_data$c_section <- as.numeric(input_data$c_section)

# extract models (numbering matches the models un run_regressions.R)

Model_0 <- model_list[[1]][[1]]
Model_1 <- model_list[[1]][[2]]
Model_2 <- model_list[[1]][[3]]
Model_3 <- readRDS(paste0("fits/", bug_specific, "_",drug_specific, "_Model4.rds"))


#specify data subset
data_subset <- input_data[species == bug_specific & 
                            antibiotic == drug_specific]

#only include those countries with 1000 or more samples
# and remove Unknown age
data_subset[, country_total := .N, by = "country"]
data_subset <- data_subset[country_total>1000 & age != "Unknown",]


num_countries <- unique(data_subset$country)
num_ages <- unique(data_subset$age)


test_data <- data.table(age = rep(num_ages,2*length(num_countries)), 
                        country = rep(rep(num_countries, each=length(num_ages)),2), 
                        gender = rep(c("m", "f"), each=(length(num_countries)*length(num_ages))))


##!!! Need to run Combine_datas.R for this to work!! (sorry, messy)
# (that script will have a warning about NAs, which is not a problem)
test_data[country_data, on = c("country"), birth_rate := i.birth_rate]
test_data[country_data, on = c("country"), c_section := i.c_section]
test_data[is.na(c_section), c_section := mean_csection]
test_data[is.na(birth_rate), birth_rate := mean_birth]
test_data$birth_rate <- as.numeric(test_data$birth_rate)
test_data$c_section <- as.numeric(test_data$c_section)



test_data0 <- copy(test_data)
test_data0[, Model := 0]
test_data0 <- cbind(test_data0, fitted(Model_0, newdata=test_data0))

country_order <- test_data0[age == "19 to 64 Years" & gender == "f", c("country", "Estimate")]
country_order <- country_order[order(Estimate)]$country

test_data1 <- copy(test_data)
test_data1[, Model := 1]
test_data1 <- cbind(test_data1, fitted(Model_1, newdata=test_data1))

test_data2 <- copy(test_data)
test_data2[, Model := 2]
test_data2 <- cbind(test_data2, fitted(Model_2, newdata=test_data2))

test_data3 <- copy(test_data)
test_data3[, Model := 3]
test_data3 <- cbind(test_data3, fitted(Model_3, newdata=test_data3))


test_data_all <- rbind(test_data0, test_data1, test_data2, test_data3
                   )


test_data_all[,Model := factor(Model)]
test_data_all[, country := factor(country, levels = country_order)]

MODEL_COMPARISON_SPLIT <- ggplot(test_data_all[age == "19 to 64 Years"], aes(x = country, y = Estimate, colour = Model)) + 
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5),  position = position_dodge(width = 0.4)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(age~gender) +
  labs(x = "County", y = "Predicted Probability", 
       title = paste0(bug_specific, ", ", drug_specific)) 


MODEL_COMPARISON_SPLIT

#### Want to look at difference between m and f, so have to do it with samples specifically

test_data[, variable := paste0("V", rownames(test_data))]

temp0 <- as.data.table(fitted(Model_0, summary = F, newdata = test_data))[15000:20000]
temp0$sample <- c(1:nrow(temp0))
temp0m <- melt.data.table(temp0, id.vars = "sample")
temp0m$Model <- 0

temp1 <- as.data.table(fitted(Model_1, summary = F, newdata = test_data))[15000:20000]
temp1$sample <- c(1:nrow(temp1))
temp1m <- melt.data.table(temp1, id.vars = "sample")
temp1m$Model <- 1

temp2 <- as.data.table(fitted(Model_2, summary = F, newdata = test_data))[15000:20000]
temp2$sample <- c(1:nrow(temp2))
temp2m <- melt.data.table(temp2, id.vars = "sample")
temp2m$Model <- 2

temp3 <- as.data.table(fitted(Model_3, summary = F, newdata = test_data))[5000:10000]
temp3$sample <- c(1:nrow(temp3))
temp3m <- melt.data.table(temp3, id.vars = "sample")
temp3m$Model <- 3

all_output_individual <- rbind(temp0m, temp1m, temp2m, temp3m)

all_output_individual <- all_output_individual[test_data, on = "variable"]

all_output_individual <- dcast.data.table(all_output_individual, sample  + Model +
                   country + age + birth_rate + c_section ~ gender, 
                 value.var = "value")

all_output_individual[,difference := m-f]
summarised_output <- all_output_individual[, quantile(difference, probs = c(0.025, 0.5, 0.975)), 
                      by = c("Model", "country", "age", "birth_rate", "c_section")]

summarised_output$quants <-  rep(c("Q2.5", "Q50", "Q97.5"), nrow(summarised_output)/3)


summarised_output <- dcast.data.table(summarised_output, Model + country + age + birth_rate + c_section ~ quants, 
                 value.var = "V1")


summarised_output$Model <- factor(summarised_output$Model)

country_order <- summarised_output[Model == 1 & age == "19 to 64 Years", c("country", "Q50")]
country_order <- country_order[order(Q50)]$country
summarised_output[, country := factor(country, levels = country_order)]

DIFF_PLOT <- ggplot(summarised_output[age == "19 to 64 Years"],aes( x = country, y = Q50,
                                                      ymin = Q2.5, ymax = Q97.5,
                                                      colour = Model, group = Model),
       ) + 
  geom_pointrange(position =position_dodge2(width = 1)) + facet_grid(.~age) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs (x = "Country", y = "Difference in proportion resistant between male to female (quantiles)", 
        title = name_to_run)

DIFF_PLOT

### But actually we want to look at the values of the effects. 

# so a) what is the covariate term on birht_rate and c_section?
Model_3_fixed <- data.frame(exp(fixef(Model_3)))
Model_3_fixed$parameter <- as.character(rownames(Model_3_fixed))
Model_3_fixed <- data.table(Model_3_fixed)
Model_3_fixed$Model <- 3

Model_2_fixed <- data.frame(exp(fixef(Model_2)))
Model_2_fixed$parameter <- as.character(rownames(Model_2_fixed))
Model_2_fixed <- data.table(Model_2_fixed)
Model_2_fixed$Model <- 2

Model_1_fixed <- data.frame(exp(fixef(Model_1)))
Model_1_fixed$parameter <- as.character(rownames(Model_1_fixed))
Model_1_fixed <- data.table(Model_1_fixed)
Model_1_fixed$Model <- 1

Models_fixed <- rbind(Model_1_fixed, Model_2_fixed, Model_3_fixed)

Models_fixed[,Model := factor(Model)]

FIXED_EFFECTS <- ggplot(Models_fixed, aes(x = parameter, ymin = Q2.5, ymax = Q97.5, y = Estimate, 
                         colour = Model)) + 
  geom_pointrange(position =position_dodge2(width = 1)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1)) + 
  labs (x = "Fixed parameter", y = "exp(Estimate) (95% quantiles)", 
        title = paste0("Fixed effects: ",name_to_run))

### want to do the same for the random effects ###
  Model_3_random <- data.frame(ranef(Model_3))
  Model_3_random$parameter <- as.character(rownames(Model_3_random))
  Model_3_random <- data.table(Model_3_random)
  Model_3_random$Model <- 3
  colnames(Model_3_random) <- gsub(x = colnames(Model_3_random), pattern = "7.5", replacement = "75")  
  colnames(Model_3_random) <- gsub(x = colnames(Model_3_random), pattern = "2.5", replacement = "25")
  colnames(Model_3_random) <- gsub(x = colnames(Model_3_random), pattern = "Est.Error", replacement = "Error")
  
  Model_3_random <- melt(Model_3_random, id.vars = c("Model", "parameter"))
  Model_3_random[, c("random_variable", 
                     "quantile", 
                     "covariate"):= tstrsplit(variable, "[.]")]
  
  
  Model_2_random <- data.frame(ranef(Model_2))
  Model_2_random$parameter <- as.character(rownames(Model_2_random))
  Model_2_random <- data.table(Model_2_random)
  Model_2_random$Model <- 2
  colnames(Model_2_random) <- gsub(x = colnames(Model_2_random), pattern = "7.5", replacement = "75")  
  colnames(Model_2_random) <- gsub(x = colnames(Model_2_random), pattern = "2.5", replacement = "25")
  colnames(Model_2_random) <- gsub(x = colnames(Model_2_random), pattern = "Est.Error", replacement = "Error")
  
  Model_2_random <- melt(Model_2_random, id.vars = c("Model", "parameter"))
  Model_2_random[, c("random_variable", 
                     "quantile", 
                     "covariate"):= tstrsplit(variable, "[.]")]
  
  
  Model_1_random <- data.frame(ranef(Model_1))
  Model_1_random$parameter <- as.character(rownames(Model_1_random))
  Model_1_random <- data.table(Model_1_random)
  Model_1_random$Model <- 1
  colnames(Model_1_random) <- gsub(x = colnames(Model_1_random), pattern = "7.5", replacement = "75")  
  colnames(Model_1_random) <- gsub(x = colnames(Model_1_random), pattern = "2.5", replacement = "25")
  colnames(Model_1_random) <- gsub(x = colnames(Model_1_random), pattern = "Est.Error", replacement = "Error")
  
  Model_1_random <- melt(Model_1_random, id.vars = c("Model", "parameter"))
  Model_1_random[, c("random_variable", 
                     "quantile", 
                     "covariate"):= tstrsplit(variable, "[.]")]
  
  Models_random <- rbind(Model_1_random, Model_2_random, Model_3_random)
  Models_random <- dcast.data.table(Models_random, Model + parameter + covariate ~ quantile, 
                   value.var = "value")

  Models_random[, Model := factor(Model)]
  
RANDOM_EFFECTS <-  ggplot(Models_random, aes(x = parameter, ymin = Q25, ymax = Q975, y = Estimate, 
                           colour = Model)) + 
    geom_pointrange(position =position_dodge2(width = 1)) + 
    theme_bw() + 
    facet_grid(covariate~., scales = "free_y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1)) + 
    labs (x = "Country", y = "Estimate (95% quantiles)", 
          title = paste0("Random effects: ",name_to_run))


grid.arrange(RANDOM_EFFECTS,FIXED_EFFECTS, ncol =2, layout_matrix = rbind(c(1,1,2)))


### Let's compare how goot the models are
#loo_compare(Model_0, Model_1, Model_2, Model_3)

# Does adding birhtrate reduce the variance in gender slopes at all? 
variation <- data.frame(VarCorr(Model_3)$country$sd)
variation$variable <- rownames(variation)
variation$Model <- "Model 3"

variation1 <- data.frame(VarCorr(Model_2)$country$sd)
variation1$variable <- rownames(variation1)
variation1$Model <- "Model 2"

var_combo <- data.table(rbind(variation, variation1))


ggplot(var_combo, aes(x = variable, y = Estimate, ymin = Q2.5, ymax = Q97.5, colour = Model)) + 
  geom_pointrange( position = position_dodge2(width = 0.5)) + 
  theme_bw() + labs(y = "Variance")

#proportion of genderslope variance explained by adding birthrate
((var_combo[ Model == "Model 2" & variable == "genderm", "Estimate"]) - 
  var_combo[ Model == "Model 3" & variable == "genderm", "Estimate"])/ 
  (var_combo[ Model == "Model 2" & variable == "genderm", "Estimate"])

#Save some plots
ggsave(paste0("plots/brms_",sub(" ", "_", bug_specific), "_", sub(" ", "_", drug_specific),".pdf"),
       plot = DIFF_PLOT, 
       width = 10, height = 5)
#Save some plots
ggsave(paste0("plots/covariates_brms_",sub(" ", "_", bug_specific), "_", sub(" ", "_", drug_specific),".pdf"),
       plot = grid.arrange(RANDOM_EFFECTS,FIXED_EFFECTS, ncol =2, layout_matrix = rbind(c(1,1,2))), 
       width = 10, height = 7)
