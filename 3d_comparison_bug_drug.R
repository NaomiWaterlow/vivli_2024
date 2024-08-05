####### August 2024 ######################################################################
## Authors: Naomi Waterlow , Alastair Clements, Chaelin Kim, Simon Procter, Gwen Knight ##
##########################################################################################
colours_friendly  <- c( "#E69F00", "#56B4E9", "#009E73",  "#CC79A7"
                       , "#0072B2", "#D55E00","#661100" )
scales::show_col(colours_friendly)

# comparison across bugs of fixed effects

# 1. Age differences are much bigger than any gender differences

# Read in models and data
model_files <- list.files(path = "output/", pattern = "fixed")
all_fixed <- do.call('rbind', lapply(paste0("output/",model_files), readRDS))
all_fixed <- data.table(all_fixed)

all_fixed[parameter == "age13to18Years", 
          parameter_nice := "Age 13 - 18"]
all_fixed[parameter == "age19to64Years", 
          parameter_nice := "Age 19 - 64"]
all_fixed[parameter == "age3to12Years", 
          parameter_nice := "Age 3 - 12"]
all_fixed[parameter == "age65to84Years", 
          parameter_nice := "Age 65 - 84"]
all_fixed[parameter == "age85andOver", 
          parameter_nice := "Age 85+"]
all_fixed[parameter == "birth_rate", 
          parameter_nice := "birth rate"]
all_fixed[parameter == "c_section", 
          parameter_nice := "c-section rate"]
all_fixed[parameter == "genderm", 
          parameter_nice := "Male"]
all_fixed[parameter == "genderm:birth_rate", 
          parameter_nice := "Male : birth rate"]
all_fixed[parameter == "genderm:c_section", 
          parameter_nice := "Male : c-section rate"]
all_fixed[parameter == "Intercept", 
          parameter_nice := "Intercept"]

all_fixed[, parameter_nice := factor(parameter_nice, levels = c(
 "Intercept", "Male",  "Age 3 - 12", "Age 13 - 18", "Age 19 - 64", "Age 65 - 84", 
  "Age 85+", "birth rate", "Male : birth rate", "c-section rate", "Male : c-section rate"
))]

all_fixed[,bug_drug := paste0(bug, ", ", drug)]

all_fixed[parameter %in% c("age13to18Years", "age19to64Years", 
                           "age3to12Years", "age65to84Years", "age85andOver"), grouping := "ages"]
all_fixed[parameter %in% c("genderm","birth_rate","genderm:birth_rate", 
                           "c_section", "genderm:c_section"), grouping := "gender"]
all_fixed[parameter == "Intercept", grouping := "Intercept"]


ALL_FIXED <- ggplot(all_fixed, aes(x = parameter_nice, y = Estimate, ymin = Q2.5, ymax = Q97.5, 
                      colour = bug_drug, shape = Model)) +
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_pointrange(position = position_dodge2(width = 0.8)) + 
  theme_bw() + 
  facet_grid(.~grouping, scale = "free",space = "free_x")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  scale_color_manual(values = c(colours_friendly[c(1:4)]))+
  labs(x = "parameter", y = "Estimate (95% CI)", colour = "Bacteria, antibiotic", 
       title = "Fixed effects")


##### 2. But we see variation in the country by Intercept, and also by slope (less).


# Read in models and data
model_files <- list.files(path = "output/", pattern = "random")
all_random <- do.call('rbind', lapply(paste0("output/",model_files), readRDS))
all_random <- data.table(all_random)

all_random[bug == "Escherichia coli",bug_short := "E.coli"]
all_random[bug == "Staphylococcus aureus",bug_short := "S.aureus"]

all_random[drug == "ampicillin",drug_short := "amp."]
all_random[drug == "levofloxacin",drug_short := "levo."]
all_random[drug == "erythromycin",drug_short := "eryth."]


temp <- all_random[Model ==2 & covariate == "Intercept" & drug == "levofloxacin" & 
                     bug == "Staphylococcus aureus"]
country_order <- temp[order(Estimate)]$parameter
all_random[,parameter := factor(parameter, levels = country_order)]

GENDER_RANDOM <- ggplot(all_random[Model ==2 & covariate == "genderm"],
                        aes(x = parameter, y = Estimate, ymin = Q25, 
                       ymax = Q975, colour = drug_short)) + 
         geom_pointrange(position = position_dodge2(width = 0.7)) +
  facet_grid(bug_short+drug_short~covariate) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  labs(x = "country", y = "Estimate (95% CI)", colour = "Antibiotic", 
       title = "Random effects (gender) - Model 2") + 
  scale_color_manual(values = colours_friendly[c(5:7)])+
  geom_hline(yintercept = 0, linetype = "dashed")

INTERCEPT_RANDOM <- ggplot(all_random[Model ==2 & covariate == "Intercept"], aes(x = parameter, y = Estimate, ymin = Q25, 
                                                                            ymax = Q975, colour = drug_short)) + 
  geom_pointrange(position = position_dodge2(width = 0.7)) +
  facet_grid(bug_short+drug_short~covariate) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  labs(x = "country", y = "Estimate (95% CI)", colour = "Antibiotic", 
       title = "Random effects (Intercept) - Model 2") + 
  scale_color_manual(values = colours_friendly[c(5:7)])+
  geom_hline(yintercept = 0, linetype = "dashed")

grid.arrange(ALL_FIXED, GENDER_RANDOM, INTERCEPT_RANDOM, layout_matrix = rbind(c(1,2), 
                                                                               c(1,3)))

leg <- get_legend(GENDER_RANDOM)
# 3. birth rate may allow us to explain some of the country specific variation, 
# but not the gender slope. 

# so this can be the variation things

model_files <- list.files(path = "output/", pattern = "var")
var_all <- do.call('rbind', lapply(paste0("output/",model_files), readRDS))
var_all <- data.table(var_all)
var_all[, bug_drug := paste0(bug, ", ", drug)]
var_all <- data.table(var_all)

ggplot(var_all, aes(x = bug_drug, y = Estimate, ymin = Q2.5, ymax = Q97.5, 
                    colour = Model)) + 
  geom_pointrange(position = position_dodge2(width = 0.1)) + facet_grid(variable~., scales = "free_y") + 
  theme_bw()

var_summarised <- dcast.data.table(var_all, variable + bug + drug + bug_drug ~ Model, value.var = "Estimate")
var_summarised[,perc_change :=( (`Model 3` - `Model 2`)/ `Model 3`)*100]

ggsave(paste0("plots/figure.pdf"),
       plot =grid.arrange(ALL_FIXED, INTERCEPT_RANDOM + theme(legend.position = "none"),
                          GENDER_RANDOM + theme(legend.position = "none"), 
                          leg
                          ,layout_matrix = rbind(c(1,1,1,1,1,2,2,2,2,4),
                                                c(1,1,1,1,1,3,3,3,3,4))), 
       width = 18, height = 10)



  