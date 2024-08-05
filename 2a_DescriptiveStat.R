####### August 2024 ######################################################################
## Authors: Naomi Waterlow , Alastair Clements, Chaelin Kim, Simon Procter, Gwen Knight ##
##########################################################################################

# ------------------------------------------------------------------------------
### Descriptive Statistic Table
# ------------------------------------------------------------------------------
# remove all objects from workspace
rm (list = ls ())

# load libraries
library(dplyr)
library(tidyr)
library(flextable)
# ------------------------------------------------------------------------------


# ---------------------------------------------------------------------
# Load data
# Read the combined_atlas data from a CSV file
combined_atlas <- read.csv("data/combined_atlas.csv")

# Filter out "Unknown" age and countries with less than 1000 samples
combined_atlas <- combined_atlas %>%
  filter(age != "Unknown") %>%
  group_by(country) %>%
  filter(n() >= 1000) %>%
  ungroup()

# Reorder the age categories
age_order <- c("0 to 2 Years", "3 to 12 Years", "13 to 18 Years", 
               "19 to 64 Years", "65 to 84 Years", "85 and Over")
combined_atlas$age <- factor(combined_atlas$age, levels = age_order)
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# Examine the data
# Check the column names of the combined_atlas data frame
colnames(combined_atlas)
# "age"        "gender"     "year"       "country"    "antibiotic"   "data"       
# "species"    "mic_label"   "birth_rate"    "c_section"

# Generate a descriptive statistics table

# Count by gender
table_count <- combined_atlas %>% 
  count(gender) %>% 
  mutate(gender = case_when(
    gender == "f" ~ "Female",
    gender == "m" ~ "Male"
  )) %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0) %>%
  mutate(Total = Female + Male,
         Female_percent = round(100 * Female / Total, 1),
         Male_percent = round(100 * Male / Total, 1),
         Female = paste0(formatC(Female, format = "f", big.mark = ",", digits = 0), " (", Female_percent, "%)"),
         Male = paste0(formatC(Male, format = "f", big.mark = ",", digits = 0), " (", Male_percent, "%)"),
         Total = paste0(formatC(Total, format = "f", big.mark = ",", digits = 0), " (100%)"),
         variable = "Number") %>%
  add_row(variable = "") %>%
  select(variable, Female, Male, Total)

# Age distribution by gender
table_age <- combined_atlas %>%
  mutate(gender = case_when(
    gender == "f" ~ "Female",
    gender == "m" ~ "Male"
  )) %>%
  group_by(gender, age) %>%
  summarise(n = n()) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  pivot_wider(names_from = gender, values_from = c(n, percent), names_sep = "_") %>%
  mutate(Total_n = n_Female + n_Male,
         Total_percent = round(100 * Total_n / sum(Total_n), 1),
         Female = paste0(formatC(n_Female, format = "f", big.mark = ",", digits = 0), " (", percent_Female, "%)"),
         Male = paste0(formatC(n_Male, format = "f", big.mark = ",", digits = 0), " (", percent_Male, "%)"),
         Total = paste0(formatC(Total_n, format = "f", big.mark = ",", digits = 0), " (", Total_percent, "%)"),
         variable = age) %>%
  select(variable, Female, Male, Total) %>%
  add_row(variable = "age category", .before = 1) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "", .))) %>%
  add_row(variable = "") %>%
  print()

# Resistance status by species, antibiotic, and gender
table_resistance <- combined_atlas %>%
  mutate(gender = case_when(
    gender == "f" ~ "Female",
    gender == "m" ~ "Male"
  ),
  antibiotic = case_when(
    antibiotic == "ampicillin" ~ "Ampicillin",
    antibiotic == "erythromycin" ~ "Erythromycin",
    antibiotic == "levofloxacin" ~ "Levofloxacin",
    TRUE ~ antibiotic
  )) %>%
  group_by(species, antibiotic, gender) %>%
  summarise(n = sum(mic_label), total = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / total, 1)) %>%
  mutate(variable = paste0(species, " (", antibiotic, ")")) %>%
  pivot_wider(names_from = gender, values_from = c(n, total, percent), names_sep = "_") %>%
  mutate(Total_n = n_Female + n_Male,
         Total_total = total_Female + total_Male,
         Total_percent = round(100 * Total_n / Total_total, 1),
         Female = paste0(formatC(n_Female, format = "f", big.mark = ",", digits = 0), "/", 
                         formatC(total_Female, format = "f", big.mark = ",", digits = 0), " (", percent_Female, "%)"),
         Male = paste0(formatC(n_Male, format = "f", big.mark = ",", digits = 0), "/", 
                       formatC(total_Male, format = "f", big.mark = ",", digits = 0), " (", percent_Male, "%)"),
         Total = paste0(formatC(Total_n, format = "f", big.mark = ",", digits = 0), "/", 
                        formatC(Total_total, format = "f", big.mark = ",", digits = 0), " (", Total_percent, "%)")) %>%
  select(variable, Female, Male, Total) %>%
  add_row(variable = "Resistance status", .before = 1) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "", .))) %>%
  add_row(variable = "") %>%
  print()

# Combine all the generated tables into a single table
table1 <- bind_rows(table_count, table_age, table_resistance)

# Create a formatted table using the flextable package
table1_w <- flextable(table1) %>%
  width(j = c(1, 2, 3, 4), width = c(2.5, 1.5, 1.5, 1.5)) %>%
  align(j = c(2, 3, 4), align = "center", part = "all") %>%
  font(fontname = "Arial", part = "all") %>%
  set_header_labels(variable = "", Female = "Female\nn (%)", Male = "Male\nn (%)", Total = "Total\nn (%)") %>%
  bold(part = "header") %>%
  compose(i = ~ variable == "Number", j = 1, as_paragraph("Count")) %>%
  compose(i = ~ variable == "age category", j = 1, as_paragraph("Age category")) %>%
  compose(i = ~ variable == "Resistance status", j = 1, as_paragraph("Resistance status")) %>%
  compose(i = ~ variable %in% unique(table1$variable[!table1$variable %in% c("Number", "age category", "Resistance status")]), j = 1,
          value = as_paragraph(paste0("   ", table1$variable[!table1$variable %in% c("Number", "age category", "Resistance status")]))) %>%
  footnote(i = ~ variable == "Resistance status", j = 1, value = as_paragraph("Resistant isolates/total tested (% resistant)"), ref_symbols = c("1")) %>%
  add_footer_lines(top = FALSE) %>%
  padding(padding = 0, part = "all") %>%
  add_header_lines(values = as_paragraph("Table 2\nDemographic and Clinical Characteristics by Gender"))

# Print the formatted table
print(table1_w)

# Save the table as a Word document
save_as_docx(table1_w, path = "table_2.docx")
# ---------------------------------------------------------------------
# end.

