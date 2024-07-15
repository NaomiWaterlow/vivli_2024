#Combine the datasets
library(data.table)
library(countrycode)

# convert the country variable in the country data to match atlas
country_data <- fread("Other_data/other_data_v1.csv")
country_data[, country := countrycode(country_code, origin = "iso3c", destination = "country.name")]

# load atlas
atlas_cleaned <- fread("data/atlas_data_cleaned.csv")
# remove  column 
atlas_cleaned[, V1 := NULL]

# match byt country names the variables
atlas_cleaned[country_data, on = c("country"), birth_rate := i.birth_rate]
# check any missing
unique(atlas_cleaned[is.na(birth_rate),]$country)

# search for the missing ones and rename to match in country dataset
country_data[grep("taiwan", country, ignore.case = T)] # this is the correct match, just no data
country_data[grep("hong", country, ignore.case = T)] 
country_data[country_code == "HKG", country := "Hong Kong"]
country_data[grep("korea", country, ignore.case = T)]
country_data[country_code == "KOR", country := "Korea, South"]
country_data[grep("czech", country, ignore.case = T)]
country_data[country_code == "CZE", country := "Czech Republic"]
country_data[grep("iv", country, ignore.case = T)]
country_data[country_code == "CIV", country := "Ivory Coast"]
country_data[grep("slov", country, ignore.case = T)]
country_data[country_code == "SVK", country := "Slovak Republic"]

# match again! Now labels should match
atlas_cleaned[country_data, on = c("country"), birth_rate := i.birth_rate]
# check any missing
unique(atlas_cleaned[is.na(birth_rate),]$country)
# just taiwan! And that's because the data is missing, not incorrect coding

# Then add the other variables
atlas_cleaned[country_data, on = c("country"), GDP := i.GDP]
atlas_cleaned[country_data, on = c("country"), c_section := i.c_section]
atlas_cleaned[country_data, on = c("country"), primary_completion_female_over_male := i.primary_completion_female_over_male]

# save the combined data
fwrite(atlas_cleaned, "data/combined_atlas.csv")

