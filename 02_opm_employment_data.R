library(tidyverse)

## Pull in the OPM raw dataset from here, https://data.opm.gov/explore-data/data/data-downloads. This is a dataset from April 2026

big_table <- read.delim("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/Trump admin response/6 Month Report/federal_workforce/2025_data/employment_202604_1_2026-06-04.txt", sep = "|")

##Pull in the executive order tables created in 01_grab_eo_data.R
the_eo_table <- read_csv("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/opm_schedule_pol_sci/new_empl_policy.csv") %>%
  mutate(department_to_join = toupper(department),
         department_to_join = gsub("  ", " ", department_to_join),
         position_name_to_join = toupper(position_name),
         major_department_clean_to_join = toupper(major_department_clean))



science_occ_series <- c("BIOLOGICAL|SCIENCE|TECHNICIAN|SOIL| CONSERVATION|NATURAL RESOURCES|LAND SURVEYING|ECOLOGY|FORESTRY|ECONOMIST|ANALYSIS|ENGINEERING|TECHNICAL|TECHNOLOGY|GEOGRAPHY|HYDROLOGY|PATHOLOGY|STATISTICAL|MATHEMATICAL|AGRONOMY|STATISTICS|BOTANY|DIETITIAN AND NUTRITIONIST|LABORATORY|ENGINEERING|MICROBIOLOGY|ANIMAL HEALTH TECHNICIAN|PHYSICAL SCIENCE|ENVIRONMENTAL PROTECTION SPECIALIST|PHYSIOLOGY|CARTOGRAPHY|FISH BIOLOGY|WATER TREATMENT PLANT OPERATING|ENTOMOLOGY|METEOROLOGY|GEOLOGY|VETERINARY|ENVIRONMENTAL ENGINEERING|FOREST PRODUCTS TECHNOLOGY|CHEMICAL ENGINEERING|METEOROLOGICAL TECHNICIAN|CHEMISTRY|AVIATION SAFETY|CARTOGRAPHIC TECHNICIAN|PHYSIOLOGY|HEALTH SCIENCE|PSYCHOLOGY|ZOOLOGY|ENGINEERING|PHARMACOLOGY|TOXICOLOGY|HEALTH PHYSICS|RESEARCH|MATHEMATICS|GEOPHYSICS|ANTHROPOLOGY|SOCIOLOGY|PHYSICS")

##Filter the employee data down to those in the EO that do science or stem work within the sub-agencies in the EO and with the position names in the EO. Some of the sub-agencies do not occur in the OPM data.
at_best <- inner_join(big_table, the_eo_table, by = c("department" = "department_to_join", "occupational_series" = "position_name_to_join"), relationship = "many-to-many") %>%
  filter(grepl("SCIENCE", occupational_group)|grepl("SCIENCE", stem_occupation_type)|grepl(science_occ_series, occupational_series))



##Filter the employees data down to those in the EO that do science or stem work within the independent agencies in the EO and with the position names in the EO.
at_most <- inner_join(big_table, the_eo_table, by = c("department" = "major_department_clean_to_join", "occupational_series" = "position_name_to_join"), relationship = "many-to-many") %>%
  filter(grepl("SCIENCE", occupational_group)|grepl("SCIENCE", stem_occupation_type)|grepl(science_occ_series, occupational_series))

