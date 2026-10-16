library(tidyverse)
gc()

##Filter to scientists and stem activities
eo_opm <- read_csv("../data_info/rapid_opm_rule/eo_join_opm.csv")

##Narrow joined table to individuals from the OPM dataset.
eo_opm_indiv <- eo_opm %>%
  mutate(pay_plan = coalesce(pay_plan.x, pay_plan.y),
         agency = coalesce(agency.x, agency.y),
         supervisory_status = coalesce(supervisory_status, supervisory_status.x, supervisory_status.y)) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  #select(-c(count, bt_tracking_id, subagency_office:subagency)) %>%
  distinct()

eo_opm_indiv_sci_occ <- eo_opm_indiv %>%
  filter(stem_occupation_type == "SCIENCE OCCUPATIONS") %>%
  select(-c(position_name, ends_with("_to_join"), subagency_office, subagency)) %>%
  distinct()

write_csv(eo_opm_indiv_sci_occ, "../data_info/rapid_opm_rule/potential_employees_impacted.csv")
