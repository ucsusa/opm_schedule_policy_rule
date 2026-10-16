library(tidyverse)

occs <- list.files("../rapid_opm_rule/data_info/")

occs <- occs[grepl("occ_series_pos_name_clean_yahoo", occs)]

setwd("../rapid_opm_rule/data_info/")

occs_dfs <- map_dfr(occs, read_csv) %>% distinct()

##Check for number of non-empty returns.


specific_positions_long <- occs_dfs %>%
  mutate(pay_plan = ifelse(grepl("Senior Executive Service", occ_series_guess), "Senior Executive Service", NA)) %>%
  filter(!is.na(occ_series_digits)|!is.na(pay_plan)) %>%
  separate_longer_delim(occ_series_digits, delim = "|") %>%
  select(-c(iteration, occ_series_guess)) %>%
  distinct()

write_csv(specific_positions_long, "../rapid_opm_rule/data_info/occ_series_pos_name_clean_all.csv")

##Check for multiple subagencies among agencies, so multiple general attorneys in an agency distributed into multiple subagencies within.
check_4_multiples <- specific_positions_long %>%
  group_by(position_name, agency) %>%
  summarise(count = n())

##Should not be an issue if I only join by agency.
