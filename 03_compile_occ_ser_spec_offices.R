library(tidyverse)
library(fuzzyjoin)
library(stringdist)

gc()
##Read in the opm data to extract the appropriate subagencies found in the google search. The google search was,   paste0("What subagency of the federal government agency ", the_eo_table$agency[i], " is the ", the_eo_table$subagency_office[i], " in?")

big_table <- read.delim("../rapid_opm_rule/data_info/employment_202604_1_2026-06-04.txt", sep = "|")

agency_subagency <- big_table %>%
  select(department, agency_subelement, agency_code) %>%
  distinct()

ag_sub_off <- read.delim("../rapid_opm_rule/data_info/DTagy.txt", sep = ",")

sub_agencies <- unique(big_table$agency_subelement)
sub_agencies <- paste0(tolower(sub_agencies), collapse = "|")

sub_agencies_code <- unique(big_table$agency_subelement_code)
sub_agencies_code <- paste0(tolower(sub_agencies_code), collapse = "|")

##Join to EO for the subagency_office section of the data that were actually subagencies and not high level offices.
##The department of defense is the department of war in the EO table, but still defense in the OPM data. Will change to defense in the EO table.
the_eo_table <- read_csv("../rapid_opm_rule/data_info/new_empl_policy_agency.csv") %>%
  mutate(agency = ifelse(agency ==  "Department of War", "Department of Defense", agency),
         subagency_office = gsub("of War", "of Defense", subagency_office),
         agency = gsub("of War", "of Defense", agency),
         subagency_office_to_join = toupper(subagency_office),
         agency_to_join = toupper(agency))

the_eo_table_ag_subag <- the_eo_table %>%
  select(-c(position_name, position_number)) %>%
  distinct()

##1. Find where the agencies are independent and do not have subagencies
the_eo_table_ind_ag <- the_eo_table_ag_subag %>%
  filter(agency == subagency_office) %>%
  mutate(subagency = subagency_office) %>%
  rename(office = subagency_office) %>%
  select(agency, subagency, office) %>%
  distinct()

##2. Find where the agencies are not high level offices and already join with the opm data.
the_eo_table_subagency <- the_eo_table_ag_subag %>%
  filter(agency != subagency_office) %>%
  left_join(., agency_subagency, by = c("agency_to_join" = "department", "subagency_office_to_join" = "agency_subelement")) %>%
  filter(!is.na(agency_code)) %>%
  mutate(subagency = subagency_office) %>%
  rename(office = subagency_office) %>%
  select(agency, subagency, office) %>%
  distinct()

##3. Find where the EO lists high level offices and not subagencies, and create the table with the information from the Google search that included agencies

the_eo_table_hl_offices <- the_eo_table_ag_subag %>%
  anti_join(., the_eo_table_subagency) %>%
  anti_join(., the_eo_table_ind_ag)

setwd("../data_info/rapid_opm_rule/")

off_subag_yahoo <- list.files()
off_subag_yahoo <- off_subag_yahoo[grepl("office_subagency_yahoo_agency", off_subag_yahoo)]

off_subag_yahoo <- map_df(off_subag_yahoo, read_csv)


##Extract any subagencies from the sub agency guesses from Google search including the agency.
off_yahoo_ag_sub_agency <- off_subag_yahoo %>%
  mutate(subagency_guess_check = tolower(subagency_guess),
         found_subagency = str_extract_all(subagency_guess_check, sub_agencies))

off_yahoo_ag_sub_agency_paste <- off_yahoo_ag_sub_agency %>%
mutate(found_subagency = map_chr(found_subagency, ~ paste(unique(unlist(.x)), collapse = "|")))

specific_offices_long <- off_yahoo_ag_sub_agency_paste %>%
  separate_longer_delim(found_subagency, delim = "|") %>%
  filter(!is.na(found_subagency),
         found_subagency != "NA",
         found_subagency != "") %>%
  distinct()

specific_offices_long <- specific_offices_long %>%
  select(agency, subagency_office, found_subagency) %>%
  distinct() %>%
  mutate(agency_to_join = tolower(agency),
         found_subagency_to_join = tolower(found_subagency))



the_eo_table_subagency_guess <- the_eo_table_hl_offices %>%
  mutate(agency_to_join = tolower(agency_to_join),
         subagency_office_to_join = tolower(subagency_office_to_join)) %>%
  left_join(., specific_offices_long, by = c("agency_to_join" = "agency_to_join", "subagency_office_to_join" = "found_subagency_to_join")) %>%
  filter(!is.na(found_subagency)) %>%
  mutate(office = ifelse(is.na(subagency_office.x), subagency_office.y, subagency_office.x),
         subagency = ifelse(is.na(subagency_office.y), subagency_office.x, subagency_office.y),
         agency = agency.x) %>%
  select(agency, subagency, office) %>%
  distinct()



the_eo_table_not_joined <- the_eo_table_ag_subag %>%
  anti_join(., the_eo_table_subagency, by = c("agency", "subagency_office" = "subagency")) %>%
  anti_join(., the_eo_table_ind_ag, by = c("agency", "subagency_office" = "subagency")) %>%
  anti_join(., the_eo_table_subagency_guess, by = c("agency", "subagency_office" = "subagency"))

##Fuzzy join the rest by two words in the subagencies
the_eo_table_fuzzy_joined <- stringdist_left_join(the_eo_table_not_joined, agency_subagency, by = c("agency_to_join" = "department", "subagency_office_to_join" = "agency_subelement"), method = "jaccard", q = 2, max_dist = 0.5) %>%
  mutate(subagency = ifelse(!is.na(agency_subelement), agency_subelement, subagency_office),
         office = ifelse(is.na(agency_subelement), subagency_office, agency_subelement)) %>%
  select(agency, subagency, office) %>%
  distinct()

the_eo_table_not_joined <- the_eo_table_ag_subag %>%
  anti_join(., the_eo_table_subagency) %>%
  anti_join(., the_eo_table_ind_ag) %>%
  anti_join(., the_eo_table_subagency_guess) %>%
  anti_join(., the_eo_table_fuzzy_joined)

##Bind all the dataframes together from the direct joins and the Google searches
the_eo_subagencies_offices <- bind_rows(the_eo_table_ind_ag, the_eo_table_subagency, the_eo_table_subagency_guess, the_eo_table_fuzzy_joined) %>%
  distinct()


write_csv(the_eo_subagencies_offices, "../rapid_opm_rule/data_info/occ_series_spec_offices_all.csv")
