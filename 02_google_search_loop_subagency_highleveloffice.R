library(tidyverse)
library(rvest)
library(httr)
library(chromote)


##Pull in the OPM data
big_table <- read.delim("../rapid_opm_rule/data_info/employment_202604_1_2026-06-04.txt", sep = "|")

agency_subagency <- big_table %>%
  select(agency, agency_subelement, agency_code) %>%
  distinct()


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
  filter(agency == subagency_office)

##2. Find where the agencies are not high level offices and already join with the opm data.
the_eo_table_subagency <- the_eo_table_ag_subag %>%
  filter(agency != subagency_office) %>%
  left_join(., agency_subagency, by = c("agency_to_join" = "agency", "subagency_office_to_join" = "agency_subelement")) %>%
  filter(!is.na(agency_code)) %>%
  select(agency, subagency_office, subagency_office_to_join, agency_to_join) %>%
  distinct()

##3. Find where the EO lists high level offices and not subagencies, and create the table with the information from the Google search that included agencies

the_eo_table_hl_offices <- the_eo_table_ag_subag %>%
  anti_join(., the_eo_table_subagency) %>%
  anti_join(., the_eo_table_ind_ag)  %>%
  mutate(subagency_office = gsub("//\n//\n", " ", subagency_office))

##4. Find which subagencies and agencies have already run.
all_text_tables_done <- read_csv("../rapid_opm_rule/data_info/office_subagency_yahoo_agency.csv")

the_eo_table_hl_offices <- anti_join(the_eo_table_hl_offices, all_text_tables_done)

##Create a Yahoo Search loop

##Set the objects for the loop
all_text_tables <- data.frame()

copy_element <- "#web > ol.reg.searchCenterTop > li > div > div.grp.grp-genAISum > div.compText.genAI.s-ans.hideOnLoad.hideCollapse.answer.fadeIn > div > div"

for(i in 1:nrow(the_eo_table_hl_offices)){
  search_phrase <- paste0("What subagency is the ", the_eo_table_hl_offices$agency[i], " office of ", the_eo_table_hl_offices$subagency_office[i])
  
  # Replace spaces with '+' 
  query_string <- gsub(" ", "+", search_phrase)
  
  # Build the complete Google search URL
  target_url <- paste0("https://www.search.yahoo.com/search?p=", query_string)
  
  b <- ChromoteSession$new() 
  Sys.sleep(6) 
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$Page$navigate(target_url)
  Sys.sleep(8)
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(10)
  
  office_sub_agency <- read_html(with_user_agent) %>%
    html_elements(copy_element) %>% 
    html_text() %>% 
    as.character()
  
  #office_sub_agency <- office_sub_agency[1]
  b$close(wait = FALSE) 
  
  if(length(office_sub_agency) != 0){
  text_table <- data.frame(iteration = i)
  text_table$subagency_guess <- office_sub_agency
  text_table$agency <- the_eo_table_hl_offices$agency[i]
  text_table$subagency_office <- the_eo_table_hl_offices$subagency_office[i]
  
  all_text_tables <- bind_rows(all_text_tables, text_table)}
}

write_csv(all_text_tables, "../rapid_opm_rule/data_info/office_subagency_yahoo_agency_1.csv")

