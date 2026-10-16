library(tidyverse)
library(rvest)
library(httr)
library(chromote)

##Pull in the EO table

the_eo_table <- read_csv("../data_info/rapid_opm_rule/new_empl_policy_agency.csv") %>%
  mutate(agency = ifelse(agency ==  "Department of War", "Department of Defense", agency),
         subagency_office = gsub("of War", "of Defense", subagency_office),
         agency = gsub("of War", "of Defense", agency),
         subagency_office_to_join = toupper(subagency_office),
         subagency_office_to_join = gsub("  ", " ", subagency_office_to_join),
         position_name_to_join = toupper(position_name),
         agency_to_join = toupper(agency)) %>%
  select(-position_number) %>%
  distinct()

##Find which rows have already run.
all_text_tables <- read_csv("../data_info/rapid_opm_rule/occ_series_pos_name_raw_agency.csv")

the_eo_table <- anti_join(the_eo_table, all_text_tables)


##Create a Yahoo Search loop

##Set the objects for the loop
all_text_tables <- data.frame()

copy_element <- "#web > ol.reg.searchCenterTop > li > div > div.grp.grp-genAISum > div.compText.genAI.s-ans.hideOnLoad.hideCollapse.answer.fadeIn > div > div"

for(i in 1379:nrow(the_eo_table)){
  if(the_eo_table$subagency_office[i] != the_eo_table$agency[i]){
 search_phrase <- paste0("What is the federal occupational series for a ", the_eo_table$position_name[i], " in the ", the_eo_table$subagency_office[i], " in the ", the_eo_table$agency_to_join[i])
  }else{search_phrase <- paste0("What is the federal occupational series for a ", the_eo_table$position_name[i], " in the ", the_eo_table$subagency_office[i])}
 
 # Replace spaces with '+' 
 query_string <- gsub(" ", "+", search_phrase)
 
 # Build the complete Google search URL
 target_url <- paste0("https://www.search.yahoo.com/search?p=", query_string)
 
 b <- ChromoteSession$new() 
 
 b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
 Sys.sleep(4)
 b$Page$navigate(target_url)
 Sys.sleep(10)
 with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
 Sys.sleep(6)
 
 occ_series_ai <- read_html(with_user_agent) %>%
   html_element(copy_element) %>% 
   html_text() %>% 
   as.character()
 
 b$close(wait = FALSE) 
 
 text_table <- data.frame(iteration = i)
 text_table$occ_series_guess <- occ_series_ai
 text_table$position_name <- the_eo_table$position_name[i]
 text_table$subagency_office <- the_eo_table$subagency_office[i]
 text_table$agency <- the_eo_table$agency[i]
 all_text_tables <- bind_rows(all_text_tables, text_table)
}


##Quickly save the table before it is lost

write_csv(all_text_tables, "../data_info/rapid_opm_rule/occ_series_pos_name_raw_yahoo_1.csv")

##To coerce the ai text into the occ_series_code and the occ_series
all_text_tables <- all_text_tables %>%
  rowwise() %>%
  mutate(occ_series_digits = str_extract_all(occ_series_guess, "\\b\\d{4}\\b"),
         occ_series_digits = paste(unique(unlist(occ_series_digits)), collapse = "|")) #%>%
  #separate_longer_delim(occ_series_digits, sep = "|")

write_csv(all_text_tables, "../data_info/rapid_opm_rule/occ_series_pos_name_clean_yahoo_1.csv")

