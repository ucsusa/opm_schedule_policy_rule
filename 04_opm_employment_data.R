##This script joins the OPM raw dataset from here, https://data.opm.gov/explore-data/data/data-downloads (April 2026) to the parsed EO table, using several formatting and estimations of the position names in the EO vs the opm categories for federal employees.

library(tidyverse)
gc()


big_table <- read.delim("../data_info/rapid_opm_rule/employment_202604_1_2026-06-04.txt", sep = "|") %>%
  mutate(bt_tracking_id = 1:nrow(.),
         agency_subelement = ifelse(agency == "Department of the Navy", "Department of the Navy", agency_subelement),
         agency_subelement = ifelse(agency == "Department of the Army", "Department of the Army", agency_subelement),
         agency_subelement = ifelse(agency == "Department of the Air Force", "Department of the Air Force", agency_subelement)) %>%
  filter(!grepl("STUDENT TRAINEE", occupational_series))

##Pull in table of the offices within the office of the secretary at the Department of Health and Human Services

dhhs_osa_subs <- read_csv("../rapid_opm_rule/dhhs_os_subelements.csv")
sub_offices_dhhs <- unique(dhhs_osa_subs$office_name)
sub_offices_dhhs <- sub_offices_dhhs[!grepl("Assistant Secretary", sub_offices_dhhs)]

##Pull in the executive order tables created in 01_grab_eo_data.R
##US Access Board is under a different/incorrect name in the EO. It should be ARCHITECTURAL AND TRANSPORTATION BARRIERS COMPLIANCE BOARD
the_eo_table <- read_csv("../data_info/rapid_opm_rule/new_empl_policy_agency.csv") %>%
  select(-position_number) %>%
  distinct() %>%
  mutate(supervisory_status = ifelse(grepl("Supervis|Manage", position_name), "SUPERVISOR OR MANAGER", "ALL OTHER POSITIONS"),
         agency = ifelse(agency ==  "Department of War", "Department of Defense", agency),
         subagency_office = gsub("of War", "of Defense", subagency_office),
         agency = gsub("of War", "of Defense", agency),
         agency = ifelse(subagency_office == "Federal Energy Regulatory Commission", "Department of Energy", agency),
         agency = ifelse(agency == "U.S. Access Board", "Architectural and Transportation Barries Compliance Board", agency),
         agency = ifelse(agency == "Court Services and Offender Supervision Agency", "Court Services and Offender Supervision Agency of the District of Columbia", agency),
         agency = ifelse(agency == "U.S. African Development Foundation", "African Development Foundation", agency),
         agency = ifelse(grepl("Defense", agency), "Department of Defense", agency),
         agency = ifelse(grepl("Humanities", agency), "National Foundation on the Arts and the Humanities", agency),
         agency = ifelse(subagency_office == "Department of the Navy", "Department of the Navy", agency),
         agency = ifelse(subagency_office == "Department of the Army", "Department of the Army", agency),
         agency = ifelse(subagency_office == "Department of the Air Force", "Department of the Air Force", agency),
         subagency_office = ifelse(agency == "U.S. Access Board", "Architectural and Transportation Barries Compliance Board", subagency_office),
         subagency_office = ifelse(agency == "Department of Labor" & subagency_office == "Office of the Secretary", "Office of the Secretary of Labor", subagency_office),
         subagency_office = ifelse(agency == "Department of Agriculture" & subagency_office == "Office of the Secretary", "Office of the Secretary of Agriculture", subagency_office),
         subagency_office = gsub("Surface Mining", "Surface Mining,", subagency_office),
         subagency_office = ifelse(subagency_office == "U.S. Geological Survey", "Geological Survey", subagency_office),
         subagency_office = ifelse(subagency_office == "Bureau of Indian Affairs", "Indian Affairs", subagency_office),
         subagency_office = ifelse(agency == "Department of Labor" & subagency_office == "Office of the Secretary", "Office of the Secretary of Labor", subagency_office),
         subagency_office = ifelse(agency == "Department of Interior" & subagency_office == "Office of the Secretary", "Office of the Secretary of Interior", subagency_office),
         subagency_office = ifelse(agency == "Department of Education" & subagency_office == "Office of Civil Rights", "Office for Civil Rights", subagency_office),
         subagency_office = ifelse(agency == "Department of Transportation" & subagency_office == "Office of the Secretary", "Office of the Secretary of Transportation", subagency_office),
         subagency_office = ifelse(agency == "Department of Health and Human Services" & subagency_office == "Office of the Secretary", "Office of the Secretary of Health and Human Services", subagency_office),
         subagency_office = ifelse(subagency_office == "Great Lakes St. Lawrence Seaway", "Great Lakes Saint Lawrence Seaway Development Corporation", subagency_office),
         subagency_office = ifelse(agency == "Department of Education" & subagency_office == "Office of the Deputy Secretary", "Office of the Under Secretary", subagency_office),
         subagency_office = gsub("Veterans'", "Veterans", subagency_office),
         subagency_office = ifelse(agency == "Federal Law Enforcement Training Center", "Federal Law Enforcement Training Centers", subagency_office),
         subagency_office = ifelse(subagency_office == "United States Marshals Service", "U.S. Marshals Service", subagency_office),
         subagency_office = ifelse(subagency_office == "United States Trustee Program", "U.S. Trustee Program", subagency_office),
         subagency_office = ifelse(subagency_office == "Offices of the United States Attorneys", "Executive Office for U.S. Attorneys and the Offices of the United States Attorneys",  subagency_office),
         subagency_office = ifelse(subagency_office == "United States Trustee Program", "U.S. Trustee Program", subagency_office),
         subagency_office = ifelse(grepl("Patent and Trademark Office", subagency_office), "Patent and Trademark Office", subagency_office),
         subagency_office = ifelse(subagency_office == "U.S. Citizenship and Immigration Services", "Citizenship and Immigration Services", subagency_office),
         subagency_office = ifelse(subagency_office == "U.S. Customs and Border Protection", "Customs and Border Protection", subagency_office),
         subagency_office = ifelse(subagency_office == "Headquarters Components, Office of the Secretary", "DHS Headquarters", subagency_office),
         subagency_office = ifelse(agency == "Department of Health and Human Services" & !grepl("Assistant Secretary", subagency_office) & subagency_office %in% sub_offices_dhhs, "Office of the Secretary of Health and Human Services", subagency_office),
         position_name = ifelse(grepl("Grants Manage", position_name), "Grants Management", position_name),
         position_name = ifelse(agency == "Department of Health and Human Services" & grepl("Biolog", position_name), "General Natural Resources and Biological Sciences", position_name),
         position_name = ifelse(position_name == "Human Resources Specialist", "Human Resources Assistance", position_name),
         position_name = ifelse(grepl("Program Specialist", position_name), "Management and Program Analysis", position_name),
         position_name = ifelse(grepl("Human Resources", position_name) & grepl("Dir|Manag|Supe", position_name), "Human Resources Management", position_name),
         position_name = ifelse(grepl("Operations Research", position_name), "Operations Research", position_name),
         position_name = ifelse(grepl("Education Research", position_name), "Education Research", position_name),
         position_name = ifelse(grepl("Nutritionist", position_name), "Dietitian and Nutritionist", position_name),
         position_name = ifelse(grepl("Natural Resource Specialist", position_name), "General Natural Resources Management and Biological Sciences", position_name),
         position_name = ifelse(grepl("Economist", position_name), "Economist", position_name),
         position_name = ifelse(grepl("Epidemiolog|Health Scien|Health Attach|Public Health", position_name), "General Health Science", position_name),
         position_name = ifelse(grepl("Intergovernmental Affairs|Affairs Specialist|Public Affairs|Communication|Media", position_name), "Public Affairs", position_name),
         position_name = ifelse(grepl("Supervisory Human Resource", position_name), "Human Resources Management", position_name),
         position_name = ifelse(grepl("Attorney|Legal|Counsel", position_name) & !grepl("Patent", position_name), "General Attorney", position_name),
         position_name = ifelse(grepl("Budget Analyst|Budget Officer", position_name), "Budget Analysis", position_name),
         position_name = ifelse(grepl("Program Manager|Group Manager|Resource Manager", position_name), "Program Management", position_name),
         position_name_to_join = toupper(position_name),
         subagency_office_to_join = toupper(subagency_office),
         subagency_office_to_join = gsub("  ", " ", subagency_office_to_join),
         agency_to_join = toupper(agency),
         eo_tracking_id = 1:nrow(.))

##Join position name to occupational series and the department to the subagency. This assumes if the "office" in the executive order aligns with a subagency in the opm data.
eo_big_table_subag_series <- left_join(big_table, the_eo_table, by = c("occupational_series" = "position_name_to_join", "agency_subelement" = "subagency_office_to_join", "department" = "agency_to_join"), relationship = "many-to-many") %>%
  filter(!is.na(position_name))

eo_still_to_join <- the_eo_table %>%
  filter(!eo_tracking_id %in% eo_big_table_subag_series$eo_tracking_id)

big_table_to_join <- big_table %>%
  filter(!bt_tracking_id %in% eo_big_table_subag_series$bt_tracking_id)


##Pull in the table of position names and occupational series digits within subagencies and agencies.
specific_positions_long <- read_csv("../data_info/rapid_opm_rule/occ_series_pos_name_clean_all.csv")

eo_still_to_join_pos <- left_join(eo_still_to_join, specific_positions_long %>% select(-subagency_office), by = c("position_name", "agency"), relationship = "many-to-many")

##Pull in the table of agencies, subagencies, and offices from 03_compile_occ_ser_spec_offices and try to join by subagency, agency, and position names again.
subagency_agency_office <- read_csv("../data_info/rapid_opm_rule/occ_series_spec_offices_all.csv")

###There are multiple position names in agencies within different subagencies.

eo_still_to_join_pos_off <- left_join(eo_still_to_join_pos, subagency_agency_office, by = c("agency", "subagency_office" = "office")) %>%
  mutate(subagency_to_join = toupper(subagency),
         occ_series_digits = ifelse(grepl("Analys|Adviso", position_name) & grepl("Chief|Manage|Senior|Superv|Administr|Deput|Direct", position_name) & !grepl("Budget", position_name), "0343", occ_series_digits)) #Policy|Program|Strateg

eo_big_table_subag_series_guess <- left_join(big_table_to_join, eo_still_to_join_pos_off, by = c("occupational_series_code" = "occ_series_digits", "agency_subelement" = "subagency_to_join", "department" = "agency_to_join"), relationship = "many-to-many") %>%
  filter(!is.na(position_name))

eo_still_to_join <- the_eo_table %>%
  filter(!eo_tracking_id %in% eo_big_table_subag_series$eo_tracking_id,
         !eo_tracking_id %in% eo_big_table_subag_series_guess$eo_tracking_id)

big_table_to_join <- big_table %>%
  filter(!bt_tracking_id %in% eo_big_table_subag_series$bt_tracking_id,
         !bt_tracking_id %in% eo_big_table_subag_series_guess$bt_tracking_id)


##Join the remaining tables using the executive pay scale instead of the position names
##Will join all directors by executive pay category
specific_positions_pay_plan <- specific_positions_long %>%
  select(position_name, pay_plan) %>%
  filter(!is.na(pay_plan)) %>%
  distinct()

eo_still_to_join_dir <- eo_still_to_join %>%
  left_join(., specific_positions_pay_plan, by = "position_name")

eo_never_joined_dir <- eo_still_to_join_dir %>%
  mutate(pay_plan = ifelse(is.na(pay_plan) & grepl("Director|Deputy|Chief|Senior Vice President|Deputy", position_name), "SENIOR EXECUTIVE SERVICE", pay_plan),
         #pay_plan = ifelse(is.na(pay_plan) & grepl("Supervisor|Manage", position_name), "EXECUTIVE PAY", pay_plan),
         pay_plan = ifelse(agency == "Corporation for National and Community Service" & grepl("Direct", position_name), "EXECUTIVE-LEVEL AND MANAGERIAL POSITIONS IN THE CORPORATION FOR NATIONAL AND COMMUNITY SERVICE, P.L. 103-82", pay_plan),
         pay_plan = toupper(pay_plan))
  
eo_big_table_directors <- left_join(big_table_to_join, eo_never_joined_dir, by = c("pay_plan", "agency_subelement" = "subagency_office_to_join", "department" = "agency_to_join"), relationship = "many-to-many") %>%
  filter(!is.na(position_name))

eo_still_to_join <- the_eo_table %>%
  filter(!eo_tracking_id %in% eo_big_table_subag_series$eo_tracking_id,
         !eo_tracking_id %in% eo_big_table_subag_series_guess$eo_tracking_id,
         !eo_tracking_id %in% eo_big_table_directors$eo_tracking_id)

big_table_to_join <- big_table %>%
  filter(!bt_tracking_id %in% eo_big_table_subag_series$bt_tracking_id,
         !bt_tracking_id %in% eo_big_table_subag_series_guess$bt_tracking_id,
         !bt_tracking_id %in% eo_big_table_directors$bt_tracking_id)

##There are many mission areas that are spread across the Department of Agriculture and not in a subagency. So, can join by occ series number and position name and agency for dept of agriculture.
eo_big_table_ag <- left_join(big_table_to_join, eo_still_to_join, by = c("occupational_series" = "position_name_to_join", "department" = "agency_to_join"), relationship = "many-to-many") %>%
  filter(!is.na(position_name),
         department == "DEPARTMENT OF AGRICULTURE")

##There are a Science Program Leader and a Scientific Executive at the National Institutes of Health that could be several positions in the opm data, will join to all possibilities that are scientific positions and in leadership roles. 

big_table_to_join_nih <- big_table_to_join %>%
  filter(agency == "DEPARTMENT OF HEALTH AND HUMAN SERVICES",
         agency_subelement == "NATIONAL INSTITUTES OF HEALTH",
         grepl("SCIEN", occupational_series),
         grepl("MANAG|SUPER|LEADER", supervisory_status)) %>%
  mutate(join_variable = "dhhs_nih_scie_supe")

eo_still_to_join_nih <- eo_still_to_join %>%
  filter(agency == "Department of Health and Human Services",
         subagency_office == "National Institutes of Health",
         grepl("Scien", position_name))  %>%
  mutate(join_variable = "dhhs_nih_scie_supe",
         supervisory_status = ifelse(position_name == "Science Program Leader", 
         "TEAM LEADER", "MANAGEMENT OFFICIAL (CSRA)"))

eo_big_table_nih <- left_join(big_table_to_join_nih, eo_still_to_join_nih, by = c("department" = "agency_to_join", "agency_subelement" = "subagency_office_to_join", "join_variable", "supervisory_status"), relationship = "many-to-many") %>%
  select(-join_variable) %>%
  filter(!is.na(position_name))


##Pull together all the tables
eo_opm <- bind_rows(eo_big_table_subag_series, eo_big_table_subag_series_guess, eo_big_table_directors, eo_big_table_ag, eo_big_table_nih) %>%
  distinct()

write_csv(eo_opm, "../data_info/rapid_opm_rule/eo_join_opm.csv")


##Troubleshooting joins
eo_still_to_join <- the_eo_table %>%
  filter(!eo_tracking_id %in% eo_big_table_subag_series$eo_tracking_id,
         !eo_tracking_id %in% eo_big_table_subag_series_guess$eo_tracking_id,
         !eo_tracking_id %in% eo_big_table_directors$eo_tracking_id,
         !eo_tracking_id %in% eo_big_table_ag$eo_tracking_id,
         !eo_tracking_id %in% eo_big_table_nih$eo_tracking_id)

big_table_to_join <- big_table %>%
  filter(!bt_tracking_id %in% eo_big_table_subag_series$bt_tracking_id,
         !bt_tracking_id %in% eo_big_table_subag_series_guess$bt_tracking_id,
         !bt_tracking_id %in% eo_big_table_directors$bt_tracking_id,
         !bt_tracking_id %in% eo_big_table_ag$bt_tracking_id,
         !bt_tracking_id %in% eo_big_table_nih$bt_tracking_id)

##Resources
##Could try this - https://presentofcoding.substack.com/p/getting-usajobs-data
