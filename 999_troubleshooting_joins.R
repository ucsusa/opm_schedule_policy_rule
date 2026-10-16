
library(tidyverse)

##None of the commodities trading commission positions match what was in the April 2026 opm data.
eo_still_to_join <- eo_still_to_join %>%
  filter(agency != "Commodity Futures Trading Commission")

big_table_to_join <- big_table_to_join %>%
  filter(agency != "COMMODITY FUTURES TRADING COMMISSION")

##There is no US Access Board in the opm data. We can omit from trying to join with the EO. It is misnamed in the EO, will change to architectural and transportation commission.
agri <- eo_still_to_join %>%
  filter(agency == "Department of Agriculture")
agri_bt <- big_table_to_join %>%
  filter(agency == "DEPARTMENT OF AGRICULTURE")


us_access <- eo_still_to_join %>%
  filter(agency == "U.S. Access Board")
#Nobody still to join from US Access Board.

us_access_bt <- big_table_to_join %>%
  filter(grepl("ACCESS", agency_subelement))
us_access_bt <- big_table_to_join %>%
  filter(grepl("ARCH", agency))


##Not in eo_still_to_join
national_comm_serv_bt <- big_table_to_join %>%
  filter(grepl("COMMUNITY SERVICE", agency))

##A scientist from the Navy didn't join, Navy is it's own dept.
defense <- eo_still_to_join %>%
  filter(grepl("Defense", agency))

defense_bt <- big_table_to_join %>%
  filter(grepl("DEFENSE", agency))

navy_bt <- big_table_to_join %>%
  filter(grepl("NAVY", agency))

##Director of programs isn't joining. There is no obvious position with this occupational series.
denali <- eo_still_to_join %>%
  filter(grepl("Denali", agency))

denali_bt <- big_table_to_join %>%
  filter(grepl("DENALI", agency))

##Nothing more should join here.
consumer <- eo_still_to_join %>%
  filter(agency == "Consumer Product Safety Commission")

consumer_bt <- big_table_to_join %>%
  filter(grepl("CONSUMER PRODUCT", agency))

##No obvious science roles in homeland, lots of supervisory and director and analyst positions
homeland <- eo_still_to_join %>%
  filter(agency == "Department of Homeland Security")

homeland_bt <- big_table_to_join %>%
  filter(grepl("HOMELAND SECURITY", agency))


#There is nothing in justice that is an obvious missed join.
justice <- eo_still_to_join %>%
  filter(agency == "Department of Justice")

justice_bt <- big_table_to_join %>%
  filter(agency == "DEPARTMENT OF JUSTICE")


##No obvious missed joins in labor
labor_bt <- big_table %>%
  filter(grepl("LABOR", agency))

labor_eo <- eo_still_to_join %>%
  filter(grepl("Labor", agency))


##No more missed joins here.
labor_rel_bt <- big_table %>%
  filter(grepl("LABOR RELATIONS", agency))

labor_rel_eo <- eo_still_to_join %>%
  filter(grepl("Labor Relations", agency))


##No missed joins here.
equal_bt <- big_table %>%
  filter(grepl("EQUAL EMPLOYMENT", agency))

equal_eo <- eo_still_to_join %>%
  filter(grepl("Equal Employment", agency))


##Only missed join is the executive secretariat, unlikely scientist roll
omb_bt <- big_table %>%
  filter(grepl("PERSONNEL", agency))

omb_eo <- eo_still_to_join %>%
  filter(grepl("Personnel", agency))

##No more obvious missed joins. Could be that this position is a missed join, but not enough information to modify the name. "Supervisory Vocational Rehabilitation Program Specialist"
ed_bt <- big_table %>%
  filter(grepl("EDUCATION", agency))

ed_eo <- eo_still_to_join %>%
  filter(grepl("Education", agency))

##Doesn't join one "operation's research" within the pipeline and hazardous waste sub_agency There are no "operations research" within the pipeline and hazardous waste sub_agency of transportation.
transportation_bt <- big_table %>%
  filter(grepl("TRANSPORTATION", agency))

transportation_eo <- eo_still_to_join %>%
  filter(grepl("Transportation", agency))

##No obvious scientist roles, several senior advisors and senior policy advisors
enviro <- eo_still_to_join %>%
  filter(grepl("Environmental", agency))

enviro_bt <- big_table %>%
  filter(grepl("ENVIRONMENTAL", agency))

##Checking if any human resource occupational series positions are stem.
hum_res <- big_table %>%
  select(occupational_series, stem_occupation, stem_occupation_type) %>%
  filter(grepl("HUMAN", occupational_series)) %>%
  unique()

##Chief risk officer does not join.
farm <- big_table %>%
  filter(grepl("FARM", agency_subelement))
farm_eo <- eo_still_to_join %>%
  filter(grepl("Farm", subagency_office))

deposit <- big_table %>%
  filter(grepl("DEPOSIT", agency_subelement))
deposit_eo <- eo_still_to_join %>%
  filter(grepl("Deposit", subagency_office))

##Multiple advisors do not join, but it is unknown what they advise.
energy_reg <- big_table %>%
  filter(grepl("ENERGY REG", agency_subelement))
energy_reg_eo <- eo_still_to_join %>%
  filter(grepl("Energy Regul", subagency_office))

##No additional and obvious stem position joins in housing and finance
hous_fin <- big_table %>%
  filter(grepl("HOUSING FINANCE", agency_subelement))
housing_fin_eo <- eo_still_to_join %>%
  filter(grepl("Housing Financ", subagency_office))


##No additional joins
lab_rel <- big_table %>%
  filter(grepl("LABOR RELATIONS", agency_subelement))
labor_rel_eo <- eo_still_to_join %>%
  filter(grepl("Labor Relations", subagency_office))

##One advisor didn't join and could be science, but not enough information to know for sure.
gen_serv <- big_table %>%
  filter(grepl("GENERAL SERVICES", agency))
gen_serv_eo <- eo_still_to_join %>%
  filter(grepl("General Service", agency))

##There are some scientists missed in this one.
##Science Program Leader and Scientific Executive at the National Institutes of Health could be a few people. I will add these into the join. There are some missed joins in the Office of the Assistant Secretary, but not seeing these in the opm data.
health <- big_table %>%
  filter(grepl("HEALTH AND HUMAN", agency))
health_eo <- eo_still_to_join %>%
  filter(grepl("Health and Human", agency))


housing <- big_table %>%
  filter(grepl("HOUSING", agency))
housing_eo <- eo_still_to_join %>%
  filter(grepl("Housing", agency))

inter_am <- big_table %>%
  filter(grepl("AMERICAN", agency))
inter_am_eo <- eo_still_to_join %>%
  filter(grepl("American", agency))

interior <- big_table %>%
  filter(grepl("INTERIOR", agency))
interior_eo <- eo_still_to_join %>%
  filter(grepl("Interior", agency))

int_dev <- big_table %>%
  filter(grepl("DEVELOPMENT", agency))
int_dev_eo <- eo_still_to_join %>%
  filter(grepl("International Development", agency))


nasa <- big_table %>%
  filter(grepl("AERONAUTICS", agency))
nasa_eo <- eo_still_to_join %>%
  filter(grepl("Aeronautics", agency))


credit <- big_table %>%
  filter(grepl("NATIONAL CREDIT", agency))
credit_eo <- eo_still_to_join %>%
  filter(grepl("National Credit", agency))

endow <- big_table %>%
  filter(grepl("HUMANITIES", agency))
endow_eo <- eo_still_to_join %>%
  filter(grepl("Endowment", agency))


counsel <- big_table %>%
  filter(grepl("COUNSEL", agency))
counsel_eo <- eo_still_to_join %>%
  filter(grepl("Counsel", agency))

liberties <- big_table %>%
  filter(grepl("LIBERTIES", agency))
liberties_eo <- eo_still_to_join %>%
  filter(grepl("Liberties", agency))

sba <- big_table %>%
  filter(grepl("SMALL", agency))
sba_eo <- eo_still_to_join %>%
  filter(grepl("Small", agency))

state <- big_table %>%
  filter(grepl("STATE", agency))
state_eo <- eo_still_to_join %>%
  filter(grepl("State", agency))

treas <- big_table %>%
  filter(grepl("TREASURY", agency))
treas_eo <- eo_still_to_join %>%
  filter(grepl("Treasury", agency))


african <- big_table %>%
  filter(grepl("AFRICAN", agency))
african_eo <- eo_still_to_join %>%
  filter(grepl("African", agency))

vets <- big_table %>%
  filter(grepl("VETERAN", agency))
vets_eo <- eo_still_to_join %>%
  filter(grepl("Veteran", agency))

children <- big_table %>%
  filter(grepl("CHILDREN", agency_subelement))
children_eo <- eo_still_to_join %>%
  filter(grepl("Children", subagency_office))

court_services <- big_table_to_join %>%
  filter(grepl("COURT SERVICES", agency))

commerce <- eo_still_to_join %>%
  filter(agency == "Department of Commerce")

commerce_bt <- big_table_to_join %>%
  filter(agency == "DEPARTMENT OF COMMERCE")

senior_exec <- big_table %>%
  filter(pay_plan == "SENIOR EXECUTIVE SERVICE")
lawyers <- big_table %>%
  filter(grepl("ATTORNEY", occupational_series))
lawyers_eo <- eo_never_joined %>%
  filter(grepl("Attorney", position_name))

##Check which are problematic, names or subagencies
eo_names <- unique(eo_still_to_join$position_name_to_join)
opm_names <- unique(big_table$occupational_series)

eo_names_not_in_opm <- eo_names[!eo_names %in% opm_names]
attorney <- opm_names[grepl("ATTORNEY", opm_names)]

check_dept_agency <- big_table %>%
  select(agency, department) %>%
  mutate(same = agency == department) %>%
  filter(same == FALSE) %>%
  distinct()

subagencies_eo <- unique(eo_still_to_join$subagency_office_to_join)
subagencies_opm <- unique(big_table$agency_subelement)
eo_subagenies_not_in_opm <- subagencies_eo[!subagencies_eo %in% subagencies_opm]
##It is more of an issue with the position names than with the subagencies not joining. But, both have issues.


eo_check <- eo_opm %>%
  select(subagency_office, position_name, position_number, agency.y) %>%
  distinct()

eo_never_joined <- anti_join(the_eo_table, eo_check, by = c("subagency_office", "position_name", "position_number", "agency" = "agency.y"))

##Resources
#https://ourpublicservice.org/know-the-facts/blog/schedule-policy-career-strips-job-protections-legal-budget-hr-independent-personnel
#https://www.opm.gov/about-us/open-government/plum-reporting/data-information/
#https://www.opm.gov/chcoc/latest-memos/opm-memo-re-executive-order-implementing-schedule-policycareer-in-the-excepted-service.pdf
#Can use https://www.opm.gov/policy-data-oversight/classification-qualifications/classifying-general-schedule-positions/occupationalhandbook.pdf to assign 0343 to analysts in this agency and subagency.
