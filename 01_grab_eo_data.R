library(tidyverse)
library(pdftools)

the_eo_fonts <- pdf_data("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/opm_schedule_pol_sci/2026SchedulePolicyCareer.eo_.APPENDIX.pdf", font_info = TRUE)

# The pdf data are saved in individual tibbles for each character with the character and the formatting. This is how to pull out the all bold lines of text, which align with the independent agencies in the EO. The following pulls the length of a tibble
find_matrix_length <- function(x){
  dim_1 <- dim(the_eo_fonts[[x]])
  length_1 <- dim_1[[1]]
}

all_themes <- data.frame(stringsAsFactors = FALSE)

# This loop goes through and pulls each line in the document that includes bold font, which aligns with independent agencies.
for(i in 1:229){
  
  comment_font <- the_eo_fonts[[i]][1:find_matrix_length(i), 7]
  
  comment_text <- the_eo_fonts[[i]][1:find_matrix_length(i), 6]
  
  comment_df <- data.frame(font = comment_font, text = comment_text, stringsAsFactors = FALSE)
  
  comment_df <- filter(comment_df, grepl("Bold", font_name))
  
  if(nrow(comment_df) > 0){
    text_in_column <- comment_df$text
    text_in_row <- paste(text_in_column, collapse = " ") %>% unlist()
    themes_from_text <- data.frame(theme = text_in_row, page = i, stringsAsFactors = FALSE)
    
  }
  all_themes <- bind_rows(themes_from_text, all_themes)
}

all_themes <- unique(all_themes) %>%
  mutate(theme = gsub("Within the", "", theme),
         theme = gsub(":", "", theme),
         theme = str_trim(theme))


the_eo <- pdf_text("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/opm_schedule_pol_sci/2026SchedulePolicyCareer.eo_.APPENDIX.pdf")

the_eo <- str_flatten(the_eo)

the_eo <- str_split(the_eo, "With") %>% unlist()

the_eo_table <- data.frame(beginning_column = the_eo)

major_departments_fonts <- paste0(all_themes$theme, collapse = "|")
the_eo_table <- the_eo_table %>%
  mutate(major_department = ifelse(grepl(major_departments_fonts, beginning_column), beginning_column, ""),
         major_department_clean = str_match(major_department, "in the (.*?):")[, 2])

##Create a table of departments, position names, and position numbers
the_eo_table <- the_eo_table %>%
  separate_wider_delim(beginning_column, delim = ":", names = c("department", "the_rest"), too_few = "align_start", cols_remove = FALSE)

the_eo_table <- the_eo_table %>%
  separate_longer_delim(., the_rest, delim = ";")

the_eo_table <- the_eo_table %>%
  separate_wider_delim(the_rest, delim = "description", names = c("position_name", "position_number"), too_few = "align_start", cols_remove = FALSE)


the_eo_table <- the_eo_table %>%
  separate_longer_delim(., position_number, delim = ",")

the_eo_table <- the_eo_table %>%
  separate_longer_delim(., position_number, delim = " and")


##Clean up the table
the_eo_table_clean <- the_eo_table %>%
  mutate(major_department_clean = ifelse(grepl("in the Department of Agriculture", department), "Department of Agriculture", major_department_clean)) %>%
  fill(major_department_clean, .direction = "down") %>%
  mutate(department = gsub("in the |in ", "", department),
         position_name = gsub("Positions of|under|position", "", position_name),
         position_number = gsub("description|number", "", position_number)) %>%
  mutate(position_name = gsub("[^A-Za-z ]", "", position_name),
         position_number = gsub("s\n\n", "", position_number),
         position_number = gsub("s | and|and\n\n", "", position_number)) %>%
  mutate(across(where(is.character), str_trim)) %>%
  filter(!is.na(position_name),
         !is.na(position_number),
         !is.null(position_number),
         position_number != "",
         position_number != "and") %>%
  select(department, position_name, position_number, major_department_clean) %>%
  mutate(major_department_clean = ifelse(is.na(major_department_clean), department, major_department_clean))


##Save the table
write_csv(the_eo_table_clean, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/opm_schedule_pol_sci/new_empl_policy.csv")

