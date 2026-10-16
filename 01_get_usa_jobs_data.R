# Install required packages if you haven't already
# install.packages(c("httr2", "jsonlite", "purrr", "dplyr"))

library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)

# 1. Define your API credentials
# NOTE: The User-Agent must be the exact email address used to request the API key.
api_key    <- "lttTwhtoJ/scY46kZci59Hq1pfzpS1YQ5JxzgxAIgAo="
user_agent <- "KEllickson@ucs.org" 

# 2. Build the request structure
req <- request("https://data.usajobs.gov/api/search") %>%
  req_headers(
    "Host"              = "data.usajobs.gov",
    "User-Agent"        = user_agent,
    "Authorization-Key" = api_key
  ) %>%
  # Add search parameters (e.g., keyword and location)
  req_url_query(
    Keyword      = "Data Scientist",
    LocationName = "Washington, DC",
    Page         = 1,
    ResultsPerPage = 25 # Maximum allowed per page is 500
  )

# 3. Execute the API call safely
response <- req_perform(req)

# 4. Parse the JSON response
raw_content <- resp_body_string(response)
parsed_data <- fromJSON(raw_content, simplifyVector = FALSE)

# 5. Extract and flatten job listings into a tidy data frame
job_results <- parsed_data$SearchResult$SearchResultItems

##Need to add occupational series
clean_jobs_df <- map_df(job_results, function(item) {
  match <- item$MatchedObjectDescriptor
  
  tibble(
    job_id       = match$PositionID,
    title        = match$PositionTitle,
    organization = match$OrganizationName,
    department   = match$DepartmentName,
    min_salary   = as.numeric(match$PositionRemuneration[[1]]$MinimumRange),
    max_salary   = as.numeric(match$PositionRemuneration[[1]]$MaximumRange),
    url          = match$PositionURI
  )
})

# View your extracted data
print(head(clean_jobs_df))


##To get the occupational series
##But this doesn't really help because it doesn't connect to the position names above.


library(httr2)
library(jsonlite)

# Define the endpoint URL
url <- "https://data.usajobs.gov/api/codelist/occupationalseries"

# Build and execute the request
req <- request(url) |> 
  req_headers(
    "Host" = "data.usajobs.gov",
    "User-Agent" = "your.email@example.com" # Replace with your actual email
  )

resp <- req_perform(req)

# Parse the JSON response
json_data <- resp |> 
  resp_body_string() |> 
  fromJSON(flatten = TRUE)

# Extract the data frame of occupational series
# The structure is nested under CodeList -> ValidValue
occ_series_df <- json_data$CodeList$ValidValue[[1]]

# View the first few rows
head(occ_series_df)


##Resources
#https://presentofcoding.substack.com/p/getting-usajobs-data