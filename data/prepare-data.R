#Code to process the original ".tsv" file
library(dplyr)
library(DT)
library(readr)


fileaddress <- "./FileToRead/Table_Test.tsv"

table_raw <- read_tsv(fileaddress) %>%
  as.data.frame() %>%
  select(
    name = `Name of predictor`,
    doi = `Access (DOI)`,
    year = Year,
    description = `Brief description`,
    more_description = `More description`,
    datasets = `Datasets used`,
    label = Label,
    reported_threshold = `Threshold/Class  reported  by authors`,
    threshold = Threshold,
    threshold_type = `Type of threhsold`,
    availability = Availability,
    online_tool = `Link to the  online tools`,
    server_activity = `Web  server  activity`,
    server_functionality = `Web  server  functionality`,
    model_repo = `Model repository`,
    model_repo_link = `Model repository link`,
    training_repo = `Training repository`,
    training_repo_link = `Training repository link`,
    excluded = Excluded
  )


#Function to exclude non-relevant tools
omit_excluded_tools <- function(df) {
  excluded_rows <- df %>% filter(excluded == TRUE)
  num_excluded <- nrow(excluded_rows)
  excluded_names <- excluded_rows$`name`
  
  if (num_excluded > 0) {
    message(num_excluded, " tool(s) have been excluded, which are: ", 
            paste(excluded_names, collapse = ", "))
    } else {message("No tools were excluded.")}
  
  df %>% filter(excluded != TRUE)}

#Function to exclude non-relevant tools
relevant_tools <- omit_excluded_tools(table_raw)


#Working on citation numbers updating automatically 2 libraries: "rcrossref", "scholar"
library(rcrossref) 
res <- cr_works(dois = "10.1093/bib/bbab065")
print(res$data$is.referenced.by.count) #79 


saveRDS(relevant_tools, file = "./FileToRead/LLPS-tools_relevant.RDS")



table_complete_test <- relevant_tools %>% 
  mutate(
    DOI = ifelse(
      !is.na(`Access (DOI)`) & `Access (DOI)` != "",
      paste0("<a href='https://doi.org/", `Access (DOI)`, "'>", `Access (DOI)`, "</a>"),
      NA),
    
  )
