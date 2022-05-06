# Load Libraries
library(here)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(gert)

# Get List of URLs
url <- "https://www.ice.gov/coronavirus"
page <- read_html(url)

# Get the total detained population as a string
total_detained <-
  html_nodes(page, "table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  pluck(1) %>% 
  str_remove_all(.,"\t") %>% 
  str_remove_all(.,"DETAINED POPULATION1\nAS OF ") %>% 
  str_split(., "\n\n", n = 2) %>% 
  map(.,2) %>% 
  str_c()

# Total tested
total_tested <- 
  html_nodes(page, "table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  pluck(3) %>% 
  str_remove_all(.,"\t") %>% 
  str_remove_all(.,"DETAINEES TESTED\nAS OF ") %>% 
  str_split(., "\n\n", n = 2) %>% 
  map(.,2) %>% 
  str_c()

# Add date to summary values
covid_summary_totals <- 
  tibble(Date = format(Sys.time(), "%Y-%m-%d"), # CHANGE DATE HERE
         `Total Detained` = total_detained,
         `Total Tested` = total_tested)
covid_summary_totals$`Total Detained` <- as.numeric(gsub(",", "", covid_summary_totals$`Total Detained`))
covid_summary_totals$`Total Tested` <- as.numeric(gsub(",", "", covid_summary_totals$`Total Tested`))
covid_summary_totals

# Get total deaths and Total COVID-19
tibble_list <- 
  map(url, ~read_html(.x) %>% 
        html_nodes("table") %>% 
        html_table() %>% 
        pluck(2) %>% 
        filter(grepl("TOTAL", `Custody/AOR/Facility`)) %>% 
        select(., -`Custody/AOR/Facility`) %>% 
        rename(`Total COVID-19 Confirmed in Custody` = `Confirmed cases currently under isolation or monitoring`) %>% 
        rename(`Total Deaths` = `Detainee deaths3`) %>% 
        rename(`Total Cumulative COVID-19`=`Total confirmed COVID-19 cases4`)
      
  )

# Take the list of tibbles and bind them together in a single tibble
# and add an ID number
covid_summary_totals2 <- 
  bind_rows(
    map(tibble_list,
        ~pluck(.x)
    )) %>% 
  mutate(Date = format(Sys.time(), "%Y-%m-%d"), .before = `Total COVID-19 Confirmed in Custody`) #CHANGE DATE HERE
covid_summary_totals2$`Total COVID-19 Confirmed in Custody` <- as.numeric(gsub(",", "", covid_summary_totals2$`Total COVID-19 Confirmed in Custody`))
covid_summary_totals2$`Total Deaths` <- as.numeric(gsub(",", "", covid_summary_totals2$`Total Deaths`))
covid_summary_totals2$`Total Cumulative COVID-19` <- as.numeric(gsub(",", "", covid_summary_totals2$`Total Cumulative COVID-19`))

covid_summary_totals2

# Bind the tables and join them
covid_summary_totals <- 
  left_join(covid_summary_totals, covid_summary_totals2, by = "Date") %>% 
  relocate(`Total Cumulative COVID-19`, .after = `Total Detained`) %>% 
  relocate(`Total Deaths`, .after = `Total Tested`)
covid_summary_totals

# Write summary values to file
write_csv(covid_summary_totals,
          here("data/covid_summaries.csv"),
          append = TRUE,
          col_names = FALSE)

# Get the table of cases by facility
confirmed_cases <- html_nodes(page, "table") %>% 
  html_table() %>% 
  pluck(2) %>% 
  filter(!grepl("Field Office", `Confirmed cases currently under isolation or monitoring`)) %>% 
  filter(!grepl("Endeavors", `Confirmed cases currently under isolation or monitoring`)) %>% 
  filter(!grepl("TOTAL", `Custody/AOR/Facility`))

# Add date to cases by facility
confirmed_cases <- 
  mutate(confirmed_cases, Date = format(Sys.time(), "%Y-%m-%d"), .before = `Custody/AOR/Facility`)
confirmed_cases

# Write out data by facility
write_csv(confirmed_cases,
          here("data/covid_by_facility.csv"),
          append = TRUE,
          col_names = FALSE)
