#------- DESCRIPTION ------
# First step to harmonize the party names, 
# that will be finished manually in a spreadsheet.
# Read the unique parties in the 'muni2019_results.csv',
# csv file, the unique long format csv build from the official results
# and make join it with the parties from the 2015 elections harmonized by Populate

# The parties left will be fixed manually in a spreadsheet. 
# Colors also be assigned manually.

#------- LIBRARIES & WD ------
library(tidyverse)
library(httr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#------- PARTIES ------

# Harmonized parties from the 2015 elections from Populate
dataset <- 'ds-elecciones-municipales-escanos'
url <- "https://data.populate.tools/visualizados/datasets/ds-elecciones-municipales-escanos.csv?filter_by_date=2015-05-24"

r <- GET(url, add_headers(authorization = "Bearer y8z3FkVkLeFInvvcYLa5tS", origin = 'http://visualizados.com'))
p15 <- content(r, "parsed") %>%
  distinct(location_id, political_party_slug, political_party_short_name, political_party_long_name) %>%
  mutate(
    location_id = str_pad(location_id, 5, pad = '0')
  ) %>% 
  rename(party_acronym = political_party_short_name)

# Get the unique parties with any seat from the 2019 elections
# join with the harmonized parties from 2015
# the ones left will be fixed manually in a spreadsheet
p19 <- read_csv('data/muni2019_results.csv') %>% 
  filter(party_seats > 0) %>% 
  distinct(location_id, muni_name, party_acronym, seats_available, party_seats, rank) %>% 
  left_join(p15) %>% 
  # reorder columns
  select(location_id, muni_name, party_acronym, political_party_slug, political_party_long_name, seats_available, party_seats, rank) 

# check NAs
p19 %>% 
  filter(is.na(political_party_slug)) %>% 
  nrow()
# 12689 to fix manually

write_csv(p19, 'data/muni2019_parties.csv')



