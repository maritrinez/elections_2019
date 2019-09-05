#------- DESCRIPTION ------
# Read the wide format csv files given by the 
# Ministerio del Interior and parse them in a 
# unique long format csv.

#------- LIBRARIES & WD ------
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#------- MUNICIPALES ------
# Colnames (vars content taken from intructions.pdf)
n_uq <- c(
  'c_autonomous_region_id',
  'c_province_id',
  'c_fix_9',
  'c_muni_id',
  'c_muni_district',
  'c_muni_name',
  'c_fix_99_a',
  'c_fix_99_b',
  'n_electoral_tables',
  'n_census_size',
  'n_census_counted',
  'p_percent_census_counted',
  'n_voters',
  'p_percent_voters',
  'n_abstention',
  'p_percent_abstention',
  'n_blanks',
  'p_percent_blanks',
  'n_nulls',
  'p_percent_nulls',
  'n_seats_available'
)
n_rep <- c(
  'c_party_id',
  'c_party_acronym',
  'n_party_voters',
  'p_percent_party_voters',
  'n_party_seats'
)

namesMuni <- c(n_uq, paste(n_rep, rep(1:50, each = 5, len = 250), sep = '-'))


# Function to convert fix wide strings to percent
# '000008765' -> -87.65
str2percent <- function (string) {
  l <- nchar(string)
  pct <- as.numeric(paste(str_sub(string, 1, l - 2), str_sub(string, l - 1, l), sep = '.'))

  return (pct)
}

# Function to convert fix wide strings to negative values
# It happens when the number of voters are bigger than the census
# '00000-87' -> -87
str2negative <- function (string) {
  neg <- paste0('-', unlist(str_split(string, '-'))[2])
  return (neg)
}


# Function to read & clean every csv file
read_muni_file <- function (file, names) {
  print(file)
  tmp <- read_delim(glue::glue('raw_data/results/municipales2019/{file}'), col_names=FALSE, delim = ';') %>% 
    setNames(names) %>% 
    mutate(
      c_muni_name = iconv(from = 'latin1', to = 'utf-8', c_muni_name),
      c_location_id = paste0(c_province_id, c_muni_id) 
    ) %>% 
    gather(variable, value, -1:-21, -c_location_id) %>% 
    separate(variable, c('variable', 'n_rank'), sep = '-') %>% 
    group_by(c_location_id, n_rank) %>% 
    spread(variable, value) %>%
    ungroup() %>%
    mutate(
      c_party_acronym = iconv(from = 'latin1', to = 'utf-8', c_party_acronym),
      n_abstention = if_else(!grepl('-', n_abstention), n_abstention, str2negative(n_abstention))
    ) %>% 
    mutate_at(vars(starts_with("c_")), as.character) %>% 
    mutate_at(vars(starts_with("c_")), str_trim) %>%
    mutate_at(vars(starts_with("n_")), as.numeric) %>% 
    mutate_at(vars(starts_with("p_")), str2percent) %>% 
    filter(n_party_voters > 0)
  print(file)
  return (tmp)
}


# Get csv files list (the unzip throws an error)
files <- list.files('raw_data/results/municipales2019/', pattern = 'MUNICIPIOS.*csv$')



# Read and bind the files
municipales2019 <- do.call(rbind, lapply(files, function(x) read_muni_file(x, namesMuni))) %>% 
  setNames(str_replace(colnames(.), pattern = '^c_|^n_|^p_', replacement = ''))

# Check NAs introduced by coercion 
nas <- municipales2019 %>% filter_all(any_vars(is.na(.)))  

write_csv(municipales2019, 'data/muni2019_results.csv')




#------- EUROPEAS ------
# This is an adaptation from the municipalities, 
# It works but i don't use it so I didn't store the original files
# TODO::  find them and make it work

n_uq_eu <- c(
  'c_autonomous_region_id',
  'c_province_id',
  'c_fix_9',
  'c_muni_id',
  'c_muni_district',
  'c_muni_name',
  'c_fix_99_a',
  'c_fix_99_b',
  'n_electoral_tables',
  'n_census_size',
  'n_census_counted',
  'p_percent_census_counted',
  'n_voters',
  'p_percent_voters',
  'n_abstention',
  'p_percent_abstention',
  'n_blanks',
  'p_percent_blanks',
  'n_nulls',
  'p_percent_nulls'
)
n_rep_eu <- c(
  'c_party_id',
  'c_party_acronym',
  'n_party_voters',
  'p_percent_party_voters'
)

namesEU <- c(n_uq_eu, paste(n_rep_eu, rep(1:50, each = 4, len = 200), sep = '-'))

# Get csv files list inside the zip
zip <- 'raw_data/results/...' # get zip from original results
files <- unzip(zip, list = TRUE) %>% 
  filter(grepl('^europeas.*RESULTADOS_MUNICIPIOS.*csv$', Name)) %>% 
  .$Name


# Read and bind the files
europeas2015 <- do.call(rbind, lapply(files, function(x) read_muni_file(x)))



read_eu_file <- function (file) {
  print(file)
  tmp <- read_delim(unz(zip, file), col_names=FALSE, delim = ';') %>% 
    setNames(names) %>% 
    mutate(
      c_muni_name = iconv(from = 'latin1', to = 'utf-8', c_muni_name),
      c_location_id = paste0(c_province_id, c_muni_id) 
    ) %>% 
    gather(variable, value, -1:-21, -c_location_id) %>% 
    separate(variable, c('variable', 'n_rank'), sep = '-') %>% 
    group_by(c_location_id, n_rank) %>% 
    spread(variable, value) %>%
    ungroup() %>%
    mutate(
      c_party_acronym = iconv(from = 'latin1', to = 'utf-8', c_party_acronym),
      n_abstention = if_else(!grepl('-', n_abstention), n_abstention, str2negative(n_abstention))
    ) %>% 
    mutate_at(vars(starts_with("c_")), as.character) %>% 
    mutate_at(vars(starts_with("c_")), str_trim) %>%
    mutate_at(vars(starts_with("n_")), as.numeric) %>% 
    mutate_at(vars(starts_with("p_")), str2percent) %>% 
    filter(n_party_voters > 0)
  
  return (tmp)
}
