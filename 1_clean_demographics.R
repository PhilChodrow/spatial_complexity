library(tidyverse)
library(magrittr)


if(!dir.exists('data')){
    dir.create('data')
}

read_lookup <- function(assumptions_path, years){
  tract_names <- c('tractid', 'trtid10')
  lookup <- read_csv(assumptions_path)
  expand.grid(colname = lookup$colname, 
              year = years) %>% 
    left_join(lookup) %>% 
    mutate(colname = ifelse(colname %in% tract_names, 
                            colname, 
                            paste0(colname, stringr::str_sub(year, -2)))) %>%
    select(-year)
}

years <- c(1970, 1980, 1990, 2000, 2010)
assumptions_path <- 'assumptions/column_lookup.csv'
lookup <- read_lookup(assumptions_path, years)

data_dir <- "~/normalized_census/LTDB_Std_All_fullcount/"

get_data <- function(data_dir, lookup, year){
  data_path <- paste0(data_dir, 'LTDB_Std_', year, '_fullcount.csv')
  data <- read.csv(data_path)
  
  colnames(data) %<>%
    stringr::str_to_lower()   
  colnames(data)[1] <- 'tractid'  
  
  year_lookup <- lookup %>% 
    filter(colname %in% colnames(data))
  
  data %>% tbl_df() %>% 
    select_(.dots = year_lookup$colname) %>% 
    gather(race, n, -tractid) %>% 
    mutate(race = plyr::mapvalues(race, 
                                  from = year_lookup$colname, 
                                  to = year_lookup$cleaned_colname, 
                                  warn_missing = FALSE)) %>% 
    mutate(tractid = as.character(tractid)) %>% 
    dplyr::group_by(tractid, race) %>% 
    dplyr::summarise(n = sum(n)) %>% 
    dplyr::mutate(year = year) %>% 
    dplyr::select(tractid, year, race, n) %>% 
    ungroup()
}

years %>% 
  purrr::map( ~ get_data(data_dir, lookup, .)) %>% 
  reduce(rbind) %>% 
  mutate(tractid = as.character(tractid)) %>% 
  tidyr::complete(tractid, year, race, fill = list(n = 0)) %>% 
  dplyr::rename(t = year) %>% 
  mutate(tract = as.character(tractid),
         n = ifelse(is.na(n), 0, n)) %>% 
  dplyr::rename(group = race) %>% 
  dplyr::select(-tractid) %>%
  write_csv('data/demographics.csv')