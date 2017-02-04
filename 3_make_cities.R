library(sp, quietly = TRUE)
library(grid, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(stringr, quietly = TRUE)
library(tidyverse, quietly = TRUE)

city_df <- read_csv('assumptions/cities.csv')
cities <- unique(city_df$name)
cities <- cities %>% setdiff(list.dirs('data/cities', full.names = F))

city_df <- city_df %>% 
    filter(name %in% cities)

v_lookup_code <- Vectorize(tigris::lookup_code)

city_df <- city_df %>% 
    dplyr::mutate(state_fips = v_lookup_code(state),
           state_fips = stringr::str_sub(state_fips, start = -4, end = -3),
           state_fips = as.integer(state_fips))

states <- city_df$state %>% 
	str_split('--') %>% 
	unique()

the_map <- states %>%
    purrr::map(~readOGR(dsn = paste0('data/states/',.x), 
                       layer = 'geo',
                       verbose = FALSE)) %>% 
    reduce(rbind, makeUniqueIDs = TRUE)

if(!dir.exists('data/cities')){
	dir.create('data/cities')
}

make_city <- function(city){
    this_city <- city_df %>% filter(name == city)
    
    counties <- this_city$county
    states   <- this_city$state_fips
    states   <- str_pad(states, width = 2, pad = 0)
    right_county <- as.integer(as.character(the_map@data$COUNTYFP)) %in% counties
    right_state  <- the_map@data$STATEFP %in% states
    
    the_map[right_county & right_state,]
}

writer <- function(tracts, city){
    writeOGR(tracts, 
             paste0("data/cities/", city),
             'geo', 
             driver = 'ESRI Shapefile', 
             morphToESRI = TRUE)
}
safe_writer <- safely(writer)

cities %>% 
    as.list() %>% 
    map(make_city) %>% 
    walk2(cities, safe_writer)


