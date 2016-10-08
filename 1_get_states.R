# library(sp, quietly = TRUE)
library(acs, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(stringr, quietly = TRUE)
library(readr)

# get list of states

cities <- readr::read_csv('assumptions/cities.csv')
cities$state <- word(cities$state, -1)

if(!dir.exists('data')){
	dir.create('data')
}

if(!dir.exists('data/states')){
	dir.create('data/states')
}

# Define data-getter

f <- function(state){
	counties <- acs::acs.fetch(geography=acs::geo.make(state=state, county="*"), 
				   endyear = 2014,
				   table.number="B01003")
	counties <- as.numeric(geography(counties)[[3]])
	
	race <- acs::acs.fetch(endyear = 2014,
						   span = 5,
						   geography = acs::geo.make(state = state, county = counties, tract = '*', block.group = '*'),
						   table.number = 'B03002',
						   col.names = "pretty")
	
	race <- cbind(data.frame(race@geography),
				  data.frame(race@estimate)) %>%
		tbl_df() 
	
	race_lookup <- read_csv('assumptions/race_lookup.csv')
	
	race <- race %>% 
		gather(key = 'original_race', value = 'n', -(NAME:blockgroup)) %>% 
		left_join(race_lookup, by = c('original_race' = 'original')) %>% 
		group_by(NAME, state, county, tract, blockgroup, aggregated) %>% 
		summarise(n = sum(n)) %>% 
		ungroup() %>% 
		filter(!is.na(aggregated)) %>% 
		spread(key = aggregated, value = n, fill = 0)
	
	race <- race %>% 
		mutate(GEOID = paste0(str_pad(state, 2, 'left', pad = '0'),
							  str_pad(county, 3, 'left', pad = '0'),
							  str_pad(tract, 6, 'left', pad = '0'),
							  str_pad(blockgroup, 1, 'left', pad = '0')))
	
	tracts <- tigris::block_groups(state = state, county = counties, cb=TRUE)
	tracts@data$area <- tracts@data$ALAND / 1000^2
	
	tracts <- tigris::geo_join(tracts, race, "GEOID", "GEOID")
	rgdal::writeOGR(tracts, paste0("data/states/", state),'geo', driver = 'ESRI Shapefile', morphToESRI = TRUE)
}

# Get all the states
cities <- c(unique(toupper(cities$state[nchar(cities$state) == 2])), 'DC')
for(state in cities){
	if(!is.element(state,list.files('data/states'))){
		print(state)
		tryCatch(f(state), error = function(e) NULL)
	}
}