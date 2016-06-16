library(sp, quietly = TRUE)
library(compx, quietly = TRUE)
library(plyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(readr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(grid, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(stringr, quietly = TRUE)


city_df <- read_csv('assumptions/cities.csv')
cities <- unique(city_df$name)
cities <- cities[!is.element(cities, list.dirs('cities', full.names = FALSE))]

states <- city_df$state %>% 
	str_split('--') %>% 
	do.call(c,.) %>% 
	unique()

shapes <- list()
for(state in states){
	print(paste('Loading', state))
	shapes[state] <- readOGR(dsn = paste0('data/states/',state), layer = 'geo', verbose = FALSE)
}

if(!dir.exists('data/cities')){
	dir.create('data/cities')
}




for(city in cities){
	print(city)
	city_info <- city_df %>%
		filter(name == city) 
	
	city_components <- list()
	for(i in 1:nrow(city_info)){
		state <- city_info$state[i]
		county <- city_info$county[i]
		component <- shapes[[state]]
		component <- component[as.integer(as.character(component@data$COUNTYFP)) == county,]
		city_components[paste(state, county, sep = '_')] <- spChFIDs(component,
																	  as.character(paste(state, county, rownames(as(component,"data.frame")))))
	}
	city_polys <- do.call(rbind, city_components)
	city_polys <- city_polys[city_polys@data$total != 0,]
	tryCatch(writeOGR(city_polys, paste0("data/cities/", city),'geo', driver = 'ESRI Shapefile', morphToESRI = TRUE),
			 error = function(e) NULL)
}
	
	
	