setwd("~/projects/spatial_complexity/tech_report")

library(sp)
library(compx)
library(acs)
library(plyr)
library(dplyr)
library(readr)
library(ggmap)
library(tidyr)
library(magrittr)
library(ggrepel)
library(grid)
library(rgdal)
library(tigris)
library(stringr)
library(acs)



ua_poly <- tigris::urban_areas(cb = TRUE)
ua <- read_tsv('ua_list_all.txt')
ua$state = word(ua$NAME, -1)

cities <- ua %>% 
	filter(POP > 1e6) 

states <- cities$state %>% 
	str_split('--') %>% 
	do.call(c,.) %>% 
	unique()

cities <- cities$NAME

shapes <- list()
for(state in states){
	shapes[state] <- readOGR(dsn = paste0('data/',state), layer = 'geo', verbose = FALSE)
}


for(city in cities){
	print(city)
	city_info <- ua %>%
		filter(NAME == city) 
	
	city_poly <- ua_poly[ua_poly@data$NAME10 == city,]
	
	
	city_components <- list()
	for(state in str_split(city_info$state, '--')[[1]]){
		state_sub <- shapes[[state]][city_poly,]
		city_components[state] <-  spChFIDs(state_sub,
											as.character(paste(state,rownames(as(state_sub,"data.frame")),
															   sep="_")))
	}
	
	city_polys <- do.call(rbind, city_components)

	tryCatch(writeOGR(city_polys, paste0("cities/", city),'geo', driver = 'ESRI Shapefile', morphToESRI = TRUE),
			 error = function(e) NULL)
}








