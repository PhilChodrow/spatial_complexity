setwd("~/projects/spatial_complexity/tech_report")

library(readr)
library(dplyr)
library(compx)
library(tidyr)
library(rgdal)


cities <- read_csv('msas.csv')

cities <- cities %>%
	separate(name, c('name','state'), sep = ',') %>%
	mutate(state = substr(county_name, start  = nchar(county_name) - 1, stop  = nchar(county_name)),
		   county_num = as.integer(substr(county_num, start = 3, stop = 5))) %>%
	select(-county_name)

county_list <- aggregate(county_num ~ state, c, data = cities)

f <- function(state){
	
	counties <- county_list$county_num[county_list$state == state][[1]]
	
	race <- acs::acs.fetch(endyear = 2014,
						   span = 5,
						   geography = acs::geo.make(state = state, county = counties, tract = '*', block.group = '*'),
						   table.number = 'B03002',
						   col.names = "pretty")
	
	race <- cbind(data.frame(race@geography),
				  data.frame(race@estimate)) %>%
		tbl_df() %>%
		mutate(GEOID = paste0(str_pad(state, 2, 'left', pad = '0'),
							  str_pad(county, 3, 'left', pad = '0'),
							  str_pad(tract, 6, 'left', pad = '0'),
							  str_pad(blockgroup, 1, 'left', pad = '0')))
	
	race$Hispanic <- race[names(race)[!grepl('.Not.Hispanic.or.Latino.',names(race))]] %>%
		select(-(`NAME`:`Hispanic.or.Latino.by.Race..Hispanic.or.Latino.`)) %>%
		select(-GEOID) %>%
		rowSums()
	
	others <- c('Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..American.Indian.and.Alaska.Native.alone',
				'Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Native.Hawaiian.and.Other.Pacific.Islander.alone',
				'Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Some.other.race.alone',
				'Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Two.or.more.races.',
				'Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Two.or.more.races..Two.races.including.Some.other.race',
				'Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Two.or.more.races..Two.races.excluding.Some.other.race..and.three.or.more.races')
	
	race$Other <- race[others] %>% rowSums()
	
	race <- race %>%
		select(Hispanic, Other,
			   White = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..White.alone,
			   Black = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Black.or.African.American.alone,
			   Asian = Hispanic.or.Latino.by.Race..Not.Hispanic.or.Latino..Asian.alone,
			   GEOID)
	
	tracts <- tigris::block_groups(state = state, county = counties, cb=TRUE)
	
	tracts <- tigris::geo_join(tracts, race, "GEOID", "GEOID")
	
	tracts@data$area <- 1:length(tracts) %>%
		as.matrix() %>%
		apply(MARGIN = 1,FUN = function(i) tracts@polygons[[i]]@Polygons[[1]]@area * 85 * 111) # lat/lon
	
	tracts@data <- tracts@data %>%
		mutate(name = row.names(.),
			   total = Hispanic + Other + White + Black + Asian,
			   density = total / area)
	
	tracts <- tracts[!is.na(tracts@data$total),]
	tracts <- tracts[tracts@data$total > 0,]
	writeOGR(tracts, paste0("data/", state),'geo', driver = 'ESRI Shapefile', morphToESRI = TRUE)
	return()
}

for(state in unique(county_list$state)){
	if(!dir.exists(paste0('data/', state))) {
		print(state)
		try(f(state))
	}
}


# -----------------
# -----------------
# -----------------

cities <- city_list('msas.csv')

h <- function(city){
	sub <- cities[cities$name == city,]
	g <- function(i){
		state    <- sub$state[i]
		tracts <- readOGR(dsn = paste0('data/',state), layer = 'geo', verbose = FALSE)
		tracts@data <- tracts@data %>%
			mutate(county = substr(GEOID, start = 3, stop = 5),
				   county = as.integer(county))
		counties <- unlist(sub$county[i])
		tracts <- tracts[is.element(tracts@data$county, counties),]
		tracts   <- spChFIDs(tracts,as.character(paste(state,rownames(as(tracts,"data.frame")),sep="_")))
		tracts@data$area <- 1:length(tracts) %>%
			as.matrix() %>%
			apply(MARGIN = 1,FUN = function(i) tracts@polygons[[i]]@Polygons[[1]]@area * 85 * 111) # lat/lon
		tracts
	}
	tracts <- lapply(1:nrow(sub), g)
	tracts <- do.call(rbind, tracts)
	density <- sum(tracts@data$total) / sum(tracts@data$area)
	weighted_density <- sum(tracts@data$total^2) / (sum(tracts@data$area) * sum(tracts@data$total))
	return(c(density, weighted_density))
}

density_list = data.frame(name = character(),
						  density = numeric(),
						  weighted_density = numeric())

for(name in unique(cities$name)){
	print(name)
	density = tryCatch(h(name), error = function(e) NULL)
	if(!is.null(density)){
		add <- data.frame(name = name,
						  density = density[1],
						  weighted_density = density[2])
		density_list <- rbind(density_list, add)	
	}
	
}

write_csv(density_list, 'density_list.csv')

