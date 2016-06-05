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


ua <- read_table('ua_list_all.txt')
ua$state = word(ua$NAME, -1)

f <- function(state){
	counties <- acs.fetch(geography=geo.make(state=state, county="*"), 
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
			   GEOID) %>%
		mutate(total = Hispanic + White + Black + Asian + Other)

	tracts <- tigris::block_groups(state = state, county = counties, cb=TRUE)
	tracts@data$area <- tracts@data$ALAND / 1000^2
	
	tracts <- tigris::geo_join(tracts, race, "GEOID", "GEOID")
	writeOGR(tracts, paste0("data/", state),'geo', driver = 'ESRI Shapefile', morphToESRI = TRUE)
}

for(state in unique(toupper(ua$state[nchar(ua$state) == 2]))){
	if(!is.element(state,list.files('data'))){
		print(state)
		tryCatch(f(state), error = function(e) NULL)
	}
}
f('DC') # need to get DC separately








