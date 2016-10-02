library(sp, quietly = TRUE)
library(compx, quietly = TRUE)
library(acs, quietly = TRUE)
library(plyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(readr, quietly = TRUE)
library(ggmap, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(ggrepel, quietly = TRUE)
library(grid, quietly = TRUE)
library(rgdal, quietly = TRUE)

resolution <- 0.5

cities <- list.files('data/cities')
# cities <- c('Boston')

if(!dir.exists('throughput/grids')){
	dir.create('throughput/grids')
}

if(!dir.exists('throughput/grid_tracts')){
	dir.create('throughput/grid_tracts')
}

for(city in cities){
	
	tracts <- readOGR(dsn = paste0('data/cities/',city), layer = 'geo', verbose = FALSE)
	
	tracts@data$area <- tracts@data$ALAND / 1000^2	
	tracts <- tracts[tracts@data$total / tracts@data$area > 50,] # at least 50 people per km^2
	
	radius = 1/sqrt(85 * 111) * resolution # (roughly resolution km after lat-lon conversion)
	xx = spsample(tracts, type="hexagonal", cellsize=radius)
	print(paste0(city, ': ',  nrow(tracts@data), ' tracts || ', length(xx), ' grid cells'))
	xxpl = HexPoints2SpatialPolygons(xx)
	cell_area <- rgeos::gArea(xxpl[1])
	
	d <- data_frame(cell = integer(), tract = numeric(), area = numeric())
	
	for(i in 1:length(xxpl)){ # for i in 1:nrow(xxpl)
		window <- tracts[xxpl[i,],]
		poly_i <- raster::intersect(xxpl[i,], window)
		# areas <- rgeos::gArea(pi, byid = T)
		areas <- sapply(poly_i@polygons, function(x) x@area)
		full_areas <- sapply(window@polygons, function(x) x@area)
		d <- rbind(d, data_frame(cell = i, tract = window@data$GEOID, area = areas, full_area = full_areas))
	}
	
	names(d) <- c('cell', 'tract', 'area', 'full_area')
	d <- d %>%
		mutate(weight = area / full_area)
	d
	write_csv(d, paste0('throughput/grid_tracts/', city,'.csv'))
}
	
	



