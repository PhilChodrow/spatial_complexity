
library(sp, quietly = TRUE)
library(compx, quietly = TRUE)
library(plyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(readr, quietly = TRUE)
library(ggmap, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(ggrepel, quietly = TRUE)
library(grid, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(data.table)

races <- c('Black', 'Hispanic', 'Asian', 'White', 'Other')

cities <- list.files('data/cities')
resolution <- 1

cities <- list.files('data/cities')
# cities <- c('Boston')

if(!dir.exists('throughput/grids')){
	dir.create('throughput/grids')
}

if(!dir.exists('throughput/grid_tracts')){
	dir.create('throughput/grid_tracts')
}


# MAIN COMPUTATION ---------------

# Cache logic

if(file.exists('throughput/info_cache.csv')){
	cache <- read_csv('throughput/info_cache.csv')
	cities <-  cities[!is.element(cities, cache$city)]
}else{
	cache <- data.frame(
		city = character(),
		area = numeric(),
		population = numeric(),
		density = numeric(),
		I_XY = numeric(),
		J_XY = numeric())
}
city = 'Boston'
for(city in cities){
	tracts <- readOGR(dsn = paste0('data/cities/',city),
	                  layer = 'geo',
	                  verbose = FALSE)
	grid_dir        <- paste0('throughput/grids/',city)
	grid_tract_path <- paste0('throughput/grid_tracts/', city, '.csv')

	if(file.exists(grid_tract_path) & dir.exists(grid_dir)){
	  grid_tract <- read_csv(grid_tract_path)
	  grid <- readOGR(dsn = paste0('throughput/grids/',city),
	                  layer = 'geo',
	                  verbose = FALSE)

	  print(city)
	  analysis <- info_analysis(tracts, 
	                            columns = races, 
	                            resolution = resolution,
	                            grid_polys = grid,
	                            grid_tract = grid_tract)
	}else{
	  
	  tryCatch(
	    {print(city)
	    analysis <- info_analysis(tracts, 
	                              columns = races, 
	                              resolution = resolution)
	    write_csv(analysis$grid_tract, paste0('throughput/grid_tracts/',city, '.csv'))
	    writeOGR(analysis$grid, paste0("throughput/grids/", city),
	                     'grid', 
	                     driver = 'ESRI Shapefile', 
	             morphToESRI = TRUE)},
	           error = function(e) e)
	}

	# summarized output
	out <- data.frame(
	  city       = city,
	  area       = sum(tracts@data$area),
	  population = sum(tracts@data$total),
	  density    = sum(tracts@data$total) / sum(tracts@data$area),
	  H_Y        = analysis$H_Y,
	  I_XY       = analysis$I_XY,
	  J_XY       = analysis$J_XY
	)

	cache <- rbind(cache, out)
	write.csv(cache, 'throughput/info_cache.csv', row.names = FALSE)
}
