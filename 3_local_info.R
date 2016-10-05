
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

races <- c('Black', 'Hispanic', 'Asian', 'White', 'Other')
resolution <- 1/2


# Toy cities ----------

# checkerboard_illustration()
# ggsave('report/figs/toy.png', width = 5, height = 1.9)

# Methodological Illustration ----------------

# method_illustration()
# ggsave('report/figs/method.png', width = 3, height = 3)



cities <- list.files('data/cities')

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
		weighted_density = numeric(),
		n_per_hex = numeric(),
		H_X = numeric(),
		H_Y = numeric(),
		I_XY = numeric(),
		local.H_Y = numeric(),
		J = numeric(),
		weighted_J = numeric())
}


for(city in cities){
	tracts <- readOGR(dsn = paste0('data/cities/',city), layer = 'geo', verbose = FALSE)
	
	tracts@data$area <- tracts@data$ALAND / 1000^2	
	tracts <- tracts[tracts@data$total / tracts@data$area > 50,] # at least 50 people per km^2
	
	radius = 1/sqrt(85 * 111) * resolution # (roughly resolution km after lat-lon conversion)
	xx = spsample(tracts, type="hexagonal", cellsize=radius)
	print(paste0(city, ': ',  nrow(tracts@data), ' tracts || ', length(xx), ' grid cells'))
	xxpl = HexPoints2SpatialPolygons(xx)
	
	local_information <- function(tracts, grid){
		h <- function(i){
			window <- tracts[xxpl[i,],]@data[,c(races, 'total', 'area')]
			window <- window / (window$area) # total becomes density
			c(mean(window$total), 
			  4 * mutual_info(window[,races]) / resolution^2,  
			  H(colSums(window[,races]) /sum(window[,races])), 
			  nrow(window)) # returns estimated density and mutual info
		}
		df <- 1:length(xxpl) %>%
			as.matrix() %>%
			apply(MARGIN = 1, FUN = h) %>% 
			t %>% 
			as.data.frame()
		names(df) <- c('density', 'info', 'entropy', 'n')
		
		SpatialPolygonsDataFrame(xxpl, data = df)
	}
	
	
	
	# Define information measures on the grid 
	h <- function(i){
		window <- tracts[xxpl[i,],]@data[,c(races, 'total', 'area')]
		window <- window / (window$area) # total becomes density
		c(mean(window$total), 
		  4 * mutual_info(window[,races]) / resolution^2,  
		  H(colSums(window[,races]) /sum(window[,races])), 
		  nrow(window)) # returns estimated density and mutual info
	}
			
	# Compute measures on the grid and collect as df
	df <- 1:length(xxpl) %>%
		as.matrix() %>%
		apply(MARGIN = 1, FUN = h) %>% 
		t %>% 
		as.data.frame()
	names(df) <- c('density', 'info', 'entropy', 'n')
	row.names(df) <- paste0('ID', row.names(df))
	
	
	to_save <- SpatialPolygonsDataFrame(xxpl, data = df)
	tryCatch(writeOGR(to_save, paste0("throughput/grids/", city),'geo', driver = 'ESRI Shapefile', morphToESRI = TRUE),
			 error = function(e) NULL)
	
	# summarized output
	out <- data.frame(
		city = city, 
		area = sum(tracts@data$area),
		population = sum(tracts@data$total),
		density = sum(tracts@data$total) / sum(tracts@data$area),
		weighted_density = ((tracts@data$total^2 / tracts@data$area) %>% sum) / sum(tracts@data$total),
		n_per_hex = mean(df$n), 
		H_X = H(tracts@data$total / sum(tracts@data$total)),
		H_Y = tracts@data[,races] %>% entropy(),
		I_XY = tracts@data[,races] %>% mutual_info(),
		local.H_Y = weighted.mean(df$entropy, df$density),
		J = mean(df$info),
		weighted_J = weighted.mean(df$info, df$density)
	)
	
	cache <- rbind(cache, out)
	write.csv(cache, 'throughput/info_cache.csv', row.names = FALSE)	
}
