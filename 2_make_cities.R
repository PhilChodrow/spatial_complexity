
clustering_distance <- .02
population_threshold <- 5e5

library(sp, quietly = TRUE)
library(compx, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(readr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(grid, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(stringr, quietly = TRUE)
library(rgeos)
library(osc)

states <- list.files('data/states')

state_polys <- list()
for(state in states){
	print(paste('Loading', state))
	state_poly <- readOGR(dsn = paste0('data/states/',state), layer = 'geo', verbose = FALSE)
	state_poly <- state_poly[state_poly$total > 0,]
	state_poly@data$state <- state
	
	state_polys[state] <- spChFIDs(state_poly,as.character(paste(state, rownames(as(state_poly,"data.frame")))))
}
state_polys <- do.call(rbind, state_polys)
centroids <- state_polys %>% gCentroid(byid = TRUE) 
state_polys@data <- cbind(state_polys@data,centroids@coords) 

clusters <- cca(state_polys@data[,c('x', 'y')],clustering_distance) 

state_polys@data <- state_polys@data %>% left_join(clusters$cluster, by = c('x' = 'long', y = 'lat'))

counts <- state_polys@data %>% 
	group_by(cluster_id) %>% 
	summarise(population = sum(total)) %>% 
	arrange(desc(population)) %>% 
	filter(population > population_threshold) 

state_polys@data <- state_polys@data %>% left_join(counts)

state_polys <- state_polys[!is.na(state_polys@data$population),] 

cbsas <- tigris::core_based_statistical_areas(cb = T)

if(!dir.exists('data/cities')){
	dir.create('data/cities')
}


for(id in counts$cluster_id){
	
	city_polys <- state_polys[state_polys@data$cluster_id == id,]
	cbsa <- cbsas[city_polys,]
	
	if(length(cbsa) == 1){
		name <- cbsa@data$NAME[1]
	}else{
		pops <- 1:length(cbsa) %>% 
			as.matrix() %>% 
			apply(MARGIN = 1, function(i) sum(city_polys[cbsa[i,],]@data$total))
		name <- cbsa@data$NAME[which.max(pops)]
	}
	
	city <- word(name,sep = '-')
	city <- word(city, sep = ',')
	state <- word(name, 2, sep = ', ')
	state <- word(state, sep = '-')
	
	name <- paste0(city, ', ', state)

	tryCatch(writeOGR(city_polys, paste0('data/cities/',name), 
					  'geo', 
					  driver = 'ESRI Shapefile', morphToESRI = TRUE), 
			 error = function(e) NULL)
}
