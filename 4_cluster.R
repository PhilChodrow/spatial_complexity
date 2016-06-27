library(dplyr, quietly = TRUE)
library(compx, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(rgeos, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(Matrix, quietly = TRUE)
library(maptools, quietly = TRUE)
library(profvis, quietly = TRUE)
library(readr, quietly = TRUE)
library(ggrepel, quietly = TRUE)
library(caTools, quietly = TRUE)

columns <- c('White', 'Black', 'Asian', 'Hispanic', 'Other')

# --------------------

cities <- list.files('data/cities/')
cities <- cities[length(cities):1]


if(file.exists('throughput/loss_curves.csv')){
	cache <- read_csv('throughput/loss_curves.csv')
	cities <- cities[!is.element(cities, cache$city)]
} else {
	cache <- data.frame('I_XY' = numeric(), 'nclust' = numeric(), 'city' = character())
}

if(!dir.exists('throughput/clusterings')){
	dir.create('throughput/clusterings')
}

for(city in cities){
	print(city)
	spdf <- readOGR(dsn = paste0('data/cities/', city), layer = 'geo')
	spdf <- spdf[sum(spdf@data[,columns]) != 0,]
	df <- spdf@data[,columns]
	row.names(df) <- 1:nrow(df)
	print('Computing adjacency matrix')
	constraint <- gRelate(spdf, byid = TRUE, pattern = '****1****')
	a <- info_clust(df, constraint)
	saveRDS(a, file = paste0('throughput/clusterings/',city))
	added <- data.frame('I_XY' = c(0,a$height), 'nclust' = 1:nrow(spdf), 'city' = city)
	cache <- rbind(cache, added)
	write_csv(cache,'throughput/loss_curves.csv')
}
