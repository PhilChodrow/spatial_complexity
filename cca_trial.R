library(CCA)
library(rgdal)
library(rgeos)
library(leaflet)
library(ggplot2)

MA <- readOGR(dsn = 'data/states/NM', layer = 'geo', verbose = FALSE)
MA <- MA[MA$total > 0,]
MA <- MA[MA$total / MA$ALAND > .0000,] # density cutoff
centroids <- MA %>% gCentroid(byid = TRUE) 
MA@data <- cbind(MA@data,centroids@coords) 

pop.list <- cca(MA@data[,c('x', 'y')],s=.02)

MA@data <- MA@data %>% left_join(pop.list$cluster, by = c('x' = 'long', y = 'lat'))

counts <- MA@data %>% 
	group_by(cluster_id) %>% 
	summarise(pop = sum(total),
			  n = n()) %>% 
	arrange(desc(pop)) %>% 
	filter(pop > 5e5)

MA@data <- MA@data %>% left_join(counts)

MA <- MA[!is.na(MA@data$pop),] 

# pop.list$cluster %>% 
# 	ggplot() + 
# 	aes(x = long, y = lat, color = cluster_id) + 
# 	geom_point()
# 
# 
# 
# MA@data <- cbind(MA@data, lag(pop.list$cluster))

colors <- MA@data$cluster_id
pal <- colorNumeric(
	palette = 'Set3',
	domain = colors
)

map <-leaflet() %>%
	addProviderTiles("CartoDB.Positron") %>%
	addPolygons(data = MA, 
				fillColor = ~pal(colors), 
				color = "#b2aeae", # you need to use hex colors
				fillOpacity = 0.7, 
				weight = 1, 
				smoothFactor = 0.2)
map