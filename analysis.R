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

setwd("~/projects/spatial_complexity/tech_report")


# Toy cities ----------

# checkerboard_illustration()
# ggsave('report/figs/toy.png', width = 5, height = 1.9)

# Methodological Illustration ----------------

# method_illustration()
# ggsave('report/figs/method.png', width = 3, height = 3)




# MAIN COMPUTATION ---------------

# Prep city list 

# cities <- city_list('msas.csv')
# density_list <- read_csv('density_list.csv') %>% filter(density > 200)
# cities <- cities %>% filter(city == 'New York City')


# Cache logic
cities <- list.files('cities') %>%
	sort(decreasing = T)

if(file.exists('cities_cache.csv')){
	cache <- read_csv('cities_cache.csv')
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
	races <- c('Black', 'Hispanic', 'Asian', 'White', 'Other')
	tracts <- readOGR(dsn = paste0('cities/',city), layer = 'geo', verbose = FALSE)
	tracts <- tracts[tracts@data$total > 0,]
	tracts@data$area <- tracts@data$ALAND / 1000^2		
	# tracts <- tracts[tracts@data$area < 20,] # careful with this
	
	xx = spsample(tracts, type="hexagonal", cellsize=.01)
	print(paste0(city, ': ',  nrow(tracts@data), ' tracts || ', length(xx), ' grid cells'))
	xxpl = HexPoints2SpatialPolygons(xx)
	grid_radius = sqrt(85 * 111) * .01
	
	
	# Define information measures on the grid 
	h <- function(i){
		window <- tracts[xxpl[i,],]@data[,c(races, 'total', 'area')]
		window <- window / (window$area) # total becomes density
		c(mean(window$total), 
		  4 * mutual_info(window[,races]) / grid_radius^2, 
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
	write.csv(cache, 'cities_cache.csv', row.names = FALSE)	
}

	



	# Make the grid  

	



# Viz 1 ------------------
lm_eqn <- function(df){
	m <- lm(weighted_J ~ density, data = df) 
	eq <- substitute(italic(y) == a + b %.% ~'log' ~italic(rho)*","~~italic(r)^2~"="~r2, 
					 list(a = format(coef(m)[1], digits = 2), 
					 	 b = format(coef(m)[2], digits = 2), 
					 	 r2 = format(summary(m)$adj.r.squared, digits = 3)))
	as.character(as.expression(eq))                 
}

cache %>% 
	filter(!grepl('Los Angeles', city)) %>%
	ggplot(aes(x = density, y = weighted_J)) +
	geom_smooth(method = 'lm', se = FALSE, color = 'grey') +
	annotate('text', x = 300, y = .1, label = lm_eqn(cache), parse = T) +
	geom_text_repel(aes(label = city), size = 3) +
	geom_point(color = 'firebrick') +
	# scale_x_continuous(trans = 'log10') +
	# scale_y_continuous(trans = 'log10') +
	theme_minimal() +
	xlab(expression(rho~(population/km^{2}))) +
	ylab(expression(J(X,Y))) +
	annotation_logticks() +
	ggtitle('Dependence of spatial complexity on population density')

ggsave('report/figs/density_fisher.png', width = 8, height = 4)


# Viz 2 ---------
cache %>%
	ggplot(aes(x = I_XY, y = weighted_J)) + 
	geom_text_repel(aes(label = city), size = 3) + 
	geom_point(color = 'firebrick') + 
	theme_minimal() + 
	ylab(expression(J(X,Y))) + 
	xlab(expression(I(X,Y))) + 
	ggtitle('Spatial variability and complexity in US cities')

ggsave('report/figs/mutual_fisher.png', width = 8, height = 4)

# Viz 3 -------- Control plot
cache %>%
	ggplot(aes(x = weighted_J, y = local.H_Y)) + 
	geom_smooth(method = 'lm', se = FALSE, color = 'grey') + 
	geom_text_repel(aes(label = city), size = 3) + 
	geom_point(color = 'firebrick') + 
	scale_x_continuous(trans = 'log10') + 
	theme_minimal() +
	xlab(expression(rho~(population/km^{2}))) + 
	ylab(expression(H[Y|X])) + 
	annotation_logticks() + 
	ggtitle('Dependence of local entropy on population density')

ggsave('report/figs/density_entropy.png', width = 8, height = 4)

# We are expecting that this should also be related, roughly linearly, to density on a log scale (like info). If this is indeed the case, it's further evidence to show that the linear trend isn't just an artifact of how the Census splits up space. 


# Table --------

cache %>% 
	select(area, population, H_Y, I_XY, J = weighted_J) %>% 
	mutate(Density = population / area) %>% 
	round(2) %>%
	cbind(cache$city, .) %>%
	rename(City = `cache$city`, 
		   `Area` = area,
		   Population = population,
		   HY = H_Y,
		   IXY = I_XY) %>%
	arrange(desc(Density)) %>% 
	select(City, `Area`, Population, Density, HY, IXY, J) %>%
	write_csv('report/figs/table.csv')


