library(compx, quietly = T)
library(tidyverse, quietly = T)
library(rgdal)

source('R/smoother.R')

demographics <- read_csv('data/demographics.csv') %>% 
    dplyr::mutate(tract = as.character(GEOID),
           tract = stringr::str_pad(tract, 12, side = 'left', pad = '0')) %>% 
    select(-GEOID)

tract_list <- demographics %>% 
    group_by(tract) %>% 
    dplyr::summarise(n = sum(n)) %>% 
    filter(n > 0) %>% 
    select(-n)

if(!dir.exists('throughput/graphs')){
    dir.create('throughput/graphs')
}

analysis <- function(city){
    print(city)
    
    # get the tracts, compute area
    tracts <- readOGR(dsn = paste0('data/cities/',city), 
                      layer = 'geo',
                      verbose = FALSE)
    
    tracts <- tracts[tracts@data$GEOID %in% tract_list$tract,] 
    area_df <- tracts@data %>% 
        select(GEOID) %>% 
        mutate(area = map_dbl(1:nrow(tracts), ~ tracts@polygons[[.]]@area))
    
    # filter the demographics to the tracts
    
    geoids <- tracts@data$GEOID %>% as.character() %>% unique()
    
    data <- demographics %>% 
        filter(tract %in% geoids)
    
    data <- rbf_smoother(data, tracts, sigma = .5)
    
    # do the analysis
    g <- construct_information_graph(tracts, data, divergence = 'DKL')
    A <- affinity_matrix(g, sigma = 5)
    L <- generalized_laplacian_matrix(A)
    eig <- eigen(L, symmetric = T)
    
    g %>% saveRDS(file = paste0('throughput/graphs/', city, '.RDS'))
    
    # return the spectra, might add more later
    data_frame(city = city, n = 1:length(eig$values), eig = rev(eig$values)) %>% 
        mutate(eig = eig - min(eig),
               eig = eig / max(eig))
}

list.dirs('data/cities', full.names = FALSE)[-1] %>% 
    map(analysis) %>% 
    reduce(rbind) %>% 
    write_csv('throughput/spectra.csv')

