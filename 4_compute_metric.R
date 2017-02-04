library(compx)
library(rgdal, quietly = TRUE)
library(stringr, quietly = TRUE)
library(tidyverse, quietly = TRUE)

source('R/smoother.R')

# Prep data

demographics <- read_csv('data/demographics.csv') %>% 
    dplyr::mutate(tract = as.character(GEOID),
           tract = stringr::str_pad(tract, 12, side = 'left', pad = '0')) %>% 
    select(-GEOID) 

tract_list <- demographics %>% 
    group_by(tract) %>% 
    dplyr::summarise(n = sum(n)) %>% 
    filter(n > 100) %>% 
    select(-n)

if(!dir.exists('throughput/metric')){
    dir.create('throughput/metric')
}

analysis <- function(city){
    
    tracts <- readOGR(dsn = paste0('data/cities/',city), 
                      layer = 'geo',
                      verbose = FALSE)
    print(paste(city, ':', nrow(tracts), 'tracts'))
    
    
    tracts <- tracts[tracts@data$GEOID %in% tract_list$tract,] 
    area_df <- tracts@data %>% 
        dplyr::select(GEOID) %>% 
        mutate(area = abs(raster::area(tracts) / 1e6))
    
    data <- demographics %>% 
        dplyr::filter(tract %in% tracts@data$GEOID)
    
    data <- rbf_smoother(data, tracts, sigma = .5)
    
    metric_df <- compute_metric(tracts, 
                                data, 
                                km = T, 
                                sigma = 10, 
                                divergence = 'DKL', 
                                smooth = T) %>% 
        left_join(area_df, by = c('geoid' = 'GEOID'))
    
    metric_df <- metric_df %>% 
        mutate(vol = map_dbl(local_g, . %>% det() %>% abs() %>% sqrt()),
               tr  = map_dbl(local_g, . %>% diag() %>% sum())) 
    

    
    
    list(metric_df = metric_df, 
         tracts    = tracts) %>% 
        saveRDS(paste0('throughput/metric/', city, '.RDS'))
    
    mutual_info <- data %>% 
        spread(key = group, value = n) %>% 
        select(-tract) %>% 
        compx::mutual_info()
    
    local_entropy <- data %>% 
        nest(-tract) %>% 
        mutate(pop  = map_dbl(data, ~ sum(.$n)),
               p    = map(data, ~ .$n / sum(.$n)),
               h    = map_dbl(p, compx::H)) %>% 
        summarise(local_h = weighted.mean(h, w = pop)) %>% 
        unlist()
    
    global_entropy <- data %>% 
        dplyr::group_by(group) %>% 
        dplyr::summarise(n = sum(n)) %>% 
        select(n) %>% 
        unlist() %>% 
        simplex_normalize() %>% 
        compx::H()
    
    metric_df %>% 
        dplyr::summarise(n_tracts = nrow(.),
                  total_pop     = sum(total),
                  total_area    = sum(area),
                  total_vol     = sum(vol),
                  trace   = mean(tr),
                  a_vol   = weighted.mean(vol, area, na.rm = T),
                  p_vol   = weighted.mean(vol, total, na.rm = T),
                  a_trace = weighted.mean(tr, area, na.rm = T),
                  p_trace = weighted.mean(tr, total, na.rm = T)) %>% 
        mutate(I_XY = mutual_info,
               H = global_entropy, 
               local_H = local_entropy, 
               city = city)
    
}

list.dirs('data/cities', full.names = FALSE)[-1] %>% 
    map(analysis) %>% 
    reduce(rbind) %>% 
    write_csv('throughput/metric.csv')