library(compx)
library(rgdal, quietly = TRUE)
library(stringr, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(sf)

# Prep data

demographics <- read_csv('data/demographics.csv') %>% 
    dplyr::mutate(tract = as.character(GEOID),
           tract = stringr::str_pad(tract, 12, side = 'left', pad = '0')) %>% 
    select(-GEOID) 

tract_list <- demographics %>% 
    group_by(tract) %>% 
    dplyr::summarise(n = sum(n)) %>% 
    filter(n > 0) %>% 
    select(-n)

if(!dir.create('throughput')){
    dir.create('throughput')
}
if(!dir.exists('throughput/metric')){
    dir.create('throughput/metric')
}

analysis <- function(city){
    
    tracts <- sf::st_read(dsn = paste0('data/cities/', city), 
                          layer = 'geo', stringsAsFactors = FALSE)
    
    print(paste(city, ':', nrow(tracts), 'tracts'))
    
    tracts <- tracts %>% 
        filter(GEOID %in% tract_list$tract) %>% 
        mutate(area = st_area(geometry) / 1000^2,
               area = as.numeric(area)) # area in km
    
    area_df <- tracts %>%  
        as_data_frame() %>% 
        select(GEOID, area) 
    
    data      <- demographics %>% 
        dplyr::filter(tract %in% tracts$GEOID) 
    
    # data <- data %>% 
    #     mutate(n = ifelse(n < 1, 1, n))

    data <- compx::rbf_smoother(data, tracts, sigma = c(.3, .3)) %>%
        arrange(tract) # expensive, dense distance matrix
    
    metric_df <- compute_metric(tracts, 
                                data, 
                                km = T, 
                                r_sigma = 10, 
                                s_sigma = 10,
                                smooth = T,
                                hessian = DKL_) 
    
    metric_df <- metric_df %>% 
        mutate(vol = map_dbl(g, . %>% det() %>% abs() %>% sqrt()),
               tr  = map_dbl(g, . %>% diag() %>% sum())) 
    
    H_df <- data %>% 
        group_by(tract) %>% 
        mutate(p = n / sum(n)) %>% 
        summarise(H = - sum(p * log(p)))
    
    metric_df <- metric_df %>% 
        left_join(H_df, by = c('geoid' = 'tract'))
        
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
        left_join(area_df, by = c('geoid' = 'GEOID')) %>% 
        dplyr::summarise(n_tracts   = nrow(.),
                         total_pop  = sum(total),
                         total_area = sum(area),
                         total_vol  = sum(vol),
                         trace      = mean(tr),
                         trim_trace = mean(tr, trim = .01),
                         trim_vol   = mean(vol, trim = .01),
                         a_vol      = weighted.mean(vol, area, na.rm = T),
                         p_vol      = weighted.mean(vol, total, na.rm = T),
                         a_trace    = weighted.mean(tr, area, na.rm = T),
                         p_trace    = weighted.mean(tr, total, na.rm = T),
                         vol        = mean(vol)) %>% 
        mutate(smoothed_I_XY = mutual_info,
               H = global_entropy, 
               local_H = local_entropy, 
               city = city)
}

list.dirs('data/cities', full.names = FALSE)[-1] %>% 
    map(analysis) %>% 
    reduce(rbind) %>% 
    write_csv('throughput/metric.csv')
