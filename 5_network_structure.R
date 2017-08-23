library(compx, quietly = T)
library(tidyverse, quietly = T)
library(sf)
# source('R/hcluster.R')

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
if(!dir.exists('throughput/hclusts')){
    dir.create('throughput/hclusts')
}

analysis <- function(city){
    print(city)
    
    # get the tracts, compute area
    tracts <- sf::st_read(dsn = paste0('data/cities/', city), 
                          layer = 'geo', stringsAsFactors = FALSE)
    
    tracts <- tracts %>% 
        filter(GEOID %in% tract_list$tract) %>% 
        mutate(area = st_area(geometry) / 1000^2,
               area = as.numeric(area)) # area in km
    
    area_df <- tracts %>% 
        as_data_frame() %>% 
        select(GEOID, area) 
    
    # filter the demographics to the tracts
    
    data <- demographics %>% 
        dplyr::filter(tract %in% tracts$GEOID) 
    
    # smoothed_data <- compx::rbf_smoother(data, tracts, sigma = c(.2, .2))
    smoothed_data <- data
    
    # do the analysis
    divergence <- function(n,m){
        p <- n / sum(n)
        q <- m / sum(m)
        sqrt(.5 * DKL(p, p+q) + .5*DKL(q, p+q))
    }
    
    # spectral
    g <- construct_information_graph(tracts, smoothed_data, divergence = divergence)
    A <- affinity_matrix(g, sigma = 50)
    L <- normalized_laplacian(A)
    eig <- RSpectra::eigs(L, 100, which = 'LM', sigma = 1e-10)         
    
    g %>% saveRDS(file = paste0('throughput/graphs/', city, '.RDS'))
    
    # return the spectra, might add more later
    spectrum <- data_frame(city = city, n = 1:length(eig$values), eig = rev(Re(eig$values))) %>% 
        mutate(eig = eig - min(eig),
               eig = eig / max(eig))
    
    # hierarchical
    g <- construct_information_graph(tracts, smoothed_data, divergence = divergence)
    M <- smoothed_data %>%
        group_by(group) %>%
        summarise(n = sum(n)) %>%
        select(n) %>% unlist()
    
    divergence <- function(n,m){
        p <- n / sum(n)
        q <- m / sum(m)
        p_bar <- sum(n) / sum(m + n) * p + sum(m) / sum(m + n) * q
        r <- M / sum(M)
        sum(n) / sum(M) * DKL(p, r) +
            sum(m) / sum(M)*DKL(q, r) -
            sum(m + n) / sum(M) * DKL(p_bar, r)
    }
    
    a <- info_cluster(g, divergence)
    a %>% saveRDS(file = paste0('throughput/hclusts/', city, '.RDS'))
    
    hclust_loss <- data_frame(city = city, 
                              n = 1:length(a$height), 
                              info = rev(max(a$height) - a$height)) 
    return(list(spectrum = spectrum, hclust_loss = hclust_loss))
}


fnames <- c('spectra', 'hclust_loss')
list.dirs('data/cities', full.names = FALSE)[-1] %>%
    map(analysis) %>% 
    transpose() %>% 
    map(~ reduce(., rbind)) %>% 
    walk2(fnames, ~ write_csv(.x, paste0('throughput/', .y, '.csv')))