
## @knitr utils

library(tidyverse)
library(compx)
library(sf)
library(RColorBrewer)
library(ggthemes)
# 
# h_clust <- function(input, data){
#     
#     if('sf' %in% class(input)){
#         tract_lookup <- id_lookup(input)
#         adj <- st_relate(input, pattern = '****1****', sparse = TRUE)
#         
#         data_lookup <- data %>% 
#             nest(-tract) %>% 
#             mutate(n = map(data, ~.$n)) %>% 
#             select(-data) %>% 
#             left_join(tract_lookup, by = c('tract' = 'geoid'))
#         
#         adj <- 1:length(adj) %>%
#             map(~data_frame(from = as.character(.),
#                             to = as.character(adj[[.]]))) %>%
#             reduce(rbind) %>%
#             filter(from != to) %>%
#             left_join(data_lookup, by = c('from' = 'row')) %>%
#             left_join(data_lookup, by = c('to' = 'row'), suffix = c('_from', '_to')) %>% 
#             rename(from_n = n_from, to_n = n_to)
#         n <- nrow(input)
#         names <- input$cluster
#         
#         M <- data %>% 
#             group_by(group) %>% 
#             summarise(n = sum(n)) %>% 
#             select(n) %>% 
#             unlist()
#         
#     }else{
#         adj <- input %>% 
#             as.directed(mode = 'mutual') %>% 
#             as_long_data_frame() %>% 
#             dplyr::as_data_frame()
#         
#         M <- V(input)$n %>% reduce(`+`)
#         n <- length(V(input))
#         names <- V(input)$name
#         }
#     
#     divergence <- function(n,m){
#         p <- n / sum(n)
#         q <- m / sum(m)
#         p_bar <- sum(n) / sum(m + n) * p + sum(m) / sum(m + n) * q
#         r <- M / sum(M)
#         sum(n) / sum(M) * DKL(p, r) +
#             sum(m) / sum(M)*DKL(q, r) -
#             sum(m + n) / sum(M) * DKL(p_bar, r)
#     }  
#     
#     adj <- adj %>%
#         mutate(dist = map2_dbl(from_n, to_n, divergence)) %>%
#         mutate(from = as.integer(from),
#                to = as.integer(to))
#     
#     merge <- matrix(nrow = n-1, ncol = 2)
#     height <- numeric(length = n-1)
#     cluster_stage <- 1
#     
#     for(i in 1:(n-1)){
#         new_cluster_name <- n + i 
#         
#         ix <- which.min(adj$dist)
#         
#         height[cluster_stage] <- min(adj$dist)
#         
#         i <- adj$from[ix]
#         j <- adj$to[ix]
#         
#         if(i <= n){
#             eye <- -i
#         }else{
#             eye <- i - n
#         }
#         
#         if(j <= n){
#             jay <- -j
#         }else{
#             jay <- j - n
#         }
#         
#         merge[cluster_stage, c(1,2)] <- c(eye, jay)
#         
#         op <- adj %>%
#             filter(from %in% c(i,j) | to %in% c(i,j))
#         
#         ij_n <- adj %>%
#             filter(from == i, to == j) %>%
#             select(from_n, to_n)
#         
#         ij_n <- ij_n$from_n[[1]] + ij_n$to_n[[1]]
#         
#         adj <- adj %>% anti_join(op, by = c('from' = 'from', 'to' = 'to'))
#         
#         op <- op %>%
#             filter(!(from == i & to == j) & !(from == j & to == i)) %>%
#             select(-dist)
#         
#         test <- op %>%
#             mutate(new = list(ij_n))
#         
#         test$from_n <- ifelse(test$from %in% c(i,j), test$new, test$from_n)
#         test$to_n   <- ifelse(test$to %in% c(i,j), test$new, test$to_n)
#         
#         test <- test %>%
#             mutate(from = ifelse(from %in% c(i,j), new_cluster_name, from),
#                    to = ifelse(to %in% c(i,j), new_cluster_name, to))
#         
#         test <- test %>%
#             select(-new) %>%
#             mutate(dist = map2_dbl(from_n, to_n, divergence))
#         
#         adj <- adj %>%
#             rbind(test) %>%
#             distinct(from, to, .keep_all = TRUE)
#         
#         cluster_stage <- cluster_stage + 1
#     }
#     
#     a <- list()
#     a$merges <- merge
#     a$height <- height
#     a$labels <- names
#     a$order  <- names
#     class(a) <- "hclust"
#     a
# }
# 


prominence_map <- function(tracts, data){
    
    marginal <- data %>% 
        group_by(group) %>% 
        summarise(n = sum(n)) %>%
        mutate(n = n / sum(n))
    
    f <- function(df){
        df$group[which.max(df$n *  log(df$n / marginal$n))][1]
    }
    
    g <- function(df){
        max(df$n *  log(df$n / marginal$n), na.rm = T)
    }
    
    prominence_df <- data %>% 
        group_by(tract) %>% 
        mutate(n = n / sum(n)) %>% 
        nest(-tract, .key = df) %>% 
        mutate(prominent_group = map_chr(df, f),
               prominence = map_dbl(df, g),
               prominent_group = factor(prominent_group, levels = unique(data$group))) 
    
    g <- tracts %>% 
        left_join(prominence_df, by = c('GEOID' = 'tract')) %>% 
        ggplot() + 
        geom_sf(aes(fill = prominent_group, 
                    alpha = prominence), size = .1)  +
        ggthemes::theme_map() + 
        scale_alpha_continuous(guide = 'none', limits = c(0,1)) +
        theme(plot.title = element_text(size=20, vjust = -5),
              legend.title = element_blank(),
              legend.position = c(.7, .1),
              legend.text=element_text(size=16)) + 
        scale_fill_brewer(palette = 'Set1', limits = levels(prominence_df$prominent_group))
    
    g
}



h_clust_map <- function(g, tracts, data, a, k, size_coef = 15){
    
    all <- tracts %>% 
        aggregate(by = list(rep(1, nrow(.))), FUN = identity) 
    
    all_bd <- st_boundary(all)
    
    for(j in 2:k){
        plot_tracts <- tracts %>% 
            mutate(new_cluster = cutree(a, j)) %>% 
            aggregate(by = list(.$new_cluster), FUN = identity) 
        
        bd <- st_boundary(plot_tracts) %>% st_difference(all_bd)
        
        
        size <- rev(a$height)[j-1]
        
        g <- g + 
            geom_sf(size = size_coef * exp(5 * size), data = bd, alpha = 0, color = 'black') 
    }
    
    g
    
}

s_clust_map <- function(g, tracts, data, blocked_tracts, size_coef = 1){
    all <- tracts %>% 
        aggregate(by = list(rep(1, nrow(.))), FUN = identity)
    
    all_bd <- st_boundary(all)
    
    bd <- st_boundary(blocked_tracts) %>% st_difference(all_bd)
    
    g + 
        geom_sf(alpha = 0, data = bd, size = size_coef, color = 'black')
}





js_dist <- function(n,m){
    
    N <- sum(n)
    M <- sum(m)
    
    p <- n/N
    q <- m/M
    
    sqrt((N/(M+N))*DKL(p, p+q) + (M/(M+N))*DKL(q, p+q))
}


read_data <- function(city, data){
    tracts <- sf::st_read(paste0('data/cities/', city), layer = 'geo', stringsAsFactors = FALSE)
    
    data <- data %>% filter(tract %in% tracts$GEOID) 
    
    tracts <- tracts %>% 
        filter(GEOID %in% data$tract)
    
    list(tracts = tracts, data = data)
}


plot_spectrum <- function(g, sigma){
    A <- affinity_matrix(g, sigma = sigma)
    L <- normalized_laplacian(A)
    eig <- RSpectra::eigs(L, 100, which = 'LM', sigma = 1e-10)
    
    df <- data_frame(eig = Re(eig$values)) %>% 
        mutate(n = nrow(.) - row_number()) %>% 
        filter(n < 100)
    
    df %>% 
        ggplot() + 
        aes(x = n, y = eig) + 
        geom_point()
}

group_data <- function(tracts, data, g){
    clusters <- data_frame(GEOID = V(g)$name, cluster = V(g)$cluster)    
    
    blocked_tracts <- tracts %>% 
        left_join(clusters) %>% 
        select(cluster) %>% 
        aggregate(by = list(cluster = .$cluster), FUN = identity) %>% 
        mutate(GEOID = as.character(cluster))
    
    blocked_data <- data %>% 
        left_join(clusters, by = c('tract' = 'GEOID')) %>% 
        group_by(cluster, group) %>% 
        summarise(n = sum(n)) %>% 
        ungroup() %>% 
        mutate(tract = as.character(cluster))
    
    list(blocked_tracts = blocked_tracts, blocked_data = blocked_data)
}


compare_hclusts <- function(a, b){
    height_df <- data_frame(hclust = rev(b$height)[1:length(a$height)], 
                            mixed = rev(a$height)) %>% 
        mutate(n = row_number() + 1) %>% 
        gather(key = type, value = info, -n) %>% 
        group_by(type) %>% 
        mutate(info = cumsum(info)) %>% 
        ungroup()
    
    height_df <- height_df %>% rbind(data_frame(n = 1, type = c('hclust', 'mixed'), info = 0))
    
    
    height_df %>% 
        ggplot() + 
        aes(x = n, y = info, color = type) + 
        geom_point() + 
        geom_line()
}
