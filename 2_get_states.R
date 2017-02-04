# library(sp, quietly = TRUE)
library(acs, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(rgdal, quietly = TRUE)

level <- 'blockgroup'

# -------------------

clean_race <- function(race){
    
    
    # transform into data frame
    race <- cbind(data.frame(race@geography),
                  data.frame(race@estimate)) %>%
        tbl_df() 
    
    # aggregate into 5 categories
    race_lookup <- read_csv('assumptions/race_lookup.csv')
    
    au_vars <- c('state', 'county', 'tract', 'blockgroup') %>% 
        intersect(names(race))
    
    gather_cols <- names(race) %>% 
        setdiff(au_vars) %>% 
        setdiff('NAME')
    
    race <- race %>% 
        gather_(key_col = 'original_race', value_col = 'n', gather_cols = gather_cols) %>% 
        left_join(race_lookup, by = c('original_race' = 'original')) %>% 
        dplyr::group_by_(.dots = au_vars %>% append(c('aggregated'))) %>% 
        dplyr::summarise(n = sum(n)) %>% 
        ungroup() %>%  
        filter(!is.na(aggregated)) %>% 
        spread(key = aggregated, value = n, fill = 0)
    
    # clean up the areal unit designations to give a single GEOID
    
    lengths <- list(state      = 2, 
                    county     = 3, 
                    tract      = 6, 
                    blockgroup = 1)
    
    lengths <- lengths[au_vars]
    
    for(colname in names(lengths)){
        race[[colname]] <- str_pad(race[[colname]], 
                                   lengths[[colname]], 
                                   'left', 
                                   pad = 0)
    }
    
    race <- race %>% 
        tidyr::unite_('GEOID', au_vars, sep = '') %>% 
        gather(key = group, value = n, -GEOID)
    race
}


getter <- function(state){
    print(state)
    counties <- fips.county %>% 
        filter(State == state) %>% 
        select(County.ANSI) %>% 
        unlist()

    if(level == 'blockgroup'){
        tracts <- tigris::block_groups(state = state, county = counties, cb=TRUE)
        geo    <- geo.make(state = state, 
                           county = counties, 
                           tract = '*', 
                           block.group = '*')
    }else{
        tracts <- tigris::tracts(state = state, county = counties, cb=TRUE)
        geo    <- geo.make(state = state, 
                           county = counties, 
                           tract = '*')    
    }
    
    
    tracts@data <- tracts@data %>% 
        mutate(GEOID = str_pad(GEOID, width = 12, side = 'left', pad = 0)) 
    
    race <- acs::acs.fetch(endyear = 2013,
                           span = 5,
                           geography = geo,
                           table.number = 'B03002',
                           col.names = "pretty")
    
    race <- clean_race(race)
    
    writeOGR(tracts, 
             paste0("data/states/", state),
             'geo', 
             driver = 'ESRI Shapefile', 
             morphToESRI = TRUE)
    
    return(race)
}

cities <- readr::read_csv('assumptions/cities.csv') %>% 
    mutate(state = word(state, -1))

if(!dir.exists('data')){
    dir.create('data')
}

if(!dir.exists('data/states')){
    dir.create('data/states')
}

# ------------------------------------------

states <- unique(c(cities$state, 'DC')) %>% 
    setdiff(list.dirs('data/states', full.names = F))

demographics <- states %>% 
    map(getter) %>% 
    reduce(rbind)

if(!is.null(demographics)){
    demographics %>% write_csv('data/demographics.csv')    
}

