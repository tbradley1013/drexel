#===============================================================================
# Querying the USGS data to add to the map 
# 
# Tyler Bradley 
# 2019-10-28
#===============================================================================


library(tidyverse)
library(dataRetrieval)

safe_nwis_sites <- purrr::possibly(whatNWISsites, otherwise = NULL)

active_gw_sites <- stateCd$STUSAB %>% 
  map_dfr(~{
    
    output <- safe_nwis_sites(stateCd = .x, siteType = "GW", site_output = "basic", 
                              siteStatus = "active")
    
    if (is.null(output)){
      output <- tibble(state = .x)
      cat("Failed: ", .x, "\n")
    } else {
      output <- output %>% 
        mutate(state = .x)
      cat("Success: ", .x, "\n")
    }
    
    
    return(output)
    
    
  })


all_gw_sites <- stateCd$STUSAB %>% 
  map_dfr(~{
    
    output <- safe_nwis_sites(stateCd = .x, siteType = "GW", site_output = "basic")
    
    if (is.null(output)){
      output <- tibble(state = .x)
      cat("Failed: ", .x, "\n")
    } else {
      output <- output %>% 
        mutate(state = .x)
      cat("Success: ", .x, "\n")
    }
    
    
    return(output)
    
    
  })


write_csv(active_gw_sites, "../drexel/enve555/data/active-gw-sites.csv")
# this data set is very large and cannot be committed to github
write_csv(all_gw_sites, "../drexel/enve555/data/all-gw-sites.csv")