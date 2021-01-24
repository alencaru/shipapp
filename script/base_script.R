#-------------------------------------------------------------------------------

library(tidyverse)
library(geosphere)

#-------------------------------------------------------------------------------
# open up the data file

tab <- read_csv("data/ships_04112020/ships.csv")

#-----------------------------------------------
# separating the ship infomation from ship from "travel" information

ships_data <- tab %>% 
  select(SHIP_ID, ship_type, SHIPNAME, FLAG, DWT, WIDTH, LENGTH) %>%
  unique()

saveRDS(ships_data, "ships_data.rds")

#--------------------------------------------------------------------------------
# consecutive obs

list <- unique(tab$SHIP_ID)

#---------------------------------------------------
# building the consecutive max distance points table


system.time(
  
tt <- map_dfr(list, function(x){
  tab %>%
    filter(SHIP_ID == x) %>%
    distinct() %>%
    select(SHIP_ID, DATETIME, LAT, LON) %>%
    mutate( LAT2 = lag(LAT)
           ,LON2 = lag(LON)
    ) %>%
    mutate(distance = diag(distm(
      cbind(LON2, LAT2), cbind(LON, LAT)
      , fun = distHaversine)
      ),
      timespan = difftime(
        DATETIME, lag(DATETIME)
        , units = "secs")) %>% 
    slice_max(distance) %>% 
    slice_max(DATETIME) %>% 
    ungroup()
}) # end loop

) # end sistem time
# took 2h 49 to end, would better use a foreach with parallelization

saveRDS(tt, "tab_a.rds")

#----------------------------------------------------------------------------------------























