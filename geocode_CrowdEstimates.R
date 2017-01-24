library(tidyverse) #ggplot2,tibble,tidyr,readr,purrr,dplyr
library(ggmap)
library(jsonlite)

turnout <- read_csv("data/CrowdEstimates.csv") %>% 
     mutate(city_state_country = paste(City, State, Country, sep = ", "))

locations <- vector(mode = "list", length = nrow(turnout))

for(i in seq_along(turnout$city_state_country)){
     locations[[i]] <- geocode(turnout$city_state_country[i], output = "all")
}

write_file(toJSON(locations), path = "data/CrowdEstimates_locations.json")

locations_flat1 <- vector(mode = "list", length = length(locations))
for(i in seq_along(locations)){
     print(i)
     if(locations[[i]]$status == "ZERO_RESULTS"){
          locations_flat1[[i]] <- data.frame(City1 = NA, State1 = NA, Country1 = NA,
                                             lat = NA, lon = NA)
     } else{
          locations_flat1[[i]]$City1 <- turnout$City[i]
          locations_flat1[[i]]$State1 <- turnout$State[i]
          locations_flat1[[i]]$Country1 <- turnout$Country[i]
          locations_flat1[[i]]$lat <- locations[[i]]$results[[1]]$geometry$location$lat
          locations_flat1[[i]]$lon <- locations[[i]]$results[[1]]$geometry$location$lat
     }
     
}

turnout_geocoded <- bind_cols(turnout, bind_rows(locations_flat1)) %>% select(-ends_with("1"))
write_csv(turnout_geocoded, "data/CrowdEstimates_Geocoded.csv")