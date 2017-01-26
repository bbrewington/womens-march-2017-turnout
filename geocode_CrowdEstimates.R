library(tidyverse) #ggplot2,tibble,tidyr,readr,purrr,dplyr
library(ggmap)
library(jsonlite)

turnout <- read_csv("data/CrowdEstimates.csv", na = character()) %>% 
     mutate(city_state_country = paste(City, State, Country, sep = ", "))
turnout <- 
     turnout %>% mutate(State = ifelse(City == "Bangor" & Country == "UK", "Gwynedd", State),
                        City = ifelse(City == "Milhelm" & State == "PA", "Millheim", City),
                        State = ifelse(City == "Lilongwe" & Country == "Malawi", "Central Region", State))

locations <- vector(mode = "list", length = nrow(turnout))

for(i in seq_along(turnout$city_state_country)){
     locations[[i]] <- geocode(turnout$city_state_country[i], output = "all")
}

write_file(toJSON(locations), path = "data/CrowdEstimates_locations.json")

locations_flat1 <- vector(mode = "list", length = length(locations))
for(i in seq_along(locations)){
     print(i)
     if(locations[[i]]$status == "ZERO_RESULTS"){
          locations_flat1[[i]] <- data.frame(City1 = as.character(NA), State1 = as.character(NA), Country1 = as.character(NA),
                                             lat = as.numeric(NA), lon = as.numeric(NA))
     } else{
          locations_flat1[[i]] <- 
               data.frame(City1 = turnout$City[i], State1 = turnout$State[i],
                     Country1 = turnout$Country[i], 
                     lat = locations[[i]]$results[[1]]$geometry$location$lat %>% as.numeric(),
                     lon = locations[[i]]$results[[1]]$geometry$location$lng %>% as.numeric(),
                     stringsAsFactors = F)
     }
     
}

turnout_geocoded <- bind_cols(turnout, bind_rows(locations_flat1)) %>% select(-ends_with("1"))
write_csv(turnout_geocoded, "data/CrowdEstimates_Geocoded.csv")