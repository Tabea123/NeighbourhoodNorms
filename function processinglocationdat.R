################################################################################
#                                                                              #
#                     Function that Processes Location Data                    #
#                                                                              #
################################################################################

# Load Data
library(raster)
library(rgdal)
library(dplyr)


processinglocationdat <- function(file){
  
  contacts <- as.data.frame(shapefile(file))
  
  # delete columns
  contacts <- contacts %>% dplyr::select(-c(createtime, geoansweri, visiblelay, zoomlevel))
  
  # get one home location per user (pick last)
  contacts$count <- ifelse(grepl("huis", contacts$buttonname), 1, 0) # count occurrence of huis in data
  
  self <- contacts %>% group_by(respondent) %>% # for each respondent
    
    mutate(count = sum(count)) %>% # sum the occurrences of huis 
    
    filter(count > 0) %>% # exclude participants that indicate huis 0 times
    
    filter(grepl("huis", buttonname)) %>% # get location of huis
    
    slice(which.max(count)) %>%  # select last location for each respondent
    
    dplyr::select(-count) # delete counting column
  
  
  # get data that refers to contacts  
  others <- contacts %>% filter(!grepl("huis", buttonname)) %>% dplyr::select(-count)
  
  # delete data from respondents that did not indicate their house
  delete <- unique(others$respondent)[!unique(others$respondent) %in% unique(self$respondent)]
  keep   <- unique(others$respondent)[unique(others$respondent) %in% unique(self$respondent)]
  
  if (length(delete) > 0) {
    others <- others %>% filter(respondent %in% keep) 
  } else {
    others <- others
  }
  
  
  # join again
  contacts2 <- bind_rows(as_tibble(self), as_tibble(others))
  
  # add weights according to relationship between respondent and contact
  weight <- rep(0, nrow(contacts2))
  weight[which(grepl("Kennissen", contacts2$buttonname))] <- 1
  weight[which(grepl("Vrienden", contacts2$buttonname))] <- 2
  weight[which(grepl("Familie", contacts2$buttonname))] <- 3
  
  contacts3 <- contacts2 %>% tibble::add_column(weight = weight) %>% arrange(respondent)
  
  return(contacts3)
}
