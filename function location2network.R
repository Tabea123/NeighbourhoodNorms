################################################################################
#                                                                              #
#                  Function that Anonymises Location Data                      #
#                                                                              #
################################################################################

# note: this function should be used in combination with processinglocationdat

location2network <- function(data){ 
  
  # round coordinates to the fourth position after the comma
  neighbourhood.data <- data %>% mutate(across(.cols = 3:4, ~ round(.x, 4)))
  
  # sample random letters/numbersfor the creation of code names
  long <- sample(c(letters, LETTERS, 100:500), 
                 length(unique(neighbourhood.data$coords.x1))) 
  
  lat  <- sample(c(letters, LETTERS, 100:500), 
                 length(unique(neighbourhood.data$coords.x2)))
  
  # add code names for each unique pair of coordinates to data set
  anon.neighbourhood.data <- neighbourhood.data %>%
    
    left_join(tibble(coords.x1 = unique(neighbourhood.data$coords.x1),
                     new.long = long)) %>%
    
    left_join(tibble(coords.x2 = unique(neighbourhood.data$coords.x2),
                     new.lat = lat))  %>%
    
    mutate(codename = paste0(new.lat, new.long))  %>%  
    
    dplyr::select(-c(new.lat, new.long, coords.x1, coords.x2)) # delete location data
  
  
  # get code names of the respondents
  respondent.codes <- ungroup(anon.neighbourhood.data) %>% 
    
    filter(grepl("huis", buttonname)) %>% 
    
    dplyr::select(codename) %>%
    
    rename(respondent.codes = codename) %>% 
    
    unlist(use.names = FALSE)
  
  # replace respondent number with code name
  anon.neighbourhood.data2 <- anon.neighbourhood.data %>% 
    
    left_join(tibble(respondent = unique(anon.neighbourhood.data$respondent), 
                     new.name = respondent.codes))  %>%
    
    mutate(node = new.name)  %>%
    
    dplyr::select(-new.name)
  
  
  # create a data frame with the network's edges 
  edges <- data.frame(from = anon.neighbourhood.data2$node, 
                      to = anon.neighbourhood.data2$codename, 
                      weight = anon.neighbourhood.data2$weight,
                      buttonname = anon.neighbourhood.data2$buttonname,
                      respondent = anon.neighbourhood.data2$respondent)
  
  
  edges$color <- rep(0, nrow(edges))
  edges$color[which(edges$weight == 1)] <- "#1b9e77" # Acquaintances
  edges$color[which(edges$weight == 2)] <- "#d95f02" # Friend
  edges$color[which(edges$weight == 3)] <- "#7570b3" # Family
  
  # create a data frame with the network's nodes
  nodes <- data.frame(id = unique(anon.neighbourhood.data2$codename), 
                      number = 1:length(unique(anon.neighbourhood.data2$codename)),
                      label = unique(anon.neighbourhood.data2$codename))
  
  
  return(list(edges, nodes))
}