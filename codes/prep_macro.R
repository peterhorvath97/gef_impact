library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)


macrovars <- read_rds(file.path('data', 'macrovars.rds')) 

for(i in 1:length(macrovars)){
  macrovars[[i]] <- macrovars[[i]] %>% 
    filter(country %in% countries)
}

#Define which vars are on quarterly frequency for LP LHS and RHS generation later in the code
q <- c('gdp', 'tb', 'ca')
m <- names(macrovars)[!(names(macrovars) %in% q)]

#Generate Controls and Targets for the LP 
suppressMessages(
  for(i in seq_along(q)){
    macrovars[[q[i]]] <- macrovars[[q[i]]] %>% 
      gather(key = var, value = value, -country, -date) %>% 
      group_by(var, country) %>%  
      #Remove Country FE
      mutate(value = value - mean(value)) %>%
      #Create RHS
      mutate(map_dfc(seq(4), ~ lag(value, n = .x)) %>%
               set_names(paste('lag', seq(4),sep = ''))) %>% 
      drop_na() %>% 
      #Create LHS
      mutate(map_dfc(seq(21), ~ lead(value, n = .x)) %>%
               set_names(paste('lead', seq(21),sep = ''))) %>% 
      #Remove unnecessary columns  
      select(-value, -var)
  }
)

suppressMessages(
  for(i in seq_along(m)){
    macrovars[[m[i]]] <- macrovars[[m[i]]] %>% 
      gather(key = var, value = value, -country, -date) %>% 
      group_by(var, country) %>%  
      #Remove Country FE
      mutate(value = value - mean(value)) %>%
      #Create RHS
      mutate(map_dfc(seq(12), ~ lag(value, n = .x)) %>%
               set_names(paste('lag', seq(12),sep = ''))) %>% 
      drop_na() %>% 
      #Create LHS
      mutate(map_dfc(seq(61), ~ lead(value, n = .x)) %>%
               set_names(paste('lead', seq(61),sep = ''))) %>% 
      select(-value, -var)
  }
)

for(i in 1:length(macrovars)){
var <- names(macrovars)[[i]]
names(macrovars[[i]])[4:length(names(macrovars[[i]]))] <- paste(var, names(macrovars[[i]])[4:length(names(macrovars[[i]]))], sep = '_')
}
rm(var, i)

saveRDS(macrovars, file.path('data', 'macrovars_est.rds'))