library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)
library(imfr)
library(sandwich)

#library(maps)
library(sf) 
library(units)
library(rnaturalearth)


source('codes/after_pull.R')
unzip('data', 
      !is_empty(list.dirs(path = 'data', full.names = TRUE, recursive = FALSE)))
rm(unzip)

update_raw <- F
update_cln <- T
update_est <- T
upload <- F

codes <- 'codes'

if (update_raw == T){
  source(file.path(codes, 'tords.R'))
  tords('data')
  rm(tords)
  source(file.path(codes, 'macrodata.R'))
  source(file.path(codes, 'clean_gta.R'))
}

shocks <- read_rds(file.path('data', 'gta_shocks.rds'))
macrovars <- read_rds(file.path('data', 'macrovars.rds'))

for(i in 1:length(macrovars)){
  macrovars[[i]] <- macrovars[[i]] %>% 
    mutate(var = names(macrovars)[i])
}

macrovars <- bind_rows(macrovars)

mindate <- max(min(shocks$date_in), min(macrovars$date))
maxdate <- min(max(shocks$date_in), max(macrovars$date))

countries <- macrovars %>% 
  janitor::tabyl(country, var) %>% 
  as_tibble() %>% 
  filter(gdp != 0,
         cpi != 0) %>% 
  pull(country)

rm(shocks, macrovars, i)

if (update_cln == T){
  source(file.path(codes, 'dist.R'))
  source(file.path(codes, 'prep_macro.R'))
  source(file.path(codes, 'shock_specs.R'))
  source(file.path(codes, 'estprep.R'))
}

if (update_est == T){
  source(file.path(codes, 'estim.R'))
}

if(upload == T){
  file.remove('data/macrovars_est.rds',
              'data/estdata.rds')
  source('codes/before_push.R')
  zip('data', 
      max(file.info(list.files('data', full.names = TRUE))$size>25*1024*1024)==1)
  rm(zip)
}




