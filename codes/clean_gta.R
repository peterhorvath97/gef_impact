library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)

load('data/GTA+Database+2008-latest.Rdata')
master <- as_tibble(master)

colnames(master) <- c('state_id',
                      'interv_id',
                      'title',
                      'announc_date',
                      'gta_eval',
                      'inforce',
                      'date_in',
                      'date_out',
                      'country_imp',
                      'type',
                      'mast',
                      'sectors',
                      'products',
                      'country_aff')


#Basic cleanups
master <- master %>% 
  filter(!is.na(country_imp),
         !is.na(gta_eval),
         !is.na(date_in),
         !is.na(type),
         !is.na(sectors),
         !is.na(products),
         year(date_in) <= 2023,
         #Exclude the "Amber" labelled group - we want to be sure that 
         #These dates refer to interventions that are reflecting on 
         #de-liberalization / liberalization measures
         gta_eval %in% c('Red', 'Green'),
         #Remove unclear measures or any other type that we might not think is 
         #related to geoeconomic fragmentation
         !str_detect(tolower(type), or('unclear',
                                       'customer', #foreign customre limit?
                                       'local',
                                       'state aid',
                                       'financial assistance',
                                       'trade finance',
                                       'trade payment'
                                       )),
         !str_detect(tolower(mast), or('unclear', 
                                       'procurement', 
                                       'migration', 
                                       'local', 
                                       'internal', 
                                       'fdi', 
                                       'capital control',
                                       'trade-balancing', #argentina specific outlier for some reason
                                       'intellectual',
                                       'l subsidies'
                                       ))) %>% 
  mutate(country_aff = replace_na(country_aff, 'General'),
         across(c(products, sectors), ~replace_na(.x, '-')))


#y <-  master %>% 
#  janitor::tabyl(mast)
#y2 <- master %>% 
#  janitor::tabyl(type)

#Assigning the dates to closest months
master <- master %>% 
  #cleaning up the dates of entry into force to the according month
  mutate(year = year(date_in),
         month = month(date_in),
         day = day(date_in),
         month = ifelse(day > 15, month+1, month),
         year = ifelse(month == 13, year +1, year),
         month = ifelse(month == 13, 1, month),
         month = ifelse(nchar(month) == 2, as.character(month), paste('0', month, sep = '')),
         date_in = as_date(paste(year, month, '01', sep = '-'))) %>% 
  #cleaning up the dates of no longer being in force to the according month
  mutate(year = year(date_out),
         month = month(date_out),
         day = day(date_out),
         month = ifelse(day > 15, month+1, month),
         year = ifelse(month == 13, year +1, year),
         month = ifelse(month == 13, 1, month),
         month = ifelse(nchar(month) == 2, as.character(month), paste('0', month, sep = '')),
         date_out = as_date(paste(year, month, '01', sep = '-'))) %>% 
  select(-year, -month, -day)

#Generate separate data frame for collecting shocks defined as lifting of implemented restrictions
master_lift <- master %>% 
  filter(!is.na(date_out))




#Generate the shock variables
master <- master %>% 
  #generate basic shock, transitory and permanent versions
  mutate(shock_base = ifelse(gta_eval == 'Red', -1, 1),
         shock_trans = ifelse(!is.na(date_out), shock_base, 0),
         shock_perm = ifelse(is.na(date_out), shock_base, 0),) %>% 
  select(-date_out) %>% 
  #weight count of restrictions by imposing country
  group_by(country_imp, date_in, gta_eval, type) %>% 
  mutate(imp_weight = sum(shock_base),
         imp_weight_trans = sum(shock_trans),
         imp_weight_perm = sum(shock_perm)) %>% 
  ungroup() %>% 
  #weight count of restriction by affected country
  group_by(country_aff, date_in, gta_eval, type) %>% 
  mutate(aff_weight = sum(shock_base),
         aff_weight_trans = sum(shock_trans),
         aff_weight_perm = sum(shock_perm)) %>% 
  ungroup() %>% 
  rename(date = date_in) %>% 
  #Remove observations that will be implemented in the future
  filter(year(date) <= 2023) %>% 
  select(
    #joining variables
    country_imp, country_aff, date,
    #shock variables
    starts_with('shock'), contains('weight'),
    #possible filtering variables
    gta_eval, type
  ) %>% 
  mutate(db = 'main')


#Generate additional shock values at times of restrictions being lifted
master_lift <- master_lift %>% 
  #generate basic shock on listing restrictions - only 'base' type shock this time
  mutate(shock_base = ifelse(gta_eval == 'Red', -1, 1)) %>% 
  select(-date_in) %>% 
  #weight count of restrictions by imposing country
  group_by(country_imp, date_out, gta_eval, type) %>% 
  mutate(imp_weight = sum(shock_base),
         imp_weight_perm = imp_weight) %>% 
  ungroup() %>% 
  #weight count of restriction by affected country
  group_by(country_aff, date_out, gta_eval, type) %>% 
  mutate(aff_weight = sum(shock_base),
         aff_weight_perm = aff_weight) %>% 
  ungroup() %>% 
  rename(date = date_out) %>% 
  #Remove observations that will be implemented in the future
  filter(year(date) <= 2023) %>% 
  select(
    #joining variables
    country_imp, country_aff, date,
    #shock variables
    starts_with('shock'), contains('weight'),
    #possible filtering variables
    gta_eval, type
  ) %>% 
  mutate(db = 'removals')


gta_shocks <- bind_rows(master, master_lift)
saveRDS(gta_shocks, file.path('data', 'gta_shocks.rds'))

