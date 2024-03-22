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
                                      'customer', #foreign customer limit?
                                       'local',
                                        'state aid',
                                        'financial assistance',
                                       'trade finance',
                                        'trade payment',
                                      'fdi',
                                      'procurement',
                                      'surrender',
                                      'anti-',
                                      'competitive',
                                      'control'
                                        )),
         !str_detect(tolower(mast), or(#'unclear', 
                                        'tendering', 
                                        'migration', 
                                        #'local', 
                                        #'internal', 
                                        'fdi', 
                                        #'capital control',
                                        'trade-balancing', #argentina specific outlier for some reason
                                        'intellectual',
                                        'l subsidies'
                                        ))) %>% 
  mutate(country_aff = replace_na(country_aff, 'General'),
         across(c(products, sectors), ~replace_na(.x, '-'))) %>% 
  distinct(date_in, announc_date, country_imp, country_aff, gta_eval, inforce, type, mast, sectors, products, .keep_all = T)


master %>% janitor::tabyl(type)

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
  #cleaning up the dates of no longer in force to the according month
  mutate(year = year(date_out),
         month = month(date_out),
         day = day(date_out),
         month = ifelse(day > 15, month+1, month),
         year = ifelse(month == 13, year +1, year),
         month = ifelse(month == 13, 1, month),
         month = ifelse(nchar(month) == 2, as.character(month), paste('0', month, sep = '')),
         date_out = as_date(paste(year, month, '01', sep = '-'))) %>% 
  #Clean announcement dates
  mutate(year = year(announc_date),
         month = month(announc_date),
         day = day(announc_date),
         month = ifelse(day > 15, month+1, month),
         year = ifelse(month == 13, year +1, year),
         month = ifelse(month == 13, 1, month),
         month = ifelse(nchar(month) == 2, as.character(month), paste('0', month, sep = '')),
         announc_date = as_date(paste(year, month, '01', sep = '-'))) %>% 
  select(-year, -month, -day)

master <- master %>% 
  select(country_imp, country_aff, date_ann = announc_date, date_in, date_out, 
         gta_eval, type, sectors, products)

saveRDS(master, file.path('data', 'gta_shocks.rds'))
rm(master)
