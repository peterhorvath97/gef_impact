library(tidyverse)
library(Haver)
library(stringr)
library(lubridate)
library(rebus)
library(fredr)
library(forecast)
library(seasonal)

#Get the codes and key identifiers for bilateral trade
concentration <- read_rds(file.path('data', 'concentration_codes.rds'))

#create codes for extracting the data from haver
varcodes <- concentration %>% 
  mutate(code2 = paste(database, code, sep = ":"))  %>% 
  pull(code2)

seq(1, length(varcodes), 5000)

#get currencies from the trade database so exchange rates for conversion can be extracted from haver also
concentration %>% 
  janitor::tabyl(currency) %>% 
  pull(currency)

currencycodes <- c('a193', 'a023', 'a532', 'a548', 'a456', 'a158')

haver.path('auto')

#extract exchange rates
currencyconv <- bind_cols(
haver.data(paste('fxrates:', currencycodes, sep = ''),
           frequency = 'M',
           start = as_date('1940-01-01'),
           end = as_date(Sys.Date())) %>% rownames(), 
haver.data(paste('fxrates:', currencycodes, sep = ''),
           frequency = 'M',
           start = as_date('1940-01-01'),
           end = as_date(Sys.Date())) %>% as_tibble()
) %>% 
  rename(date = ...1) %>% 
  gather(key = var, value = value, -date) %>% 
  drop_na() %>% 
  mutate(var = case_when(var == 'a193' ~ 'AD',
                         var == 'a023' ~ 'EUR',
                         var == 'a532' ~ 'HKD',
                         var == 'a548' ~ 'Ringgit',
                         var == 'a456' ~ 'Riyal',
                         var == 'a158' ~ 'Yen'),
         date = paste(substr(date, 1, 4),
                      case_when(str_detect(date, 'Jan') ~ '01',
                                str_detect(date, 'Feb') ~ '02',
                                str_detect(date, 'Mar') ~ '03',
                                str_detect(date, 'Apr') ~ '04',
                                str_detect(date, 'May') ~ '05',
                                str_detect(date, 'Jun') ~ '06',
                                str_detect(date, 'Jul') ~ '07',
                                str_detect(date, 'Aug') ~ '08',
                                str_detect(date, 'Sep') ~ '09',
                                str_detect(date, 'Oct') ~ '10',
                                str_detect(date, 'Nov') ~ '11',
                                str_detect(date, 'Dec') ~ '12'),
                      '01',
                      sep = '-') %>% as_date())

#extract trade data - the loop is set up so that it gets around the 5000 series per query limit
raw <- NULL
for(i in 1:length(seq(1, length(varcodes), 5000)) ){
  raw[[i]] <- haver.data(varcodes[seq(1, length(varcodes), 5000)[i]:min(seq(1, length(varcodes), 5000)[i] + 4999, length(varcodes))],
                         frequency = 'M',
                         start = as_date('1940-01-01'),
                         end = as_date(Sys.Date()))
  raw[[i]] <- bind_cols(raw[[i]] %>% rownames(),
                        raw[[i]] %>%  as_tibble()) %>% 
    rename(date = ...1) %>% 
    gather(key = code, value = value, -date) %>% 
    drop_na()
                         
}

#pool data, drop missing, make dates fancy
data <- bind_rows(raw) %>% 
  mutate(date = paste(substr(date, 1, 4),
                      case_when(str_detect(date, 'Jan') ~ '01',
                                str_detect(date, 'Feb') ~ '02',
                                str_detect(date, 'Mar') ~ '03',
                                str_detect(date, 'Apr') ~ '04',
                                str_detect(date, 'May') ~ '05',
                                str_detect(date, 'Jun') ~ '06',
                                str_detect(date, 'Jul') ~ '07',
                                str_detect(date, 'Aug') ~ '08',
                                str_detect(date, 'Sep') ~ '09',
                                str_detect(date, 'Oct') ~ '10',
                                str_detect(date, 'Nov') ~ '11',
                                str_detect(date, 'Dec') ~ '12'),
                      '01',
                      sep = '-') %>% as_date()) %>% 
  left_join(concentration, by = 'code') %>% 
  select(-database, -code)


#More dataprep
data <- data %>% 
  #Deal with currency conversion
  left_join(currencyconv, by = c('date', 'currency' = 'var')) %>% 
  mutate(value.y = ifelse(currency == 'USD', 1, value.y)) %>% 
  mutate(value = value.x/value.y) %>% 
  select(-currency, -contains('.')) %>% 
  #Japan in Bn isntead of Mn
  mutate(value = ifelse(country == 'Japan', value*1000, value))

#Do seasonal adjustments, !!!!WARNING!!!!: This takes very long, as the code has to iterate through
#abt. 52k data series and in some cases several model specifications for the ARiMA SEATS adjustor
x13adj <- function(value, freq, date){
  require(forecast)
  require(seasonal)
  require(tidyverse)
  tryCatch(
  value %>% 
    ts(frequency = freq,
       start = c(date %>% min %>% year, 
                 date %>% min %>% month)) %>% 
    seas() %>% 
    seasadj(),
  error = function(error){value}
  )
}

#data <- data %>% 
#  group_by(country, expimp, partner) %>% 
#  #Seasonal adjustments
#  mutate(value = x13adj(value, 12, date))

#clean up extra discepancies
#1) data from 2023 is not fully available as of yet, so remove anything past 2022 december,
#2) If the value of trade btw partners is 0 it should be dropped as it is not a vaild observation as trade partner relations
data <- data %>% 
  filter(year(date) < 2023) %>% 
  mutate(value = ifelse(value == 0, NA, value)) %>% 
  drop_na()

#If country exists as trade partner but not as a country (or vice versa) - correct the discrepancy by swapping the two columns (and exports/import relation)

#This gets the list of those countries / partners
countrypartner <- full_join(  
  data %>% 
    arrange(country) %>% 
    select(country) %>% 
    distinct(country) %>% 
    mutate(val = country),
  data %>% 
    arrange(partner) %>% 
    select(partner) %>% 
    distinct(partner) %>% 
    mutate(val = partner),
  by = c('country' = 'partner')
) %>% 
  filter(is.na(val.x) | is.na(val.y))


#Addig the new data back to the pool
data <- data %>% 
  bind_rows(
    data %>% 
      filter(country %in% countrypartner$country) %>% 
      mutate(country2 = partner,
             partner2 = country) %>% 
      select(-country, -partner) %>% 
      rename(country = country2,
             partner = partner2) %>% 
      mutate(expimp = ifelse(expimp == 'Exports', 'Imports', 'Exports')) ,
    
    data %>% 
      filter(partner %in% countrypartner$country) %>% 
      mutate(country2 = partner,
             partner2 = country) %>% 
      select(-country, -partner) %>% 
      rename(country = country2,
             partner = partner2) %>% 
      mutate(expimp = ifelse(expimp == 'Exports', 'Imports', 'Exports')) 
  )


#Clean up import-export relationship discrepancies 
#(simlarly as before, if country i exports to / imports from partner j and is not indicated as a separate data series, 
#add these is as extra datapoints)
data <- data %>% 
  mutate(countrypartner = paste(expimp, ': ', country, ' - ', partner, sep = ''),
         partnercountry = paste(ifelse(expimp == 'Exports', 'Imports', 'Exports'), ': ', partner, ' - ', country, sep = '')) 

#This gets the missing datapoints
importsexports <- full_join(
  data %>% 
    distinct(countrypartner) %>% 
    mutate(val = countrypartner),
  data %>% 
    distinct(partnercountry) %>% 
    mutate(val = partnercountry),
  by = c('countrypartner' = 'partnercountry')
) %>% 
  filter(is.na(val.x) | is.na(val.y)) 

#This adds the new datapoints back in the pool
data <- data %>% 
  bind_rows(
    data %>% 
      filter(countrypartner %in% importsexports$countrypartner) %>% 
      mutate(country2 = partner,
             partner2 = country) %>% 
      select(-country, -partner) %>% 
      rename(country = country2,
             partner = partner2) %>% 
      mutate(expimp = ifelse(expimp == 'Exports', 'Imports', 'Exports')),
    data %>% 
      filter(partnercountry %in% importsexports$countrypartner) %>% 
      mutate(country2 = partner,
             partner2 = country) %>% 
      select(-country, -partner) %>% 
      rename(country = country2,
             partner = partner2) %>% 
      mutate(expimp = ifelse(expimp == 'Exports', 'Imports', 'Exports'))
  ) %>% 
  select(-countrypartner, -partnercountry) %>% 
  #this is just in case my logic was flawed and duplicated some things - turns out I did
  distinct(date, country, expimp, partner, .keep_all = T)


#Create Total trade data to append to the original database
data <- data %>% 
  bind_rows(data %>% 
              group_by(date, country, partner) %>% 
              summarize(value = sum(value, na.rm = T)) %>% 
              mutate(expimp = 'Total Trade')
  )

saveRDS(data, file.path('data', 'concentration.rds'))

data %>% 
  filter(date >= 1970) %>% 
  group_by(date, country, expimp) %>% 
  count() %>% 
  filter(n < 10)
  


data %>% 
  group_by(country, date, expimp) %>% 
  mutate(n = n(),
            sum = sum(value),
            share = value/sum,
            hhi = sum(share*share),
            hhin = hhi/(1-1/n)) %>% 
  select(-share, -sum) %>% 
  filter(hhi <= 1,
         hhin <= 1) %>% 
  ungroup() %>% 
  group_by(date, expimp) %>% 
  mutate(mhhi = mean(hhi, na.rm = T),
         mhhin = mean(hhin, na.rm = T),
         wmhhi = sum(hhi*value/sum(value)),
         wmhhin = sum(hhin*value/sum(value))) %>% 
  ungroup() %>% 
  distinct(date, expimp, .keep_all = T) %>%
  filter(year(date) >= 1970) %>% 
  ggplot(aes(x = date, y = 100*wmhhin, color = expimp)) +
  geom_line()
  


data %>% 
  group_by(country, date, expimp) %>% 
  summarize(n = n(),
         sum = sum(value),
         share = value/sum,
         hhi = sum(share*share),
         hhin = hhi/(1-1/n)) %>% 
  select(hhi, hhin) %>% 
  filter(hhi <= 1,
         hhin <= 1) %>% 
  distinct(country, date, expimp, .keep_all = TRUE) %>% 
  filter(country %in% c('US', 'UK', 'China', 'Brazil',  'South Korea',
                        'Japan', 'Germany', 'France', 'Russia',
                        'Taiwan', 'Thailand', 'Ukraine', 'Hungary',
                        'Uruguay', 'North Korea')) %>% 
  ggplot(aes(x = date, y = 100*hhin, color = expimp)) + 
  geom_line() +
  facet_wrap(~country, scales = 'free')




