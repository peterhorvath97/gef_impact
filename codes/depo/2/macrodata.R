library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)
library(imfr)

#rm(list = ls())
#dbs <- imfr::imf_databases() %>% 
#  filter(!str_detect(database_id, one_or_more(DGT)))

#params <- imfr::imf_parameters('bop')
#vars <- params[['indicator']] %>% 
#  as_tibble()

#PCPS
#Primary Commodity Price System (PCPS)

#PCTOT
#Commodity Terms of Trade

#vars %>% 
#  filter(str_detect(tolower(description), 'current')) %>% 
#  print(n = 100)


#IFS - indicator
'
#Production
NGDP_SA_XDC    Gross Domestic Product, Nominal, Seasonally Adjusted, Domestic Currency                              
AIP_IX            Economic Activity, Industrial Production, Index 


#Prices
PCPI_IX           Prices, Consumer Price Index, All items, Index                                                     
PXP_IX           Prices, Export Price Index, All Commodities, Index                                                 
PMP_IX           Prices, Import Price Index, All Commodities, Index                                                 
PPPI_IX          Prices, Producer Price Index, All Commodities, Index    

#International
EREER_IX         Exchange Rates, Real Effective Exchange Rate based on Consumer Price Index, Index
NX_R_SA_XDC          Exports of Goods and Services, Real, Seasonally Adjusted, Domestic Currency   
NM_R_SA_XDC          Imports of Goods and Services, Real, Seasonally Adjusted, Domestic Currency                    
'

#BOP - indicator
'
BCA_BP6_XDC      Current Account, Total, Net, National Currency
'
#m(params, vars)

#Quarterly
gdp <- imfr::imf_dataset('ifs', 
                  indicator = 'NGDP_SA_XDC',
                  freq = 'Q') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, gdp = value) %>% 
  as_tibble() %>% 
  mutate(year = substr(date, 1, 4),
         quarter = substr(date, nchar(date), nchar(date)),
         quarter = case_when(quarter == '1' ~ '-01-01',
                             quarter == '2' ~ '-04-01',
                             quarter == '3' ~ '-07-01',
                             quarter == '4' ~ '-10-01'),
         date = paste(year, quarter, sep = '') %>% as_date()) %>% 
  select(-year, -quarter)

ca <- imfr::imf_dataset('bop', 
                         indicator = 'BCA_BP6_XDC',
                         freq = 'Q') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, ca = value) %>% 
  as_tibble() %>% 
  mutate(year = substr(date, 1, 4),
         quarter = substr(date, nchar(date), nchar(date)),
         quarter = case_when(quarter == '1' ~ '-01-01',
                             quarter == '2' ~ '-04-01',
                             quarter == '3' ~ '-07-01',
                             quarter == '4' ~ '-10-01'),
         date = paste(year, quarter, sep = '') %>% as_date()) %>% 
  select(-year, -quarter)

exp <- imfr::imf_dataset('ifs', 
                         indicator = 'NX_R_SA_XDC',
                         freq = 'Q') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, exp = value) %>% 
  as_tibble() %>% 
  mutate(year = substr(date, 1, 4),
         quarter = substr(date, nchar(date), nchar(date)),
         quarter = case_when(quarter == '1' ~ '-01-01',
                             quarter == '2' ~ '-04-01',
                             quarter == '3' ~ '-07-01',
                             quarter == '4' ~ '-10-01'),
         date = paste(year, quarter, sep = '') %>% as_date()) %>% 
  select(-year, -quarter)

imp <- imfr::imf_dataset('ifs', 
                         indicator = 'NM_R_SA_XDC',
                         freq = 'Q') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, imp = value) %>% 
  as_tibble() %>% 
  mutate(year = substr(date, 1, 4),
         quarter = substr(date, nchar(date), nchar(date)),
         quarter = case_when(quarter == '1' ~ '-01-01',
                             quarter == '2' ~ '-04-01',
                             quarter == '3' ~ '-07-01',
                             quarter == '4' ~ '-10-01'),
         date = paste(year, quarter, sep = '') %>% as_date()) %>% 
  select(-year, -quarter)


ip <- imfr::imf_dataset('ifs', 
                         indicator = 'AIP_IX',
                         freq = 'M') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, ip = value) %>% 
  as_tibble() %>% 
  mutate(date = paste(date, '-01', sep = '') %>% as_date())

cpi <- imfr::imf_dataset('ifs', 
                        indicator = 'PCPI_IX',
                        freq = 'M') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, cpi = value) %>% 
  as_tibble() %>% 
  mutate(date = paste(date, '-01', sep = '') %>% as_date())

ppi <- imfr::imf_dataset('ifs', 
                        indicator = 'PPPI_IX',
                        freq = 'M') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, ppi = value) %>% 
  as_tibble() %>% 
  mutate(date = paste(date, '-01', sep = '') %>% as_date())

pimp <- imfr::imf_dataset('ifs', 
                        indicator = 'PMP_IX',
                        freq = 'M') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, pimp = value) %>% 
  as_tibble() %>% 
  mutate(date = paste(date, '-01', sep = '') %>% as_date())

pexp <- imfr::imf_dataset('ifs', 
                        indicator = 'PXP_IX',
                        freq = 'M') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, pexp = value) %>% 
  as_tibble() %>% 
  mutate(date = paste(date, '-01', sep = '') %>% as_date())

exch <- imfr::imf_dataset('ifs', 
                        indicator = 'EREER_IX',
                        freq = 'M') %>% 
  mutate(value = as.numeric(value)) %>% 
  select(ccode2 = ref_area, date, exch = value) %>% 
  as_tibble() %>% 
  mutate(date = paste(date, '-01', sep = '') %>% as_date())

countrycode::codelist %>% 
  select(ccode2 = iso2c,
         country = country.name.en)

dfs <- mget(ls())
dfs <- Filter(function(x) inherits(x, "tbl_df"), dfs)


suppressMessages(
dfs <- lapply(dfs, function(df) {
  left_join(df, countrycode::codelist %>% 
              select(ccode2 = iso2c, country = country.name.en)) %>% 
    select(-ccode2) %>% 
    drop_na()
})
)  


#Some cleanups in the macro aggregates
dfs[['tb']] <- dfs[['exp']] %>% 
  inner_join(dfs[['imp']]) %>% 
  mutate(tb = exp-imp) %>% 
  inner_join(dfs[['gdp']]) %>% 
  mutate(tb = 100*tb/gdp) %>% 
  select(country, date, tb)
dfs[['exp']] <- NULL
dfs[['imp']] <- NULL

dfs[['tot']] <- dfs[['pexp']] %>% 
  inner_join(dfs[['pimp']]) %>% 
  mutate(tot = 100*pexp/pimp) %>% 
  select(country, date, tot)
dfs[['pexp']] <- NULL
dfs[['pimp']] <- NULL

dfs[['ca']] <- dfs[['ca']] %>% 
  inner_join(dfs[['gdp']]) %>% 
  mutate(ca = 100*ca/gdp) %>% 
  select(country, date, ca)

dfs[['gdp']] <- dfs[['gdp']] %>% 
  group_by(country) %>%
  #mutate(gdp = 100*(gdp/lag(gdp,4)-1)) %>% 
  mutate(gdp = log(gdp)) %>% 
  drop_na() %>% 
  select(country, date, gdp)

dfs[['cpi']] <- dfs[['cpi']] %>% 
  group_by(country) %>%
  #mutate(cpi = 100*(cpi/lag(cpi,12)-1)) %>% 
  drop_na() %>% 
  select(country, date, cpi)

dfs[['ppi']] <- dfs[['ppi']] %>% 
  group_by(country) %>%
  #mutate(ppi = 100*(ppi/lag(ppi,12)-1)) %>% 
  drop_na() %>% 
  select(country, date, ppi)

dfs[['exch']] <- dfs[['exch']] %>% 
  group_by(country) %>%
  #mutate(exch = 100*(exch/lag(exch,12)-1)) %>% 
  drop_na() %>% 
  select(country, date, exch)

dfs[['ip']] <- dfs[['ip']] %>% 
  group_by(country) %>%
  #mutate(ip = 100*(ip/lag(ip,12)-1)) %>% 
  drop_na() %>% 
  select(country, date, ip)

saveRDS(dfs, file.path('data', 'macrovars.rds'))
#rm(list = ls())
