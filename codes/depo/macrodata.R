library(tidyverse)
library(Haver)
library(stringr)
library(lubridate)
library(rebus)
library(fredr)
library(forecast)
library(seasonal)
library(fredr)

haver.path('auto')
fredr_set_key("cda47ae66b38ed7988c0a9c2ec80c94f")


meta <- bind_rows(
  haver.metadata(database = 'EMERGE') %>% 
    as.tibble(),
  haver.metadata(database = 'G10') %>% 
    as.tibble()
)


ip <- meta %>% 
  filter(str_detect(code, START %R% or('x', 's', 'h') %R% '\\d\\d\\d' %R% 'd' %R% END  )) %>% 
  mutate(country = substr(descriptor, 
                          start = 1, 
                          stop = str_locate(descriptor, or(':', 'IP', 'Industrial'))[, 'start']) %>% 
           str_remove_all(':') %>% 
           str_remove_all(DOT) %>% 
           str_remove_all('I' %R% END) %>% 
           str_remove_all('IP') %>% 
           str_remove_all('Industrial') %>% 
           str_remove_all(SPACE %R% END)) %>% 
  filter(frequency == 'M') %>% 
  group_by(country) %>% 
  mutate(numobsmax = max(numobs)) %>% 
  filter(numobs == numobsmax) %>%
  ungroup() %>% 
  mutate(querycode = paste(database, ':', code, sep = ''),
         ccode = str_extract(code, '\\d\\d\\d')) %>% 
  select(querycode, country, descriptor, ccode, code)  %>% 
  mutate(variable = 'Industrial Production',
         varnam = 'ip') %>% 
  distinct(code, .keep_all = T)


pi <- meta %>% 
  filter(str_detect(code, START %R% or('x', 's', 'h') %R% '\\d\\d\\d' %R% 'pc' %R% END  )) %>% 
  mutate(country = substr(descriptor, 
                          start = 1, 
                          stop = str_locate(descriptor, or(':'))[, 'start']) %>% 
           str_remove_all(':') %>% 
           str_remove_all(DOT) %>% 
           str_remove_all(SPACE %R% END)) %>%  
  filter(frequency == 'M') %>% 
  group_by(country) %>% 
  mutate(numobsmax = max(numobs)) %>% 
  filter(numobs == numobsmax) %>%
  ungroup() %>% 
  mutate(querycode = paste(database, ':', code, sep = ''),
         ccode = str_extract(code, '\\d\\d\\d')) %>% 
  select(querycode, country, descriptor, ccode, code) %>% 
  mutate(variable = 'CPI', 
         varnam = 'pi') %>% 
  distinct(code, .keep_all = T)


r <- meta %>% 
  filter(str_detect(code, START %R% 'n' %R% '\\d\\d\\d' %R% or('ri3', 'rg3m') %R% END  )) %>%
  mutate(country = substr(descriptor, 
                          start = 1, 
                          stop = str_locate(descriptor, or(':'))[, 'start']) %>% 
           str_remove_all(':') %>% 
           str_remove_all(DOT) %>% 
           str_remove_all(SPACE %R% END)) %>%  
  filter(frequency == 'M') %>% 
  group_by(country) %>% 
  mutate(numobsmax = max(numobs)) %>% 
  filter(numobs == numobsmax) %>%
  ungroup() %>% 
  mutate(querycode = paste(database, ':', code, sep = ''),
         ccode = str_extract(code, '\\d\\d\\d')) %>% 
  select(querycode, country, descriptor, ccode, code) %>% 
  mutate(variable = 'Interest rate',
         varnam = 'r') %>% 
  distinct(code, .keep_all = T)
#Needs FRED augmentation for euro area countries


e <- haver.metadata(database = 'BIS') %>% 
  as.tibble() %>% 
  filter(str_detect(code, START %R% 'b' %R% '\\d\\d\\d' %R% 'xrb' %R% END  )) %>% 
  mutate(country = substr(descriptor, 
                          start = 1, 
                          stop = str_locate(descriptor, or(':'))[, 'start']) %>% 
           str_remove_all(':') %>% 
           str_remove_all(DOT) %>% 
           str_remove_all(SPACE %R% END)) %>%  
  filter(frequency == 'M') %>% 
  group_by(country) %>% 
  mutate(numobsmax = max(numobs)) %>% 
  filter(numobs == numobsmax) %>%
  ungroup() %>% 
  mutate(querycode = paste(database, ':', code, sep = ''),
         ccode = str_extract(code, '\\d\\d\\d')) %>% 
  select(querycode, country, descriptor, ccode, code) %>% 
  mutate(variable = 'Exchange rate',
         varnam = 'e') %>% 
  distinct(code, .keep_all = T)


tb <- meta %>% 
  filter(str_detect(code, START %R% or('x', 's', 'h') %R% '\\d\\d\\d' %R% or('ibd', 'ib') %R% END  )) %>% 
  mutate(country = substr(descriptor, 
                          start = 1, 
                          stop = str_locate(descriptor, or(':'))[, 'start']) %>% 
           str_remove_all(':') %>% 
           str_remove_all(DOT) %>% 
           str_remove_all(SPACE %R% END)) %>%  
  filter(frequency == 'M') %>%
  mutate(is_usa = ifelse(country == 'US', 1, 0),
         is_ibd = ifelse(str_detect(code, 'ibd'), 1, 0),
         keep = is_usa + is_ibd) %>% 
  filter(keep != 0) %>% 
  group_by(country) %>% 
  mutate(numobsmax = max(numobs)) %>% 
  filter(numobs == numobsmax) %>%
  ungroup() %>% 
  mutate(querycode = paste(database, ':', code, sep = ''),
         ccode = str_extract(code, '\\d\\d\\d')) %>% 
  select(querycode, country, descriptor, ccode, code) %>% 
  mutate(variable = 'Trade Balance',
         varnam = 'tb') %>% 
  distinct(code, .keep_all = T)


gdp <- meta %>% 
  filter(str_detect(code, START %R% or('x', 's', 'h') %R% '\\d\\d\\d' %R% 'ngpd' %R% END  )) %>% 
  mutate(country = substr(descriptor, 
                          start = 1, 
                          stop = str_locate(descriptor, or(':'))[, 'start']) %>% 
           str_remove_all(':') %>% 
           str_remove_all(DOT) %>% 
           str_remove_all(SPACE %R% END)) %>%  
  filter(frequency == 'Q') %>% 
  group_by(country) %>% 
  mutate(numobsmax = max(numobs)) %>% 
  filter(numobs == numobsmax) %>%
  ungroup() %>% 
  mutate(querycode = paste(database, ':', code, sep = ''),
         ccode = str_extract(code, '\\d\\d\\d')) %>% 
  select(querycode, country, descriptor, ccode, code) %>% 
  mutate(variable = 'GDP',
         varnam = 'y') %>% 
  distinct(code, .keep_all = T)


meta <- mget(objects()[!(objects() %in% c('meta', 'IMFenvs', 'data', 'gdp', 'i'))], 
                       envir = globalenv()) 

data <- NULL
for(i in 1:length(meta)){
  #download
data[[i]] <-haver.data( 
  codes = mget(objects()[!(objects() %in% c('meta', 'IMFenvs', 'data', 'gdp', 'i'))], 
                  envir = globalenv())[[i]]$querycode,
  frequency = 'M',
  start = as_date('1940-01-01'),
  end = as_date(Sys.time())
)

#clean
data[[i]] <- bind_cols(
  rownames(data[[i]]),
  as_tibble(data[[i]])
) %>% 
  rename(date = ...1) %>% 
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
  gather(key = code, value = value, -date) %>% 
  drop_na() %>% 
  left_join(meta[[i]],
            by = 'code')

}

#add gdp
data[[(length(meta)+1)]] <- bind_cols(haver.data(codes = gdp$querycode,
                                                 frequency = 'Q',
                                                 start = as_date('1940-01-01'),
                                                 end = as_date(Sys.time())) %>% rownames(),
                                      haver.data(codes = gdp$querycode,
                                                 frequency = 'Q',
                                                 start = as_date('1940-01-01'),
                                                 end = as_date(Sys.time())) %>% as_tibble()
) %>% 
  rename(date = ...1) %>% 
  mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                          str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                          str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                          str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
  ) %>% 
    as_date()
  ) %>% 
  gather(key = code, value = value, -date) %>% 
  drop_na() %>% 
  left_join(gdp,
            by = 'code')


#Cleaning up country names / codes
data <- bind_rows(data) %>% 
  mutate(country = case_when(ccode == 963 ~ 'Bosnia',
                             ccode == 936 ~ 'Slovakia',
                             ccode == 935 ~ 'Chech Republic',
                             ccode == 924 ~ 'China',
                             ccode == 576 ~ 'Singapore',
                             ccode == 542 ~ 'South Korea',
                             ccode == 532 ~ 'Hong Kong',
                             ccode == 466 ~ 'UAE',
                             TRUE ~ country))

data %>% 
  janitor::tabyl(ccode, country) %>% 
  mutate(across(-ccode, ~ifelse(.x == 0, 0, 1))) %>% 
  mutate(ccode = as.numeric(ccode)) %>% 
  mutate(rowsum = rowSums(.) - ccode) %>% 
  select(ccode, rowsum) %>% 
  filter(rowsum != 1) %>% 
  pull(ccode)

#Check
data %>% 
  filter(ccode %in% c('466', '532', '542', '576', '924', '935', '936', '963')) %>% 
  janitor::tabyl(country, ccode)

#FRED  
data <- data %>% 
  bind_rows(  fredr(series_id = 'IR3TIB01SKM156N',
                    frequency = 'm') %>% 
                select(date, value) %>% 
                mutate(country = 'Slovakia',
                       code = 'IR3TIB01SKM156N',
                       value = ifelse(is.na(value), lag(value), value)),
              fredr(series_id = 'LTUIR3TIB01STM',
                    frequency = 'm') %>% 
                select(date, value) %>% 
                mutate(country = 'Lithuania',
                       code = 'LTUIR3TIB01STM',
                       value = ifelse(is.na(value), lag(value), value)),
              fredr(series_id = 'IR3TIB01EEM156N',
                    frequency = 'm') %>% 
                select(date, value) %>% 
                mutate(country = 'Estonia',
                       code = 'IR3TIB01EEM156N',
                       value = ifelse(is.na(value), lag(value), value))) %>% 
  bind_rows(
    fredr(series_id = 'IR3TIB01ATM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Austria',
             code = 'IR3TIB01ATM156N',
             value = ifelse(is.na(value), lag(value), value)),
    fredr(series_id = 'IR3TIB01DEM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Germany',
             code = 'IR3TIB01DEM156N',
             value = ifelse(is.na(value), lag(value), value)),
    fredr(series_id = 'IR3TIB01FIM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Finland',
             code = 'IR3TIB01FIM156N',
             value = ifelse(is.na(value), lag(value), value)),
    fredr(series_id = 'IR3TIB01NLM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Netherlands',
             code = 'IR3TIB01NLM156N',
             value = ifelse(is.na(value), lag(value), value)),
    fredr(series_id = 'IR3TIB01GRM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Greece',
             code = 'IR3TIB01GRM156N',
             value = ifelse(is.na(value), lag(value), value)),
    fredr(series_id = 'IR3TIB01IEM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Ireland',
             code = 'IR3TIB01IEM156N',
             value = ifelse(is.na(value), lag(value), value)),
    fredr(series_id = 'IR3TIB01LUM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Luxembourg',
             code = 'IR3TIB01LUM156N',
             value = ifelse(is.na(value), lag(value), value)),
    fredr(series_id = 'IR3TIB01PTM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Portugal',
             code = 'IR3TIB01PTM156N',
             value = ifelse(is.na(value), lag(value), value)),
    fredr(series_id = 'IR3TIB01ESM156N',
          frequency = 'm') %>% 
      select(date, value) %>% 
      mutate(country = 'Spain',
             code = 'IR3TIB01ESM156N',
             value = ifelse(is.na(value), lag(value), value))) %>% 
  group_by(country) %>% 
  mutate(varnam = ifelse(is.na(varnam), 'r', varnam),
         variable = ifelse(is.na(variable), 'Interest rate', variable),
         ccode = max(ccode),
         descriptor = ifelse(is.na(descriptor), '3-month Interbank Rate', descriptor)
         )


  

data_clean <- data %>% 
  select(date, value, country, varnam) %>% 
  distinct(date, country, varnam, .keep_all = T) %>% 
  spread(varnam, value) %>% 
  mutate(quarter = quarter(date),
         year = year(date)) %>% 
  group_by(country, year, quarter) %>% 
  mutate(y = mean(y, na.rm = T)) %>% 
  ungroup() %>% 
  select(-quarter, -year) %>% 
  drop_na() %>% 
  mutate(tb = 100*tb/(1000*y)) %>% 
  select(-y)



metadata <- data %>% 
  filter(country %in% data_clean$country) %>% 
  select(country, descriptor, variable, code) %>% 
  distinct() 



data_clean %>% saveRDS(file.path('data', 'macrodata.rds'))
metadata %>% saveRDS(file.path('data', 'macrometa.rds'))