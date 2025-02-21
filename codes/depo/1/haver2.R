#Terms of trade data
get_tot <- function(){
  
  require(tidyverse)
  require(Haver)
  require(stringr)
  require(lubridate)
  require(rebus)
  
  haver.path('auto')
  
  emer_meta <- haver.metadata(database = 'EMERGE') %>% 
    as.tibble() %>% 
    mutate(database = 'EMERGE')
  
  adv_meta <- haver.metadata(database = 'G10') %>% 
    as.tibble() %>% 
    mutate(database = 'G10')
  
  
  all_meta <- bind_rows(emer_meta, adv_meta)
  
  tot_meta <- all_meta %>% 
    filter(frequency %in% c('M', 'Q'),
           str_detect(descriptor, 'Terms')) %>% 
    arrange(descriptor) %>% 
    mutate(numobs_adj = ifelse(frequency == 'M', numobs, 3*numobs),
           country = substr(descriptor,
                            start = 1,
                            stop = str_locate(descriptor, ':')[, 'start']) %>% 
             str_remove_all(':') %>% 
             str_remove_all(one_or_more(SPACE) %R% END)) %>% 
    group_by(country) %>% 
    filter(numobs_adj == max(numobs_adj)) %>% 
    distinct(country, .keep_all = T)
  
  
  raw1 <- haver.data(codes = filter(tot_meta, database == 'EMERGE')$code, 
                     database = 'EMERGE', 
                     frequency = 'Q', 
                     start = as_date('1940-01-01'), 
                     end =as_date(Sys.time())) 
  
  raw2 <- haver.data(codes = filter(tot_meta, database == 'G10')$code, 
                     database = 'G10', 
                     frequency = 'Q', 
                     start = as_date('1940-01-01'), 
                     end =as_date(Sys.time())) 
  
  tot_data <- bind_rows(
    bind_cols(rownames(raw2), as_tibble(raw2)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(tot_meta, by = 'code') %>% 
      select(date, country, tot = value) %>% 
      drop_na(),
    bind_cols(rownames(raw1), as_tibble(raw1)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(tot_meta, by = 'code') %>% 
      select(date, country, tot = value) %>% 
      drop_na()
  )
  
  meta <- bind_rows(
    bind_cols(rownames(raw2), as_tibble(raw2)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(tot_meta, by = 'code') %>% 
      select(country, code, descriptor) %>% 
      distinct(code, .keep_all = TRUE),
    bind_cols(rownames(raw1), as_tibble(raw1)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(tot_meta, by = 'code') %>% 
      select(country, code, descriptor) %>% 
      distinct(code, .keep_all = TRUE)
  )
  
  data <- list(data = tot_data, 
               meta = meta)
  
  data
}

get_open <- function(){
  haver.path('auto')
  
  emer_meta <- haver.metadata(database = 'EMERGE') %>% 
    as.tibble() %>% 
    mutate(database = 'EMERGE')
  
  adv_meta <- haver.metadata(database = 'G10') %>% 
    as.tibble() %>% 
    mutate(database = 'G10')
  
  
  all_meta <- bind_rows(emer_meta, adv_meta)
  
  #Get all metadata required
  expimp <- all_meta %>% 
    filter(str_detect(code, or(
      #DGT %R% 'ix' %R%  END, 
      # DGT %R% 'im' %R% END, 
      DGT %R% 'nx' %R% zero_or_more('c') %R% END,
      DGT %R% 'nm' %R% zero_or_more('c') %R% END) ),
      frequency != 'A') %>% 
    mutate(db = str_extract_all(code, or(
      #DGT %R% 'ix' %R%  END, 
      #DGT %R% 'im' %R% END, 
      DGT %R% 'nx' %R% zero_or_more('c') %R% END,
      DGT %R% 'nm' %R% zero_or_more('c') %R% END)) %>% 
        str_remove_all(DGT),
      db = case_when(db %in% c('im', 'ix') ~ 'BOP',
                     db %in% c('nm', 'nx', 'nmc', 'nxc') ~ 'GDP' ),
      numobs_adj = ifelse(frequency == 'M', numobs, 3*numobs),
      country = substr(descriptor,
                       start = 1,
                       stop = str_locate(descriptor, ':')[, 'start']) %>% 
        str_remove_all(':') %>% 
        str_remove_all(one_or_more(SPACE) %R% END) %>% 
        str_remove_all(DOT),
      datatype = ifelse(country == 'US', 'LocCur', datatype),
      expimp = ifelse(str_detect(descriptor, or('Import', 'import')), 'Import', 'Export'),
      seas = ifelse(str_detect(descriptor, 'NSA'), 'NSA', 'SA'),
      realnom = ifelse(str_detect(descriptor, DGT), 'Real', 'Nominal')) %>% 
    group_by(country, expimp, seas, realnom, aggtype, db) %>% 
    filter(numobs_adj == max(numobs_adj)) %>%  
    distinct(country, expimp, seas, realnom, aggtype, db, .keep_all = T) %>% 
    ungroup() %>% 
    group_by(country) %>% 
    mutate(n = n()) %>% 
    select(country, code, descriptor, aggtype, expimp, seas, realnom, n, numobs_adj, database, db) 
  
  
  gdp <- all_meta %>% 
    arrange(descriptor) %>% 
    mutate(numobs_adj = ifelse(frequency == 'M', numobs, 3*numobs),
           country = substr(descriptor,
                            start = 1,
                            stop = str_locate(descriptor, SPACE %R% or('G', 'R'))[, 'start']) %>% 
             str_remove_all(':') %>% 
             str_remove_all(one_or_more(SPACE) %R% END) %>% 
             str_remove_all(DOT),
           seas = ifelse(str_detect(descriptor, 'NSA'), 'NSA', 'SA'),
           realnom = ifelse(str_detect(descriptor, DGT), 'Real', 'Nominal'),
           datatype = ifelse(country == 'US', 'LocCur', datatype)) %>% 
    filter(str_detect(descriptor, or('GDP', 'Gross Domestic Product')),
           frequency == 'Q',
           str_detect(code, or('gpc', 'gdp')),
           datatype == 'LocCur') %>% 
    select(country, code, descriptor, aggtype, seas, realnom, numobs_adj, database) %>% 
    group_by(country, seas, realnom, aggtype) %>% 
    filter(numobs_adj == max(numobs_adj)) %>% 
    distinct(country, seas, realnom, aggtype, .keep_all = T) 
  
  
  codes_open <- bind_rows(expimp, gdp) %>% 
    ungroup() %>% 
    mutate(expimp = ifelse(!is.na(expimp), expimp, 'GDP')) %>% 
    rename(var = expimp) %>% 
    select(-descriptor, -database, -n, -numobs_adj, -db) %>% 
    spread(var, code) %>% 
    mutate(n = ifelse(!is.na(GDP), 1, 0) +
             ifelse(!is.na(Export), 1, 0)+
             ifelse(!is.na(Import), 1, 0)) %>% 
    filter(n == 3) %>% 
    group_by(country, aggtype, seas, realnom) %>% 
    select(-n) %>% 
    nest() %>% 
    spread(realnom, data) %>% 
    mutate(Real = ifelse(is.null(Real %>% unlist), Nominal, Real)) %>% 
    select(-Nominal) %>% 
    spread(seas, Real) %>% 
    mutate(SA = ifelse(is.null(SA %>% unlist), NSA, SA)) %>% 
    select(-NSA) %>% 
    spread(aggtype, SA) %>% 
    select(SUM) %>% 
    unnest(SUM) %>% 
    gather(key = var, value = code, Export, GDP, Import) %>% 
    pull(code)
  
  expimp <- expimp %>% 
    filter(code %in% codes_open)
  gdp <- gdp %>% 
    filter(code %in% codes_open)
  
  #Export-import data fetching
  raw1 <- haver.data(codes = filter(expimp, database == 'EMERGE')$code, 
                     database = 'EMERGE', 
                     frequency = 'Q', 
                     start = as_date('1940-01-01'), 
                     end =as_date(Sys.time())) 
  
  raw2 <- haver.data(codes = filter(expimp, database == 'G10')$code, 
                     database = 'G10', 
                     frequency = 'Q', 
                     start = as_date('1940-01-01'), 
                     end =as_date(Sys.time())) 
  
  
  expimp_data <- bind_rows(
    bind_cols(rownames(raw2), as_tibble(raw2)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(expimp, by = 'code') %>% 
      select(date, country, expimp, value) %>% 
      spread(expimp, value) %>% 
      rename(exp = Export,
             imp = Import) %>% 
      drop_na(),
    bind_cols(rownames(raw1), as_tibble(raw1)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(expimp, by = 'code') %>% 
      select(date, country, expimp, value) %>% 
      spread(expimp, value) %>% 
      rename(exp = Export,
             imp = Import) %>% 
      drop_na()
  )
  
  expimp_meta <- bind_rows(
    bind_cols(rownames(raw2), as_tibble(raw2)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(expimp, by = 'code') %>% 
      select(country, code, var = expimp, descriptor)  %>% 
      distinct(code, .keep_all = TRUE),
    bind_cols(rownames(raw1), as_tibble(raw1)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(expimp, by = 'code') %>% 
      select(country, code, var = expimp, descriptor)  %>% 
      distinct(code, .keep_all = TRUE)
  )
  
  
  
  #GDP data
  raw1 <- haver.data(codes = filter(gdp, database == 'EMERGE')$code, 
                     database = 'EMERGE', 
                     frequency = 'Q', 
                     start = as_date('1940-01-01'), 
                     end =as_date(Sys.time())) 
  
  raw2 <- haver.data(codes = filter(gdp, database == 'G10')$code, 
                     database = 'G10', 
                     frequency = 'Q', 
                     start = as_date('1940-01-01'), 
                     end =as_date(Sys.time())) 
  
  
  gdp_data <- bind_rows(
    bind_cols(rownames(raw2), as_tibble(raw2)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(gdp, by = 'code') %>% 
      select(date, country, gdp = value) %>% 
      drop_na(),
    bind_cols(rownames(raw1), as_tibble(raw1)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(gdp, by = 'code') %>% 
      select(date, country, gdp = value) %>% 
      drop_na()
  )
  
  gdp_meta <- bind_rows(
    bind_cols(rownames(raw2), as_tibble(raw2)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(gdp, by = 'code') %>% 
      select(country, code, descriptor)  %>% 
      distinct(code, .keep_all = TRUE) %>% 
      mutate(var = 'GDP'),
    bind_cols(rownames(raw1), as_tibble(raw1)) %>% 
      rename(date = ...1) %>% 
      mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                              str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                              str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                              str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
      ) %>% 
        as_date()
      ) %>% 
      gather(key = code, value = value, -date) %>% 
      left_join(gdp, by = 'code') %>% 
      select(country, code, descriptor)  %>% 
      distinct(code, .keep_all = TRUE) %>% 
      mutate(var = 'GDP')
  )
  
  #Adding china data
  china <- bind_rows(
    all_meta %>% 
      filter(str_detect(descriptor, 'China'),
             str_detect(descriptor, or('Export', 'Import')),
             frequency != 'A',
             str_detect(datatype, or('LocCur', 'US' %R% DOLLAR 
             ))) %>% 
      select(database, code, descriptor) %>% 
      filter(code %in% c('h924bcgm', 'h924bcgx', 'h924bcsm', 'h924bcsx')),
    
    all_meta %>% 
      filter(str_detect(descriptor, 'China'),
             str_detect(descriptor, or('GDP', 'Gross Domestic')),
             frequency != 'A',
             datatype == 'US$') %>% 
      select(database, code, descriptor) %>% 
      filter(code == 'n924ngpd')
  )
  
  china_raw <- haver.data(codes = filter(china, database == 'EMERGE')$code, 
                          database = 'EMERGE', 
                          frequency = 'Q', 
                          start = as_date('1940-01-01'), 
                          end =as_date(Sys.time())) 
  
  china_data <- bind_cols(rownames(china_raw), as_tibble(china_raw)) %>% 
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
    spread(code, value) %>% 
    mutate(exp = h924bcgx + h924bcsx,
           imp = h924bcgm + h924bcsx,
           gdp = n924ngpd,
           country = 'China') %>% 
    select(date, country, exp, imp, gdp)
  
  china_meta <- china %>% 
    mutate(country = 'China') %>% 
    select(-database)
  
  expimp_data <- expimp_data %>% 
    bind_rows(china_data %>% 
                select(date, country, exp, imp) %>% 
                drop_na())
  gdp_data <- gdp_data %>% 
    bind_rows(china_data %>% 
                select(date, country, gdp) %>% 
                drop_na())
  expimp_meta <- expimp_meta %>% 
    bind_rows(china_meta %>% 
                filter(!str_detect(descriptor, 'Gross Domestic Product')) %>% 
                mutate(var = ifelse(str_detect(descriptor, 'Export'), 'Export', 'Import')) )
  gdp_meta <- gdp_meta %>% 
    bind_rows(china_meta %>% 
                filter(str_detect(descriptor, 'Gross Domestic Product')) %>% 
                mutate(var = 'GDP'))
  
  #Seasonal Adjustments
  nsa_expimp <- expimp_meta %>% 
    filter(str_detect(descriptor, 'NSA')) %>% 
    select(country) %>% 
    distinct(country) %>% 
    pull(country) %>% c('Iceland', 'Latvia')
  
  nsa_gdp <- gdp_meta %>% 
    filter(str_detect(descriptor, 'NSA')) %>% 
    select(country) %>% 
    distinct(country) %>% 
    pull(country) %>% c('Iceland', 'Latvia')
  
  
  
  gdp_data <- gdp_data %>% 
    filter(country != 'Ghana') %>% 
    group_by(country) %>% 
    mutate(gdp = ifelse(country %in% nsa_gdp,
                        gdp %>% 
                          ts(frequency = 4,
                             start = c(date %>% min %>% year, 
                                       date %>% min %>% month)) %>% 
                          seas() %>% 
                          seasadj(),
                        gdp))
  
  
  expimp_data <- expimp_data %>% 
    filter(country != 'Ghana') %>% 
    group_by(country) %>% 
    mutate(imp = ifelse(country %in% nsa_expimp,
                        imp %>% 
                          ts(frequency = 4,
                             start = c(date %>% min %>% year, 
                                       date %>% min %>% month)) %>% 
                          seas() %>% 
                          seasadj(),
                        imp)) %>% 
    mutate(exp = ifelse(country %in% nsa_expimp,
                        exp %>% 
                          ts(frequency = 4,
                             start = c(date %>% min %>% year, 
                                       date %>% min %>% month)) %>% 
                          seas() %>% 
                          seasadj(),
                        exp))
  
  
  
  
  
  #Finally calculate openness measure
  open_data <- expimp_data %>% 
    inner_join(gdp_data, by = c('country', 'date')) %>% 
    mutate(gdp = ifelse(country == 'China', gdp*1000, gdp),
           open = 100*(exp+imp)/gdp) 
  
  
  #USD gdp weights
  gdpusd <- all_meta %>% 
    filter(datatype == 'US$' | str_detect(descriptor, START %R% 'U.S.') ,
           str_detect(descriptor, or('GDP', 'Gross Domestic Product')),
           frequency == 'Q',
           str_detect(descriptor, "\\d{2,}"),
           !str_detect(descriptor, 'GDP:'),
           aggtype == 'SUM'
    ) %>% 
    mutate(country = substr(descriptor, 
                            start = 1, 
                            stop = str_locate(descriptor, SPACE %R% or('G', 'R'))[, 'start']) %>% 
             str_remove_all(':') %>% 
             str_replace_all('Palestinian Territories', 'Palestine') %>% 
             str_replace_all(' NIPA', '') %>% 
             str_replace_all('&', 'and') %>% 
             str_remove_all(SPACE %R% END) %>% 
             str_remove_all(DOT)) %>% 
    mutate(last = substr(code, 
                         start = nchar(code) -2,
                         stop = nchar(code))) %>% 
    filter(last  %in%  c('gcd', 'gpc')) %>% 
    select(-last) %>% 
    mutate(seas = ifelse(str_detect(descriptor, 'NSA'), 'NSA', 'SA'),
           seas2 = ifelse(seas == 'SA', 2, 1),
           has2010 = ifelse(str_detect(descriptor, '2010'), 1, 0)) %>% 
    group_by(country) %>% 
    filter(seas2 == max(seas2),
           has2010 == max(has2010)) %>% 
    filter(numobs == max(numobs)) %>% 
    distinct(country, .keep_all = TRUE)
  
  
  gdpusd_raw1 <- haver.data(codes = filter(gdpusd, database == 'G10')$code, 
                            database = 'G10', 
                            frequency = 'Q', 
                            start = as_date('1940-01-01'), 
                            end =as_date(Sys.time())) 
  
  gdpusd_raw2 <- haver.data(codes = filter(gdpusd, database == 'EMERGE')$code, 
                            database = 'EMERGE', 
                            frequency = 'Q', 
                            start = as_date('1940-01-01'), 
                            end =as_date(Sys.time())) 
  
  gdpusd_data <- bind_rows(
    bind_cols(rownames(gdpusd_raw1), as_tibble(gdpusd_raw1)) %>% 
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
      left_join(gdpusd %>% 
                  select(code, country),
                by = 'code') %>% 
      select(country, date, gdpusd = value),
    bind_cols(rownames(gdpusd_raw2), as_tibble(gdpusd_raw2)) %>% 
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
      left_join(gdpusd %>% 
                  select(code, country),
                by = 'code') %>% 
      select(country, date, gdpusd = value)
  )
  
  gdpusd_meta <- gdpusd %>% 
    select(code, country, descriptor) %>% 
    mutate(var = 'GDP in USD')
  
  
  open_data <- open_data %>% 
    select(date, country, open) %>% 
    left_join(
      open_data %>% 
        select(date, country, open) %>% 
        inner_join(gdpusd_data, by = c('country', 'date'))  %>% 
        filter(country != 'EA20',
               country != 'EU 27') %>% 
        drop_na() %>% 
        ungroup %>% 
        group_by(date) %>% 
        filter(date < as_date('2023-04-01')) %>% 
        mutate(w = gdpusd/sum(gdpusd),
               wopen = w*open) %>% 
        summarize(wopen = sum(wopen),
                  mopen = mean(open)) %>% 
        select(date, gopen = wopen),
      by = 'date'
    )
  
  
  open_data  
  
}

haver.path('auto')

's001itt'

bind_cols(
  haver.data(codes =c('s001iqxm', 's001iuxm'),
             database = 'G10',
             frequency = 'Q',
             start = as_date('1940-01-01'),
             end = as_date(Sys.time())) %>% rownames(),
  haver.data(codes =c('s001iqxm', 's001iuxm'),
             database = 'G10',
             frequency = 'Q',
             start = as_date('1940-01-01'),
             end = as_date(Sys.time())) %>% as_tibble()
) %>% 
  rename(date = ...1,
         vol = s001iqxm,
         price = s001iuxm) %>% 
  drop_na() %>% 
  mutate(date = case_when(str_detect(date, 'Q1')  ~ paste(substr(date, 1,4), '-01-01', sep = ''),
                          str_detect(date, 'Q2')  ~ paste(substr(date, 1,4), '-04-01', sep = ''),
                          str_detect(date, 'Q3')  ~ paste(substr(date, 1,4), '-07-01', sep = ''),
                          str_detect(date, 'Q4')  ~ paste(substr(date, 1,4), '-10-01', sep = '')
  ) %>% 
    as_date()) %>%
  select(-date) %>% 
  vars::VAR(p = 3) %>% 
  resid() %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  gather(key = 'var',
         value = 'value', 
         vol, price) %>% 
  ggplot(aes(x = as.numeric(rowname), y = value)) +
  geom_line() +
  facet_wrap(~var, scales = 'free')


meta_list <- NULL
pb = txtProgressBar(min = 0, max = length(haver.databases()), initial = 0, style = 3) 
for(i in 1:length(haver.databases())){
  meta_list[[i]] <- haver.metadata(database = haver.databases()[i]) %>%
    as_tibble() 
  setTxtProgressBar(pb,i)
}
close(pb)
rm(pb)

bind_rows(meta_list) %>% 
  distinct(code, .keep_all = T)



meta_list %>% bind_rows() %>% saveRDS('metadata.rds')
