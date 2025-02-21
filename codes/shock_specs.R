library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)
library(sandwich)

shocks <- read_rds(file.path('data', 'gta_shocks.rds')) 

dates_m <- seq(mindate, maxdate, by = 'month') %>% 
  as_tibble() %>% 
  rename(date = value) %>% 
  mutate(temp = 'temp')

dates_q <- seq(mindate-31, maxdate, by = 'quarter') %>% 
  as_tibble() %>% 
  rename(date = value) %>% 
  mutate(temp = 'temp')

specs <- NULL
shock_to_q <- function(data){
  data %>% 
    mutate(y = year(date),
           q = quarter(date)) %>% 
    group_by_at(vars(!contains('shock') & !contains('date'))) %>% 
    mutate(across(contains('shock'), ~sum(.x))) %>% 
    filter(q == 1 & month(date) == 1 | q == 2 & month(date) == 4 | q == 3 & month(date) == 7 | q == 4 & month(date) == 10) %>% 
    ungroup() %>% 
    select(-y, -q)
  
}

specs[['main']] <- shocks %>% 
  filter(country_aff %in% countries,
         country_imp %in% countries) %>% 
  arrange(country_aff, country_imp, date_in) %>% 
  group_by(country_aff, country_imp, date_in, gta_eval) %>% 
  count() %>% 
  mutate(n = ifelse(gta_eval == 'Red', -n, n)) %>% 
  ungroup() %>% 
  group_by(country_aff, country_imp, date_in) %>% 
  summarize(shock = sum(n)) %>% 
  rename(date = date_in)


specs[['sign']] <- shocks %>% 
  filter(country_aff %in% countries,
         country_imp %in% countries) %>% 
  arrange(country_aff, country_imp, date_in) %>% 
  group_by(country_aff, country_imp, date_in, gta_eval) %>% 
  count() %>% 
  spread(gta_eval, n) %>% 
  mutate(Red = replace_na(Red, 0) %>% 
           abs(),
         Green = replace_na(Green, 0) %>% 
           abs()) %>% 
  rename(shock_pos = Green,
         shock_neg = Red,
         date = date_in)


specs[['duration']] <- shocks %>% 
  filter(country_aff %in% countries,
         country_imp %in% countries) %>% 
  mutate(dur = ifelse(is.na(date_out), 'Permanent', 'Transitory')) %>% 
  arrange(country_aff, country_imp, date_in) %>% 
  group_by(country_aff, country_imp, date_in, dur, gta_eval) %>% 
  count() %>% 
  mutate(n = ifelse(gta_eval == 'Red', -n, n)) %>% 
  ungroup() %>% 
  group_by(country_aff, country_imp, date_in, dur) %>% 
  summarize(shock = sum(n)) %>% 
  spread(dur, shock) %>% 
  mutate(Permanent = replace_na(Permanent, 0),
         Transitory = replace_na(Transitory, 0)) %>% 
  rename(date = date_in,
         shock_perm = Permanent,
         shock_trans = Transitory)

specs_m <- specs
specs_q <- lapply(specs, shock_to_q)


for(i in 1:length(specs_q)){
  if(ncol(specs_q[[i]]) == 4){
    suppressMessages(
      specs_q[[i]] <- specs_q[[i]] %>% 
        ungroup() %>% 
        group_by(country_aff, country_imp) %>% 
        nest() %>% 
        mutate(data = map(data, ~.x %>% 
                            full_join(dates_q) %>% 
                            select(-temp) %>% 
                            arrange(date))) %>% 
        unnest(data) %>% 
        mutate(shock = replace_na(shock, 0)) %>% 
        group_by(country_aff, country_imp) %>% 
        mutate(map_dfc(seq(4), ~ lag(shock, n = .x)) %>%
                 set_names(paste('shock_lag', seq(4),sep = ''))) %>% 
        drop_na()
    )
  }
  
}

suppressMessages(
specs_q[['sign']] <- specs_q[['sign']] %>% 
  ungroup() %>% 
  group_by(country_aff, country_imp) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% 
                      full_join(dates_q) %>% 
                      select(-temp) %>% 
                      arrange(date))) %>% 
  unnest(data) %>% 
  mutate(shock_pos = replace_na(shock_pos, 0),
         shock_neg = replace_na(shock_neg, 0)) %>% 
  group_by(country_aff, country_imp) %>% 
  mutate(map_dfc(seq(4), ~ lag(shock_pos, n = .x)) %>%
           set_names(paste('shock_pos_lag', seq(4),sep = ''))) %>% 
  mutate(map_dfc(seq(4), ~ lag(shock_neg, n = .x)) %>%
           set_names(paste('shock_neg_lag', seq(4),sep = ''))) %>% 
  drop_na()
)
suppressMessages(
specs_q[['duration']] <- specs_q[['duration']] %>% 
  ungroup() %>% 
  group_by(country_aff, country_imp) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% 
                      full_join(dates_q) %>% 
                      select(-temp) %>% 
                      arrange(date))) %>% 
  unnest(data) %>%  
  mutate(shock_perm = replace_na(shock_perm, 0),
         shock_trans = replace_na(shock_trans, 0)) %>% 
  group_by(country_aff, country_imp) %>% 
  mutate(map_dfc(seq(4), ~ lag(shock_perm, n = .x)) %>%
           set_names(paste('shock_perm_lag', seq(4),sep = ''))) %>% 
  mutate(map_dfc(seq(4), ~ lag(shock_trans, n = .x)) %>%
           set_names(paste('shock_trans_lag', seq(4),sep = ''))) %>% 
  drop_na()
)

for(i in 1:length(specs_m)){
  if(ncol(specs_m[[i]]) == 4){
    suppressMessages(
      specs_m[[i]] <- specs_m[[i]] %>% 
        ungroup() %>% 
        group_by(country_aff, country_imp) %>% 
        nest() %>% 
        mutate(data = map(data, ~.x %>% 
                            full_join(dates_m) %>% 
                            select(-temp) %>% 
                            arrange(date))) %>% 
        unnest(data) %>% 
        mutate(shock = replace_na(shock, 0)) %>% 
        group_by(country_aff, country_imp) %>% 
        mutate(map_dfc(seq(12), ~ lag(shock, n = .x)) %>%
                 set_names(paste('shock_lag', seq(12),sep = ''))) %>% 
        drop_na()
    )
  }
  
}

suppressMessages(
specs_m[['sign']] <- specs_m[['sign']] %>%
  ungroup() %>% 
  group_by(country_aff, country_imp) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% 
                      full_join(dates_m) %>% 
                      select(-temp) %>% 
                      arrange(date))) %>% 
  unnest(data) %>% 
  mutate(shock_pos = replace_na(shock_pos, 0),
         shock_neg = replace_na(shock_neg, 0)) %>% 
  group_by(country_aff, country_imp) %>% 
  mutate(map_dfc(seq(12), ~ lag(shock_pos, n = .x)) %>%
           set_names(paste('shock_pos_lag', seq(12),sep = ''))) %>% 
  mutate(map_dfc(seq(12), ~ lag(shock_neg, n = .x)) %>%
           set_names(paste('shock_neg_lag', seq(12),sep = ''))) %>% 
  drop_na()
)
suppressMessages(
specs_m[['duration']] <- specs_m[['duration']] %>% 
  ungroup() %>% 
  group_by(country_aff, country_imp) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% 
                      full_join(dates_m) %>% 
                      select(-temp) %>% 
                      arrange(date))) %>% 
  unnest(data) %>% 
  mutate(shock_perm = replace_na(shock_perm, 0),
         shock_trans = replace_na(shock_trans, 0)) %>%  
  group_by(country_aff, country_imp) %>% 
  mutate(map_dfc(seq(12), ~ lag(shock_perm, n = .x)) %>%
           set_names(paste('shock_perm_lag', seq(12),sep = ''))) %>% 
  mutate(map_dfc(seq(12), ~ lag(shock_trans, n = .x)) %>%
           set_names(paste('shock_trans_lag', seq(12),sep = ''))) %>% 
  drop_na()
)

specs <- list(specs_m = specs_m, specs_q = specs_q)
saveRDS(specs, file.path('data', 'shocks_est.rds'))
