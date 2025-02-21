library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)

macrodata <- read_rds(file.path('data', 'macrodata.rds')) %>% 
  mutate(country = ifelse(country == 'Chech Repbublic', 'Czech Republic', country))

macrodata %>% 
  mutate(year = year(date)) %>% 
  filter(year == min(year))

concentration <- read_rds(file.path('data', 'concentration.rds'))

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


concentration_loc <- concentration %>% 
  group_by(country, date, expimp) %>% 
  mutate(n = n(),
         sum = sum(value),
         share = value/sum,
         hhi = sum(share*share),
         hhin = hhi/(1-1/n)) %>% 
  select(-share, -sum) %>%
  mutate(hhi = ifelse(hhi > 1, 1, hhi),
         hhin = ifelse(hhin > 1, 1, hhin),
         hhi = ifelse(hhi < 1/n, 1/n, hhi),
         hhin = ifelse(hhin < 0, 0, hhin)) %>% 
  filter(year(date) >= 1994) %>%
  mutate(value = sum(value)) %>% 
  select(-partner) %>% 
  distinct(country, date, expimp, .keep_all =  T) %>% 
  mutate(hhi = 100*hhi,
         hhin = 100* hhin) #%>% 
  #mutate(hhi = x13adj(hhi, 12, date),
  #       hhin = x13adj(hhin, 12, date)) %>% 
  
concentration_glob <-concentration_loc %>% 
  ungroup() %>% 
  group_by(date, expimp) %>% 
  summarize(hhi = sum(hhi*value/sum(value)),
            hhin = sum(hhin*value/sum(value)))


concentration_loc <- concentration_loc %>% 
  ungroup() %>% 
  filter(expimp == 'Total Trade') %>% 
  select(date, country, hhin)

concentration_glob <- concentration_glob %>% 
  filter(expimp == 'Total Trade') %>% 
  select(date, hhin)


#data <- data %>% 
#  group_by(country, expimp, partner) %>% 
#  #Seasonal adjustments
#  mutate(value = x13adj(value, 12, date))


spec1 <- macrodata %>% 
  left_join(concentration_loc,
            by = c('country', 'date')) %>% 
  drop_na() %>% 
  group_by(country) %>% 
  select(date, hhin, e, tb, ip, pi, r) %>% 
  nest()


spec2 <- macrodata %>% 
  left_join(concentration_glob,
            by = 'date') %>% 
  drop_na() %>% 
  group_by(country) %>% 
  select(date, hhin, e, tb, ip, pi, r) %>% 
  nest()

amat <- matrix(nrow = 6,
               ncol = 6, 
               data = 0)

amat[lower.tri(amat)] <- NA
diag(amat) <- 1

bmat <- matrix(nrow = 6,
               ncol = 6, 
               data = 0)
diag(bmat) <- NA

spec1 <- spec1 %>% 
  mutate(data_ts = map(data, ~.x %>% 
                         select(-date) %>% 
                         ts()),
         var = map(data_ts, ~vars::VAR(.x, p = 12, type = 'const')),
         svar = map(var, ~vars::SVAR(.x, Amat = amat, Bmat = bmat)),
         irf = map(svar, ~vars::irf(.x,
                                    impulse = 'hhin', 
                                    response = c('hhin', names(macrodata)[!str_detect(names(macrodata), or('date', 'country'))]), 
                                    n.ahead = 61, 
                                    ci = .68)),
         irf = map(irf, ~bind_rows(
           as_tibble(.x[['Upper']][[1]]) %>% 
             mutate(irf = 'ub') %>% 
             rownames_to_column() %>% 
             mutate(rowname = as.numeric(rowname) - 1 ) %>% 
             rename(t = rowname),
           as_tibble(.x[['irf']][[1]]) %>% 
             mutate(irf = 'irf') %>% 
             rownames_to_column() %>% 
             mutate(rowname = as.numeric(rowname) - 1 ) %>% 
             rename(t = rowname),
           as_tibble(.x[['Lower']][[1]]) %>% 
             mutate(irf = 'lb') %>% 
             rownames_to_column() %>% 
             mutate(rowname = as.numeric(rowname) - 1 ) %>% 
             rename(t = rowname)
         )
                     ))


spec1[7, ]$irf[[1]] %>% 
  gather(key = var, value = value, -t, -irf) %>% 
  spread(irf, value) %>% 
  ggplot(aes(x = t, y = irf, ymin = lb, ymax = ub)) +
  geom_line() +
  geom_ribbon(linetype = 0, alpha = .1) +
  facet_wrap(~var, scales = 'free')
  


spec2 <- spec2 %>% 
  mutate(data_ts = map(data, ~.x %>% 
                         select(-date) %>% 
                         ts()),
         var = map(data_ts, ~vars::VAR(.x, p = 12, type = 'const')),
         svar = map(var, ~vars::SVAR(.x, Amat = amat, Bmat = bmat)),
         irf = map(svar, ~vars::irf(.x,
                                    impulse = 'hhin', 
                                    response = c('hhin', names(macrodata)[!str_detect(names(macrodata), or('date', 'country'))]), 
                                    n.ahead = 61, 
                                    ci = .68)),
         irf = map(irf, ~bind_rows(
           as_tibble(.x[['Upper']][[1]]) %>% 
             mutate(irf = 'ub') %>% 
             rownames_to_column() %>% 
             mutate(rowname = as.numeric(rowname) - 1 ) %>% 
             rename(t = rowname),
           as_tibble(.x[['irf']][[1]]) %>% 
             mutate(irf = 'irf') %>% 
             rownames_to_column() %>% 
             mutate(rowname = as.numeric(rowname) - 1 ) %>% 
             rename(t = rowname),
           as_tibble(.x[['Lower']][[1]]) %>% 
             mutate(irf = 'lb') %>% 
             rownames_to_column() %>% 
             mutate(rowname = as.numeric(rowname) - 1 ) %>% 
             rename(t = rowname)
         )
         ))


spec2[7, ]$irf[[1]] %>% 
  gather(key = var, value = value, -t, -irf) %>% 
  spread(irf, value) %>% 
  ggplot(aes(x = t, y = irf, ymin = lb, ymax = ub)) +
  geom_line() +
  geom_ribbon(linetype = 0, alpha = .1) +
  facet_wrap(~var, scales = 'free')
