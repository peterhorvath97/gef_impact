library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)
library(sandwich)

macrovars <- read_rds(file.path('data', 'macrovars_est.rds'))
shocks <- read_rds(file.path('data', 'gta_shocks.rds')) 


q <- c('gdp', 'tb', 'ca')
m <- names(macrovars)[!(names(macrovars) %in% q)]

suppressMessages(
shock_m <- shocks %>% 
  group_by(date_in, gta_eval) %>%
  count() %>% 
  mutate(n = ifelse(gta_eval == 'Red', -n, n)) %>% 
  ungroup(gta_eval) %>% 
  summarize(shock = sum(n)) %>% 
  rename(date = date_in) %>%
  mutate(map_dfc(seq(12), ~ lag(shock, n = .x)) %>%
           set_names(paste('shock_lag', seq(12),sep = '')))
)


suppressMessages(
  shock_q <- shocks %>% 
    group_by(date_in, gta_eval) %>%
    count() %>% 
    mutate(n = ifelse(gta_eval == 'Red', -n, n)) %>% 
    ungroup(gta_eval) %>% 
    summarize(shock = sum(n)) %>% 
    rename(date = date_in) %>% 
    mutate(y = year(date),
           q = quarter(date)) %>% 
    group_by(y, q) %>% 
    mutate(shock = sum(shock)) %>% 
    filter(q == 1 & month(date) == 1 | q == 2 & month(date) == 4 | q == 3 & month(date) == 7 | q == 4 & month(date) == 10) %>% 
    ungroup() %>% 
    select(-y, -q) %>% 
    mutate(map_dfc(seq(4), ~ lag(shock, n = .x)) %>%
             set_names(paste('shock_lag', seq(4),sep = '')))
)

suppressMessages(
for(i in seq_along(m)){
  macrovars[[m[i]]] <- macrovars[[m[i]]] %>% 
    inner_join(shock_m)
}
)

suppressMessages(
  for(i in seq_along(q)){
    macrovars[[q[i]]] <- macrovars[[q[i]]] %>% 
      inner_join(shock_q)
  }
)
















valami <- gen_lpirfs_allvars(data = macrovars, vars = names(macrovars))
  
valami %>% 
  ggplot(aes(x = t, y = mean, ymin = lb, ymax = ub, fill = shock, color = shock)) +
  geom_line() +
  geom_hline(yintercept = 0, color = 'red') +
  geom_ribbon(alpha = .1, linetype = 0) +
  facet_wrap(~var, scales = 'free') + 
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(8, 'Set1')[-c(1,3)] ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(8, 'Set1')[-c(1,3)] ) +
  labs(x = '',
       y = '')




