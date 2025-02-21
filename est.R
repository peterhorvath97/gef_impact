varQ <- dataQ %>% 
  group_by(ccode2) %>% 
  nest() %>%
  mutate(var = map(data, ~.x %>% 
                     select(R, E, TOT, P, Y) %>% 
                     ts() %>% 
                     vars::VAR(p = 4)),
         varres = map(var, ~.x %>% resid)) 

varM <- dataM %>% 
  group_by(ccode2) %>% 
  nest() %>% 
  mutate(var = map(data, ~.x %>% 
                     select(R, E, TOT, P, IP) %>% 
                     ts() %>% 
                     vars::VAR(p = 12)),
         varres = map(var, ~ .x %>% resid)) 





GTAdata %>% 
  group_by(country_aff, gta_eval, date_in) %>% 
  count() %>% 
  mutate(n = ifelse(gta_eval == 'Red', -n, n)) %>% 
  ungroup() %>% 
  group_by(country_aff, date_in) %>% 
  summarize(n = sum(n)) %>% 
  rename(country = country_aff,
         date = date_in) %>% 
  inner_join(countrycode::codelist %>% 
               select(ccode2 = iso2c,
                      country = country.name.en)) %>% 
  ungroup() %>% 
  select(-country) %>% 
  group_by(ccode2) %>% 
  nest() %>% 
  rename(z = data) %>% 
  inner_join(varM) %>% 
  print(n = 100)
