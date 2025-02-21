caps <- maps::world.cities %>% 
  as_tibble() %>% 
  mutate(capital = ifelse(str_detect(name, 'Podgorica'), 1, capital)) %>% 
  filter(capital == 1) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  select(country = country.etc, geometry) %>% 
  distinct(country, .keep_all = T) %>% 
  mutate(country = ifelse(country == 'Czech Republic', 'Czechia', country),
         country = ifelse(country == 'UK', 'United Kingdom', country),
         country = ifelse(country == 'USA', 'United States', country)) 


gta_shocks <- read_rds(file.path('data', 'gta_shocks.rds')) %>% 
  filter(country_imp %in% countries,
         country_aff %in% countries)

cpair <- gta_shocks %>% 
  select(country_imp, country_aff) %>% 
  distinct() 

coords <- as_tibble(countries) %>%
  rename(country = value) %>% 
  left_join(caps) 

dist <- st_distance(coords$geometry)
colnames(dist) <- countries


dist <- bind_cols(coords, dist) %>%
  select(-geometry) %>% 
  gather(key = key, 
         value = value,
         -country) %>% 
  arrange(country, key) %>% 
  rename(country_imp = country,
          country_aff = key,
         dist = value)

dist <- cpair %>% 
  inner_join(dist, by = c('country_imp', 'country_aff')) %>% 
  mutate(dist = dist/1000)

saveRDS(dist, 'data/dist.rds')
rm(caps, gta_shocks, cpair, coords, dist)
