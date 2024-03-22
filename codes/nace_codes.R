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


gta_nace <- master %>% select(sectors) %>% 
  separate_rows(sectors, sep = ', ') %>% 
  distinct(sectors)

nace_code <- read_csv("https://gist.githubusercontent.com/b-rodrigues/4218d6daa8275acce80ebef6377953fe/raw/99bb5bc547670f38569c2990d2acada65bb744b3/nace_rev2.csv")


nace_code %>% 
  filter(Level == 3) %>% 
  select(Code, Description) %>% 
  mutate(Code = str_remove_all(Code, DOT)) %>% 
  filter(Code %in% gta_nace$sectors) %>% 
  rename(nace3 = Code,
         desc3 = Description) %>% 
  mutate(nace2 = substr(nace3, 1,2)) %>% 
  left_join(nace_code %>% 
              select(Code, Parent) %>% 
              filter(!str_detect(Parent, DGT)) %>% 
              rename(nace2 = Code,
                     nace1 = Parent)) %>% 
  select(-nace2) %>% 
  left_join(nace_code %>% 
              filter(Level == 1) %>%
              select(nace1 = Code, desc1 = Description))
  
