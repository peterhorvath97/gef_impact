macrovars <- read_rds(file.path('data', 'macrovars_est.rds'))
specs <- read_rds(file.path('data', 'shocks_est.rds'))
dist <- read_rds(file.path('data', 'dist.rds'))

estprep <- function(specs, macrovars, freq, spec, var, dist){
  specs <- specs
  macrovars <- macrovars
  freq <- tolower(freq)
  spec <- tolower(spec)
  var <- tolower(var)
  
country_filter <- unique(macrovars[[var]]$country)
x <- specs[[paste('specs', freq, sep = '_')]][[spec]] %>% 
  filter(country_imp %in% country_filter,
         country_aff %in% country_filter)
  
x <- x %>% 
  full_join(macrovars[[var]] %>% 
               ungroup %>% 
               select(country, date, contains('lag')),
             by = c('date', 'country_imp' = 'country'))

names(x)[str_detect(names(x), var)] <- paste('imp', names(x)[str_detect(names(x), var)], sep = '_') 
names(x)

x <- x %>% 
  full_join(macrovars[[var]] %>% 
               ungroup %>% 
               select(country, date, contains('lag'), contains('lead')),
             by = c('date', 'country_aff' = 'country'))

x <- x %>% 
  left_join(dist)

x
}

estdata <- vector(mode = 'list', length = length(specs[[1]]))
names(estdata) <- names(specs[[1]])

for(i in 1:length(estdata)){
  estdata[[i]] <- vector(mode = 'list', length = length(macrovars))
  names(estdata[[i]]) <- names(macrovars)
}

spec_seq <- names(estdata)
var_seq <- names(macrovars)

q <- c('gdp', 'tb', 'ca')
m <- names(macrovars)[!(names(macrovars) %in% q)]


for(i in seq_along(spec_seq)){
  for(j in seq_along(var_seq)){
    if(var_seq[j] %in% q){
      estdata[[i]][[j]] <- estprep(specs, macrovars, 'q', spec_seq[i], var_seq[j], dist)
    } else {
      estdata[[i]][[j]] <- estprep(specs, macrovars, 'm', spec_seq[i], var_seq[j], dist)
    }
  }
}

saveRDS(estdata, 'data/estdata.rds')
