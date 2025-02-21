library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)


#DB-s
macrovars <- read_rds(file.path('data', 'macrovars.rds')) 
shocks <- read_rds(file.path('data', 'gta_shocks.rds')) 

#Define which vars are on quarterly frequency for LP LHS and RHS generation later in the code
q <- c('gdp', 'tb', 'ca')
m <- names(macrovars)[!(names(macrovars) %in% q)]


#Generate Controls and Targets for the LP 
suppressMessages(
for(i in seq_along(q)){
  macrovars[[q[i]]] <- macrovars[[q[i]]] %>% 
    gather(key = var, value = value, -country, -date) %>% 
    group_by(var, country) %>%  
    #Remove Country FE
    mutate(value = value - mean(value)) %>%
    #Create RHS
    mutate(map_dfc(seq(4), ~ lag(value, n = .x)) %>%
             set_names(paste('lag', seq(4),sep = ''))) %>% 
    #Create LHS
    mutate(map_dfc(seq(21), ~ lead(value, n = .x)) %>%
             set_names(paste('lead', seq(21),sep = ''))) %>% 
    #Remove unnecessary columns  
    select(-value, -var)
}
)

suppressMessages(
for(i in seq_along(m)){
  macrovars[[m[i]]] <- macrovars[[m[i]]] %>% 
    gather(key = var, value = value, -country, -date) %>% 
    group_by(var, country) %>%  
    #Remove Country FE
    mutate(value = value - mean(value)) %>%
    #Create RHS
    mutate(map_dfc(seq(12), ~ lag(value, n = .x)) %>%
             set_names(paste('lag', seq(12),sep = ''))) %>% 
    #Create LHS
    mutate(map_dfc(seq(61), ~ lead(value, n = .x)) %>%
             set_names(paste('lead', seq(61),sep = ''))) %>% 
    select(-value, -var)
}
)


shocks %>% 
  group_by(date_in, gta_eval) %>% 
  count() %>% 
  mutate(n = n*ifelse(gta_eval == 'Red', -1, 1)) %>% 
  ungroup(gta_eval) %>% 
  summarize(shock = sum(n)) %>%
  mutate(map_dfc(seq(12), ~ lag(shock, n = .x)) %>%
           set_names(paste('shock_l', seq(12),sep = '')))
  

### Functions ###

# Create the function to get modes where non-weighted shocks need to be aggregated
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Create the target variables at horizon h
genlhs <- function(data, var, h){
  
  require(tidyverse)
  
  suppressMessages(
    data <- data %>% 
      group_by(country) %>% 
      select(date, sym(var)) %>% 
      mutate(
        #h lead of lhs values
        map_dfc(seq(h), ~ lead(!!sym(var), n = .x)) %>%
          set_names(paste('lead',seq(h),'_', var, sep = ''))
        
      ) %>% 
      ungroup() %>% 
      select(country, date, starts_with('lead'))
  )
  
  data
}

#Create the control variables at lags up to k
genrhs_macro <- function(data, var, k){
  
  require(tidyverse)
  
  suppressMessages(
    data <- data %>% 
      group_by(country) %>% 
      select(date, sym(var)) %>% 
      mutate(
        #k lags of control
        map_dfc(seq(k), ~ lag(!!sym(var), n = .x)) %>%
          set_names(paste('lag',seq(k),'_', var, sep = ''))
      ) %>% 
      ungroup() %>% 
      select(country, date, starts_with('lag'))
  )
  
  data
}

#Create the shock variables 
genrhs_shock <- function(shocks, var, k){
  require(tidyverse)
  
  suppressMessages(
    data <- shocks %>% 
      group_by_at(vars(-contains('shock'), -contains('lead'), -contains('lag'), -date)) %>% 
      mutate(
        #k lags of control
        map_dfc(seq(k), ~ lag(!!sym(var), n = .x)) %>%
          set_names(paste('lag',seq(k),'_', var, sep = ''))
      ) %>% 
      ungroup() 
  )
  
  data
}

#Add all shock variations
add_allshocks <- function(shocks){
  
  require(tidyverse)
  
  shocks_all <- NULL  
  shocks_funs <- NULL
  
  #Binary type shocks - global
  
  shocks_all[['base']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date) %>% 
    summarize(shock = getmode(shock_base)) #%>% 
  shocks_funs[['base']] <- function(shocks, k) genrhs_shock(shocks, 'shock', k)
  
  shocks_all[['sign_asym']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date, gta_eval) %>% 
    summarize(shock = getmode(shock_base)) %>% 
    spread(gta_eval, shock) %>% 
    rename(shock_pos = Green,
           shock_neg = Red) %>% 
    mutate(across(starts_with('shock'), ~abs(.x)))#%>% 
  shocks_funs[['sign_asym']] <- function(shocks, k) genrhs_shock(shocks, 'shock_neg', k) %>% genrhs_shock('shock_pos', k)
  
  shocks_all[['perm_vs_trans']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date) %>% 
    summarize(shock_trans = getmode(shock_trans),
              shock_perm = getmode(shock_perm)) #%>% 
  shocks_funs[['perm_vs_trans']] <- function(shocks, k) genrhs_shock(shocks, 'shock_trans', k) %>% genrhs_shock('shock_perm', k)
  
  
  #Weighted shocks - global
  
  shocks_all[['base_w']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date) %>% 
    summarize(shock = sum(shock_base)) #%>% 
  shocks_funs[['base_w']] <- function(shocks, k) genrhs_shock(shocks, 'shock', k)
  
  shocks_all[['sign_asym_w']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date, gta_eval) %>% 
    summarize(shock = sum(shock_base)) %>% 
    spread(gta_eval, shock) %>% 
    rename(shock_pos = Green,
           shock_neg = Red) %>% 
    mutate(across(starts_with('shock'), ~abs(.x)))#%>%
  shocks_funs[['sign_asym_w']] <- function(shocks, k) genrhs_shock(shocks, 'shock_neg', k) %>% genrhs_shock('shock_pos', k)
  
  shocks_all[['perm_vs_trans_w']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date) %>% 
    summarize(shock_trans = sum(shock_trans),
              shock_perm = sum(shock_perm)) #%>% 
  shocks_funs[['perm_vs_trans_w']] <- function(shocks, k) genrhs_shock(shocks, 'shock_trans', k) %>% genrhs_shock('shock_perm', k)
  
  
  #Weighted shocks - imposer country
  
  shocks_all[['base_imp']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date, country_imp) %>% 
    summarize(shock = sum(shock_base)) %>%
    rename(country = country_imp)
  shocks_funs[['base_imp']] <- function(shocks, k) genrhs_shock(shocks, 'shock', k)
  
  shocks_all[['sign_asym_imp']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date, gta_eval, country_imp) %>% 
    summarize(shock = sum(shock_base)) %>% 
    spread(gta_eval, shock) %>% 
    rename(shock_pos = Green,
           shock_neg = Red)  %>%
    rename(country = country_imp) %>% 
    mutate(across(starts_with('shock'), ~abs(.x)))#%>%
  shocks_funs[['sign_asym_imp']] <- function(shocks, k) genrhs_shock(shocks, 'shock_neg', k) %>% genrhs_shock('shock_pos', k)
  
  shocks_all[['perm_vs_trans_imp']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date, country_imp) %>% 
    summarize(shock_trans = sum(shock_trans),
              shock_perm = sum(shock_perm))  %>%
    rename(country = country_imp)
  shocks_funs[['perm_vs_trans_imp']] <- function(shocks, k) genrhs_shock(shocks, 'shock_trans', k) %>% genrhs_shock('shock_perm', k) 
  
  
  #Weighted shocks - affected country
  
  shocks_all[['base_aff']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date, country_aff) %>% 
    summarize(shock = sum(shock_base))  %>%
    rename(country = country_aff)
  shocks_funs[['base_aff']] <- function(shocks, k) genrhs_shock(shocks, 'shock', k)
  
  shocks_all[['sign_asym_aff']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date, gta_eval, country_aff) %>% 
    summarize(shock = sum(shock_base)) %>% 
    spread(gta_eval, shock) %>% 
    rename(shock_pos = Green,
           shock_neg = Red) %>%
    rename(country = country_aff) %>% 
    mutate(across(starts_with('shock'), ~abs(.x)))#%>%
  shocks_funs[['sign_asym_aff']] <- function(shocks, k) genrhs_shock(shocks, 'shock_neg', k) %>% genrhs_shock('shock_pos', k)
  
  shocks_all[['perm_vs_trans_aff']] <- shocks %>% 
    filter(db == 'main') %>% 
    group_by(date, country_aff) %>% 
    summarize(shock_trans = sum(shock_trans),
              shock_perm = sum(shock_perm)) %>%
    rename(country = country_aff) 
  shocks_funs[['perm_vs_trans_aff']] <- function(shocks, k) genrhs_shock(shocks, 'shock_trans', k) %>% genrhs_shock('shock_perm', k) 
  
  list(shocks_all, shocks_funs)
  
}

#Combine lhs and rhs macro variables
genvars <- function(data, k, h){
  
  vars <- names(data)[!str_detect(names(data), or('country', 'date'))] 
  
  dlist <- NULL
  for(i in 1:length(vars)){
    lhs <- genlhs(data = data,
                  var = vars[i],
                  h = h)
    
    rhs <- genrhs_macro(data = data,
                        var = vars[i],
                        k = k)
    suppressMessages(joined <- left_join(rhs, lhs))
    dlist[[i]] <- joined
  }
  
  names(dlist) <- vars
  
  dlist
}

#Combine lhs and rhs macro variables
genvars2 <- function(data, shocks, k, h){
  
  macrodata <- data
  vars <- names(macrodata)[!str_detect(names(macrodata), or('country', 'date'))] 
  suppressMessages(shocks_all <- add_allshocks(shocks) )
  k <- k
  h <- h
  
  
  macrovars <- genvars(macrodata,
                       k,
                       h)
  
  temp <- NULL
  out <- NULL
  for(j in 1:length(shocks_all[[1]])){
    genfun <- shocks_all[[2]][[j]]
    for(i in 1:length(macrovars)){
      temp[[i]] <- macrovars[[i]] %>% left_join(shocks_all[[1]][[j]]) %>% 
        mutate(across(starts_with('shock'), ~replace_na(.x, 0))) %>% 
        genfun(k = k)
    }
    out[[j]] <- temp
    names(out[[j]]) <- vars
  }
  
  names(out) <- names(shocks_all[[1]])
  
  out
  
}


estdata <- genvars2(macrodata,
                    shocks,
                    12,
                    61)

saveRDS(estdata, file.path('data', 'estdata.rds'))