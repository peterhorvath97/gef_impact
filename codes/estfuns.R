#Generate the Local Projections IRFS for a specific variable with a specific shock
gen_lpirf1 <- function(data, var){
  
  require(tidyverse)
  require(stringr)
  require(lubridate)
  require(rebus)
  require(sandwich)
  
  var <- var
  
  x <- data[[var]] %>% 
    ungroup() %>% 
    select(contains('shock') | contains('lag'))
  x_names <- names(x)
  shocks_names <- x_names[!str_detect(x_names, 'lag')]
  x <- as.matrix(x)
  
  y <- data[[var]] %>% 
    ungroup() %>% 
    select(contains('lead'))
  y_names <- names(y)
  y <- as.matrix(y)
  
  
  m <- round(0.75*sqrt(nrow(x)))
  h <-  names(data[[var]])[str_detect(names(data[[var]]), 'lead')] %>% 
    str_extract_all(one_or_more(DGT)) %>% 
    as.matrix() %>% 
    as.numeric() %>% 
    max()
  
  
  
  
  #Estimate the IRF for one variable
  out <- NULL
  for(i in 1:h){  
    model <- lm(y[,i] ~ -1 + x)
    irf <- model$coefficients[!str_detect(names(model$coefficients), or('lag', 'Intercept'))]
    names(irf) <- shocks_names
    nw <- NeweyWest(model, 
                    lag = m - 1, 
                    prewhite = F, 
                    adjust = T)
    se <- sqrt(diag(nw))[1:(ncol(x))]
    se <- se[!str_detect(names(se), or('lag', 'Intercept'))]
    names(se) <- shocks_names
    irf_ub <- irf + se
    names(irf_ub) <- paste(names(irf_ub), '_ub', sep = '')
    irf_lb <- irf - se
    names(irf_lb) <- paste(names(irf_lb), '_lb', sep = '')
    out[[i]] <- as_tibble(t(c(irf, irf_ub, irf_lb))) %>% 
      gather(key = key,
             value = value) %>% 
      mutate(key2 = case_when(str_detect(key, 'ub') ~ 'ub',
                              str_detect(key, 'lb') ~ 'lb',
                              TRUE ~ 'mean'),
             key = str_remove_all(key, key2) %>% 
               str_remove_all('_' %R% END)) %>% 
      spread(key2, value) %>% 
      rename(shock = key) %>% 
      mutate(var = var) %>% 
      mutate(t = i)
  }
  
  out <- bind_rows(out) %>% 
    mutate(t = t - 1) 
  
  out
}

#Generate the Local Projections IRFS for all variables with a specific shocks
gen_lpirfs_allvars <- function(data, vars){
  out <- NULL
  for(i in 1:length(vars)){
    var <- vars[i]
    out[[i]] <- gen_lpirf1(data, var)
    
  }
  out <- bind_rows(out)
  
  out
}