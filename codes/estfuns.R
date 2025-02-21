#Generate the Local Projections IRFS for a specific variable with a specific shock
gen_lpirf1 <- function(data, var){
  
  require(tidyverse)
  require(stringr)
  require(lubridate)
  require(rebus)
  require(sandwich)
  
  
  x <- data[[var]] %>% 
    #drop_na() %>% 
    ungroup() %>% 
    select(contains('shock') | contains('lag') | contains('dist'))
  x_names <- names(x)
  shocks_names <- x_names[!str_detect(x_names, or('lag', 'dist'))]
  x <- as.matrix(x)
  
  y <- data[[var]] %>% 
    #drop_na() %>% 
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
  print(paste('Calculating LP IRF-s for variable:', toupper(var), sep  = ' '))
  pb = txtProgressBar(min = 0, max = h, initial = 0) 
  for(i in 1:h){
    
    setTxtProgressBar(pb,i)
    
    model <- lm(y[,i] ~ -1 + x)
    irf <- model$coefficients[!str_detect(names(model$coefficients), or('lag', 'Intercept', 'dist'))]
    names(irf) <- shocks_names
    sum <- summary(model)
    se <- sum$coefficients
    se <- se[rownames(se)[!str_detect(rownames(se),or('lag', 'Intercept', 'dist'))], 
             colnames(se)[colnames(se) == 'Std. Error']]
     
    #nw <- NeweyWest(model, 
    #                lag = m - 1, 
    #                prewhite = F, 
    #                adjust = T)
    #se <- sqrt(diag(nw))[1:(ncol(x))]
    #se <- se[!str_detect(names(se), or('lag', 'Intercept'))]
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
  close(pb)
  out <- bind_rows(out) %>% 
    mutate(t = t - 1) 
  
  gc()
  
  out
}

gen_lpirf2 <- function(data, var){
  
  require(tidyverse)
  require(stringr)
  require(lubridate)
  require(rebus)
  require(sandwich)
  
  
  x <- data %>% 
    #drop_na() %>% 
    ungroup() %>% 
    select(contains('shock') | contains('lag') | contains('dist'))
  x_names <- names(x)
  shocks_names <- x_names[!str_detect(x_names, or('lag', 'dist'))]
  x <- as.matrix(x)
  
  y <- data %>% 
    #drop_na() %>% 
    ungroup() %>% 
    select(contains('lead'))
  y_names <- names(y)
  y <- as.matrix(y)
  
  
  m <- round(0.75*sqrt(nrow(x)))
  h <-  names(data)[str_detect(names(data), 'lead')] %>% 
    str_extract_all(one_or_more(DGT)) %>% 
    as.matrix() %>% 
    as.numeric() %>% 
    max()
  
  
  
  #Estimate the IRF for one variable
  out <- NULL
  print(paste('Calculating LP IRF-s for variable:', toupper(var), sep  = ' '))
  pb = txtProgressBar(min = 0, max = h, initial = 0) 
  for(i in 1:h){
    
    setTxtProgressBar(pb,i)
    
    model <- lm(y[,i] ~ -1 + x)
    irf <- model$coefficients[!str_detect(names(model$coefficients), or('lag', 'Intercept', 'dist'))]
    names(irf) <- shocks_names
    sum <- summary(model)
    se <- sum$coefficients
    se <- se[rownames(se)[!str_detect(rownames(se),or('lag', 'Intercept', 'dist'))], 
             colnames(se)[colnames(se) == 'Std. Error']]
    
    #nw <- NeweyWest(model, 
    #                lag = m - 1, 
    #                prewhite = F, 
    #                adjust = T)
    #se <- sqrt(diag(nw))[1:(ncol(x))]
    #se <- se[!str_detect(names(se), or('lag', 'Intercept'))]
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
  close(pb)
  out <- bind_rows(out) %>% 
    mutate(t = t - 1) 
  
  gc()
  
  out
}

