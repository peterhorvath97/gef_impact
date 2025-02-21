library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)
library(sandwich)

#rm(list = ls()[!(ls() == 'IMFenvs')])

#Load data
macrodata <- read_rds(file.path('data', 'macrodata.rds'))
shocks <- read_rds(file.path('data', 'gta_shocks.rds'))

#Annualized growth and inflation rates
macrodata <- macrodata %>% 
  arrange(country, date) %>% 
  group_by(country) %>% 
  mutate(ip = ip - lag(ip, n = 12),
         pi = pi - lag(pi, n = 12)) %>% 
  drop_na() %>% 
  ungroup()

#Set range
start <- max(
  min(shocks$date),
  min(macrodata$date)
)
end <- min(
  max(shocks$date),
  max(macrodata$date)
)

macrodata <- macrodata %>% 
  filter(date >= start,
         date <= end)

shocks <- shocks %>% 
  filter(date >= start,
         date <= end)


#Country FE - demean variables 
macrodata <- macrodata %>% 
  group_by(country) %>% 
  mutate(across(-date, ~.x - mean(.x))) 


# Create the function to get modes where needed
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Basic shock globally - was there a trade shock at a given time t
data <- macrodata %>% 
  full_join(
    shocks %>% 
      filter(db == 'main') %>% 
      group_by(date) %>% 
      summarize(shock = getmode(shock_base)),
    by = 'date') %>% 
  mutate(shock = replace_na(shock, 0),
         shock = - shock) %>% 
  arrange(country, date)


#Create the target variables and controls
genvars <- function(data, var, k, h){
  
  require(tidyverse)
  suppressMessages(
  data <- data %>% 
    group_by(country) %>% 
    select(date, sym(var), shock) %>% 
    mutate(
      #k lags of control
      map_dfc(seq(k), ~ lag(!!sym(var), n = .x)) %>%
        set_names(paste('lag',seq(k),'_', var, sep = '')),
      #h lead of lhs values
      map_dfc(seq(h), ~ lead(!!sym(var), n = .x)) %>%
        set_names(paste('lead',seq(h),'_', var, sep = ''))
      
    ) %>% 
    ungroup() %>% 
    select(contains('shock'), starts_with('lag'), starts_with('lead'))
  )
  
  data
}


gen_lpirf <- function(data, var, k, h){
  
var <- genvars(data = data,
              var = var, 
              k = k, 
              h = h)

#numshock <- sum(str_detect(names(var), 'shock'))

x <- as.matrix(var[1:(k+1)])
m <- round(0.75*sqrt(nrow(x)))
out <- NULL
for(i in (k+2):(h+k+1)){
y <- as.matrix(var[i])  
model <- lm(y ~ -1 + x)
irf <- model$coefficients[!str_detect(names(model$coefficients), or('lag', 'Intercept'))]
nw <- NeweyWest(model, 
                lag = m - 1, 
                prewhite = F, 
                adjust = T)
se <- sqrt(diag(nw))[1:(ncol(x))] 
irf_up <- irf + se[!str_detect(names(se), or('lag', 'Intercept'))]
irf_dn <- irf - se[!str_detect(names(se), or('lag', 'Intercept'))]
out[[i]] <- as_tibble( t(c(irf, irf_dn, irf_up)) )
names(out[[i]]) <- c('irf', 'irf_lb', 'irf_ub')
}

out <- bind_rows(out) %>% 
  rownames_to_column() %>% 
  rename(t = rowname) %>% 
  mutate(t = as.numeric(t) - 1) 

}

vars <- names(data)[!str_detect(names(data), or('country', 'date', 'shock'))]
irfs <- NULL
for(i in 1:length(vars)){
  irfs[[i]] <- gen_lpirf(data = data,
                         var = vars[[i]],
                         k = 12,
                         h = 61) %>% 
    mutate(var = vars[[i]])
}




bind_rows(irfs) %>% 
  #mutate(across(c(-t, -var), ~-.x)) %>% 
  ggplot(aes(x = t, y = irf, ymin = irf_lb, ymax = irf_ub)) +
  geom_line() +
  geom_ribbon(alpha = .2, fill = 'blue') +
  facet_wrap(~var, scales = 'free')


