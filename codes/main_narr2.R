library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)
library(sandwich)

#rm(list = ls()[!(ls() == 'IMFenvs')])

## Load the list of datasets for the estimation ##

estdata <- read_rds(file.path('data', 'estdata.rds'))

rearrange <- function(estdata){
for(k in 1:length(estdata)){


estdata_control <- estdata[[k]]
for(i in 1:length(estdata_control)){
  estdata_control[[i]] <- estdata_control[[i]] %>%
  select(-contains('shock'), -contains('lead')) 
}

controls <- estdata_control[[1]]
for(i in 2:length(estdata_control)){
controls <- controls %>%
  left_join(estdata_control[[i]]) 
}

rem <- names(controls)[!str_detect(names(controls), or('country', 'date'))]

estdata_target <- estdata[[k]]
for(i in 1:length(estdata_target)){
  estdata_target[[i]] <- estdata_target[[i]] %>% 
    select(-contains(rem)) %>% 
    left_join(controls) 
}
estdata[[k]] <- estdata_target
}
estdata

}
  
rearrange_old <- function(estdata){
for(k in 1:length(estdata)){

estdata1 <- estdata[[k]]

estdata_varint <- list(estdata1[['ip']], estdata1[['pi']])
names(estdata_varint) <- c('ip', 'pi')

estdata_control <- list(estdata1[['e']], estdata1[['tb']], estdata1[['r']])
names(estdata_control) <- c('e', 'tb', 'r')

for(i in 1:length(estdata_control)){
  estdata_control[[i]] <- estdata_control[[i]] %>%
    select(-contains('shock'), -contains('lead'))
}

for(i in 1:length(estdata_varint)){
  for(j in 1:length(estdata_control)){
    estdata_varint[[i]] <- estdata_varint[[i]] %>% 
    left_join(estdata_control[[j]]) 
    }
}

estdata[[k]] <- estdata_varint


}
  estdata
}

estdata <- suppressMessages(rearrange(estdata))

## Functions to estimate LP IRFs ##

#Generate the Local Projections IRFS for a specific variable with a specific shock
gen_lpirf1 <- function(data, var){

controls <- data %>% 
  select(starts_with('lag'))
cont_names <- names(controls)
controls <- as.matrix(controls)

shocks <- data %>% 
  select(starts_with('shock'))
shocks_names <- names(shocks)
shocks <- as.matrix(shocks)

y <- data %>% 
  select(starts_with('lead'))
y_names <- names(y)
y <- as.matrix(y)

x <- cbind(shocks, controls)
m <- round(0.75*sqrt(nrow(x)))
h <-  names(data)[str_detect(names(data), 'lead')] %>% 
  str_extract_all(one_or_more(DGT)) %>% 
  as.matrix() %>% 
  as.numeric() %>% 
  max()
var <- var


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
gen_lpirfs_allvars <- function(datas, vars){
  out <- NULL
  for(i in 1:length(vars)){
    var <- vars[i]
    data <- datas[[i]]
    out[[i]] <- gen_lpirf1(data, var)
    
  }
  out <- bind_rows(out)
  
  out
}

#Generate the Local Projections IRFS for all variables with all specs
gen_lpirfs_allspecs <- function(estdata){
out <- NULL
for(i in 1:length(estdata)){
  vars <- names(estdata[[i]])
  datas <- estdata[[i]]
  out[[i]] <- gen_lpirfs_allvars(datas, vars)
}
names(out) <- names(estdata)

out
}

## Estimate all specs and save ##

estout <- gen_lpirfs_allspecs(estdata)

saveRDS(estout, file.path('data', 'lpirf_res.rds'))

estout <- read_rds(file.path('data', 'lpirf_res.rds'))

estout[[10]] %>% 
  filter(var %in% c('ip', 'pi')) %>% 
  mutate(var = case_when(var == 'e' ~ 'REER',
                         var == 'ip' ~ 'Output',
                         var == 'pi' ~ 'Inflation',
                         var == 'r' ~ 'Int. rate',
                         var == 'tb' ~ 'TB')) %>%  
  ggplot(aes(x = t, y = mean, ymin = lb, ymax = ub, color = shock, fill = shock)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red") +
  geom_ribbon(alpha = .1, linetype = 0) +
  facet_wrap(~var, scales = 'free', nrow = 2) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(8, 'Set1')[-c(1,3)] ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(8, 'Set1')[-c(1,3)] ) +
  labs(x = '',
       y = '')




