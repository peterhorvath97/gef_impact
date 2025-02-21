estdata <- read_rds('data/estdata.rds')

var <- NULL
data <- NULL
lpirfs <- NULL
out <- vector(mode = 'list', length = length(estdata[[1]]))
estout <- vector(mode = 'list', length = length(estdata))


source(file.path('codes', 'estfuns.R'))

for(i in 1:length(estdata)){
  print(paste('Calculating LP IRF-s for specification:', toupper(names(estdata)[i]), sep = ' '))
  data <- estdata[[i]]
  for(j in 1:length(data)){
    var <- names(data)[[j]] 
    lpirfs <- gen_lpirf1(data, var)
    out[[j]] <- lpirfs
  }
  estout[[i]] <- out
}

names(estout) <- names(estdata)

for(i in 1:length(estout)){
  names(estout[[i]]) <- names(estdata[[i]])
}

saveRDS(estout, file.path('data', 'estout.rds'))

for(i in 1:length(estdata)){
  for(j in 1:length(estdata[[i]])){
  estdata[[i]][[j]] <- estdata[[i]][[j]] %>% 
      ungroup() %>% 
      group_by(country_aff) %>%
      nest() 
}}

estout <- estdata

for(i in 1:length(estdata)){
  for(j in 1:length(estdata[[i]])){
    estout[[i]][[j]] <- estdata[[i]][[j]] %>%
      mutate(lpirf = map(data, ~tryCatch({gen_lpirf2(.x, var = names(estdata[[i]])[j])},
                                         error = function(error){NA}) ))
}}


for(i in 1:length(estout)){
  for(j in 1:length(estout[[i]])){
    estout[[i]][[j]] <- estout[[i]][[j]] %>% 
      mutate(x = map(lpirf, ~class(.x)), 
             x = unlist(x)[1]) %>% 
      filter(x != 'logical') %>% 
      select(-x, -data) %>% 
      unnest(lpirf)
  }
}


saveRDS(estout, 'data/estout_heterogen.rds')


estout[[1]] %>%
  bind_rows() %>% 
  mutate(var = toupper(var),
         #across(c(mean, lb, ub), ~-.x)
         ) %>% 
  ggplot(aes(x = t, y = mean, color = shock, fill = shock)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), color = 'red') +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = .2, linetype = 0) +
  facet_wrap(~var, scales = 'free') +
  theme_minimal()
