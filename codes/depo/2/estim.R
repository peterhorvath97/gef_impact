macrovars <- read_rds(file.path('data', 'macrovars_est.rds'))
specs <- read_rds(file.path('data', 'shocks_est.rds'))

estdata <- vector(mode = 'list', length = length(specs[[1]]))
names(estdata) <- names(specs[[1]])

for(i in 1:length(estdata)){
  estdata[[i]] <- vector(mode = 'list', length = length(macrovars))
  names(estdata[[i]]) <- names(macrovars)
}

q <- c('gdp', 'tb', 'ca')
m <- names(macrovars)[!(names(macrovars) %in% q)]
for(i in 1:length(specs[[1]])){
  for(j in seq_along(q)){
    suppressMessages(
      estdata[[i]][[q[j]]] <- inner_join(macrovars[[q[j]]], specs[['specs_q']][[i]])
    )
  }
}

for(i in 1:length(specs[[1]])){
  for(j in seq_along(m)){
    suppressMessages(
      estdata[[i]][[m[j]]] <- inner_join(macrovars[[m[j]]], specs[['specs_q']][[i]])
    )
  }
}



estout <- estdata
source(file.path('codes', 'estfuns.R'))

for(i in 1:length(estdata)){
  estout[[i]] <- gen_lpirfs_allvars(estout[[i]], names(estout[[i]]))
}

saveRDS(estout, file.path('data', 'estout.rds'))
estout[[3]] %>% 
  mutate(var = toupper(var),
         across(c(mean, lb, ub), ~-.x)
         ) %>% 
  ggplot(aes(x = t, y = mean, color = shock, fill = shock)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), color = 'red') +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = .2, linetype = 0) +
  facet_wrap(~var, scales = 'free') +
  theme_minimal()
