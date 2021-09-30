dists <- readRDS("temp/shooting_demos.rds")

dists <- dists %>% 
  mutate(turnout = ifelse(year == "2016", turnout - turnout_14,
                          turnout - turnout_18))

out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  full_treat <- bind_rows(
    dists %>% 
      filter(year == "2020",
             dist_pre <= threshold,
             dist_post > threshold) %>% 
      select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
             turnout_16, turnout_18, turnout_14) %>% 
      mutate(treated = T,
             d2 = as.integer(date - as.Date("2020-11-03"))),
    dists %>% 
      filter(year == "2020",
             dist_pre > threshold,
             dist_post <= threshold) %>% 
      select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
             turnout_16, turnout_18, turnout_14) %>% 
      mutate(treated = F,
             d2 = as.integer(date - as.Date("2020-11-03")))
  ) %>% 
    mutate(year = as.integer(year))
  
  
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens)), ]
  
  ########################
  
  
  mb <- ebalance(full_treat$treated,
                 select(full_treat,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens))
  
  full_treat <- bind_rows(
    filter(full_treat, treated) %>%
      mutate(weight = 1),
    filter(full_treat, !treated) %>%
      mutate(weight = mb$w)
  )
  
  ########################################
  
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens))
  
  f <- tibble(coef = l$coef,
              se = l$se, 
              pv = l$pv,
              p = threshold)
}))

out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar() + 
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Estimated Effect Size", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/different_dists_first_diff_20.rds")
##############################
################################
################################
###############################
out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  full_treat <- bind_rows(
    dists %>% 
      filter(year == "2016",
             dist_pre <= threshold,
             dist_post > threshold) %>% 
      select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
             turnout_16, turnout_18, turnout_14) %>% 
      mutate(treated = T,
             d2 = as.integer(date - as.Date("2016-11-08"))),
    dists %>% 
      filter(year == "2016",
             dist_pre > threshold,
             dist_post <= threshold) %>% 
      select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
             turnout_16, turnout_18, turnout_14) %>% 
      mutate(treated = F,
             d2 = as.integer(date - as.Date("2016-11-08")))
  ) %>% 
    mutate(year = as.integer(year))
  
  
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens)), ]
  
  ########################
  
  
  mb <- ebalance(full_treat$treated,
                 select(full_treat,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens))
  
  full_treat <- bind_rows(
    filter(full_treat, treated) %>%
      mutate(weight = 1),
    filter(full_treat, !treated) %>%
      mutate(weight = mb$w)
  )
  
  ########################################
  
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens))
  
  f <- tibble(coef = l$coef,
              se = l$se, 
              pv = l$pv,
              p = threshold)
}))

out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar() + 
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Estimated Effect Size", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/different_dists_first_diff_16.rds")
