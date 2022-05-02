
## read in primary data
dists <- readRDS("temp/shooting_demos.rds")

dists <- left_join(dists,
                   readRDS("temp/trends.rds") %>% 
                     select(id = id2, pre_hits = pre, post_hits = post)) %>% 
  mutate(trend = post_hits > (2*pre_hits)) %>% 
  filter(!is.na(trend))

## loop over thresholds
out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  ##keep the observations within the threshold closest to election day
  set_pre <- dists[dists$dist <= threshold & dists$pre,
                   .SD[date %in% max(date)], by=list(year, GEOID)]
  set_pre <- set_pre[, .SD[1], by = .(year, GEOID)] %>% 
    mutate(treated = T)
  
  ##keep the observations within the threshold closest to election day
  set_post <- dists[dists$dist <= threshold & !dists$pre,
                    .SD[date %in% min(date)], by=list(year, GEOID)]
  set_post <- set_post[!(paste0(set_post$GEOID, set_post$year) %in% paste0(set_pre$GEOID, set_pre$year)),
                       .SD[1], by = .(year, GEOID)] %>% 
    mutate(treated = F)
  
  full_treat <- bind_rows(set_pre, set_post) %>% 
    select(GEOID, id, date, dist, year, turnout,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           some_college, turnout_pre, treated, trend) %>% 
    mutate(d2 = ifelse(year == "2020", as.integer(date - as.Date("2020-11-03")),
                       as.integer(date - as.Date("2016-11-08"))),
           t16 = year == "2016")
  
  
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens, turnout_pre, some_college)), ]
  
  ########################
  ## balance within 2016
  d16 <- filter(full_treat, year == "2016")
  
  mb <- ebalance(d16$treated,
                 select(d16,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, turnout_pre, some_college))
  
  d16 <- bind_rows(
    filter(d16, treated) %>%
      mutate(weight = 1),
    filter(d16, !treated) %>%
      mutate(weight = mb$w)
  )
  
  # balance within 2020
  d20 <- filter(full_treat, year == "2020")
  
  mb <- ebalance(d20$treated,
                 select(d20,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, turnout_pre, some_college))
  
  d20 <- bind_rows(
    filter(d20, treated) %>%
      mutate(weight = 1),
    filter(d20, !treated) %>%
      mutate(weight = mb$w)
  )
  
  full_treat <- bind_rows(d16, d20)
   ## keep only the trending observations
  trend_obs <- filter(full_treat, trend)
  l10 <- rdrobust(y = trend_obs$turnout, x = trend_obs$d2, p = 1, c = 0, cluster = trend_obs$id,
                  weights = trend_obs$weight,
                  covs = select(trend_obs,
                                latino, nh_white, asian,
                                nh_black, median_income, median_age,
                                pop_dens, turnout_pre, some_college))
  ##keep only the nontrending observations
  non_trend_obs <- filter(full_treat, !trend)
  l11 <- rdrobust(y = non_trend_obs$turnout, x = non_trend_obs$d2, p = 1, c = 0, cluster = non_trend_obs$id,
                  weights = non_trend_obs$weight,
                  covs = select(non_trend_obs,
                                latino, nh_white, asian,
                                nh_black, median_income, median_age,
                                pop_dens, turnout_pre, some_college))
  
  ## combine results of all these models
  f <- bind_rows(
    tibble(coef = l10$coef,
           se = l10$se, 
           pv = l10$pv,
           p = threshold,
           t = "Trending Killings",
           u = l10[["ci"]][,2],
           l = l10[["ci"]][,1]),
    tibble(coef = l11$coef,
           se = l11$se, 
           pv = l11$pv,
           p = threshold,
           t = "Non-Trending Killings",
           u = l11[["ci"]][,2],
           l = l11[["ci"]][,1])
  )
}))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
# out$t <- factor(out$t, levels = c('Double Bandwidth','30 Days','60 Days'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists