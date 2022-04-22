dists <- readRDS("temp/shooting_demos.rds")

### loop over thresholds for RDITS
out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  
  ##keep the observations within the threshold closest to election day
  ## using data.table notation to speed this up
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
           some_college, turnout_pre, treated) %>% 
    mutate(d2 = ifelse(year == "2020", as.integer(date - as.Date("2020-11-03")),
                       as.integer(date - as.Date("2016-11-08"))),
           t16 = year == "2016")
  
  ## keep complete cases, necessary for balancing
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens, turnout_pre)), ]
  
  ########################
  
  ##balancing pre/post obs within 2016
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
  
  ##balancing pre/post obs within 2020
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
  
  ## recombine
  full_treat <- bind_rows(d16, d20)
  
  ########################################
  
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, turnout_pre, t16, some_college))
  
  f <- tibble(coef = l$coef,
              se = l$se, 
              pv = l$pv,
              p = threshold,
              n = l$N_h[1] + l$N_h[2],
              bw = l[["bws"]][1],
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
})) %>% 
  mutate(t = "6 months")


###################################################################
###################################################################
###################################################################
###################################################################
dists <- readRDS("temp/shooting_demos.rds") %>% 
  filter((date >= "2016-10-08" & date <= "2016-12-08") |
           (date >= "2020-10-03" & date <= "2020-12-03"))

### loop over thresholds for RDITS
out2 <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  
  ##keep the observations within the threshold closest to election day
  ## using data.table notation to speed this up
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
           some_college, turnout_pre, treated) %>% 
    mutate(d2 = ifelse(year == "2020", as.integer(date - as.Date("2020-11-03")),
                       as.integer(date - as.Date("2016-11-08"))),
           t16 = year == "2016")
  
  ## keep complete cases, necessary for balancing
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens, turnout_pre)), ]
  
  ########################
 
  ########################################
  
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, turnout_pre, t16, some_college))
  
  f <- tibble(coef = l$coef,
              se = l$se, 
              pv = l$pv,
              p = threshold,
              n = l$N_h[1] + l$N_h[2],
              bw = l[["bws"]][1],
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
})) %>% 
  mutate(t = "1 month")

out <- bind_rows(out, out2)

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/narrower_dataset.rds")
