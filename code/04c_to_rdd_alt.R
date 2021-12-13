
dists <- readRDS("temp/shooting_demos.rds") %>% 
  mutate(turnout_pre = ifelse(year == "2016", turnout_14, turnout_18))

dists <- left_join(dists, readRDS("temp/geocoded_shootings.rds") %>% 
                     ungroup() %>% 
                     mutate(id = row_number(),
                            score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
                     filter(score > 95) %>% 
                     select(id_pre = id, race_pre = race))

dists <- left_join(dists, readRDS("temp/geocoded_shootings.rds") %>% 
                     ungroup() %>% 
                     mutate(id = row_number(),
                            score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
                     filter(score > 95) %>% 
                     select(id_post = id, race_post = race)) %>% 
  mutate(across(starts_with("race"), ~substring(., 1, 1)),
         across(starts_with("race"), ~ifelse(. == "", "U",
                                             ifelse(. %in% c("A", "N", "P"), "O", .))))

out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  t2 <- 0
  full_treat <- bind_rows(
    dists %>% 
      filter(year == "2020",
             dist_pre <= threshold,
             dist_pre > t2,
             dist_post > threshold) %>% 
      select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
             turnout_16, turnout_18, turnout_14, turnout_pre, new) %>% 
      mutate(treated = T,
             d2 = as.integer(date - as.Date("2020-11-03"))),
    dists %>% 
      filter(year == "2020",
             dist_pre > threshold,
             dist_post <= threshold) %>% 
      select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
             turnout_16, turnout_18, turnout_14, turnout_pre, new) %>% 
      mutate(treated = F,
             d2 = as.integer(date - as.Date("2020-11-03"))),
    dists %>% 
      filter(year == "2016",
             dist_pre <= threshold,
             dist_pre > t2,
             dist_post > threshold) %>% 
      select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
             turnout_16, turnout_18, turnout_14, turnout_pre, new) %>% 
      mutate(treated = T,
             d2 = as.integer(date - as.Date("2016-11-08"))),
    dists %>% 
      filter(year == "2016",
             dist_pre > threshold,
             dist_post <= threshold) %>% 
      select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
             turnout_16, turnout_18, turnout_14, turnout_pre, new) %>% 
      mutate(treated = F,
             d2 = as.integer(date - as.Date("2016-11-08")))
  ) %>% 
    mutate(year = as.integer(year))
  
  
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens, turnout_pre)), ]
  
  ########################
  d16 <- filter(full_treat, year == 1)
  
  mb <- ebalance(d16$treated,
                 select(d16,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, turnout_pre))
  
  d16 <- bind_rows(
    filter(d16, treated) %>%
      mutate(weight = 1),
    filter(d16, !treated) %>%
      mutate(weight = mb$w)
  )
  
  d20 <- filter(full_treat, year == 3)
  
  mb <- ebalance(d20$treated,
                 select(d20,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, turnout_pre))
  
  d20 <- bind_rows(
    filter(d20, treated) %>%
      mutate(weight = 1),
    filter(d20, !treated) %>%
      mutate(weight = mb$w)
  )
  
  full_treat <- bind_rows(d16, d20)
  ########################################
  
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, turnout_pre))
  
  l2 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight)
  
  l3 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id)
  
  f <- bind_rows(
    tibble(coef = l$coef,
           se = l$se, 
           pv = l$pv,
           p = threshold,
           t = "OLS"),
    tibble(coef = l2$coef,
           se = l2$se, 
           pv = l2$pv,
           p = threshold,
           t = "Entropy Balancing"),
    tibble(coef = l3$coef,
           se = l3$se, 
           pv = l3$pv,
           p = threshold,
           t = "No Adjustment")
  )
}))

out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out$t <- factor(out$t, levels = c('Entropy Balancing','OLS','No Adjustment'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/alt_proc_rdd.rds")
