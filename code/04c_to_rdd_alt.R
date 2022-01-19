
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
    mutate(year = as.integer(year),
           t16 = year == 1)
  
  
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
  d16 <- filter(full_treat, year == 1)
  
  mb <- ebalance(d16$treated,
                 select(d16,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens))
  
  d16 <- bind_rows(
    filter(d16, treated) %>%
      mutate(weight2 = 1),
    filter(d16, !treated) %>%
      mutate(weight2 = mb$w)
  )
  
  d20 <- filter(full_treat, year == 3)
  
  mb <- ebalance(d20$treated,
                 select(d20,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens))
  
  d20 <- bind_rows(
    filter(d20, treated) %>%
      mutate(weight2 = 1),
    filter(d20, !treated) %>%
      mutate(weight2 = mb$w)
  )
  
  full_treat2 <- bind_rows(d16, d20) %>% 
    select(GEOID, year, weight2)
  
  full_treat <- left_join(full_treat, full_treat2) %>% 
    mutate(change_to = turnout - turnout_pre)
  ########################################
  
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, turnout_pre, t16))
  
  l2 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight)
  
  l3 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id)
  
  l4 <- rdrobust(y = full_treat$change_to, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight2,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, t16))
  
  bw <- rdbwselect(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                   weights = full_treat$weight,
                   covs = select(full_treat,
                                 latino, nh_white, asian,
                                 nh_black, median_income, median_age,
                                 pop_dens, turnout_pre, t16))
  
  l5 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16), h = bw[["bws"]][1]*2)
  
  l6 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16), h = c(bw[["bws"]][1], 30))
  
  l7 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16), h = c(bw[["bws"]][1], 60))
  
  o16 <- filter(full_treat, year == 1)
  
  l8 <- rdrobust(y = o16$turnout, x = o16$d2, p = 1, c = 0, cluster = o16$id,
                 weights = o16$weight,
                 covs = select(o16,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre))
  
  o20 <- filter(full_treat, year != 1)
  l9 <- rdrobust(y = o20$turnout, x = o20$d2, p = 1, c = 0, cluster = o20$id,
                 weights = o20$weight,
                 covs = select(o20,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre))
  
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
           t = "No Adjustment"),
    tibble(coef = l4$coef,
           se = l4$se, 
           pv = l4$pv,
           p = threshold,
           t = "First Difference in Turnout"),
    tibble(coef = l5$coef,
           se = l5$se, 
           pv = l5$pv,
           p = threshold,
           t = "Double Bandwidth"),
    tibble(coef = l6$coef,
           se = l6$se, 
           pv = l6$pv,
           p = threshold,
           t = "Nonpara, 30"),
    tibble(coef = l7$coef,
           se = l7$se, 
           pv = l7$pv,
           p = threshold,
           t = "Nonpara, 60"),
    tibble(coef = l8$coef,
           se = l8$se, 
           pv = l8$pv,
           p = threshold,
           t = "Only 2016"),
    tibble(coef = l9$coef,
           se = l9$se, 
           pv = l9$pv,
           p = threshold,
           t = "Only 2020",
           u = l[["ci"]][,2],
           l = l[["ci"]][,1])
  )
}))

saveRDS(out, "temp/alt_rdds2.rds")
out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t %in% c('Entropy Balancing','OLS','No Adjustment'))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out$t <- factor(out$t, levels = c('Entropy Balancing','OLS','No Adjustment'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/alt_proc_rdd.rds")

######################

out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t == "First Difference in Turnout")

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists


saveRDS(different_dists, "temp/first_diff_plot.rds")
######################

out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t %in% c('Double Bandwidth','Nonpara, 30','Nonpara, 60')) %>% 
  mutate(t = ifelse(t == "Nonpara, 30", "30 Days",
                    ifelse(t == "Nonpara, 60", "60 Days", t)))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out$t <- factor(out$t, levels = c('Double Bandwidth','30 Days','60 Days'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/alt_bws_rdd.rds")

######################

out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t %in% c('Only 2016', 'Only 2020'))

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

saveRDS(different_dists, "temp/individual_years.rds")

#################################################################
#################################################################
#################################################################
#################################################################

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
    mutate(year = as.integer(year),
           t16 = year == 1)
  
  
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens)), ]
  
  ########################
  d16 <- filter(full_treat, year == 1)
  
  mb <- ebalance(d16$treated,
                 select(d16,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens))
  
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
                        pop_dens))
  
  d20 <- bind_rows(
    filter(d20, treated) %>%
      mutate(weight = 1),
    filter(d20, !treated) %>%
      mutate(weight = mb$w)
  )
  
  full_treat <- bind_rows(d16, d20)
  ########################################
  
  l <- rdrobust(y = full_treat$turnout_pre, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, t16))
  
  f <- tibble(coef = l$coef,
              se = l$se, 
              pv = l$pv,
              p = threshold,
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
}))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/placebo-prior.rds")
