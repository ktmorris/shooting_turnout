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
                                             ifelse(. %in% c("A", "N", "P"), "O", .))),
         so = 1 - nh_white - nh_black - latino,
         plu = ifelse(nh_white > nh_black & nh_white > latino & nh_white > so, "w",
                      ifelse(nh_black > nh_white & nh_black > latino & nh_black > so, "b",
                             ifelse(latino > nh_white & latino > nh_black & latino > so, "l", "o"))))

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
out <- rbindlist(lapply(c("w", "b", "l"), function(pl){
  rbindlist(lapply(c(seq(0.35, 1, 0.05), seq(2, 10, 1)), function(threshold){
    t2 <- 0
    full_treat <- bind_rows(
      dists %>% 
        filter(year == "2020",
               dist_pre <= threshold,
               dist_pre > t2,
               dist_post > threshold,
               plu == pl) %>% 
        select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
               median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
               turnout_16, turnout_18, turnout_14, turnout_pre, new) %>% 
        mutate(treated = T,
               d2 = as.integer(date - as.Date("2020-11-03"))),
      dists %>% 
        filter(year == "2020",
               dist_pre > threshold,
               dist_post <= threshold,
               plu == pl) %>% 
        select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
               median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
               turnout_16, turnout_18, turnout_14, turnout_pre, new) %>% 
        mutate(treated = F,
               d2 = as.integer(date - as.Date("2020-11-03"))),
      dists %>% 
        filter(year == "2016",
               dist_pre <= threshold,
               dist_pre > t2,
               dist_post > threshold,
               plu == pl) %>% 
        select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
               median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
               turnout_16, turnout_18, turnout_14, turnout_pre, new) %>% 
        mutate(treated = T,
               d2 = as.integer(date - as.Date("2016-11-08"))),
      dists %>% 
        filter(year == "2016",
               dist_pre > threshold,
               dist_post <= threshold,
               plu == pl) %>% 
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
   
    ########################################
    
    l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                  covs = select(full_treat,
                                latino, nh_white, asian,
                                nh_black, median_income, median_age,
                                pop_dens, turnout_pre, t16))
    
    f <- tibble(coef = l$coef,
                se = l$se, 
                pv = l$pv,
                p = threshold,
                n = l$N_h[1] + l$N_h[2],
                bwidth = l[["bws"]][1])
  })) %>% 
    mutate(bw = pl)
}))
saveRDS(out, "temp/plu_data.rds")
out <- readRDS("temp/plu_data.rds")


out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)
out <- filter(out, estimate == "Robust")

out <- mutate(out,
              bw = ifelse(bw == "b", "Plurality Black",
                          ifelse(bw == "l", "Plurality Latinx",
                                 "Plurality White")))

different_dists1 <- ggplot(filter(out, p <= 1),
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~bw) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "LATE", x = "Radius Around Shooting (Miles)") +
  ggtitle("(a) By Block Group Racial Composition")
different_dists1

saveRDS(different_dists1, "temp/plurality_nhood.rds")

different_dists2 <- ggplot(filter(out, p >= 1, bw == "Plurality Black"),
                           aes(x = p, y = coef, ymin = l, ymax = u)) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(plot.title = element_text(size = 12)) +
  labs(y = "LATE", x = "Radius Around Shooting (Miles)") +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  ggtitle("(b) Extended Distance,
Plurality-Black Block Groups")
  
different_dists2

different_dists1b <- ggplot(filter(out, p <= 1),
                           aes(x = p, y = n)) +
  facet_grid(~bw) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = comma) +
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Effective Sample Size", x = "Radius Around Shooting (Miles)") +
  ggtitle("(a) By Block Group Racial Composition")
different_dists1b
different_dists2b <- ggplot(filter(out, p >= 1, bw == "Plurality Black"),
                           aes(x = p, y = n)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = comma) +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(plot.title = element_text(size = 12)) +
  labs(y = "Effective Sample Size", x = "Radius Around Shooting (Miles)") +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  ggtitle("(b) Extended Distance,
Plurality-Black Block Groups")
different_dists2b
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

out <- rbindlist(lapply(c("W", "B", "H"), function(r){
  rbindlist(lapply(c(seq(0.35, 1, 0.05), seq(2, 10, 1)), function(threshold){
    t2 <- 0
    full_treat <- bind_rows(
      dists %>% 
        filter(year == "2020",
               dist_pre <= threshold,
               dist_pre > t2,
               dist_post > threshold) %>% 
        select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
               median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
               turnout_16, turnout_18, turnout_14, turnout_pre, new,
               race = race_pre) %>% 
        mutate(treated = T,
               d2 = as.integer(date - as.Date("2020-11-03"))),
      dists %>% 
        filter(year == "2020",
               dist_pre > threshold,
               dist_post <= threshold) %>% 
        select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
               median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
               turnout_16, turnout_18, turnout_14, turnout_pre, new,
               race = race_post) %>% 
        mutate(treated = F,
               d2 = as.integer(date - as.Date("2020-11-03"))),
      dists %>% 
        filter(year == "2016",
               dist_pre <= threshold,
               dist_pre > t2,
               dist_post > threshold) %>% 
        select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
               median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
               turnout_16, turnout_18, turnout_14, turnout_pre, new,
               race = race_pre) %>% 
        mutate(treated = T,
               d2 = as.integer(date - as.Date("2016-11-08"))),
      dists %>% 
        filter(year == "2016",
               dist_pre > threshold,
               dist_post <= threshold) %>% 
        select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
               median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
               turnout_16, turnout_18, turnout_14, turnout_pre, new,
               race = race_post) %>% 
        mutate(treated = F,
               d2 = as.integer(date - as.Date("2016-11-08")))
    ) %>% 
      mutate(year = as.integer(year),
             t16 = year == 1) %>% 
      filter(race == r)
    
    
    full_treat <- full_treat[complete.cases(select(full_treat,
                                                   latino, nh_white, asian, nh_black, median_income, median_age,
                                                   pop_dens, turnout_pre)), ]
    
    
    ########################################
    
    l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                  covs = select(full_treat,
                                latino, nh_white, asian,
                                nh_black, median_income, median_age,
                                pop_dens, turnout_pre, t16))
    
    f <- tibble(coef = l$coef,
                se = l$se, 
                pv = l$pv,
                p = threshold,
                n = l$N_h[1] + l$N_h[2],
                bwidth = l[["bws"]][1])
  })) %>% 
    mutate(bw = r)
}))

saveRDS(out, "temp/victim_data.rds")

out <- readRDS("temp/victim_data.rds")

out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)
out <- filter(out, estimate == "Robust")

out <- mutate(out,
              bw = ifelse(bw == "B", "Black Victim",
                          ifelse(bw == "H", "Latinx Victim",
                                 "White Victim")))

different_dists3 <- ggplot(filter(out, p <= 1),
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~bw) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "LATE", x = "Radius Around Shooting (Miles)") +
  ggtitle("(c) By Victim Race")
different_dists3

saveRDS(different_dists3, "temp/victim_race.rds")

different_dists4 <- ggplot(filter(out, p >= 1, bw == "Black Victim"),
                           aes(x = p, y = coef, ymin = l, ymax = u)) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "LATE", x = "Radius Around Shooting (Miles)") +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  ggtitle("(d) Extended Distance,
Black Victims")
different_dists4



p <- plot_grid(different_dists1, different_dists2, different_dists3, different_dists4,
          label_size = 12, rel_widths = c(3, 2),
          label_fontfamily = "LM Roman 10")
p
saveRDS(p, "temp/all_plots.rds")

#######################

different_dists3b <- ggplot(filter(out, p <= 1),
                           aes(x = p, y = n)) +
  facet_grid(~bw) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = comma) +
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Effective Sample Size", x = "Radius Around Shooting (Miles)") +
  ggtitle("(c) By Victim Race")
different_dists3b

different_dists4b <- ggplot(filter(out, p >= 1, bw == "Black Victim"),
                           aes(x = p, y = n)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = comma) +
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Effective Sample Size", x = "Radius Around Shooting (Miles)") +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  ggtitle("(d) Extended Distance,
Black Victims")
different_dists4b


p <- plot_grid(different_dists1b, different_dists2b, different_dists3b, different_dists4b,
               label_size = 12, rel_widths = c(3, 2),
               label_fontfamily = "LM Roman 10")
p

saveRDS(p, "temp/sample_sizes_breakout.rds")
