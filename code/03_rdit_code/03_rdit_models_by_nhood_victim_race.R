
### read in data saved previously
dists <- readRDS("temp/shooting_demos.rds")

dists <- left_join(dists, readRDS("temp/geocoded_shootings.rds") %>% 
                     ungroup() %>% 
                     mutate(score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
                     filter(score > 95) %>% 
                     select(id = id2, race)) %>% 
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
## loop over thresholds for different plurality neighborhoods

## the logic in this file follows that of the previous one; less commenting is hence given here

out <- rbindlist(lapply(c("b", "l", "w"), function(pl){
  rbindlist(lapply(c(seq(0.25, 1, 0.05), seq(2, 20, 2)), function(threshold){
    if(pl == "b" | threshold <= 1){
      print(paste(pl, threshold))
      ##keep the observations within the threshold closest to election day
      ## using data.table notation to speed this up
      set_pre <- dists[dists$dist <= threshold & dists$pre & dists$plu == pl,
                       .SD[date %in% max(date)], by=list(year, GEOID)]
      set_pre <- set_pre[, .SD[1], by = .(year, GEOID)] %>% 
        mutate(treated = T)
      
      ##keep the observations within the threshold closest to election day
      set_post <- dists[dists$dist <= threshold & !dists$pre & dists$plu == pl,
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
      ########################################
      
      l2 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                     weights = full_treat$weight,
                     covs = select(full_treat,
                                   latino, nh_white, asian,
                                   nh_black, median_income, median_age,
                                   pop_dens, turnout_pre, t16, some_college))
      
      f <- bind_rows(
        tibble(coef = l2$coef,
               se = l2$se, 
               pv = l2$pv,
               p = threshold,
               n = l2$N_h[1] + l2$N_h[2],
               bwidth = l2[["bws"]][1],
               u = l2[["ci"]][,2],
               l = l2[["ci"]][,1],
               t = "covs"))
      
      return(f)
    }
  })) %>% 
    mutate(bw = pl)
}))
saveRDS(out, "temp/plu_data_wide_ebal.rds")
out <- readRDS("temp/plu_data_wide_ebal.rds")


out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)
out <- filter(out, estimate == "Robust", t == "covs")

out <- mutate(out,
              bw = ifelse(bw == "b", "Plurality Black",
                          ifelse(bw == "l", "Plurality Latinx",
                                 "Plurality White")))
# Create Figure 4A 
different_dists1 <- ggplot(filter(out, p <= 1, p > 0.31),
                           aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~bw) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)") +
  ggtitle("(a) By Block Group Racial Composition")
different_dists1

saveRDS(different_dists1, "temp/plurality_nhood.rds")

# create figure 4B
different_dists2 <- ggplot(filter(out, p >= 1, bw == "Plurality Black"),
                           aes(x = p, y = coef, ymin = l, ymax = u)) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(plot.title = element_text(size = 12)) +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)") +
  scale_x_continuous(breaks = seq(2, 20, 2)) +
  ggtitle("(b) Extended Distance,
Plurality-Black Block Groups")

different_dists2

# create figure A5A
different_dists1b <- ggplot(filter(out, p <= 1, p > 0.31),
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

#create figure A5B
different_dists2b <- ggplot(filter(out, p >= 1, bw == "Plurality Black"),
                            aes(x = p, y = n)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = comma) +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(plot.title = element_text(size = 12)) +
  labs(y = "Effective Sample Size", x = "Radius Around Shooting (Miles)") +
  scale_x_continuous(breaks = seq(2, 20, 2)) +
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
### now loop over victim race
out <- rbindlist(lapply(c("B", "H", "W"), function(r){
  rbindlist(lapply(c(seq(0.3, 1, 0.05), seq(2, 20, 1)), function(threshold){
    if(r == "B" | threshold <= 1){
      print(paste(r, threshold))
      ##keep the observations within the threshold closest to election day
      ## using data.table notation to speed this up
      exposed_other <- dists[dists$dist <= threshold & dists$pre & dists$race != r,]
      
      set_pre <- dists[dists$dist <= threshold & dists$pre & dists$race == r,
                       .SD[date %in% max(date)], by=list(year, GEOID)]
      set_pre <- set_pre[, .SD[1], by = .(year, GEOID)] %>% 
        mutate(treated = T) %>% 
        filter(!(paste(GEOID, year) %in% paste(exposed_other$GEOID, exposed_other$year)))
      
      
      ##keep the observations within the threshold closest to election day
      set_post <- dists[dists$dist <= threshold & !dists$pre & dists$race == r,
                        .SD[date %in% min(date)], by=list(year, GEOID)]
      set_post <- set_post[!(paste0(set_post$GEOID, set_post$year) %in% paste0(set_pre$GEOID, set_pre$year)),
                           .SD[1], by = .(year, GEOID)] %>% 
        mutate(treated = F) %>% 
        filter(!(paste(GEOID, year) %in% paste(exposed_other$GEOID, exposed_other$year)))
      
      
      full_treat <- bind_rows(set_pre, set_post) %>% 
        select(GEOID, id, date, dist, year, turnout,
               median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
               some_college, turnout_pre, treated) %>% 
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
      ########################################
      
      l2 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                     weights = full_treat$weight,
                     covs = select(full_treat,
                                   latino, nh_white, asian,
                                   nh_black, median_income, median_age,
                                   pop_dens, turnout_pre, t16, some_college))
      
      
      f <- bind_rows(
        tibble(coef = l2$coef,
               se = l2$se, 
               pv = l2$pv,
               p = threshold,
               n = l2$N_h[1] + l2$N_h[2],
               bwidth = l2[["bws"]][1],
               u = l2[["ci"]][,2],
               l = l2[["ci"]][,1],
               t = "covs"))
      return(f)
    }
  })) %>% 
    mutate(bw = r)
}))

saveRDS(out, "temp/victim_data_wide_ebal.rds")
out <- readRDS("temp/victim_data_wide_ebal.rds")

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)
out <- filter(out, estimate == "Robust", t == "covs")

out <- mutate(out,
              bw = ifelse(bw == "B", "Black Victim",
                          ifelse(bw == "H", "Latinx Victim",
                                 "White Victim")))
#create figure 4C 
different_dists3 <- ggplot(filter(out, p <= 1, p > 0.31),
                           aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~bw) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)") +
  ggtitle("(c) By Victim Race")
different_dists3

saveRDS(different_dists3, "temp/victim_race.rds")
## create figure 4D
different_dists4 <- ggplot(filter(out, p >= 1, bw == "Black Victim",
                                  p %% 2 == 0 | p == 1),
                           aes(x = p, y = coef, ymin = l, ymax = u)) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)") +
  scale_x_continuous(breaks = seq(2, 20, 2)) +
  ggtitle("(d) Extended Distance,
Black Victims")
different_dists4


#Create figure 4
p <- plot_grid(different_dists1, different_dists2, different_dists3, different_dists4,
               label_size = 12, rel_widths = c(3, 2),
               label_fontfamily = "LM Roman 10")
p
saveRDS(p, "temp/all_plots.rds")

#######################
#create figure A5C
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

#create Figure A5D
different_dists4b <- ggplot(filter(out, p >= 1, bw == "Black Victim"),
                            aes(x = p, y = n)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = comma) +
  theme_bc(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Effective Sample Size", x = "Radius Around Shooting (Miles)") +
  scale_x_continuous(breaks = seq(2, 20, 2)) +
  ggtitle("(d) Extended Distance,
Black Victims")
different_dists4b

# create figure A5
p <- plot_grid(different_dists1b, different_dists2b, different_dists3b, different_dists4b,
               label_size = 12, rel_widths = c(3, 2),
               label_fontfamily = "LM Roman 10")
p

saveRDS(p, "temp/sample_sizes_breakout.rds")
