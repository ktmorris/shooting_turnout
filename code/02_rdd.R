# ballots_14 <- rbindlist(lapply(unique(filter(fips_codes, state_code <= 56)$state_code), function(s1){
#   s <- unique(filter(fips_codes, state_code == s1)$state)
#   print(s)
#   if(!(file.exists(paste0("../regular_data/bg_turnout/", s, "_bg_to_14.rds")))){
#   j <- bind_rows(
#   l <-  readRDS(paste0("../fees_fines_to/temp/", s, "_to_14_block.rds")) %>%
#       mutate(GEOID = paste0(s1, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
#                             str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
#                             Residence_Addresses_CensusBlockGroup)),
#     readRDS(paste0("../fees_fines_to/temp/cleaned_", s, "_2014.rds")) %>%
#      mutate(GEOID = paste0(s1, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
#                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
#                            Residence_Addresses_CensusBlockGroup)) %>%
#     select(-Voters_FIPS,
#            -Residence_Addresses_CensusTract,
#            -Residence_Addresses_CensusBlockGroup,
#            -Residence_Addresses_CensusBlock)
#   )
# 
#   if(s == "HI"){
#     j <- rename(j, General_2014.11.04 = `General_2014-11-04`)
#   }
# 
#   j <- j %>%
#     group_by(GEOID) %>%
#     summarize(ballots = sum(General_2014.11.04 == "Y")) %>%
#     mutate(year = "2014")
# 
#   saveRDS(j, paste0("../regular_data/bg_turnout/", s, "_bg_to_14.rds"))}
#   else{
#     readRDS(paste0("../regular_data/bg_turnout/", s, "_bg_to_14.rds"))
#   }
# }))
# 
# files <- list.files("temp", pattern = "^16_ballots_by_bg", full.names = T)
# 
# ballots_16 <- rbindlist(lapply(files, readRDS)) %>%
#   select(GEOID, ballots, new) %>%
#   mutate(year = "2016",
#          new = new / ballots)
# 
# ballots_18 <- readRDS("../avr_turnout/temp/full_bg_turnout_18.rds") %>%
#   mutate(year = "2018") %>%
#   select(-voter_count)
# 
# files <- list.files("temp", pattern = "^ballots_by_bg", full.names = T)
# 
# ballots_20 <- rbindlist(lapply(files, readRDS)) %>%
#   select(GEOID, ballots, new) %>%
#   mutate(year = "2020",
#          new = new / ballots)
# 
# ballots_20 <- left_join(ballots_20, rename(select(ballots_14, -year), ballots_14 = ballots))
# ballots_20 <- left_join(ballots_20, rename(select(ballots_18, -year), ballots_18 = ballots))
# ballots_20 <- left_join(ballots_20, rename(select(ballots_16, -year, -new), ballots_16 = ballots))
# 
# ballots <- bind_rows(ballots_14, ballots_16, ballots_18, ballots_20)
# cleanup("ballots")
# 
# ########################### cvap
# cvap14 <- fread("../regular_data/CVAP_2010-2014_ACS_csv_files/BlockGr.csv") %>%
#   filter(lntitle == "Total") %>%
#   mutate(GEOID = substring(geoid, 8),
#          year = "2014") %>%
#   select(GEOID, cvap = CVAP_EST, year)
# 
# cvap16 <- fread("../regular_data/CVAP_2012-2016_ACS_csv_files/BlockGr.csv") %>%
#   filter(lntitle == "Total") %>%
#   mutate(GEOID = substring(geoid, 8),
#          year = "2016") %>%
#   select(GEOID, cvap = CVAP_EST, year)
# 
# cvap18 <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/BlockGr.csv") %>%
#   filter(lntitle == "Total") %>%
#   mutate(GEOID = substring(geoid, 8),
#          year = "2018") %>%
#   select(GEOID, year, cvap = cvap_est)
# 
# cvap20 <- fread("../regular_data/CVAP_2015-2019_ACS_csv_files/BlockGr.csv") %>%
#   filter(lntitle == "Total") %>%
#   mutate(GEOID = substring(geoid, 8),
#          year = "2020") %>%
#   select(GEOID, year, cvap = cvap_est)
# 
# cvap20 <- left_join(cvap20, rename(select(cvap18, -year), cvap_18 = cvap))
# cvap20 <- left_join(cvap20, rename(select(cvap16, -year), cvap_16 = cvap))
# cvap20 <- left_join(cvap20, rename(select(cvap14, -year), cvap_14 = cvap))
# 
# cvap <- bind_rows(cvap14, cvap16, cvap18, cvap20)
# 
# ballots <- inner_join(ballots, cvap)
# cleanup("ballots")
# ########################### census
# c14 <- readRDS("../regular_data/census_bgs_14.rds") %>%
#   mutate(year = "2014") %>%
#   select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
#          pop_dens)
# 
# c16 <- readRDS("../regular_data/census_bgs_16.rds") %>%
#   mutate(year = "2016") %>%
#   select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
#          pop_dens)
# 
# c18 <- readRDS("../regular_data/census_bgs_18.rds") %>%
#   mutate(year = "2018") %>%
#   select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
#          pop_dens)
# 
# c20 <- readRDS("../regular_data/census_bgs_19.rds") %>%
#   mutate(year = "2020") %>%
#   select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
#          pop_dens)
# census <- bind_rows(c14, c16, c18, c20)
# 
# ballots <- inner_join(ballots, census)
# 
# saveRDS(filter(ballots, year == 2020), "temp/to_ey.rds")
# cleanup("ballots")
# ####################
# dists <- readRDS("temp/dists_long.rds")
# 
# dists <- inner_join(dists, ballots) %>%
#   mutate(turnout = ballots / cvap,
#          turnout_18 = ballots_18 / cvap_18,
#          turnout_16 = ballots_16 / cvap_16,
#          turnout_14 = ballots_14 / cvap_14,
#          year = factor(year)) %>%
#   mutate_at(vars(starts_with("turnout")), ~ ifelse(. > 1, 1, .))
# 
# d1 <- filter(dists, year != "2016")
# d2 <- filter(dists, year == "2016")
# 
# d2 <- left_join(select(d2, -turnout_18, -turnout_14), filter(d1, year == "2020") %>%
#                   select(GEOID, turnout_18, turnout_14))
# dists <- bind_rows(d1, d2)
# 
# saveRDS(dists, "temp/shooting_demos.rds")

dists <- readRDS("temp/shooting_demos.rds") %>% 
  mutate(turnout_pre = ifelse(year == "2016", turnout_14, turnout_18))

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
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, turnout_pre))
  
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

saveRDS(different_dists, "temp/different_dists_primary.rds")
###########################
threshold <- 0.5
full_treat <- bind_rows(
  dists %>%
    filter(year == "2020",
           dist_pre <= threshold,
           dist_post > threshold) %>%
    select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout, cvap,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           turnout_16, turnout_18, turnout_pre) %>%
    mutate(treated = T,
           d2 = as.integer(date - as.Date("2020-11-03"))),
  dists %>%
    filter(year == "2020",
           dist_pre > threshold,
           dist_post <= threshold) %>%
    select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           turnout_16, turnout_18, turnout_pre) %>%
    mutate(treated = F,
           d2 = as.integer(date - as.Date("2020-11-03"))),
  dists %>%
    filter(year == "2016",
           dist_pre <= threshold,
           dist_post > threshold) %>%
    select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           turnout_16, turnout_18, turnout_pre) %>%
    mutate(treated = T,
           d2 = as.integer(date - as.Date("2016-11-08"))),
  dists %>%
    filter(year == "2016",
           dist_pre > threshold,
           dist_post <= threshold) %>%
    select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           turnout_16, turnout_18, turnout_pre) %>%
    mutate(treated = F,
           d2 = as.integer(date - as.Date("2016-11-08")))
) %>%
  mutate(year = as.integer(year))

full_treat <- full_treat[complete.cases(select(full_treat,
                                               latino, nh_white, asian,
                                               nh_black, median_income, median_age,
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

out <- rbindlist(lapply(c(1:5), function(x){
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = x, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              nh_black, latino, nh_white, asian, median_income, median_age,
                              pop_dens, turnout_pre))

  f <- tibble(coef = l$coef,
              se = l$se,
              pv = l$pv,
              p = x)
}))

out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out <- mutate_at(out, vars(coef, l, u), ~. * -1)

dd <- ggplot(out,
       aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar() +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Estimated Effect Size", x = "Polynomial")
dd
saveRDS(dd, "temp/diff_polys_primary.rds")
###########################
j <- rdplot(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0,
       weights = full_treat$weight,
       covs = select(full_treat,
                     nh_black, latino, nh_white, asian, median_income, median_age,
                     pop_dens, turnout_pre))[["rdplot"]]
j[["labels"]] <- j[["labels"]][-1]
j <- j +
  theme_bc(base_family = "LM Roman 10") +
  labs(x = "Days Between Police Killing and Election",
       y = "Turnout",
       caption = "Notes: Post-election units are balanced using an entropic process to mirror pre-election units.
Covariates for entropy balancing and RDD include: % Black, % white, % Asian, % Latinx, median income,
median age, population density, turnout in previous midterm election.") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60 Days\nbefore Election", "30", "Election Day", "30 Days\nAfter Election", "60"))
j

saveRDS(j, "temp/rd_plot.rds")
#################################

demos <- full_treat %>% 
  group_by(year, treated) %>% 
  summarize_at(vars(nh_black, latino, nh_white, asian, median_income, median_age,
                    pop_dens, turnout_pre), ~ weighted.mean(., weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Weighted Controls"))

demos2 <- full_treat %>% 
  filter(!treated) %>%
  group_by(year) %>% 
  summarize_at(vars(nh_black, latino, nh_white, asian, median_income, median_age,
                    pop_dens, turnout_pre), mean) %>% 
  mutate(treated = "Unweighted Controls")

demos <- bind_rows(demos, demos2)

f <- bind_rows(readRDS("temp/to_ey.rds") %>% 
                 mutate(year = 1,
                        turnout_pre = ballots_14 / cvap_14),
               readRDS("temp/to_ey.rds") %>% 
                 mutate(year = 3,
                        turnout_pre = ballots_18 / cvap_18))

f <- filter(f, !(GEOID %in% full_treat$GEOID)) %>% 
  select(nh_black, latino, nh_white, asian, median_income, median_age,
         pop_dens, turnout_pre, year)

f <- f[complete.cases(f), ] %>% 
  filter(is.finite(turnout_pre)) %>% 
  group_by(year) %>% 
  summarize_at(vars(nh_black, latino, nh_white, asian, median_income, median_age,
                    pop_dens, turnout_pre), mean, na.rm = T) %>% 
  mutate(treated = "Not in Dataset")


demos <- bind_rows(demos, f)


demos <- pivot_longer(demos, cols = c("nh_black", "latino", "nh_white",
                                  "asian", "median_income", "median_age",
                                  "pop_dens", "turnout_pre"))

demos <- pivot_wider(demos, id_cols = c("year", "name"), names_from = "treated", values_from = "value")

demos <- select(demos, year, name, `Not in Dataset`,
                `Treated`, `Unweighted Controls`, `Weighted Controls`)

###############################
##############################
vs <- fread("raw_data/var_orders.csv")

demos <- left_join(rename(demos, variable = name), vs)

demos <- demos %>% 
  mutate_at(vars(`Not in Dataset`,
                 `Treated`, `Unweighted Controls`, `Weighted Controls`),
            ~ ifelse(name == "Median Income", dollar(round(as.numeric(gsub(",", "", .)))), .)) %>% 
  mutate_at(vars(`Not in Dataset`,
                 `Treated`, `Unweighted Controls`, `Weighted Controls`),
            ~ ifelse(name == "Median Age", round(as.numeric(.), 1), .)) %>% 
  mutate_at(vars(`Not in Dataset`,
                 `Treated`, `Unweighted Controls`, `Weighted Controls`),
            ~ ifelse(name == "Population Density", comma(as.numeric(.), accuracy = 1), .)) %>% 
  mutate_at(vars(`Not in Dataset`,
                 `Treated`, `Unweighted Controls`, `Weighted Controls`),
            ~ ifelse(substring(name, 1, 1) == "%" |
                       name == "Previous Turnout", percent(as.numeric(.), accuracy = .1), .))
demos <- demos %>% 
  arrange(year, order) %>% 
  mutate(Year = ifelse(year == 1, 2016, 2020)) %>% 
  ungroup() %>% 
  select(-year, -order, -variable) %>%
  select(Year, Variable = name, everything())

saveRDS(demos, "temp/demo_table_half_mile.rds")
