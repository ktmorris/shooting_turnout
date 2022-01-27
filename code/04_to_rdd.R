## read in 2014 BG level turnout
files <- list.files("temp", pattern = "^14_ballots_by_bg", full.names = T)

ballots_14 <- rbindlist(lapply(files, readRDS)) %>%
  select(GEOID, ballots) %>%
  mutate(year = "2014")

## read in 2016 BG level turnout
files <- list.files("temp", pattern = "^16_ballots_by_bg", full.names = T)

ballots_16 <- rbindlist(lapply(files, readRDS)) %>%
  select(GEOID, ballots) %>%
  mutate(year = "2016")

## read in 2018 BG level turnout
files <- list.files("temp", pattern = "^18_ballots_by_bg", full.names = T)

ballots_18 <- rbindlist(lapply(files, readRDS)) %>%
  select(GEOID, ballots) %>%
  mutate(year = "2018")

## read in 2020 BG level turnout
files <- list.files("temp", pattern = "^ballots_by_bg", full.names = T)

ballots_20 <- rbindlist(lapply(files, readRDS)) %>%
  select(GEOID, ballots) %>%
  mutate(year = "2020")

## combine all years' turnout in long format
ballots <- bind_rows(ballots_14, ballots_16, ballots_18, ballots_20)
## remove unnecessary files from memory
cleanup("ballots")

########################### cvap
### read in block group cvap from each year
cvap14 <- fread("../regular_data/CVAP_2010-2014_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8),
         year = "2014") %>%
  select(GEOID, cvap = CVAP_EST, year)

cvap16 <- fread("../regular_data/CVAP_2012-2016_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8),
         year = "2016") %>%
  select(GEOID, cvap = CVAP_EST, year)

cvap18 <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8),
         year = "2018") %>%
  select(GEOID, year, cvap = cvap_est)

cvap20 <- fread("../regular_data/CVAP_2015-2019_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8),
         year = "2020") %>%
  select(GEOID, year, cvap = cvap_est)

cvap <- bind_rows(cvap14, cvap16, cvap18, cvap20)


## merge cvap and ballot count data
ballots <- inner_join(ballots, cvap)
cleanup("ballots")
########################### census

##read in ACS census demographic info for each BG for each year
c14 <- readRDS("../regular_data/census_bgs_14.rds") %>%
  mutate(year = "2014") %>%
  select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
         pop_dens)

c16 <- readRDS("../regular_data/census_bgs_16.rds") %>%
  mutate(year = "2016") %>%
  select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
         pop_dens)

c18 <- readRDS("../regular_data/census_bgs_18.rds") %>%
  mutate(year = "2018") %>%
  select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
         pop_dens)

c20 <- readRDS("../regular_data/census_bgs_19.rds") %>%
  mutate(year = "2020") %>%
  select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
         pop_dens)
census <- bind_rows(c14, c16, c18, c20)

## merge census data in, keep only primary data. save 2020 data
ballots <- inner_join(ballots, census)
saveRDS(filter(ballots, year == 2020), "temp/to_ey.rds")
cleanup("ballots")
####################

## read in dists for each BG
dists <- readRDS("temp/dists_long.rds")

## calculate turnout, set to 100% if over 100%
dists <- inner_join(dists, ballots) %>%
  mutate(turnout = ballots / cvap,
         turnout_18 = ballots_18 / cvap_18,
         turnout_16 = ballots_16 / cvap_16,
         turnout_14 = ballots_14 / cvap_14,
         year = factor(year)) %>%
  mutate_at(vars(starts_with("turnout")), ~ ifelse(. > 1, 1, .))

d1 <- filter(dists, year != "2016")
d2 <- filter(dists, year == "2016")

d2 <- left_join(select(d2, -turnout_18, -turnout_14), filter(d1, year == "2020") %>%
                  select(GEOID, turnout_18, turnout_14))
dists <- bind_rows(d1, d2)
##### SAVE PRIMARY DATASET FOR RDITS!!!
saveRDS(dists, "temp/shooting_demos.rds")
####################################################
####################################################
####################################################

dists <- readRDS("temp/shooting_demos.rds") %>% 
  mutate(turnout_pre = ifelse(year == "2016", turnout_14, turnout_18))


### loop over thresholds for RDITS
out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  t2 <- 0
  ## keep observations within threshold before / after each election
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
  
  ## keep complete cases, necessary for balancing
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens, turnout_pre)), ]
  
  ########################
  
  ##balancing pre/post obs within 2016
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
  
  ##balancing pre/post obs within 2020
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
  
  ## recombine
  full_treat <- bind_rows(d16, d20)
  ## save half-mile observations for robustness checks, other analysis
  if(threshold == 0.5){
    saveRDS(full_treat, "temp/primary_half.rds")
  }
  ########################################
  
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, turnout_pre, t16))
  
  f <- tibble(coef = l$coef,
              se = l$se, 
              pv = l$pv,
              p = threshold,
              n = l$N_h[1] + l$N_h[2],
              bw = l[["bws"]][1],
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
}))

## create figure 3
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))

different_dists <- ggplot(out,
       aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/different_dists_primary.rds")

## create figure A4
sample_sizes <- ggplot(filter(out, estimate == "Traditional"),
                          aes(x = p, y = n)) +
  geom_line() +
  geom_point() +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Effective Sample Size", x = "Radius Around Shooting (Miles)") +
  scale_y_continuous(labels = comma)
sample_sizes
saveRDS(sample_sizes, "temp/sample_size_plot.rds")
###########################
###########################
###########################
## read in half-mile observations
full_treat <- readRDS("temp/primary_half.rds")

l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
              weights = full_treat$weight,
              covs = select(full_treat,
                            nh_black, latino, nh_white, asian, median_income, median_age,
                            pop_dens, turnout_pre, t16))
########################################
## run with different polynomials for Figure A7
out <- rbindlist(lapply(c(1:5), function(x){
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = x, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              nh_black, latino, nh_white, asian, median_income, median_age,
                              pop_dens, turnout_pre, t16))

  f <- tibble(coef = l$coef,
              se = l$se,
              pv = l$pv,
              p = x,
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
}))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out <- mutate_at(out, vars(coef, l, u), ~. * -1)

dd <- ggplot(out,
       aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar(width = 0) +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Estimated Effect Size", x = "Polynomial")
dd
saveRDS(dd, "temp/diff_polys_primary.rds")
########################################
## run with different cutpoints for Figure A8
out <- rbindlist(lapply(c(-7:7), function(x){
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = x, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              nh_black, latino, nh_white, asian, median_income, median_age,
                              pop_dens, turnout_pre, t16))
  
  f <- tibble(coef = l$coef,
              se = l$se,
              pv = l$pv,
              p = x,
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
}))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$z <- out$p == 0

dd <- ggplot(out,
             aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_errorbar(width = 0) +
  geom_point(aes(color = z)) +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Estimated Effect Size", x = "Cut-Point") +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none")
dd
saveRDS(dd, "temp/placebos.rds")
###########################

## create naive RDIT plot for Figure 2
j <- rdplot(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0)[["rdplot"]]
j[["labels"]] <- j[["labels"]][-1]
j <- j +
  theme_bc(base_family = "LM Roman 10") +
  labs(x = "Days Between Police Killing and Election",
       y = "Turnout") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("Killing Occurs\n60 Days\nBefore Election",
                                "30", "Killing Occurs on\nElection Day",
                                "30", "Killing Occurs\n60 Days\nAfter Election"),
                     limits = c(-63, 63))
j

saveRDS(j, "temp/rd_plot.rds")
#################################
## the rest of the script creates Table 1

## start with weighted means
demos <- full_treat %>% 
  group_by(year, treated) %>% 
  summarize(across(c("nh_black", "latino", "nh_white", "asian", "median_income", "median_age",
                    "pop_dens", "turnout_pre"), ~ weighted.mean(., weight)),
            n = n_distinct(GEOID),
            n_shooting = n_distinct(id)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Weighted Controls"))

## unweighted means
demos2 <- full_treat %>% 
  filter(!treated) %>%
  group_by(year) %>% 
  summarize(across(c("nh_black", "latino", "nh_white", "asian", "median_income", "median_age",
                     "pop_dens", "turnout_pre"), mean),
            n = n_distinct(GEOID),
            n_shooting = n_distinct(id)) %>% 
  mutate(treated = "Unweighted Controls")

demos <- bind_rows(demos, demos2)

## pull in previous turnout from file saved above.
f <- bind_rows(readRDS("temp/to_ey.rds") %>% 
                 mutate(year = 1,
                        turnout_pre = ballots_14 / cvap_14),
               readRDS("temp/to_ey.rds") %>% 
                 mutate(year = 3,
                        turnout_pre = ballots_18 / cvap_18))

## keep only untreated / not in data observations
f <- filter(f, !(GEOID %in% full_treat$GEOID)) %>% 
  select(nh_black, latino, nh_white, asian, median_income, median_age,
         pop_dens, turnout_pre, year, GEOID)

f <- f[complete.cases(f), ] %>% 
  filter(is.finite(turnout_pre)) %>% 
  group_by(year) %>% 
  summarize(across(c("nh_black", "latino", "nh_white", "asian", "median_income", "median_age",
                     "pop_dens", "turnout_pre"), mean),
            n = n_distinct(GEOID),
            n_shooting = 0) %>% 
  mutate(treated = "Not in Dataset")


demos <- bind_rows(demos, f)


demos <- pivot_longer(demos, cols = c("nh_black", "latino", "nh_white",
                                  "asian", "median_income", "median_age",
                                  "pop_dens", "turnout_pre", "n", "n_shooting"))

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
            ~ ifelse(name %in% c("Population Density", "Number of Block Groups",
                                 "Number of Killings"), comma(as.numeric(.), accuracy = 1), .)) %>% 
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

