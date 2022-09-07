## read in 2014 BG level turnout
files <- list.files("temp", pattern = "^14_ballots_by_bg", full.names = T)

ballots_14 <- rbindlist(lapply(files, readRDS)) %>%
  select(GEOID, ballots_pre = ballots) %>%
  mutate(year = "2016")

## read in 2016 BG level turnout
files <- list.files("temp", pattern = "^16_ballots_by_bg", full.names = T)

ballots_16 <- rbindlist(lapply(files, readRDS)) %>%
  select(GEOID, ballots) %>%
  mutate(year = "2016")

ballots_16 <- left_join(ballots_16, ballots_14)

## read in 2018 BG level turnout
files <- list.files("temp", pattern = "^18_ballots_by_bg", full.names = T)

ballots_18 <- rbindlist(lapply(files, readRDS)) %>%
  select(GEOID, ballots_pre = ballots) %>%
  mutate(year = "2020")

## read in 2020 BG level turnout
files <- list.files("temp", pattern = "^ballots_by_bg", full.names = T)

ballots_20 <- rbindlist(lapply(files, readRDS)) %>%
  select(GEOID, ballots) %>%
  mutate(year = "2020")

ballots_20 <- left_join(ballots_20, ballots_18)

## combine all years' turnout in long format
ballots <- bind_rows(ballots_16, ballots_20)
## remove unnecessary files from memory
cleanup("ballots")

########################### cvap
### read in block group cvap from each year

cvap14 <- fread("../regular_data/CVAP_2010-2014_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8),
         year = "2016") %>%
  select(GEOID, cvap_pre = CVAP_EST, year)

cvap16 <- fread("../regular_data/CVAP_2012-2016_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8),
         year = "2016") %>%
  select(GEOID, cvap = CVAP_EST, year)

cvap16 <- left_join(cvap16, cvap14)

cvap18 <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8),
         year = "2020") %>%
  select(GEOID, year, cvap_pre = cvap_est)

cvap20 <- fread("../regular_data/CVAP_2015-2019_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8),
         year = "2020") %>%
  select(GEOID, year, cvap = cvap_est)

cvap20 <- left_join(cvap20, cvap18)

cvap <- bind_rows(cvap16, cvap20)


## merge cvap and ballot count data
ballots <- inner_join(ballots, cvap)
cleanup("ballots")
########################### census

##read in ACS census demographic info for each BG for each year

c16 <- readRDS("../regular_data/census_bgs_16.rds") %>%
  mutate(year = "2016") %>%
  select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
         pop_dens, some_college)

c20 <- readRDS("../regular_data/census_bgs_19.rds") %>%
  mutate(year = "2020") %>%
  select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
         pop_dens, some_college)
census <- bind_rows(c16, c20)

## merge census data in, keep only primary data. save 2020 data
ballots <- inner_join(ballots, census)
saveRDS(filter(ballots, year == 2020), "temp/to_ey.rds")
cleanup("ballots")
saveRDS(ballots, "temp/full_demos.rds")
####################

## read in dists for each BG
dists <- readRDS("temp/dists_long_new_plac.rds") %>% 
  mutate(year = as.character(floor((year(date) + 1) / 2) * 2))

## calculate turnout, set to 100% if over 100%
dists <- inner_join(dists, ballots) %>% 
  mutate(turnout = ballots / cvap,
         turnout_pre = ballots_pre / cvap_pre,
         date = date %m+% years(1),
         pre = (year == "2016" & date <= "2016-11-08") | (year == "2020" & date <= "2020-11-03")) %>% 
  mutate_at(vars(starts_with("turnout")), ~ ifelse(. > 1, 1, .))

##### SAVE PRIMARY DATASET FOR RDITS!!!
saveRDS(dists, "temp/shooting_demos_plac.rds")
####################################################
####################################################
####################################################

dists <- readRDS("temp/shooting_demos_plac.rds")

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
  ## save half-mile observations for robustness checks, other analysis
  if(threshold == 0.3){
    saveRDS(full_treat, "temp/primary_third_plac.rds")
  }
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
}))

saveRDS(out, "temp/primary_out_data_plac.rds")
out <- readRDS("temp/primary_out_data_plac.rds")
## create figure 3
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))

different_dists <- ggplot(out,
       aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/different_dists_primary_plac.rds")
