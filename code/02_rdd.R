threshold <- .25
#################
dists1 <- readRDS("temp/all_bgs_dist_shooting.rds") %>% 
  select(GEOID, starts_with("dist_")) %>% 
  pivot_longer(cols = c(starts_with("dist_")), names_to = "year", names_prefix = "dist_",
               values_to = "dist") %>% 
  separate(year, into = c("type", "year"), sep = "_") %>% 
  pivot_wider(id_cols = c(GEOID, year), names_from = "type", values_from = "dist", names_prefix = "dist_") %>% 
  mutate(year = paste0("20", year))

dists2 <- readRDS("temp/all_bgs_dist_shooting.rds") %>% 
  select(GEOID, starts_with("date_")) %>% 
  pivot_longer(cols = c(starts_with("date_")), names_to = "year", names_prefix = "date_",
               values_to = "dist") %>% 
  separate(year, into = c("type", "year"), sep = "_") %>% 
  pivot_wider(id_cols = c(GEOID, year), names_from = "type", values_from = "dist", names_prefix = "date_") %>% 
  mutate(year = paste0("20", year))

dists3 <- readRDS("temp/all_bgs_dist_shooting.rds") %>% 
  select(GEOID, starts_with("id_")) %>% 
  pivot_longer(cols = c(starts_with("id_")), names_to = "year", names_prefix = "id_",
               values_to = "dist") %>% 
  separate(year, into = c("type", "year"), sep = "_") %>% 
  pivot_wider(id_cols = c(GEOID, year), names_from = "type", values_from = "dist", names_prefix = "id_") %>% 
  mutate(year = paste0("20", year))

dists <- full_join(dists1, full_join(dists2, dists3))
cleanup(c("dists", "threshold"))

#################### 2016

set_2016 <- bind_rows(
  dists %>% 
    filter(year == "2016",
           dist_pre <= threshold,
           dist_post > threshold) %>% 
    select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year) %>% 
    mutate(treated = T,
           d2 = date - as.Date("2016-11-08")),
  dists %>% 
    filter(year == "2016",
           dist_pre > threshold,
           dist_post <= threshold) %>% 
    select(GEOID, id = id_post, date = date_post, dist = dist_post, year) %>% 
    mutate(treated = F,
           d2 = date - as.Date("2016-11-08"))
)

set_2018 <- bind_rows(
  dists %>% 
    filter(year == "2018",
           dist_pre <= threshold,
           dist_post > threshold) %>% 
    select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year) %>% 
    mutate(treated = T,
           d2 = date - as.Date("2018-11-06")),
  dists %>% 
    filter(year == "2018",
           dist_pre > threshold,
           dist_post <= threshold) %>% 
    select(GEOID, id = id_post, date = date_post, dist = dist_post, year) %>% 
    mutate(treated = F,
           d2 = date - as.Date("2018-11-06"))
)

set_2020 <- bind_rows(
  dists %>% 
    filter(year == "2020",
           dist_pre <= threshold,
           dist_post > threshold) %>% 
    select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year) %>% 
    mutate(treated = T,
           d2 = date - as.Date("2020-11-03")),
  dists %>% 
    filter(year == "2020",
           dist_pre > threshold,
           dist_post <= threshold) %>% 
    select(GEOID, id = id_post, date = date_post, dist = dist_post, year) %>% 
    mutate(treated = F,
           d2 = date - as.Date("2020-11-03"))
)

full_treat <- bind_rows(set_2016, set_2018, set_2020)
cleanup("full_treat")
########################## ballots

ballots_16 <- readRDS("../avr_turnout/temp/full_bg_turnout_16.rds") %>% 
  mutate(year = "2016") %>% 
  select(-voter_count)

ballots_18 <- readRDS("../avr_turnout/temp/full_bg_turnout_18.rds") %>% 
  mutate(year = "2018") %>% 
  select(-voter_count)

files <- list.files("../blm_turnout/temp", pattern = "^ballots_by_bg", full.names = T)

ballots_20 <- rbindlist(lapply(files, readRDS)) %>% 
  select(GEOID, ballots) %>% 
  mutate(year = "2020")

ballots <- bind_rows(ballots_16, ballots_18, ballots_20)
full_treat <- inner_join(full_treat, ballots)
cleanup("full_treat")

########################### cvap
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

cvap <- bind_rows(cvap16, cvap18, cvap20)

full_treat <- inner_join(full_treat, cvap)
cleanup("full_treat")

########################### census

c16 <- readRDS("../regular_data/census_bgs_18.rds") %>% 
  mutate(year = "2016") %>% 
  select(GEOID, year, latino, nh_white, nh_black, median_income, median_age,
         pop_dens)

c18 <- readRDS("../regular_data/census_bgs_18.rds") %>% 
  mutate(year = "2018") %>% 
  select(GEOID, year, latino, nh_white, nh_black, median_income, median_age,
         pop_dens)

c20 <- readRDS("../regular_data/census_bgs_19.rds") %>% 
  mutate(year = "2020") %>% 
  select(GEOID, year, latino, nh_white, nh_black, median_income, median_age,
         pop_dens)
census <- bind_rows(c16, c18, c20)

full_treat <- inner_join(full_treat, census)
cleanup("full_treat")

saveRDS(full_treat, "temp/full_treat_quarter.rds")

full_treat <- readRDS("temp/full_treat_half.rds") %>% 
  filter(year == 2020,
         date >= "2020-10-03",
         date < "2020-12-03") %>% 
  mutate(t_state = factor(substring(GEOID, 1, 2)))

full_treat <- full_treat %>% 
  mutate(turnout = ballots / cvap,
         turnout = ifelse(turnout > 1, 1, turnout),
         d2 = as.integer(d2),
         year = factor(year))

out <- rbindlist(lapply(c(1:5), function(x){
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = x, c = 0, cluster = full_treat$id,
                covs = select(full_treat, median_income, nh_white, nh_black, median_age, pop_dens, latino))
  
  f <- tibble(coef = l$coef,
              se = l$se, 
              pv = l$pv,
              p = x)
}))

out$l <- out$coef - 1.96*out$se
out$u <- out$coef + 1.96*out$se
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

ggplot(out %>% 
         filter(estimate == "Traditional"),
       aes(x = p, y = coef, ymin = l, ymax = u)) +
  geom_point() +
  geom_errorbar() + 
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Estimated Effect Size", x = "Polynomial")

###########################

rdplot(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0,
       covs = select(full_treat, median_income, nh_white, nh_black, median_age, pop_dens, latino))
