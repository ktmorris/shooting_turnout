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

ballots_20 <- left_join(ballots_20, rename(select(ballots_14, -year), ballots_14 = ballots))
ballots_20 <- left_join(ballots_20, rename(select(ballots_18, -year), ballots_18 = ballots))
ballots_20 <- left_join(ballots_20, rename(select(ballots_16, -year), ballots_16 = ballots))

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

cvap20 <- left_join(cvap20, rename(select(cvap18, -year), cvap_18 = cvap))
cvap20 <- left_join(cvap20, rename(select(cvap16, -year), cvap_16 = cvap))
cvap20 <- left_join(cvap20, rename(select(cvap14, -year), cvap_14 = cvap))

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
cleanup("ballots")
####################

## read in dists for each BG
dists <- readRDS("temp/oldxxx_dists_long.rds")

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
saveRDS(dists, "temp/oldxxx_shooting_demos.rds")
##### SAVE PRIMARY DATASET FOR RDITS!!!




dists <- readRDS("temp/oldxxx_shooting_demos.rds") %>% 
  mutate(turnout_pre = ifelse(year == "2016", turnout_14, turnout_18))

dists <- left_join(dists, readRDS("temp/geocoded_shootings.rds") %>% 
                     ungroup() %>% 
                     mutate(id = row_number(),
                            score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
                     filter(score > 95) %>% 
                     select(id_pre = id2, race_pre = race))

dists <- left_join(dists, readRDS("temp/geocoded_shootings.rds") %>% 
                     ungroup() %>% 
                     mutate(id = row_number(),
                            score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
                     filter(score > 95) %>% 
                     select(id_post = id2, race_post = race)) %>% 
  mutate(across(starts_with("race"), ~substring(., 1, 1)),
         across(starts_with("race"), ~ifelse(. == "", "U",
                                             ifelse(. %in% c("A", "N", "P"), "O", .))),
         so = 1 - nh_white - nh_black - latino,
         plu = ifelse(nh_white > nh_black & nh_white > latino & nh_white > so, "w",
                      ifelse(nh_black > nh_white & nh_black > latino & nh_black > so, "b",
                             ifelse(latino > nh_white & latino > nh_black & latino > so, "l", "o"))))

threshold = 0.3
pl = "b"

t2 <- 0
full_treat2 <- bind_rows(
  dists %>% 
    filter(year == "2020",
           dist_pre <= threshold,
           dist_pre > t2,
           dist_post > threshold,
           plu == pl) %>% 
    select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, year, turnout,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           turnout_16, turnout_18, turnout_14, turnout_pre) %>% 
    mutate(treated = T,
           d2 = as.integer(date - as.Date("2020-11-03"))),
  dists %>% 
    filter(year == "2020",
           dist_pre > threshold,
           dist_post <= threshold,
           plu == pl) %>% 
    select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           turnout_16, turnout_18, turnout_14, turnout_pre) %>% 
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
           turnout_16, turnout_18, turnout_14, turnout_pre) %>% 
    mutate(treated = T,
           d2 = as.integer(date - as.Date("2016-11-08"))),
  dists %>% 
    filter(year == "2016",
           dist_pre > threshold,
           dist_post <= threshold,
           plu == pl) %>% 
    select(GEOID, id = id_post, date = date_post, dist = dist_post, year, turnout,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           turnout_16, turnout_18, turnout_14, turnout_pre) %>% 
    mutate(treated = F,
           d2 = as.integer(date - as.Date("2016-11-08")))
) %>% 
  mutate(year = as.integer(year),
         t16 = year == 1)


full_treat2 <- full_treat2[complete.cases(select(full_treat2,
                                               latino, nh_white, asian, nh_black, median_income, median_age,
                                               pop_dens, turnout_pre)), ]
