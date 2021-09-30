## https://www.nytimes.com/interactive/2021/upshot/2020-election-map.html

shp <- readOGR("raw_data/precincts-with-results", "precincts-with-results")

centroids <- gCentroid(shp, byid=TRUE)

dat <- bind_cols(shp@data, as.data.table(centroids@coords))

#####################################################
bg_ps <- rbindlist(lapply(unique(filter(fips_codes, state_code <= 56)$state_code), function(s){
  print(s)
  # if(!(file.exists(paste0("temp/bgs_", s, ".rds")))){
  bgs <- block_groups(state = s, class = "sp", year = 2017)
  
  centroids <- SpatialPoints(
    data.table(x = as.numeric(bgs@data$INTPTLON), y = as.numeric(bgs@data$INTPTLAT)),
    proj4string = shp@proj4string)
  
  bg_dat <- bgs@data %>% 
    select(GEOID)
  bg_dat$precinct <- over(centroids, shp)$GEOID
  
  return(bg_dat)
}))
saveRDS(bg_ps, "temp/precinct_bg_cross.rds")

d2 <- filter(dat, !(GEOID %in% bg_ps$precinct)) %>% 
  mutate(l = NA)

centroids <- SpatialPoints(
  data.table(x = as.numeric(d2$x), y = as.numeric(d2$y)),
  proj4string = bgs@proj4string)

for(s in unique(filter(fips_codes, state_code <= 56)$state_code)){
  bgs <- block_groups(state = s, class = "sp", year = 2017)
  print(s)

  d2$bg <- over(centroids, bgs)$GEOID
  
  d2 <- mutate(d2,
               l = ifelse(is.na(bg), l, bg))
}

all_ps <- bind_rows(filter(bg_ps, !is.na(precinct)),
                    d2 %>% 
                      select(GEOID = l, precinct = GEOID))

census_data <- readRDS("../regular_data/census_bgs_19.rds")

all_ps <- left_join(all_ps, census_data)
all_ps <- all_ps %>% 
  group_by(precinct) %>% 
  summarize_at(vars(median_income, nh_white, nh_black, median_age, pop_dens, latino, asian),
               ~ weighted.mean(., population))

dat <- full_join(dat, all_ps, by = c("GEOID" = "precinct"))

saveRDS(dat, "temp/precinct_demos.rds")
#####################################################
cleanup("dat")
centroids <- SpatialPoints(
  data.table(x = as.numeric(dat$x), y = as.numeric(dat$y)))

find_closest <- function(bg_data_f, centroids_f, d, type){
  d = as.Date(d)
  if(type == "pre"){
    sites <- filter(full_set,
                    date >= d - months(2),
                    date < d) %>% 
      mutate(id = row_number())
  }else{
    sites <- filter(full_set,
                    date >= d,
                    date < d + months(2)) %>% 
      mutate(id = row_number())
  }
  
  tree <- createTree(coordinates(select(sites, x = longitude, y = latitude)))
  
  inds <- knnLookup(tree , newdat = coordinates(centroids_f), k = 1)
  
  bg_data_f <- left_join(cbind(bg_data_f, inds),
                         select(sites, id, longitude, latitude, date, id2),
                         by = c("inds" = "id"))
  
  dist <- data.table(dist = pointDistance(select(bg_data_f, x, y),
                                          select(bg_data_f, longitude, latitude), lonlat = T) * 0.000621371,
                     date = bg_data_f$date,
                     id = bg_data_f$id2)
  return(dist)
}

#####
full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(id2 = row_number(),
         score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
  filter(score > 95)

dat$dist_pre <- find_closest(dat, centroids_f = centroids, d = "2020-11-03", type = "pre")$dist
dat$dist_post <- find_closest(dat, centroids_f = centroids, d = "2020-11-03", type = "post")$dist

dat$id_pre <- find_closest(dat, centroids_f = centroids, d = "2020-11-03", type = "pre")$id
dat$id_post <- find_closest(dat, centroids_f = centroids, d = "2020-11-03", type = "post")$id

dat$date_pre <- find_closest(dat, centroids_f = centroids, d = "2020-11-03", type = "pre")$date
dat$date_post <- find_closest(dat, centroids_f = centroids, d = "2020-11-03", type = "post")$date

dat$dem <- dat$votes_dem / dat$votes_tota

##############################################

out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  full_treat <- bind_rows(
    dat %>% 
      filter(dist_pre <= threshold,
             dist_post > threshold) %>% 
      select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, dem,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian) %>% 
      mutate(treated = T,
             d2 = as.integer(date - as.Date("2020-11-03"))),
    dat %>% 
      filter(dist_pre > threshold,
             dist_post <= threshold) %>% 
      select(GEOID, id = id_post, date = date_post, dist = dist_post, dem,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian) %>% 
      mutate(treated = F,
             d2 = as.integer(date - as.Date("2020-11-03"))))
  
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 median_income, nh_white, nh_black,
                                                 median_age, pop_dens, latino, asian)), ]
  
  mb <- ebalance(full_treat$treated,
                 select(full_treat,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens))
  
  full_treat <- bind_rows(
    filter(full_treat, treated) %>%
      mutate(weight = 1),
    filter(full_treat, !treated) %>%
      mutate(weight = mb$w)
  )
  
  ########################################
  
  l <- rdrobust(y = full_treat$dem, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens))
  
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

saveRDS(different_dists, "temp/different_dists_results.rds")
##########################################
threshold <- 0.5
full_treat <- bind_rows(
  dat %>% 
    filter(dist_pre <= threshold,
           dist_post > threshold) %>% 
    select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, dem,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian) %>% 
    mutate(treated = T,
           d2 = as.integer(date - as.Date("2020-11-03"))),
  dat %>% 
    filter(dist_pre > threshold,
           dist_post <= threshold) %>% 
    select(GEOID, id = id_post, date = date_post, dist = dist_post, dem,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian) %>% 
    mutate(treated = F,
           d2 = as.integer(date - as.Date("2020-11-03"))))

full_treat <- full_treat[complete.cases(select(full_treat,
                                               latino, nh_white, asian,
                                               nh_black, median_income, median_age,
                                               pop_dens)), ]
########################
mb <- ebalance(full_treat$treated,
               select(full_treat,
                      nh_black, latino, nh_white, asian, median_income, median_age,
                      pop_dens))

full_treat <- bind_rows(
  filter(full_treat, treated) %>%
    mutate(weight = 1),
  filter(full_treat, !treated) %>%
    mutate(weight = mb$w)
)


j <- rdplot(y = full_treat$dem, x = full_treat$d2, p = 1, c = 0,
            weights = full_treat$weight,
            covs = select(full_treat,
                          nh_black, latino, nh_white, asian, median_income, median_age,
                          pop_dens))[["rdplot"]] +
  theme_bc(base_family = "LM Roman 10") +
  ggtitle("Police Killing and Neighborhood Support for Biden, 2016 and 2020", "0.5 Mile Bandwidth") +
  labs(x = "Days Between Police Killing and Election",
       y = "Biden Voteshare") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60 Days\nbefore Election", "30", "Election Day", "30 Days\nAfter Election", "60"))
j

saveRDS(j, "temp/rd_plot_results.rds")
