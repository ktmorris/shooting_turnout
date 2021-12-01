cens <- readRDS("../regular_data/census_bgs_19.rds")

d <- list.dirs("raw_data/precinct_results_ep")
d <- d[d != "raw_data/precinct_results_ep"]

db <- dbConnect(SQLite(), "D:/national_file_post20.db")

precinct_demos <- rbindlist(lapply(d, function(s){
  state <- substring(s, 30, 31)
  su <- toupper(state)
  sc <- unique(filter(fips_codes, state == su)$state_code)
  print(state)
  if(!(file.exists(paste0("temp/", state, "_precinct_demos.rds")))){
    
    pd <- readOGR(paste0("raw_data/precinct_results_ep/", state, "_2020"),
                  paste0(state, "_2020"))
    
    pd@data$id <- c(1:nrow(pd@data))
    pd <- spTransform(pd, "+proj=longlat +datum=NAD83 +no_defs")
    if("GEOID" %in% colnames(pd@data)){
      pd@data <- select(pd@data, -GEOID)
    }
    
    pd_data <- cbind(pd@data, gCentroid(pd, byid = T)@coords) %>% 
      mutate_at(vars(starts_with("G20PRE")), as.numeric) %>% 
      mutate(share_biden = G20PREDBID / rowSums(select(., starts_with("G20PRE")))) %>% 
      select(prec = id, share_biden, long = x, lat = y)
    
    
    vf <- dbGetQuery(db, paste0("select Voters_FIPS,
                        Residence_Addresses_CensusTract,
                        Residence_Addresses_CensusBlockGroup,
                        Residence_Addresses_Latitude,
                        Residence_Addresses_Longitude
                                from [", su, "]")) %>% 
      mutate(GEOID = paste0(sc, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                            Residence_Addresses_CensusBlockGroup)) %>% 
      filter(!is.na(Residence_Addresses_Longitude), !is.na(Residence_Addresses_Latitude))
    
    vf <- left_join(vf, cens)
    
    pings  <- SpatialPoints(vf[,c("Residence_Addresses_Longitude", "Residence_Addresses_Latitude")],
                            proj4string = pd@proj4string)
    
    vf$prec <- over(pings, pd)$id
    
    demos <- vf %>% 
      group_by(prec) %>% 
      summarize_at(vars(asian,
                        latino,
                        nh_white,
                        nh_black,
                        median_income,
                        some_college,
                        median_age,
                        pop_dens), mean, na.rm = T)
    
    int <- inner_join(pd_data, demos)
    saveRDS(int, paste0("temp/", state, "_precinct_demos.rds"))
  }else{
    int <- readRDS(paste0("temp/", state, "_precinct_demos.rds"))
  }
  return(int %>% 
           mutate(state = su))
}))



#####################################################
centroids <- SpatialPoints(
  data.table(x = as.numeric(precinct_demos$long),
             y = as.numeric(precinct_demos$lat)))

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
  
  bg_data_f <- left_join(cbind(bg_data_f, inds) %>% 
                           rename(inds = V1),
                         select(sites, id, longitude, latitude, date, id2),
                         by = c("inds" = "id"))
  
  dist <- data.table(dist = pointDistance(select(bg_data_f, long, lat),
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

precinct_demos$dist_pre <- find_closest(bg_data_f = precinct_demos, centroids_f = centroids, d = "2020-11-03", type = "pre")$dist
precinct_demos$dist_post <- find_closest(precinct_demos, centroids_f = centroids, d = "2020-11-03", type = "post")$dist

precinct_demos$id_pre <- find_closest(precinct_demos, centroids_f = centroids, d = "2020-11-03", type = "pre")$id
precinct_demos$id_post <- find_closest(precinct_demos, centroids_f = centroids, d = "2020-11-03", type = "post")$id

precinct_demos$date_pre <- find_closest(precinct_demos, centroids_f = centroids, d = "2020-11-03", type = "pre")$date
precinct_demos$date_post <- find_closest(precinct_demos, centroids_f = centroids, d = "2020-11-03", type = "post")$date


##############################################

out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  full_treat <- bind_rows(
    precinct_demos %>% 
      filter(dist_pre <= threshold,
             dist_post > threshold) %>% 
      select(prec, id = id_pre, date = date_pre, dist = dist_pre, share_biden,
             median_income, nh_white, nh_black, median_age, pop_dens, latino, asian) %>% 
      mutate(treated = T,
             d2 = as.integer(date - as.Date("2020-11-03"))),
    precinct_demos %>% 
      filter(dist_pre > threshold,
             dist_post <= threshold) %>% 
      select(prec, id = id_post, date = date_post, dist = dist_post, share_biden,
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
  
  l <- rdrobust(y = full_treat$share_biden, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
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
  precinct_demos %>% 
    filter(dist_pre <= threshold,
           dist_post > threshold) %>% 
    select(prec, id = id_pre, date = date_pre, dist = dist_pre, share_biden,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian) %>% 
    mutate(treated = T,
           d2 = as.integer(date - as.Date("2020-11-03"))),
  precinct_demos %>% 
    filter(dist_pre > threshold,
           dist_post <= threshold) %>% 
    select(prec, id = id_post, date = date_post, dist = dist_post, share_biden,
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

l <- rdrobust(y = full_treat$share_biden, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
         weights = full_treat$weight,
         covs = select(full_treat,
                       latino, nh_white, asian,
                       nh_black, median_income, median_age,
                       pop_dens))


j <- rdplot(y = full_treat$share_biden, x = full_treat$d2, p = 1, c = 0,
            weights = full_treat$weight,
            covs = select(full_treat,
                          latino, nh_white, asian,
                          nh_black, median_income, median_age,
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
