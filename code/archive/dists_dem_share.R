full_set <- readRDS("temp/geocoded_shootings.rds") %>%
  ungroup() %>%
  mutate(id2 = row_number(),
         score = ifelse(is.na(score), 100, as.numeric(score))) %>%
  filter(score > 95)
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
  dist <- data.table(dist = pointDistance(select(bg_data_f, INTPTLON, INTPTLAT),
                                          select(bg_data_f, longitude, latitude), lonlat = T) * 0.000621371,
                     date = bg_data_f$date,
                     id = bg_data_f$id2)
  return(dist)
}
d <- list.dirs("raw_data/precinct_results_ep")
d <- d[d != "raw_data/precinct_results_ep"]


dat <- rbindlist(lapply(d, function(s){
  state <- substring(s, 30, 31)
  print(state)
  if(!(file.exists(paste0("temp/precinct_dists_", state, ".rds")))){
    pd <- readOGR(paste0("raw_data/precinct_results_ep/", state, "_2020"),
                  paste0(state, "_2020"))
    pd@data$id <- c(1:nrow(pd@data))
    centroids <- gCentroid(pd, byid=TRUE)
    pd_data <- pd@data
    pd_data$INTPTLON <- centroids@coords[,1]
    pd_data$INTPTLAT <- centroids@coords[,2]
    pd_data <- pd_data %>%
      mutate_at(vars(starts_with("G20")), ~ as.integer(gsub(",", "", .))) %>%
      mutate(share_dem = G20PREDBID /
               (rowSums(select(., starts_with("G20PRE")))))
    
    if(ncol(select(pd_data, starts_with("G20H"))) > 0){
      pd_data <- pd_data %>% 
        mutate(house_share_pre = rowSums(select(., starts_with("G20H"))) /
                 (rowSums(select(., starts_with("G20PRE")))))
    }
    
    if(ncol(select(pd_data, starts_with("G20USS"))) > 0){
      pd_data <- pd_data %>% 
        mutate(senate_share_pre = rowSums(select(., starts_with("G20USS"))) /
                 (rowSums(select(., starts_with("G20PRE")))))
    }
    #########################################
    pd_data$dist_pre_20 <- find_closest(pd_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$dist
    pd_data$dist_post_20 <- find_closest(pd_data, centroids_f = centroids, d = "2020-11-03", type = "post")$dist
    pd_data$date_pre_20 <- find_closest(pd_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$date
    pd_data$date_post_20 <- find_closest(pd_data, centroids_f = centroids, d = "2020-11-03", type = "post")$date
    pd_data$id_pre_20 <- find_closest(pd_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$id
    pd_data$id_post_20 <- find_closest(pd_data, centroids_f = centroids, d = "2020-11-03", type = "post")$id
    
    if(ncol(select(pd_data, ends_with("share_pre"))) > 0){
      pd_data <- select(pd_data, id, starts_with("dist_"), starts_with("date_"), starts_with("id_p"),
                        share_dem, ends_with("share_pre"))
    }else{
      pd_data <- select(pd_data, id, starts_with("dist_"), starts_with("date_"), starts_with("id_p"),
                        share_dem)
    }
    saveRDS(pd_data, paste0("temp/precinct_dists_", state, ".rds"))
    return(pd_data)
  }else{
    pd_data <- readRDS(paste0("temp/precinct_dists_", state, ".rds")) %>%
      mutate(state = toupper(state))
  }
}), fill = T) %>%
  rename(precinct_id = id)


#######################################
#######################################
#######################################
#######################################


out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  full_treat <- bind_rows(
    dat %>% 
      filter(dist_pre_20 <= threshold,
             dist_post_20 > threshold) %>% 
      select(precinct_id, id = id_pre_20, date = date_pre_20, dist = dist_pre_20, share_dem,
             senate_share_pre) %>% 
      mutate(treated = T,
             d2 = as.integer(date - as.Date("2020-11-03"))),
    dat %>% 
      filter(dist_pre_20 > threshold,
             dist_post_20 <= threshold) %>% 
      select(precinct_id, id = id_post_20, date = date_post_20, dist = dist_post_20, share_dem,
             senate_share_pre) %>% 
      mutate(treated = F,
             d2 = as.integer(date - as.Date("2020-11-03"))))
  
  # full_treat <- full_treat[complete.cases(select(full_treat,
  #                                                median_income, nh_white, nh_black,
  #                                                median_age, pop_dens, latino, asian)), ]
  # 
  # mb <- ebalance(full_treat$treated,
  #                select(full_treat,
  #                       nh_black, latino, nh_white, asian, median_income, median_age,
  #                       pop_dens))
  # 
  # full_treat <- bind_rows(
  #   filter(full_treat, treated) %>%
  #     mutate(weight = 1),
  #   filter(full_treat, !treated) %>%
  #     mutate(weight = mb$w)
  # )
  # 
  ########################################
  
  l <- rdrobust(y = full_treat$senate_share_pre, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id)
  
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
