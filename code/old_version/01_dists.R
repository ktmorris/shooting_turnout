##########################
# read coded killings keep only well-coded killings
full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
  filter(score > 95)


#### create a function that identifies the date, location, id, and distance of the 
#### closest killing to a given point in a given time period

find_closest <- function(bg_data_f, centroids_f, d, type){
  d = as.Date(d)
  ## if type is 'pre', keep only killings in the two months before the date provided (ie, election day)
  if(type == "pre"){
    sites <- filter(full_set,
                    date >= d - months(2),
                    date < d) %>% 
      mutate(id = row_number())
  }else{ # if type is post, keep only killings that occur in the 2 months after election day
    sites <- filter(full_set,
                    date >= d,
                    date < d + months(2)) %>% 
      mutate(id = row_number())
  }
  
  ##create a tree of the locations of the retained killings
  tree <- createTree(coordinates(select(sites, x = longitude, y = latitude)))
  
  ## identify closest killing to each centroid
  inds <- knnLookup(tree , newdat = coordinates(centroids_f), k = 1)
  
  ## combine block group data and the killing data
  bg_data_f <- left_join(cbind(bg_data_f, inds),
                         select(sites, id, longitude, latitude, date, id2),
                         by = c("inds" = "id"))
  
  ## calculate the distance between the centroid and the killing
  dist <- data.table(dist = pointDistance(select(bg_data_f, INTPTLON, INTPTLAT),
                                          select(bg_data_f, longitude, latitude), lonlat = T) * 0.000621371,
                     date = bg_data_f$date,
                     id = bg_data_f$id2)
  return(dist)
}

## loop over each state in the country to identify nearest killings pre / post election for each block group

for(s in unique(filter(fips_codes, state_code <= 56)$state_code)){
  print(s)
  ## download block group shapefile data using tigris package 
  bgs <- block_groups(state = s, class = "sp", year = 2017)
  
  ## turn centroids into spatial points
  centroids <- SpatialPoints(
    data.table(x = as.numeric(bgs@data$INTPTLON), y = as.numeric(bgs@data$INTPTLAT))
  )
  
  
  bg_data <- bgs@data %>% 
    mutate_at(vars(INTPTLON, INTPTLAT), as.numeric)
  
  #########################################
  ## run user defined function to find closest killing in 2 months before and after 2016, 2018, 2020 elections
  bg_data$dist_pre_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "pre")$dist
  bg_data$dist_post_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "post")$dist
  
  
  bg_data$dist_pre_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$dist
  bg_data$dist_post_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "post")$dist
  
  bg_data$date_pre_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "pre")$date
  bg_data$date_post_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "post")$date
  
  
  bg_data$date_pre_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$date
  bg_data$date_post_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "post")$date
  
  bg_data$id_pre_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "pre")$id
  bg_data$id_post_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "post")$id
  
  
  bg_data$id_pre_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$id
  bg_data$id_post_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "post")$id
  
  bg_data <- select(bg_data, GEOID, starts_with("dist_"), starts_with("date_"), starts_with("id_p"))
  
  saveRDS(bg_data, paste0("temp/oldxxx_bgs_", s, ".rds"))
}

files <- list.files(path = "temp/", pattern = "^oldxxx_bgs_[0-9][0-9].rds", full.names = T)

all_bgs <- rbindlist(lapply(files, readRDS))

saveRDS(all_bgs, "temp/oldxxx_all_bgs_dist_shooting.rds")

### reshape these into a long format

dists1 <- readRDS("temp/oldxxx_all_bgs_dist_shooting.rds") %>% 
  select(GEOID, starts_with("dist_")) %>% 
  pivot_longer(cols = c(starts_with("dist_")), names_to = "year", names_prefix = "dist_",
               values_to = "dist") %>% 
  separate(year, into = c("type", "year"), sep = "_") %>% 
  pivot_wider(id_cols = c(GEOID, year), names_from = "type", values_from = "dist", names_prefix = "dist_") %>% 
  mutate(year = paste0("20", year))

dists2 <- readRDS("temp/oldxxx_all_bgs_dist_shooting.rds") %>% 
  select(GEOID, starts_with("date_")) %>% 
  pivot_longer(cols = c(starts_with("date_")), names_to = "year", names_prefix = "date_",
               values_to = "dist") %>% 
  separate(year, into = c("type", "year"), sep = "_") %>% 
  pivot_wider(id_cols = c(GEOID, year), names_from = "type", values_from = "dist", names_prefix = "date_") %>% 
  mutate(year = paste0("20", year))

dists3 <- readRDS("temp/oldxxx_all_bgs_dist_shooting.rds") %>% 
  select(GEOID, starts_with("id_")) %>% 
  pivot_longer(cols = c(starts_with("id_")), names_to = "year", names_prefix = "id_",
               values_to = "dist") %>% 
  separate(year, into = c("type", "year"), sep = "_") %>% 
  pivot_wider(id_cols = c(GEOID, year), names_from = "type", values_from = "dist", names_prefix = "id_") %>% 
  mutate(year = paste0("20", year))

dists <- full_join(dists1, full_join(dists2, dists3))

saveRDS(dists, "temp/oldxxx_dists_long.rds")