# 
# #################################### read protest data
# 
# # download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv",
# #               "raw_data/wapo_shootings.csv")
# 
# # download.file("https://mappingpoliceviolence.org/s/MPVDatasetDownload.xlsx",
# #               "raw_data/MPVDatasetDownload.xlsx")
# 
# shootings_wapo <- fread("raw_data/wapo_shootings.csv") %>%
#   select(id, date, latitude, longitude, age, race) %>%
#   mutate(date = as.Date(date))
# shootings_mapping <- read_xlsx("raw_data/MPVDatasetDownload.xlsx")
# colnames(shootings_mapping) <- clean_names(shootings_mapping)
# shootings_mapping <- shootings_mapping %>%
#   select(date = date_of_incident__month_day_year_,
#          street = street_address_of_incident, age = victim_s_age, race = victim_s_race,
#          city, state, zipcode,
#          wapo_id__if_included_in_wapo_database_) %>%
#   mutate(date = as.Date(date),
#          age = as.integer(age))
# 
# full_set <- bind_rows(
#   inner_join(shootings_wapo, select(shootings_mapping,
#                                     id = wapo_id__if_included_in_wapo_database_,
#                                     street, city, state, zipcode)) %>%
#     mutate(type = "both"),
#   filter(shootings_wapo, !(id %in% shootings_mapping$wapo_id__if_included_in_wapo_database_)) %>%
#     mutate(type = "wapo"),
#   filter(shootings_mapping, is.na(wapo_id__if_included_in_wapo_database_),
#          !(wapo_id__if_included_in_wapo_database_ %in% shootings_wapo$id)) %>%
#     mutate(type = "mapping")
# )
# 
# geos <- filter(full_set, !is.na(latitude))
# 
# to_geo <- filter(full_set, is.na(latitude))
# 
# hold <- c()
# 
# for(i in c(1:nrow(to_geo))){
#   te <- filter(to_geo, row_number() == i)
# 
#   print(i)
#   es = httr::GET("https://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?",
#            query = list(streetAddress= te$street,
#                         city=te$city,
#                         state=te$state,
#                         zip=te$zipcode,
#                         apiKey="34134ae2bb26464991daef81e3d7cfd2",
#                         format="json",
#                         version="4.01",
#                         census = "true",
#                         censusYear = "2010"))
# 
#   data = jsonlite::fromJSON(rawToChar(es$content))
# 
#   n <- cbind(data$InputAddress,
#              data[["OutputGeocodes"]][["OutputGeocode"]],
#              data[["OutputGeocodes"]][["CensusValues"]][[1]][["CensusValue1"]]) %>%
#     select(latitude = Latitude,
#            longitude = Longitude,
#            score = MatchScore)
# 
#   hold <- rbind(hold, n)
# }
# 
# full_set <- bind_rows(
#   geos,
#   cbind(select(to_geo, -latitude, -longitude), hold) %>%
#     mutate_at(vars(latitude, longitude), as.double)
# )
# 
# saveRDS(full_set, "temp/geocoded_shootings.rds")
##########################
full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(id2 = row_number(),
         score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
  filter(score > 95)
## turn protest locations into spatial data

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


for(s in unique(filter(fips_codes, state_code <= 56)$state_code)){
  print(s)
  # if(!(file.exists(paste0("temp/bgs_", s, ".rds")))){
    bgs <- block_groups(state = s, class = "sp", year = 2017)
    
    centroids <- SpatialPoints(
      data.table(x = as.numeric(bgs@data$INTPTLON), y = as.numeric(bgs@data$INTPTLAT))
    )
    
    
    bg_data <- bgs@data %>% 
      mutate_at(vars(INTPTLON, INTPTLAT), as.numeric)
    
    #########################################
    
    bg_data$dist_pre_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "pre")$dist
    bg_data$dist_post_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "post")$dist
    
    bg_data$dist_pre_18 <- find_closest(bg_data, centroids_f = centroids, d = "2018-11-06", type = "pre")$dist
    bg_data$dist_post_18 <- find_closest(bg_data, centroids_f = centroids, d = "2018-11-06", type = "post")$dist
    
    bg_data$dist_pre_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$dist
    bg_data$dist_post_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "post")$dist
    
    bg_data$date_pre_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "pre")$date
    bg_data$date_post_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "post")$date
    
    bg_data$date_pre_18 <- find_closest(bg_data, centroids_f = centroids, d = "2018-11-06", type = "pre")$date
    bg_data$date_post_18 <- find_closest(bg_data, centroids_f = centroids, d = "2018-11-06", type = "post")$date
    
    bg_data$date_pre_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$date
    bg_data$date_post_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "post")$date
    
    bg_data$id_pre_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "pre")$id
    bg_data$id_post_16 <- find_closest(bg_data, centroids_f = centroids, d = "2016-11-08", type = "post")$id
    
    bg_data$id_pre_18 <- find_closest(bg_data, centroids_f = centroids, d = "2018-11-06", type = "pre")$id
    bg_data$id_post_18 <- find_closest(bg_data, centroids_f = centroids, d = "2018-11-06", type = "post")$id
    
    bg_data$id_pre_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "pre")$id
    bg_data$id_post_20 <- find_closest(bg_data, centroids_f = centroids, d = "2020-11-03", type = "post")$id
    
    bg_data <- select(bg_data, GEOID, starts_with("dist_"), starts_with("date_"), starts_with("id_p"))
    
    saveRDS(bg_data, paste0("temp/bgs_", s, ".rds"))
# }
}

files <- list.files(path = "temp/", pattern = "^bgs_[0-9][0-9].rds", full.names = T)

all_bgs <- rbindlist(lapply(files, readRDS))

saveRDS(all_bgs, "temp/all_bgs_dist_shooting.rds")

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

saveRDS(dists, "temp/dists_long.rds")
