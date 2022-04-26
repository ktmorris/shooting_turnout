
################################### read protest data
## start by downloading raw data. commented out so that we can use a constant dataset
# download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv",
#               "raw_data/wapo_shootings.csv")
# 
# download.file("https://mappingpoliceviolence.org/s/MPVDatasetDownload.xlsx",
#               "raw_data/MPVDatasetDownload.xlsx")

# read wapo data 
shootings_wapo <- fread("raw_data/wapo_shootings.csv") %>%
  select(id, date, latitude, longitude, age, race) %>%
  mutate(date = as.Date(date))

# read MPV data
shootings_mapping <- read_xlsx("raw_data/MPVDatasetDownload.xlsx")
colnames(shootings_mapping) <- gsub("[.]", "_", make.unique(make.names(tolower(colnames(shootings_mapping)))))

# rename variables, keep only what's necessary
shootings_mapping <- shootings_mapping %>%
  select(date = date_of_incident__month_day_year_,
         street = street_address_of_incident, age = victim_s_age, race = victim_s_race,
         city, state, zipcode,
         wapo_id__if_included_in_wapo_database_) %>%
  mutate(date = as.Date(date),
         age = as.integer(age))

# combine wapo and MPV datasets
full_set <- bind_rows(
  inner_join(shootings_wapo, select(shootings_mapping,
                                    id = wapo_id__if_included_in_wapo_database_,
                                    street, city, state, zipcode)) %>%
    mutate(type = "both"),
  filter(shootings_wapo, !(id %in% shootings_mapping$wapo_id__if_included_in_wapo_database_)) %>%
    mutate(type = "wapo"),
  filter(shootings_mapping, is.na(wapo_id__if_included_in_wapo_database_),
         !(wapo_id__if_included_in_wapo_database_ %in% shootings_wapo$id)) %>%
    mutate(type = "mapping")
) %>%
  filter(date < "2021-08-30")

### geocode killings with TAMU. NEED TO ADD USERS OWN API KEY!!!
full_set <- left_join(full_set %>%
                        select(-latitude, -longitude, -zipcode),
                      readRDS("temp/geocoded_shootings.rds"))

geos <- filter(full_set, !is.na(latitude))

to_geo <- filter(full_set, is.na(latitude))

hold <- c()

for(i in c(1:nrow(to_geo))){
  te <- filter(to_geo, row_number() == i)

  print(i)
  es = httr::GET("https://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?",
           query = list(streetAddress= te$street,
                        city=te$city,
                        state=te$state,
                        zip=te$zipcode,
                        apiKey="XXX",
                        format="json",
                        version="4.01",
                        census = "true",
                        censusYear = "2010"))

  data = jsonlite::fromJSON(rawToChar(es$content))

  n <- cbind(data$InputAddress,
             data[["OutputGeocodes"]][["OutputGeocode"]],
             data[["OutputGeocodes"]][["CensusValues"]][[1]][["CensusValue1"]]) %>%
    select(latitude = Latitude,
           longitude = Longitude,
           score = MatchScore)

  hold <- rbind(hold, n)
}

full_set <- bind_rows(
  geos,
  cbind(select(to_geo, -latitude, -longitude, -score), hold) %>%
    mutate_at(vars(latitude, longitude), as.double)
)

full_set$id2 <- c(1:nrow(full_set))


saveRDS(full_set, "temp/geocoded_shootings.rds")

##########################
# read coded killings keep only well-coded killings
full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
  filter(score > 95)

## create function to find closest killing on any given day
find_closest <- function(bg_data_f, centroids_f, d){
  d = as.Date(d)
  
  ## keep killings occuring on that day
  sites <- filter(full_set,
                  date == d) %>%
    mutate(id = row_number())
  
  if(nrow(sites) > 0){ #if there were any killings on that day, do the following:
    ## create tree of those killings
    tree <- createTree(coordinates(select(sites, x = longitude, y = latitude)))
    
    ## find closest killing to each point in centroids_f
    inds <- knnLookup(tree , newdat = coordinates(centroids_f), k = 1)
    
    ## combine killing info and BG info
    bg_data_f <- left_join(cbind(bg_data_f, inds),
                           select(sites, id, longitude, latitude, date, id2),
                           by = c("inds" = "id"))
    
    ## calculate distance between BG and killing
    dist <- data.table(dist = pointDistance(select(bg_data_f, INTPTLON, INTPTLAT),
                                            select(bg_data_f, longitude, latitude), lonlat = T) * 0.000621371,
                       date = bg_data_f$date,
                       id = bg_data_f$id2,
                       GEOID = bg_data_f$GEOID) %>% 
      filter(dist <= 20)
  }else{
    dist <- data.table(dist = double(),
                       date = as.Date(character()),
                       id = integer(),
                       GEOID = character())
  }
  
  return(dist)
}

## this will keep one observation for every block group for every shooting within 20 miles over the 2 year-long periods
## loop over every state
for(s in unique(filter(fips_codes, state_code <= 56)$state_code)){
  print(s)
  # if(!(file.exists(paste0("temp/bgs_dists_new_", s, ".rds")))){
    
    ## pull BG shapefiles using tigris package
    bgs <- block_groups(state = s, class = "sp")
    
    centroids <- SpatialPoints(
      data.table(x = as.numeric(bgs@data$INTPTLON), y = as.numeric(bgs@data$INTPTLAT))
    )
    
    
    bg_data <- bgs@data %>%
      mutate_at(vars(INTPTLON, INTPTLAT), as.numeric)
    
    #########################################
    ## loop over every day between Jan 1, 2020, and Election day
    ## only retain the info if within 0.5 miles to cut down on number of observations
    tot <- rbindlist(lapply(seq(as.Date("2020-05-03"), as.Date("2021-05-02"), by="days"), function(d){
      l <- find_closest(bg_data, centroids, d)
    }))
    
    tot2 <- rbindlist(lapply(seq(as.Date("2016-05-08"), as.Date("2017-05-07"), by="days"), function(d){
      l <- find_closest(bg_data, centroids, d)
    }))
    
    tot <- bind_rows(tot, tot2)
    
    ## for each state, save a table with 1 observation for each BG for each day with closest killing < 0.5
    saveRDS(tot, paste0("temp/bgs_dists_new_", s, ".rds"))
  # }
}

files <- list.files(path = "temp/", pattern = "^bgs_dists_new_*", full.names = T)

all_bgs <- rbindlist(lapply(files, readRDS))

saveRDS(all_bgs, "temp/dists_long_new.rds")

####################
####################
####################
####################
####################


full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
  filter(score > 95,
         (date >= "2016-09-08" & date <= "2017-01-08") |
           (date >= "2020-09-03" & date <= "2021-01-03")) %>% 
  select(id2, latitude, longitude)


cents <- rbindlist(lapply(unique(filter(fips_codes, state_code <= 56)$state_code), function(s){
  print(s)

  bgs <- block_groups(state = s, class = "sp")
  
  return(data.table(GEOID = bgs@data$GEOID,
                    long = as.numeric(bgs@data$INTPTLON),
                    lat = as.numeric(bgs@data$INTPTLAT)))
}))


ds <- rbindlist(lapply(c(1:nrow(full_set)), function(k){
  
  cents$dist <- pointDistance(select(cents, long, lat),
                              cbind(full_set$longitude[k], full_set$latitude[k]), lonlat = T) * 0.000621371
  
  return(filter(cents, dist <= 20) %>% 
           mutate(k_id = full_set$id2[k]))
  
}))

cvap20 <- fread("../regular_data/CVAP_2015-2019_ACS_csv_files/BlockGr.csv") %>%
  filter(lntitle == "Total") %>%
  mutate(GEOID = substring(geoid, 8)) %>%
  select(GEOID, cvap = cvap_est)

ds <- left_join(ds, cvap20)

ll <- ds %>% 
  group_by(k_id) %>% 
  summarize(count_5 = sum(dist <= 5),
            pop_5 = sum((cvap * (dist <= 5))),
            count_1 = sum(dist <= 1),
            pop_1 = sum((cvap * (dist <= 1))),
            count_half = sum(dist <= .5),
            pop_half = sum((cvap * (dist <= .5))))

mean(ll$count_half)

l2 <- ds %>% 
  filter(dist <= 1) %>% 
  group_by(GEOID) %>% 
  filter(row_number() == 1)
