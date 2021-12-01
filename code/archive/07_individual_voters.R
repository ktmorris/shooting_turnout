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
                         by = c("V1" = "id"))
  
  dist <- data.table(dist = pointDistance(select(bg_data_f, long, lat),
                                          select(bg_data_f, longitude, latitude), lonlat = T) * 0.000621371,
                     date = bg_data_f$date,
                     id = bg_data_f$id2)
  return(dist)
}

### start by connecting to an (empty, at first) SQL database where we'll dump the voter files
db <- dbConnect(SQLite(), "D:/national_file_post20.db")
tabs <- dbListTables(db)

db_hist <- dbConnect(SQLite(), "D:/national_file_post20_history.db")

all_half <- rbindlist(lapply(tabs, function(s){
  if(!file.exists(paste0("temp/indivs_", s, ".rds"))){
    print(paste0("Processing ", s))
    code_good <- unique(filter(fips_codes, state == s)$state_code)
    tt <- dbGetQuery(db, paste0("select LALVOTERID,
                                         Residence_Addresses_Latitude,
                                         Residence_Addresses_Longitude,
                                         Residence_Addresses_CensusTract,
                                         Residence_Addresses_CensusBlockGroup,
                                         Parties_Description,
                                         Voters_FIPS, Voters_Gender,
                              Voters_Age, EthnicGroups_EthnicGroup1Desc,
                              CommercialData_EstimatedHHIncome from [", s, "]")) %>%
      rename(race = EthnicGroups_EthnicGroup1Desc) %>% 
      mutate(GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                            Residence_Addresses_CensusBlockGroup),
             white = race == "European",
             black = race == "Likely African-American",
             latino = race == "Hispanic and Portuguese",
             asian = race == "East and South Asian",
             dem = Parties_Description == "Democratic",
             rep = Parties_Description == "Republican",
             male = Voters_Gender == "M") %>%
      select(-Residence_Addresses_CensusTract,
             -Residence_Addresses_CensusBlockGroup,
             -Voters_FIPS, -race, -Voters_Gender, -Parties_Description) %>% 
      rename(income = CommercialData_EstimatedHHIncome,
             lat = Residence_Addresses_Latitude,
             long = Residence_Addresses_Longitude)
    
    tt <- cSplit(tt, "income", sep = "-", type.convert = F)
    
    tt <- tt %>% 
      mutate(income_1 = as.integer(gsub("[$]|[+]", "", income_1)),
             income_2 = as.integer(income_2),
             income = ifelse(income_1 == 250000, 250000,
                             (income_1 + income_2) / 2)) %>% 
      select(-income_1, -income_2)
    
    
    hist <- dbGetQuery(db_hist, paste0("select LALVOTERID,
                                     General_2020_11_03, General_2018_11_06, General_2016_11_08 from ", s, "_history_20")) %>% 
      mutate_at(vars(starts_with("General")), ~.=="Y")
    
    tt <- left_join(tt, hist) %>% 
      select(-LALVOTERID)
    
    tt <- filter(tt, !is.na(lat), !is.na(long))
    
    centroids <- SpatialPoints(
      data.table(x = as.numeric(tt$long), y = as.numeric(tt$lat))
    )
    
    tt <- cbind(tt,
                find_closest(tt, centroids_f = centroids, d = "2020-11-03", type = "pre") %>% 
                  rename_all(~ paste0(., "_pre")))
    
    tt <- cbind(tt,
                find_closest(tt, centroids_f = centroids, d = "2020-11-03", type = "post") %>% 
                  rename_all(~ paste0(., "_post"))) %>% 
      filter(dist_pre <= 1 | dist_post <= 1)
    
    saveRDS(tt, paste0("temp/indivs_", s, ".rds"))
  }
  return(readRDS(paste0("temp/indivs_", s, ".rds")))
}))

census_data <- readRDS("../regular_data/census_bgs_19.rds") %>% 
  select(GEOID, median_income, median_age, pop_dens)

all_half <- left_join(all_half, census_data)

##########################################
threshold <- 0.5
full_treat <- bind_rows(
  all_half %>% 
    filter(dist_pre <= threshold,
           dist_post > threshold) %>% 
    select(GEOID, id = id_pre, date = date_pre, dist = dist_pre, dem,
           rep, white, black, latino, asian, male,
           General_2020_11_03,
           General_2018_11_06, median_income, pop_dens, Voters_Age) %>% 
    mutate(treated = T,
           d2 = as.integer(date - as.Date("2020-11-03"))),
  all_half %>% 
    filter(dist_pre > threshold,
           dist_post <= threshold) %>% 
    select(GEOID, id = id_post, date = date_post, dist = dist_post, dem,
           rep, white, black, latino, asian, male,
           General_2020_11_03,
           General_2018_11_06, median_income, pop_dens, Voters_Age) %>% 
    mutate(treated = F,
           d2 = as.integer(date - as.Date("2020-11-03"))))

full_treat <- full_treat[complete.cases(select(full_treat,
                                               dem, General_2020_11_03,
                                               rep, white, black, latino, asian, male,
                                               General_2018_11_06, median_income, pop_dens, Voters_Age)), ]
########################
mb <- ebalance(full_treat$treated,
               select(full_treat, dem, rep,
                      white, black, latino, asian, male,
                      General_2018_11_06, median_income, pop_dens, Voters_Age))

full_treat <- bind_rows(
  filter(full_treat, treated) %>%
    mutate(weight = 1),
  filter(full_treat, !treated) %>%
    mutate(weight = mb$w)
)


j <- rdplot(y = full_treat$General_2020_11_03, x = full_treat$d2, p = 1, c = 0,
            weights = full_treat$weight)[["rdplot"]] +
  theme_bc(base_family = "LM Roman 10") +
  ggtitle("Police Killing and Neighborhood Support for Biden, 2016 and 2020", "0.5 Mile Bandwidth") +
  labs(x = "Days Between Police Killing and Election",
       y = "Biden Voteshare") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(-60, -30, 0, 30, 60),
                     labels = c("60 Days\nbefore Election", "30", "Election Day", "30 Days\nAfter Election", "60"))
j


ll <- full_treat %>% 
  filter(rep) %>% 
  group_by(d2) %>% 
  summarize(General_2020_11_03 = weighted.mean(General_2020_11_03, weight))

ggplot(ll, aes(x = d2, y = General_2020_11_03)) + geom_point()

################################
b <- filter(full_treat, black, !male)

f <- rdrobust(y = full_treat$General_2020_11_03, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
       weights = full_treat$weight)
View(f)
