db <- dbConnect(SQLite(), "D:/national_file_post18.db")
dbh <- dbConnect(SQLite(), "D:/national_file_post18_history.db")

tabs <- dbListTables(db)
tabs <- tabs[tabs != "CA_032020"]

for(f in tabs){
  print(f)
  scode <- filter(fips_codes, state == f)[1, "state_code"]
  if(!file.exists(paste0("temp/18_ballots_by_bg_", f, ".rds"))){
    k <- dbGetQuery(db, paste0("select LALVOTERID,
                                         Residence_Addresses_Latitude,
                                         Residence_Addresses_Longitude from [", f, "]")) %>% 
      mutate(across(starts_with("Residence"), as.numeric)) %>% 
      filter(!is.na(Residence_Addresses_Longitude))
    
    bgs <- block_groups(state = f, year = 2020, class = "sp")
    
    pings  <- SpatialPoints(k[,c('Residence_Addresses_Longitude','Residence_Addresses_Latitude')],
                            proj4string = bgs@proj4string)
    
    k$GEOID <- over(pings, bgs)$GEOID
    
    k <- select(k, LALVOTERID, GEOID)
    
    h <- dbGetQuery(dbh, paste0("select LALVOTERID, General_2018_11_06 from ", f, "_history_18")) %>% 
      mutate(across(starts_with("General"), ~ ifelse(is.na(.) | . == "", "N", .)))
    
    
    k <- left_join(k, h) %>% 
      group_by(GEOID) %>% 
      summarize(voter_count = n(),
                ballots = sum(General_2018_11_06 != "N"))
    
    saveRDS(k, paste0("temp/18_ballots_by_bg_", f, ".rds"))
  }
}
