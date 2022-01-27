## these functions assume that the raw L2 post-2020 voter files are in SQL databases 
## (one for file, one for history)

db <- dbConnect(SQLite(), "D:/national_file_post20.db")
tabs <- dbListTables(db)

db_hist <- dbConnect(SQLite(), "D:/national_file_post20_history.db")

bg_ballots <- lapply(tabs, function(s){
  if(file.exists(paste0("temp/ballots_by_bg_", s, ".rds"))){
    print(paste0(s, " Already Processed"))
  }else{
    print(paste0("Processing ", s))
    code_good <- unique(filter(fips_codes, state == s)$state_code)
    ## pull data
    tt <- dbGetQuery(db, paste0("select LALVOTERID,
                                         Residence_Addresses_CensusTract,
                                         Residence_Addresses_CensusBlockGroup,
                                         Parties_Description,
                                         Voters_FIPS from [", s, "]")) %>%
      ## take separate county, tract, and block group codes and combine to single GEOID
      mutate(GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                            Residence_Addresses_CensusBlockGroup)) %>%
      select(LALVOTERID, GEOID, Parties_Description)
    
    ## pull voter history data
    hist <- dbGetQuery(db_hist, paste0("select * from ", s, "_history_20")) %>% 
      mutate(across(starts_with("General"), ~ ifelse(is.na(.) | . == "", "N", .)))
    
    ## merge file and history
    tt <- left_join(tt, hist) %>%
      group_by(GEOID) %>%
      summarize(ballots = sum(General_2020_11_03 != "N"),
                voters = n())
    
    saveRDS(tt, paste0("temp/ballots_by_bg_", s, ".rds"))
  }
})
