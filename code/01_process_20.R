db <- dbConnect(SQLite(), "D:/national_file_post20.db")
tabs <- dbListTables(db)

db_hist <- dbConnect(SQLite(), "D:/national_file_post20_history.db")

bg_ballots <- lapply(tabs, function(s){
  if(file.exists(paste0("temp/ballots_by_bg_", s, ".rds"))){
    print(paste0(s, " Already Processed"))
  }else{
    print(paste0("Processing ", s))
    code_good <- unique(filter(fips_codes, state == s)$state_code)
    tt <- dbGetQuery(db, paste0("select LALVOTERID,
                                         Residence_Addresses_CensusTract,
                                         Residence_Addresses_CensusBlockGroup,
                                         Parties_Description,
                                         Voters_FIPS from [", s, "]")) %>%
      mutate(GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                            Residence_Addresses_CensusBlockGroup)) %>%
      select(LALVOTERID, GEOID, Parties_Description)

    hist <- dbGetQuery(db_hist, paste0("select * from ", s, "_history_20")) %>% 
      mutate(across(starts_with("General"), ~ ifelse(is.na(.) | . == "", "N", .)),
             new = General_2020_11_03 != "N" &
               General_2018_11_06 == "N" & General_2016_11_08 == "N" & General_2014_11_04 == "N" &
               General_2012_11_06 == "N" & General_2010_11_02 == "N")

    tt <- left_join(tt, hist) %>%
      group_by(GEOID) %>%
      summarize(ballots = sum(General_2020_11_03 != "N"),
                voters = n(),
                new = sum(new))

    saveRDS(tt, paste0("temp/ballots_by_bg_", s, ".rds"))
  }
})
