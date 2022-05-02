
full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(score = ifelse(is.na(score), 100, as.numeric(score)),
         name = gsub("[\"]", "", name)) %>% 
  filter(score > 95,
         (date >= "2016-05-08" & date <= "2017-05-07") |
           (date >= "2020-05-03" & date <= "2021-05-02"))

if(file.exists("temp/trends.rds")){
  t <- readRDS("temp/trends.rds") %>% 
    select(id2, pre, post)
  
  full_set <- left_join(full_set, t)
  
  to_code <- filter(full_set, is.na(pre), name != "")
  good <- filter(full_set, !is.na(pre) | name == "")
}else{
  to_code <- full_set
  to_code$pre <- NULL
  to_code$post <- NULL
}


for(i in c(1:nrow(to_code))){
  if(to_code$name[i] != ""){
    print(i)
    t1 <- to_code$date[i]
    t <- paste(t1 - 2, t1 + 2)
    x <- gtrends(to_code$name[i], geo = c("US"), time = t)
    
    
    if(is.null(x[["interest_over_time"]])){
      to_code$pre[i] <- 0
      to_code$post[i] <- 0
    }else{
      x[["interest_over_time"]] <- x[["interest_over_time"]] %>% 
        mutate(hits = ifelse(hits == "<1", "0", hits),
               hits = as.integer(hits))
      to_code$pre[i] <- mean(filter(x[["interest_over_time"]], date < t1)$hits)
      to_code$post[i] <- mean(filter(x[["interest_over_time"]], date >= t1)$hits)
    } 
  }
}
if(file.exists("temp/trends.rds")){
  t <- bind_rows(good, to_code)
}else{
  t <- to_code %>% 
    filter(!is.na(pre))
}

saveRDS(t, "temp/trends.rds")


