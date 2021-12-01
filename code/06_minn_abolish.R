
pd <- readOGR("raw_data/shp_bdry_votingdistricts", "bdry_votingdistricts")
pd <- spTransform(pd, "+proj=longlat +datum=NAD83 +no_defs")
pd <- subset(pd, MCDNAME == "Minneapolis")
########################################
ps <- fread("raw_data/minneapolis_Police_Stop_Data.csv")

pings  <- SpatialPoints(ps[,c('long',
                                'lat')], proj4string = pd@proj4string)
ps$precinct <- over(pings, pd)$SHORTLABEL

ps <- filter(ps,
             substring(responseDate, 1, 4) == "2021")

ps <- ps %>% 
  mutate(p = trimws(gsub("W|P|-", "", precinct)))

ps <- cSplit(ps, "p", sep = " ") %>% 
  mutate(p_2 = paste0(str_pad(gsub("[^0-9.-]", "", p_2), side = "left",
                              width = 2, pad = "0"),
                      gsub('[[:digit:]]+', '', p_2))) %>% 
  group_by(p_1, p_2) %>% 
  summarize(police_stops = n())

##############################################
pd_data <- cbind(pd@data, gCentroid(pd, byid = T)@coords)

pd_data <- pd_data %>% 
  mutate(p = trimws(gsub("W|P|-", "", SHORTLABEL)))

pd_data <- cSplit(pd_data, "p", sep = " ") %>% 
  mutate(p_2 = paste0(str_pad(gsub("[^0-9.-]", "", p_2), side = "left",
                              width = 2, pad = "0"),
                      gsub('[[:digit:]]+', '', p_2)))

######################

results <- fread("./raw_data/minneapolis_resuts.csv") %>% 
  mutate(p = trimws(gsub("Hennepin: MINNEAPOLIS |W|P|-", "", precinct)))

results <- cSplit(results, "p", sep = " ") %>% 
  mutate(p_2 = paste0(str_pad(gsub("[^0-9.-]", "", p_2), side = "left",
                              width = 2, pad = "0"),
                      gsub('[[:digit:]]+', '', p_2)),
         share_yes = yes / (yes + no)) %>% 
  select(share_yes, p_1, p_2, roll_off, ro_q1)

tot <- inner_join(results, pd_data %>% 
                    select(p_1, p_2, x, y))

tot <- left_join(tot, ps)

##########################

centroids <- SpatialPoints(
  data.table(x = as.numeric(tot$x),
             y = as.numeric(tot$y)))

full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
  filter(score > 95,
         city == "Minneapolis") %>% 
  mutate(id2 = row_number())

tree <- createTree(coordinates(select(full_set, x = longitude, y = latitude)))

inds <- knnLookup(tree , newdat = coordinates(centroids), k = 1)

tot <- left_join(cbind(tot, inds) %>% 
                         rename(inds = V1),
                       select(full_set, longitude, latitude, date, id2),
                       by = c("inds" = "id2"))

tot <- cbind(tot,
             data.table(dist = pointDistance(select(tot, x, y),
                                             select(tot, longitude, latitude), lonlat = T) * 0.000621371))

tot <- left_join(tot,
                 readRDS("temp/mn_precinct_demos.rds"),
                 by = c("x" = "long", "y" = "lat"))


m1 <- feols(share_yes ~ dist +
              police_stops +
              nh_black + nh_white + latino + asian +
              median_income + some_college + median_age +
              pop_dens + share_biden, tot, cluster = "inds")
summary(m1)

ggplot(tot, aes(x = dist, y = share_yes)) + geom_point() +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = percent) +
  labs(x = "Distance Between Precinct and Closest Police Killing (2013--2021)",
       y = "Share Supporting Police Abolition") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1))
