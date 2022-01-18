pd <- readOGR("raw_data/shp_bdry_votingdistricts", "bdry_votingdistricts")
pd <- spTransform(pd, "+proj=longlat +datum=NAD83 +no_defs")
pd <- subset(pd, MCDNAME == "Minneapolis")
###########################
cens <- readRDS("../regular_data/census_bgs_19.rds")

db <- dbConnect(SQLite(), "D:/national_file_post20.db")

vf <- dbGetQuery(db, "select Voters_FIPS,
                        Residence_Addresses_CensusTract,
                        Residence_Addresses_CensusBlockGroup,
                        Residence_Addresses_Latitude,
                        Residence_Addresses_Longitude
                                from MN") %>% 
  mutate(GEOID = paste0(27, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                        str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                        Residence_Addresses_CensusBlockGroup)) %>% 
  filter(!is.na(Residence_Addresses_Longitude), !is.na(Residence_Addresses_Latitude))

vf <- left_join(vf, cens)

pings  <- SpatialPoints(vf[,c("Residence_Addresses_Longitude", "Residence_Addresses_Latitude")],
                        proj4string = pd@proj4string)

vf$prec <- over(pings, pd)$SHORTLABEL

vf <- filter(vf, !is.na(prec))

vf <- left_join(vf, select(pd@data, prec = SHORTLABEL, VTDID))

demos <- vf %>% 
  group_by(p = prec, VTDID) %>% 
  summarize_at(vars(asian,
                    latino,
                    nh_white,
                    nh_black,
                    median_income,
                    some_college,
                    median_age,
                    pop_dens), mean, na.rm = T) %>% 
  mutate(p = trimws(gsub("W|P|-", "", p)))

demos <- cSplit(demos, "p", sep = " ") %>% 
  mutate(p_2 = paste0(str_pad(gsub("[^0-9.-]", "", p_2), side = "left",
                              width = 2, pad = "0"),
                      gsub('[[:digit:]]+', '', p_2)))

results_2020 <- read_xlsx("raw_data/2020-general-federal-state-results-by-precinct-official.xlsx",
                          sheet = "Precinct-Results") %>% 
  mutate(share_biden = USPRSDFL / USPRSTOTAL)

demos <- left_join(demos, select(results_2020, VTDID, share_biden))

saveRDS(demos, "temp/MSP2_precinct_demos.rds")

########################################
ps <- fread("raw_data/minneapolis_Police_Stop_Data.csv")

pings  <- SpatialPoints(ps[,c('long',
                              'lat')], proj4string = pd@proj4string)
ps$precinct <- over(pings, pd)$SHORTLABEL

ps <- ps %>% 
  mutate(date = as.Date(substring(responseDate, 1, 10), "%Y/%m/%d")) %>% 
  filter(year(date) == 2021,
         date < "2021-11-02")

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
                 demos)

tot <- tot %>% 
  mutate(median_income = median_income / 10000,
         police_stops = log(police_stops),
         pop_dens = log(pop_dens))


m1 <- feols(share_yes ~ dist, tot, cluster = "inds")
summary(m1)
m2 <- feols(share_yes ~ dist +
              police_stops, tot, cluster = "inds")
summary(m2)
m3 <- feols(share_yes ~ dist +
              police_stops +
              nh_black + nh_white + latino + asian +
              median_income + some_college + median_age +
              pop_dens + share_biden, tot, cluster = "inds")
summary(m3)


modelsummary(list(m1, m2, m3),
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("dist" = "Distance to Closest Police Killing",
                          "police_stops" = "Logged Number of Police Stops in 2021",
                          "nh_black" = "Pct. Non-Hispanic Black",
                          "nh_white" = "Pct. Non-Hispanic White",
                          "latino" = "Pct. Latinx",
                          "asian" = "Pct. Asian",
                          "median_income" = "Median Income (dollarsign10,000s)",
                          "some_college" = "Pct. with Some College",
                          "median_age" = "Median Age",
                          "pop_dens" = "Logged Population Density",
                          "share_biden" = "Biden Voteshare in 2020",
                          "(Intercept)" = "Intercept"),
             notes = list("Standard errors clustered by nearest killing."),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "\\label{tab:minn-reg} Support for Abolishing Minneapolis Police Department",
             latex_options = "scale_down",
             output = "temp/minn_reg.tex",
             escape = FALSE)

j <- fread("./temp/minn_reg.tex", header = F, sep = "+") %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1))

write.table(j, "./temp/minn_reg.tex", quote = F, col.names = F,
            row.names = F)

####################

pl <- ggplot(tot, aes(x = dist, y = share_yes)) + geom_point() +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = percent) +
  labs(x = "Distance Between Precinct and Closest Police Killing (2013–2021)",
       y = "Share Supporting Police Abolition") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1))
pl
saveRDS(pl, "temp/minn_scatter.rds")
