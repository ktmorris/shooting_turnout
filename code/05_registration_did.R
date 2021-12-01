library(PanelMatch)
library(did)
library(tidyverse)
library(data.table)

# full_set <- readRDS("temp/geocoded_shootings.rds") %>%
#   ungroup() %>%
#   mutate(id2 = row_number(),
#          score = ifelse(is.na(score), 100, as.numeric(score))) %>%
#   filter(score > 95)
# 
# find_closest <- function(bg_data_f, centroids_f, d){
#   d = as.Date(d)
# 
#   sites <- filter(full_set,
#                   date == d) %>%
#     mutate(id = row_number())
# 
#   if(nrow(sites) > 0){
#     tree <- createTree(coordinates(select(sites, x = longitude, y = latitude)))
# 
#     inds <- knnLookup(tree , newdat = coordinates(centroids_f), k = 1)
# 
#     bg_data_f <- left_join(cbind(bg_data_f, inds),
#                            select(sites, id, longitude, latitude, date, id2),
#                            by = c("inds" = "id"))
# 
#     dist <- data.table(dist = pointDistance(select(bg_data_f, INTPTLON, INTPTLAT),
#                                             select(bg_data_f, longitude, latitude), lonlat = T) * 0.000621371,
#                        date = bg_data_f$date,
#                        id = bg_data_f$id2,
#                        GEOID = bg_data_f$GEOID)
#   }else{
#     dist <- data.table(dist = double(),
#                        date = as.Date(character()),
#                        id = integer(),
#                        GEOID = character())
#   }
# 
#   return(dist)
# }
# 
# for(s in unique(filter(fips_codes, state_code <= 56)$state_code)){
#   print(s)
#   if(!(file.exists(paste0("temp/bgs_", s, ".rds")))){
#   bgs <- block_groups(state = s, class = "sp")
# 
#   centroids <- SpatialPoints(
#     data.table(x = as.numeric(bgs@data$INTPTLON), y = as.numeric(bgs@data$INTPTLAT))
#   )
# 
# 
#   bg_data <- bgs@data %>%
#     mutate_at(vars(INTPTLON, INTPTLAT), as.numeric)
# 
#   #########################################
# 
#   tot <- rbindlist(lapply(seq(as.Date("2020-01-01"), as.Date("2020-10-31"), by="days"), function(d){
#     l <- find_closest(bg_data, centroids, d) %>%
#       filter(dist < 0.5)
#   }))
# 
# 
#   saveRDS(tot, paste0("temp/bgs_", s, "_2020.rds"))
#   }
# }
# 
# files <- list.files(path = "temp/", pattern = "^bgs_[0-9][0-9]_2020.rds", full.names = T)
# 
# all_bgs <- rbindlist(lapply(files, readRDS))
# 
# saveRDS(all_bgs, "temp/all_bgs_dist_shooting_fall.rds")
# 
# cleanup('all_bgs')
# 
# all_bgs <- all_bgs %>%
#   mutate(week = week(date)) %>%
#   select(GEOID, week, id) %>%
#   group_by(GEOID, week) %>%
#   filter(row_number() == 1) %>%
#   mutate(t = 1)
# 
# ######################################
# 
# db <- dbConnect(SQLite(), "D:/national_file_post20.db")
# 
# regs <- rbindlist(lapply(dbListTables(db), function(t){
#   if(!(t %in% c("ND", "NH"))){
#     if(!file.exists(paste0("temp/", t, "_daily_regs_2020.rds"))){
#       print(t)
#       scode <- fips_codes %>%
#         filter(state == t) %>%
#         select(state_code) %>%
#         distinct() %>%
#         pull()
# 
#       v <- dbGetQuery(db, paste0("select Voters_FIPS,
#                         Residence_Addresses_CensusTract,
#                         Residence_Addresses_CensusBlockGroup,
#                         Voters_OfficialRegDate
#                                 from [", t, "]")) %>%
#         mutate(reg_date = as.Date(Voters_OfficialRegDate, "%m/%d/%Y")) %>%
#         filter(reg_date >= "2020-01-01",
#                reg_date <= "2020-10-31") %>%
#         mutate(GEOID = paste0(scode, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
#                               str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
#                               Residence_Addresses_CensusBlockGroup)) %>%
#         group_by(GEOID, reg_date) %>%
#         tally()
#       saveRDS(v, paste0("temp/", t, "_daily_regs_2020.rds"))
#     }else{
#       v <- readRDS(paste0("temp/", t, "_daily_regs_2020.rds"))
#     }
# 
#     return(v)
#   }
# }))
# 
# saveRDS(regs, "temp/daily_national_regs.rds")
# 
# regs <- readRDS("temp/daily_national_regs.rds") %>%
#   group_by(week = week(reg_date), GEOID) %>%
#   summarize(n = sum(n)) %>%
#   filter(!grepl("NA", GEOID))
# 
# 
# l <- data.frame(GEOID = rep(unique(regs$GEOID), length(unique(regs$week))),
#                 week = sort(rep(unique(regs$week), length(unique(regs$GEOID)))))
# 
# full <- left_join(l, regs)
# 
# full <- full %>%
#   mutate(n = ifelse(is.na(n), 0, n))
# 
# full <- left_join(full, all_bgs, by = c("GEOID", "week"))
# 
# full$t <- ifelse(is.na(full$t), 0, full$t)
# 
# full$state <- substring(full$GEOID, 1, 2)
# 
# census <- readRDS("../regular_data/census_bgs_19.rds")
# 
# full <- left_join(full, census)
# 
# full$state <- as.numeric(full$state)
# 
# full <- full %>%
#   group_by(GEOID) %>%
#   mutate(group_id = cur_group_id())
# 
# full <- left_join(full, all_bgs %>%
#                     group_by(GEOID) %>%
#                     summarize(tw = min(week))) %>%
#   mutate(tw = ifelse(is.na(tw), 0, tw))
# 
# full$state <- as.integer(substring(full$GEOID, 1, 2))
# 
# cleanup("full")
# 
# full <- as.data.frame(full)
# 
# saveRDS(full, "temp/balanced_registration_data.rds")

full <- readRDS("temp/balanced_registration_data.rds")

full <- select(full, nh_white, nh_black, asian, latino, median_age,
               median_income, pop_dens, state, some_college, state,
               week, GEOID, t, n, tw, group_id, id)

full$state <- as.factor(full$state)

full$id <- ifelse(is.na(full$id), 9999999, full$id)

full <- full %>% 
  group_by(group_id) %>% 
  mutate(id = min(id))

full <- full[complete.cases(full), ]
# 
# pm_ps <- PanelMatch(lag = 4, time.id = "week", unit.id = "group_id",
#                     treatment = "t", refinement.method = "ps.match",
#                     data = full, match.missing = TRUE,
#                     size.match = 1, qoi = "att", outcome.var = "n",
#                     lead = 0:4, forbid.treatment.reversal = FALSE,
#                     use.diagonal.variance.matrix = TRUE,
#                     covs.formula = ~ nh_white + nh_black + asian + latino + median_age +
#                       median_income + pop_dens + some_college + state,
#                     exact.match.variables = "state")
# PE.results2 <- PanelEstimate(sets = pm_ps, data = full)
# 
# summary(PE.results2)
# 
# 
# pm <- PanelMatch(lag = 1, time.id = "week", unit.id = "group_id",
#                  treatment = "t", refinement.method = "none",
#                  data = full, match.missing = TRUE,
#                  size.match = 5, qoi = "att", outcome.var = "n",
#                  lead = 0:4, forbid.treatment.reversal = FALSE,
#                  use.diagonal.variance.matrix = TRUE)
# PE.results1 <- PanelEstimate(sets = pm, data = full)
# 
# summary(PE.results1)
# 
# save(PE.results1, PE.results2, file = "temp/panel_match.rdata")

out <- att_gt(yname = "n",
              gname = "tw",
              idname = "group_id",
              tname = "week",
              control_group = c("nevertreated", "notyettreated"),
              xformla = ~median_income + nh_white + nh_black + asian + latino +
                median_age + median_income + pop_dens + state*week,
              data = full,
              clustervars = c("group_id", "id")
)

save(out, "temp/did_out.rds")

out <- readRDS("temp/did_out_nocovs.rds")

agg.ct <- aggte(out, type = "calendar")
agg.dy <- aggte(out, type = "dynamic")
agg.gr <- aggte(out, type = "group")

save(agg.ct, agg.dy, agg.gr, file = "temp/aggregates_did_nocovs.rdata")
