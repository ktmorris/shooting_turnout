# ## read in registration deadlines (from ballotpedia)
# reg_deadlines <- fread("raw_data/reg_deadlines.csv") %>%
#   mutate(reg_deadline = as.Date(reg_deadline, "%m/%d/%Y"))
# 
# reg_deadlines <- left_join(reg_deadlines,
#                            fips_codes %>%
#                              select(state = state_name,
#                                     state_code) %>%
#                              distinct()) %>%
#   select(state_code, reg_deadline) %>%
#   mutate(reg_deadline = as.Date(reg_deadline))
# 
# ## read in all the killings, keeping well-coded ones
# full_set <- readRDS("temp/geocoded_shootings.rds") %>%
#   ungroup() %>%
#   mutate(id2 = row_number(),
#          score = ifelse(is.na(score), 100, as.numeric(score))) %>%
#   filter(score > 95)
# 
# race <- full_set %>%
#   select(id = id2, race)
# 
# ## create function to find closest killing on any given day
# find_closest <- function(bg_data_f, centroids_f, d){
#   d = as.Date(d)
#   
#   ## keep killings occuring on that day
#   sites <- filter(full_set,
#                   date == d) %>%
#     mutate(id = row_number())
#   
#   if(nrow(sites) > 0){ #if there were any killings on that day, do the following:
#     ## create tree of those killings
#     tree <- createTree(coordinates(select(sites, x = longitude, y = latitude)))
#     
#     ## find closest killing to each point in centroids_f
#     inds <- knnLookup(tree , newdat = coordinates(centroids_f), k = 1)
#     
#     ## combine killing info and BG info
#     bg_data_f <- left_join(cbind(bg_data_f, inds),
#                            select(sites, id, longitude, latitude, date, id2),
#                            by = c("inds" = "id"))
#     
#     ## calculate distance between BG and killing
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
# ## loop over every state
# for(s in unique(filter(fips_codes, state_code <= 56)$state_code)){
#   print(s)
#   if(!(file.exists(paste0("temp/bgs_", s, ".rds")))){
#     
#     ## pull BG shapefiles using tigris package
#     bgs <- block_groups(state = s, class = "sp")
#     
#     centroids <- SpatialPoints(
#       data.table(x = as.numeric(bgs@data$INTPTLON), y = as.numeric(bgs@data$INTPTLAT))
#     )
#     
#     
#     bg_data <- bgs@data %>%
#       mutate_at(vars(INTPTLON, INTPTLAT), as.numeric)
#     
#     #########################################
#     ## loop over every day between Jan 1, 2020, and Election day
#     ## only retain the info if within 0.5 miles to cut down on number of observations
#     tot <- rbindlist(lapply(seq(as.Date("2020-01-01"), as.Date("2020-11-03"), by="days"), function(d){
#       l <- find_closest(bg_data, centroids, d) %>%
#         filter(dist < 0.5)
#     }))
#     
#     ## for each state, save a table with 1 observation for each BG for each day with closest killing < 0.5
#     saveRDS(tot, paste0("temp/bgs_", s, "_2020.rds"))
#   }
# }
# 
# files <- list.files(path = "temp/", pattern = "^bgs_[0-9][0-9]_2020.rds", full.names = T)
# 
# all_bgs <- rbindlist(lapply(files, readRDS)) %>%
#   mutate(state_code = substring(GEOID, 1, 2))
# 
# all_bgs <- left_join(all_bgs, reg_deadlines)
# 
# all_bgs <- all_bgs %>%
#   ## create week of year relative to registration deadline
#   mutate(week = ceiling(as.integer((date - reg_deadline)) / 7)) %>%
#   ## keep 26 weeks ending with election day
#   filter(week >= -25,
#          week <= 0) %>%
#   ## collapse down to the week level--so first week in whihc BG was treated
#   select(GEOID, week, id) %>%
#   group_by(GEOID, week) %>%
#   filter(row_number() == 1) %>%
#   ## create treatment dummy, make week numbers positive
#   mutate(t = 1,
#          week = week + 26)
# 
# ## merge in info about race of victim
# all_bgs <- left_join(all_bgs, race)
# 
# ######################################
# ## return to the L2 data in a SQL database.
# db <- dbConnect(SQLite(), "D:/national_file_post20.db")
# 
# ## loop over each state, excluding ND (no registrations) and NH (no reg dates in VF)
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
#       ## pull in registration date and geolocation
#       v <- dbGetQuery(db, paste0("select Voters_FIPS,
#                         Residence_Addresses_CensusTract,
#                         Residence_Addresses_CensusBlockGroup,
#                         Voters_OfficialRegDate
#                                 from [", t, "]")) %>%
#         mutate(reg_date = as.Date(Voters_OfficialRegDate, "%m/%d/%Y")) %>%
#         ## keep only the voters registered between Janu 1 and election day
#         filter(reg_date >= "2020-01-01",
#                reg_date <= "2020-11-03") %>%
#         ## create GEOID
#         mutate(GEOID = paste0(scode, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
#                               str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
#                               Residence_Addresses_CensusBlockGroup)) %>%
#         group_by(GEOID, reg_date) %>%
#         ##tally registrations by day by BG
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
#   mutate(state_code = substring(GEOID, 1, 2))
# ## merge in registration deadlines
# regs <- inner_join(regs, reg_deadlines)
# 
# regs <- regs %>%
#   ## calcualte week relative to election day
#   mutate(week = ceiling(as.integer((reg_date - reg_deadline)) / 7)) %>%
#   #sum weekly registrations by block group
#   group_by(week, GEOID) %>%
#   summarize(n = sum(n)) %>%
#   filter(!grepl("NA", GEOID)) %>%
#   filter(week >= -25,
#          week <= 0) %>%
#   mutate(week = week + 26)
# 
# ## next few lines fill out / balance panel for block groups that may have had no regs in a given week
# l <- data.frame(GEOID = rep(unique(regs$GEOID), length(unique(regs$week))),
#                 week = sort(rep(unique(regs$week), length(unique(regs$GEOID)))))
# 
# full <- left_join(l, regs)
# 
# ## if this is week and missing for some reason, set to 0
# full <- full %>%
#   mutate(n = ifelse(is.na(n), 0, n))
# 
# full$state <- substring(full$GEOID, 1, 2)
# 
# ## read in census info, merge in
# census <- readRDS("../regular_data/census_bgs_19.rds")
# 
# full <- left_join(full, census)
# 
# full$state <- as.numeric(full$state)
# 
# ## create integer group numbers for twfe
# full <- full %>%
#   group_by(GEOID) %>%
#   mutate(group_id = cur_group_id())
# 
# ##merge registration data with the first-week-treated-by-killing table
# full <- left_join(full, all_bgs %>%
#                     group_by(GEOID) %>%
#                     filter(week == min(week)) %>%
#                     rename(tw = week)) %>%
#   ## for never-treated BGs, make tw ('treatment week') 0
#   mutate(tw = ifelse(is.na(tw), 0, tw),
#          ## treatment dummy = 0 for never treated
#          t = ifelse(is.na(t), 0, t))
# 
# full$state <- as.integer(substring(full$GEOID, 1, 2))
# 
# cleanup("full")
# 
# ## keep only the covariates we want
# full <- select(full, nh_white, nh_black, asian, latino, median_age,
#                median_income, pop_dens, state, some_college, state,
#                week, GEOID, t, n, tw, group_id, id, population, race)
# 
# full$state <- as.factor(full$state)
# 
# ## create give all never-treated BGs a different 'killing id'
# ## necessary because we cluster standard errors at this level
# ## never treated BGs shouldnt be clustered together
# full$id <- ifelse(is.na(full$id), 9999999, full$id)
# 
# full <- full %>%
#   group_by(group_id) %>%
#   mutate(id = min(id),
#          id = ifelse(tw == 0, 7000 + group_id, id))
# 
# full <- full[complete.cases(select(full, -race)), ]
# 
# 
# saveRDS(full, "temp/balanced_registration_data.rds")

full <- readRDS("temp/balanced_registration_data.rds")


#######################################################
# run difference in difference / twfe model
out <- att_gt(yname = "n",
              gname = "tw",
              idname = "group_id",
              tname = "week",
              control_group = c("nevertreated"),
              xformla = ~median_income + nh_white + nh_black + asian + latino +
                median_age + median_income + pop_dens + some_college,
              data = full,
              clustervars = c("group_id", "id"),
              alp = 0.05
)

saveRDS(out, "temp/did_out.rds")

out <- readRDS("temp/did_out.rds")

agg.ct <- aggte(out, type = "calendar")
agg.dy <- aggte(out, type = "dynamic")
agg.gr <- aggte(out, type = "group")

save(agg.ct, agg.dy, agg.gr, file = "temp/aggregates_did.rdata")

###########################################

load("temp/aggregates_did.rdata")

#create figure 6A

p <- ggdid(agg.dy, title = " ", xgap = 4, ylim = c(-5, 5)) +
  theme_bc(base_family = "LM Roman 10",
           legend.position = "bottom") +
  ggtitle(NULL) +
  labs(y = "Weekly Registration Count",
       x = "Weeks Since Treatment") +
  scale_color_manual(values = c("black", "red"))
p
p[["layers"]][[2]][["geom_params"]]$width <- 0
p[["data"]][["post"]] <- factor(ifelse(p[["data"]][["post"]] == 0,
                                       "Pre-Treatment",
                                       "Post-Treatment"),
                                levels = c("Pre-Treatment",
                                           "Post-Treatment"))
p[["layers"]][[2]][["mapping"]]["linetype"] <- p[["layers"]][[2]][["mapping"]][1]
p[["layers"]][[1]][["mapping"]]["shape"] <- p[["layers"]][[2]][["mapping"]][1]
p <- p +
  scale_shape_manual(values = c(17, 16)) +
  scale_linetype_manual(values = c("dashed", "solid"))
p
####
#create figure 6B
p2 <- ggdid(agg.ct, title = " ", ylim = c(-5, 5)) +
  theme_bc(base_family = "LM Roman 10",
           legend.position = "bottom") +
  ggtitle(NULL) +
  labs(y = "Weekly Registration Count",
       x = "Weeks Before Deadline") +
  scale_color_manual(values = c("red")) +
  scale_x_continuous(labels = -1*seq(-25, -1, 2), breaks = seq(2, 26, 2))
p2
p2[["layers"]][[2]][["geom_params"]]$width <- 0
p2[["data"]][["post"]] <- factor(ifelse(p2[["data"]][["post"]] == 0,
                                       "Pre-Treatment",
                                       "Post-Treatment"),
                                levels = c("Pre-Treatment",
                                           "Post-Treatment"))
p2

saveRDS(p, "temp/did_length.rds")
saveRDS(p2, "temp/did_week.rds")
