
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
# full_set <- readRDS("temp/geocoded_shootings.rds") %>%
#   ungroup() %>%
#   mutate(id2 = row_number(),
#          score = ifelse(is.na(score), 100, as.numeric(score))) %>%
#   filter(score > 95)
# 
# race <- full_set %>%
#   select(id = id2, race)
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
# all_bgs <- rbindlist(lapply(files, readRDS)) %>%
#   mutate(state_code = substring(GEOID, 1, 2))
# 
# all_bgs <- left_join(all_bgs, reg_deadlines)
# 
# all_bgs <- all_bgs %>%
#   mutate(week = ceiling(as.integer((date - reg_deadline)) / 7)) %>%
#   filter(week >= -25,
#          week <= 0) %>%
#   select(GEOID, week, id) %>%
#   group_by(GEOID, week) %>%
#   filter(row_number() == 1) %>%
#   mutate(t = 1,
#          week = week + 26)
# 
# all_bgs <- left_join(all_bgs, race)
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
#   mutate(state_code = substring(GEOID, 1, 2))
# 
# regs <- inner_join(regs, reg_deadlines)
# 
# regs <- regs %>%
#   mutate(week = ceiling(as.integer((reg_date - reg_deadline)) / 7)) %>%
#   group_by(week, GEOID) %>%
#   summarize(n = sum(n)) %>%
#   filter(!grepl("NA", GEOID)) %>%
#   filter(week >= -25,
#          week <= 0) %>%
#   mutate(week = week + 26)
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
#                     filter(week == min(week)) %>% 
#                     rename(tw = week)) %>%
#   mutate(tw = ifelse(is.na(tw), 0, tw),
#          t = ifelse(is.na(t), 0, t))
# 
# full$state <- as.integer(substring(full$GEOID, 1, 2))
# 
# cleanup("full")
# 
# full <- as.data.frame(full) %>%
#   mutate(so = 1 - nh_white - nh_black - latino,
#          plu = ifelse(nh_white > nh_black & nh_white > latino & nh_white > so, "w",
#                       ifelse(nh_black > nh_white & nh_black > latino & nh_black > so, "b",
#                              ifelse(latino > nh_white & latino > nh_black & latino > so, "l", "o")))) %>% 
#   mutate(race = ifelse(race %in% c("B", "Black"), "Black",
#                        ifelse(race %in% c("", "Unknown race"), "U",
#                               ifelse(is.na(race), "C", "Other"))))
# 
# full <- select(full, nh_white, nh_black, asian, latino, median_age,
#                median_income, pop_dens, state, some_college, state,
#                week, GEOID, t, n, tw, group_id, id, population, race)
# 
# full$state <- as.factor(full$state)
# 
# full$id <- ifelse(is.na(full$id), 9999999, full$id)
# 
# full <- full %>%
#   group_by(group_id) %>%
#   mutate(id = min(id),
#          id = ifelse(tw == 0, 7000 + group_id, id))
# 
# full <- full[complete.cases(full), ]
# 
# 
# saveRDS(full, "temp/balanced_registration_data.rds")

full <- readRDS("temp/balanced_registration_data.rds")

#######################################################
out <- att_gt(yname = "n",
              gname = "tw",
              idname = "group_id",
              tname = "week",
              control_group = c("nevertreated"),
              xformla = ~median_income + nh_white + nh_black + asian + latino +
                median_age + median_income + pop_dens,
              data = full %>% 
                filter(race %in% c("C", "Black")),
              clustervars = c("group_id", "id"),
              alp = 0.05
)

saveRDS(out, "temp/did_out_black.rds")

agg.ct <- aggte(out, type = "calendar")
agg.dy <- aggte(out, type = "dynamic")
agg.gr <- aggte(out, type = "group")

save(agg.ct, agg.dy, agg.gr, file = "temp/aggregates_did_black.rdata")

#######################################################
out <- att_gt(yname = "n",
              gname = "tw",
              idname = "group_id",
              tname = "week",
              control_group = c("nevertreated"),
              xformla = ~median_income + nh_white + nh_black + asian + latino +
                median_age + median_income + pop_dens,
              data = full %>% 
                filter(race %in% c("C", "Other")),
              clustervars = c("group_id", "id"),
              alp = 0.05
)

saveRDS(out, "temp/did_out_nonblack.rds")

agg.ct <- aggte(out, type = "calendar")
agg.dy <- aggte(out, type = "dynamic")
agg.gr <- aggte(out, type = "group")

save(agg.ct, agg.dy, agg.gr, file = "temp/aggregates_did_nonblack.rdata")


#######################################################
out <- att_gt(yname = "n",
              gname = "tw",
              idname = "group_id",
              tname = "week",
              control_group = c("nevertreated"),
              xformla = ~median_income + nh_white + nh_black + asian + latino +
                median_age + median_income + pop_dens,
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

############################################
# 
# 
# load("temp/aggregates_did.rdata")
# 
# p <- ggdid(agg.dy, title = " ", xgap = 4, ylim = c(-5, 5)) +
#   theme_bc(base_family = "LM Roman 10",
#            legend.position = "bottom") +
#   ggtitle(NULL) +
#   labs(y = "Weekly Registration Count",
#        x = "Weeks Since Treatment") +
#   scale_color_manual(values = c("black", "red"))
# p
# p[["layers"]][[2]][["geom_params"]]$width <- 0
# p[["data"]][["post"]] <- factor(ifelse(p[["data"]][["post"]] == 0,
#                                        "Pre-Treatment",
#                                        "Post-Treatment"),
#                                 levels = c("Pre-Treatment",
#                                            "Post-Treatment"))
# p[["layers"]][[2]][["mapping"]]["linetype"] <- p[["layers"]][[2]][["mapping"]][1]
# p[["layers"]][[1]][["mapping"]]["shape"] <- p[["layers"]][[2]][["mapping"]][1]
# p <- p +
#   scale_shape_manual(values = c(17, 16)) +
#   scale_linetype_manual(values = c("dashed", "solid"))
# p
# ####
# 
# p2 <- ggdid(agg.ct, title = " ", xgap = 4, ylim = c(-5, 5)) +
#   theme_bc(base_family = "LM Roman 10",
#            legend.position = "bottom") +
#   ggtitle(NULL) +
#   labs(y = "Weekly Registration Count",
#        x = "Week of Year") +
#   scale_color_manual(values = c("red"))
# p2
# p2[["layers"]][[2]][["geom_params"]]$width <- 0
# p2[["data"]][["post"]] <- factor(ifelse(p2[["data"]][["post"]] == 0,
#                                        "Pre-Treatment",
#                                        "Post-Treatment"),
#                                 levels = c("Pre-Treatment",
#                                            "Post-Treatment"))
# p2
# 
# saveRDS(p, "temp/did_length.rds")
# saveRDS(p2, "temp/did_week.rds")
