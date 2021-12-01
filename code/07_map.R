
state_map <- states(cb = T) %>% 
  filter(STATEFP <= "56",
         )

state_map <- shift_geometry(state_map)

state_map <- as_Spatial(state_map)

state_map <- spTransform(state_map, "+proj=longlat +datum=NAD83 +no_defs")

state_map <- fortify(state_map)

###################################

full_set <- readRDS("temp/geocoded_shootings.rds") %>% 
  ungroup() %>% 
  mutate(id2 = row_number(),
         score = ifelse(is.na(score), 100, as.numeric(score))) %>% 
  filter(score > 95)

pre_post <- filter(full_set,
                   (date >= as.Date("2020-11-03") - months(2) &
                        date < as.Date("2020-11-03") + months(2))) %>% 
  mutate(year = floor(year(date) / 2) * 2,
         pre = date < "2016-11-08" | (year == 2020 & date < "2020-11-03"))

h <- st_as_sf(pre_post, coords = c("longitude", "latitude"),
                     crs = "+proj=longlat +datum=NAD83 +no_defs")

h <- shift_geometry(h)

h <- as_Spatial(h)

h <- spTransform(h, "+proj=longlat +datum=NAD83 +no_defs")

pre_post$longitude <- h@coords[,1]
pre_post$latitude <- h@coords[,2]

pre_post$pre <- ifelse(pre_post$pre,
                       "Before Election",
                       "After Election")
pre_post$pre <- factor(pre_post$pre, levels = c("Before Election", "After Election"))

t <- ggplot() +
  geom_path(data = filter(state_map,
                          long > -130), mapping = aes(x = long, y = lat, group = group)) +
  geom_point(aes(x = longitude, y = latitude, fill = pre), color = "black", data = pre_post,
             shape = 21, alpha = 0.5) +
  coord_map() +
  theme_bc(base_family = "LM Roman 10") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(fill = "Killing Timing", x = NULL, y = NULL)

t +
  ggtitle("Police Killing within 2 Months of 2020 Election")
saveRDS(t, "temp/map.rds")

ggsave("temp/map.png")
