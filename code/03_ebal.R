
full_treat <- readRDS("temp/full_treat_half.rds") %>% 
  filter(year == 2020,
         date >= "2020-10-03",
         date < "2020-12-03")

full_treat <- full_treat %>% 
  mutate(turnout = ballots / cvap,
         turnout = ifelse(turnout > 1, 1, turnout),
         d2 = as.integer(d2),
         year = factor(year),
         state = substring(GEOID, 1, 2))

full_treat <- full_treat[complete.cases(select(full_treat,
                                               latino, nh_white, nh_black, median_income, median_age,
                                               pop_dens)), ]

mb <- ebalance(full_treat$treated,
               select(full_treat,
                      latino, nh_white, nh_black, median_income, median_age,
                      pop_dens))

full_treat <- bind_rows(
  filter(full_treat, treated) %>% 
    mutate(weight = 1),
  filter(full_treat, !treated) %>% 
    mutate(weight = mb$w)
)

m1 <- lm(turnout ~ treated + 
           latino + nh_white + nh_black + median_income + median_age +
         pop_dens + state, full_treat, weights = weight)
summary(m1)