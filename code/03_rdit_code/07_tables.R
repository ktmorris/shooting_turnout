


primary <- readRDS("temp/primary_out_data.rds")
primary$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(primary)/3)
primary <- mutate_at(primary, vars(coef, l, u), ~. * -1)

primary <- primary %>% 
  mutate(cint = paste0("[", round(u, digits = 3), ", ", round(l, digits = 3), "]"),
         across(c(coef, pv, se, n), ~round(., digits = 3)),
         n = comma(n, accuracy = 1),
         bw = floor(bw))

primary$`Threshold (Miles)` <- min(primary$p)
primary$Bandwidth <- primary$bw[1]
primary$`Effective Sample Size` <- primary$n[1]

for(i in c(2:nrow(primary))){
  primary$`Threshold (Miles)`[i] <- ifelse(primary$p[i] == primary$p[i-1], "", primary$p[i])
  primary$Bandwidth[i] <- ifelse(primary$p[i] == primary$p[i-1], "", primary$bw[i])
  primary$`Effective Sample Size`[i] <- ifelse(primary$p[i] == primary$p[i-1], "", primary$n[i])
}

primary <- select(primary, `Threshold (Miles)`,
                  `Effective Sample Size`,
                  `Band-\nwidth` = Bandwidth,
                  Type = estimate,
                  `RD Estimate` = coef,
                  `p-value` = pv,
                  `Confidence\nInterval` = cint,
                  `Std.\nError` = se)

saveRDS(primary, "temp/big_table_prim.rds")

########################
out <- readRDS("temp/plu_data_wide_ebal.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out <- mutate_at(out, vars(coef, l, u), ~. * -1) %>% 
  mutate(cint = paste0("[", round(u, digits = 3), ", ", round(l, digits = 3), "]"),
         across(c(coef, pv, se, n), ~round(., digits = 3)),
         n = comma(n, accuracy = 1),
         bwidth = floor(bwidth))
out$`Threshold (Miles)` <- min(out$p)
out$Bandwidth <- out$bwidth[1]
out$`Effective Sample Size` <- out$n[1]

for(i in c(2:nrow(out))){
  out$`Threshold (Miles)`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$p[i])
  out$Bandwidth[i] <- ifelse(out$p[i] == out$p[i-1], "", out$bwidth[i])
  out$`Effective Sample Size`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$n[i])
}

out_b <- select(filter(out, bw == "b"), `Threshold (Miles)`,
                `Effective Sample Size`,
                `Band-\nwidth` = Bandwidth,
                Type = estimate,
                `RD Estimate` = coef,
                `p-value` = pv,
                `Confidence\nInterval` = cint,
                `Std.\nError` = se)

out_h <- select(filter(out, bw == "l"), `Threshold (Miles)`,
                `Effective Sample Size`,
                `Band-\nwidth` = Bandwidth,
                Type = estimate,
                `RD Estimate` = coef,
                `p-value` = pv,
                `Confidence\nInterval` = cint,
                `Std.\nError` = se)

out_w <- select(filter(out, bw == "w"), `Threshold (Miles)`,
                `Effective Sample Size`,
                `Band-\nwidth` = Bandwidth,
                Type = estimate,
                `RD Estimate` = coef,
                `p-value` = pv,
                `Confidence\nInterval` = cint,
                `Std.\nError` = se)

########################
out <- readRDS("temp/victim_data_wide_ebal.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out <- mutate_at(out, vars(coef, l, u), ~. * -1) %>% 
  mutate(cint = paste0("[", round(u, digits = 3), ", ", round(l, digits = 3), "]"),
         across(c(coef, pv, se, n), ~round(., digits = 3)),
         n = comma(n, accuracy = 1),
         bwidth = floor(bwidth))
out$`Threshold (Miles)` <- min(out$p)
out$Bandwidth <- out$bwidth[1]
out$`Effective Sample Size` <- out$n[1]

for(i in c(2:nrow(out))){
  out$`Threshold (Miles)`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$p[i])
  out$Bandwidth[i] <- ifelse(out$p[i] == out$p[i-1], "", out$bwidth[i])
  out$`Effective Sample Size`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$n[i])
}

out_b1 <- select(filter(out, bw == "B"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_h1 <- select(filter(out, bw == "H"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_w1 <- select(filter(out, bw == "W"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

########################
out <- readRDS("temp/victim_data_wide_ebal_model.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out <- mutate_at(out, vars(coef, l, u), ~. * -1) %>% 
  mutate(cint = paste0("[", round(u, digits = 3), ", ", round(l, digits = 3), "]"),
         across(c(coef, pv, se, n), ~round(., digits = 3)),
         n = comma(n, accuracy = 1),
         bwidth = floor(bwidth))
out$`Threshold (Miles)` <- min(out$p)
out$Bandwidth <- out$bwidth[1]
out$`Effective Sample Size` <- out$n[1]

for(i in c(2:nrow(out))){
  out$`Threshold (Miles)`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$p[i])
  out$Bandwidth[i] <- ifelse(out$p[i] == out$p[i-1], "", out$bwidth[i])
  out$`Effective Sample Size`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$n[i])
}

out_b2 <- select(filter(out, bw == "B"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_h2 <- select(filter(out, bw == "H"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_w2 <- select(filter(out, bw == "W"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

###############
out <- readRDS("temp/out_run_trend.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out <- mutate_at(out, vars(coef, l, u), ~. * -1) %>% 
  mutate(cint = paste0("[", round(u, digits = 3), ", ", round(l, digits = 3), "]"),
         across(c(coef, pv, se, n), ~round(., digits = 3)),
         n = comma(n, accuracy = 1),
         bwidth = floor(bwidth)) %>% 
  filter(p <= 1 | p %% 2 == 0)

out <- mutate(out, ord = ifelse(estimate == "Traditional", 1,
                                ifelse(estimate == "Robust", 3, 2)))

out <- out %>% 
  arrange(t, p, ord)

out$`Threshold (Miles)` <- min(out$p)
out$Bandwidth <- out$bw[1]
out$`Effective Sample Size` <- out$n[1]

for(i in c(2:nrow(out))){
  out$`Threshold (Miles)`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$p[i])
  out$Bandwidth[i] <- ifelse(out$p[i] == out$p[i-1], "", out$bwidth[i])
  out$`Effective Sample Size`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$n[i])
}

out_t <- select(filter(out, t == "Trending Killings"), `Threshold (Miles)`,
                `Effective Sample Size`,
                `Band-\nwidth` = Bandwidth,
                Type = estimate,
                `RD Estimate` = coef,
                `p-value` = pv,
                `Confidence\nInterval` = cint,
                `Std.\nError` = se)

out_nt <- select(filter(out, t != "Trending Killings"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

###############################
out <- readRDS("temp/alt_rdds2.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out <- mutate_at(out, vars(coef, l, u), ~. * -1) %>% 
  mutate(cint = paste0("[", round(u, digits = 3), ", ", round(l, digits = 3), "]"),
         across(c(coef, pv, se, n), ~round(., digits = 3)),
         n = comma(n, accuracy = 1)) %>% 
  filter(p <= 1 | p %% 2 == 0)

out <- mutate(out, ord = ifelse(estimate == "Traditional", 1,
                                ifelse(estimate == "Robust", 3, 2)))

out <- out %>% 
  arrange(t, p, ord)

out$`Threshold (Miles)` <- min(out$p)
out$Bandwidth <- out$bw[1]
out$`Effective Sample Size` <- out$n[1]

for(i in c(2:nrow(out))){
  out$`Threshold (Miles)`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$p[i])
  out$Bandwidth[i] <- ifelse(out$p[i] == out$p[i-1], "", out$bwidth[i])
  out$`Effective Sample Size`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$n[i])
}

out_e <- select(filter(out, t == "Entropy Balancing"), `Threshold (Miles)`,
                `Effective Sample Size`,
                `Band-\nwidth` = Bandwidth,
                Type = estimate,
                `RD Estimate` = coef,
                `p-value` = pv,
                `Confidence\nInterval` = cint,
                `Std.\nError` = se)

out_ols <- select(filter(out, t == "OLS"), `Threshold (Miles)`,
                  `Effective Sample Size`,
                  `Band-\nwidth` = Bandwidth,
                  Type = estimate,
                  `RD Estimate` = coef,
                  `p-value` = pv,
                  `Confidence\nInterval` = cint,
                  `Std.\nError` = se)

out_na <- select(filter(out, t == "No Adjustment"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_16 <- select(filter(out, t == "Only 2016"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_20 <- select(filter(out, t == "Only 2020"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_half <- select(filter(out, t == "Half Bandwidth"), `Threshold (Miles)`,
                   `Effective Sample Size`,
                   `Band-\nwidth` = Bandwidth,
                   Type = estimate,
                   `RD Estimate` = coef,
                   `p-value` = pv,
                   `Confidence\nInterval` = cint,
                   `Std.\nError` = se)

out_db <- select(filter(out, t == "Double Bandwidth"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_60 <- select(filter(out, t == "Nonpara, 60"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_90 <- select(filter(out, t == "Nonpara, 90"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

out_180 <- select(filter(out, t == "Nonpara, 180"), `Threshold (Miles)`,
                  `Effective Sample Size`,
                  `Band-\nwidth` = Bandwidth,
                  Type = estimate,
                  `RD Estimate` = coef,
                  `p-value` = pv,
                  `Confidence\nInterval` = cint,
                  `Std.\nError` = se)

out_nar <- select(filter(out, t == "Narrowest"), `Threshold (Miles)`,
                  `Effective Sample Size`,
                  `Band-\nwidth` = Bandwidth,
                  Type = estimate,
                  `RD Estimate` = coef,
                  `p-value` = pv,
                  `Confidence\nInterval` = cint,
                  `Std.\nError` = se)

out_wid <- select(filter(out, t == "Widest"), `Threshold (Miles)`,
                  `Effective Sample Size`,
                  `Band-\nwidth` = Bandwidth,
                  Type = estimate,
                  `RD Estimate` = coef,
                  `p-value` = pv,
                  `Confidence\nInterval` = cint,
                  `Std.\nError` = se)

out_fd <- select(filter(out, t == "First Difference in Turnout"), `Threshold (Miles)`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)
###############################
out <- readRDS("temp/alt_polys_data.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out <- mutate_at(out, vars(coef, l, u), ~. * -1) %>% 
  mutate(cint = paste0("[", round(u, digits = 3), ", ", round(l, digits = 3), "]"),
         across(c(coef, pv, se, n), ~round(., digits = 3)),
         n = comma(n, accuracy = 1),
         bwidth = floor(bwidth))

out$Polynomial <- min(out$p)
out$Bandwidth <- out$bw[1]
out$`Effective Sample Size` <- out$n[1]

for(i in c(2:nrow(out))){
  out$Polynomial[i] <- ifelse(out$p[i] == out$p[i-1], "", out$p[i])
  out$Bandwidth[i] <- ifelse(out$p[i] == out$p[i-1], "", out$bwidth[i])
  out$`Effective Sample Size`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$n[i])
}

out_poly <- select(out, Polynomial,
                   `Effective Sample Size`,
                   `Band-\nwidth` = Bandwidth,
                   Type = estimate,
                   `RD Estimate` = coef,
                   `p-value` = pv,
                   `Confidence\nInterval` = cint,
                   `Std.\nError` = se)
###############################
out <- readRDS("temp/alt_cut_tab_data.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out <- mutate_at(out, vars(coef, l, u), ~. * -1) %>% 
  mutate(cint = paste0("[", round(u, digits = 3), ", ", round(l, digits = 3), "]"),
         across(c(coef, pv, se, n), ~round(., digits = 3)),
         n = comma(n, accuracy = 1),
         bwidth = floor(bwidth))

out$`Cut-Point` <- min(out$p)
out$Bandwidth <- out$bw[1]
out$`Effective Sample Size` <- out$n[1]

for(i in c(2:nrow(out))){
  out$`Cut-Point`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$p[i])
  out$Bandwidth[i] <- ifelse(out$p[i] == out$p[i-1], "", out$bwidth[i])
  out$`Effective Sample Size`[i] <- ifelse(out$p[i] == out$p[i-1], "", out$n[i])
}

out_cp <- select(out, `Cut-Point`,
                 `Effective Sample Size`,
                 `Band-\nwidth` = Bandwidth,
                 Type = estimate,
                 `RD Estimate` = coef,
                 `p-value` = pv,
                 `Confidence\nInterval` = cint,
                 `Std.\nError` = se)

###############################


knitr::kable(primary, booktabs = T, caption = "\\label{tab:big-tab} RDiT Outcomes, Primary Model", linesep = "",
             longtable = T, align=rep('c', ncol(primary)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/primary.tex"))

knitr::kable(out_b, booktabs = T, caption = "\\label{tab:bhood} RDiT Outcomes, Black Neighborhoods", linesep = "",
             longtable = T, align=rep('c', ncol(out_b)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/bhood.tex"))

knitr::kable(out_b1, booktabs = T, caption = "\\label{tab:bvic} RDiT Outcomes, Black Victims", linesep = "",
             longtable = T, align=rep('c', ncol(out_b1)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/bvic.tex"))

knitr::kable(out_b2, booktabs = T, caption = "\\label{tab:bvic_m} RDiT Outcomes, Black Victims\\\\(`Unknowns' Modelled with \\texttt{rethnicity})", linesep = "",
             longtable = T, align=rep('c', ncol(out_b2)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/bvic_m.tex"))

knitr::kable(out_h, booktabs = T, caption = "\\label{tab:hhood} RDiT Outcomes, Latinx Neighborhoods", linesep = "",
             longtable = T, align=rep('c', ncol(out_h)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/hhood.tex"))

knitr::kable(out_h1, booktabs = T, caption = "\\label{tab:hvic} RDiT Outcomes, Latinx Victims", linesep = "",
             longtable = T, align=rep('c', ncol(out_h1)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/lvic.tex"))

knitr::kable(out_h2, booktabs = T, caption = "\\label{tab:hvic_m} RDiT Outcomes, Latinx Victims\\\\(`Unknowns' Modelled with \\texttt{rethnicity})", linesep = "",
             longtable = T, align=rep('c', ncol(out_h2)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/lvic_m.tex"))

knitr::kable(out_w, booktabs = T, caption = "\\label{tab:whood} RDiT Outcomes, White Neighborhoods", linesep = "",
             longtable = T, align=rep('c', ncol(out_w)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/whood.tex"))

knitr::kable(out_w1, booktabs = T, caption = "\\label{tab:wvic} RDiT Outcomes, White Victims", linesep = "",
             longtable = T, align=rep('c', ncol(out_w1)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/wvic.tex"))

knitr::kable(out_w2, booktabs = T, caption = "\\label{tab:wvic_m} RDiT Outcomes, White Victims\\\\(`Unknowns' Modelled with \\texttt{rethnicity})", linesep = "",
             longtable = T, align=rep('c', ncol(out_w2)), format = "latex", escape = F) %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/wvic_m.tex"))

knitr::kable(out_t, booktabs = T, caption = "\\label{tab:trend} RDiT Outcomes, Trending Killings", linesep = "",
             longtable = T, align=rep('c', ncol(out_t)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/trend_tab.tex"))

knitr::kable(out_nt, booktabs = T, caption = "\\label{tab:hvic} RDiT Outcomes, Non-Trending Killings", linesep = "",
             longtable = T, align=rep('c', ncol(out_nt)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/nontrend_tab.tex"))

knitr::kable(out_e, booktabs = T, caption = "\\label{tab:ent} RDiT Outcomes, Entropy Balancing Only", linesep = "",
             longtable = T, align=rep('c', ncol(out_e)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/ent_only_tab.tex"))

knitr::kable(out_ols, booktabs = T, caption = "\\label{tab:ols} RDiT Outcomes, OLS Only", linesep = "",
             longtable = T, align=rep('c', ncol(out_ols)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/ols_only_tab.tex"))

knitr::kable(out_na, booktabs = T, caption = "\\label{tab:ols} RDiT Outcomes, No Adjustments", linesep = "",
             longtable = T, align=rep('c', ncol(out_na)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/na_tab.tex"))

knitr::kable(out_16, booktabs = T, caption = "\\label{tab:o16} RDiT Outcomes, 2016 Only", linesep = "",
             longtable = T, align=rep('c', ncol(out_16)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/16_tab.tex"))

knitr::kable(out_20, booktabs = T, caption = "\\label{tab:o20} RDiT Outcomes, 2020 Only", linesep = "",
             longtable = T, align=rep('c', ncol(out_20)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/20_tab.tex"))

knitr::kable(out_half, booktabs = T, caption = "\\label{tab:db} RDiT Outcomes, Half Bandwidth", linesep = "",
             longtable = T, align=rep('c', ncol(out_db)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/half_tab.tex"))

knitr::kable(out_db, booktabs = T, caption = "\\label{tab:db} RDiT Outcomes, Double Bandwidth", linesep = "",
             longtable = T, align=rep('c', ncol(out_db)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/db_tab.tex"))

knitr::kable(out_60, booktabs = T, caption = "\\label{tab:p60} RDiT Outcomes, 60 Days After ED", linesep = "",
             longtable = T, align=rep('c', ncol(out_60)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/60_tab.tex"))

knitr::kable(out_90, booktabs = T, caption = "\\label{tab:p90} RDiT Outcomes, 90 Days After ED", linesep = "",
             longtable = T, align=rep('c', ncol(out_90)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/90_tab.tex"))

knitr::kable(out_180, booktabs = T, caption = "\\label{tab:p180} RDiT Outcomes, 180 Days After ED", linesep = "",
             longtable = T, align=rep('c', ncol(out_180)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/180_tab.tex"))

knitr::kable(out_nar, booktabs = T, caption = "\\label{tab:naw} RDiT Outcomes, Narrowest Bandwidth", linesep = "",
             longtable = T, align=rep('c', ncol(out_nar)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>%  
  save_kable(paste0("temp/tabs/nar_tab.tex"))

knitr::kable(out_wid, booktabs = T, caption = "\\label{tab:wid} RDiT Outcomes, Widest Bandwidth", linesep = "",
             longtable = T, align=rep('c', ncol(out_wid)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/wid_tab.tex"))

knitr::kable(out_fd, booktabs = T, caption = "\\label{tab:fd} RDiT Outcomes, First Difference in Turnout", linesep = "",
             longtable = T, align=rep('c', ncol(out_fd)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/fd_tab.tex"))

knitr::kable(out_poly, booktabs = T, caption = "\\label{tab:poly} RDiT Outcomes, Different Polynomials (0.3 Mile Threshold)", linesep = "",
             longtable = T, align=rep('c', ncol(out_poly)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/poly_tab.tex"))

knitr::kable(out_cp, booktabs = T, caption = "\\label{tab:cp} RDiT Outcomes, Different Cut-Points (0.3 Mile Threshold)", linesep = "",
             longtable = T, align=rep('c', ncol(out_cp)), format = "latex") %>%
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position", "repeat_header")) %>%
  column_spec(c(1), width = "1.6cm") %>%
  column_spec(c(2), width = "1.9cm") %>% 
  column_spec(c(3, 5), width = "1.5cm") %>%
  column_spec(c(6, 8), width = "1.3cm") %>%
  column_spec(c(4, 7), width = "2.25cm") %>% 
  save_kable(paste0("temp/tabs/cp_tab.tex"))


#########################

for(t in c("wvic_m.tex", "bvic_m.tex", "lvic_m.tex")){
  j <- fread(paste0("temp/tabs/", t), sep = "+", header = F) %>% 
    mutate(n = row_number())
  
  l <- filter(j, grepl("continued", V1)) %>% 
    select(n) %>% 
    pull()
  
  add <- data.frame(V1 = "\\captionsetup{justification=centering}",
                    n = (l - 0.01))
  
  j <- bind_rows(j, add) %>% 
    arrange(n) %>% 
    select(-n) %>% 
    filter(V1 != "")
  
  fwrite(j, paste0("temp/tabs/", t), sep = "+", col.names = F)
}
