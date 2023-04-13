# load ---------------------------------------------------------------------

library(tidyverse)
library(highcharter)

source("R/00_strings.R")
source("R/00_functions.R")
source("R/00_colours.R")

single <- readRDS("data/tables_1yr.rds")
three <- readRDS("data/tables.rds")
persistent <- readRDS("data/persistentpoverty.rds")
CIs <- readRDS("data/indicative_CIs.rds")


# 20/21 bad data is just excluded from each of the 3 datasets below

years <- levels(labels$years$formatted)
periods <- levels(labels$years$periods)
years_exp <- levels(labels$years_exp$formatted)

cpcharts <- list()

# rel pov ----------------------------------------------------------------------

rel1 <- single$relAHC$rates %>%
  filter(Group == "Children") %>%
  select(-Group) %>%
  pivot_longer(cols = years, names_to = "x", values_to = "y")

rel3 <- three$relAHC$rates %>%
  filter(Group == "Children") %>%
  select(-Group) %>%
  pivot_longer(cols = periods, names_to = "periods", values_to = "three")

rel <- rbind(data.frame(periods = NA, three = NA),
             rel3) %>%
  rbind(data.frame(periods = NA, three = NA)) %>%
  cbind(rel1) %>%
  full_join(data.frame(x = years_exp, stringsAsFactors = FALSE), by = "x") %>%
  mutate(y = ifelse(x == "2020/21", NA, y)) %>%

  # add CI data
  left_join(CIs$rel_ch_1yr, by = c("x" = "period")) %>%
  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

# add in CI info for first 3 years
rel$low[1:3] <- rel$low[4]
rel$high[1:3] <- rel$high[4]

# add in CI info for 2021
rel$low[27] <- rel$low[28]
rel$high[27] <- rel$high[28]

cpcharts$rel <- hc_target(rel) %>%
  add_act_bars() %>%
  hc_add_series(type = "arearange",
                data = rel,
                hcaes(x = x, low = low, high = high),
                color = SGmix6_cat[2],
                fillOpacity = 0.3,
                enableMouseTracking = FALSE,
                lineColor = "transparent",
                marker = list(enabled = FALSE),
                visible = TRUE,
                zIndex = -1) %>%
  hc_title(text = "Relative poverty") %>%
  hc_subtitle(text = "Measures low income relative to the rest of society")

# abs pov ----------------------------------------------------------------------
abs1 <- single$absAHC$rates %>%
  filter(Group == "Children") %>%
  select(-Group) %>%
  pivot_longer(cols = years, names_to = "x", values_to = "y")

abs3 <- three$absAHC$rates %>%
  filter(Group == "Children") %>%
  select(-Group) %>%
  pivot_longer(cols = periods, names_to = "periods", values_to = "three")

abs <- rbind(data.frame(periods = NA, three = NA),
             abs3) %>%
  rbind(data.frame(periods = NA, three = NA)) %>%
  cbind(abs1) %>%
  full_join(data.frame(x = years_exp, stringsAsFactors = FALSE), by = "x") %>%
  mutate(y = ifelse(x == "2020/21", NA, y)) %>%

  # add CI data
  left_join(CIs$abs_ch_1yr, by = c("x" = "period")) %>%
  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

# add in CI info for first 3 years
abs$low[1:3] <- abs$low[4]
abs$high[1:3] <- abs$high[4]

# add in CI info for 2021
abs$low[27] <- abs$low[28]
abs$high[27] <- abs$high[28]

cpcharts$abs <- hc_target(abs, targets = c(0.14, 0.05)) %>%
  add_act_bars(annotations = FALSE) %>%
  hc_add_series(type = "arearange",
                data = abs,
                hcaes(x = x, low = low, high = high),
                color = SGmix6_cat[2],
                fillOpacity = 0.3,
                enableMouseTracking = FALSE,
                lineColor = "transparent",
                marker = list(enabled = FALSE),
                visible = TRUE,
                zIndex = -1) %>%
  hc_title(text = "Absolute poverty") %>%
  hc_subtitle(text = "Measures low living standards relative to 2010/11")

# cmd --------------------------------------------------------------------------

cmd1 <- single$cmd$rates %>%
  filter(Measure %in% c("Old measure, after housing costs",
                        "New measure, after housing costs")) %>%
  pivot_longer(cols = years[11:length(years)], names_to = "x", values_to = "y") %>%
  filter(!is.na(y),
         y < 99990) %>%
  arrange(x, desc(Measure)) %>%
  select(-Measure) %>%
  rbind(data.frame(x = "2020/21", y = NA)) %>%
  arrange(x)

cmd3 <- three$cmd$rates %>%
  filter(Measure %in% c("Old measure, after housing costs",
                        "New measure, after housing costs")) %>%
  pivot_longer(cols = periods[11:length(periods)], names_to = "periods", values_to = "three") %>%
  filter(!is.na(three),
         three < 99990) %>%
  arrange(periods) %>%
  select(-Measure)

cmd <- rbind(data.frame(periods = NA, three = NA),
             cmd3[1:5, ]) %>%
  rbind(data.frame(periods = c(NA, NA), three = c(NA, NA))) %>%
  rbind(cmd3[6:nrow(cmd3), ]) %>%
  rbind(data.frame(periods = NA, three = NA)) %>%
  cbind(cmd1) %>%
  full_join(data.frame(x = years_exp, stringsAsFactors = FALSE), by = "x") %>%
  arrange(x, desc(y)) %>%

  # add CI data
  left_join(CIs$dep_ch_1yr, by = c("x" = "period")) %>%
  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

# add in CI info for 2021
cmd$low[28] <- cmd$low[29]
cmd$high[28] <- cmd$high[29]

cpcharts$cmd <- hc_target(cmd, targets = c(0.08, 0.05)) %>%
  hc_tooltip(formatter = JS('function () {
               if (this.x === 16 && this.y > 0.14 && this.point.high > 0) {
               return "<strong>" + this.point.name + " (old)</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "% (" +
                       Highcharts.numberFormat(this.point.low*100, 0) + "-" +
                       Highcharts.numberFormat(this.point.high*100, 0) + "%)";
               } else if (this.x === 16 && this.y < 0.14 && this.point.high > 0) {
               return "<strong>" + this.point.name + " (new)</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "% (" +
                       Highcharts.numberFormat(this.point.low*100, 0) + "-" +
                       Highcharts.numberFormat(this.point.high*100, 0) + "%)";
               } else if (this.point.high > 0) {
               return "<strong>" + this.point.name + "</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "% (" +
                       Highcharts.numberFormat(this.point.low*100, 0) + "-" +
                       Highcharts.numberFormat(this.point.high*100, 0) + "%)";
               } else {
               return "<strong>" + this.point.name + "</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "%"
              }} ')) %>%
  add_act_bars(annotations = FALSE, cmdbreak = TRUE) %>%
  hc_add_series(type = "arearange",
                data = cmd,
                hcaes(x = x, low = low, high = high),
                color = SGmix6_cat[2],
                fillOpacity = 0.3,
                enableMouseTracking = FALSE,
                lineColor = "transparent",
                marker = list(enabled = FALSE),
                visible = TRUE,
                zIndex = -1) %>%
  hc_title(text = "Low income and material deprivation") %>%
  hc_subtitle(text = "Measures if unable to afford basic necessities") %>%
  hc_xAxis(plotBands = list(from = 26,
                            to = 28,
                            color = SGlightgrey,
                            borderColor = SGmidgrey,
                            borderWidth = 2,
                            label = list(
                              text = "Responses<br>affected by<br>the pandemic",
                              rotation = 0,
                              x = -10,
                              align = "left",
                              textAlign = "right",
                              style = list(fontSize = "smaller")))) %>%

  # mark covid estimates
  hc_plotOptions(
    scatter = list(
      zoneAxis = "x",
      zones = list(
        list(value = 26),
        list(value = 28,
             color = SGdarkgrey)
      )
      )
    )


# pers pov ---------------------------------------------------------------------

per <- persistent %>%
  filter(group == "ch", nation == "Scotland", housingcosts == "AHC") %>%
  select(period, value) %>%
  rename(y = value) %>%
  mutate(x = str_sub(period, -2L, -1L),
         x = str_c(x, as.numeric(x) + 1),
         x = factor(x, levels = labels$years_exp$years,
                    labels = labels$years_exp$formatted,
                    ordered = TRUE),
         x = as.character(x),
         three = get3yrcentavg(y)) %>%
  full_join(data.frame(x = years_exp, stringsAsFactors = FALSE), by = "x") %>%
  arrange(x, desc(y))

cpcharts$per <- hc_target(per, targets = c(0.08, 0.05)) %>%
  hc_tooltip(formatter = JS('function () {
              if (this.x === 29) {
              return "<strong>2023/24</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "%";
              } else if (this.x === 36) {
              return "<strong>2030/31</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "%";
              } else {
               return "<strong>" + this.point.period + "</strong>: " +
                       Highcharts.numberFormat(this.y*100, 0) + "%";
                              }
                            } ')) %>%
  add_act_bars(annotations = FALSE) %>%
  hc_credits(text = "Source: Understanding Society Survey",
             href = "download.html") %>%
  hc_title(text = "Persistent poverty" ) %>%
  hc_subtitle(text = "Measures if in poverty for several years" )

# save all ---------------------------------------------------------------------

saveRDS(cpcharts, "data/cpcharts.rds")
rm(list = ls())

cat("Child poverty update charts created", fill = TRUE)
