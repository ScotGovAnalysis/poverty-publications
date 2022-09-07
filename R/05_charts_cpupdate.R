# load packages ----------------------------------------------------------------
library(tidyverse)
library(ggiraph)
library(ggrepel)
library(scales)

source("R/00_strings.R")
source("R/00_functions.R")
source("R/00_colours.R")

single <- readRDS("data/tables_1yr.rds")
three <- readRDS("data/tables.rds")
persistent <- readRDS("data/persistentpoverty.rds")

years <- levels(labels$years$formatted)
years <- years[1:length(years) - 1]

periods <- levels(labels$years$periods)
periods <- periods[1:length(periods) - 1]

years_exp <- levels(labels$years_exp$formatted)

mytheme <- theme_grey() +
  theme(text = element_text(colour = SGgreys[1], size = 14),
        line = element_line(colour = SGgreys[1],
                            linetype = 1,
                            lineend = 2,
                            size = 0.5),

        plot.title = element_text(hjust = 0, colour = SGgreys[1], size = 12),
        plot.caption = element_text(hjust = 1),
        plot.title.position = "plot",

        legend.position = "top",
        legend.title = element_blank(),

        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),

        axis.line.x = element_line(),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_blank(),

        axis.title = element_blank(),
        axis.text.y = element_blank())

theme_set(mytheme)

cpcharts <- list()

# rel pov ----------------------------------------------------------------------
rel1 <- single$relAHC$rates %>%
  filter(Group == "Children") %>%
  select(-Group, -"2020/21") %>%
  pivot_longer(cols = years, names_to = "x", values_to = "y")

rel3 <- three$relAHC$rates %>%
  filter(Group == "Children") %>%
  select(-Group, -"2018-21") %>%
  pivot_longer(cols = periods, names_to = "periods", values_to = "three")

rel <- rbind(data.frame(periods = NA, three = NA),
      rel3) %>%
  rbind(data.frame(periods = NA, three = NA)) %>%
  cbind(rel1) %>%
  mutate(tooltip = case_when(is.na(three) ~ paste0(x, ": ", fmtpct(y)),
                             TRUE ~ paste0(x, ": ", fmtpct(y), "\n",
                                    periods, ": ", fmtpct(three))),
         label = case_when(is.na(three) ~ fmtpct(y)),
         data_id = x) %>%
  full_join(data.frame(x = years_exp, stringsAsFactors = FALSE), by = "x") %>%
  mutate(x = factor(x, ordered = TRUE))

cpcharts$rel <- rel %>%
  ggplot(aes(x = x,
                 y = y,
                 label = label,
             group = "all")) +
  addtargetbars() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994/95", "2007/08", "2019/20", "2030/31"),
                   labels = c("1994/95", "2007/08", "2019/20", "2030/31"),
                   expand = expansion(add = c(5, 5))) +

  geom_line(aes(y = three),
            size = 3,
            lineend = "round",
            colour = SGblue,
            alpha = 0.8,
            show.legend = FALSE) +

  geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +

  geom_text_repel(data = rel %>% filter(x == min(x)),
                  size = 5, nudge_x = -3,
                  segment.size = NA) +

  geom_text_repel(data = rel %>% filter(x == max(years)),
                  size = 5, nudge_x = 3,
                  segment.size = NA) +

  annotate("text", x = 23.5, y = 0.5,
           label = "Child Poverty (Scotland) Act 2017",
           hjust = 1,
           colour = SGgreys[2],
           size = 5) +
  annotate("text", x = 30, y = 0.45,
           label = "Interim\ntarget\n2023/24",
           hjust = 0,
           colour = SGgreys[2],
           size = 5) +
  annotate("text", x = 37, y = 0.4,
           label = "Final\ntarget\n2030/31",
           hjust = 0,
           colour = SGgreys[2],
           size = 5) +
  scale_y_continuous(limits = c(0, 0.53)) +
  addinterimtarget(target = 0.18) +
  addfinaltarget(target = 0.10) +
  addsource()

# abs pov ----------------------------------------------------------------------
abs1 <- single$absAHC$rates %>%
  filter(Group == "Children") %>%
  select(-Group, -"2020/21") %>%
  pivot_longer(cols = years, names_to = "x", values_to = "y")

abs3 <- three$absAHC$rates %>%
  filter(Group == "Children") %>%
  select(-Group, -"2018-21") %>%
  pivot_longer(cols = periods, names_to = "periods", values_to = "three")

abs <- rbind(data.frame(periods = NA, three = NA),
             abs3) %>%
  rbind(data.frame(periods = NA, three = NA)) %>%
  cbind(abs1) %>%
  mutate(tooltip = case_when(is.na(three) ~ paste0(x, ": ", fmtpct(y)),
                             TRUE ~ paste0(x, ": ", fmtpct(y), "\n",
                                           periods, ": ", fmtpct(three))),
         label = case_when(is.na(three) ~ fmtpct(y)),
         data_id = x) %>%
  full_join(data.frame(x = years_exp, stringsAsFactors = FALSE), by = "x") %>%
  mutate(x = factor(x, ordered = TRUE))

cpcharts$abs <- abs %>%
  ggplot(aes(x = x,
             y = y,
             label = label,
             group = "all")) +
  addtargetbars() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994/95", "2007/08", "2019/20", "2030/31"),
                   labels = c("1994/95", "2007/08", "2019/20", "2030/31"),
                   expand = expansion(add = c(5, 5))) +

  geom_line(aes(y = three),
            size = 3,
            lineend = "round",
            colour = SGblue,
            alpha = 0.8,
            show.legend = FALSE) +

  geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +

  geom_text_repel(data = abs %>% filter(x == min(x)),
                  size = 5, nudge_x = -3,
                  segment.size = NA) +

  geom_text_repel(data = abs %>% filter(x == max(years)),
                  size = 5, nudge_x = 3,
                  segment.size = NA) +

  scale_y_continuous(limits = c(0, 0.53)) +
  addinterimtarget(target = 0.14) +
  addfinaltarget(target = 0.05) +
  addsource()

# cmd --------------------------------------------------------------------------

cmd1 <- single$cmd$rates %>%
  filter(Measure %in% c("Old measure, after housing costs",
                        "New measure, after housing costs")) %>%
  select(-"2020/21") %>%
  pivot_longer(cols = years[11:length(years)], names_to = "x", values_to = "y") %>%
  filter(!is.na(y)) %>%
  arrange(x, desc(Measure)) %>%
  select(-Measure)

cmd3 <- three$cmd$rates %>%
  filter(Measure %in% c("Old measure, after housing costs",
                        "New measure, after housing costs")) %>%
  select( -"2018-21") %>%
  pivot_longer(cols = periods[11:length(periods)], names_to = "periods", values_to = "three") %>%
  filter(!is.na(three)) %>%
  arrange(periods) %>%
  select(-Measure)

cmd <- rbind(data.frame(periods = NA, three = NA),
             cmd3[1:5, ]) %>%
  rbind(data.frame(periods = c(NA, NA), three = c(NA, NA))) %>%
  rbind(cmd3[6:nrow(cmd3), ]) %>%
  rbind(data.frame(periods = NA, three = NA)) %>%
  cbind(cmd1) %>%
  mutate(tooltip = case_when(x == "2010/11" & (lag(x) != x) ~ paste0(x, " (old measure): ", fmtpct(y)),
                             x == "2010/11" & (lag(x) == x) ~ paste0(x, " (new measure): ", fmtpct(y)),
                             is.na(three) ~ paste0(x, ": ", fmtpct(y)),
                             TRUE ~ paste0(x, ": ", fmtpct(y), "\n",
                                           periods, ": ", fmtpct(three))),
         label = case_when(is.na(three) & x != "2010/11" ~ fmtpct(y)),
         data_id = x) %>%
  full_join(data.frame(x = years_exp, stringsAsFactors = FALSE), by = "x") %>%
  mutate(x = factor(x, ordered = TRUE)) %>%
  arrange(x, desc(y))

cpcharts$cmd <- cmd %>%
  ggplot(aes(x = x,
             y = y,
             label = label,
             group = "all")) +
  addtargetbars() +
  geom_vline(aes(xintercept = 17), colour = SGgreys[4]) +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994/95", "2007/08", "2019/20", "2030/31"),
                   labels = c("1994/95", "2007/08", "2019/20", "2030/31"),
                   expand = expansion(add = c(5, 5))) +

  geom_line(aes(y = three),
            size = 3,
            lineend = "round",
            colour = SGblue,
            alpha = 0.8,
            show.legend = FALSE) +

  geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +

  geom_text_repel(data = cmd %>% filter(x == "2004/05"),
                  size = 5, nudge_x = -3,
                  segment.size = NA) +

  geom_text_repel(data = cmd %>% filter(x == max(years)),
                  size = 5, nudge_x = 3,
                  segment.size = NA) +
  scale_y_continuous(limits = c(0, 0.53)) +
  addsource() +

  annotate("text", x = 16.7, y = 0.02,
           label = "Methodology change 2010/11",
           hjust = 1,
           colour = SGgreys[2],
           size = 5) +

  addinterimtarget(target = 0.08) +
  addfinaltarget(target = 0.05)

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
         tooltip = str_c(period, ": ", fmtpct(y)),
         label = case_when(period == min(period) ~ fmtpct(y),
                           period == max(period) ~ fmtpct(y)),
         three = get3yrcentavg(y),
         data_id = x)

cpcharts$per <- per %>%
  ggplot(aes(x = x,
             y = y,
             label = label,
             group = "all")) +
  addtargetbars() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994/95", "2007/08", "2019/20", "2030/31"),
                   labels = c("1994/95", "2007/08", "2019/20", "2030/31"),
                   expand = expansion(add = c(5, 5))) +

  geom_line(aes(y = three),
            size = 3,
            lineend = "round",
            colour = SGblue,
            alpha = 0.8,
            show.legend = FALSE) +

  geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +

  geom_text_repel(data = per %>% filter(period == min(period)),
                  size = 5, nudge_x = -3,
                  segment.size = NA) +

  geom_text_repel(data = per %>% filter(period == max(period)),
                  size = 5, nudge_x = 2, nudge_y = 0.02,
                  segment.size = NA) +

  addinterimtarget(target = 0.08) +
  addfinaltarget(target = 0.05) +
  scale_y_continuous(limits = c(0, 0.53)) +
  labs(caption = "Source: Understanding Society Survey")


saveRDS(cpcharts, "data/cpcharts.rds")
rm(list = ls())

