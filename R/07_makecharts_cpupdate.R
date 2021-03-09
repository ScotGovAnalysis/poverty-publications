library(tidyverse)
library(ggiraph)
library(ggrepel)
library(scales)

source("R/00_colours.R")
source("R/00_functions.R")

cp_data <- readRDS("data/cp_data.rds")
cp_charts <- list()

## Chart cp1 - Relpov ch -------------------------------------------------------

data <- cp_data$rel

cp_charts$rel <- ggplot(data, aes(x = x,
                                  y = y,
                                  colour = "all",
                                  label = label,
                                  group = "all")) +
  addtargetbars() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   labels = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   expand = c(0.1, 0.1)) +

  geom_line(data = filter(data, !is.na(y)),
            aes(y = y3),
            size = 3,
            lineend = "round",
            colour = SGmix[1],
            alpha = 0.8,
            show.legend = FALSE) +

  geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip),
             size = 3,
             shape = 21,
             colour = SGgreys[1],
             fill = SGgreys[1],
             alpha = 0.4) +

  addlabels(size = 5) +

  scale_colour_manual(values = SGgreys) +
  annotate("text", x = 23.5, y = 0.5,
           label = "Child Poverty (Scotland) Act 2017",
           hjust = 1,
           colour = SGgreys[2],
           size = 5) +
  annotate("text", x = 30, y = 0.45,
           label = "Interim\ntarget\n2023/24",
           hjust = 1,
           colour = SGgreys[2],
           size = 5) +
  annotate("text", x = 37, y = 0.4,
           label = "Final\ntarget\n2030/31",
           hjust = 1,
           colour = SGgreys[2],
           size = 5) +
  addinterimtarget(target = 0.18) +
  addfinaltarget(target = 0.10) +
  addyaxis() +
  addsource()

## Chart cp2 - Abspov ch -------------------------------------------------------

data <- cp_data$abs

cp_charts$abs  <- ggplot(data, aes(x = x, y = y, colour = "all", label = label,
                                   group = "all")) +
  addtargetbars() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   labels = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   expand = c(0.1, 0.1)) +
  geom_line(data = filter(data, !is.na(y)),
            aes(y = y3),
            size = 3,
            lineend = "round",
            colour = SGmix[1],
            alpha = 0.8,
            show.legend = FALSE) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +
  addlabels(size = 5) +
  scale_colour_manual(values = SGgreys) +
  addinterimtarget(target = 0.14) +
  addfinaltarget(target = 0.05) +
  addyaxis() +
  addsource()

## Chart cp3 - Matdep ch -------------------------------------------------------

data <- cp_data$md

cp_charts$md <- ggplot(data, aes(x = x, y = y, colour = "all", label = label,
                                 group = "all")) +

  geom_vline(aes(xintercept = 17),
             colour = SGgreys[4]) +

  addtargetbars() +

  scale_x_discrete(drop = FALSE,
                   breaks = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   labels = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   expand = c(0.1, 0.1)) +
  annotate("text", x = 16.7, y = 0.02,
           label = "Methodology change 2010/11",
           hjust = 1,
           colour = SGgreys[2],
           size = 5) +

  geom_line(data = filter(data, !is.na(y)),
            aes(y = y3),
            size = 3,
            lineend = "round",
            colour = SGmix[1],
            alpha = 0.8,
            show.legend = FALSE) +

  geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +

  addlabels(size = 5) +
  scale_colour_manual(values = SGgreys) +
  addinterimtarget(target = 0.08) +
  addfinaltarget(target = 0.05) +
  addyaxis() +
  addsource()

## Chart cp4 - Pers pov --------------------------------------------------------

data <- cp_data$pers

cp_charts$pers <- ggplot(data, aes(x = x, y = y, colour = "all", label = label,
                                   group = "all")) +
  addtargetbars() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   labels = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   expand = c(0.1, 0.1)) +
  geom_line(data = filter(data, !is.na(y)),
            aes(y = y3),
            size = 3,
            lineend = "round",
            colour = SGmix[1],
            alpha = 0.8,
            show.legend = FALSE) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = tooltip),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +
  addlabels(size = 5) +
  scale_colour_manual(values = SGgreys) +

  addinterimtarget(target = 0.08) +
  addfinaltarget(target = 0.05) +
  addyaxis() +
  labs(caption = "Source: Understanding Society Survey")

saveRDS(cp_charts, "data/cp_charts.rds")
rm(list = ls())

