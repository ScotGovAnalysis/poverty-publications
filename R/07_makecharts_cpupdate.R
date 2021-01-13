
library(ggiraph)
library(ggrepel)
library(scales)

source("R/00_colours.R")
source("R/06_prepcharts_cpupdate.R")

cp_charts <- list()

# Theme ----

mytheme <- theme_grey() +
  theme(text = element_text(colour = SGgreys[1], size = 20),

        line = element_line(colour = SGgreys[1],
                            linetype = 1,
                            lineend = 2,
                            size = 0.5),

        plot.title = element_text(hjust = 0, colour = SGgreys[1]),
        plot.subtitle = element_text(hjust = 0, colour = SGgreys[1]),
        plot.caption = element_text(hjust = 1,
                                    margin = margin(t = 20)),

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

## Chart cp1 - Relpov ch ----

data <- cp_data[["rel"]]

interimtarget <- 0.18
finaltarget <- 0.10

cp_charts[["rel"]] <- ggplot(data, aes(x = years,
                                       y = value,
                                       colour = "all",
                                       labels = percent(value, 1),
                                       group = "all")) +
  addtargetbars() +
  geom_line(aes(y = chrate3),
            size = 3,
            lineend = "round",
            colour = SGmix[1],
            alpha = 0.8,
            show.legend = FALSE) +
  geom_point_interactive(aes(tooltip = text,
                             data_id = years),
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
  scale_x_discrete(drop = FALSE,
                   breaks = c("9495", "0607", "1819", "3031"),
                   labels = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   expand = c(0.1, 0.1)) +
  addinterimtarget(y = interimtarget) +
  addfinaltarget(y = finaltarget) +
  addyaxis() +
  addsource()

## Chart cp2 - Abspov ch ----

data <- cp_data[["abs"]]

interimtarget <- 0.14
finaltarget <- 0.05

cp_charts[["abs"]]  <- ggplot(data, aes(x = years,
                                        y = value,
                                        colour = "all",
                                        labels = percent(value, 1),
                                        group = "all")) +
  addtargetbars() +
  geom_line(aes(y = chrate3),
            size = 3,
            lineend = "round",
            colour = SGmix[1],
            alpha = 0.8,
            show.legend = FALSE) +
  geom_point_interactive(aes(tooltip = text,
                             data_id = years),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +
  addlabels(size = 5) +
  scale_colour_manual(values = SGgreys) +
  scale_x_discrete(drop = FALSE,
                   breaks = c("9495", "0607", "1819", "3031"),
                   labels = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   expand = c(0.1, 0.1)) +
  addinterimtarget(y = interimtarget) +
  addfinaltarget(y = finaltarget) +
  addyaxis() +
  addsource()


## Chart cp3 - Matdep ch ----

data <- cp_data[["md"]]

interimtarget <- 0.08
finaltarget <- 0.05

cp_charts[["md"]] <- ggplot(data, aes(x = years,
                                      y = value,
                                      colour = "all",
                                      labels = percent(value, 1),
                                      group = "all")) +

  geom_vline(aes(xintercept = 17),
             colour = SGgreys[4]) +

  addtargetbars() +

  annotate("text", x = 16.7, y = 0.02,
           label = "Methodology change 2010/11",
           hjust = 1,
           colour = SGgreys[2],
           size = 5) +

  geom_line(aes(y = chrate3),
            size = 3,
            lineend = "round",
            colour = SGmix[1],
            alpha = 0.8,
            show.legend = FALSE) +

  geom_point_interactive(aes(tooltip = text,
                             data_id = years),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +

  geom_point_interactive(aes(y = chrate_new,
                             tooltip = text_new,
                             data_id = years),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +

  addlabels(size = 5) +
  scale_colour_manual(values = SGgreys) +

  scale_x_discrete(drop = FALSE,
                   breaks = c("9495", "0607", "1819", "3031"),
                   labels = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   expand = c(0.1, 0.1)) +
  addinterimtarget(y = interimtarget) +
  addfinaltarget(y = finaltarget) +
  addyaxis() +
  addsource()


## Chart cp4 - Pers pov ----

data <- cp_data[["pers"]]

interimtarget <- 0.08
finaltarget <- 0.05

cp_charts[["pers"]] <- ggplot(data, aes(x = years,
                                        y = value,
                                        colour = "all",
                                        labels = percent(value, 1),
                                        group = "all")) +
  addtargetbars() +
  geom_line(aes(y = chrate3),
            size = 3,
            lineend = "round",
            colour = SGmix[1],
            alpha = 0.8,
            show.legend = FALSE) +
  geom_point_interactive(aes(tooltip = text,
                             data_id = years),
                         size = 3,
                         shape = 21,
                         colour = SGgreys[1],
                         fill = SGgreys[1],
                         alpha = 0.4) +
  addlabels(size = 5) +
  scale_colour_manual(values = SGgreys) +
  scale_x_discrete(drop = FALSE,
                   breaks = c("9495", "0607", "1819", "3031"),
                   labels = c("1994/95", "2006/07", "2018/19", "2030/31"),
                   expand = c(0.1, 0.1)) +
  addinterimtarget(y = interimtarget) +
  addfinaltarget(y = finaltarget) +
  addyaxis() +
  labs(caption = "Source: Understanding Society Survey")

remove(data, cp_data, labels, mytheme, SGblue,
       SGblue2, SGblues, SGgreys, SGmix, SGmix2, SGoranges, yearlevels)


