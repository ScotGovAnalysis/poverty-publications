
# Create charts for poverty publication - both, interactive and non-interactive versions

library(tidyverse)
library(labelled)
library(ggrepel)
library(ggiraph)

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

povertychartdata <- readRDS("data/povertychartdata.rds")

povertycharts <- list()

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

# Key trends -------------------------------------------------------------------

# chart0a ----------------------------------------------------------------------
data <- filter(povertychartdata$relpov,
               weight == "gs_newpp",
               key == "After housing costs")
povertycharts$chart0a <- linechart_small(data) + addscales()
saveplot("website/img/chart0a.png")

# chart0b ----
data <- filter(povertychartdata$palma, key == "Before housing costs")
povertycharts$chart0b <- linechart_small(data, yrange = c(0.6, 1.7),
                                         col = SGblue2) + addscales()
saveplot("website/img/chart0b.png")

# chart0c ----------------------------------------------------------------------
data <- filter(povertychartdata$medians,
               key == "Before housing costs")
povertycharts$chart0c <- linechart_small(data, yrange = c(250, 600),
                                         col = SGblue2) + addscales()
saveplot("website/img/chart0c.png")

# Poverty ----------------------------------------------------------------------

## chart01 rel pov pp ----------------------------------------------------------
data <- filter(povertychartdata$relpov,
               weight == "gs_newpp")

povertycharts$chart01 <- linechart(data) +
  scale_y_continuous(limits = c(0.05, 0.35)) +
  addscales() +
  addsource() +
  addlabels() +
  addnames(up = c(-0.04, +0.03))

## chart02 abs pov pp ----------------------------------------------------------
data <- filter(povertychartdata$abspov,
               weight == "gs_newpp")

povertycharts$chart02 <- linechart(data) +
  scale_y_continuous(limits = c(0.13, 0.6)) +
  addnames(up = c(-0.18, +0.035)) +
  addscales() +
  addsource() +
  addlabels()

## chart03 food security pp ----------------------------------------------------
data <- filter(povertychartdata$foodsec, weight == "gs_newpp")

povertycharts$chart03 <- ggplot(data, aes(x = x, y = y, group = key,
                                          fill = key, width = 0.8)) +

  geom_bar_interactive(aes(tooltip = tooltip,
                           data_id = tooltip),
                       position = 'fill',
                       stat = "identity",
                       colour = "white") +
  coord_flip() +
  scale_fill_manual(values = SGmix2,
                    guide = guide_legend(reverse = TRUE)) +

  scale_y_continuous(labels = percent_format(1)) +

  theme(axis.line.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.length = unit(2, "pt"),
        legend.position = "top") +

  addsource()

## chart04 rel pov wa ----------------------------------------------------------
data <- filter(povertychartdata$relpov, weight == "gs_newwa")

povertycharts$chart04 <- linechart(data) +
  scale_y_continuous(limits = c(0.05, 0.35)) +
  addnames(up = c(-0.04, 0.05)) +
  addscales() +
  addsource() +
  addlabels()

## chart05 work pov wa ---------------------------------------------------------
data <- filter(povertychartdata$workpov, weight == "gs_newwa")

povertycharts$chart05 <- linechart(data) +
  scale_y_continuous(limits = c(0.0, 0.3)) +
  addscales() +
  addsource() +
  addlabels() +
  geom_ribbon(data = filter(data, yearn >= 5),
              mapping = aes(ymin = ymax, ymax = y),
              show.legend = FALSE,
              fill = SGblue,
              alpha = 0.2) +
  geom_area(data = filter(data, yearn >= 5),
            aes(y = ymax),
            show.legend = FALSE,
            fill = SGoranges[1],
            alpha = 0.2,
            outline.type = "both",
            colour = SGoranges[1]) +
  geom_point_interactive(data = filter(data, yearn >= 5),
                         aes(x = x, y = ymax,
                             tooltip = tooltip,
                             data_id = tooltip),
                         show.legend = FALSE,
                         size = 6,
                         colour = "white",
                         alpha = 0.01) +
  annotate("text", x = 3.5, y = 0.15, label = "In workless households",
           colour = SGblue, hjust = 0) +
  annotate("text", x = 3.5, y = 0.06, label = "In working households",
           colour = SGoranges[1], hjust = 0)

## chart06 abs pov wa ----------------------------------------------------------
data <- filter(povertychartdata$abspov, weight == "gs_newwa")

povertycharts$chart06 <- linechart(data) +
  scale_y_continuous(limits = c(0.1, 0.57)) +
  addnames(up = c(-0.13, +0.04)) +
  addscales() +
  addsource() +
  addlabels()

## chart07 rel pov pn ----------------------------------------------------------
data <- filter(povertychartdata$relpov, weight == "gs_newpn")

povertycharts$chart07 <- linechart(data) +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  addnames(up = c(-0.11, 0.03)) +
  addscales() +
  addsource() +
  addlabels()

## chart08 abs pov pn ----------------------------------------------------------
data <- filter(povertychartdata$abspov, weight == "gs_newpn")

povertycharts$chart08 <- linechart(data) +
  scale_y_continuous(limits = c(0.11, 0.58)) +
  addnames(up = c(0.035, -0.27)) +
  addscales() +
  addsource() +
  addlabels()

## chart09 dep pn --------------------------------------------------------------
data <- povertychartdata$pndep %>%
  mutate(key = "none")

povertycharts$chart09 <- linechart(data, recession = FALSE) +
  scale_y_continuous(limits = c(0, 0.3)) +
  addscales() +
  addsource() +
  addlabels()

# Child poverty ----------------------------------------------------------------

# chart10 rel pov ch -----------------------------------------------------------
data <- filter(povertychartdata$relpov, weight == "gs_newch")

povertycharts$chart10 <- linechart(data) +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  addscales() +
  addsource() +
  addlabels() +
  addnames(up = c(-0.05, +0.03))

## chart11 work pov ch ---------------------------------------------------------
data <- filter(povertychartdata$workpov, weight == "gs_newch")

povertycharts$chart11 <- linechart(data) +
  scale_y_continuous(limits = c(0.0, 0.44)) +
  addscales() +
  addsource() +
  addlabels() +

  geom_ribbon(data = filter(data, yearn >= 5),
              mapping = aes(ymin = ymax, ymax = y),
              show.legend = FALSE,
              fill = SGblue,
              alpha = 0.2) +
  geom_area(data = filter(data, yearn >= 5),
            aes(y = ymax),
            show.legend = FALSE,
            fill = SGoranges[1],
            alpha = 0.2,
            outline.type = "both",
            colour = SGoranges[1]) +
  geom_point_interactive(data = filter(data, yearn >= 5),
                         aes(x = x, y = ymax,
                             tooltip = tooltip,
                             data_id = tooltip),
                         show.legend = FALSE,
                         size = 6,
                         colour = "white",
                         alpha = 0.01) +
  annotate("text", x = 3.5, y = 0.20, label = "In workless households",
           colour = SGblue, hjust = 0) +
  annotate("text", x = 3.5, y = 0.06, label = "In working households",
           colour = SGoranges[1], hjust = 0)

# chart12 abs pov ch -----------------------------------------------------------
data <- filter(povertychartdata$abspov, weight == "gs_newch")

povertycharts$chart12 <- linechart(data) +
  scale_y_continuous(limits = c(0.06, 0.53)) +
  addnames(up = c(-0.2, 0.03)) +
  addscales() +
  addsource() +
  addlabels()

# chart13 mat dep ch -----------------------------------------------------------
data <- povertychartdata$cmd

povertycharts$chart13 <- linechart(data, recession = FALSE) +
  scale_y_continuous(limits = c(0.05, 0.35)) +
  addscales() +
  addlabels() +
  geom_vline(aes(xintercept = 16),
             colour = SGgreys[3],
             alpha = 0.9) +
  annotate("text",
           label = "Methodology change\n2010/11",
           size = 3,
           colour = SGgreys[2],
           x = 16.2,
           y = Inf,
           hjust = 0,
           vjust = 2) +
  addnames(up = c(0.05, -0.05)) +
  addsource()

# chart14 food sec ch ----------------------------------------------------------
data <- filter(povertychartdata$foodsec, weight == "gs_newch")

povertycharts$chart14 <- ggplot(data, aes(x = x, y = y, group = key,
                                          fill = key, width = 0.8)) +

  geom_bar_interactive(aes(tooltip = tooltip,
                           data_id = tooltip),
                       position = 'fill',
                       stat = "identity",
                       colour = "white") +
  coord_flip() +
  scale_fill_manual(values = SGmix2,
                    guide = guide_legend(reverse = TRUE)) +

  scale_y_continuous(labels = percent_format(1)) +

  theme(axis.line.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.length = unit(2, "pt"),
        legend.position = "top") +

  addsource()

# chart15 priority ch ----------------------------------------------------------
data <- filter(povertychartdata$priority, type == "low60ahc")
povertycharts$chart15 <- barchart(data)

# chart16 priority ch ----------------------------------------------------------
data <- filter(povertychartdata$priority, type == "low60ahcabs")
povertycharts$chart16 <- barchart(data)

# chart17 priority ch ----------------------------------------------------------
data <- filter(povertychartdata$priority,  type == "cmdahc")
povertycharts$chart17 <- barchart(data)

# Equality ---------------------------------------------------------------------

## chart18 age -----------------------------------------------------------------
data <- povertychartdata$age
povertycharts$chart18 <- linechart(data, recession = FALSE) +
  # smaller y-range to spread out categories
  scale_y_continuous(limits = c(0.1, 0.35)) +
  addscales(palette = SGmix3[4:9]) +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "2017-20"),
                   expand = c(0.2, 0))

## chart18b age2 -----------------------------------------------------------------
data <- povertychartdata$age2
povertycharts$chart18b <- linechart(data, recession = FALSE) +
  scale_y_continuous(limits = c(0.135, 0.61)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "2017-20"),
                   expand = c(0.2, 0))

## chart19 age3 -----------------------------------------------------------------
data <- povertychartdata$age3
povertycharts$chart19 <- barchart(data, palette = SGmix3)

## chart20 gender wa -----------------------------------------------------------
data <- povertychartdata$gender %>%
  filter(key %in% c("Single man, no children",
                    "Single woman, no children",
                    "Single mother"))

povertycharts$chart20 <- linechart(data, recession = FALSE) +
  scale_y_continuous(limits = c(0.21, 0.68)) +
  addscales() +
  addsource() +
  addlabels() +
  addnames(up = c(0.03, -0.05, 0.05))

## chart21 gender pn -----------------------------------------------------------
data <-  filter(povertychartdata$gender,
                key %in% c("Single male pensioner",
                           "Single female pensioner"))

povertycharts$chart21 <- linechart(data, recession = FALSE) +
  scale_y_continuous(limits = c(0.07, 0.54)) +
  addscales() +
  addsource() +
  addlabels() +
  addnames(up = c(0.04,-0.14))

## chart22 marital -------------------------------------------------------------
data <- povertychartdata$marital

povertycharts$chart22 <- linechart(data, recession = FALSE) +
  scale_y_continuous(limits = c(0.03, 0.5)) +
  addscales() +
  addlabels() +
  addsource() +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "",
                              "", "", "2000-03", "",
                              "", "", "", "",
                              "2006-09", "", "", "",
                              "", "", "2012-15", "",
                              "", "", "", "2017-20"),
                   expand = c(0.3, 0))

## chart23 ethnic --------------------------------------------------------------
data <- povertychartdata$ethnic
povertycharts$chart23 <- barchart(data)

## chart24 religion ------------------------------------------------------------
data <- povertychartdata$religion
povertycharts$chart24 <- barchart(data)

## chart25 disability ----------------------------------------------------------
data <- povertychartdata$disability
povertycharts$chart25 <- linechart(data, recession = FALSE) +
  disabilitybreaks() +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  addscales() +
  addsource() +
  addlabels() +
  addnames(up = c(0.07, -0.07))

## chart26 disability2 ---------------------------------------------------------
data <- povertychartdata$disability2
povertycharts$chart26 <- linechart(data, recession = FALSE) +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  addscales() +
  disabilitybreaks() +
  addsource() +
  addlabels() +
  addnames(up = c(-0.05, -0.07))

# Income inequality ------------------------------------------------------------

## chart27 palma ---------------------------------------------------------------
data <- povertychartdata$palma
povertycharts$chart27 <- linechart(data) +
  scale_y_continuous(limits = c(0.8, 1.8)) +
  addscales() +
  addsource() +
  addlabels() +
  addnames(up = c(-0.06, 0.26))

## chart28 gini ----------------------------------------------------------------
data <- povertychartdata$gini
povertycharts$chart28 <- linechart(data) +
  scale_y_continuous(limits = c(0.27, 0.4)) +
  addscales() +
  addsource() +
  addlabels() +
  addnames(up = c(-0.01, 0.035))

# Income -----------------------------------------------------------------------

## chart29 medians -------------------------------------------------------------
data <- povertychartdata$medians
povertycharts$chart29 <- linechart(data) +
  scale_y_continuous(limits = c(250, 600)) +
  addscales() +
  addsource() +
  addlabels() +
  addnames(up = c(110, -30))

## chart30 deciles -------------------------------------------------------------
data <- povertychartdata$deciles
povertycharts$chart30 <- ggplot(data,
                                aes(x = x, y = y, fill = key, group = key)) +

  geom_bar_interactive(aes(tooltip = tooltip,
                           data_id = tooltip),
                       position = 'dodge',
                       colour = "white",
                       stat = "identity") +

  scale_fill_manual(values = rev(SGblues[1:5])) +

  scale_x_discrete(labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th",
                              "8th", "9th"),
                   expand = c(0.1, 0.1)) +

  scale_y_continuous(labels = comma_format(prefix = "£")) +

  theme(axis.line.y = element_line(),
        axis.text.y = element_text(hjust = 1,
                                   margin = margin(0, 3, 0, 0, "pt")),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_line(),
        axis.title = element_blank()) +

  addsource()

## chart31 income growth ------------------------------------------------------

data <- povertychartdata$growth
povertycharts$chart31 <- ggplot(data,
                                aes(x = x, y = y,
                                    label = label)) +

  geom_bar_interactive(aes(tooltip = tooltip,
                           data_id = tooltip),
                       position = 'dodge',
                       fill = SGblue,
                       colour = "white",
                       stat = "identity") +
  geom_text(aes(y = ifelse(y < 0, y - 0.009, y + 0.009)),
            colour = SGgreys[1]) +

  scale_x_discrete(labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th",
                              "8th", "9th"),
                   expand = c(0.1, 0.1)) +

  scale_y_continuous(labels = percent_format(1),
                     limits = c(-0.1, 0.1)) +

  theme(axis.line.y = element_line(),
        axis.text.y = element_text(hjust = 1,
                                   margin = margin(0, 3, 0, 0, "pt")),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_line(),
        axis.title = element_blank()) +

  addsource()

## chart32 distribution --------------------------------------------------------

data <- povertychartdata$distribution %>%
  filter(income > 0,
         income < 1400)

decilepts <- povertychartdata$distdecs
povthreshold <- povertychartdata$distthresh$povthresh
Scotmedian <- povertychartdata$distthresh$Scotmedian

povertycharts$chart32 <- ggplot(data = data,
                                mapping = aes(x = income, weight = gs_newpp)) +
  geom_density(colour = NA,
               fill = SGmix[1],
               adjust = 1/2) +

  geom_text(data = decilepts,
            mapping = aes(x = xpos, y = 0.0001, label = x, weight = NULL),
            colour = SGgreys[5],
            fontface = "bold") +

  scale_x_continuous(labels = comma_format(prefix = "£"),
                     breaks = c(seq(0, 1400, 200)),
                     expand = c(0.1, 0.1),
                     limits = c(0, 1400)) +

  annotate("rect", fill = "white", alpha = 0.4,
           xmin = 0,
           xmax = decilepts$value[1],
           ymin = -Inf,
           ymax = +Inf) +

  annotate("rect", fill = "white", alpha = 0.4,
           xmin = decilepts$value[2],
           xmax = decilepts$value[3],
           ymin = -Inf, ymax = +Inf) +

  annotate("rect", fill = "white", alpha = 0.4,
           xmin = decilepts$value[4],
           xmax = decilepts$value[5],
           ymin = -Inf, ymax = +Inf) +

  annotate("rect", fill = "white", alpha = 0.4,
           xmin = decilepts$value[6],
           xmax = decilepts$value[7],
           ymin = -Inf, ymax = +Inf) +

  annotate("rect", fill = "white", alpha = 0.4,
           xmin = decilepts$value[8],
           xmax = decilepts$value[9],
           ymin = -Inf, ymax = +Inf) +

  annotate("segment", x = povthreshold, xend = povthreshold,
           y = 0, yend = Inf, colour = SGgreys[4],
           linetype = "dashed") +

  annotate("segment", x = Scotmedian, xend = Scotmedian,
           y = 0, yend = Inf, colour = SGgreys[4],
           linetype = "dashed") +

  annotate("text", x = 280, y = 0.0016,
           label = str_c("Poverty threshold: ",
                         comma2(povthreshold,
                                prefix = "£")),
           colour = SGgreys[2],
           hjust = 1) +

  annotate("text", x = 570, y = 0.0016,
           label = str_c("Median income: ",
                         comma2(Scotmedian,
                                prefix = "£")),
           colour = SGgreys[2],
           hjust = 0) +

  addsource()

## chart33 sources -------------------------------------------------------------
data <- povertychartdata$sources

povertycharts$chart33 <- ggplot(data,
                                aes(x = x, y = y, fill = key, width = 1)) +

  geom_bar_interactive(aes(tooltip = tooltip,
                           data_id = tooltip),
                       position = "fill",
                       stat = "identity",
                       colour = "white") +

  scale_fill_manual(values = SGmix) +

  scale_y_continuous(labels = percent_format(1)) +
  scale_x_discrete(breaks = c(seq(1, 10, 1)),
                   labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"),
                   expand = c(0.05, 0.05)) +

  theme(axis.line = element_line(),
        axis.text.y = element_text(hjust = 1,
                                   margin = margin(0, 3, 0, 0, "pt")),
        axis.title.y = element_text(hjust = 0.5),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_line(),
        legend.position = c(0.75, 0.72)) +

  ylab("Proportion of income") +
  addsource()

saveRDS(povertycharts, "data/povertycharts.rds")
rm(list = ls())
