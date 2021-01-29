library(tidyverse)

source("R/00_functions.R")
source("R/00_strings.R")

hbai <- readRDS("data/tidyhbai.rds")

povertytables <- list()

# table 1 - BHC poverty thresholds ----

latesthbai_1 <- hbai[[length(labels$years[[1]])]]
latesthbai_2 <- hbai[[length(labels$years[[1]]) - 1]]
latesthbai_3 <- hbai[[length(labels$years[[1]]) - 2]]

df1 <- getpovertythresholdsbhc(latesthbai_1)
df2 <- getpovertythresholdsbhc(latesthbai_2)
df3 <- getpovertythresholdsbhc(latesthbai_3)

df <- data.frame(df1[1])
df$weekly1 <- (df1$weekly1 + df2$weekly1 + df3$weekly1)/3
df$annual1 <- (df1$annual1 + df2$annual1 + df3$annual1)/3
df$weekly2 <- (df1$weekly2 + df2$weekly2 + df3$weekly2)/3
df$annual2 <- (df1$annual2 + df2$annual2 + df3$annual2)/3
df$weekly3 <- (df1$weekly3 + df2$weekly3 + df3$weekly3)/3
df$annual3 <- (df1$annual3 + df2$annual3 + df3$annual3)/3
df$weekly4 <- (df1$weekly4 + df2$weekly4 + df3$weekly4)/3
df$annual4 <- (df1$annual4 + df2$annual4 + df3$annual4)/3

povertytables[["table1"]] <- df %>%
  mutate_at(vars(starts_with("weekly")), ~comma2(., 1, prefix = "£")) %>%
  mutate_at(vars(starts_with("annual")), ~comma2(., 100, prefix = "£")) %>%
  head(4L)


# table 2 - equivalence scale

povertytables[["table2"]] <- data.frame( " " = c("First adult",
                          "Spouse",
                          "Subsequent adults",
                          "Children aged under 14 years",
                          "Children aged 14 years and over"),
                  BHC = c(0.67,
                          0.33,
                          0.33,
                          0.20,
                          0.33),
                  AHC = c(0.58,
                          0.42,
                          0.42,
                          0.20,
                          0.42))

saveRDS(povertytables, "data/povertytables.rds")
rm(list = ls())

