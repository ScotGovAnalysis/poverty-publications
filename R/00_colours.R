
# Define colour palette with SG colours

SGblue <- "#0065bd"
SGblue2 <- "#002d54"
SGred <- "#d4351c"

SGblues <- c("#333333", "#002d54", "#00437e", "#0065bd", "#4c93d0", "#7fb2de", "#d9effc")
SGgreys <- c("#333333", "#727272", "#b3b3b3", "#ebebeb", "#f8f8f8")
SGoranges <- c("#aa5327", "#f47738", "#f79f73", "#fac8af")

SGmix3 <- c(SGblue, SGgreys[2], SGoranges[2])
SGmix4 <- c("Very low" = SGoranges[1], "Low" = SGoranges[2],
            "Marginal" = SGoranges[4], "High" = SGblues[5])

SGmix6 <- c(SGblues[4], SGblues[2], SGgreys[2], SGoranges[1], SGoranges[2],
           SGblues[6], SGgreys[3])

SGmix9 <- c(SGoranges[2:4], SGblues[6:2], SGgreys[2])

# scales::show_col(SGblues)
# scales::show_col(SGgreys)
# scales::show_col(SGoranges)
# scales::show_col(SGmix3)


