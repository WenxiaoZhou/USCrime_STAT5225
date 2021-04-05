setwd("/Users/zhouwenxiao/Desktop/USCrime")
library(tidyverse)
load("fbi_2005to2019.RData")

transform <- function(col, maximum = 100) {
        lambda_list <- MASS::boxcox(col ~ 1, 
                                    lambda = seq(-3, 3, 0.1), plotit = FALSE)
        lambda <- (lambda_list$x)[which.max(lambda_list$y)]
        if (near(lambda, 0)) {
                res <- log(col)
        } else {
                res <- (col^lambda - 1) / lambda
        }
        min <- min(res, na.rm = TRUE)
        max <- max(res, na.rm = TRUE)
        (res - min) / (max - min) * maximum
}

state_level_2005to2019 <- state_rate_per_100k_2005to2019 %>% 
        mutate(across(4:14, transform))

##--------------
##  An example  
##--------------

example <- state_level_2005to2019 %>% 
        filter(State %in% c("New York", "Connecticut", "Massachusetts"), 
               Year == "2019") %>% droplevels() %>%
        select(-Year, -Population, -ViolentCrimeLegacy, -RapeLegacy) %>% 
        column_to_rownames("State")
example <- rbind(rep(100, 9), rep(0, 9), example) # fxxking weird

library(fmsb)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
        radarchart(
                data, axistype = 1,
                # Customize the polygon
                pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
                # Customize the grid
                cglcol = "grey", cglty = 1, cglwd = 0.8,
                # Customize the axis
                axislabcol = "grey", 
                # Variable labels
                vlcex = vlcex, vlabels = vlabels,
                caxislabels = caxislabels, title = title, ...
        )
}
# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
        data = example, caxislabels = c(0, 25, 50, 75, 100),
        color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
        x = "bottom", legend = rownames(example[-c(1,2),]), horiz = TRUE,
        bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
        text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)
