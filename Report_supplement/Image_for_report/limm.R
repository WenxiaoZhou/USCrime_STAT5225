library(tidyverse)
library(lme4)
setwd("C:/Users/Min/OneDrive/UConn/Spring 2021/BIST 5225_Data Management and Programming in R and SAS/Group Assignment/LIMM")
load("fbi_with_gdp.RData")

# Rape began going up in 2013, when the revised definition is applied.
state_gdp %>% 
        ggplot(aes(x = Year, y = Rape, group = State, color = index)) + 
        geom_point(shape = 18, color = "grey60") + 
        geom_line(color = "grey60") + theme_bw() + 
        geom_smooth(aes(group = 15), method = 'loess', size = 1.5, 
                    se = FALSE, color = "darkred") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# find max: DC dominates all but Rape, which is won by Alaksa 
state_gdp %>% group_by(Year) %>% 
        dplyr::summarize(
                across(c(GDP_real_per_capita, ViolentCrime:MotorTheft), 
                       function(.col) {
                               .max <- max(.col, na.rm = TRUE)
                               .max == -Inf && return(NA_character_)
                               State[which(.col == .max)]
                       }) %>% view
        ) %>% 
        select(-contains("Definition"), -contains("Crime")) %>% view

# emphasize DC and Alaska 
state_gdp %>% 
        pivot_longer(ViolentCrime:MotorTheft, names_to = "Crime", values_to = "rate") %>% 
        mutate(Crime = as_factor(Crime)) %>% 
        mutate(color = case_when(
                State == "District of Columbia" ~ "District of Columbia",
                State == "Alaska" ~ "Alaska",
                TRUE              ~ "Others"
        ) %>% factor(levels = c("District of Columbia", "Alaska", "Others"))
        ) %>% 
        ggplot(aes(x = Year, y = rate, group = State, color = color)) + 
        facet_wrap(~Crime, scales = "free_y") + 
        scale_color_manual(values =  c("District of Columbia" = "darkred",
                                       "Alaska" = "#E69F00", 
                                       "Others" = "grey60")) + 
        geom_point(shape = 18, alpha = .5) + 
        geom_line(alpha = .5) + theme_bw() + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






