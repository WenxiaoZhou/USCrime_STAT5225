library(tidyverse)
setwd("../../Group Assignment/R shiny")
gdp <- read_csv("SAGDP1__ALL_AREAS_1997_2020.csv")
gdp_real <- gdp %>% filter(LineCode == 1) %>% 
        select(GeoName, `1997`:`2019`) %>% 
        pivot_longer(`1997`:`2019`, names_to = "Year", values_to = "GDP_real_in_million")

load("fbi_all.RData")
state_gdp <- 
        state_rate_per_100k %>% 
        mutate(State = fct_recode(State, 
                                  "District of Columbia" = "District Of Columbia" )) %>% 
        left_join(gdp_real, by = c("State" = "GeoName", "Year" = "Year")) %>% 
        mutate(GDP_real_per_capita = GDP_real_in_million / Population * 1e6, 
               .after = Population) %>% 
        mutate(Year = factor(Year, levels = as.character(1995:2019), ordered = TRUE)) %>% 
        select(!GDP_real_in_million) %>% 
        mutate(
                Definition_of_rape_and_violent = case_when(
                        Year < 2013 ~ "Legacy",
                        Year >= 2013 ~ "Revised"
                ), 
                .before = Murder
        ) %>% 
        mutate(
                Rape = if_else(Definition_of_rape_and_violent == "Legacy", RapeLegacy, RapeRevised), 
                .after = Murder
        ) %>% 
        mutate(
                ViolentCrime = if_else(Definition_of_rape_and_violent == "Legacy", ViolentCrimeLegacy, ViolentCrimeRevised), 
                .before = Murder
        ) %>% 
        select(-ViolentCrimeLegacy, -ViolentCrimeRevised, 
               -RapeLegacy, -RapeRevised)


area <- 
        area_level %>% 
        mutate(Year = factor(Year, levels = as.character(1995:2019), ordered = TRUE)) %>% 
        mutate(State = fct_recode(State, 
                                  "District of Columbia" = "District Of Columbia" )) %>% 
                mutate(
                        Definition_of_rape_and_violent = case_when(
                                Year < 2013 ~ "Legacy",
                                Year >= 2013 ~ "Revised"
                        ), 
                        .before = Murder
                ) %>% 
                mutate(
                        Rape = if_else(Definition_of_rape_and_violent == "Legacy", RapeLegacy, RapeRevised), 
                        .after = Murder
                ) %>% 
                mutate(
                        ViolentCrime = if_else(Definition_of_rape_and_violent == "Legacy", ViolentCrimeLegacy, ViolentCrimeRevised), 
                        .before = Murder
                ) %>% 
                select(-ViolentCrimeLegacy, -ViolentCrimeRevised, 
                       -RapeLegacy, -RapeRevised)
        

save(area, state_gdp, file = "fbi_with_gdp.RData")        

load("fbi_with_gdp.RData")
