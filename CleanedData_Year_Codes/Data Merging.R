library(tidyverse)
library(glue)
library(rlang)
setwd("C:/Users/Min/OneDrive/UConn/Spring 2021/BIST 5225_Data Management and Programming in R and SAS/Group Assignment/Cleaned/")
dat <- vector("list", 25)
for(i in 1995:2019) {
        load(glue("tidy_{i}.RData"))
        dat[[i - 1994]] <- glue("fbi{i}_est") %>% parse_expr() %>% eval()
}
names(dat) <- glue("fbi{1995:2019}")

fbi_all <- dat %>%
        map2(names(dat), ~ mutate(.x, Year = str_remove_all(.y, "[:letter:]"),
                                  .after = Area)) %>%
        reduce(bind_rows) %>%
        mutate(State = as_factor(State),
               Area = if_else(Area == "Rural", "Nonmetropolitan counties", Area),
               Area = as_factor(Area))
rm(list = ls()[ls() != "fbi_all"])

# check ID
fbi_all %>%
        group_by(State, Area, Year) %>%
        count() %>% filter(n != 1) %>% nrow() # ID uniquely defines a row

area_level <- fbi_all %>%
        filter(Area != "State Total")

state_rate_per_100k <- fbi_all %>%
        filter(Area == "State Total") %>%
        select(-Area, - AreaActuallyReporting)

area_level %>% group_by(State, Year) %>%
        summarize(across(Population:MotorTheft, sum, na.rm = TRUE)) %>%
        mutate(across(ViolentCrimeLegacy:MotorTheft, ~ .x / Population * 100000)) %>%
        nrow
# 1258 rows < 1294 rows of state_rate because some states in some years only have state total

save(area_level, state_rate_per_100k, file = "fbi_all.RData")
















