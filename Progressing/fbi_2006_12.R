title: "Data Cleaning Report for FBI 2006-2012"
author: "BN"
date: "2021/03/20"
#2012 DATA_cleaning
path <- "C:/Users/nbris/Desktop/data group project/2012.xls"
library(tidyverse)
fbi2012 <- readxl::read_excel(path, skip = 3) # skip titles
fbi2012 <- fbi2012[1:502, 1:13] # remove footnotes and empty columns
name <- c("State","Area", "index", "Population", "ViolentCrime", 
          "Murder", "Rape", "Robbery", "AggravatedAssault", 
          "PropertyCrime", "Burglary", "LarcenyTheft",	"MotorTheft")
###############################
fbi2012_1 <- fbi2012 %>% 
  # Replace NA in the "State" column. 
  # Every NA is filled by the first non-NA value on its above
  fill(State, .direction = "down") %>% 
  # since State has been fixed, grouped by it
  group_by(State) %>% 
  # within each state, fill for Area
  # grouped by State to prevent the NA of an Area from being filled 
  #       by values from the previous state
  fill(Area, .direction = "down") %>% 
  # remove "State Total"/Total rows
  filter(!(Area %in% c("State Total", "Total"))) %>% 
  set_names(name) %>% 
  mutate(index = case_when(
    index == "Area actually reporting" ~ "act",
    index == "Estimated total"         ~ "est",
    TRUE                               ~ "popu" # NA -> "popu"
  )) %>% ungroup()
fbi2012_1

#nest the table by State
fbi2012_1 %>% group_nest(State)
#Change values of State to propery case and trim ending digits
state <- fbi2012_1 %>% pull(State)
state[str_detect(state, "\\d$")] %>% unique # ends with a digit
fbi2012_1$State <- state %>% str_remove_all("\\d") %>% str_to_title()
fbi2012_1$State %>% unique # nice! 50 states + DC + Puerto Rico
#Pivot wider
fbi2012_2 <- fbi2012_1 %>% 
  pivot_wider(names_from = index, values_from = Population:MotorTheft) 
fbi2012_2 %>% print(width = Inf)
fbi2012_3 <- fbi2012_2 %>%
  rename(Population = Population_popu) %>%
  mutate(
    AreaActuallyReporting = round(as.numeric(Population_act), 3), # three digits
    .after = Area
  ) %>% select(
    -Population_est, 
    -Population_act,
    # we have renamed Population_popu, so simply drop all columns ending with "_popu"
    -ends_with("_popu")  
  ) %>% 
  # Population contains "None"; remove those rows 
  # also, before pivoting, Population is a character because of the appearance of "None"
  filter(Population != "None" ) %>% 
  mutate(Population = as.numeric(Population)) 
fbi2012_3
#Pivoting wider makes it explicit and automatically replaces missing elements by NA
fbi2012_3 %>% map_dbl(~ sum(is.na(.x))) 
#whether or not the NA in est
fbi2012_3 %>% filter(AreaActuallyReporting %>% near(1)) %>% 
  select(ends_with("_est")) %>% 
  map_dbl(~ sum(is.na(.x)))
# Actually can be replaced by tidyr::complete() -> tidyr::fill() workflow before pivoting.
# But I have no idea how to expand the dataset. 
impute_est <- function(.tbl) {
  # To fill NA row-wise, we first have to obtain sub-tibbles
  #       that consist of all numeric columns. 
  # Then we can treat them as matrices. 
  suppressWarnings(suppressMessages(require(zoo)))
  non_dbl <- .tbl %>% select(!where(is_numeric))
  non_dbl %>% bind_cols(
    .tbl %>% select(where(is_numeric)) %>% 
      apply(1, zoo::na.locf) %>% 
      t() %>% 
      as_tibble()
  )
}

fbi2012_4 <- fbi2012_3 %>% impute_est()
fbi2012_4 %>% complete.cases() %>% all # all complete

#Split
fbi2012_act <- fbi2012_4 %>% 
  select(State:Population, ends_with("_act")) %>% 
  rename_with(~ str_remove(.x, "_act"), ends_with("_act"))


fbi2012_est <- fbi2012_4 %>% 
  select(State:Population, ends_with("_est")) %>% 
  rename_with(~ str_remove(.x, "_est"), ends_with("_est"))

save(fbi2012_act, fbi2012_est, file = "data_2012.RData")



#######2011 DATA_cleaning#########
path <- "C:/Users/nbris/Desktop/data group project/2011.xls"

library(tidyverse)
fbi2011 <- readxl::read_excel(path, skip = 3) # skip titles
fbi2011 <- fbi2011[1:511, 1:13] # remove footnotes and empty columns

name <- c("State","Area", "index", "Population", "ViolentCrime", 
          "Murder", "Rape", "Robbery", "AggravatedAssault", 
          "PropertyCrime", "Burglary", "LarcenyTheft",	"MotorTheft")
fbi2011_1 <- fbi2011 %>% 
  fill(State, .direction = "down") %>% 
  # since State has been fixed, grouped by it
  group_by(State) %>% 
  # within each state, fill for Area
  # grouped by State to prevent the NA of an Area from being filled 
  #       by values from the previous state
  fill(Area, .direction = "down") %>% 
  # remove "State Total"/Total rows
  filter(!(Area %in% c("State Total", "Total"))) %>% 
  set_names(name) %>% 
  mutate(index = case_when(
    index == "Area actually reporting" ~ "act",
    index == "Estimated total"         ~ "est",
    TRUE                               ~ "popu" # NA -> "popu"
  )) %>% ungroup()
fbi2011_1

#nest the table by State
fbi2011_1 %>% group_nest(State)
#Change values of State to propery case and trim ending digits
state <- fbi2011_1 %>% pull(State)
state[str_detect(state, "\\d$")] %>% unique # ends with a digit
fbi2011_1$State <- state %>% str_remove_all("\\d") %>% str_to_title()
fbi2011_1$State %>% unique # nice! 50 states + DC + Puerto Rico
#Pivot wider
fbi2011_2 <- fbi2011_1 %>% 
  pivot_wider(names_from = index, values_from = Population:MotorTheft) 
fbi2011_2 %>% print(width = Inf)
fbi2011_3 <- fbi2011_2 %>%
  rename(Population = Population_popu) %>%
  mutate(
    AreaActuallyReporting = round(as.numeric(Population_act), 3), # three digits
    .after = Area
  ) %>% select(
    -Population_est, 
    -Population_act,
    # we have renamed Population_popu, so simply drop all columns ending with "_popu"
    -ends_with("_popu")  
  ) %>% 
  # Population contains "None"; remove those rows 
  # also, before pivoting, Population is a character because of the appearance of "None"
  filter(Population != "None" ) %>% 
  mutate(Population = as.numeric(Population)) 
fbi2011_3
#Pivoting wider makes it explicit and automatically replaces missing elements by NA
fbi2011_3 %>% map_dbl(~ sum(is.na(.x))) 
#whether or not the NA in est
fbi2011_3 %>% filter(AreaActuallyReporting %>% near(1)) %>% 
  select(ends_with("_est")) %>% 
  map_dbl(~ sum(is.na(.x)))
# Actually can be replaced by tidyr::complete() -> tidyr::fill() workflow before pivoting.
# But I have no idea how to expand the dataset. 
impute_est <- function(.tbl) {
  # To fill NA row-wise, we first have to obtain sub-tibbles
  #       that consist of all numeric columns. 
  # Then we can treat them as matrices. 
  suppressWarnings(suppressMessages(require(zoo)))
  non_dbl <- .tbl %>% select(!where(is_numeric))
  non_dbl %>% bind_cols(
    .tbl %>% select(where(is_numeric)) %>% 
      apply(1, zoo::na.locf) %>% 
      t() %>% 
      as_tibble()
  )
}

fbi2011_4 <- fbi2011_3 %>% impute_est()
fbi2011_4 %>% complete.cases() %>% all # all complete

#Split
fbi2011_act <- fbi2011_4 %>% 
  select(State:Population, ends_with("_act")) %>% 
  rename_with(~ str_remove(.x, "_act"), ends_with("_act"))


fbi2011_est <- fbi2011_4 %>% 
  select(State:Population, ends_with("_est")) %>% 
  rename_with(~ str_remove(.x, "_est"), ends_with("_est"))

save(fbi2011_act, fbi2011_est, file = "data_2011.RData")





###########2010 DATA_cleaning###########
path <- "C:/Users/nbris/Desktop/data group project/2010.xls"

library(tidyverse)
fbi2010 <- readxl::read_excel(path, skip = 3) # skip titles
fbi2010 <- fbi2010[1:503, 1:13] # remove footnotes and empty columns

name <- c("State","Area", "index", "Population", "ViolentCrime", 
          "Murder", "Rape", "Robbery", "AggravatedAssault", 
          "PropertyCrime", "Burglary", "LarcenyTheft",	"MotorTheft")

fbi2010_1 <- fbi2010 %>% 
  # Replace NA in the "State" column. 
  # Every NA is filled by the first non-NA value on its above
  fill(State, .direction = "down") %>% 
  # since State has been fixed, grouped by it
  group_by(State) %>% 
  # within each state, fill for Area
  # grouped by State to prevent the NA of an Area from being filled 
  #       by values from the previous state
  fill(Area, .direction = "down") %>% 
  # remove "State Total"/Total rows
  filter(!(Area %in% c("State Total", "Total"))) %>% 
  set_names(name) %>% 
  mutate(index = case_when(
    index == "Area actually reporting" ~ "act",
    index == "Estimated total"         ~ "est",
    TRUE                               ~ "popu" # NA -> "popu"
  )) %>% ungroup()
fbi2010_1

#nest the table by State
fbi2010_1 %>% group_nest(State)
#Change values of State to propery case and trim ending digits
state <- fbi2010_1 %>% pull(State)
state[str_detect(state, "\\d$")] %>% unique # ends with a digit
fbi2010_1$State <- state %>% str_remove_all("\\d") %>% str_to_title()
fbi2010_1$State %>% unique # nice! 50 states + DC + Puerto Rico
#Pivot wider
fbi2010_2 <- fbi2010_1 %>% 
  pivot_wider(names_from = index, values_from = Population:MotorTheft) 
fbi2010_2 %>% print(width = Inf)
fbi2010_3 <- fbi2010_2 %>%
  rename(Population = Population_popu) %>%
  mutate(
    AreaActuallyReporting = round(as.numeric(Population_act), 3), # three digits
    .after = Area
  ) %>% select(
    -Population_est, 
    -Population_act,
    # we have renamed Population_popu, so simply drop all columns ending with "_popu"
    -ends_with("_popu")  
  ) %>% 
  # Population contains "None"; remove those rows 
  # also, before pivoting, Population is a character because of the appearance of "None"
  filter(Population != "None" ) %>% 
  mutate(Population = as.numeric(Population)) 
fbi2010_3
#Pivoting wider makes it explicit and automatically replaces missing elements by NA
fbi2010_3 %>% map_dbl(~ sum(is.na(.x))) 
#whether or not the NA in est
fbi2010_3 %>% filter(AreaActuallyReporting %>% near(1)) %>% 
  select(ends_with("_est")) %>% 
  map_dbl(~ sum(is.na(.x)))
# Actually can be replaced by tidyr::complete() -> tidyr::fill() workflow before pivoting.
# But I have no idea how to expand the dataset. 
impute_est <- function(.tbl) {
  # To fill NA row-wise, we first have to obtain sub-tibbles
  #       that consist of all numeric columns. 
  # Then we can treat them as matrices. 
  suppressWarnings(suppressMessages(require(zoo)))
  non_dbl <- .tbl %>% select(!where(is_numeric))
  non_dbl %>% bind_cols(
    .tbl %>% select(where(is_numeric)) %>% 
      apply(1, zoo::na.locf) %>% 
      t() %>% 
      as_tibble()
  )
}

fbi2010_4 <- fbi2010_3 %>% impute_est()
fbi2010_4 %>% complete.cases() %>% all # all complete

#Split
fbi2010_act <- fbi2010_4 %>% 
  select(State:Population, ends_with("_act")) %>% 
  rename_with(~ str_remove(.x, "_act"), ends_with("_act"))


fbi2010_est <- fbi2010_4 %>% 
  select(State:Population, ends_with("_est")) %>% 
  rename_with(~ str_remove(.x, "_est"), ends_with("_est"))

save(fbi2010_act, fbi2010_est, file = "data_2010.RData")

###########2009 DATA_cleaning###########
path <- "C:/Users/nbris/Desktop/data group project/2009.xls"

library(tidyverse)
fbi2009 <- readxl::read_excel(path, skip = 3) # skip titles
fbi2009 <- fbi2009[1:495, 1:13] # remove footnotes and empty columns

name <- c("State","Area", "index", "Population", "ViolentCrime", 
          "Murder", "Rape", "Robbery", "AggravatedAssault", 
          "PropertyCrime", "Burglary", "LarcenyTheft",	"MotorTheft")
fbi2009_1 <- fbi2009 %>% 
  # Replace NA in the "State" column. 
  # Every NA is filled by the first non-NA value on its above
  fill(State, .direction = "down") %>% 
  # since State has been fixed, grouped by it
  group_by(State) %>% 
  # within each state, fill for Area
  # grouped by State to prevent the NA of an Area from being filled 
  #       by values from the previous state
  fill(Area, .direction = "down") %>% 
  # remove "State Total"/Total rows
  filter(!(Area %in% c("State Total", "Total"))) %>% 
  set_names(name) %>% 
  mutate(index = case_when(
    index == "Area actually reporting" ~ "act",
    index == "Estimated total"         ~ "est",
    TRUE                               ~ "popu" # NA -> "popu"
  )) %>% ungroup()
fbi2009_1

#nest the table by State
fbi2009_1 %>% group_nest(State)
#Change values of State to propery case and trim ending digits
state <- fbi2009_1 %>% pull(State)
state[str_detect(state, "\\d$")] %>% unique # ends with a digit
fbi2009_1$State <- state %>% str_remove_all("\\d") %>% str_to_title()
fbi2009_1$State %>% unique # nice! 50 states + DC + Puerto Rico
#Pivot wider
fbi2009_2 <- fbi2009_1 %>% 
  pivot_wider(names_from = index, values_from = Population:MotorTheft) 
fbi2009_2 %>% print(width = Inf)
fbi2009_3 <- fbi2009_2 %>%
  rename(Population = Population_popu) %>%
  mutate(
    AreaActuallyReporting = round(as.numeric(Population_act), 3), # three digits
    .after = Area
  ) %>% select(
    -Population_est, 
    -Population_act,
    # we have renamed Population_popu, so simply drop all columns ending with "_popu"
    -ends_with("_popu")  
  ) %>% 
  # Population contains "None"; remove those rows 
  # also, before pivoting, Population is a character because of the appearance of "None"
  filter(Population != "None" ) %>% 
  mutate(Population = as.numeric(Population)) 
fbi2009_3
#Pivoting wider makes it explicit and automatically replaces missing elements by NA
fbi2009_3 %>% map_dbl(~ sum(is.na(.x))) 
#whether or not the NA in est
fbi2009_3 %>% filter(AreaActuallyReporting %>% near(1)) %>% 
  select(ends_with("_est")) %>% 
  map_dbl(~ sum(is.na(.x)))
# Actually can be replaced by tidyr::complete() -> tidyr::fill() workflow before pivoting.
# But I have no idea how to expand the dataset. 
impute_est <- function(.tbl) {
  # To fill NA row-wise, we first have to obtain sub-tibbles
  #       that consist of all numeric columns. 
  # Then we can treat them as matrices. 
  suppressWarnings(suppressMessages(require(zoo)))
  non_dbl <- .tbl %>% select(!where(is_numeric))
  non_dbl %>% bind_cols(
    .tbl %>% select(where(is_numeric)) %>% 
      apply(1, zoo::na.locf) %>% 
      t() %>% 
      as_tibble()
  )
}

fbi2009_4 <- fbi2009_3 %>% impute_est()
fbi2009_4 %>% complete.cases() %>% all # all complete

#Split
fbi2009_act <- fbi2009_4 %>% 
  select(State:Population, ends_with("_act")) %>% 
  rename_with(~ str_remove(.x, "_act"), ends_with("_act"))


fbi2009_est <- fbi2009_4 %>% 
  select(State:Population, ends_with("_est")) %>% 
  rename_with(~ str_remove(.x, "_est"), ends_with("_est"))

save(fbi2009_act, fbi2009_est, file = "data_2009.RData")


###########2008 DATA_cleaning###########
path <- "C:/Users/nbris/Desktop/data group project/2008.xls"

library(tidyverse)
fbi2008 <- readxl::read_excel(path, skip = 3) # skip titles
fbi2008 <- fbi2008[1:499, 1:13] # remove footnotes and empty columns

name <- c("State","Area", "index", "Population", "ViolentCrime", 
          "Murder", "Rape", "Robbery", "AggravatedAssault", 
          "PropertyCrime", "Burglary", "LarcenyTheft",	"MotorTheft")

fbi2008_1 <- fbi2008 %>% 
  # Replace NA in the "State" column. 
  # Every NA is filled by the first non-NA value on its above
  fill(State, .direction = "down") %>% 
  # since State has been fixed, grouped by it
  group_by(State) %>% 
  # within each state, fill for Area
  # grouped by State to prevent the NA of an Area from being filled 
  #       by values from the previous state
  fill(Area, .direction = "down") %>% 
  # remove "State Total"/Total rows
  filter(!(Area %in% c("State Total", "Total"))) %>% 
  set_names(name) %>% 
  mutate(index = case_when(
    index == "Area actually reporting" ~ "act",
    index == "Estimated total"         ~ "est",
    TRUE                               ~ "popu" # NA -> "popu"
  )) %>% ungroup()
fbi2008_1

#nest the table by State
fbi2008_1 %>% group_nest(State)
#Change values of State to propery case and trim ending digits
state <- fbi2008_1 %>% pull(State)
state[str_detect(state, "\\d$")] %>% unique # ends with a digit
fbi2008_1$State <- state %>% str_remove_all("\\d") %>% str_to_title()
fbi2008_1$State %>% unique # nice! 50 states + DC + Puerto Rico
#Pivot wider
fbi2008_2 <- fbi2008_1 %>% 
  pivot_wider(names_from = index, values_from = Population:MotorTheft) 
fbi2008_2 %>% print(width = Inf)
fbi2008_3 <- fbi2008_2 %>%
  rename(Population = Population_popu) %>%
  mutate(
    AreaActuallyReporting = round(as.numeric(Population_act), 3), # three digits
    .after = Area
  ) %>% select(
    -Population_est, 
    -Population_act,
    # we have renamed Population_popu, so simply drop all columns ending with "_popu"
    -ends_with("_popu")  
  ) %>% 
  # Population contains "None"; remove those rows 
  # also, before pivoting, Population is a character because of the appearance of "None"
  filter(Population != "None" ) %>% 
  mutate(Population = as.numeric(Population)) 
fbi2008_3
#Pivoting wider makes it explicit and automatically replaces missing elements by NA
fbi2008_3 %>% map_dbl(~ sum(is.na(.x))) 
#whether or not the NA in est
fbi2008_3 %>% filter(AreaActuallyReporting %>% near(1)) %>% 
  select(ends_with("_est")) %>% 
  map_dbl(~ sum(is.na(.x)))
# Actually can be replaced by tidyr::complete() -> tidyr::fill() workflow before pivoting.
# But I have no idea how to expand the dataset. 
impute_est <- function(.tbl) {
  # To fill NA row-wise, we first have to obtain sub-tibbles
  #       that consist of all numeric columns. 
  # Then we can treat them as matrices. 
  suppressWarnings(suppressMessages(require(zoo)))
  non_dbl <- .tbl %>% select(!where(is_numeric))
  non_dbl %>% bind_cols(
    .tbl %>% select(where(is_numeric)) %>% 
      apply(1, zoo::na.locf) %>% 
      t() %>% 
      as_tibble()
  )
}

fbi2008_4 <- fbi2008_3 %>% impute_est()
fbi2008_4 %>% complete.cases() %>% all # all complete

#Split
fbi2008_act <- fbi2008_4 %>% 
  select(State:Population, ends_with("_act")) %>% 
  rename_with(~ str_remove(.x, "_act"), ends_with("_act"))


fbi2008_est <- fbi2008_4 %>% 
  select(State:Population, ends_with("_est")) %>% 
  rename_with(~ str_remove(.x, "_est"), ends_with("_est"))

save(fbi2008_act, fbi2008_est, file = "data_2008.RData")





###########2007 DATA_cleaning###########
path <- "C:/Users/nbris/Desktop/data group project/2007.xls"

library(tidyverse)
fbi2007 <- readxl::read_excel(path, skip = 3) # skip titles
fbi2007 <- fbi2007[1:495, 1:13] # remove footnotes and empty columns

name <- c("State","Area", "index", "Population", "ViolentCrime", 
          "Murder", "Rape", "Robbery", "AggravatedAssault", 
          "PropertyCrime", "Burglary", "LarcenyTheft",	"MotorTheft")

fbi2007_1 <- fbi2007 %>% 
  # Replace NA in the "State" column. 
  # Every NA is filled by the first non-NA value on its above
  fill(State, .direction = "down") %>% 
  # since State has been fixed, grouped by it
  group_by(State) %>% 
  # within each state, fill for Area
  # grouped by State to prevent the NA of an Area from being filled 
  #       by values from the previous state
  fill(Area, .direction = "down") %>% 
  # remove "State Total"/Total rows
  filter(!(Area %in% c("State Total", "Total"))) %>% 
  set_names(name) %>% 
  mutate(index = case_when(
    index == "Area actually reporting" ~ "act",
    index == "Estimated total"         ~ "est",
    TRUE                               ~ "popu" # NA -> "popu"
  )) %>% ungroup()
fbi2007_1

#nest the table by State
fbi2007_1 %>% group_nest(State)
#Change values of State to propery case and trim ending digits
state <- fbi2007_1 %>% pull(State)
state[str_detect(state, "\\d$")] %>% unique # ends with a digit
fbi2007_1$State <- state %>% str_remove_all("\\d") %>% str_to_title()
fbi2007_1$State %>% unique # nice! 50 states + DC + Puerto Rico
#Pivot wider
fbi2007_2 <- fbi2007_1 %>% 
  pivot_wider(names_from = index, values_from = Population:MotorTheft) 
fbi2007_2 %>% print(width = Inf)
fbi2007_3 <- fbi2007_2 %>%
  rename(Population = Population_popu) %>%
  mutate(
    AreaActuallyReporting = round(as.numeric(Population_act), 3), # three digits
    .after = Area
  ) %>% select(
    -Population_est, 
    -Population_act,
    # we have renamed Population_popu, so simply drop all columns ending with "_popu"
    -ends_with("_popu")  
  ) %>% 
  # Population contains "None"; remove those rows 
  # also, before pivoting, Population is a character because of the appearance of "None"
  filter(Population != "None" ) %>% 
  mutate(Population = as.numeric(Population)) 
fbi2007_3
#Pivoting wider makes it explicit and automatically replaces missing elements by NA
fbi2007_3 %>% map_dbl(~ sum(is.na(.x))) 
#whether or not the NA in est
fbi2007_3 %>% filter(AreaActuallyReporting %>% near(1)) %>% 
  select(ends_with("_est")) %>% 
  map_dbl(~ sum(is.na(.x)))
# Actually can be replaced by tidyr::complete() -> tidyr::fill() workflow before pivoting.
# But I have no idea how to expand the dataset. 
impute_est <- function(.tbl) {
  # To fill NA row-wise, we first have to obtain sub-tibbles
  #       that consist of all numeric columns. 
  # Then we can treat them as matrices. 
  suppressWarnings(suppressMessages(require(zoo)))
  non_dbl <- .tbl %>% select(!where(is_numeric))
  non_dbl %>% bind_cols(
    .tbl %>% select(where(is_numeric)) %>% 
      apply(1, zoo::na.locf) %>% 
      t() %>% 
      as_tibble()
  )
}

fbi2007_4 <- fbi2007_3 %>% impute_est()
fbi2007_4 %>% complete.cases() %>% all # all complete

#Split
fbi2007_act <- fbi2007_4 %>% 
  select(State:Population, ends_with("_act")) %>% 
  rename_with(~ str_remove(.x, "_act"), ends_with("_act"))


fbi2007_est <- fbi2007_4 %>% 
  select(State:Population, ends_with("_est")) %>% 
  rename_with(~ str_remove(.x, "_est"), ends_with("_est"))

save(fbi2007_act, fbi2007_est, file = "data_2007.RData")





###########2006 DATA_cleaning###########
path <- "C:/Users/nbris/Desktop/data group project/2006.xls"

library(tidyverse)
fbi2006 <- readxl::read_excel(path, skip = 3) # skip titles
fbi2006 <- fbi2006[1:501, 1:13] # remove footnotes and empty columns

name <- c("State","Area", "index", "Population", "ViolentCrime", 
          "Murder", "Rape", "Robbery", "AggravatedAssault", 
          "PropertyCrime", "Burglary", "LarcenyTheft",	"MotorTheft")

fbi2006_1 <- fbi2006 %>% 
  # Replace NA in the "State" column. 
  # Every NA is filled by the first non-NA value on its above
  fill(State, .direction = "down") %>% 
  # since State has been fixed, grouped by it
  group_by(State) %>% 
  # within each state, fill for Area
  # grouped by State to prevent the NA of an Area from being filled 
  #       by values from the previous state
  fill(Area, .direction = "down") %>% 
  # remove "State Total"/Total rows
  filter(!(Area %in% c("State Total", "Total"))) %>% 
  set_names(name) %>% 
  mutate(index = case_when(
    index == "Area actually reporting" ~ "act",
    index == "Estimated total"         ~ "est",
    TRUE                               ~ "popu" # NA -> "popu"
  )) %>% ungroup()
fbi2006_1

#nest the table by State
fbi2006_1 %>% group_nest(State)
#Change values of State to propery case and trim ending digits
state <- fbi2006_1 %>% pull(State)
state[str_detect(state, "\\d$")] %>% unique # ends with a digit
fbi2006_1$State <- state %>% str_remove_all("\\d") %>% str_to_title()
fbi2006_1$State %>% unique # nice! 50 states + DC + Puerto Rico
#Pivot wider
fbi2006_2 <- fbi2006_1 %>% 
  pivot_wider(names_from = index, values_from = Population:MotorTheft) 
fbi2006_2 %>% print(width = Inf)
fbi2006_3 <- fbi2006_2 %>%
  rename(Population = Population_popu) %>%
  mutate(
    AreaActuallyReporting = round(as.numeric(Population_act), 3), # three digits
    .after = Area
  ) %>% select(
    -Population_est, 
    -Population_act,
    # we have renamed Population_popu, so simply drop all columns ending with "_popu"
    -ends_with("_popu")  
  ) %>% 
  # Population contains "None"; remove those rows 
  # also, before pivoting, Population is a character because of the appearance of "None"
  filter(Population != "None" ) %>% 
  mutate(Population = as.numeric(Population)) 
fbi2006_3
#Pivoting wider makes it explicit and automatically replaces missing elements by NA
fbi2006_3 %>% map_dbl(~ sum(is.na(.x))) 
#whether or not the NA in est
fbi2006_3 %>% filter(AreaActuallyReporting %>% near(1)) %>% 
  select(ends_with("_est")) %>% 
  map_dbl(~ sum(is.na(.x)))
# Actually can be replaced by tidyr::complete() -> tidyr::fill() workflow before pivoting.
# But I have no idea how to expand the dataset. 
impute_est <- function(.tbl) {
  # To fill NA row-wise, we first have to obtain sub-tibbles
  #       that consist of all numeric columns. 
  # Then we can treat them as matrices. 
  suppressWarnings(suppressMessages(require(zoo)))
  non_dbl <- .tbl %>% select(!where(is_numeric))
  non_dbl %>% bind_cols(
    .tbl %>% select(where(is_numeric)) %>% 
      apply(1, zoo::na.locf) %>% 
      t() %>% 
      as_tibble()
  )
}

fbi2006_4 <- fbi2006_3 %>% impute_est()
fbi2006_4 %>% complete.cases() %>% all # all complete

#Split
fbi2006_act <- fbi2006_4 %>% 
  select(State:Population, ends_with("_act")) %>% 
  rename_with(~ str_remove(.x, "_act"), ends_with("_act"))


fbi2006_est <- fbi2006_4 %>% 
  select(State:Population, ends_with("_est")) %>% 
  rename_with(~ str_remove(.x, "_est"), ends_with("_est"))

save(fbi2006_act, fbi2006_est, file = "data_2006.RData")



