library(pacman)
p_load(dplyr,purrr, readr, ggplot2, tidyr,readxl, lubridate, stringr, scales, forcats, ggthemes, ggpubr, gganimate, gganimate)
#read in an excel file called empl.xlsx. Filter the data where unit contains the following: 11 East, 6 South, 8 East, FH (ignoring the case)
empl <- readxl::read_excel("~/Downloads/empl.xlsx")

#read an excel file of transactions named tlist.xlsx with multiple tabs. Ignore first 5 rows of each tab and merge all tabs into 1 using the tab name to create a first column named cost_centre in the merged data frame

tlist <- readxl::excel_sheets("~/Downloads/tlist.xlsx")
tlist <- lapply(tlist, function(x) readxl::read_excel("~/Downloads/tlist.xlsx", sheet = x, skip = 5) %>% mutate(cost_centre = x))
tlist <- bind_rows(tlist)
tlist <- tlist %>% mutate(
  ward = case_when(
    cost_centre == 73457 ~ 'FH_infusion',
    cost_centre == 45106 ~ '11E',
    cost_centre == 21790 ~ '6S',
    cost_centre == 11034 ~ '8E',
    TRUE ~ 'Other'
  )
) %>% filter(ward != 'Other')
#filter tlist Account Code column to include fields starting with Nurse and not ending with Total

str_detect(tlist$`Account Code`, "^Nurse")# & !str_detect(`Account Code`, "Total$"))
