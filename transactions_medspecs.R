library(pacman)
p_load(dplyr,purrr,stringi, readr,janitor, ggplot2, tidyr,readxl, lubridate, stringr, scales, forcats, ggthemes, ggpubr, gganimate, gganimate)
#read in an excel file called empl.xlsx. Filter the data where unit contains the following: 11 East, 6 South, 8 East, FH (ignoring the case)
empl <- readxl::read_excel("~/Downloads/empl.xlsx") %>% clean_names() %>% 
  select(surname, forenames, assignment_no, grade, unit)


#read an excel file of transactions named tlist.xlsx with multiple tabs. Ignore first 5 rows of each tab and merge all tabs into 1 using the tab name to create a first column named cost_centre in the merged data frame

tlist <- readxl::excel_sheets("~/Downloads/tlist.xlsx")
tlist <- lapply(tlist, function(x) readxl::read_excel("~/Downloads/tlist.xlsx", sheet = x, skip = 5) %>% mutate(cost_centre = x))
tlist <- bind_rows(tlist)
tlist <- tlist %>% mutate(
  unit = case_when(
    cost_centre == 73457 ~ 'FH Infusional Suite (Nurses)',
    cost_centre == 45106 ~ 'RF 11 East',
    cost_centre == 21790 ~ 'RF 6 South',
    cost_centre == 11034 ~ 'RF 8 East',
    cost_centre == 45108 ~ 'RF Oncology OPD & SACT Bay',
    TRUE ~ 'Other'
  )
) %>% filter(unit != 'Other')

pattern <- c("Total", "M7", "Royal Free")

tlist <- tlist %>% filter(grepl("Nurse", `Account Code`),
                          !grepl(paste(pattern, collapse = "|"), `Account Code`),
                          grepl("^[0-9]{4} - Nurse", `Account Code`)) %>% 
  mutate(`Account Code` = str_remove(`Transaction Description`, "For Month 07"))

tlist <- tlist %>% filter(!grepl("M7|For Month|Royal Free", `Account Code`)) %>% 
  mutate(surname = sub("(\\w+\\s+){1}", "", `Account Code`),
         surname = trimws(surname))

queries <- left_join(tlist, empl, by = c("surname", "unit")) %>% 
  filter(is.na(forenames), WTE != 0)

write.csv(queries, "~/Downloads/queries.csv", row.names = FALSE)
