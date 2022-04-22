library(tidyverse)
library(magrittr)
library(lubridate)
library(tidyr)
library(rvest)
library(janitor)

nato_members <- read_html("https://en.wikipedia.org/wiki/Member_states_of_NATO")

nato_tables <- nato_members %>% html_table(header = TRUE, fill = TRUE)

nato_member_joined <- nato_tables[[1]]

nato_member_joined %>% 
  clean_names() %>% 
  select(country = member_state, 
         accession = accession_3) %>% 
  mutate(member_2020 = 2020,
         country = sub("\\[.*", "", country),
         accession = sub("\\[.*", "", accession),
         accession = parse_date_time(accession, "dmy"),
         accession = format(as.Date(accession, format = "%d/%m/%Y"),"%Y"),
         accession = as.numeric(as.character(accession))) %>% 
  pivot_longer(!country, names_to = "event", values_to = "year") %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  mutate(year = ymd(year)) %>% 
  complete(country, year = seq.Date(min(year), max(year), by = "year")) %>% 
  mutate(nato_member = ifelse(event == "accession", 1, 
                              ifelse(event == "member_2020", 1, 0))) %>% 
  group_by(country) %>% 
  fill(nato_member, .direction = "down") %>%
  ungroup() %>% 
  mutate(nato_member = replace_na(nato_member, 0),
         year = parse_number(as.character(year)),
         event = ifelse(nato_member == 0, "not member", event),
         event = ifelse(nato_member == 1 & is.na(event), "member", event),
         event = ifelse(event == "member_2020", "member", event))  %>% 
  distinct(country, year, .keep_all = TRUE) -> nato_panel
  