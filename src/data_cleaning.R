# Libraries

library(tidyverse)
library(readxl)
library(here)
library(readabs)


raw_italy <- read_excel(paste0(here(),"/data/raw/home_affairs_italians.xlsx"))

data_italy <- raw_italy %>% group_by(`Census Year`) %>%
  summarise(
    `Percentage of Total overseas born` = first(na.omit(Percent)),
    Rank = first(na.omit(Rank)),
    `Italian born` = Number[Birthplace == "Italy"],
    `Total overseas born` = Number[Birthplace == "Total overseas born"],
    `Total population` = Number[Birthplace == "Total population"],
    .groups = "drop"
  )

write_csv(data_italy, file = paste0(here(),"/data/clean/data_italy.csv"))


raw_est_cacs <- read_excel(paste0(here(),"/data/raw/H4511B Cafes and Coffee Shops in Australia Industry Report.xlsx"), sheet = "Business Locations Region Data") %>%
  rename(State = "State/Territory")
raw_est_coff <- read_excel(paste0(here(),"/data/raw/OD5381 Coffee Shops in Australia Industry Report.xlsx"), sheet = "Business Locations Region Data")
raw_est_cbd <- read_excel(paste0(here(),"/data/raw/OD5477 Coffee Bean Distributors in Australia Industry Report.xlsx"), sheet = "Business Locations Region Data")
raw_est_tcp <- read_excel(paste0(here(),"/data/raw/OD5293 Tea  Coffee Production in Australia Industry Report.xlsx"), sheet = "Business Locations Region Data")
raw_pop <- read_excel(paste0(here(),"/data/raw/31010do001_202512.xlsx"), sheet = "Table_3", skip = 5)

data_pop <- raw_pop %>% rename(State = `...1`, Population = `2025...5`) %>% select(State, Population) %>%
  mutate(State = case_when(
    State == "New South Wales" ~ "NSW",
    State =="Victoria" ~ "VIC",
    State =="Queensland" ~ "QLD",
    State =="South Australia" ~ "SA",
    State =="Western Australia" ~ "WA",
    State =="Tasmania" ~ "TAS",
    State =="Northern Territory" ~ "NT",
    State =="Australian Capital Territory"~ "ACT", .default = NA)) %>% filter(!is.na(State))

raw_data_caf <- raw_est_cacs %>% rename(State = "State/Territory")
raw_data_caf <- left_join(raw_est_cacs %>% rename(State = "State/Territory"),raw_est_coff, join_by(State)) %>%
  mutate(Estab.Units = Estab.Units.x - Estab.Units.y) %>% select(c(State,Estab.Units))

detail_data <- function(raw_data) {
  data <- raw_data %>% left_join(data_pop, by = "State") %>%
  mutate("Percentage of Establishments" = round((Estab.Units/sum(Estab.Units))*100, digits = 1),
         Population = as.integer(Population),
         Establishments = Estab.Units,
         "Establishments Per 10,000" = round(Estab.Units/(sum(Population)/10000),digits = 2),
         "Percentage of Population" = round((Population/sum(Population))*100, digits = 2)) %>%
  select(c("State",Establishments,"Percentage of Establishments","Percentage of Population", "Establishments Per 10,000"))
  data
}

data_est <- bind_rows(detail_data(raw_data_caf) %>% mutate("Industry" = "Cafes"),
                      detail_data(raw_est_coff) %>% mutate("Industry" = "Coffee Shops"),
                      detail_data(raw_est_cbd) %>% mutate("Industry" = "Coffee Bean Distributers"),
                      detail_data(raw_est_tcp) %>% mutate("Industry" = "Tea and Coffee Producers"),
                      detail_data(raw_est_cacs) %>% mutate("Industry" = "Cafes and Coffee Shops"))
  

write_csv(data_est, file = paste0(here(),"/data/clean/data_est.csv"))


