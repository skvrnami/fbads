library(here)
library(dplyr)
library(haven)
library(udpipe)
library(tidylo)
library(ggplot2)

select_iccp_vars <- function(df){
    df %>%
        select(SEX, AGE, matches("^IP[0-9]"))
}

recode_age_categories <- function(df){
    df %>%
        mutate(age_category = case_when(
            between(AGE, 13, 17) ~ "13-17", 
            between(AGE, 18, 24) ~ "18-24",
            between(AGE, 25, 34) ~ "25-34",
            between(AGE, 35, 44) ~ "35-44",
            between(AGE, 45, 54) ~ "45-54",
            between(AGE, 55, 64) ~ "55-64",
            AGE >= 65 ~ "65+", 
            TRUE ~ "unknown"
        ))
}

iccp_wave1 <- read_spss(here("data", "data_FINAL_V01kli.sav")) %>%
    select_iccp_vars() %>%
    mutate(SEX = ifelse(SEX == 1, "male", "female")) %>%
    recode_age_categories()

same_sex_marriage_agreement <- iccp_wave1 %>%
    group_by(SEX, age_category) %>%
    summarise(IP6_agree = mean(IP6 <= 3), 
              n_respondents = n()) %>% View

# parties_pages <- readRDS("output/party_profiles.RData")

parties_ads_files <- list.files("output/", "parties_ads*", full.names = TRUE)
leaders_ads_files <- list.files("output/", "leaders_ads*", full.names = TRUE)

parties_demo_files <- list.files("output/", "parties_demo*", full.names = TRUE)
leaders_demo_files <- list.files("output/", "leaders_demo*", full.names = TRUE)

read_ad_files <- function(party_files){
    purrr::map(party_files, function(x) readRDS(x) %>% 
                   mutate(date = as.Date(stringr::str_extract(x, "[0-9]{4}-[0-9]{2}-[0-9]{2}")))) %>%
        bind_rows() %>%
        group_by(adlib_id) %>%
        arrange(desc(date)) %>%
        filter(row_number() == 1) %>%
        ungroup
}

read_demo_files <- function(demo_files){
    purrr::map(demo_files, function(x) readRDS(x) %>% 
                   mutate(date = as.Date(stringr::str_extract(x, "[0-9]{4}-[0-9]{2}-[0-9]{2}")))) %>%
        bind_rows() %>%
        group_by(adlib_id, age, gender) %>%
        arrange(desc(date)) %>%
        filter(row_number() == 1) %>%
        ungroup
}

parties_ads <- read_ad_files(parties_ads_files)
parties_demo <- read_demo_files(parties_demo_files)

same_sex_marriage_ads <- parties_ads %>% 
    filter(grepl("stejnopohl|manželství pro všechny", ad_creative_body))

parties_demo %>%
    filter(adlib_id %in% same_sex_marriage_ads$adlib_id) %>% View
