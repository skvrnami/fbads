library(dplyr)
library(Radlibrary)

# Script for downloading ads data from party and party leaders pages

(expiration <- token_get()$expiry)
# source("config.R")

# https://findfb.id/

# TODO: Publisher platform?
# The platform on which the ads appeared. One or more of "FACEBOOK", "INSTAGRAM", 
# "AUDIENCE_NETWORK", "MESSENGER", "WHATSAPP".
# By default only "FACEBOOK"

tibble(
    party = c(
        "Mayors and Independents (STAN)", "Social Democracy (ČSSD)", 
        "Civic Democrats (ODS)", "ANO", 
        "Pirates (Piráti)", "Communists (KSČM)", 
        "TOP 09", "SPD", "Christian Democrats (KDU-ČSL)"
    ),
    page_id = as.character(c(
        370583064327, # STAN
        111041662264882, # ČSSD
        30575632699, # ODS
        211401918930049, # ANO
        109323929038, # Piráti
        298789466930469, # KSČM
        90002267161, # TOP 09
        937443906286455, # SPD
        251656685576 # KDU-ČSL
    )), 
    party_leader_id = as.character(c(
        1477535869227488, # Rakušan
        129237636702, # Hamáček
        487445514669670, # Fiala
        214827221987263, # Babiš
        NA, # Bartoš
        119687288907752, # Filip
        511497365600515, # Pekarová Adamová
        179497582061065, # Okamura
        356451014434612 # Jurečka
    ))
) -> party_ids

# saveRDS(party_ids, "output/party_profiles.RData")

party_leaders <- party_ids %>% filter(!is.na(party_leader_id)) %>% pull(party_leader_id)
# search_pages <- c(party_ids$page_id, party_leaders)

build_query <- function(page_ids, 
                        countries = "CZ", 
                        activity_status = "ALL",
                        type = "POLITICAL_AND_ISSUE_ADS", 
                        limit = 1000, 
                        fields, 
                        ...){
    adlib_build_query(ad_reached_countries = countries, 
                      ad_active_status = activity_status, 
                      ad_type = type, 
                      search_page_ids = page_ids,
                      limit = limit, 
                      fields = fields, 
                      ...)
}

get_ads_data <- function(page_ids, ...){
    build_query(page_ids,
                fields = c("ad_data"), 
                ...) %>% adlib_get %>%
        ad_table(censor_access_token = TRUE)
}

get_demo_data <- function(page_ids, ...){
    build_query(page_ids,
                fields = c("demographic_data"), 
                ...) %>% adlib_get %>%
        demographic_table()
}

get_region_data <- function(page_ids, ...){
    build_query(page_ids,
                fields = c("region_data"), 
                ...) %>% adlib_get %>%
        region_table()
}

parties_ads <- get_ads_data(page_ids = party_ids$page_id, limit = 1000)
parties_demo <- get_demo_data(page_ids = party_ids$page_id)
parties_region <- get_region_data(page_ids = party_ids$page_id)

leaders_ads <- get_ads_data(page_ids = party_leaders)
leaders_demo <- get_demo_data(page_ids = party_leaders)
leaders_region <- get_region_data(page_ids = party_leaders)

today <- as.character(as.Date(Sys.Date()))

saveRDS(parties_ads, file = glue::glue("output/parties_ads_{today}.RData"))
saveRDS(parties_demo, file = glue::glue("output/parties_demo_{today}.RData"))
saveRDS(parties_region, file = glue::glue("output/parties_region_{today}.RData"))

saveRDS(leaders_ads, file = glue::glue("output/leaders_ads_{today}.RData"))
saveRDS(leaders_demo, file = glue::glue("output/leaders_demo_{today}.RData"))
saveRDS(leaders_region, file = glue::glue("output/leaders_region_{today}.RData"))
