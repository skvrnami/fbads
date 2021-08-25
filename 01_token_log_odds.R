library(dplyr)
library(udpipe)
library(tidylo)
library(ggplot2)
library(patchwork)

# https://doi.org/10.1093/pan/mpn018

parties_pages <- readRDS("output/party_profiles.RData")

parties_ads_files <- list.files("output/", "parties_ads*", full.names = TRUE)
leaders_ads_files <- list.files("output/", "leaders_ads*", full.names = TRUE)

read_ad_files <- function(party_files){
    purrr::map(party_files, function(x) readRDS(x) %>% 
                   mutate(date = as.Date(stringr::str_extract(x, "[0-9]{4}-[0-9]{2}-[0-9]{2}")))) %>%
        bind_rows() %>%
        group_by(adlib_id) %>%
        arrange(desc(date)) %>%
        filter(row_number() == 1) %>%
        ungroup
}

parties_ads <- read_ad_files(parties_ads_files) %>%
    full_join(., parties_pages, by = "page_id") %>%
    select(-party_leader_id)
leaders_ads <- read_ad_files(leaders_ads_files) %>%
    full_join(., parties_pages %>% select(-page_id), by = c("page_id"="party_leader_id")) %>%
    mutate(party = ifelse(is.na(party), "Oath (Přísaha)", party))

all_ads <- bind_rows(parties_ads, leaders_ads)

unique_ads <- all_ads %>%
    select(ad_creation_time, ad_creative_body, party, page_name) %>%
    filter(ad_creation_time >= "2021-01-01") %>%
    unique %>%
    mutate(doc_id = row_number())

write.csv(unique_ads, file = "output/unique_ads.csv", row.names = FALSE)

# Lemmatize ads
udpipe_file <- list.files(".", "*.udpipe")
if(length(udpipe_file)){
    ud_model <- udpipe_load_model(udpipe_file) 
}else{
    ud_model <- udpipe_download_model(language = "czech")    
    ud_model <- udpipe_load_model(ud_model$file_model)
}

x <- udpipe_annotate(ud_model, 
                     x = unique_ads$ad_creative_body, 
                     doc_id = unique_ads$doc_id)
lemmas <- as.data.frame(x)
lemmas_df <- lemmas %>%
    filter(upos %in% c("ADJ", "ADV", "NOUN", "PROPN", "SYM", "VERB")) %>%
    left_join(unique_ads %>% mutate(doc_id = as.character(doc_id)), by = "doc_id")

lemma_log_odds <- lemmas_df %>% 
    count(party, lemma) %>% 
    bind_log_odds(party, lemma, n) 

write.csv(lemma_log_odds, file = "output/fb_ads_words.csv", row.names = FALSE)

ad_parties <- unique(lemma_log_odds$party)

chart_log_odds <- function(df = lemma_log_odds, party = ad_parties[1]){
    party_name <- party
    PARTY_COLS <- c("ANO" = "#00BFFF", 
                    "Social Democracy (ČSSD)" = "#FF8C00", 
                    "Communists (KSČM)" = "#FF0000", 
                    "TOP 09" = "#800080", 
                    "Civic Democrats (ODS)" = "#0000FF", 
                    "Christian Democrats (KDU-ČSL)" = "#e6ac21", 
                    "SPD" = "#8B4513", 
                    "Mayors and Independents (STAN)" = "#99FFCC",
                    "Pirates (Piráti)" = "#000000", 
                    "Oath (Přísaha)"="#0033FF")
    
    tmp <- df %>%
        filter(party == party_name) %>%
        arrange(desc(log_odds_weighted)) %>%
        head(20)
    
    tmp %>%
        ggplot(., aes(x = reorder(lemma, log_odds_weighted), y = log_odds_weighted)) + 
        geom_bar(stat = "identity", fill = PARTY_COLS[party_name]) + 
        coord_flip() + 
        theme_minimal() + 
        labs(y = "Weighted log odds", x = "Lemma", 
             title = glue::glue("{party}"))
} 

parties_charts <- purrr::map(ad_parties, function(x) chart_log_odds(party = x))

(parties_charts[[1]] | parties_charts[[2]] | parties_charts[[3]]) / 
    (parties_charts[[4]] | parties_charts[[5]] | parties_charts[[6]]) / 
    (parties_charts[[7]] | parties_charts[[8]] | parties_charts[[9]]) / 
    (parties_charts[[10]] | plot_spacer() | plot_spacer()) + 
    plot_annotation(title = 'Log of odds of words used in FB Ads by parties')

ggsave("output/charts/log-odds.png", width = 12, height = 8)
