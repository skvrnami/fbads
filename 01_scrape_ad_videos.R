library(rvest)
library(RSelenium)
library(Radlibrary)

access_token <- token_get()$token
# leaders_ads <- readRDS("output/leaders_ads_2021-09-01.RData")
# parties_ads <- readRDS("output/parties_ads_2021-09-01.RData")

url <- glue::glue("https://www.facebook.com/ads/archive/render_ad/?id=1040437186723679&access_token={access_token}")

remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445L,
    browserName = "firefox"
)

download_video <- function(remote_driver, url){
    id <- stringr::str_extract(url, "(?<=id=)[0-9]+")
    
    remote_driver$open()
    remote_driver$navigate(url)
    html_source <- remote_driver$getPageSource()[[1]] %>%
        read_html()
    
    video_src <- html_source %>%
        html_node("video") %>%
        html_attr("src")
    
    if(length(video_src)){
        download.file(video_src, destfile = paste0(id, ".mp4"))
    }
    
    remote_driver$close()
}

download_video(remDr, url)
