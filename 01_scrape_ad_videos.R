library(rvest)
library(RSelenium)
library(Radlibrary)

leaders_ads <- readRDS("output/leaders_ads_2021-09-08.RData")
# parties_ads <- readRDS("output/parties_ads_2021-09-01.RData")

babis <- leaders_ads %>%
    filter(page_name == "Andrej Babiš")

remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445L,
    browserName = "firefox"
)

download_video <- function(remote_driver, url, folder = "videos/"){
    id <- stringr::str_extract(url, "(?<=id=)[0-9]+")
    
    remote_driver$open()
    remote_driver$navigate(url)
    html_source <- remote_driver$getPageSource()[[1]] %>%
        read_html()
    
    video_src <- html_source %>%
        html_node("video") %>%
        html_attr("src")
    
    if(length(video_src) & !is.na(video_src)){
        download.file(video_src, destfile = paste0(folder, id, ".mp4"))
    }
    
    remote_driver$close()
}

options(timeout = max(300, getOption("timeout")))

for(i in babis$ad_snapshot_url){
    Sys.sleep(sample(2:10, 1))
    url <- glue::glue(i)
    cat(i)
    download_video(remDr, url)    
}


