library(jsonlite)
library(tidyverse)
library(stringr)
library(igraph)
library(sand)
library(RMySQL)
library(tnet)
library(ggraph)
library(lme4)
library(lmerTest)
library(car)
library(purrr)

# Collase with summarise
# bbc
# Source.. 
url_boiler <- function(df){
    df <- df %>%
        mutate(media_url = str_replace(media_url, "^(http|https)://","")) %>% 
        mutate(media_url = str_replace(media_url, "/$","")) %>%
        mutate(media_url = str_replace(media_url, "(.com)$", '')) %>%
        mutate(media_url = str_replace(media_url, "(.org)$", '')) %>%
        mutate(media_url = str_replace(media_url, "(.gov)$", '')) %>%
        mutate(media_url = str_replace(media_url, "(.edu)$", '')) %>%
        mutate(media_url = str_replace(media_url, "(.co.kr)$", '')) %>%
        mutate(media_url = str_replace(media_url, "^(www.)","")) %>% 
        mutate(media_url = str_replace(media_url, "(/#)$", "")) %>%
        mutate(media_url = str_replace(media_url, "(.com)$","")) %>%
        mutate(media_url = str_replace(media_url, "bbc.co.uk/\\?ok", "bbc")) %>%
        mutate(media_url = str_replace(media_url, "worldnetdaily.com/#spider", "wnd")) %>%
        mutate(media_url = str_replace(media_url, "boston.com/bostonglobe", "bostonglobe")) %>%
        mutate(media_url = str_replace(media_url, "online.wsj", "wsj")) %>%
        mutate(media_url = str_replace(media_url, "msnbc.msn", "msnbc")) %>%
        mutate(media_url = str_replace(media_url, "guardian.co.uk", "theguardian")) %>%
        mutate(media_url = str_replace(media_url, "thehill.com/rss/syndicator/19109", "thehill")) %>%
        mutate(media_url = str_replace(media_url, "washingtonexaminer.com/politics", "washingtonexaminer")) %>%
        mutate(media_url = str_replace(media_url, "theonion.com/content/index", "theonion")) %>%
        mutate(media_url = str_replace(media_url, "newrepublic.com#spide", "newrepublic")) %>%
        mutate(media_url = str_replace(media_url, "pbs.org/newshour", "pbs")) %>%
        mutate(media_url = str_replace(media_url, "people.com/news", "people")) %>%
        mutate(media_url = str_replace(media_url, "dailymail.co.uk/home/index.html", "dailymail.co.uk"))
    return(df)}


url_simplify <- function(url) {
    rules = list(washingtonpost.wufoo = "washingtonpost",
                 mobile.nytimes = "nytimes",
                 wapo.st = "washingtonpost",
                 stats.washingtonpost = "washingtonpost",
                 live.washingtonpost = "washingtonpost",
                 voices.washingtonpost = "washingtonpost",
                 apps.washingtonpost = "washingtonpost",
                 t.co = "twitter",
                 Twitter = "twitter",
                 blogs.wsj = "wsj",
                 mobile.twitter = "twitter",
                 realestate.washingtonpost = "washingtonpost",
                 query.nytimes = "nytimes",
                 cityroom.blogs.nytimes = "nytimes",
                 mobile.nytimes = "nytimes",
                 markets.on.nytimes = "nytimes",
                 p.nytimes = "nytimes",
                 health.nytimes = "nytimes",
                 autos.nytimes = "nytimes",
                 artsbeat.blogs.nytimes = "nytimes",
                 topics.nytimes = "nytimes",
                 lens.blogs.nytimes = "nytimes",
                 well.blogs.nytimes = "nytimes",
                 nytlive.nytimes = "nytimes",
                 nyti.ms  = "nytimes",
                 cnn.us11.list.manage = "cnn",
                 cnn.us11.list.manage1 = "cnn",
                 cnnespanol.cnn = "cnn",
                 edition.cnn = "cnn",
                 money.cnn = "cnn",
                 cnn.it = "cnn",
                 apple.news = "apple",
                 itunes.apple = "apple",
                 apple.news = "apple",
                 blog.google = "google",
                 goo.gl = "google",
                 docs.google = "google",
                 books.google = "google",
                 google.com.hk = "google",
                 play.google = "google",
                 books.google.co.uk = "google",
                 google.co.uk = "google",
                 scholoar.google = "google",
                 jigsaw.google = "google",
                 webcache.googleusercontent = "google",
                 trends.google = "google",
                 support.google = "google",
                 soprweb.senate = "senate",
                 cotton.senate = "senate",
                 energy.senate = "senate",
                 finance.senate = "senate",
                 amzn.to = "amazon",
                 amazon.co.uk = "amazon",
                 politico.us2.list.manage = "politico",
                 politico.eu = "politico",
                 politico.pro = "politico",
                 politicopro = "politico",
                 politi.co = "politico",
                 abokhari.breitbart = "breitbart",
                 news.huffingtonpost = "huffingtonpost",
                 huffingtonpost.co.uk = "huffingtonpost", 
                 huffingtonpost.in = "huffingtonpost",
                 fox13now = "foxnews",
                 fox8 = "foxnews",
                 fox25boston = "foxnews",
                 fox26houston = "foxnews",
                 fox10phoenix = "foxnews",
                 fox19 = "foxnews",
                 foxla = "foxnews",
                 fox5dc = "foxnews",
                 insider.foxnews = "foxnews",
                 fox6now = "foxnews",
                 fox5sandiego = "foxnews",
                 fox59 = "foxnews",
                 profootballtalk.nbcsports = "nbcnews",
                 media4.s.nbcnews = "nbcnews",
                 media1.s.nbcnews = "nbcnews",
                 media2.s.nbcnews = "nbcnews",
                 media3.s.nbcnews = "nbcnews",
                 nbcwashington = "nbcnews",
                 nbcchicago = "nbcnews",
                 nbcnews.to = "nbcnews",
                 secure.nbcnews = "nbcnews",
                 nbcasianamerica.tumblr = "nbcnews",
                 abc7news = "abcnews.go",
                 abcn.ws = "abcnews.go",
                 abc7 = "abcnews.go",
                 abc13 = "abcnews.go",
                 abc11 = "abcnews.go",
                 abcn.ws = "abcnews.go",
                 usatoday30.usatoday = "usatoday",
                 sports.usatoday = "usatoday",
                 letters.usatoday = "usatoday",
                 preview.usatoday = "usatoday",
                 ftw.usatoday = "usatoday",
                 register.theguardian = "theguardian",
                 bookshop.theguardian = "theguardian",
                 guardian.co.uk = "theguardian",
                 voxcom.createsend1 = "vox",
                 membership.theguardian = "theguardian",
                 play.spotify = "spotify", 
                 open.spotify = "spotify",
                 news.bbc.co.uk = "bbc",
                 bbc.co.uk = "bbc",
                 bbc.in = "bbc",
                 pages.email.bbc = "bbc",
                 msnbcmedia.msn = "msnbc",
                 msnbc.msn = "msnbc",
                 contact.buzzfeed = "buzzfeed",
                 ondemand.npr = "npr",
                 media.npr = "npr",
                 latimesblogs.latimes = "latimes",
                 mailto.david.cloud.latimes = "latimes",
                 latimes.us10.list.manage = "latimes",
                 documents.latimes = "latimes",
                 articles.latimes = "latimes",
                 recipes.latimes = "latimes",
                 spreadsheets.latimes = "latimes",
                 jeff.landa.latimes = "latimes",
                 checkout.latimes = "latimes",
                 hailey.branson.latimes = "latimes",
                 new.time = "time",
                 motto.time = "time",
                 thenypost.files.wordpress = "nypost",
                 projects.fivethirtyeight = "fivethrityeight",
                 realestate.usnews = "usnews",
                 creditcards.usnews = "usnews",
                 colleges.usnews.rankingsandreviews = "usnews",
                 articles.chicagotribune = "chicagotribune",
                 ad.chicagotribune = "chicagotribune",
                 archives.chicagotribune = "chicagotribune",
                 cms.ibtimes = "ibtimes",
                 superstore.wnd = "wnd",
                 celebritybabies.people = "people",
                 pages.email.people = "people",
                 video.foxnews = "foxnews",
                 aphis.usda = "usda",
                 nal.usda = "usda",
                 fs.usda = "usda",
                 youtu.be = "youtube",
                 link.mail.bloombergbusiness = "bloomberg"
    )
    if(url %in% names(rules)){
        out <- `[[`(rules, url)
    } else {
        out <- url
    }
    return(out)
}


url_json_reader <- function(json){
    dt <- lapply(readLines(json), fromJSON)
    
    dt_frm_origin <- dt %>%
        map(~ .[[1]]) %>%
        map(as.data.frame) %>%
        map(t) %>%
        map(~ data.frame(., "to" = row.names(.))) %>%
        tibble(data = .) %>%
        bind_cols(map_chr(dt, names) %>% tbl_df()) %>%
        unnest()
    
    names(dt_frm_origin) <- c("from", "freq", "to")
    
    dt_frm_reduced <- dt_frm_origin %>%
        filter(freq > 10) %>%
        mutate(from = str_replace(from, "^(http|https)://","")) %>%
        mutate(from = str_replace(from, "/$","")) %>%
        mutate(from = str_replace(from, "(.org)$", '')) %>%
        mutate(from = str_replace(from, "(.gov)$", '')) %>%
        mutate(from = str_replace(from, "(.edu)$", '')) %>%
        mutate(from = str_replace(from, "(.co.kr)$", '')) %>%
        mutate(to = str_replace(to, "^(http|https)://","")) %>%
        mutate(to = str_replace(to, "/$","")) %>%
        mutate(to = str_replace(to, "(.com)$", '')) %>%
        mutate(to = str_replace(to, "(.org)$", '')) %>%
        mutate(to = str_replace(to, "(.gov)$", '')) %>%
        mutate(to = str_replace(to, "(.edu)$", '')) %>%
        mutate(to = str_replace(to, "(.co.kr)$", '')) %>%
        mutate(from = str_replace(from, "^(www.)","")) %>%
        mutate(from = str_replace(from, "(.com)$","")) %>%
        mutate(to = str_replace(to, "^(www.)","")) %>%
        mutate(from = str_replace(from, "(/#)$", "")) %>%
        mutate(from = str_replace(from, "(.com)$", '')) %>%
        mutate(to = str_replace(to, "(.com)$","")) %>%
        mutate(from = str_replace(from, "bbc.co.uk/\\?ok", "bbc")) %>%
        mutate(from = str_replace(from, "worldnetdaily.com/#spider", "wnd")) %>%
        mutate(from = str_replace(from, "boston.com/bostonglobe", "bostonglobe")) %>%
        mutate(to = str_replace(to, "bbc.co.uk/\\?ok", "bbc")) %>%
        mutate(to = str_replace(to, "worldnetdaily.com/#spider", "wnd")) %>%
        mutate(to = str_replace(to, "boston.com/bostonglobe", "bostonglobe")) %>%
        mutate(from = str_replace(from, "thehill.com/rss/syndicator/19109", "thehill")) %>%
        mutate(from = str_replace(from, "washingtonexaminer.com/politics", "washingtonexaminer")) %>%
        mutate(from = str_replace(from, "theonion.com/content/index", "theonion")) %>%
        mutate(from = str_replace(from, "newrepublic.com#spide", "newrepublic")) %>%
        mutate(from = str_replace(from, "pbs.org/newshour", "pbs")) %>%
        mutate(from = str_replace(from, "people.com/news", "people")) %>%
        mutate(from = str_replace(from, "dailymail.co.uk/home/index.html", "dailymail.co.uk")) %>%
        mutate(to = str_replace(to, "thehill.com/rss/syndicator/19109", "thehill")) %>%
        mutate(to = str_replace(to, "washingtonexaminer.com/politics", "washingtonexaminer")) %>%
        mutate(to = str_replace(to, "theonion.com/content/index", "theonion")) %>%
        mutate(to = str_replace(to, "newrepublic.com#spide", "newrepublic")) %>%
        mutate(to = str_replace(to, "pbs.org/newshour", "pbs")) %>%
        mutate(to = str_replace(to, "people.com/news", "people")) %>%
        mutate(to = str_replace(to, "dailymail.co.uk/home/index.html", "dailymail.co.uk"))
        
        
    #df_frm_reduced <- url_boiler(df_frm_reduced)
        
    dt_frm_reduced$to <- sapply(dt_frm_reduced$to, url_simplify)
    dt_frm_reduced$from <- sapply(dt_frm_reduced$from, url_simplify)
    
    dt_frm_reduced <- dt_frm_reduced %>%
        group_by(from, to) %>%
        summarise(freq = sum(freq))
    
    return(dt_frm_reduced)
}