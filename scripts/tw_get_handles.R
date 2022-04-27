library(dplyr)
library(stringr)
library(stringi)
library(rvest)


# ---- 1. get list of names ----

# 1.1 names from Hochschullehrer Verzeichnis Universitäten Deutschland (De Gruyter)
hlvu <- readRDS("../data/emails.RDS") %>% 
  pull(name)


# 1.2 names from Bundestag
mdbs <- readRDS("../data/mdbs.RDS") %>% 
  filter(wp >= 19, str_detect(anrede_titel, "Dr|Prof")) %>% 
  transmute(name = paste(vorname, praefix, nachname) %>% 
              str_remove("NA ")) %>%
  distinct() %>% 
  pull()


# 1.3 prepare list of names
names <- c(hlvu, mdbs) %>%
  sample(size = 500) %>% 
  str_replace_all(" ", "+") %>%
  stri_trans_general("de-ascii") # remove Umlaute



# ---- 2. scrape twitter handles for given list of names

url_tw <- "https://nitter.net/search?f=users&q="

tw_handles <- lapply(names, function(name) {
  
  message(name)
  
  Sys.sleep(1)
  
  read_html(paste0(url_tw, name)) %>%
    html_nodes(".username") %>%
    html_text()
})

names(tw_handles) <- names

plot(table(sapply(tw_handles, length)))


# Alternative mit Twitter API und rtweet package:
# https://rdrr.io/cran/rtweet/man/lookup_users.html


# Markus R. Meyer - 4x
# Hans-Joachim Lang - falscher Account