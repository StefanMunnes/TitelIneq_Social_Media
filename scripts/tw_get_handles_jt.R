library(dplyr)
library(stringr)
library(stringi)
library(rvest)
library(academictwitteR)
library(tidyverse)


# ---- 1. get list of names ----

# 1.1 names from Hochschullehrer Verzeichnis UniversitC$ten Deutschland (De Gruyter)
hlvu <- readRDS("C:/Users/Jasper Tjaden/OneDrive/work/Studies/Working/title discrimination/github/Data/emails.RDS") %>% 
  pull(name)
hlvu

# 1.2 names from Bundestag
mdbs <- readRDS("C:/Users/Jasper Tjaden/OneDrive/work/Studies/Working/title discrimination/github/Data/mdbs.RDS") %>% 
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

names

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

## Create list of handles

# first, create dataframe and restructure
tw_handles_df <- plyr::ldply(tw_handles, rbind) %>%
  pivot_longer(2:21, names_to = "num", values_to = "handles") %>%
  filter(!is.na(handles)) %>% mutate(handles = str_remove_all(handles, "@"))

# second, create list of handles (just take 10)
tw_handles_ls <- tw_handles_df %>% select(handles) %>% top_n(50) %>%
  unlist()

### feed handles into Twitter API

# get user ID first (example with 2)
users <- c("JasperTjaden", "llhipp")
get_user_id(users)

# all from list

# get Twitter ids
tw_id =get_user_id(tw_handles_ls)

# get profiles using ids
tw_profile= get_user_profile(tw_id)

# combine into df
tw_df <- data.frame(tw_handles_ls, tw_id, tw_profile)

# merge with full list
tw_handles_df1 <- merge(tw_handles_df, tw_df, by.x = "handles",
                        by.y="tw_handles_ls",all.y = TRUE)


# Alternative mit Twitter API und rtweet package:
# https://rdrr.io/cran/rtweet/man/lookup_users.html


# Markus R. Meyer - 4x
# Hans-Joachim Lang - falscher Account