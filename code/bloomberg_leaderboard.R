###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###   Header ----
###   DRAFT
###   PRIVILEGED AND CONFIDENTIAL
###   PREPARED AT THE REQUEST OF COUNSEL
###
###   CASE NUMBER: 
###   PURPOSE:  
###   AUTHOR: Peter Kress 
###
###   AUDITED: NO
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##############################-
###  Initialize workspace  ----
##############################-

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load(data.table, stringr, ggplot2, digest)

`%p%` = paste0

############################-
### Create anonymizer function ----
############################-

anonymize <- function(x, algo="crc32"){
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  unname(unq_hashes[x])
}

############################-
### Read In Emails ----
############################-

## Read in un-anonymized emails
emails = fread("input/bloomberg_chain_2021_01_25.txt", sep = "\n", header = F)

## create From and Send date fields by casting data to wide format
emails_cln = emails[
  grepl(":", V1)
  ][
  , tstrsplit(V1, ": ")
  ][
  , .(field = V1, info = V2%p%fifelse(is.na(V3), "", ": "%p%V3))
  ][
  field%in%c("From", "Sent")
  ][
  field=="From"
  , email:=.I
  ][
  , email:=nafill(email, "locf")
  ] %>% 
  dcast.data.table(email~field, value.var = "info")

## Drop any full emails for people outside my contacts list
emails_cln[
  , From:=gsub(" <.{1,}>", "", From)
]

## anonymize data
emails_anon = copy(emails_cln)[
  , From:=anonymize(From)
]
## Convert to date
emails_anon[
  , date:=as.POSIXct(Sent, format = "%A, %B %d, %Y %R %p")
  ]

## filter to 2020 season
emails_anon = emails_anon[
  year(date)==2020
]

############################-
### Create summary table and charts ----
############################-

total_ranking = emails_anon[
  , .N
  , From
  ][
  order(-N)
  ][
  , share:=N/sum(N)
  ][
  , ranking:=.I
  ][
  order(N)
  ][
  , From := factor(From, levels = From)
  ] %>% print()

fwrite(total_ranking, "output/anon_total_ranking_list.csv")

top_15 = total_ranking[1:15]
top_5 = emails_anon[, .N, From][order(-N)][1:5, .(From, rank = .I)]

total_ranking_dist = total_ranking %>% 
  ggplot()+
  geom_point(aes(x = From, y = N, color = ranking<=5), size = 4, show.legend = F)+
  geom_segment(aes(x = From, xend = From, y = 0, yend = N, color = ranking<=5), size = 1.25, show.legend = F)+
  coord_flip()+
  labs(x = "Contributors", y = "Total Messages", fill = "In Top 5"
       , title = "Bloomberg 2020: Total Messages by Contributor")+
  theme_bw()
ggsave("output/anon_total_ranking_dist.png", width = 6, height = 8)

monthly_top = emails_anon[
  top_5
  , rank:=i.rank
  , on = "From"
  ][
  , .N
  , .(month = paste0(year(date), "-", fifelse(month(date)<10, "0"%p%month(date), as.character(month(date))))
      , From = fifelse(From%in%top_5$From, rank%p%": "%p%From, "Other"))
  ][
  order(month, -N)
  ][
  , share:=N/sum(N)
  , month
  ]

monthly_top %>% 
  ggplot()+
  geom_col(aes(x = month, y = share, fill = From), color = "white")+
  scale_y_continuous(labels = function(x){paste0(round(100*x),"%")})+
  labs(x = "Month", y = "Share of Emails", fill = "Sender", title = "Bloomberg 2020: Monthly Leaderboard")+
  theme_bw()+
  theme(legend.position = "bottom"
        , legend.title = element_blank())
ggsave("output/Anonymous Monthly Leaderboard.png", width = 8, height = 6)
