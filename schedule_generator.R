library(jsonlite)
library(xml2)
library(dplyr)
library(rvest)
library(httr)
library(stringr)

###Get list of gameids
setwd("H:/Documents/R/Volleyball")
rankings <- read.csv(file="ncaa_vb_rankings_current.csv",stringsAsFactors=FALSE)
team_ratings <- read.csv(file="ncaa_volleyball_ratings.csv",stringsAsFactors=FALSE)
homeimp  <- 0.02661852
iter     <- 10
gameids  <- character(0)
gamedays <- seq.Date(Sys.Date(),as.Date("2023-11-26"),1)
mos   <- substr(gamedays,6, 7)
dys   <- substr(gamedays,9,10)
ndays <- length(mos)
schedule <- data.frame(month=character(0),day=character(0),
                       home=character(0),away=character(0),
                       homerk=character(0),awayrk=character(0),
                       stringsAsFactors=FALSE)

for(i in 1:ndays){
  url  <- sprintf("https://www.ncaa.com/scoreboard/volleyball-women/d1/2023/%s/%s/all-conf",mos[i],dys[i])
  x    <- GET(url,add_headers('user-agent'='r'))
  gameid_locs <- unlist(gregexpr("href=\"/game/", as.character(x)))
  teams <- str_match_all(as.character(x),'gamePod-game-team-name\">\\s*(.*?)\\s*</span>')[[1]][,2]
  ranks <- str_match_all(as.character(x),'gamePod-game-team-rank\">\\s*(.*?)\\s*</span>')[[1]][,2]
  if(length(teams) > 0){
    day_schedule <- data.frame(month=rep(mos[i],length(teams)/2),day=rep(dys[i],length(teams)/2),
                               home=teams[c(FALSE,TRUE)],away=teams[c(TRUE,FALSE)],
                               homerk=ranks[c(FALSE,TRUE)],awayrk=ranks[c(TRUE,FALSE)],
                               stringsAsFactors=FALSE)
    schedule <- rbind(schedule,day_schedule)
  }
}

schedule$date <- as.Date(paste("2023",schedule$month,schedule$day,sep="-"))
schedule$hometrk <- rankings$rank[match(schedule$home,rankings$teams)]
schedule$awaytrk <- rankings$rank[match(schedule$away,rankings$teams)]
schedule$homeserv <- team_ratings$serve_rat[match(schedule$home,team_ratings$team)]
schedule$homerecv <- team_ratings$reciv_rat[match(schedule$home,team_ratings$team)]
schedule$awayserv <- team_ratings$serve_rat[match(schedule$away,team_ratings$team)]
schedule$awayrecv <- team_ratings$reciv_rat[match(schedule$away,team_ratings$team)]
schedule <- subset(schedule,!(is.na(homeserv) | is.na(awayserv)))
schedule$homeservprob <- 1/(1+exp(schedule$awayrecv - schedule$homeserv - homeimp))
schedule$awayservprob <- 1/(1+exp(schedule$homerecv - schedule$awayserv + homeimp))
write.csv(schedule,file="ncaa_vb_schedule.csv",row.names=FALSE)

###Iter =   10:   12.40
###Iter =  100:  113.08
###Iter = 1000: 1073.22