library(jsonlite)
library(xml2)
library(dplyr)
library(rvest)
library(httr)
library(stringr)

###Get list of gameids
setwd("H:/Documents/R/Volleyball")
gameids <- character(0)
gamedays <- seq.Date(as.Date("2023-08-25"),Sys.Date()-1,1)
mos   <- substr(gamedays,6, 7)
dys   <- substr(gamedays,9,10)
ndays <- length(mos)

for(i in 1:ndays){
  url  <- sprintf("https://www.ncaa.com/scoreboard/volleyball-women/d1/2023/%s/%s/all-conf",mos[i],dys[i])
  x    <- GET(url,add_headers('user-agent'='r'))
  gameids_day <- str_match_all(as.character(x),'=\"/game/\\s*(.*?)\\s*\">\n')[[1]][,2]
  gameids <- c(gameids,gameids_day)
}
missing_ids <- c(6174516,6174529,6174983,6175487)
gameids <- gameids[!(gameids %in% missing_ids)]

###Get PBP data
first_match <- TRUE
for(id in gameids){
  url <- sprintf("https://data.ncaa.com/casablanca/game/%s/pbp.json",id)
  #url_exists <- url.exists(url) ###DOES NOT WORK FOR JSON FILES
  url_exists <- TRUE
  if(url_exists == TRUE){
    pbp <- read_json(url)
    
    team1 <- pbp$meta$teams[[1]]$shortName
    team2 <- pbp$meta$teams[[2]]$shortName
    team1_home <- pbp$meta$teams[[1]]$homeTeam
    team2_home <- pbp$meta$teams[[2]]$homeTeam
    
    nsets <- length(pbp$periods)
    if(nsets > 0){
      for(i in 1:nsets){
        if(i == 1){ 
          match_pbp <- as.data.frame(do.call(rbind,pbp$periods[[i]]$playStats)) 
          match_pbp$set <- rep(i,dim(match_pbp)[1])
        } 
        else {
          set_pbp   <- as.data.frame(do.call(rbind,pbp$periods[[i]]$playStats))
          set_pbp$set <- rep(i,dim(set_pbp)[1])
          match_pbp <- rbind(match_pbp,set_pbp)
        }
      }
    }
    match_pbp$team1      <- rep(team1,dim(match_pbp)[1])
    match_pbp$team2      <- rep(team2,dim(match_pbp)[1])
    match_pbp$team1_home <- rep(team1_home,dim(match_pbp)[1])
    match_pbp$team2_home <- rep(team2_home,dim(match_pbp)[1])
    match_pbp$gameid     <- rep(id,dim(match_pbp)[1])
    if(first_match == TRUE){ full_pbp <- match_pbp } else { full_pbp <- rbind(full_pbp,match_pbp) }
    first_match <- FALSE
  }
}

###Clean up PBP data, remove bad data, add information
full_pbp_sub         <- subset(full_pbp,score != "" & score != "-")
score_splt           <- as.numeric(unlist(strsplit(unlist(full_pbp_sub$score),"-")))
full_pbp_sub$team1_score <- score_splt[c(FALSE,TRUE)]
full_pbp_sub$team2_score <- score_splt[c(TRUE,FALSE)]
full_pbp_sub$team1_point <- 1-as.numeric(full_pbp_sub$homeText == "")
full_pbp_sub$team2_point <- 1-as.numeric(full_pbp_sub$visitorText == "")

set_scores <- as.data.frame(
  full_pbp_sub %>% group_by(gameid,set) %>% summarize(team1_setscore = max(team1_score),team2_setscore = max(team2_score),ptsinset = n())
)

badgames <- unique(c(subset(set_scores,set <= 4 & team1_setscore <= 24 & team2_setscore <= 24)$gameid,subset(set_scores,set == 5 & team1_setscore <= 14 & team2_setscore <= 14)$gameid))
full_pbp_sub <- subset(full_pbp_sub,!(gameid %in% badgames))
full_pbp_sub <- merge(full_pbp_sub,set_scores)
full_pbp_sub$team1_winset <- as.numeric(full_pbp_sub$team1_setscore > full_pbp_sub$team2_setscore)

full_pbp_sub <- as.data.frame(full_pbp_sub %>% arrange(gameid,set,team1_score,team2_score) %>% group_by(gameid,set) %>% mutate(pointnum = seq(n())))

curpt_serve <- numeric(dim(full_pbp_sub)[1])
nxtpt_serve <- numeric(dim(full_pbp_sub)[1])
for(k in 1:dim(full_pbp_sub)[1]){
  if(full_pbp_sub$pointnum[k] > 1){
    if(full_pbp_sub$team1_point[k-1] == 1){ curpt_serve[k] <- 1 } else { curpt_serve[k] <- 2 }
  }
  if(full_pbp_sub$team1_point[k]   == 1){ nxtpt_serve[k] <- 1 } else { nxtpt_serve[k] <- 2 }
  if(full_pbp_sub$pointnum[k] == full_pbp_sub$ptsinset[k]){ nxtpt_serve[k] <- 0 }
}
full_pbp_sub$curpt_serve <- curpt_serve
full_pbp_sub$nxtpt_serve <- nxtpt_serve

full_pbp_sub <- as.data.frame(lapply(full_pbp_sub,unlist))
write.csv(full_pbp_sub,file="full_ncaa_vball_pbp.csv",row.names=FALSE)


###Win Probability
#win_prob <- as.data.frame(
#  full_pbp_sub %>% group_by(team1_score,team2_score,set,nxtpt_serve) %>% summarize(team1_set_winprob = mean(team1_winset), n_matches = n())
#)

#pt_prob <- as.data.frame(
#  full_pbp_sub %>% group_by(set,curpt_serve) %>% summarize(team1_pt_winprob = mean(team1_point), n_points = n())
#)