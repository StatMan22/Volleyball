###Read in Data
library(dplyr)
setwd("H:/Documents/R/Volleyball")
vball_pbp <- read.csv("full_ncaa_vball_pbp.csv",stringsAsFactors=FALSE)

###Define sets/winners
vball_pbp_sets <- as.data.frame(
  vball_pbp %>% group_by(set,gameid,team1,team2) %>% summarize(team1pts = max(team1_setscore), team2pts = max(team2_setscore), 
                                                               team1setwon = as.numeric(team1pts>team2pts), team2setwon = as.numeric(team2pts>team1pts))
)

vball_pbp_matches <- as.data.frame(
  vball_pbp_sets %>% group_by(gameid,team1,team2) %>% summarize(team1sets = sum(team1setwon), team2sets = sum(team2setwon),
                                                                team1win  = as.numeric(team1sets == 3), team2win  = as.numeric(team2sets == 3))
)

write.csv(vball_pbp_matches,"ncaa_vb_results.csv",row.names=FALSE)

vball_pbp_setsmatches <- merge(vball_pbp_sets,vball_pbp_matches)
write.csv(vball_pbp_setsmatches,file="ncaa_vb_detail_results.csv",row.names=FALSE)

comeback_02 <- as.data.frame(
  vball_pbp_setsmatches %>% group_by(gameid,team1,team2,)
)