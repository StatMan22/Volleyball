###Read In Data
setwd("H:/Documents/R/Volleyball")
team_ratings <- read.csv(file="ncaa_volleyball_ratings.csv",stringsAsFactors=FALSE)
team_ratings <- team_ratings[order(team_ratings$serve_rat+team_ratings$reciv_rat,decreasing=TRUE),]
teams <- team_ratings$team
nteams <- length(teams)

###Pts Won Pct
ptswon  <- function(serv,recv){
  n     <- 10000
  probs <- c(serv,recv)
  sims  <- runif(n,0,1)
  pts   <- numeric(n)
  ind   <- 1
  for(i in 1:n){
    pts[i] <- as.numeric(sims[i] < probs[ind])
    if(pts[i] == 1){ ind <- 1 } else { ind <- 2 }
  }
  return(mean(pts))
}

rnd_robin <- as.data.frame(t(combn(teams,2)))
names(rnd_robin) <- c("team1","team2")
rnd_robin$team1pts <- numeric(dim(rnd_robin)[1])
rnd_robin$team2pts <- numeric(dim(rnd_robin)[1])

system.time(
for(i in 1:dim(rnd_robin)[1]){
  team1idx <- which(teams == rnd_robin$team1[i])
  team2idx <- which(teams == rnd_robin$team2[i])
  
  team1serv <-     1/(1+exp(team_ratings$reciv_rat[team2idx] - team_ratings$serve_rat[team1idx]))
  team1recv <-  1- 1/(1+exp(team_ratings$reciv_rat[team1idx] - team_ratings$serve_rat[team2idx]))
  rnd_robin$team1pts[i] <- ptswon(team1serv,team1recv)
  rnd_robin$team2pts[i] <- 1-rnd_robin$team1pts[i]
}
)

rankings <- data.frame(teams,pts=numeric(nteams),stringsAsFactors=FALSE)
for(i in 1:nteams){
  rankings$pts[i] <- round((sum(rnd_robin$team1pts[rnd_robin$team1 == teams[i]]) + sum(rnd_robin$team2pts[rnd_robin$team2 == teams[i]]))/(nteams-1),3)
}

rankings <- rankings[order(rankings$pts,decreasing=TRUE),]
rankings$rank <- 1:nteams
rankings <- rankings[,c("rank","teams","pts")]
head(rankings,25)

bigten <- c("Rutgers","Maryland","Penn St.","Ohio St.","Michigan","Michigan St.","Indiana","Purdue",
            "Illinois","Northwestern","Wisconsin","Iowa","Minnesota","Nebraska")

subset(rankings,teams %in% bigten)

write.csv(rankings,file=sprintf("ncaa_vb_rankings_%s.csv",Sys.Date()),row.names=FALSE)
write.csv(rankings,file="ncaa_vb_rankings_current.csv",row.names=FALSE)