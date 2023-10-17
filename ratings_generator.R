###Read In Data
setwd("H:/Documents/R/Volleyball")
vball_pbp <- read.csv("full_ncaa_vball_pbp.csv",stringsAsFactors=FALSE)
vball_pbp <- subset(vball_pbp,curpt_serve != 0)
vball_pbp$neutral <- rep(0,dim(vball_pbp)[1])

###Do Math
teams    <- unique(c(vball_pbp$team1,vball_pbp$team2))
nteams   <- length(teams)
hindex   <- match(vball_pbp$team1,teams)
aindex   <- match(vball_pbp$team2,teams)
team1_pt <- vball_pbp$team1_point
serveind <- vball_pbp$curpt_serve
offdef   <- numeric(nteams*2 + 1)

team1ptprob <- 1/(1+exp(offdef[aindex + nteams*(2-serveind)] - offdef[hindex + nteams*(serveind-1)] - offdef[nteams*2+1]))
summary(team1ptprob)

SSE <- function(offdef,team1ptprob,team1_pts){
  team1ptprob <- 1/(1+exp(offdef[aindex + nteams*(2-serveind)] - offdef[hindex + nteams*(serveind-1)] - offdef[nteams*2+1]))
  
  ###Maximum Likelihood
  SSETot <- sum(-team1_pt*log(team1ptprob)-(1-team1_pt)*log(1-team1ptprob))
  ###Maximum Likelihood
  
  return(SSETot)
}

system.time(
  Results <- optim(offdef,SSE,team1_pt=team1_pt,team1ptprob=team1ptprob,method="BFGS",
                   control=list(maxit=10000,reltol=10e-10,trace=TRUE))
)

team_ratings <- data.frame(team=teams,
                           serve_rat=Results$par[1:nteams],
                           reciv_rat=Results$par[(nteams+1):(2*nteams)],
                           stringsAsFactors=FALSE)
write.csv(team_ratings,file="ncaa_volleyball_ratings.csv",row.names=FALSE)


###Append to df
offdef <- Results$par
team1ptprob <- 1/(1+exp(offdef[aindex + nteams*(2-serveind)] - offdef[hindex + nteams*(serveind-1)] - offdef[nteams*2+1]))
vball_pbp$team1ptprob <- team1ptprob


###Stuff
teamchk <- "Purdue"
team_stats_home <- as.data.frame(
  vball_pbp %>% group_by(team1,curpt_serve) %>% summarize(pt_win_pct=mean(team1_point),npoints = n())
)
team_stats_away <- as.data.frame(
  vball_pbp %>% group_by(team2,curpt_serve) %>% summarize(pt_win_pct=mean(team2_point),npoints = n())
)
subset(team_stats_home,team1 == teamchk)
subset(team_stats_away,team2 == teamchk)

