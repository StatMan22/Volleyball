sim_match <- function(team1serv,team2serv,iter){
  team1wins <- numeric(iter)
  sets      <- numeric(iter)
  for(i in 1:iter){
    sim  <- runif(500,0,1)
    pt    <- 1
    set   <- 1
    serve <- (i %% 2) + 1
    match_won <- FALSE
    team1sets <- 0
    team2sets <- 0
    ###Simulate Match
    while(match_won == FALSE){
      set_won <- FALSE
      team1pts <- 0
      team2pts <- 0
      while(set_won == FALSE){
        ###If Team 1 is Serving
        if(serve == 1){
          ###Team 1 Wins Point
          if(sim[pt] < team1serv){
            serve <- 1
            team1pts <- team1pts + 1
          ###Team 2 Wins Point
          } else {
            serve <- 2
            team2pts <- team2pts + 1
          }
        ###If Team 2 is Serving
        } else {
          ###Team 2 Wins Point
          if(sim[pt] < team2serv){
            serve <- 2
            team2pts <- team2pts + 1
          ###Team 1 Wins Point
          } else {
            serve <- 1
            team1pts <- team1pts + 1
          }
        }
        ###Check if Team 1 won set
        if((set <= 4 & (team1pts > team2pts + 1) & team1pts >= 25) | (set == 5 & (team1pts > team2pts + 1) & team1pts >= 15)){ 
          set_won <- TRUE
          team1sets <- team1sets + 1
        }
        ###Check if Team 2 won set
        if((set <= 4 & (team2pts > team1pts + 1) & team2pts >= 25) | (set == 5 & (team2pts > team1pts + 1) & team2pts >= 15)){ 
          set_won <- TRUE
          team2sets <- team2sets + 1
        }
        pt <- pt + 1
      }
      if(team1sets >= 3 | team2sets >= 3){ match_won <- TRUE}
      set <- set + 1
    }
    if(team1sets == 3){ team1wins[i] <- 1 }
    sets[i] <- set - 1
  }
  return(list(mean(team1wins),team1wins,table(team1wins,sets)))
}

#a <- Sys.time()
#sim_match(0.303,0.4,1000)
#b <- Sys.time()
#b-a


###0.30,0.40: 0.0838
###0.31,0.40: 0.1087
###0.32,0.40: 0.1404
###0.33,0.40: 0.1673

###0.300,0.40: 0.0838
###0.301,0.40: 0.0885
###0.302,0.40: 0.0896
###0.303,0.40: 0.0934

###Iter =   100:  0.0480
###Iter =  1000:  0.4518
###Iter = 10000:  4.2906

###0.150: 0.31754
###0.160: 0.32798
###0.170: 0.33514
###0.180: 0.35056
###0.190: 0.36518
###0.200: 0.37452