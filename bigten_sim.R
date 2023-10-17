a <- proc.time()
###Read in Files
library(jsonlite)
library(xml2)
library(dplyr)
library(rvest)
library(httr)
library(stringr)
schedule <- read.csv("ncaa_vb_schedule.csv",stringsAsFactors=FALSE)
results  <- read.csv("ncaa_vb_results.csv",stringsAsFactors=FALSE)
setwd("H:/Documents/R/Volleyball")
source("match_sim.R")
iter <- 10000

###Simulate
conf <- c("Rutgers","Maryland","Penn St.","Ohio St.","Michigan","Michigan St.","Indiana","Purdue",
            "Illinois","Northwestern","Wisconsin","Iowa","Minnesota","Nebraska")
nteams <- length(conf)
conf_schedule <- subset(schedule,home %in% conf & away %in% conf)
nmat   <- dim(conf_schedule)[1]
conf_sims <- mapply(sim_match,conf_schedule$homeservprob,conf_schedule$awayservprob,iter)
conf_schedule$team1winprob <- conf_sims[1,]
conf_schedule_sims <- conf_schedule[,c("date","home","hometrk","away","awaytrk","team1winprob")]
conf_weekend_preview <- subset(conf_schedule_sims,date <= as.Date("2023-10-01") & home %in% conf & away %in% conf)
conf_schedule_sims$sim_results <- conf_sims[2,]
conf_sim_results <- t(matrix(unlist(conf_sims[2,]),ncol=nmat))
conf_schedule_results <- subset(results,team1 %in% conf & team2 %in% conf)
nres <- dim(conf_schedule_results)[1]
conf_act_results <- t(matrix(rep(conf_schedule_results$team1win,each=iter),ncol=nres))
conf_tot_results <- rbind(conf_sim_results,conf_act_results)

hindex <- match(c(conf_schedule_sims$home,conf_schedule_results$team1),conf)
aindex <- match(c(conf_schedule_sims$away,conf_schedule_results$team2),conf)
wins   <- matrix(0,nteams,iter)
for(i in 1:nteams){
  hwins <- apply(conf_tot_results[hindex == i,],2,sum)
  awins <- apply(conf_tot_results[aindex == i,],2,function(x) sum(x==0))
  wins[i,] <- hwins+awins
}
standings_rank <- function(x){ return(nteams + 1 - rank(x,ties.method="random")) }
ranks <- apply(wins,2,standings_rank)
pos_matrix <- matrix(0,nteams,nteams)
row.names(pos_matrix) <- conf
for(i in 1:nteams){ pos_matrix[i,] <- colSums(sapply(1:nteams, `==`,ranks[i,])) }
pts_sort <- apply(pos_matrix,1,function(x) sum(x*14:1))
pos_matrix <- pos_matrix[order(pts_sort,decreasing=TRUE),]
pos_matrix

source("match_impacts.R")
write.csv(match_impacts,file="match_impacts_bigten.csv",row.names=FALSE)
write.csv(pos_matrix,file="simulations_bigten.csv")
b <- proc.time()
b-a