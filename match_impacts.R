###Match Impacts
nmat <- dim(conf_schedule_sims)[1]
match_impacts <- conf_schedule_sims[,c("date","home","hometrk","away","awaytrk","team1winprob")]

homewinch=numeric(nmat);homelosch=numeric(nmat)
awaywinch=numeric(nmat);awaylosch=numeric(nmat)
homewint5=numeric(nmat);homelost5=numeric(nmat)
awaywint5=numeric(nmat);awaylost5=numeric(nmat)
homewint8=numeric(nmat);homelost8=numeric(nmat)
awaywint8=numeric(nmat);awaylost8=numeric(nmat)
names(match_impacts)[names(match_impacts) == "team1winprob"] <- "homewinprob"
match_impacts$homewinprob <- as.numeric(match_impacts$homewinprob)

###Calculate Impacts
for(i in 1:nmat){
  hidx  <- which(conf == match_impacts$home[i])
  aidx  <- which(conf == match_impacts$away[i])
  hwins <- conf_sim_results[i,] == 1
  awins <- conf_sim_results[i,] == 0
  
  ###Championship Race
  homewinch[i] <- round(sum(ranks[hidx,hwins] == 1)/sum(hwins),3)
  homelosch[i] <- round(sum(ranks[hidx,awins] == 1)/sum(awins),3)
  awaywinch[i] <- round(sum(ranks[aidx,awins] == 1)/sum(awins),3)
  awaylosch[i] <- round(sum(ranks[aidx,hwins] == 1)/sum(hwins),3)
  
  ###Top 5 Race
  homewint5[i] <- round(sum(ranks[hidx,hwins] <= 5)/sum(hwins),3)
  homelost5[i] <- round(sum(ranks[hidx,awins] <= 5)/sum(awins),3)
  awaywint5[i] <- round(sum(ranks[aidx,awins] <= 5)/sum(awins),3)
  awaylost5[i] <- round(sum(ranks[aidx,hwins] <= 5)/sum(hwins),3)
  
  ###Top 8 Race
  homewint8[i] <- round(sum(ranks[hidx,hwins] <= 8)/sum(hwins),3)
  homelost8[i] <- round(sum(ranks[hidx,awins] <= 8)/sum(awins),3)
  awaywint8[i] <- round(sum(ranks[aidx,awins] <= 8)/sum(awins),3)
  awaylost8[i] <- round(sum(ranks[aidx,hwins] <= 8)/sum(hwins),3)
  
}

match_impacts <- data.frame(match_impacts,
                            homewinch,homelosch,awaywinch,awaylosch,
                            homewint5,homelost5,awaywint5,awaylost5,
                            homewint8,homelost8,awaywint8,awaylost8,
                            stringsAsFactors=FALSE)