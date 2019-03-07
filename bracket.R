
brkt <- read.csv("2018_initial_bracket.csv", as.is=TRUE, stringsAsFactors=FALSE)
source("calc_probs.R")
score_data <- read.csv("NCAA_mens_tournament_scores.csv", stringsAsFactors=FALSE)
probs <- calc_prob_table(score_data)

brkt$teamname_orig <- brkt$teamname
brkt$teamname <- paste0("(", brkt$seed, ") ", brkt$teamname_orig)

# first round games...
brkt$rnd1games <- paste(brkt$quadrant, brkt$rnd1, sep="_")
brkt$rnd1winner <- rep(NA, nrow(brkt))
first_round_games <- unique(brkt$rnd1games)
for(game in first_round_games){
  seeds <- brkt$seed[brkt$rnd1games==game]
  top_seed <- min(seeds)
  bot_seed <- max(seeds)
  matchup <- paste0(top_seed, " : ", bot_seed)
  pct_upset <- probs$percent_upset[probs$matchup==matchup] / 100
  if(identical(pct_upset, numeric(0))) pct_upset <- .5
  upset <- rbinom(1, 1, pct_upset) == 1
  winner <- ifelse(upset, brkt$teamname[brkt$rnd1games==game & 
                                        brkt$seed==bot_seed],
                          brkt$teamname[brkt$rnd1games==game & 
                                        brkt$seed==top_seed])
  brkt$rnd1winner[brkt$rnd1games==game] <- winner
}

# second round games...
brkt$rnd2games <- paste(brkt$quadrant, brkt$rnd2, sep="_")
brkt$rnd2winner <- rep(NA, nrow(brkt))
second_round_games <- unique(brkt$rnd2games)
for(game in second_round_games){
  teams <- unique(brkt$rnd1winner[brkt$rnd2games==game])
  seeds <- brkt$seed[brkt$rnd2games==game &
                     brkt$teamname %in% teams]
  top_seed <- min(seeds)
  bot_seed <- max(seeds)
  matchup <- paste0(top_seed, " : ", bot_seed)
  pct_upset <- probs$percent_upset[probs$matchup==matchup] / 100
  if(identical(pct_upset, numeric(0))) pct_upset <- .5
  upset <- rbinom(1, 1, pct_upset) == 1
  winner <- ifelse(upset, brkt$teamname[brkt$rnd2games==game & 
                                          brkt$seed==bot_seed],
                   brkt$teamname[brkt$rnd2games==game & 
                                   brkt$seed==top_seed])
  brkt$rnd2winner[brkt$rnd2games==game] <- winner
}

# third round games...
brkt$rnd3games <- paste(brkt$quadrant, brkt$rnd3, sep="_")
brkt$rnd3winner <- rep(NA, nrow(brkt))
third_round_games <- unique(brkt$rnd3games)
for(game in third_round_games){
  teams <- unique(brkt$rnd2winner[brkt$rnd3games==game])
  seeds <- brkt$seed[brkt$rnd3games==game &
                       brkt$teamname %in% teams]
  top_seed <- min(seeds)
  bot_seed <- max(seeds)
  matchup <- paste0(top_seed, " : ", bot_seed)
  pct_upset <- probs$percent_upset[probs$matchup==matchup] / 100
  if(identical(pct_upset, numeric(0))) pct_upset <- .5
  upset <- rbinom(1, 1, pct_upset) == 1
  winner <- ifelse(upset, brkt$teamname[brkt$rnd3games==game & 
                                          brkt$seed==bot_seed],
                   brkt$teamname[brkt$rnd3games==game & 
                                   brkt$seed==top_seed])
  brkt$rnd3winner[brkt$rnd3games==game] <- winner
}

# fourth round games...
brkt$rnd4games <- paste(brkt$quadrant, "1", sep="_")
brkt$rnd4winner <- rep(NA, nrow(brkt))
fourth_round_games <- unique(brkt$rnd4games)
for(game in fourth_round_games){
  teams <- unique(brkt$rnd3winner[brkt$rnd4games==game])
  seeds <- brkt$seed[brkt$rnd4games==game &
                       brkt$teamname %in% teams]
  top_seed <- min(seeds)
  bot_seed <- max(seeds)
  matchup <- paste0(top_seed, " : ", bot_seed)
  pct_upset <- probs$percent_upset[probs$matchup==matchup] / 100
  if(identical(pct_upset, numeric(0))) pct_upset <- .5
  upset <- rbinom(1, 1, pct_upset) == 1
  winner <- ifelse(upset, brkt$teamname[brkt$rnd4games==game & 
                                          brkt$seed==bot_seed],
                   brkt$teamname[brkt$rnd4games==game & 
                                   brkt$seed==top_seed])
  brkt$rnd4winner[brkt$rnd4games==game] <- winner
}

# final four, W vs Y, X vs Z
# W vs Y
teams <- unique(brkt$rnd4winner[brkt$quadrant %in% c("W", "Y")])
seeds <- brkt$seed[brkt$teamname %in% teams]
if(length(unique(seeds)) > 1){
  top_seed <- min(seeds)
  bot_seed <- max(seeds)
  matchup <- paste0(top_seed, " : ", bot_seed)
  pct_upset <- probs$percent_upset[probs$matchup==matchup] / 100
  if(identical(pct_upset, numeric(0))) pct_upset <- .5
  upset <- rbinom(1, 1, pct_upset) == 1
  WY_winner <- ifelse(upset, teams[which(seeds==max(seeds))],
                      teams[which(seeds==min(seeds))])
}else{
  # same seed, just a coin flip now...
  WY_winner <- ifelse(rbinom(1,1,.5) > .5, teams[1], teams[2])
}
# X vs Z
teams <- unique(brkt$rnd4winner[brkt$quadrant %in% c("X", "Z")])
seeds <- brkt$seed[brkt$teamname %in% teams]
if(length(unique(seeds)) > 1){
  top_seed <- min(seeds)
  bot_seed <- max(seeds)
  matchup <- paste0(top_seed, " : ", bot_seed)
  pct_upset <- probs$percent_upset[probs$matchup==matchup] / 100
  if(identical(pct_upset, numeric(0))) pct_upset <- .5
  upset <- rbinom(1, 1, pct_upset) == 1
  XZ_winner <- ifelse(upset, teams[which(seeds==max(seeds))],
                      teams[which(seeds==min(seeds))])
}else{
  # same seed, just a coin flip now...
  XZ_winner <- ifelse(rbinom(1,1,.5) > .5, teams[1], teams[2])
}

# Championship WY_winner vs XZ_winner
teams <- c(WY_winner, XZ_winner)
seeds <- brkt$seed[brkt$teamname %in% teams]
if(length(unique(seeds)) > 1){
  top_seed <- min(seeds)
  bot_seed <- max(seeds)
  matchup <- paste0(top_seed, " : ", bot_seed)
  pct_upset <- probs$percent_upset[probs$matchup==matchup] / 100
  if(identical(pct_upset, numeric(0))) pct_upset <- .5
  upset <- rbinom(1, 1, pct_upset) == 1
  champ_winner <- ifelse(upset, teams[which(seeds==max(seeds))],
                         teams[which(seeds==min(seeds))])
}else{
  # same seed, just a coin flip now...
  champ_winner <- ifelse(rbinom(1,1,.5) > .5, teams[1], teams[2])
}




pdf("seedbracket.pdf",width=11,height=8.5)

x<-seq(0,220,(221/67))
y<-0:66

plot(x,y,type="l", col.axis="white", col.lab="white", bty="n", 
	axes=F, col="white")
segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2))) 
segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
segments(60,c(3,19,37,53),60,c(11,27,45,61))
segments(60,c(7,23,41,57),80,c(7,23,41,57))
segments(80,c(7,41),80,c(23,57))
segments(80,c(15,49),100,c(15,49))
segments(100,c(27,37),120,c(27,37))
segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2))) 
segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
segments(160,c(3,19,37,53),160,c(11,27,45,61))
segments(140,c(7,23,41,57),160,c(7,23,41,57))
segments(140,c(7,41),140,c(23,57))
segments(120,c(15,49),140,c(15,49))

text(9.8,64.5,brkt[1,"teamname"],cex=.4)
text(9.8,62.5,brkt[2,"teamname"],cex=.4)
text(9.8,60.5,brkt[3,"teamname"],cex=.4)
text(9.8,58.5,brkt[4,"teamname"],cex=.4)
text(9.8,56.5,brkt[5,"teamname"],cex=.4)
text(9.8,54.5,brkt[6,"teamname"],cex=.4)
text(9.8,52.5,brkt[7,"teamname"],cex=.4)
text(9.8,50.5,brkt[8,"teamname"],cex=.4)
text(9.8,48.5,brkt[9,"teamname"],cex=.4)
text(9.8,46.5,brkt[10,"teamname"],cex=.4)
text(9.8,44.5,brkt[11,"teamname"],cex=.4)
text(9.8,42.5,brkt[12,"teamname"],cex=.4)
text(9.8,40.5,brkt[13,"teamname"],cex=.4)
text(9.8,38.5,brkt[14,"teamname"],cex=.4)
text(9.8,36.5,brkt[15,"teamname"],cex=.4)
text(9.8,34.5,brkt[16,"teamname"],cex=.4)

text(29.8,63.5,brkt[1, "rnd1winner"],cex=.4)
text(29.8,59.5,brkt[3, "rnd1winner"],cex=.4)
text(29.8,55.5,brkt[5, "rnd1winner"],cex=.4)
text(29.8,51.5,brkt[7, "rnd1winner"],cex=.4)
text(29.8,47.5,brkt[9, "rnd1winner"],cex=.4)
text(29.8,43.5,brkt[11, "rnd1winner"],cex=.4)
text(29.8,39.5,brkt[13, "rnd1winner"],cex=.4)
text(29.8,35.5,brkt[15, "rnd1winner"],cex=.4)

text(49.8,61.5,brkt[1, "rnd2winner"],cex=.4)
text(49.8,53.5,brkt[5, "rnd2winner"],cex=.4)
text(49.8,45.5,brkt[9, "rnd2winner"],cex=.4)
text(49.8,37.5,brkt[13, "rnd2winner"],cex=.4)

text(69.8,57.5,brkt[1, "rnd3winner"],cex=.4)
text(69.8,41.5,brkt[9, "rnd3winner"],cex=.4)


text(9.8,30.5,brkt[33,"teamname"],cex=.4)
text(9.8,28.5,brkt[34,"teamname"],cex=.4)
text(9.8,26.5,brkt[35,"teamname"],cex=.4)
text(9.8,24.5,brkt[36,"teamname"],cex=.4)
text(9.8,22.5,brkt[37,"teamname"],cex=.4)
text(9.8,20.5,brkt[38,"teamname"],cex=.4)
text(9.8,18.5,brkt[39,"teamname"],cex=.4)
text(9.8,16.5,brkt[40,"teamname"],cex=.4)
text(9.8,14.5,brkt[41,"teamname"],cex=.4)
text(9.8,12.5,brkt[42,"teamname"],cex=.4)
text(9.8,10.5,brkt[43,"teamname"],cex=.4)
text(9.8,8.5,brkt[44,"teamname"],cex=.4)
text(9.8,6.5,brkt[45,"teamname"],cex=.4)
text(9.8,4.5,brkt[46,"teamname"],cex=.4)
text(9.8,2.5,brkt[47,"teamname"],cex=.4)
text(9.8,0.5,brkt[48,"teamname"],cex=.4)

text(29.8,29.5,brkt[33,"rnd1winner"],cex=.4)
text(29.8,25.5,brkt[35,"rnd1winner"],cex=.4)
text(29.8,21.5,brkt[37,"rnd1winner"],cex=.4)
text(29.8,17.5,brkt[39,"rnd1winner"],cex=.4)
text(29.8,13.5,brkt[41,"rnd1winner"],cex=.4)
text(29.8,9.5,brkt[43,"rnd1winner"],cex=.4)
text(29.8,5.5,brkt[45,"rnd1winner"],cex=.4)
text(29.8,1.5,brkt[47,"rnd1winner"],cex=.4)

text(49.8,27.5,brkt[33,"rnd2winner"],cex=.4)
text(49.8,19.5,brkt[37,"rnd2winner"],cex=.4)
text(49.8,11.5,brkt[41,"rnd2winner"],cex=.4)
text(49.8,3.5,brkt[45,"rnd2winner"],cex=.4)

text(69.8,23.5,brkt[33,"rnd3winner"],cex=.4)
text(69.8,7.5,brkt[41,"rnd3winner"],cex=.4)


text(209.8,64.5,brkt[17,"teamname"],cex=.4)
text(209.8,62.5,brkt[18,"teamname"],cex=.4)
text(209.8,60.5,brkt[19,"teamname"],cex=.4)
text(209.8,58.5,brkt[20,"teamname"],cex=.4)
text(209.8,56.5,brkt[21,"teamname"],cex=.4)
text(209.8,54.5,brkt[22,"teamname"],cex=.4)
text(209.8,52.5,brkt[23,"teamname"],cex=.4)
text(209.8,50.5,brkt[24,"teamname"],cex=.4)
text(209.8,48.5,brkt[25,"teamname"],cex=.4)
text(209.8,46.5,brkt[26,"teamname"],cex=.4)
text(209.8,44.5,brkt[27,"teamname"],cex=.4)
text(209.8,42.5,brkt[28,"teamname"],cex=.4)
text(209.8,40.5,brkt[29,"teamname"],cex=.4)
text(209.8,38.5,brkt[30,"teamname"],cex=.4)
text(209.8,36.5,brkt[31,"teamname"],cex=.4)
text(209.8,34.5,brkt[32,"teamname"],cex=.4)

text(189.8,63.5,brkt[17,"rnd1winner"],cex=.4)
text(189.8,59.5,brkt[19,"rnd1winner"],cex=.4)
text(189.8,55.5,brkt[21,"rnd1winner"],cex=.4)
text(189.8,51.5,brkt[23,"rnd1winner"],cex=.4)
text(189.8,47.5,brkt[25,"rnd1winner"],cex=.4)
text(189.8,43.5,brkt[27,"rnd1winner"],cex=.4)
text(189.8,39.5,brkt[29,"rnd1winner"],cex=.4)
text(189.8,35.5,brkt[31,"rnd1winner"],cex=.4)

text(169.8,61.5,brkt[17,"rnd2winner"],cex=.4)
text(169.8,53.5,brkt[21,"rnd2winner"],cex=.4)
text(169.8,45.5,brkt[25,"rnd2winner"],cex=.4)
text(169.8,37.5,brkt[29,"rnd2winner"],cex=.4)

text(149.8,57.5,brkt[17,"rnd3winner"],cex=.4)
text(149.8,41.5,brkt[25,"rnd3winner"],cex=.4)


text(209.8,30.5,brkt[49,"teamname"],cex=.4)
text(209.8,28.5,brkt[50,"teamname"],cex=.4)
text(209.8,26.5,brkt[51,"teamname"],cex=.4)
text(209.8,24.5,brkt[52,"teamname"],cex=.4)
text(209.8,22.5,brkt[53,"teamname"],cex=.4)
text(209.8,20.5,brkt[54,"teamname"],cex=.4)
text(209.8,18.5,brkt[55,"teamname"],cex=.4)
text(209.8,16.5,brkt[56,"teamname"],cex=.4)
text(209.8,14.5,brkt[57,"teamname"],cex=.4)
text(209.8,12.5,brkt[58,"teamname"],cex=.4)
text(209.8,10.5,brkt[59,"teamname"],cex=.4)
text(209.8,8.5,brkt[60,"teamname"],cex=.4)
text(209.8,6.5,brkt[61,"teamname"],cex=.4)
text(209.8,4.5,brkt[62,"teamname"],cex=.4)
text(209.8,2.5,brkt[63,"teamname"],cex=.4)
text(209.8,0.5,brkt[64,"teamname"],cex=.4)

text(189.8,29.5,brkt[49,"rnd1winner"],cex=.4)
text(189.8,25.5,brkt[51,"rnd1winner"],cex=.4)
text(189.8,21.5,brkt[53,"rnd1winner"],cex=.4)
text(189.8,17.5,brkt[55,"rnd1winner"],cex=.4)
text(189.8,13.5,brkt[57,"rnd1winner"],cex=.4)
text(189.8,9.5,brkt[59,"rnd1winner"],cex=.4)
text(189.8,5.5,brkt[61,"rnd1winner"],cex=.4)
text(189.8,1.5,brkt[63,"rnd1winner"],cex=.4)

text(169.8,27.5,brkt[49,"rnd2winner"],cex=.4)
text(169.8,19.5,brkt[53,"rnd2winner"],cex=.4)
text(169.8,11.5,brkt[57,"rnd2winner"],cex=.4)
text(169.8,3.5,brkt[61,"rnd2winner"],cex=.4)

text(149.8,23.5,brkt[49,"rnd3winner"],cex=.4)
text(149.8,7.5,brkt[57,"rnd3winner"],cex=.4)

text(89.8,49.5,brkt[1,"rnd4winner"],cex=.4)
text(129.8,49.5,brkt[17,"rnd4winner"],cex=.4)
text(89.8,15.5,brkt[33,"rnd4winner"],cex=.4)
text(129.8,15.5,brkt[49,"rnd4winner"],cex=.4)

text(109.8,37.5,WY_winner,cex=.4)
text(109.8,27.5,XZ_winner,cex=.4)

text(109.8,32.5,champ_winner,cex=2.5)

dev.off()

print(champ_winner)