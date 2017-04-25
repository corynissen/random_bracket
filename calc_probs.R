
library("stringr")

df <- read.csv("NCAA_mens_tournament_scores.csv", 
               stringsAsFactors=FALSE)

calc_prob_table <- function(df){
  # create string with seeds as "lowest seed:highest seed" eg: 3:14
  df$matchup <- paste0(apply(df[,c("team1_rank", "team2_rank")], 1, min), 
                       " : ",
                       apply(df[,c("team1_rank", "team2_rank")], 1, max))

  df$which_won <- ifelse(df$team1_score > df$team2_score, "team1", "team2")
  df$which_lost <- ifelse(df$team1_score > df$team2_score, "team2", "team1")
  df$won_rank <- ifelse(df$which_won=="team1", df$team1_rank, df$team2_rank)
  df$lost_rank <- ifelse(df$which_won=="team1", df$team2_rank, df$team1_rank)
  df$upset <- df$won_rank > df$lost_rank
  
  tmp <- by(df$upset, df$matchup, sum)
  matchup_tab <- data.frame(matchup = names(tmp),
                            upset_count = as.numeric(tmp),
                            stringsAsFactors=FALSE)
  tmp <- table(df$matchup)
  matchup_tab <- merge(matchup_tab,
                       data.frame(matchup=names(tmp),
                                  total=as.numeric(tmp),
                                  stringsAsFactors=FALSE), 
                       by="matchup")
  matchup_tab$percent_upset <- round(matchup_tab$upset_count / 
                                       matchup_tab$total, 3) * 100
  tmp <- str_split(matchup_tab$matchup, " : ")
  matchup_tab$first <- as.numeric(sapply(tmp, "[", 1))
  matchup_tab$second <- as.numeric(sapply(tmp, "[", 2))
  matchup_tab <- matchup_tab[order(matchup_tab$first, 
                                   matchup_tab$second), ]
  matchup_tab$first <- matchup_tab$second <- NULL
  return(matchup_tab)
}
  