
library("rvest")
library("stringr")
library("xml2")

parse_score <- function(node_set){
  if(length(node_set)==0) return(NULL)
  tmp <- node_set %>% 
    html_nodes("dt") %>%  
    xml_contents
  the_break <- which(html_name(tmp) == "br")
  team1_tmp <- tmp[1:(the_break - 1)]
  team2_tmp <- tmp[(the_break + 1):length(tmp)]
  if(length(team1_tmp) == 1){
    tmp <- str_split(html_text(team1_tmp), " ", 2)[[1]]
  }else{
    tmp <- html_text(team1_tmp)
  }
  team1_name <- tmp[2]
  team1_rank <- tmp[1]
  
  if(length(team2_tmp) == 1){
    tmp <- str_split(html_text(team2_tmp), " ", 2)[[1]]
  }else{
    tmp <- html_text(team2_tmp)
  }
  team2_name <- tmp[2]
  team2_rank <- tmp[1]
  
  tmp <- node_set %>% 
    html_nodes("dd") %>%  
    xml_contents %>% 
    html_text
  tmp <- tmp[!tmp == ""]
  team1_score <- tmp[1]
  team2_score <- tmp[2]
  
  return(c(team1_name=team1_name, team1_rank=team1_rank, 
           team1_score=team1_score, team2_name=team2_name, team2_rank=team2_rank, 
           team2_score=team2_score))
}

get_bracket <- function(year){
  url <- paste0("http://www.espn.com/mens-college-basketball/tournament/bracket/_/id/",
                year, "22")
  html <- read_html(url)
  tables <- html %>% html_nodes(".region")

  df <- rbind(c(parse_score(html %>% html_node("#match61")), round = "4"),
              c(parse_score(html %>% html_node("#match62")), round = "4"),
              c(parse_score(html %>% html_node("#match63")), round = "2"))
  for(i in 1:4){
    for(j in 1:30){
      if(j %in% c(1:8, 16:23)) round <- "64"
      if(j %in% c(9:12, 24:27)) round <- "32"
      if(j %in% c(13, 14, 28, 29)) round <- "16"
      if(j %in% c(15, 30)) round <- "8"
      
      df <- rbind(df, c(parse_score(tables[i] %>% 
                                      html_nodes(paste0("#match", j))),
                        round =round))
    }
  }
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  remove <- !is.na(suppressWarnings(as.numeric(df$team1_name)))
  df <- df[!remove, ]
  df$year <- year
  return(df)
}

# espn goes back to 2002...
years <- 2002:2017

tmp <- lapply(years, get_bracket)
df <- do.call("rbind", tmp)

write.csv(df, "NCAA_mens_tournament_scores.csv", row.names=FALSE)

  
  
 