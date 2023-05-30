# DATA WRANGLING

## Database
load("Data/atp_matches1.RData")
load("Data/atp_matches2.RData")
load("Data/wta_matches.RData")
atp_matches <- rbind(atp_matches1, atp_matches2)

## Packages
library(sjmisc)

## 1. Filtering by tournaments
atp_data <- subset(atp_matches, tourney_level == "Challenger" | 
                     tourney_level == "Futures")
wta_data <- subset(wta_matches, tourney_level == "Challenger" | 
                     tourney_level == "C125" | tourney_level == "Satellite" |
                     tourney_level == "C10" | tourney_level == "C20" | 
                     tourney_level == "C25" | tourney_level == "C50" | 
                     tourney_level == "C75" | tourney_level == "C100")


## 2. Creation of new variables
### 2.1. Tourney Category
for(i in 1:length(atp_data$tourney_level)) {
  if(atp_data$tourney_level[i] == "Challenger")
    atp_data$tourney_category[i] <- "ATP Challenger Tour"
  if(atp_data$tourney_level[i] == "Futures")
    atp_data$tourney_category[i] <- "ITF Men's World Tennis Tour"
}
for(i in 1:length(wta_data$tourney_level)) {
  if(wta_data$tourney_level[i] == "Challenger" | 
     wta_data$tourney_level[i] == "C125")
    wta_data$tourney_category[i] <- "WTA 125 Tournaments"
  if(wta_data$tourney_level[i] == "Satellite" | 
     wta_data$tourney_level[i] == "C10" |
     wta_data$tourney_level[i] == "C20" | 
     wta_data$tourney_level[i] == "C25" | 
     wta_data$tourney_level[i] == "C50" | 
     wta_data$tourney_level[i] == "C75" | 
     wta_data$tourney_level[i] == "C100")
    wta_data$tourney_category[i] <- "ITF Women's World Tennis Tour"
}

### 2.2. Number of games
atp_data$games <- rowSums(atp_data[ , c(49:58)], na.rm=TRUE)
wta_data$games <- rowSums(wta_data[ , c(49:58)], na.rm=TRUE)

### 2.3. Retirement
for(i in 1:length(atp_data$score)) {
  if(str_contains(atp_data$score[i], "RET"))
    atp_data$retirement[i] <- "YES"
  if(!str_contains(atp_data$score[i], "RET"))
    atp_data$retirement[i] <- "NO"
}
for(i in 1:length(wta_data$score)) {
  if(str_contains(wta_data$score[i], "RET"))
    wta_data$retirement[i] <- TRUE
  if(!str_contains(wta_data$score[i], "RET"))
    wta_data$retirement[i] <- FALSE
}

### 2.4. Age difference
atp_data$dif_age <- (atp_data$winner_age - atp_data$loser_age)
wta_data$dif_age <- (wta_data$winner_age - wta_data$loser_age)

### 2.5. Rank difference
atp_data$dif_rank <- (atp_data$winner_rank - atp_data$loser_rank)
wta_data$dif_rank <- (wta_data$winner_rank - wta_data$loser_rank)

### 2.6. Sex
atp_data$sex <- "Male"
wta_data$sex <- "Female"


## 3. Selection of variables
atp_v1 <- atp_data[c("tourney_category", "tourney_level", "tourney_id", "tourney_name", "year", "surface", "best_of", "round", "score", "games", "retirement", "winner_name", "loser_name", "winner_hand", "loser_hand", "winner_age", "loser_age", "dif_age", "winner_rank", "loser_rank", "dif_rank", "sex")]
wta_v1 <- wta_data[c("tourney_category", "tourney_level", "tourney_id", "tourney_name", "year", "surface", "best_of", "round", "score", "games", "retirement", "winner_name", "loser_name", "winner_hand", "loser_hand", "winner_age", "loser_age", "dif_age", "winner_rank", "loser_rank", "dif_rank", "sex")]
save(atp_v1, file = "Data/atp_v1.RData")
save(wta_v1, file = "Data/wta_v1.RData")

## 4. Updating database
load("Data/atp_v2.RData")
load("Data/wta_v2.RData")

### 4.1. Updating variable: number of games
atp_v2$games <- gsub("\\(\\d+\\)", "", atp_v2$score)
atp_v2$games <- regmatches(atp_v2$games, gregexpr("[[:digit:]]+", atp_v2$games))
atp_v2$games[atp_v2$games=="character(0)"] <- 0
for (i in 1:length(atp_v2$games)){ 
  x <- as.data.frame(matrix(unlist(atp_v2$games[i]), nrow=1))
  atp_v2$games[i] <- sum(as.numeric(x))
}
atp_v2$games <- as.numeric(atp_v2$games)

wta_v2$games <- gsub("\\(\\d+\\)", "", wta_v2$score)
wta_v2$games <- regmatches(wta_v2$games, gregexpr("[[:digit:]]+", wta_v2$games))
wta_v2$games[wta_v2$games=="character(0)"] <- 0
for (i in 1:length(wta_v2$games)){ 
  x <- as.data.frame(matrix(unlist(wta_v2$games[i]), nrow=1))
  wta_v2$games[i] <- sum(as.numeric(x))
}
wta_v2$games <- as.numeric(wta_v2$games)

### 4.2. Updating variable: retirement
for(i in 1:length(atp_v2$score)) {
  if(str_contains(atp_v2$score[i], "RET"))
    atp_v2$retirement[i] <- TRUE
  if(!str_contains(atp_v2$score[i], "RET"))
    atp_v2$retirement[i] <- FALSE
}
for(i in 1:length(wta_v2$score)) {
  if(str_contains(wta_v2$score[i], "RET"))
    wta_v2$retirement[i] <- TRUE
  if(!str_contains(wta_v2$score[i], "RET"))
    wta_v2$retirement[i] <- FALSE
}

### 4.3. New variable: sum of ages
atp_v2$sum_age <- (atp_v2$winner_age + atp_v2$loser_age)
wta_v2$sum_age <- (wta_v2$winner_age + wta_v2$loser_age)

### 4.4. New variable: mean of ages
atp_v2$mean_age <- rowMeans(atp_v2[,16:17], na.rm = TRUE)
wta_v2$mean_age <- rowMeans(wta_v2[,16:17], na.rm = TRUE)

### 4.5. New variable: round
for(i in 1:length(atp_v2$round)) {
  if(atp_v2$round[i] == "Q1" | atp_v2$round[i] == "Q2" | 
     atp_v2$round[i] == "Q3" | atp_v2$round[i] == "Q4")
    atp_v2$round_level[i] <- "Qualifying Round"
  else
    if(atp_v2$round[i] == "R64" | atp_v2$round[i] == "R32" | 
       atp_v2$round[i] == "R16" | atp_v2$round[i] == "RR")
      atp_v2$round_level[i] <- "Preliminary Round"
    else
      if(atp_v2$round[i] == "QF" | atp_v2$round[i] == "SF" | 
         atp_v2$round[i] == "F" | atp_v2$round[i] == "BR")
        atp_v2$round_level[i] <- "Final Round"
}
for(i in 1:length(wta_v2$round)) {
  if(wta_v2$round[i] == "Q1" | wta_v2$round[i] == "Q2")
    wta_v2$round_level[i] <- "Qualifying Round"
  else
    if(wta_v2$round[i] == "R32" | wta_v2$round[i] == "R16" | 
       wta_v2$round[i] == "R29" | wta_v2$round[i] == "R31" |
       wta_v2$round[i] == "R15")
      wta_v2$round_level[i] <- "Preliminary Round"
    else
      if(wta_v2$round[i] == "QF" | wta_v2$round[i] == "SF" | 
         wta_v2$round[i] == "F")
        wta_v2$round_level[i] <- "Final Round"
}

### 4.6. New variable: match outcome
library(sjmisc)
for(i in 1:length(atp_v2$score)) {
  if(str_contains(atp_v2$score[i], "RET"))
    atp_v2$match_outcome[i] <- "Incomplete: RET"
  else
    if(str_contains(atp_v2$score[i], "W/O"))
      atp_v2$match_outcome[i] <- "Incomplete: W/O"
    else
      if(str_contains(atp_v2$score[i], "DEF"))
        atp_v2$match_outcome[i] <- "Incomplete: DEF"
      else
        if(is.na(atp_v2$score[i]))
          atp_v2$match_outcome[i] <- "Unknown"
        else
          atp_v2$match_outcome[i] <- "Complete"
}
for(i in 1:length(wta_v2$score)) {
  if(str_contains(wta_v2$score[i], "RET"))
    wta_v2$match_outcome[i] <- "Incomplete: RET"
  else
    if(str_contains(wta_v2$score[i], "W/O"))
      wta_v2$match_outcome[i] <- "Incomplete: W/O"
    else
      if(str_contains(wta_v2$score[i], "DEF"))
        wta_v2$match_outcome[i] <- "Incomplete: DEF"
      else
        wta_v2$match_outcome[i] <- "Complete"
}

## 5. Final database
atp_final <- atp_v2[c("tourney_category", "tourney_level", "tourney_id", "tourney_name", "year", "surface", "best_of", "round_level", "round", "score", "games", "retirement", "match_outcome", "winner_name", "loser_name", "winner_hand", "loser_hand", "winner_age", "loser_age", "dif_age", "sum_age", "mean_age", "winner_rank", "loser_rank", "dif_rank", "sex")]
wta_final <- wta_v2[c("tourney_category", "tourney_level", "tourney_id", "tourney_name", "year", "surface", "best_of", "round_level", "round", "score", "games", "retirement", "match_outcome", "winner_name", "loser_name", "winner_hand", "loser_hand", "winner_age", "loser_age", "dif_age", "sum_age", "mean_age", "winner_rank", "loser_rank", "dif_rank", "sex")]
save(atp_final, file = "Data/atp_final.RData")
save(wta_final, file = "Data/wta_final.RData")