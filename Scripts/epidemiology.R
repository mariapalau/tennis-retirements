# EPIDEMIOLOGICAL STUDY

## Database
load("Data/atp_final.RData")
load("Data/wta_final.RData")

atp <- atp_final[c("tourney_category", "year", "surface", "best_of", "round_level", "score", "games", "retirement", "match_outcome", "mean_age", "dif_rank", "sex")]
wta <- wta_final[c("tourney_category", "year", "surface", "best_of", "round_level", "score", "games", "retirement", "match_outcome", "mean_age", "dif_rank", "sex")]

## Packages
library(epiR)
library(SmartEDA)
library(ggplot2)


## 1. Retirement incidence through the years
### 1.1. ATP
#### 1.1.1. Dataframe with number of retirements, number of games and incidence per year
(retYearATP <- as.data.frame(table(atp$retirement, atp$year)))
colnames(retYearATP)[1] <- 'retirement'
colnames(retYearATP)[2] <- 'year'
colnames(retYearATP)[3] <- 'N'
retYearATP <- subset(retYearATP, retirement == TRUE)
(totalgamesYear <- aggregate(games~year, data=atp, sum))
retYearATP <- cbind(retYearATP, totalgamesYear$games)
colnames(retYearATP)[4] <- 'games'
retYearATP$incidence <- retYearATP$N/retYearATP$games*1000

#### 1.1.2. Retirement Incidence Rate per 1000 games
ggplot(retYearATP, aes(x=as.Date(year, format="%Y"), y=incidence)) +
  geom_line(col = "#1874CD", lwd = 1) +
  labs(x="Year", y="Retirement Incidence Rate",
       title="ATP Incidence of Retirements by year") +
  geom_smooth(col="#838B8B", lwd=0.6, se=FALSE, method='loess', formula=y~x)


### 1.2. WTA
#### 1.2.1. Dataframe with number of retirements, number of games and incidence per year
(retYearWTA <- as.data.frame(table(wta$retirement, wta$year)))
colnames(retYearWTA)[1] <- 'retirement'
colnames(retYearWTA)[2] <- 'year'
colnames(retYearWTA)[3] <- 'N'
retYearWTA <- subset(retYearWTA, retirement == TRUE)
(totalgamesYear <- aggregate(games~year, data=wta, sum))
retYearWTA <- cbind(retYearWTA, totalgamesYear$games)
colnames(retYearWTA)[4] <- 'games'
retYearWTA$incidence <- retYearWTA$N/retYearWTA$games*1000

#### 1.2.2. Retirement Incidence Rate per 1000 games
ggplot(retYearWTA, aes(x=as.Date(year, format="%Y"), y=incidence)) +
  geom_line(col = "#1874CD", lwd = 1) +
  labs(x="Year", y="Retirement Incidence Rate",
       title="WTA Incidence of Retirements by year") +
  geom_smooth(col="#838B8B", lwd=0.6, se=FALSE, method='loess', formula=y~x)

### 1.3. Combining ATP and WTA Incidence plots
ggplot(retYearATP, aes(x=as.Date(year, format="%Y"), y=incidence)) +
  geom_line(col = "#1874CD", lwd = 1) +
  geom_smooth(col="#B0E2FF", lwd=0.6, se=FALSE, method='loess', formula=y~x) +
  geom_line(data=retYearWTA, col = "#CD5555", lwd = 1) +
  geom_smooth(data=retYearWTA, col="#FFB6C1", lwd=0.6, se=FALSE, method='loess', formula=y~x) +
  labs(x="Year", y="Retirement Incidence Rate",
       title="Incidence of Retirements by year")


## 2. Epidemiological measures
### 2.1. ATP
ExpCustomStat(atp, Cvar = "retirement")
sum(atp$games)
IR<-19314/12343510
confint <- qnorm(0.975)*sqrt(IR*(1-IR)/12343510)
IR*1000
(IR+confint)*1000
(IR-confint)*1000

#### 2.1.1. Tourney Category 
ExpCustomStat(atp, Cvar = c("tourney_category", "retirement"))
(retTourney <- table(atp$retirement, atp$tourney_category))
(totalgamesTourney <- aggregate(games~tourney_category, data=atp, sum))
#### Comparing ITF Men's World Tennis Tour vs. ATP Challenger Tour
ATP_ITF <- c(14737, 8922704, 4577, 3420806)
epi.2by2(dat = ITF_ATP, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

#### 2.1.2. Surface
ExpCustomStat(atp, Cvar = c("surface", "retirement"))
(retSurface <- table(atp$retirement, atp$surface))
(totalgamesSurface <- aggregate(games~surface, data=atp, sum))
#### Comparing Clay Surface vs. Grass Surface
G_Cl <- c(10176, 6367080, 121, 153697)
epi.2by2(dat = G_Cl, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")
#### Comparing Hard Surface vs. Grass Surface
G_Hd <- c(8300, 5211733, 121, 153697)
epi.2by2(dat = G_Hd, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")
#### Comparing Carpet Surface vs. Grass Surface
G_Cp <- c(717, 611000, 121, 153697)
epi.2by2(dat = G_Cp, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

#### 2.1.3. Round
ExpCustomStat(atp, Cvar = c("round_level", "retirement"))
(retRound <- table(atp$retirement, atp$round_level))
(totalgamesRound <- aggregate(games~round_level, data=atp, sum))
#### Comparing Preliminary Round vs. Final Round
F_Pr <- c(14600, 9316707, 4307, 2785171)
epi.2by2(dat = F_Pr, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")
#### Comparing Qualifying Round vs. Final Round
F_Qf <- c(407, 241632, 4307, 2785171)
epi.2by2(dat = F_Qf, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")


### 2.2. WTA
ExpCustomStat(wta, Cvar = "retirement")
sum(wta$games)
IR<-7306/5381938
confint <- qnorm(0.975)*sqrt(IR*(1-IR)/12343510)
IR*1000
(IR+confint)*1000
(IR-confint)*1000

#### 2.2.1. Tourney Category
ExpCustomStat(wta, Cvar = c("tourney_category", "retirement"))
(retTourney <- table(wta$retirement, wta$tourney_category))
(totalgamesTourney <- aggregate(games~tourney_category, data=wta, sum))
#### Comparing ITF Women's World Tennis Tour vs. WTA 125 Tournaments
WTA_ITF <- c(7291, 5371100, 15, 10838)
epi.2by2(dat = WTA_ITF, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

#### 2.2.2. Surface
ExpCustomStat(wta, Cvar = c("surface", "retirement"))
(retSurface <- table(wta$retirement, wta$surface))
(totalgamesSurface <- aggregate(games~surface, data=wta, sum))
#### Comparing Clay Surface vs. Grass Surface
G_Cl <- c(3509, 2574655, 124, 116669)
epi.2by2(dat = G_Cl, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")
#### Comparing Hard Surface vs. Grass Surface
G_Hd <- c(3380, 2435619, 124, 116669)
epi.2by2(dat = G_Hd, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")
#### Comparing Carpet Surface vs. Grass Surface
G_Cp <- c(293, 254995, 124, 116669)
epi.2by2(dat = G_Cp, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

#### 2.2.3. Round
ExpCustomStat(wta, Cvar = c("round_level", "retirement"))
(retRound <- table(wta$retirement, wta$round_level))
(totalgamesRound <- aggregate(games~round_level, data=wta, sum))
#### Comparing Preliminary Round vs. Final Round
F_Pr <- c(5488, 4120616, 1816, 1260368)
epi.2by2(dat = F_Pr, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")
#### Comparing Qualifying Round vs. Final Round
F_Qf <- c(2, 954, 1816, 1260368)
epi.2by2(dat = F_Qf, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")