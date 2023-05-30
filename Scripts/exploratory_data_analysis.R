# EXPLORATORY DATA ANALYSIS

## Database
load("Data/atp_final.RData")
load("Data/wta_final.RData")

atp <- atp_final[c("tourney_category", "year", "surface", "round_level", "score", "games", "retirement", "match_outcome", "mean_age", "dif_rank", "sex")]
wta <- wta_final[c("tourney_category", "year", "surface", "round_level", "score", "games", "retirement", "match_outcome", "mean_age", "dif_rank", "sex")]

## Packages
library(DataExplorer)
library(SmartEDA)
library(tidyverse)
library(ggpubr)

## 1. Exploratory data analysis report
create_report(atp, y="retirement", output_file="atp_eda_report.html")
create_report(wta, y="retirement", output_file="wta_eda_report.html")

## 2. Variables distribution
### 2.1. Match Outcome
(atp_mo <- ExpCustomStat(atp, Cvar = "match_outcome") %>%
    arrange(desc(Count)))
(wta_mo <- ExpCustomStat(wta, Cvar = "match_outcome")%>%
    arrange(desc(Count)))

ggplot(atp_mo, aes(Count, match_outcome)) + 
  geom_col(fill="#87CEFA") +
  aes(y = fct_reorder(match_outcome, Count)) +
  labs(x="Count", y=NULL, title="ATP Match Outcome") +
  scale_x_continuous(labels = scales::comma)
ggplot(wta_mo, aes(Count, match_outcome)) + 
  geom_col(fill="#87CEFA") +
  aes(y = fct_reorder(match_outcome, Count)) +
  labs(x="Count", y=NULL, title="WTA Match Outcome") +
  scale_x_continuous(labels = scales::comma)

### 2.2. Retirements
(atp_ret <- ExpCustomStat(atp, Cvar = "retirement") %>%
    arrange(desc(Count)))
(wta_ret <- ExpCustomStat(wta, Cvar = "retirement")%>%
    arrange(desc(Count)))

ggplot(atp_ret, aes(Prop, retirement)) + 
  geom_col(fill="#66CDAA") +
  aes(y = fct_reorder(retirement, Prop)) +
  labs(x="Percentage", y=NULL, title="ATP Retirements")
ggplot(wta_ret, aes(Prop, retirement)) + 
  geom_col(fill="#66CDAA") +
  aes(y = fct_reorder(retirement, Prop)) +
  labs(x="Percentage", y=NULL, title="WTA Retirements")

### 2.3. Tourney Category
(atp_cat <- ExpCustomStat(atp, Cvar = "tourney_category") %>%
    arrange(desc(Count)))
(wta_cat <- ExpCustomStat(wta, Cvar = "tourney_category")%>%
    arrange(desc(Count)))

ggplot(atp_cat, aes(Count, tourney_category)) + 
  geom_col(fill="#7AC5CD") +
  aes(y = fct_reorder(tourney_category, Count)) +
  labs(x="Count", y=NULL, title="ATP Tourney Category") +
  scale_x_continuous(labels = scales::comma)
ggplot(wta_cat, aes(Count, tourney_category)) + 
  geom_col(fill="#7AC5CD") +
  aes(y = fct_reorder(tourney_category, Count)) +
  labs(x="Count", y=NULL, title="WTA Tourney Category") +
  scale_x_continuous(labels = scales::comma)

### 2.4. Surface
(atp_surf <- ExpCustomStat(atp, Cvar = "surface") %>%
    arrange(desc(Count)))
(wta_surf <- ExpCustomStat(wta, Cvar = "surface")%>%
    arrange(desc(Count)))

ggplot(atp_surf, aes(Count, surface)) + 
  geom_col(fill="#7AC5CD") +
  aes(y = fct_reorder(surface, Count)) +
  labs(x="Count", y=NULL, title="ATP Surface") +
  scale_x_continuous(labels = scales::comma)
ggplot(wta_surf, aes(Count, surface)) + 
  geom_col(fill="#7AC5CD") +
  aes(y = fct_reorder(surface, Count)) +
  labs(x="Count", y=NULL, title="WTA Surface") +
  scale_x_continuous(labels = scales::comma)

### 2.5. Round
(atp_rou <- ExpCustomStat(atp, Cvar = "round_level") %>%
    arrange(desc(Count)))
(wta_rou <- ExpCustomStat(wta, Cvar = "round_level")%>%
    arrange(desc(Count)))

ggplot(atp_rou, aes(Count, round_level)) + 
  geom_col(fill="#7AC5CD") +
  aes(y = fct_reorder(round_level, Count)) +
  labs(x=NULL, y=NULL, title="ATP Round") +
  scale_x_continuous(labels = scales::comma)
ggplot(wta_rou, aes(Count, round_level)) + 
  geom_col(fill="#7AC5CD") +
  aes(y = fct_reorder(round_level, Count)) +
  labs(x=NULL, y=NULL, title="WTA Round") +
  scale_x_continuous(labels = scales::comma)

### 2.6. Mean of Ages
ggqqplot(atp$mean_age) +
  labs(title='QQ Plot', subtitle='ATP Mean of Ages')
ggqqplot(wta$mean_age) +
  labs(title='QQ Plot', subtitle='WTA Mean of Ages')

median(atp$mean_age, na.rm=TRUE)
range(atp$mean_age, na.rm=TRUE)
ggplot(atp, aes(x=mean_age)) + 
  geom_histogram(bins=50, fill="#7AC5CD", col="#53868B") +
  labs(x="Age Mean", y="Count", title="ATP Mean of Ages") +
  geom_vline(xintercept = median(atp$mean_age, na.rm = TRUE), col = "#EEB422", lwd = 1)
median(wta$mean_age, na.rm=TRUE)
range(wta$mean_age, na.rm=TRUE)
ggplot(wta, aes(x=mean_age)) +
  geom_histogram(bins=50, fill="#7AC5CD", col="#53868B") +
  labs(x="Age Mean", y="Count", title="WTA Mean of Ages") +
  geom_vline(xintercept = median(wta$mean_age, na.rm = TRUE), col = "#EEB422", lwd = 1)