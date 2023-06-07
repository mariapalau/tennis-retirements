# BIVARIABLE ANALYSIS

## Database
load("Data/atp_final.RData")
load("Data/wta_final.RData")

atp <- atp_final[c("tourney_category", "year", "best_of", "surface", "round_level", "score", "games", "retirement", "match_outcome", "mean_age", "dif_rank", "sex")]
wta <- wta_final[c("tourney_category", "year", "best_of", "surface", "round_level", "score", "games", "retirement", "match_outcome", "mean_age", "dif_rank", "sex")]

## Packages
library(compareGroups)
library(tidyverse)
library(ggdist)

## 1. Group comparison
### 1.1. ATP
compare_atp <- descrTable(retirement ~ . -year-score-games-match_outcome-sex-dif_rank, data = atp, byrow = TRUE, show.p.overall = FALSE, digits = 2, method = c(mean_age=NA), alpha=0.05)
compare_atp

### 1.2. WTA
compare_wta <- descrTable(retirement ~ . -year-score-games-match_outcome-sex-dif_rank, data = wta, byrow = TRUE, show.p.overall = FALSE, digits = 2, method = c(mean_age=NA), alpha=0.05)
compare_wta


## 2. Plots for the bivariable analysis
### 2.1. Tourney Category
ggplot(atp, aes(y=tourney_category)) +
  geom_bar(aes(fill = retirement), position = "fill") +
  scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
  labs(x="Proportion",y=NULL,title="ATP Tourney Category",fill="Retirement")
ggplot(wta, aes(y=tourney_category)) +
  geom_bar(aes(fill = retirement), position = "fill") +
  scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
  labs(x="Proportion",y=NULL,title="WTA Tourney Category",fill="Retirement")

### 2.2. Surface
ggplot(atp, aes(y=surface)) +
  geom_bar(aes(fill = retirement), position = "fill") +
  scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
  labs(x="Proportion",y=NULL,title="ATP Surface",fill="Retirement")
ggplot(wta, aes(y=surface)) +
  geom_bar(aes(fill = retirement), position = "fill") +
  scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
  labs(x="Proportion",y=NULL,title="WTA Surface",fill="Retirement")

### 2.3. Round
ggplot(atp, aes(y=round_level)) +
  geom_bar(aes(fill = retirement), position = "fill") +
  scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
  labs(x="Proportion",y=NULL,title="ATP Round",fill="Retirement")
ggplot(wta, aes(y=round_level)) +
  geom_bar(aes(fill = retirement), position = "fill") +
  scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
  labs(x="Proportion",y=NULL,title="WTA Round",fill="Retirement")

### 2.4. Mean of Ages
ggplot(atp, aes(x = factor(retirement), y = mean_age, fill = factor(retirement))) +
  stat_halfeye(adjust = 0.5, justification = -0.1, .width = 0, point_colour = NA, scale=0.8) +
  geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.8) +
  scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
  labs(title = "ATP Mean of Ages", x = "Retirement", y = "Age Mean", fill = "Retirement") +
  coord_flip()
ggplot(wta, aes(x = factor(retirement), y = mean_age, fill = factor(retirement))) +
  stat_halfeye(adjust = 0.5, justification = -0.1, .width = 0, point_colour = NA, scale=0.8) +
  geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.8) +
  scale_fill_manual(values=c("#B9D3EE", "#36648B")) +
  labs(title = "WTA Mean of Ages", x = "Retirement", y = "Age Mean", fill = "Retirement") +
  coord_flip()