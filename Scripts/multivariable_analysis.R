# MULTIVARIABLE ANALYSIS

## Database
load("Data/atp_final.RData")
load("Data/wta_final.RData")

atp <- atp_final[c("tourney_category", "year", "surface", "best_of", "round_level", "score", "games", "retirement", "match_outcome", "mean_age", "dif_rank", "sex")]
wta <- wta_final[c("tourney_category", "year", "surface", "best_of", "round_level", "score", "games", "retirement", "match_outcome", "mean_age", "dif_rank", "sex")]

## Packages
library(party)
library(DHARMa)
library(sjPlot)
library(dplyr)
library(MASS)


## 1. Gender interaction
atp$retirement_num <- as.numeric(as.logical(atp$retirement))
wta$retirement_num <- as.numeric(as.logical(wta$retirement))
tennis <- rbind(atp, wta)
### 1.1. Interaction Plot: Round
interaction.plot(x.factor = tennis$round_level, trace.factor = tennis$sex, response = tennis$retirement_num, xlab = "Round", ylab = "Retirements", col=c("#4F94CD", "#00CD66"), lty=2, lwd=2.5)
### 1.2. Interaction Plot: Surface
interaction.plot(x.factor = tennis$surface, trace.factor = tennis$sex, response = tennis$retirement_num, xlab = "Surface", ylab = "Retirements", col=c("#4F94CD", "#00CD66"), lty=2, lwd=2.5)


## 2. Preparing covariables and setting references
atp$tourney_category <- as.factor(atp$tourney_category)
atp$surface <- as.factor(atp$surface)
atp$round_level <- as.factor(atp$round_level)
atp$retirement <- as.factor(atp$retirement)
#### Reference level
atp$tourney_category <- relevel(atp$tourney_category, ref = "ATP Challenger Tour")
atp$surface <- relevel(atp$surface, ref = "Grass")
atp$round_level <- relevel(atp$round_level, ref = "Final Round")

wta$tourney_category <- as.factor(wta$tourney_category)
wta$surface <- as.factor(wta$surface)
wta$round_level <- as.factor(wta$round_level)
wta$retirement <- as.factor(wta$retirement)
#### Reference level
wta$tourney_category <- relevel(wta$tourney_category, ref = "WTA 125 Tournaments")
wta$surface <- relevel(wta$surface, ref = "Grass")
wta$round_level <- relevel(wta$round_level, ref = "Final Round")


## 3. Decision trees
atpct <- party::ctree(retirement ~ tourney_category + year + surface + round_level + mean_age, data = atp, control = party::ctree_control(maxdepth=4))
plot(atpct)
wtact <- party::ctree(retirement ~ tourney_category + year + surface + round_level + mean_age, data = wta, control = party::ctree_control(maxdepth=4))
plot(wtact)

## 4. Linearity
### 4.1. ATP
t_year <- table(atp$year,atp$retirement) 
log_odd_year <- log((t_year[,2]+.5) / (t_year[,1]+.5)) 
year <- as.numeric(rownames(t_year)) 
plot(year,log_odd_year,xlab="Years",ylab="logodd",las=1, main="ATP")
lines(lowess(log_odd_year~year), col=4, lwd=2)
### 4.2. WTA
t_year <- table(wta$year,wta$retirement) 
log_odd_year <- log((t_year[,2]+.5) / (t_year[,1]+.5)) 
year <- as.numeric(rownames(t_year)) 
plot(year,log_odd_year,xlab="Years",ylab="logodd",las=1, main="WTA")
lines(lowess(log_odd_year~year), col=4, lwd=2)

## 4. Poisson Regression
### 4.1. ATP
atp_sub <- atp[c(1,2,3,5,7,10,12,13)]
atp_group <- atp_sub %>% 
  group_by(tourney_category,year,surface,round_level) %>%
  summarise(mean_age = mean(mean_age, na.rm = TRUE),
            games = sum(games),
            retirement_num = sum(retirement_num)
  )
#### 4.1.1. Initial Model
atp_poisson0 <- glm(retirement_num ~ tourney_category + year + surface + round_level + mean_age + year:mean_age, offset = log(games), family = "poisson", data = atp_group)
summary(atp_poisson0)
#### 4.1.2. Stepwise (AIC)
atp_poisson <- step(atp_poisson0)
summary(atp_poisson)
#### 4.1.3. Testing for overdispersion
testDispersion(atp_poisson)
sum(residuals(atp_poisson,type ="pearson")^2)/atp_poisson$df.residual
#### 4.1.4. Negative Binomial Model
atp_negbin <- glm.nb(retirement_num ~ tourney_category + year + surface + mean_age + year:mean_age + offset(log(games)), data = atp_group)
summary(atp_negbin)
#### 4.1.5. Results
summary(atp_poisson)$coefficients
exp(cbind(IRR = coef(atp_poisson), confint(atp_poisson)))
plot_model(atp_poisson, title="Number of Retirements", vline.color = "#4D4D4D")

### 5.2. WTA
wta_sub <- wta[c(1,2,3,5,7,10,12,13)]
wta_group <- wta_sub %>% 
  group_by(tourney_category,year,surface,round_level) %>%
  summarise(mean_age = mean(mean_age, na.rm = TRUE),
            games = sum(games),
            retirement_num = sum(retirement_num)
  )
wta_group$year2 <- wta_group$year-2000 ##Transformation of the variable year
#### 5.2.1. Initial Model
wta_poisson0 <- glm(retirement_num ~ tourney_category + I(year2^2) + year2 + surface + round_level + mean_age + year2:mean_age, offset = log(games), family = "poisson", data = wta_group)
summary(wta_poisson0)
#### 5.2.2. Stepwise
wta_poisson <- step(wta_poisson0)
summary(wta_poisson)
#### 5.2.3. Testing for overdispersion
testDispersion(wta_poisson)
sum(residuals(wta_poisson,type ="pearson")^2)/wta_poisson$df.residual
#### 5.2.4. Results
summary(wta_poisson)$coefficients
exp(cbind(IRR = coef(wta_poisson), confint(wta_poisson)))
plot_model(wta_poisson, title="Number of Retirements", vline.color = "#4D4D4D")