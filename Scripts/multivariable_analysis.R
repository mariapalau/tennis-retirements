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


## 2. Preparing covariables
atp$tourney_category <- as.factor(atp$tourney_category)
atp$surface <- as.factor(atp$surface)
atp$round_level <- as.factor(atp$round_level)
atp$retirement <- as.factor(atp$retirement)

wta$tourney_category <- as.factor(wta$tourney_category)
wta$surface <- as.factor(wta$surface)
wta$round_level <- as.factor(wta$round_level)
wta$retirement <- as.factor(wta$retirement)


## 3. Decision trees
atpct <- party::ctree(retirement ~ tourney_category + year + surface + round_level + mean_age, data = atp, control = party::ctree_control(maxdepth=4))
plot(atpct)
wtact <- party::ctree(retirement ~ tourney_category + year + surface + round_level + mean_age, data = wta, control = party::ctree_control(maxdepth=4))
plot(wtact)


## 4. Logistic Regression
### 4.1. ATP
atp_data <- subset(atp, games != 0) #Delating observations where games=0
#### 4.1.1. Initial Model
atp_logistic0 <- glm(retirement ~ tourney_category + year + surface + round_level + mean_age, offset = log(games), family = "binomial", data = atp_data)
summary(atp_logistic0)
anova(atp_logistic0)
#### 4.1.2. Stepwise
atp_logistic <- step(atp_logistic0)
summary(atp_logistic)
anova(atp_logistic)
#### 4.1.3.Testing for overdispersion
testDispersion(atp_logistic)
sum(residuals(atp_logistic,type ="pearson")^2)/atp_logistic$df.residual
#### 4.1.4. Quasibinomial Model
atp_qbinom <- glm(retirement ~ tourney_category + year + surface + round_level + mean_age, offset = log(games), family="quasibinomial", data = atp_data)
summary(atp_qbinom)
anova(atp_qbinom)
#### 4.1.5. Results
summary(atp_qbinom)$coefficients
exp(cbind(IRR = coef(atp_qbinom), confint(atp_qbinom)))
plot_model(atp_qbinom, title="Retirements", vline.color = "#4D4D4D")

### 4.2. WTA
wta_data <- subset(wta, games != 0) #Delating observations where games=0
#### 4.2.1. Initial Model
wta_logistic0 <- glm(retirement ~ tourney_category + year + surface + round_level + mean_age, offset = log(games), family = "binomial", data = wta_data)
summary(wta_logistic0)
anova(wta_logistic0)
#### 4.2.2. Stepwise
wta_logistic <- step(wta_logistic0)
summary(wta_logistic)
anova(wta_logistic)
#### 4.2.3. Testing for overdispersion
testDispersion(wta_logistic)
sum(residuals(wta_logistic,type ="pearson")^2)/wta_logistic$df.residual
#### 4.2.4. Quasibinomial Model
wta_qbinom <- glm(retirement ~ year + surface + round_level + mean_age, offset = log(games), family="quasibinomial", data = wta_data)
summary(wta_qbinom)
anova(wta_qbinom)
#### 4.2.5. Results
summary(wta_qbinom)$coefficients
exp(cbind(IRR = coef(wta_qbinom), confint(wta_qbinom)))
plot_model(wta_qbinom, title="Retirements", vline.color = "#4D4D4D")


## 5. Poisson Regression
### 5.1. ATP
atp_sub <- atp[c(1,2,3,5,7,10,12,13)]
atp_group <- atp_sub %>% 
  group_by(tourney_category,year,surface,round_level) %>%
  summarise(mean_age = mean(mean_age, na.rm = TRUE),
            games = sum(games),
            retirement_num = sum(retirement_num)
  )
#### 5.1.1. Initial Model
atp_poisson0 <- glm(retirement_num ~ tourney_category + year + surface + round_level + mean_age, offset = log(games), family = "poisson", data = atp_group)
summary(atp_poisson0)
anova(atp_poisson0)
#### 5.1.2. Stepwise
atp_poisson <- step(atp_poisson0)
summary(atp_poisson)
anova(atp_poisson)
#### 5.1.3. Testing for overdispersion
testDispersion(atp_poisson)
sum(residuals(atp_poisson,type ="pearson")^2)/atp_poisson$df.residual
#### 5.1.4. Negative Binomial Model
atp_negbin <- glm.nb(retirement_num ~ tourney_category + year + surface + offset(log(games)), data = atp_group)
summary(atp_negbin)
anova(atp_negbin)
#### 5.1.5. Results
summary(atp_negbin)$coefficients
exp(cbind(IRR = coef(atp_negbin), confint(atp_negbin)))
plot_model(atp_negbin, title="Number of Retirements", vline.color = "#4D4D4D")

### 5.2. WTA
wta_sub <- wta[c(1,2,3,5,7,10,12,13)]
wta_group <- wta_sub %>% 
  group_by(tourney_category,year,surface,round_level) %>%
  summarise(mean_age = mean(mean_age, na.rm = TRUE),
            games = sum(games),
            retirement_num = sum(retirement_num)
  )
#### 5.2.1. Initial Model
wta_poisson0 <- glm(retirement_num ~ tourney_category + year + surface + round_level + mean_age, offset = log(games), family = "poisson", data = wta_group)
summary(wta_poisson0)
anova(wta_poisson0)
#### 5.2.2. Stepwise
wta_poisson <- step(wta_poisson0)
summary(wta_poisson)
anova(wta_poisson)
#### 5.2.3. Testing for overdispersion
testDispersion(wta_poisson)
sum(residuals(wta_poisson,type ="pearson")^2)/wta_poisson$df.residual
#### 5.2.4. Results
summary(wta_poisson)$coefficients
exp(cbind(Incidence_Rate_Ratio = coef(wta_poisson), confint(wta_poisson)))
plot_model(wta_poisson, title="Number of Retirements", vline.color = "#4D4D4D")