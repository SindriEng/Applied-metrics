---
title: "Assignment 3 - Weighting and Matching"
author: "S. Engilbertsson and I.v.d. Voort"
date: "19/9/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Code to load dataset and compute regressions
Commenting style follows http://adv-r.had.co.nz/Style.html`

```{r, message=FALSE, warning=FALSE}
rm(list = ls())

library(foreign)
library(cobalt)
library(xtable)
library(stargazer)
library(plm)
library(dplyr)
library(MatchIt)
library(arm)
library(ggplot2)
```

### List of working directories
Default working directory
```{r}
cd <- 'C:/Users/sindr/Desktop/Tinbergen/2nd-year/Block 1/Applied-Microeconometrics/Assignments/code'
```

Folder containing data and folder for outputs
```{r}
dt <- 'C:/Users/sindr/Desktop/Tinbergen/2nd-year/Block 1/Applied-Microeconometrics/Assignments/data'
tb <- 'C:/Users/sindr/Desktop/Tinbergen/2nd-year/Block 1/Applied-Microeconometrics/Assignments/Final'
```

### Reading in data
```{r}
setwd(dt)
df <- read_dta("C:/Users/sindr/Desktop/Tinbergen/2nd-year/Block 1/Applied-Microeconometrics/Assignments/data/cardkrueger.dta")
setwd(tb)
```

### Reading in data
```{r}
setwd(dt)
df <- as.data.frame(read.dta('CardKrueger2.dta'))
setwd(tb)
```

# Assignment
## **Naive estimation**
### *(i)* Compute separately for New Jersey and Pennsylvania the average number of employees in both waves, and compute the difference-in-difference estimate. Next repeat this, but only considering the restaurants that responded in both waves of the survey.
# Problem: Minimum wages in the US (Card & Krueger)
In their famous study Card and Krueger (AER, 1994) investigated the effects of increasing the minimum wage on employment. Therefore, they used data from fast-food restaurants in New Jersey and Pennsylvania. The data consist of two waves of a survey, of which one was conducted prior to a minimum wage increase in New Jersey and the other afterwards. This paper has been one of the first in which a difference-in-difference model has been used to estimate a policy parameters (the effect of a minimum wage increase).

The first columns of the data describe the variables collected in the first wave of the survey, while the later columns describe the second survey (see also the file containing the description of the data). Each survey containing the number of full-time employees and the number of part-time employees. Construct a variable full-time equivalent for both waves, which is the number of full-time employees plus the number of part-time employees divided by two and also add the number of managers. I will simply refer to employees for this outcome variable. In this exercise we investigate the robustness of the results reported by Card and Krueger (1994).
```{r}
# Construct the employee variable
df$emp_before <- df$EMPFT+(df$EMPPT)/2+df$NMGRS
df$emp_after <- df$EMPFT2+(df$EMPPT2)/2+df$NMGRS2
df$emp_diff <- df$emp_after-df$emp_before
```
### *(i)* Compute separately for New Jersey and Pennsylvania the average number of employees in both waves, and compute the difference-in-difference estimate.

### *Solution*
We start by calculating the averages and setting up a table:
```{r, message=FALSE, warning=FALSE}
Mean_emp_before <- mean(df[["emp_before"]], na.rm =T)
Mean_emp_after <- mean(df[["emp_after"]], na.rm =T)
Mean_emp_before_NJ <- mean(df[["emp_before"]][df[["STATE"]]==1], na.rm =T)
Mean_emp_after_NJ <- mean(df[["emp_after"]][df[["STATE"]]==1], na.rm =T)
Mean_emp_before_P <- mean(df[["emp_before"]][df[["STATE"]]==0], na.rm =T)
Mean_emp_after_P <- mean(df[["emp_after"]][df[["STATE"]]==0], na.rm =T)

print(Mean_emp_before)
print(Mean_emp_after)
print(Mean_emp_before_NJ)
print(Mean_emp_after_NJ)
print(Mean_emp_before_P)
print(Mean_emp_after_P)
```

Now we calculate the diff-in-diff estimator:
```{r}
did <- Mean_emp_after_NJ - Mean_emp_after_P - (Mean_emp_before_NJ - Mean_emp_before_P)
print(did)
```

### Next repeat this, but only considering the restaurants that responded in both waves of the survey
We start by calculating the averages and setting up a table:
```{r}
df_state2 <- filter(df, STATUS2==1)
Mean_emp_before_2 <- mean(df_state2[["emp_before"]], na.rm =T)
Mean_emp_after_2 <- mean(df_state2[["emp_after"]], na.rm =T)
Mean_emp_before_NJ_2 <- mean(df_state2[["emp_before"]][df[["STATE"]]==1], na.rm =T)
Mean_emp_after_NJ_2 <- mean(df_state2[["emp_after"]][df[["STATE"]]==1], na.rm =T)
Mean_emp_before_P_2 <- mean(df_state2[["emp_before"]][df[["STATE"]]==0], na.rm =T)
Mean_emp_after_P_2 <- mean(df_state2[["emp_after"]][df[["STATE"]]==0], na.rm =T)

print(Mean_emp_before_2)
print(Mean_emp_after_2)
print(Mean_emp_before_NJ_2)
print(Mean_emp_after_NJ_2)
print(Mean_emp_before_P_2)
print(Mean_emp_after_P_2)
```

Now we calculate the diff-in-diff estimator only for restaurants who replied both times:
```{r}
did2 <- Mean_emp_after_NJ_2 - Mean_emp_after_P_2 - (Mean_emp_before_NJ_2 - Mean_emp_before_P_2)
print(did2)
```

#### Next we focus on the regression model:
$$
  E_{1i}-E_{0i}=\alpha+\delta NJ_i+U_i
$$
  
#### where $E_{1i}-E_{0i}$ is the change in employment in restaurant $i$ between the two waves, and $NJ_i$ is a dummy variable indicating whether restaurant $i$ is located in New Jersey.
  
### *(ii)* Estimate this model and next subsequently add characteristics of the restaurants observed in the first wave. But think carefully which characteristics can be included. How does the latter affect the estimate for the coefficient $\delta$?

### *Solution*
The base regression:
```{r}
df2 <- filter(df, STATUS2==1)
df2$emp_diff <- df2$emp_after-df2$emp_before
reg1 <- lm(emp_diff ~ STATE, data = df2)
cov1 <- vcovHC(reg1, type = "HC1")
rob1 <- sqrt(diag(cov1))
coeftest(reg1, vcov = sandwich)
```

Now we add some characteristics which might play a part in explaining the change in employment and would thus lead to an omitted variable bias in our base regression. We do not include the variable "PCTAFF", as it is determined by the level of the minimum wage increase, and is thus an intermediate variable and a bad control.

We first include all variables:
```{r}
reg2 <- lm(emp_diff ~ STATE + CENTRALJ + NORTHJ + SHORE + PA1 + CHAIN + CO_OWNED + NCALLS + WAGE_ST + INCTIME + FIRSTINC + BONUS + MEALS + OPEN + HRSOPEN +  PSODA + PFRY + PENTREE + NREGS + NREGS11, data = df2)
cov2 <- vcovHC(reg2, type = "HC1")
rob2    <- sqrt(diag(cov2))
coeftest(reg2, vcov = sandwich)
```

We wish to include variables which are relevant to assignment, but are not intermediate variables that affect outcome through their correlation with receiving treatment. We thus run the following regression using robust standard errors:
```{r}
regX <- lm(STATE ~  CHAIN + CO_OWNED + NCALLS + WAGE_ST + INCTIME + FIRSTINC + BONUS + MEALS + OPEN + HRSOPEN + PSODA + PFRY + PENTREE + NREGS + NREGS11, data = df2)
covX <- vcovHC(regX, type = "HC1")
robX    <- sqrt(diag(covX))
coeftest(regX, vcov = sandwich)
```

Next, we run sparser regressions:
```{r}
reg3 <- lm(emp_diff ~ STATE + CENTRALJ + NORTHJ + SHORE + PA1 + CHAIN + WAGE_ST + HRSOPEN + MEALS + OPEN + PSODA + PFRY + NREGS, data = df2)
cov3 <- vcovHC(reg3, type = "HC1")
rob3 <- sqrt(diag(cov3))
coeftest(reg3, vcov = sandwich)
```

```{r}
models <- list(reg1, reg2, reg3)
stargazer(models,
          se = list(rob1, rob2, rob3), header = FALSE,
          type = "latex", out = './Q2tbl.tex', omit.stat = c("adj.rsq", "ser", "f"), title = "Three OLS regressions with robust standard errors.",  
          table.placement = "H", no.space = TRUE)
```

### *(iii)* See STATA attachment

### *(iv)*  Check for the different characteristics if there is a common support for restaurants in New Jersey and Pennsylvania. And estimate a propensity score for being a restaurant in New Jersey.

### *Solution*
LOGIT MODELS:  
```{r}
df3 <- filter(df_state2, WAGE_ST>=0, INCTIME>=0, FIRSTINC>=0, PENTREE>=0, NREGS>=0, NREGS11>=0)
logit1 <- glm(STATE ~ WAGE_ST +INCTIME + FIRSTINC + BONUS + OPEN + HRSOPEN + PENTREE + NREGS11 + CHAIN + CO_OWNED, data = df3, family="binomial")
summary(logit1)

df4 <- filter(df_state2, WAGE_ST>=0)
logit2 <- glm(STATE ~ OPEN + HRSOPEN + WAGE_ST + CHAIN, data = df4, family="binomial")
summary(logit2)
```

PROPENSITY SCORES:
```{r}
df3 <- filter(df_state2, WAGE_ST>=0, INCTIME>=0, FIRSTINC>=0, PENTREE>=0, NREGS>=0, NREGS11>=0)
logit1 <- glm(STATE ~ WAGE_ST +INCTIME + FIRSTINC + BONUS + OPEN + HRSOPEN + PENTREE + NREGS11 + CHAIN + CO_OWNED, data = df3, family="binomial")
df3$prs1 <- predict(logit1, type = "response")

df4 <- filter(df_state2, WAGE_ST>=0)
logit2 <- glm(STATE ~ OPEN + HRSOPEN + WAGE_ST + CHAIN, data = df4, family="binomial")
df4$prs2 <- predict(logit2, type = "response")
```

GRAPH COMPARE PROPENSITY SCORES FOR LOGIT 1:
```{r}
ggplot(df3, aes(x = prs1, fill = STATE, colour = STATE)) +
  theme_minimal() +
  geom_density(alpha = 0.5, position = "identity")
ggsave('./prs1.pdf', width = 6, height = 4)

df3a <- filter(df3, STATE==1)
ggplot(df3a, aes(x = prs1, fill = STATE, colour = STATE)) +
  theme_minimal() +
  geom_density(alpha = 0.5, position = "identity")
ggsave('./prs1_S1.pdf', width = 6, height = 4)


df3b <- filter(df3, STATE==0)
ggplot(df3b, aes(x = prs1, fill = STATE, colour = STATE)) +
  theme_minimal() +
  geom_density(alpha = 0.5, position = "identity") + scale_x_continuous(limits = c(0, 1), breaks = 0:1)
ggsave('./prs1_S0.pdf', width = 6, height = 4)
```

GRAPH COMPARE PROPENSITY SCORES FOR LOGIT 2:
```{r}
ggplot(df4, aes(x = prs2, fill = STATE, colour = STATE)) +
  theme_minimal() +
  geom_density(alpha = 0.5, position = "identity")
ggsave('./prs2.pdf', width = 6, height = 4)

df4a <- filter(df4, STATE==1)
ggplot(df4a, aes(x = prs2, fill = STATE, colour = STATE)) +
  theme_minimal() +
  geom_density(alpha = 0.5, position = "identity") + scale_x_continuous(limits = c(0, 1), breaks = 0:1)
ggsave('./prs2_S1.pdf', width = 6, height = 4)

df4b <- filter(df4, STATE==0)
ggplot(df4b, aes(x = prs2, fill = STATE, colour = STATE)) +
  theme_minimal() +
  geom_density(alpha = 0.5, position = "identity") + scale_x_continuous(limits = c(0, 1), breaks = 0:1)
ggsave('./prs2_S0.pdf', width = 6, height = 4)
```

### *(v)* Use propensity score matching to estimate the average treatment effect on the treated for the employment before and after the minimum wage increase in New Jersey, so on $E_{0i}$ and $E_{1i}$ separately. + *(vi)* Now use propensity score matching to estimate the average treatment effect on the treated on the change in employment in the restaurants, so $E_{1i}-E_{0i}$. + *(vi)* Now use propensity score matching to estimate the average treatment effect on the treated on the change in employment in the restaurants, so $E_{1i}-E_{0i}$.

### *Solution*
We use the MatchIt package to create new "matched" data frames, sacrificing more than half of our observations in doing so.
```{r}
psm1 <- matchit(STATE ~ WAGE_ST +INCTIME + FIRSTINC + BONUS + OPEN + HRSOPEN + 
                  PENTREE + NREGS11 + CHAIN + CO_OWNED, 
                method = "nearest", df3)
psm2 <- matchit(STATE ~ OPEN + HRSOPEN + WAGE_ST + CHAIN,
                method = "nearest", df4)
df_psm1 <- match.data(psm1)
df_psm2 <- match.data(psm2)
```

Now we estimate the ATET with robust s.e.
```{r}
psm1_bef <- lm(emp_before ~ STATE, df_psm1)
psm2_bef <- lm(emp_before ~ STATE, df_psm2)

psm1_aft <- lm(emp_after ~ STATE, df_psm1)
psm2_aft <- lm(emp_after ~ STATE, df_psm2)

psm1_diff <- lm(emp_diff ~ STATE , df_psm1)
psm2_diff <- lm(emp_diff ~ STATE , df_psm2)


stargazer(psm1_bef, psm2_bef,
          psm1_aft, psm2_aft,
          psm1_diff, psm2_diff,
          se = list(robust(psm1_bef), robust(psm2_bef),
                    robust(psm1_aft), robust(psm2_aft),
                    robust(psm1_diff), robust(psm2_diff)),
          type = "latex", out = './q5.tex', omit.stat = c("adj.rsq", "ser", "f"), 
          title = "Propensity Score Matching Regressions",  
          table.placement = "H", no.space = TRUE, column.sep.width = "-5pt",
          column.labels = c("Larger Model", "Parser Model", 
                            "Larger Model", "Parser Model", 
                            "Larger Model", "Parser Model"))
```

### *(vii)* Now check the sensitivity of the propensity score matching estimate by also computing the weighting estimators for the average treatment effect on the treated.

### *Solution*
ATET FOR LOGIT1:
BEFORE:
```{r}
df3$D_b <- df3$STATE
df3$Y_b <- df3$emp_before
df3$DY_b <- df3$D_b*df3$Y_b

ATET_nominator_brackets_before <- df3$DY_b/df3$prs1 - ((1 - df3$D_b)*df3$Y_b)/(1 - df3$prs1)
ATET_nominator_before <- sum(df3$prs1*ATET_nominator_brackets_before, na.rm=T)
ATET_denominator_before <- sum(df3$prs1)
ATET_before <- ATET_nominator_before/ATET_denominator_before
print(ATET_before)
```
AFTER:
```{r}
df3$D_a <- df3$STATE
df3$Y_a <- df3$emp_after
df3$DY_a <- df3$D_a*df3$Y_a

ATET_nominator_brackets_after <- df3$DY_a/df3$prs1 - ((1 - df3$D_a)*df3$Y_a)/(1 - df3$prs1)
ATET_nominator_after <- sum(df3$prs1*ATET_nominator_brackets_after, na.rm=T)
ATET_denominator_after <- sum(df3$prs1)
ATET_after <- ATET_nominator_after/ATET_denominator_after
print(ATET_after)
```
DIFFERENCE:
```{r}
ATET_final_logit1 <- ATET_after - ATET_before
print(ATET_final_logit1)
```

SAME THING WITH LOGIT2:
BEFORE:
```{r}
df4$D_b <- df4$STATE
df4$Y_b <- df4$emp_before
df4$DY_b <- df4$D_b*df4$Y_b

ATET_nominator_brackets_before_2 <- df4$DY_b/df4$prs2 - ((1 - df4$D_b)*df4$Y_b)/(1 - df4$prs2)
ATET_nominator_before_2 <- sum(df4$prs2*ATET_nominator_brackets_before_2, na.rm=T)
ATET_denominator_before_2 <- sum(df4$prs2)
ATET_before_2 <- ATET_nominator_before_2/ATET_denominator_before_2
print(ATET_before_2)
```
AFTER:
```{r}
df4$D_a <- df4$STATE
df4$Y_a <- df4$emp_after
df4$DY_a <- df4$D_a*df4$Y_a

ATET_nominator_brackets_after_2 <- df4$DY_a/df4$prs2 - ((1 - df4$D_a)*df4$Y_a)/(1 - df4$prs2)
ATET_nominator_after_2 <- sum(df4$prs2*ATET_nominator_brackets_after_2, na.rm=T)
ATET_denominator_after_2 <- sum(df4$prs2)
ATET_after_2 <- ATET_nominator_after_2/ATET_denominator_after_2
print(ATET_after_2)
```
DIFFERENCE:
```{r}
ATET_final_logit2 <- ATET_after_2 - ATET_before_2
print(ATET_final_logit2)
```

NOW DO THE SAME THING DIRECTLY WITH THE DIFFERENCE
LOGIT1:
```{r}
df3$D_d <- df3$STATE
df3$Y_d <- df3$emp_diff
df3$DY_d <- df3$D_d*df3$Y_d

ATET_nominator_brackets_d <- df3$DY_d/df3$prs1 - ((1 - df3$D_d)*df3$Y_d)/(1 - df3$prs1)
ATET_nominator_d <- sum(df3$prs1*ATET_nominator_brackets_d, na.rm=T)
ATET_denominator_d <- sum(df3$prs1)
ATET_d <- ATET_nominator_d/ATET_denominator_d
print(ATET_d)
```
LOGIT2:
```{r}
df4$D_d <- df4$STATE
df4$Y_d <- df4$emp_diff
df4$DY_d <- df4$D_d*df4$Y_d

ATET_nominator_brackets_d2 <- df4$DY_d/df4$prs2 - ((1 - df4$D_d)*df4$Y_d)/(1 - df4$prs2)
ATET_nominator_d2 <- sum(df4$prs2*ATET_nominator_brackets_d2, na.rm=T)
ATET_denominator_d2 <- sum(df4$prs2)
ATET_d2 <- ATET_nominator_d2/ATET_denominator_d2
print(ATET_d2)
```
