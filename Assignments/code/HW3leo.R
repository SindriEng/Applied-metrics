#--------------------- Libraries ----------------------------------
library(tidyverse)
library(stargazer)
library(xtable)
library(plm)
library(haven)
library(rio)
library(RCT)
library(MatchIt)

# Make a function to calculate robust s.e.
calc_robust_se <- function(model){
  cov_model <- vcovHC(model, type = "HC3")
  robust_se <- sqrt(diag(cov_model))
  return(robust_se)
}
#----------------------------------------------------------------------
df <- read_dta("C:/Users/sindr/Desktop/Tinbergen/2nd-year/Block 1/Applied-Microeconometrics/Assignments/data/cardkrueger.dta")
df$STATE <- as.factor(df$STATE)
df$MEALS <- as.factor(df$MEALS)
df$employees_w0 <- df$EMPFT + df$EMPPT/2 + df$NMGRS
df$employees_w1 <- df$EMPFT2 + df$EMPPT2/2 + df$NMGRS2
df$employees_diff <- df$employees_w1 - df$employees_w0

# Q1: Estimate diff-in-diff with missing data
mean_nj_w1 <- mean(df[df$STATE == 1,]$employees_w1, na.rm = TRUE)
mean_nj_w0 <- mean(df[df$STATE == 1,]$employees_w0, na.rm = TRUE)
mean_pa_w1 <- mean(df[df$STATE == 0,]$employees_w1, na.rm = TRUE)
mean_pa_w0 <- mean(df[df$STATE == 0,]$employees_w0, na.rm = TRUE)
did <- (mean_nj_w1 - mean_pa_w1) - (mean_nj_w0 - mean_pa_w0)

# Estimate diff-in-diff with non-missing data
df <- drop_na(df, "employees_diff")
mean_nj_w1 <- mean(df[df$STATE == 1,]$employees_w1, na.rm = TRUE)
mean_nj_w0 <- mean(df[df$STATE == 1,]$employees_w0, na.rm = TRUE)
mean_pa_w1 <- mean(df[df$STATE == 0,]$employees_w1, na.rm = TRUE)
mean_pa_w0 <- mean(df[df$STATE == 0,]$employees_w0, na.rm = TRUE)
did_no_miss <- (mean_nj_w1 - mean_pa_w1) - (mean_nj_w0 - mean_pa_w0)

# Q2: Run a regression with and without covariates
lm_dummy <- lm(employees_diff ~ STATE, df) 
lm_some <- lm(employees_diff ~ STATE + 
                BONUS + CHAIN + CO_OWNED + HRSOPEN + NREGS11 + OPEN + PENTREE + WAGE_ST, df) 
lm_all <- lm(employees_diff ~ STATE + 
               STATE + 
               BONUS + CHAIN + CO_OWNED + FIRSTINC + HRSOPEN + INCTIME + NREGS + 
               NREGS11 + OPEN + PENTREE + PFRY + PSODA + WAGE_ST + MEALS, df) 
stargazer(lm_dummy, lm_some, lm_all,
          se = list(calc_robust_se(lm_dummy), calc_robust_se(lm_some), calc_robust_se(lm_all)),
          type = "latex", out = './q2.tex', omit.stat = c("adj.rsq", "ser", "f"), title = "The regressiion results",  
          table.placement = "H", no.space = TRUE)

# Q3: Make a balancing table
balance <- balance_table(df[,-c(1, 5:11, 12:14, 20,28:50)], "STATE") %>%
  as.data.frame()
balance <- cbind(balance[,c(1:3)], round(balance[2]-balance[3],3), balance[,4])
colnames(balance) <- c("Variable", "New Jersey", "Pennsylvania", "Diff in means", "p-value")
stargazer(balance, type="latex", summary=FALSE, 
          header = F, out = './q3.tex', title = "The balancing table of variables")

# Q4: Check for common support of covariates and estimate PSM

Pr_D1_BONUS1 = c("BONUS = 1",nrow(df[df$STATE==1 & df$BONUS==1,])/nrow(df[df$BONUS==1,]))
Pr_D1_BONUS0 = c("BONUS = 0",nrow(df[df$STATE==1 & df$BONUS==0,])/nrow(df[df$BONUS==0,]))
Pr_D1_CHAIN1 = c("CHAIN = 1",nrow(df[df$STATE==1 & df$CHAIN==1,])/nrow(df[df$CHAIN==1,]))
Pr_D1_CHAIN2 = c("CHAIN = 2",nrow(df[df$STATE==1 & df$CHAIN==2,])/nrow(df[df$CHAIN==2,]))
Pr_D1_CHAIN3 = c("CHAIN = 3",nrow(df[df$STATE==1 & df$CHAIN==3,])/nrow(df[df$CHAIN==3,]))
Pr_D1_CHAIN4 = c("CHAIN = 4",nrow(df[df$STATE==1 & df$CHAIN==4,])/nrow(df[df$CHAIN==4,]))
Pr_D1_CO_OWNED1 = c("CO-OWNED = 0",nrow(df[df$STATE==1 & df$CO_OWNED==1,])/nrow(df[df$CO_OWNED==1,]))
Pr_D1_CO_OWNED0 = c("CO-OWNED = 1",nrow(df[df$STATE==1 & df$CO_OWNED==0,])/nrow(df[df$CO_OWNED==0,]))
Pr_D1_FIRSTINC0 = c("FIRSTINC<0.1",nrow(df[df$STATE==1 & df$FIRSTINC<0.1,])/nrow(df[df$FIRSTINC<0.1,]))
Pr_D1_FIRSTINC1 = c("0.1 <= FIRSTINC<0.2",nrow(df[df$STATE==1 & df$FIRSTINC>=0.1 & df$FIRSTINC<0.2,])/nrow(df[df$FIRSTINC>=0.1 & df$FIRSTINC<0.2,]))
Pr_D1_FIRSTINC2 = c("0.2 <= FIRSTINC<0.3",nrow(df[df$STATE==1 &df$FIRSTINC>=0.2 & df$FIRSTINC<0.3,])/nrow(df[df$FIRSTINC>=0.2 & df$FIRSTINC<0.3,]))
Pr_D1_FIRSTINC3 = c("FIRSTINC>=0.3",nrow(df[df$STATE==1 & df$FIRSTINC>=0.3,])/nrow(df[df$FIRSTINC>=0.3,]))
Pr_D1_HRSOPEN0 = c("HRSOPEN<12.5",nrow(df[df$STATE==1 & df$HRSOPEN<12.5,])/nrow(df[df$HRSOPEN<12.5,]))
Pr_D1_HRSOPEN1 = c("12.5 <= HRSOPEN<15",nrow(df[df$STATE==1 & df$HRSOPEN>=12.5 & df$HRSOPEN<15,])/nrow(df[df$HRSOPEN>=12.5 & df$HRSOPEN<15,]))
Pr_D1_HRSOPEN2 = c("15 <= HRSOPEN<17.5",nrow(df[df$STATE==1 &df$HRSOPEN>=15 & df$HRSOPEN<17.5,])/nrow(df[df$HRSOPEN>=15 & df$HRSOPEN<17.5,]))
Pr_D1_HRSOPEN3 = c("HRSOPEN>=17.5",nrow(df[df$STATE==1 & df$HRSOPEN>=17.5,])/nrow(df[df$HRSOPEN>=17.5,]))
Pr_D1_INCTIME0 = c("INCTIME<10",nrow(df[df$STATE==1 & df$INCTIME<10,])/nrow(df[df$INCTIME<10,]))
Pr_D1_INCTIME1 = c("10 <= INCTIME<20",nrow(df[df$STATE==1 & df$INCTIME>=10 & df$INCTIME<20,])/nrow(df[df$INCTIME>=10 & df$INCTIME<20,]))
Pr_D1_INCTIME2 = c("20 <= INCTIME<25",nrow(df[df$STATE==1 & df$INCTIME>=20 & df$INCTIME<25,])/nrow(df[df$INCTIME>=20 & df$INCTIME<25,]))
Pr_D1_INCTIME3 = c("INCTIME>=25",nrow(df[df$STATE==1 & df$INCTIME>=25,])/nrow(df[df$INCTIME>=25,]))
Pr_D1_NREGS0 = c("NREGS<3",nrow(df[df$STATE==1 & df$NREGS<3,])/nrow(df[df$NREGS<3,]))
Pr_D1_NREGS1 = c("3 <= NREGS<4",nrow(df[df$STATE==1 & df$NREGS>=3 & df$NREGS<4,])/nrow(df[df$NREGS>=3 & df$NREGS<4,]))
Pr_D1_NREGS2 = c("4 <= NREGS<6",nrow(df[df$STATE==1 & df$NREGS>=4 & df$NREGS<6,])/nrow(df[df$NREGS>=4 & df$NREGS<6,]))
Pr_D1_NREGS3 = c("NREGS>=5",nrow(df[df$STATE==1 & df$NREGS>=6,])/nrow(df[df$NREGS>=6,]))
Pr_D1_NREGS110 = c("NREGS11<2",nrow(df[df$STATE==1 & df$NREGS11<2,])/nrow(df[df$NREGS11<2,]))
Pr_D1_NREGS111 = c("2 <= NREGS11<3",nrow(df[df$STATE==1 & df$NREGS11>=2 & df$NREGS11<3,])/nrow(df[df$NREGS11>=2 & df$NREGS11<3,]))
Pr_D1_NREGS112 = c("3 <= NREGS11<4",nrow(df[df$STATE==1 & df$NREGS11>=3 & df$NREGS11<4,])/nrow(df[df$NREGS11>=3 & df$NREGS11<4,]))
Pr_D1_NREGS113 = c("NREGS11>=4",nrow(df[df$STATE==1 & df$NREGS11>=4,])/nrow(df[df$NREGS11>=4,]))
Pr_D1_OPEN0 = c("OPEN<7.5",nrow(df[df$STATE==1 & df$OPEN<7.5,])/nrow(df[df$OPEN<7.5,]))
Pr_D1_OPEN1 = c("7.5 <= OPEN<9",nrow(df[df$STATE==1 & df$OPEN>=7.5 & df$OPEN<9,])/nrow(df[df$OPEN>=7.5 & df$OPEN<9,]))
Pr_D1_OPEN2 = c("9 <= OPEN<10.5",nrow(df[df$STATE==1 & df$OPEN>=9 & df$OPEN<10.5,])/nrow(df[ df$OPEN>=9 & df$OPEN<10.5,]))
Pr_D1_OPEN3 = c("OPEN>=10.5",nrow(df[df$STATE==1 & df$OPEN>=10.5,])/nrow(df[df$OPEN>=10.5,]))
Pr_D1_PCTAFF0 = c("PCTAFF<25",nrow(df[df$STATE==1 & df$PCTAFF<25,])/nrow(df[df$PCTAFF<25,]))
Pr_D1_PCTAFF1 = c("25 <= PCTAFF<50",nrow(df[df$STATE==1 & df$PCTAFF>=25 & df$PCTAFF<50,])/nrow(df[df$PCTAFF>=25 & df$PCTAFF<50,]))
Pr_D1_PCTAFF2 = c("50 <= PCTAFF<75",nrow(df[df$STATE==1 & df$PCTAFF>=50 & df$PCTAFF<75,])/nrow(df[df$PCTAFF>=50 & df$PCTAFF<75,]))
Pr_D1_PCTAFF3 = c("PCTAFF>=75",nrow(df[df$STATE==1 & df$PCTAFF>=75,])/nrow(df[df$PCTAFF>=75,]))
Pr_D1_PENTREE0 = c("PENTREE<1",nrow(df[df$STATE==1 & df$PENTREE<1,])/nrow(df[df$PENTREE<1,]))
Pr_D1_PENTREE1 = c("1 <= PENTREE<2",nrow(df[df$STATE==1 & df$PENTREE>=1 & df$PENTREE<2,])/nrow(df[df$PENTREE>=1 & df$PENTREE<2,]))
Pr_D1_PENTREE2 = c("PENTREE>=2",nrow(df[df$STATE==1 & df$PENTREE>=2,])/nrow(df[df$PENTREE>=2,]))
Pr_D1_PFRY0 = c("PFRY<0.8",nrow(df[df$STATE==1 & df$PFRY<0.8,])/nrow(df[df$PFRY<0.8,]))
Pr_D1_PFRY1 = c("0.8 <= PFRY<1",nrow(df[df$STATE==1 & df$PFRY>=0.8 & df$PFRY<1,])/nrow(df[df$PFRY>=0.8 & df$PFRY<1,]))
Pr_D1_PFRY2 = c("PFRY>=1",nrow(df[df$STATE==1 & df$PFRY>=1,])/nrow(df[df$PFRY>=1,]))
Pr_D1_PSODA0 = c("PSODA<1",nrow(df[df$STATE==1 & df$PSODA<1,])/nrow(df[df$PSODA<1,]))
Pr_D1_PSODA1 = c("1 <= PSODA<1.125",nrow(df[df$STATE==1 & df$PSODA>=1 & df$PSODA<1.125,])/nrow(df[df$PSODA>=1 & df$PSODA<1.125,]))
Pr_D1_PSODA2 = c("PSODA>=1.125",nrow(df[df$STATE==1 & df$PSODA>=1.125,])/nrow(df[df$PSODA>=1.125,]))
Pr_D1_WAGE_ST0 = c("WAGE_ST<4.5",nrow(df[df$STATE==1 & df$WAGE_ST<4.5,])/nrow(df[df$WAGE_ST<4.5,]))
Pr_D1_WAGE_ST1 = c("4.5 <= WAGE_ST<5",nrow(df[df$STATE==1 & df$WAGE_ST>=4.5 & df$WAGE_ST<5,])/nrow(df[df$WAGE_ST>=4.5 & df$WAGE_ST<5,]))
Pr_D1_WAGE_ST2 = c("WAGE_ST>=5",nrow(df[df$STATE==1 & df$WAGE_ST>=5,])/nrow(df[df$WAGE_ST>=5,]))

table = rbind.data.frame(Pr_D1_BONUS1,Pr_D1_BONUS0, Pr_D1_CHAIN1,Pr_D1_CHAIN2,Pr_D1_CHAIN3,Pr_D1_CHAIN4,Pr_D1_CO_OWNED1 , Pr_D1_CO_OWNED0 , Pr_D1_FIRSTINC0 , Pr_D1_FIRSTINC1 , Pr_D1_FIRSTINC2 , Pr_D1_FIRSTINC3 , Pr_D1_HRSOPEN0 , Pr_D1_HRSOPEN1 , Pr_D1_HRSOPEN2 , Pr_D1_HRSOPEN3 , Pr_D1_INCTIME0 , Pr_D1_INCTIME1 , Pr_D1_INCTIME2 , Pr_D1_INCTIME3 , Pr_D1_NREGS0 , Pr_D1_NREGS1 , Pr_D1_NREGS2 , Pr_D1_NREGS3 , Pr_D1_NREGS110 , Pr_D1_NREGS111 , Pr_D1_NREGS112 , Pr_D1_NREGS113 , Pr_D1_OPEN0 , Pr_D1_OPEN1 , Pr_D1_OPEN2 , Pr_D1_OPEN3 , Pr_D1_PCTAFF0 , Pr_D1_PCTAFF1 , Pr_D1_PCTAFF2 , Pr_D1_PCTAFF3 , Pr_D1_PENTREE0 , Pr_D1_PENTREE1 , Pr_D1_PENTREE2 , Pr_D1_PFRY0 , Pr_D1_PFRY1 , Pr_D1_PFRY2 , Pr_D1_PSODA0 , Pr_D1_PSODA1 , Pr_D1_PSODA2 , Pr_D1_WAGE_ST0 , Pr_D1_WAGE_ST1 , Pr_D1_WAGE_ST2)
colnames(table) <- c("Variable and values, x","Pr(D=1|X=x)")
table$`Pr(D=1|X=x)` = as.numeric(as.character(table$`Pr(D=1|X=x)`))


print(xtable(table,digits=3,
             caption="Informal test for common support of each covariate (individually).",
             type = "latex", label = "commonsupp"),
      file = "./Tablecommonsup.tex",
      table.placement = getOption("xtable.table.placement", "h"),
      caption.placement = "top", include.rownames=FALSE)

df_no_miss <- drop_na(df, c("PCTAFF", 
                            "BONUS", "CHAIN", "CO_OWNED", "NREGS11", "HRSOPEN", "OPEN", "PENTREE", "WAGE_ST",))
psm <- glm(STATE ~ PCTAFF + 
             BONUS + CHAIN + CO_OWNED + HRSOPEN + NREGS11 + OPEN + PENTREE + WAGE_ST, 
             family = binomial(), df_no_miss)
df_no_miss$psm <- predict(psm, type="response")
ggplot(df_no_miss, aes(x = psm, fill = STATE, colour = STATE)) +
  theme_minimal() +
  geom_density(alpha = 0.5, position = "identity")
ggsave('./density.pdf', width = 6, height = 4)

# Apply PSM with and without replacement and min and max
psm_match <- matchit(STATE ~ PCTAFF + 
                       CHAIN + CO_OWNED +
                       WAGE_ST + BONUS + 
                       OPEN + HRSOPEN + PENTREE + NREGS11, method = "nearest", df_no_miss)
psm_match_replace <- matchit(STATE ~ PCTAFF + 
                               CHAIN + CO_OWNED +
                               WAGE_ST + BONUS + 
                               OPEN + HRSOPEN + PENTREE + NREGS11, method = "nearest",
                             replace = TRUE, df_no_miss)
df_no_miss_min_max <- df_no_miss[df_no_miss$psm > min(df_no_miss[df_no_miss$STATE == 1,]$psm) &
             df_no_miss$psm < max(df_no_miss[df_no_miss$STATE == 0,]$psm),]
psm_match_replace_min_max <- matchit(STATE ~ PCTAFF + 
                               CHAIN + CO_OWNED +
                               WAGE_ST + BONUS + 
                               OPEN + HRSOPEN + PENTREE + NREGS11, method = "nearest",
                             replace = TRUE, df_no_miss_min_max)
df_match <- match.data(psm_match)
df_match_replace <- match.data(psm_match_replace)
df_match_replace_min_max <- match.data(psm_match_replace_min_max)

# Check for balance with matched groups
balance <- balance_table(df_match_replace[,-c(1, 5:11, 12:14, 16:17, 20, 23:24, 26, 28:53)], "STATE") %>%
  as.data.frame()
balance <- cbind(balance[,c(1:3)], round(balance[2]-balance[3],3), balance[,4])
colnames(balance) <- c("Variable", "New Jersey", "Pennsylvania", "Diff in means", "p-value")
stargazer(balance, type="latex", summary=FALSE, 
          header = F, out = './q4.tex', title = "The balancing table of variables after matching")

# Q 5&6: Estimate ATET
# Bootsrtap s.e.
psm_w0_near_replace <- lm(employees_w0 ~ STATE, df_match_replace)
psm_w0_near <- lm(employees_w0 ~ STATE, df_match)
psm_w0_min_max <- lm(employees_w0 ~ STATE, df_match_replace_min_max)
psm_w1_near_replace <- lm(employees_w1 ~ STATE, df_match_replace)
psm_w1_near <- lm(employees_w1 ~ STATE, df_match)
psm_w1_min_max <- lm(employees_w1 ~ STATE, df_match_replace_min_max)
psm_diff_near_replace <- lm(employees_diff ~ STATE , df_match_replace)
psm_diff_near <- lm(employees_diff ~ STATE , df_match)
psm_diff_min_max <- lm(employees_diff ~ STATE, df_match_replace_min_max)

stargazer(psm_w0_near, psm_w0_near_replace, psm_w0_min_max,
          psm_w1_near, psm_w1_near_replace, psm_w1_min_max,
          psm_diff_near, psm_diff_near_replace, psm_diff_min_max,
          se = list(calc_robust_se(psm_w0_near), calc_robust_se(psm_w0_near_replace), calc_robust_se(psm_w0_min_max),
                    calc_robust_se(psm_w1_near), calc_robust_se(psm_w1_near_replace), calc_robust_se(psm_w1_min_max),
                    calc_robust_se(psm_diff_near), calc_robust_se(psm_diff_near_replace), calc_robust_se(psm_diff_min_max)),
          type = "latex", out = './q5.tex', omit.stat = c("adj.rsq", "ser", "f"), title = "The regressiion results with PSM",  
          table.placement = "H", no.space = TRUE, column.sep.width = "-5pt",
          column.labels = c("With R", "Without R", "MinMax", "With R", "Without R", "MinMax", "With R", "Without R", "MinMax"),
          notes = c("With R - nearest neighbor with replacement", 
          "Without R - nearest neighbor without replacement",
          "MinMax - minima and maxima comparison"))

# Q7: Estimate ATET with weighting estimators
df_no_miss$STATE <-  as.numeric(as.character(df_no_miss$STATE))
df_no_miss$psm <- as.numeric(as.character(df_no_miss$psm))
df_no_miss$employees_w0 <-  as.numeric(as.character(df_no_miss$employees_w0))
df_no_miss$employees_w1 <-  as.numeric(as.character(df_no_miss$employees_w1))
df_no_miss$employees_w1 <-  as.numeric(as.character(df_no_miss$employees_w1))
df_no_miss$employees_diff <-  as.numeric(as.character(df_no_miss$employees_diff))
df_no_miss$psm_weights <- (1 / df_no_miss$psm * df_no_miss$STATE + 1 / (1 - df_no_miss$psm) * (1 - df_no_miss$STATE))

psm_wights_w0 <- lm(employees_w0 ~ STATE, df_no_miss, weights = psm_weights)
psm_wights_w1 <- lm(employees_w1 ~ STATE, df_no_miss, weights = psm_weights)
psm_wights_diff <- lm(employees_diff ~ STATE, df_no_miss, weights = psm_weights)
stargazer(psm_wights_w0, psm_wights_w1, psm_wights_diff,
          se = list(calc_robust_se(psm_wights_w0), calc_robust_se(psm_wights_w1), calc_robust_se(psm_wights_diff)),
          type = "latex", out = './q7.tex', omit.stat = c("adj.rsq", "ser", "f"), title = "The regressiion results with weightning estimator",  
          table.placement = "H", no.space = TRUE, column.sep.width = "-5pt")



