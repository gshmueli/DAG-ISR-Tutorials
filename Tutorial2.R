## START HERE (PLACE CSV IN SAME FOLDER AS THIS FILE)
library(dplyr)
# load data (n=100,000)
tut2_data <-read.csv("Tutor_2_data.csv", header=TRUE, sep = ",")

# x is Treatment variable AnonBrows in Table A2 and Figure 5
# z1 (view_sent) is ViewsSnt post-treatment variable
# z2 (msg_rcd_weaksignal) is MsgRcd post-treatment variable in Figure 5
# z3 (msg_reply_tofirstmsg_fg5) is MsgSnt post-treatment variable in Figure 5
# y (matchrcd_fg5) is MatchRcd outcome in Figure 5
tut2_data <- rename(tut2_data, y = matchrcd_fg5, 
                    x = anon,
                    z1 = view_sent,
                    z2 = msg_rcd_weaksignal,
                    z3 = msg_reply_tofirstmsg_fg5)

# split into womens and men data
women_data <- tut2_data[tut2_data$gender==0,]
men_data <- tut2_data[tut2_data$gender==1,]

### Tables A2 and A3, Row 1: Effect of Treatment on MatchRcd. 
# Column (1) Matches the -5 estimate difference of means
regr.lm <- lm(y ~ x, women_data)
summary(regr.lm)

# Table A2 (Columns (2)). Terribly biased results once you include post-treatment variables. 
regr3.lm <- lm(y ~ x + z1 + z2 + z3, women_data)
summary(regr3.lm)

# Table A3 (Column (3)). Model with interaction betwen treatment and attractiveness score, and other covariates
regr_full.lm <- lm(y ~ x*attract + age  + asian + black + indian + latino + white, women_data)
summary(regr_full.lm)

## Table A3, Row 1: AnonBrows -> MatchRcd
coefficients(regr.lm)[2]

#### Producing the first two columns of Table A7
### ATE on match rcd for women: About -5,  Table A7 numbers also correspond with Table A3, Row 1
women_means <- aggregate(women_data$y, by=list(women_data$x), FUN=mean)
women_means[2,2] - women_means[1,2]

### Table A3, Row 2: Effect of Treatment on ViewsSent 
regr_fullinteract.lm <- lm(z1 ~ x*attract + age  + asian + black + indian + latino + white + u, data=women_data)
summary(regr_fullinteract.lm)
regr_full.lm <- lm(z1 ~ x + attract + age  + asian + black + indian + latino + white + u, data=women_data)
summary(regr_full.lm)
regr.lm <- lm(z1~x, women_data)
summary(regr.lm)

## Table A3, Row 2: AnonBrows -> ViewsSnt
coefficients(regr.lm)[2]


## Table A3 Row 3, and Table A4 Row 1: Marginal effects of ViewsSent on MsgReceived for Treatment and Control groups (slightly biased due to U)
### Effect of ViewsSent on MsgReceived, Table A3
regr4_2way.lm <-lm(z2~ z1*x  +  attract + age  + asian + black + indian + latino + white, data=women_data)
summary(regr4_2way.lm)
regr4_2way_u.lm <-lm(z2~ z1*x  +  attract + age  + asian + black + indian + latino + white + u, data=women_data)
summary(regr4_2way_u.lm)

coeffs_regr4 <- coefficients(regr4_2way.lm)
coeffs_regr4_unbiased <- coefficients(regr4_2way_u.lm)

## Table A3, Row 3: ViewsSnt -> MsgReceived
z1_marg_effect_treatment <-coeffs_regr4[2] + coeffs_regr4[11]
z1_marg_effect_treatment
z1_marg_effect_control <-coeffs_regr4[2] + 0*coeffs_regr4[11]
z1_marg_effect_control

### Table A3 row 4: Effect of MsgReceived on MsgSnt
regr5_full.lm <-lm(z3 ~ z2 + age + attract + asian + black + indian + latino + white + u, data=women_data)
## 0.85 (unbiased) coefficient of z2 matches construction of MsgReceived on MsgSent 
summary(regr5_full.lm)
regr5_no_u.lm <-lm(z3 ~ z2 + age + attract + asian + black + indian + latino + white, data=women_data)
summary(regr5_no_u.lm)
regr5.lm <-lm(z3 ~ z2, data=women_data)
summary(regr5.lm)

### Table A3 row 4 ,MsgRcd -> MsgSnt, slightly biased due to omitted U
coefficients(regr5_no_u.lm)[2]


### Table A3 row 5: Effect of MsgSnt on MatchRcd
# Expect an effect size of 0.89 based on the data generating process
regr6.lm <-lm(y ~ z3 + age + attract + asian + black + indian + latino + white + u, data=women_data)
summary(regr6.lm)
regr6_no_u.lm <-lm(y ~ z3 + age + attract + asian + black + indian + latino + white, data=women_data)
summary(regr6_no_u.lm)
regr6_nocontrols.lm <-lm(y ~ z3, data=women_data)
summary(regr6_nocontrols.lm)

### Table A3 row 5, MsgSnt -> MatchRcd
coefficients(regr6_no_u.lm)[2]

#Table A4 row 1, col 1, AnonBrows=0
z1_marg_effect_control
#Table A4 row 1, col 2, AnonBrows=1
z1_marg_effect_treatment

## Table A4 row 2: Marginal effects of ViewsSent on MsgReceived for Treatment and Control groups (unbiased)
z1_marg_effect_treatment_unbiased <-coeffs_regr4_unbiased[2] + coeffs_regr4_unbiased[12]
z1_marg_effect_control_unbiased <-coeffs_regr4_unbiased[2] + 0*coeffs_regr4_unbiased[12]

#Table A4 row 2, col1, AnonBrows=0
z1_marg_effect_control_unbiased

#Table A4 row 2, col2, AnonBrows=1
z1_marg_effect_treatment_unbiased