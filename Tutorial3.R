## START HERE (PLACE CSV IN SAME FOLDER AS THIS FILE)
library(dplyr)
# load data (n=100,000)
tut3_data <-read.csv("Tutor_1_3_data.csv", header=TRUE, sep = ",")
# The Premium feature (x = prem_feature) is the treatment variable subject to selection bias
# The AnonBrows (z = anon_choice_ne) is the partially-randomized intermediate variable determined by the treatment and a random perturbation
tut3_data <- rename(tut3_data, y = matchrcd_fg5, 
                    x_prem = prem_feature,
                    z = anon_choice_ne)

# split into womens and men data
women_data <- tut3_data[tut3_data$gender==0,]
men_data <- tut3_data[tut3_data$gender==1,]

# Below are analyses for Women. To analyze results for Men, replace data=women_data with data=men_data

# Table A5 counts and means
aggregate(y~x_prem +z, data=women_data,length)
aggregate(y~x_prem +z, data=women_data,mean)

aggregate(y~x_prem, data=women_data,length)
aggregate(y~x_prem, data=women_data,mean)

# Table A6
# Regression model. Simplifies the FDP analysis if there are many coniditional terms; but requires linearity assumption
regr.lm <- lm(y ~ z+ x_prem, data=women_data)
# The effect of Z on Y, obtained by regressing Y on Z and controlling for X, is beta_yz.x = -4.96
summary(regr.lm)
coeffs_regr <- coefficients(regr.lm)

regr2.lm <- lm(z ~ x_prem, data=women_data)
# The effect of X on Z is obtained by regressing Z on X, and using the coefficient of X:  beta_zx = 0.9. 
summary(regr2.lm)
coeffs_regr2 <- coefficients(regr2.lm)

# "Linear regression approach to computing the FDP adjustment."
# FDP effect Linear Model: The causal effect of X on Y, can be computed as: (beta_zx) * (beta_yz.x) =  (0.9)*(-4.96) = -4.46
FDPeffect_linear <-coeffs_regr[2]*coeffs_regr2[2]
FDPeffect_linear

# Compare to the "non-parametric FDP adjustment method" results using equation (A2).
# If there were a mismatch between the two methods of FDP adjustment, we would favor the non-parametric approach
# which does not assume a functional form.