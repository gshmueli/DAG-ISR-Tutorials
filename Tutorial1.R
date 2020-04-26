## START HERE (PLACE CSV IN SAME FOLDER AS THIS FILE)
# load data (n=100,000)
tut1_data <-read.csv("Tutor_1_3_data.csv", header=TRUE, sep = ",")

# split into womens and men data
women_data <- tut1_data[tut1_data$gender==0,]
men_data <- tut1_data[tut1_data$gender==1,]
 
# prem_feature is the treatment variable in the quasi-experiment: The anonymous browsing feature that is gifted to selected VIPs 
# (prem_feature is AnonBrows in Table A1)
# u is the set of Selection factors other than the demographic covariates for attractiveness score, age and race
# regression models for women, with and without u
regr_full_quasi.lm <- lm(matchrcd_fg5 ~ prem_feature + age + attract + white + black + asian + latino + indian, data=women_data)
regr_full_quasi_u.lm <- lm(matchrcd_fg5 ~ prem_feature + age + attract + white + black + asian + latino + indian + u, data=women_data)

# Results appearing in Table A1 of Tutorial 1
summary(regr_full_quasi.lm)
summary(regr_full_quasi_u.lm)


# regression models for men, with and without u

# regr_full_quasi.lm <- lm(matchrcd_fg5 ~ prem_feature + age + attract + white + black + asian + latino + indian, data=men_data)
# regr_full_quasi_u.lm <- lm(matchrcd_fg5 ~ prem_feature + age + attract + white + black + asian + latino + indian + u, data=men_data)

