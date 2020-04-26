# Simulating collider DAG from Figs. A4 and A5. Using random.seed(123) allows reproducing tables A9-A11

# Simulate data from figure A4
# Fig A4 DAG: Feature -> Web_use <- Purchases  
set.seed(123)
pop_size <- 100000
feature <- rbinom(pop_size, 1, 0.5)
purchase <-rnorm(pop_size)
z <- feature + purchase - 1 # linear combination
pr <- 1/(1+exp(-z))  # inverse-logit function
web_use = rbinom(pop_size,1,pr)  # bernoulli with probability pr

#Table A9, column (1), true model
lm1 <- lm(purchase ~ feature)
summary(lm1)

# Table A9, column (2)
# When conditioning on the collider, we see a very strong spurious relationship
lm2 <- lm(purchase ~ feature + web_use)
summary(lm2)

# Table A9, column (3); includes F * W interaction
# When conditioning on the collider, we see a strong spurious relationship
lm3 <- lm(purchase ~ feature * web_use)
summary(lm3)

tut5_data <- data.frame(feature,purchase,web_use)

# split into subgroups web_use=0 and web_use=1
data_w0 <- tut5_data[tut5_data$web_use==0,]
data_w1 <- tut5_data[tut5_data$web_use==1,]

# Table 10: subgroup level regressions 
# When subgrouping by a collider, we see a strong spurious relationship
lm4 <- lm(purchase ~ feature, data=data_w0)
lm5 <- lm(purchase ~ feature, data=data_w1)
summary(lm4)
summary(lm5)


# Simulate data from figure A4
# A phantom mediation effect caused by an unobserved confounder U;
# the spurious "mediator" is actually a collider. 
## Fig A4 DAG:  Feature -> Web_use <- U -> Purchase

U <- rnorm(pop_size)
### Purchase is regenerated
purchase1 <- U + 0.1*rnorm(pop_size)
### Web_use is regenerated
z1 <- feature + U - 1 # linear combination
pr1 <- 1/(1+exp(-z1))  # inverse-logit function
web_use1 = rbinom(pop_size,1,pr1)  # bernoulli with probability pr1

#Table A11, column (1), true model
lm6 <- lm(purchase1 ~ feature)
summary(lm6)

# Table A11, column (2)
# When conditioning on the collider, we see a strong spurious effect of feature
lm7 <- lm(purchase1 ~ feature + web_use1)
summary(lm7)
# similar result if including interaction terms, such as
# lm7 <- lm(purchase1 ~ feature * web_use1)

# Table A11, column (3), true model
# When conditioning on W and U, the spurious path is blocked. However, it may not be feasible to observe U.
lm8 <- lm(purchase1 ~ feature + web_use1 + U)
summary(lm8)
# similar result if including interaction terms, such as
# lm8 <- lm(purchase1 ~ feature * web_use1 * U )


