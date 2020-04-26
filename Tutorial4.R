### Information from an RCT used to transport the measured causal effect to a different non-RCT environment S

### Note the data used here represents an RCT scenario in which causal estimates are transported to the observational (non-RCT) scenario represented by Figure A3 (dependent variable Y here is TotMatch). 
### Besides the data used here, we generated a separate sample that simulates the (non-RCT) environment S, generated 
### by a process that is faithful to Figure A3 including the confounding influences (dashed-line arrows) that jointly affect X and Y. 
### The transport formula (equation A3) uses a combination of interventional outcomes from the RCT (the CSV file used by this script), together with information about the non-RCT environment S.
### The depiction of environmental variable S affecting Z in Fig. A3 indicates that Z has a different response to X in the non-RCT environment S. 
### The task here is to transport the RCT result to a causal effect in the (non-RCT or observational data) environment S. 
### Since only two basic summary statistics are needed from the data sample in environment S to do this task, we do not need to reference the non-RCT sample here.


## START HERE (PLACE CSV IN SAME FOLDER AS THIS FILE)
library(dplyr)
# load data (n=100,000)
tut4_data <-read.csv("Tutor_4_data.csv", header=TRUE, sep = ",")

## This is data that simulates an RCT process, in which x_prem_rct is a randomized variable. 
## This data is used in the transport formula (equation A3) to compute the causal effect in an observational-data environment S 
tut4_data <- rename(tut4_data, y = total_matches, 
                    x_prem_rct = prem_rct,
                    z = anon)

# split into womens and men data
women_data <- tut4_data[tut4_data$gender==0,]
men_data <- tut4_data[tut4_data$gender==1,]

## Table A8 (women). 
### This is the RCT information to be used in the transport formula, together with two basic statistics from environment S:  P(Z=1|X=1, S) = 0.7  and  P(Z=0|X=1, S) = 0.3.
## First two rows of Table A8
aggregate(y ~ x_prem_rct + z, data=women_data, mean)
## Third row of Table A8
aggregate(y ~ x_prem_rct, data=women_data, mean)

## Similar table for men
aggregate(y ~ x_prem_rct + z, data=men_data, mean)
aggregate(y ~ x_prem_rct, data=men_data, mean)
