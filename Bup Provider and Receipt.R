library(gap)
library(strucchange)

# Input data
patients=c(19026,63174,108325,170005,288677,436904,517292,603494,710244,730890,774276,861312,914792,1003521,1113512,1186830,1295524,1361265,1440878)
providers=c(1800,3256,5484,8556,10637,14365,15818,18024,20148,22198,23629,26441,30987,35531,44183,56638,74344,94223,114376)

length(providers); length(patients)

data.all = data.frame(patients=patients,providers=providers)

# Build two time segments: 2003-2016 vs. 2017-2021
patients_timesegment1=patients[1:14]
patients_timesegment2=patients[15:19]

providers_timesegment1=providers[1:14]
providers_timesegment2=providers[15:19]

# First approach
chow.test(patients_timesegment1,providers_timesegment1,
          patients_timesegment2,providers_timesegment2,x=NULL)

# Second approach
sctest(data.all$patients ~ data.all$providers, type = "Chow", point = 14)

# Quantify the associations
mod1=lm(patients_timesegment1~providers_timesegment1)
mod1
confint(mod1)

mod2=lm(patients_timesegment2~providers_timesegment2)
mod2
confint(mod2)