source("xgboost.R")
source("xgboost_with_transform.R")
source("H2O.R")
r1 = read.csv("xgboost.csv", header=T)
r2 = read.csv("xgboost_with_transform.csv", header=T)
r3 = read.csv("H2O.csv", header=T)
r3 = 0.4*r1 + 0.4*r2 + 0.2*r3
write.csv(r3, "final_submission.csv", quote=F, row.names=F)