#Linear Model Selection
rm(list = setdiff(ls(), lsf.str()))
library(readxl)
library(leaps)
nba<- read_excel("~/Basketball/bballdata/Code/nbaMaster.xlsx", range="A1:BP1480", col_names = TRUE,na="")
dim(nba)
attach(nba)
summary(nba)
Year=as.factor(Year)
Won=as.factor(Won)
MVP=as.factor(MVP)


variables<-colnames(nba)
rank_stats<-c(variables[38],variables[42],variables[45],variables[48],variables[51:55],variables[59:62])
raw_stats<-c(variables[10],variables[14],variables[17],variables[20],variables[23:27],variables[31:34])
names(nba)

interactions_test = regsubsets( nba$Votes ~ ((W_PCT + FGM*FG_PCT + FG3M*FG3_PCT + FT_PCT*FTM + REB + AST + TOV + STL + BLK + PTS + PLUS_MINUS + DD2 + TD3) * W + Won), data = nba, nvmax = 30)
inter_summary = summary( interactions_test )
min_interactions <- which.min(inter_summary$cp)
inter_summary$rsq
inter_summary$cp

which.min(inter_summary$cp)
max_r=which.max(inter_summary$adjr2)

plot(inter_summary$cp,type = "l", xlab="p", ylab = "Cp")
min_p = which.min( inter_summary$cp )
max_r=which.max(inter_summary$rsq)
points( min_p, inter_summary$cp[ min_p ], col="red", cex=2, pch=20 )
points( max_r, inter_summary$cp[max_r],col="blue",cex=2, pch=20 )
title("Cp vs Parameters ")
legend(599,legend=c("Cp Minimizing Point", "Rsq Maximizing Point"),
       col=c("red", "blue"),pch=20)

coef(interactions_test,min_p)
coef(interactions_test,max_r)
max_r
#It is clear that 29 is a much better model


plot(inter_summary$adjr2,type = "l", xlab="p", ylab = "Cp")
max_r=which.max(inter_summary$rsq)
title("R2 vs Parameters ")

coef(interactions_test,min_p)
coef(interactions_test,max_r)

plot(inter_summary)

