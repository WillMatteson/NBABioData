library(pls)
set.seed(2)

#Poisson Regression
rm(list = setdiff(ls(), lsf.str()))
library(pls)
set.seed(2)

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
PCR = pcr( Votes ~ ((W_PCT + FGM*FG_PCT + FG3M*FG3_PCT + FT_PCT*FTM + REB + AST + TOV + STL + BLK + PTS + PLUS_MINUS + DD2 + TD3) * W + Won), data=nba, scale=TRUE, validation="CV")
PCR


pcr_pred <- predict(pcr_model, test, ncomp = 30)
mean((pcr_pred - y_test)^2)


data17<- read_excel("~/Basketball/bballdata/Code/NewData.xlsx",col_names = TRUE,na="",range="A1:BP61")

predict(PCR,data17,ncomp=40)
data17[379,]

summary(m1 <- glm(Votes ~ ((W_PCT + FGM*FG_PCT + FG3M*FG3_PCT + FT_PCT*FTM + REB + AST + TOV + STL + BLK + PTS + PLUS_MINUS + DD2 + TD3 + Won) * W), family="poisson", data=nba))


#New MVP
new_mvp=which.max(predict(m1,data17,type="response"))
data17[new_mvp,]

z=predict(m1,data17,type="response")
a=sort(z)
length(a)
#Best player of the past years (as measured by votes)
old_max=which.max(predict(m1,nba,type="response"))
nba[old_max]

#Actual highest voted player
old_actual=max(unlist(nba[,65]))
nba[old_actual]


