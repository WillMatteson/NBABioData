rm(list = setdiff(ls(), lsf.str()))
library(readxl)
nba<- read_excel("~/Basketball/bballdata/Code/nbaMaster.xlsx", range="A1:BP1480", col_names = TRUE,na="")
#nbanew <- read_excel("~/Basketball/bballdata/Code/BBallR/nbaMasterWorking.xlsx", range="A1:BO00", col_names = TRUE,na="")
#dim(nbanew)
#clear
#attach(nbanew)
dim(nba)
attach(nba)
summary(nba)
Year=as.factor(Year)
Won=as.factor(Won)
MVP=as.factor(MVP)
PTS_RANK=as.factor(MVP)
#relative<-apply(nba[,38:62])


variables<-colnames(nba)
rank_stats<-c(variables[38],variables[42],variables[45],variables[48],variables[51:55],variables[59:62])
raw_stats<-c(variables[10],variables[14],variables[17],variables[20],variables[23:27],variables[31:34])
names(nba)
mvp <- glm(MVP ~ ., data=nba[,rank], family=binomial)
mvp
dim(W_PCT_RANK)
mvp
mvp=lm(Votes ~ ., data=nba[,relevant])

#mvpPoly = lm(Votes ~ ., poly(nba[,relevant],3,raw=TRUE))
mvpPoly = lm(Votes ~ poly(W_PCT_RANK,FGM_RANK, degree=2, raw=TRUE))
mvPPG=lm(Votes ~ poly(PTS_RANK),2)
#plot(Votes,PTS_RANK)
#lines(predict(lm(Votes~factor+I(factor^2))))
library(ggplot2)
qplot(PTS,Votes)+stat_smooth(method="lm", formula="y~poly(x,3)", se=FALSE)
mvp <- lm(Votes ~ ., data=nba[, raw_stats]) 
mvp
mvp.log <- glm(MVP ~ ., data=nba[,raw_stats], family=binomial)
mvp.rank <- lm(Votes ~ ., data=nba[, rank_stats])
summary(mvp)
summary(mvp.log)
summary(mvp.rank)
set.seed(1)

library(boot)
LR2 = glm( MVP ~ poly( PTS, 2),family = binomial )
LOOCV2 = cv.glm( nba, LR2 )
LOOCV2$delta

LR3 = glm( MVP ~ poly( PTS, 3),family = binomial )
LOOCV3 = cv.glm( nba, LR3 )
LOOCV3$delta
install.packages("leaps")
library( leaps )
SS.raw = regsubsets( Votes ~ . , data = nba[,raw_stats], nvmax = 20 )
sum.raw=summary(SS.raw)
sum.raw

SS.rank = regsubsets( Votes ~ . , data = nba[,rank_stats] )
sum.rank=summary(SS.rank)

sum.rank$cp
which.min(sum.raw$cp)
which.min(sum.rank$cp)
min_p.raw=which.min(sum.raw$cp)
min_p.rank=which.min(sum.rank$cp)

coef(SS.raw,min_p.raw)
coef(SS.rank,min_p.rank)
