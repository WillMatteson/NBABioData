#Initial messing around


rm(list = setdiff(ls(), lsf.str()))
library(readxl)
nba<- read_excel("nbaMaster.xlsx", range="A1:BP1480", col_names = TRUE,na="")

dim(nba)
attach(nba)
summary(nba)
Year=as.factor(Year)
Won=as.factor(Won)
MVP=as.factor(MVP)


variables<-colnames(nba)
rank_stats<-c(variables[38],variables[42],variables[45],variables[48],variables[51:55],variables[59:62])
rank_stats
raw_stats
raw_stats<-c(variables[10],variables[14],variables[17],variables[20],variables[23:27],variables[31:34])
names(nba)

mvp_rank <- lm(Votes ~ ., data=nba[, rank_stats]) 
mvp_rank
plot(mvp_rank)

mvp_raw <- lm(Votes ~ ., data=nba[, raw_stats]) 
mvp_raw
plot(mvp_raw)

