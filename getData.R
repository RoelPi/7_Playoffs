d <- data.table(read.csv('data.csv',sep=';'))
d <- d[,c('Year','Type','Team','Pos','Pts'),with=F]

d <- data.table(dcast(d,Year + Team ~ Type,value.var=c('Pos','Pts')))
d <- d[complete.cases(d),]
d <- d[,Diff:=Pos_Regular - Pos_Playoff]
d <- d[,Pts_Playoff_Start := ceiling(Pts_Regular/2)]

d <- d[order(-Year,Pos_Regular)]
d <- d[,.(Team,Pos_Playoff,Pos_Regular,Pts_Playoff,Pts_Playoff_Start,
          Pos_Diff=Pos_Playoff-Pos_Regular,
          is_Champion=ifelse(Pos_Playoff==1,1,0),
          Rise_1=ifelse(Pos_Playoff - Pos_Regular >= 1,1,0),
          Rise_2=ifelse(Pos_Playoff - Pos_Regular >= 2,1,0),
          Rise_3=ifelse(Pos_Playoff - Pos_Regular >= 3,1,0),
          Rise_4=ifelse(Pos_Playoff - Pos_Regular >= 4,1,0),
          Rise_5=ifelse(Pos_Playoff - Pos_Regular >= 5,1,0),
          Follow_Pts=c(NA,diff(Pts_Playoff_Start)),
          Pts_Leader = head(Pts_Playoff_Start,1) - Pts_Playoff_Start,
          Pts_2 = Pts_Playoff_Start[2] - Pts_Playoff_Start,
          Pts_3 = Pts_Playoff_Start[3] - Pts_Playoff_Start,
          Pts_4 = Pts_Playoff_Start[4] - Pts_Playoff_Start,
          Pts_5 = Pts_Playoff_Start[5] - Pts_Playoff_Start,
          Pts_6 = Pts_Playoff_Start[6] - Pts_Playoff_Start),by=.(Year)]
d <- d[,Pts_Total:=Pts_Leader+Pts_2+Pts_3+Pts_4+Pts_5+Pts_6]

d <- d[order(-Year,Pos_Playoff)]
f <- d[,.(Team,Pos_Playoff,Pos_Regular,Pts_Playoff,Pts_Playoff_Start,Pts_Leader,Pts_2,Pts_3,Pts_4,Pts_5,Pts_6,
          PtsPO_Leader = head(Pts_Playoff,1) - Pts_Playoff,
          PtsPO_2 = Pts_Playoff[2] - Pts_Playoff,
          PtsPO_3 = Pts_Playoff[3] - Pts_Playoff,
          PtsPO_4 = Pts_Playoff[4] - Pts_Playoff,
          PtsPO_5 = Pts_Playoff[5] - Pts_Playoff,
          PtsPO_6 = Pts_Playoff[6] - Pts_Playoff),by=.(Year)]

f1 <- f[,c("Team","Year","Pts_Leader","PtsPO_Leader"),with=F]
f1$Pos <- 1
f2 <- f[,c("Team","Year","Pts_2","PtsPO_2"),with=F]
f2$Pos <- 2
f3 <- f[,c("Team","Year","Pts_3","PtsPO_3"),with=F]
f3$Pos <- 3
f4 <- f[,c("Team","Year","Pts_4","PtsPO_4"),with=F]
f4$Pos <- 4
f5 <- f[,c("Team","Year","Pts_5","PtsPO_5"),with=F]
f5$Pos <- 5
f6 <- f[,c("Team","Year","Pts_6","PtsPO_6"),with=F]
f6$Pos <- 6


f <- data.table(rbindlist(list(f1,f2,f3,f4,f5,f6)))
rm(f1,f2,f3,f4,f5,f6)
colnames(f) <- c("Team","Year","Start","End","Pos")
f$Success <- with(f,ifelse((Start>0&End<=0)|(Start<=0&End<0),1,0))
f <- f[Start>0]