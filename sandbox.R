library(data.table)
library(ggplot2)
library(MASS)
source("getData.R")
options(scipen=999)

pal = "Dark2"

ePlot <- ggplot(d,aes(x=-Pos_Diff)) + 
    geom_histogram(stat="bin",binwidth=1,fill="#43a2ca",color="#0868ac") +
    labs(x="Positieverschil",y="Aantal keer") +
    scale_x_continuous(limits=c(-5,5),breaks=(-5:5)) +
    scale_y_continuous(limits=c(0,17),breaks=0:17)
g <- with(d,lm(Pos_Playoff~Pos_Regular))

gPlot <- ggplot(d,aes(x=Pos_Regular,y=Pos_Playoff,label=paste0(Team,"-",Year))) + 
    geom_point() + 
    geom_smooth(method='lm') +
    geom_smooth(method='lm', formula=y~poly(x,3,raw=T)) +
    geom_text(size=2,nudge_y=0.1) +
    xlim(0,7) +
    labs(x="Stand Reguliere Competitie",y="Stand Playoffs",title="Verband eindstand reguliere competitie & playoffs")

k <- with(d,lm(Pos_Playoff~Pts_Leader))
kPlot <- ggplot(d,aes(x=Pts_Leader,y=Pos_Playoff,label=paste0(Team,"-",Year))) + 
    geom_point() + 
    geom_smooth(method='lm') +
    geom_smooth(method='lm', formula=y~poly(x,3,raw=T))

l <- glm(d$Rise_1~d$Follow_Pts,family=binomial(link='logit'))

lPlot <- ggplot(d,aes(x=Pts_Leader,y=is_Champion,label=paste0(Team,"-",Year))) + 
    geom_point() + 
    stat_smooth(method='glm',method.args=list(family="binomial"))

m <- with(d,lm(Pos_Playoff~Pts_Total))
mPlot <- ggplot(d,aes(x=Pts_Total,y=Pos_Playoff,label=paste0(Team,"-",Year))) + 
    geom_point() + 
    geom_smooth(method='lm') +
    geom_smooth(method='lm', formula=y~poly(x,3,raw=T)) +
    geom_text(size=2,nudge_y=0.1) +
    labs(x="Stand Reguliere Competitie",y="Stand Playoffs",title="Verband eindstand reguliere competitie & playoffs")

n <- with(f,glm(Success~Start,family=binomial(link='logit')))

nPlot <- ggplot(f,aes(x=Start,y=Success,label=paste0(Team,"-",Year))) + 
    geom_jitter(height=0.1,width=0.1) + 
    stat_smooth(method='glm',method.args=list(family="binomial")) +
    scale_x_continuous(limits=c(0,15),breaks=(0:15)) +
    scale_y_continuous(limits=c(-0.2,1.2),breaks=c(0,0.2,0.4,0.6,0.8,1))
rankings <- data.table(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
colnames(rankings) <- c("Start")

final <- exp(1)^(predict(n,rankings))/(1+exp(1)^(predict(n,rankings)))
finalUpr <- exp(1)^(predict(n,rankings)+1.96*(predict(n,rankings,type="link",se.fit=TRUE)$se.fit))/(1+exp(1)^(predict(n,rankings)+1.96*(predict(n,rankings,type="link",se.fit=TRUE)$se.fit)))
finalLwr <- exp(1)^(predict(n,rankings)-1.96*(predict(n,rankings,type="link",se.fit=TRUE)$se.fit))/(1+exp(1)^(predict(n,rankings)-1.96*(predict(n,rankings,type="link",se.fit=TRUE)$se.fit)))


anderlecht <- (1-final[2])*(1-final[5])*(1-final[7])^3
brugge <- final[2]*(1-final[4])*(1-final[7])^3
zw <- (1-final[2])*final[4]*(1-final[7])^3
gent <- (1-final[2])*(1-final[4])*(1-final[7])^2*final[7]