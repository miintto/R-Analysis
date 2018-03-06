### working diresctory 설정 / 패키지 실행
setwd("C:/Users/Minjae/Downloads")
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(wordcloud)
library(dplyr)



################################################################

### 텍스트파일 불러오기
Text<-readLines("KakaoTalk_20180205_1755_19_341_group.txt")


### 분석 시작할 날짜
t<-grep('2018년 2월 4일', Text)[1]
t<-4

################################################################


###
Chat<-Text[t:length(Text)]

###
Chat<-strsplit(Chat, " ")


print0=function(x, i){
  return(x[i])
}

Dat<-data.frame(name=1:length(Chat))
Dat$name<-as.character(lapply(Chat, print0, i=1))
Chat<-ifelse(is.na(Dat$name), '', Chat)

for(i in length(Chat):2){
  if(substr(Chat[[i]][1], 1, 1)!='[' & Chat[[i]][1]!='---------------'){
    Chat[[i]]<-c(Chat[[i-1]], Chat[[i]])
    Chat[[i]]<-''
  }
}

Dat$name<-as.character(lapply(Chat, print0, i=1))
Dat$AMPM<-as.character(lapply(Chat, print0, i=2))
Dat$time<-as.character(lapply(Chat, print0, i=3))
Dat$date<-as.character(lapply(Chat, print0, i=4))
which_date<-c(which(Dat$name=='---------------'), length(Chat)+1)
for(i in 1:(length(which_date)-1)){
  Dat$date[(which_date[i]+1):(which_date[i+1]-1)]<-paste(Dat[which_date[i], 2], Dat[which_date[i], 3], Dat[which_date[i], 4])

}
i=2
Dat<-Dat[-which_date,]
Dat<-na.omit(Dat)

Dat$time<-gsub('\\]', '', Dat$time)
str(Dat)

###
###dlfma
Name<-names(table(Dat$name))[-c(1, 2)]



i=2
Dat$AMPM<-lapply(Chat, print0, i=2)
Dat$time<-lapply(Chat, print0, i=3)













Chat[grep('---------------', Chat)]<-''
for(i in length(Chat):2){
  if(substr(Chat[i], 1, 1)!='[') {
    Chat[i-1]<-paste(Chat[i-1], Chat[i])
    Chat[i]<-NA
  }
}

Chat[grep('섹', Chat)]

a<-grep('ㅔ', Chat)
b<-grep('ㅅ', Chat)
tab<-table(c(a, b))
Chat[as.numeric(names(tab)[tab>1])]




#