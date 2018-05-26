### working diresctory 설정 / 패키지 실행
  setwd("C:/Users/Minjae/Downloads")
  library(ggplot2)
  library(gridExtra)
  library(ggrepel)
  library(dplyr)
##  library(wordcloud)



################################################################

### 텍스트파일 불러오가 (Encoding : ANSI)
  Text<-readLines("KakaoTalkChats(11).txt")


### 분석 시작할 날짜
  t<-grep('2018년 1월 1일', Text)[1]


### 사용자 이름
  name<-'박민재'

################################################################



  ### Encoding (UTF-8 를 자동으로 읽음)
  #	Text<-iconv(Text, "UTF-8", localeToCharset())
  ### 이렇게 인코딩을 할시에는 못읽는 문자가 있는 열은 모두 NA로 처리가 됨...
  
  
  ### 시작점부터 자르기
  Chat<-Text[t:length(Text)]
  Chat<-Chat[!Chat=='']
  Chat<-strsplit(Chat, " ")
  
  
  
  ### 엔터 없애기 (엔터는 바로 전 줄로 )
  for(i in length(Chat):1){
    if(Chat[[i]][1]!='2016년' & Chat[[i]][1]!='2017년' & Chat[[i]][1]!='2018년'){
      Chat[[i-1]]<-c(Chat[[i-1]], '\\n', Chat[[i]])
      Chat[[i]]<-NULL
    }
  }
  for(i in length(Chat):1){
    if(Chat[[i]][4]!='오전' & Chat[[i]][4]!='오후'){
      Chat[[i-1]]<-c(Chat[[i-1]], '\n', Chat[[i]])
      Chat[[i]]<-NULL
    }
  }
  
  
  ### 날짜, 시간, 이름 추출
  print0=function(x, i){
    return(x[i])
  }
  Dat<-data.frame(Year=1:length(Chat))
  Dat$Year<-as.character(lapply(Chat, print0, i=1))
  Dat$Month<-as.character(lapply(Chat, print0, i=2))
  Dat$Day<-as.character(lapply(Chat, print0, i=3))
  Dat$AMPM<-as.character(lapply(Chat, print0, i=4))
  Dat$Time<-as.character(lapply(Chat, print0, i=5))
  Dat$Names<-as.character(lapply(Chat, print0, i=6))
  Dat$Time<-gsub(",", "", Dat$Time)
  Dat$Chat<-0
  for(i in 1:length(Chat)){
    Dat$Chat[i]<-paste(Chat[[i]][-(1:7)], collapse = ' ')
  }
  Dat<-Dat[!is.na(Dat$Names),]

  Names<-names(table(Dat$Names))
  Names[grep('님이', Names)]<-NA
  Names<-Names[!is.na(Names)]



### 개인별로 말풍선만 추출
  User<-list()
  for(i in 1:length(Names)){
    User[[i]]<-unlist(strsplit(Dat$Chat[Dat$Names==Names[i]], ' '))
  }



### 필요없는 단어 제거
  for(i in 1:length(User)){
    User[[i]][User[[i]]=='<사진>']<-''
    User[[i]][User[[i]]=='(이모티콘)']<-''
    User[[i]][User[[i]]=='[공지]']<-''
    User[[i]][User[[i]]=='<음성메시지>']<-''
    User[[i]]<-gsub("ㅋ", "", User[[i]])
    User[[i]]<-gsub("\\.", "", User[[i]])
    User[[i]]<-gsub("\\~", "", User[[i]])
    User[[i]]<-gsub("\\;", "", User[[i]])
    User[[i]]<-gsub("\\:", "", User[[i]])
    User[[i]]<-gsub("\\?", "", User[[i]])
    User[[i]]<-gsub("\\!", "", User[[i]])
    User[[i]]<-gsub("\\+", "", User[[i]])
    User[[i]]<-gsub("\\(답장)", "", User[[i]])
    User[[i]]<-gsub("\\=", "", User[[i]])
    User[[i]]<-gsub("\\\\n", "", User[[i]])
    User[[i]][grep("\\(", User[[i]])]<-''
    User[[i]][grep("\\/", User[[i]])]<-''
  }



### 출력하고 싶은 단어 개수
  N<-30

  Words_tab<-list()
  for(i in 1:length(User)){
    Words_tab[[i]]<-sort(table(User[[i]]), decreasing=T)[2:(N+1)]
  }

  Count<-NULL
  for(i in 1:length(User)) Count<-c(Count, as.numeric(Words_tab[[i]]))
  Words_chat<-NULL
  for(i in 1:length(User)) Words_chat<-c(Words_chat, names(Words_tab[[i]]))

  Words_dat<-expand.grid(Chat=1:N, User=Names)
  Words_dat$Count<-Count
  Words_dat$Words<-Words_chat



### 출력
  ggplot(Words_dat[Words_dat$Count>1,], aes(1, -Chat))+
  geom_text_repel(aes(color=User, label=Words, size=Count), segment.alpha=0, force=5)+
  scale_size(range = c(4, 15), guide = FALSE)+
  scale_y_continuous(breaks = NULL)+
  scale_x_continuous(breaks = NULL)+
  labs(x = '', y = '')+
  facet_grid(.~User)+
  ggtitle('개인별 최빈 단어')+ 
  theme(plot.title=element_text(size=20, face="bold"))




### wordcloud
##  tab_all<-sort(table(unlist(User)), decreasing=T)[-1]
##  col_cloud<-c("#69D295", "#F8DB4A", "#968DFF", "#FFAC8D", "#788D4D", "#267180")
##  wordcloud(names(tab_all[1:250]), scale=c(7, 1), random.order=F, random.color=F, freq=tab_all, rot.per=0, col=col_cloud)	

