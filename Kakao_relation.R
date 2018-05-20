### working diresctory ���� / ��Ű�� ����
  setwd("C:/Users/Minjae/Downloads")
  library(ggplot2)



################################################################

### �ؽ�Ʈ���� �ҷ����� (Encoding : ANSI)
  Text<-readLines("KakaoTalkChats(12).txt")


### �м� ������ ��¥
  t<-grep('2018�� 1�� 1��', Text)[1]


### ����� �̸�
  name<-'�ڹ���'

################################################################



### Encoding (UTF-8 �� �ڵ����� ����)
#	Text<-iconv(Text, "UTF-8", localeToCharset())
### �̷��� ���ڵ��� �ҽÿ��� ���д� ���ڰ� �ִ� ���� ��� NA�� ó���� ��...


### ���������� �ڸ���
  Chat<-Text[t:length(Text)]
  Chat<-Chat[!Chat=='']
  Chat<-strsplit(Chat, " ")



### ���� ���ֱ� (���ʹ� �ٷ� �� �ٷ� )
  for(i in length(Chat):1){
    if(Chat[[i]][1]!='2016��' & Chat[[i]][1]!='2017��' & Chat[[i]][1]!='2018��'){
      Chat[[i-1]]<-c(Chat[[i-1]], '\\n', Chat[[i]])
      Chat[[i]]<-NULL
    }
  }
  for(i in length(Chat):1){
    if(Chat[[i]][4]!='����' & Chat[[i]][4]!='����'){
      Chat[[i-1]]<-c(Chat[[i-1]], '\n', Chat[[i]])
      Chat[[i]]<-NULL
    }
  }


### ��¥, �ð�, �̸� ����
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
  Names[grep('����', Names)]<-NA
  Names<-Names[!is.na(Names)]





### �м� ����

  Dat$Time_2<-gsub(':', "", Dat$Time)
  Dat$Time_2<-as.integer(Dat$Time_2)
  Dat$Time_2<-ifelse(Dat$AMPM=='����', Dat$Time_2+1200, Dat$Time_2)
  Dat$Time_2<-ifelse((Dat$AMPM=='����' & Dat$Time_2>1200), Dat$Time_2-1200, Dat$Time_2)
  Dat<-Dat[!is.na(Dat$Time_2),]



  n=0
  Dat$Group<-0
  for(i in 2:dim(Dat)[1]){
    if((Dat$Time_2[i]-Dat$Time_2[i-1] <= 30) & (Dat$Time_2[i]-Dat$Time_2[i-1] >= 0)){
      Dat$Group[i]<-n
    }else{
      Dat$Group[i]<-n+1
      n<-n+1
    }
  }


  dat_chat<-table(Dat$Group, Dat$Names)
  if(length(grep('����', colnames(dat_chat)))>0){
      dat_chat<-dat_chat[,-grep('����', colnames(dat_chat))]
  }
  dat_chat_2<-expand.grid(names_1=Names, names_2=Names, Val=0)
  dat_chat_2$Val<-round(expand.grid(cor(dat_chat))*100)
  dat_chat_2$Val[dat_chat_2$Val==100]<-0

  ggplot(aes(x=names_1, y=names_2, fill=Val), data=dat_chat_2)+
    geom_tile(color = 'white')+
    scale_fill_gradient2(low="white", high="steelblue")+
    geom_text(aes(names_1, names_2, label=Val), size=5)