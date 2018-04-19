### working diresctory ���� / ��Ű�� ����
setwd("C:/Users/Minjae/Downloads")
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(wordcloud)
library(dplyr)



################################################################

### �ؽ�Ʈ���� �ҷ����� (Encoding : ANSI)
Text<-readLines("KakaoTalkChats(10).txt")


### �м� ������ ��¥
t<-grep('2018�� 1�� 1��', Text)[1]


### ����� �̸�
name<-'�ڹ���'

################################################################



### Encoding (UTF-8 �� �ڵ����� ����)
#	Text<-iconv(Text, "UTF-8", localeToCharset())
### �̷��� ���ڵ��� �ҽÿ��� ���д� ���ڰ� �ִ� ���� ��� NA�� ó���� ��...


### ���������� ����
Chat<-Text[t:length(Text)]
Chat<-strsplit(Chat, " ")

### ���� ���ֱ� (����[1] �� '' ����)
for(i in 1:length(Chat)){
  if(is.na(Chat[[i]][1])) Chat[[i]]<-''
}

### ���� ���ֱ� (���ʹ� �ٷ� �� �ٷ� )
for(i in length(Chat):1){
  if(Chat[[i]][1]!='2016��' & Chat[[i]][1]!='2017��' & Chat[[i]][1]!='2018��'){
    Chat[[i-1]]<-c(Chat[[i-1]], Chat[[i]])
    Chat[[i]]<-c('', '', '', '', '', '')
  }
}
for(i in length(Chat):1){
  if(Chat[[i]][4]!='����' & Chat[[i]][4]!='����'){
    Chat[[i-1]]<-c(Chat[[i-1]], Chat[[i]])
    Chat[[i]]<-c('', '', '', '', '', '')
  }
}

### �̸� ��� ä���([6]�� ''����)
for(i in 1:length(Chat)){
  if(is.na(Chat[[i]][6])) Chat[[i]][6]<-''
}

### ��¥, �ð�, �̸� ����
print0=function(x, i){
  return(x[i])
}
Dat<-data.frame(year=1:length(Chat))
Dat$year<-as.character(lapply(Chat, print0, i=1))
Dat$month<-as.character(lapply(Chat, print0, i=2))
Dat$day<-as.character(lapply(Chat, print0, i=3))
Dat$AMPM<-as.character(lapply(Chat, print0, i=4))
Dat$time<-as.character(lapply(Chat, print0, i=5))
Dat$name<-as.character(lapply(Chat, print0, i=6))
Dat$time<-gsub(",", "", Dat$time)
Names<-names(table(Dat$name))[-1]
Names[grep('����', Names)]<-NA
Names<-Names[!is.na(Names)]


### ���κ��� ��ǳ���� ����
User<-list()
for(j in 1:length(Names)){
  User_1<-list()
  for(i in 1:length(Chat)) if(Chat[[i]][6]==Names[j]) User_1[[i]]<-Chat[[i]][-(1:7)]
  User_1<-unlist(User_1)
  User[[j]]<-User_1
}


### �ʿ���� �ܾ� ����
for(i in 1:length(User)){
  User[[i]][User[[i]]=='<����>']<-''
  User[[i]][User[[i]]=='(�̸�Ƽ��)']<-''
  User[[i]][User[[i]]=='[����]']<-''
  User[[i]][User[[i]]=='<�����޽���>']<-''
  User[[i]]<-gsub("��", "", User[[i]])
  User[[i]]<-gsub("\\.", "", User[[i]])
  User[[i]]<-gsub("\\~", "", User[[i]])
  User[[i]]<-gsub("\\;", "", User[[i]])
  User[[i]]<-gsub("\\:", "", User[[i]])
  User[[i]]<-gsub("\\?", "", User[[i]])
  User[[i]]<-gsub("\\!", "", User[[i]])
  User[[i]]<-gsub("\\+", "", User[[i]])
  User[[i]]<-gsub("\\=", "", User[[i]])
  User[[i]][grep("\\(", User[[i]])]<-''
  User[[i]][grep("\\/", User[[i]])]<-''
}


### ����ϰ� ���� �ܾ� ����
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



### ���
ggplot(Words_dat[Words_dat$Count>1,], aes(1, -Chat))+
  geom_text_repel(aes(color=User, label=Words, size=Count), segment.alpha=0, force=5)+
  scale_size(range = c(4, 15), guide = FALSE)+
  scale_y_continuous(breaks = NULL)+
  scale_x_continuous(breaks = NULL)+
  labs(x = '', y = '')+
  facet_grid(.~User)+
  ggtitle('���κ� �ֺ� �ܾ�')+ 
  theme(plot.title=element_text(size=20, face="bold"))




### wordcloud
tab_all<-sort(table(unlist(User)), decreasing=T)[-1]
col_cloud<-c("#69D295", "#F8DB4A", "#968DFF", "#FFAC8D", "#788D4D", "#267180")
wordcloud(names(tab_all[1:250]), scale=c(7, 1), random.order=F, random.color=F, freq=tab_all, rot.per=0, col=col_cloud)	
