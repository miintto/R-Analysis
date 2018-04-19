### working diresctory ���� / ��Ű�� ����
	setwd("C:/Users/Minjae/Downloads")
	library(ggplot2)
	library(gridExtra)
	library(ggrepel)
	library(dplyr)



################################################################

### �ؽ�Ʈ���� �ҷ����� (Encoding : ANSI)
	Text<-readLines("KakaoTalkChats(10).txt")


### �м� ������ ��¥
	t<-grep('2016�� 4�� 9��', Text)[1]
	
	
### ����� �̸�
	name<-'�ڹ���'

################################################################



### Encoding (UTF-8 �� �ڵ����� ����)
#	Text<-iconv(Text, "UTF-8", localeToCharset())
### �̷��� ���ڵ��� �ҽÿ��� ���д� ���ڰ� �ִ� ���� ��� NA�� ó���� ��...
	

### ���������� �ڸ���
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

### �Ŵ� ���� ���� (��� �ִ� 3��... ���� ��������)
	mon<-0
	for(i in 1:12){
	  mon[i]<-which(Dat$year=='2016��'& Dat$month==paste0(i, '��'))[1]
	}
	for(i in 1:12){
	  mon[i+12]<-which(Dat$year=='2017��'& Dat$month==paste0(i, '��'))[1]
	}
	for(i in 1:12){
	  mon[i+24]<-which(Dat$year=='2018��'& Dat$month==paste0(i, '��'))[1]
	}
	mon<-mon[1:(which.max(mon)+1)]
	mon[which.max(mon)+1]<-length(Chat)	
	mon[is.na(mon)]<-1

### �Ŵ޸��� ��ǳ�� ī��Ʈ
	Dat_2<-data.frame(name=Names)
	for(i in 1:(length(mon)-1)){
	  tab<-table(Dat$name[mon[i]:mon[i+1]])
	  tab<-data.frame(name=names(tab), chat=as.numeric(tab))
	  Dat_2<-merge(Dat_2, tab, by='name', all.x=T)
	}
	Dat_2[is.na(Dat_2)]<-0
	month<-c('1��', '2��', '3��', '4��', '5��', '6��', '7��', '8��', '9��', '10��', '11��', '12��')
	names(Dat_2)[-1]<-c(month, month, month)
	Dat_2<-Dat_2[,c(T, apply(Dat_2[-1], 2, sum)>0)]
	Dat_2$name<-as.character(Dat_2$name)
	Dat_2$name[Dat_2$name=='ȸ����']<-name
	
### �ð��뺰 ī��Ʈ
	Dat$hour<-as.numeric(gsub('\\:', '', Dat$time))/100
	Dat$hour<-floor(Dat$hour)
	Dat$hour<-ifelse(Dat$AMPM=='����', Dat$hour+12, Dat$hour)
	Dat$hour[Dat$hour==12]=0
	Dat$hour[Dat$hour==24]=12

### ���Ϻ� ī��Ʈ
	Dat$date<-paste0(Dat$year, Dat$month, Dat$day)
	Dat$date<-gsub('��', '-',Dat$date)
	Dat$date<-gsub('��', '-',Dat$date)
	Dat$date<-gsub('��', '',Dat$date)
	Dat$date<-as.Date(Dat$date)
	Dat$weekday<-weekdays(Dat$date, abbreviate=T)
	Dat$weekday<-factor(Dat$weekday, levels=c("��", "��", "ȭ", "��", "��", "��", "��"))



### Dataframe���� ����
	Dat_chat<-expand.grid(Name=Dat_2$name, Month=colnames(Dat_2)[-1])
	Dat_chat$Chat<-as.vector(as.matrix(Dat_2[-1]))
	Dat_pie<-arrange(data.frame(Name=Dat_2$name, Chat=apply(Dat_2[-1], 1, sum)), Chat)
	Dat_pie$Percent<-round(Dat_pie$Chat/sum(Dat_pie$Chat)*100, digits=1)
	Dat_pie$mid<-cumsum(Dat_pie$Percent)-Dat_pie$Percent/2
	Dat_Freq<-table(Dat$hour, Dat$weekday)
	Dat_Freq<-data.frame(Dat_Freq)


### ��Ʈ ����
	chart1<-ggplot(Dat_chat, aes(Month, reorder(Name,Chat), fill=Chat))+
	geom_tile(col='white')+
	scale_fill_gradient2(low="white", high="steelblue")+
	geom_text(aes(Month, Name, label=Chat), size=3)+
	ggtitle('���� ��ȭ��')+
	labs(x='�Ⱓ', y='�����')+
	theme(plot.title=element_text(size=15, face="bold"))

	chart2<-ggplot(Dat_pie, aes(factor(0), Percent, fill=reorder(Name, -Percent)))+
	geom_bar(stat='identity')+
	scale_fill_brewer(palette='Set3')+
	guides(fill=guide_legend(title=NULL))+
	geom_text_repel(aes(x=1, y=mid, label=Percent), segment.size=0, size=3, force=0.002)+
	coord_polar(theta='y')+
	labs(x = '', y = '')+
	ggtitle('��� ������')+ 
	theme(plot.title=element_text(size=15, face="bold"))

	chart3<- ggplot(Dat_Freq)+
	geom_tile(aes(Var1, Var2, fill=Freq), col='white')+
	scale_fill_gradient(low = "white", high = "steelblue")+
	geom_text(aes(Var1, Var2, label=Freq), size=3)+
	ggtitle('�ð�*���Ϻ� ��ȭ��')+
	labs(x='�ð�', y='����')+
	theme(plot.title=element_text(size=15, face="bold"))

	chart4<-ggplot(Dat_chat, aes(Month, Chat, fill=reorder(Name, -Chat)))+
	geom_bar(stat='identity', position='fill')+
	scale_fill_brewer(palette='Set3')+
	guides(fill=guide_legend(title=NULL))+
	ggtitle('���� ������')+
	labs(x='�Ⱓ', y='������')+
	theme(plot.title=element_text(size=15, face="bold"))

	range<-paste('�м� �Ⱓ :', Dat[1,8], '~', na.omit(Dat$date)[length(na.omit(Dat$date))])

### ��Ʈ ���
	grid.arrange(chart1, chart2, chart3, chart4, nrow=2, ncol=2, bottom=range)
