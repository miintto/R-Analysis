### working diresctory 설정 / 패키지 실행
	setwd("C:/Users/Minjae/Downloads")
	library(ggplot2)
	library(gridExtra)
	library(ggrepel)
	library(dplyr)



################################################################

### 텍스트파일 불러오기 (Encoding : ANSI)
	Text<-readLines("KakaoTalkChats(12).txt")


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
	

### 매달 간격 추출 (대신 최대 3년... 추후 개선예정)
	mon<-0
	for(i in 1:12){
	  mon[i]<-which(Dat$Year=='2016년'& Dat$Month==paste0(i, '월'))[1]
	}
	for(i in 1:12){
	  mon[i+12]<-which(Dat$Year=='2017년'& Dat$Month==paste0(i, '월'))[1]
	}
	for(i in 1:12){
	  mon[i+24]<-which(Dat$Year=='2018년'& Dat$Month==paste0(i, '월'))[1]
	}
	mon<-mon[1:(which.max(mon)+1)]
	mon[which.max(mon)+1]<-length(Chat)	
	mon[is.na(mon)]<-1

	Dat[1,]
	mon
	i=1
### 매달마다 말풍선 카운트
	Dat_2<-data.frame(name=Names)
	for(i in 1:(length(mon)-1)){
	  tab<-table(Dat$Name[mon[i]:mon[i+1]])
	  tab<-data.frame(name=names(tab), chat=as.numeric(tab))
	  Dat_2<-merge(Dat_2, tab, by='name', all.x=T)
	}
	Dat_2[is.na(Dat_2)]<-0
	month<-c('1월', '2월', '3월', '4월', '5월', '6월', '7월', '8월', '9월', '10월', '11월', '12월')
	names(Dat_2)[-1]<-c(month, month, month)
	Dat_2<-Dat_2[,c(T, apply(Dat_2[-1], 2, sum)>1)]
	Dat_2$name<-as.character(Dat_2$name)
	Dat_2$name[Dat_2$name=='회원님']<-name
	
### 시간대별 카운트
	Dat$Hour<-as.numeric(gsub('\\:', '', Dat$Time))/100
	Dat$Hour<-floor(Dat$Hour)
	Dat$Hour<-ifelse(Dat$AMPM=='오후', Dat$Hour+12, Dat$Hour)
	Dat$Hour[Dat$Hour==12]=0
	Dat$Hour[Dat$Hour==24]=12

### 요일별 카운트
	Dat$Date<-paste0(Dat$Year, Dat$Month, Dat$Day)
	Dat$Date<-gsub('년', '-',Dat$Date)
	Dat$Date<-gsub('월', '-',Dat$Date)
	Dat$Date<-gsub('일', '',Dat$Date)
	Dat$Date<-as.Date(Dat$Date)
	Dat$Weekday<-weekdays(Dat$Date, abbreviate=T)
	Dat$Weekday<-factor(Dat$Weekday, levels=c("일", "월", "화", "수", "목", "금", "토"))



### Dataframe으로 가공
	Dat_chat<-expand.grid(Name=Dat_2$name, Month=colnames(Dat_2)[-1])
	Dat_chat$Chat<-as.vector(as.matrix(Dat_2[-1]))
	Dat_pie<-arrange(data.frame(Name=Dat_2$name, Chat=apply(Dat_2[-1], 1, sum)), Chat)
	Dat_pie$Percent<-round(Dat_pie$Chat/sum(Dat_pie$Chat)*100, digits=1)
	Dat_pie$mid<-cumsum(Dat_pie$Percent)-Dat_pie$Percent/2
	Dat_Freq<-table(Dat$Hour, Dat$Weekday)
	Dat_Freq<-data.frame(Dat_Freq)


### 차트 생성
	chart1<-ggplot(Dat_chat, aes(Month, reorder(Name,Chat), fill=Chat))+
	geom_tile(col='white')+
	scale_fill_gradient2(low="white", high="steelblue")+
	geom_text(aes(Month, Name, label=Chat), size=3)+
	ggtitle('월별 대화량')+
	labs(x='기간', y='사용자')+
	theme(plot.title=element_text(size=15, face="bold"))

	chart2<-ggplot(Dat_pie, aes(factor(0), Percent, fill=reorder(Name, -Percent)))+
	geom_bar(stat='identity')+
	scale_fill_brewer(palette='Set3')+
	guides(fill=guide_legend(title=NULL))+
	geom_text_repel(aes(x=1, y=mid, label=Percent), segment.size=0, size=3, force=0.002)+
	coord_polar(theta='y')+
	labs(x = '', y = '')+
	ggtitle('톡방 점유율')+ 
	theme(plot.title=element_text(size=15, face="bold"))

	chart3<- ggplot(Dat_Freq)+
	geom_tile(aes(Var1, Var2, fill=Freq), col='white')+
	scale_fill_gradient(low = "white", high = "steelblue")+
	geom_text(aes(Var1, Var2, label=Freq), size=3)+
	ggtitle('시간*요일별 대화량')+
	labs(x='시간', y='요일')+
	theme(plot.title=element_text(size=15, face="bold"))

	chart4<-ggplot(Dat_chat, aes(Month, Chat, fill=reorder(Name, -Chat)))+
	geom_bar(stat='identity', position='fill')+
	scale_fill_brewer(palette='Set3')+
	guides(fill=guide_legend(title=NULL))+
	ggtitle('월별 점유율')+
	labs(x='기간', y='점유율')+
	theme(plot.title=element_text(size=15, face="bold"))

	range<-paste('분석 기간 :', Dat[1,8], '~', na.omit(Dat$Date)[length(na.omit(Dat$Date))])

### 차트 출력
	grid.arrange(chart1, chart2, chart3, chart4, nrow=2, ncol=2, bottom=range)

