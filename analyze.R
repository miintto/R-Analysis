Kakao_Chat=function(name, txt, year=0, month=0, day=0){


	###패키지실행

library(plotrix)



	###텍스트 파일 불러오기

dat<-paste("C://Users/Minjae/Downloads/", txt)
dat<-gsub(" ", "", dat)
Chat<-readLines(dat)



	###PC버전 판독기

if (length(grep("---------------", Chat[4]))==1){PC=T
}else{PC=F}



	###주어진 날짜부터 대화 읽음

start<-gsub("-", " ", paste0(year, "년", "-", month, "월", "-", day, "일"))



	###읽기 시작할 위치 지정

if(PC==T){
	if(year+month+day==0){t<-4
	}else{t<-grep(start, Chat)[1]}
}else{
	if(year+month+day==0){t<-5
	}else{t<-grep(start, Chat)[1]}
}



	###띄어쓰기 분리

Chat2<-strsplit(Chat[t:length(Chat)], " ")



	###이름없는 줄 제거(모바일만)

if(PC==F){Chat3<-Chat2[c(grep("오전", Chat2) ,grep("오후", Chat2))]}




#if(day+year+month==0){
#	year<-gsub("년", "", Chat2[[1]][2])
#	month<-gsub("월", "", Chat2[[1]][3])
#	day<-gsub("일", "", Chat2[[1]][4])
#	date<-as.Date(gsub(" ", "", paste(year, "-", month, "-", day)))
#}


	###이름 추출

if(PC==T){
	Name<-c()
	for(i in 1:length(Chat2)){Name[i]<-Chat2[[i]][1]}
	User<-sort(table(Name[strtrim(Name, 1)=='[']), decreasing=F)
	names(User)<-gsub("\\[", "", names(User))
	names(User)<-gsub("\\]", "", names(User))
}else{
	Name<-c()
	for(i in 1:length(Chat3)){Name[i]<-Chat3[[i]][6]}	###이름만 추출
	Name2<-sort(table(Name))		###크기순 정렬
	Name_rm<-length(grep("님이", names(Name2)))+1
	User<-Name2[Name_rm:length(Name2)]
	names(User)[names(User)=="회원님"]<-name
}



	###시간대별 대화

if(PC==T){
	Time<-c()
	for(i in 1:length(Chat2)){Time[i]<-Chat2[[i]][3]}
	AMPM<-c()
	for(i in 1:length(Chat2)){AMPM[i]<-Chat2[[i]][2]}
	Time<-gsub(":", "", Time)
	Time<-gsub("]", "", Time)
	Time<-as.numeric(Time)
	Time[is.na(Time)]<-0
	AMPM[is.na(AMPM)]<-0
	Time[AMPM=='[오후']<-Time[AMPM=='[오후']+1200
}else{
	Time<-c()
	for(i in 1:length(Chat3)){Time[i]<-Chat3[[i]][5]}
	AMPM<-c()
	for(i in 1:length(Chat3)){AMPM[i]<-Chat3[[i]][4]}
	Time<-gsub(":", "", Time)
	Time<-gsub(",", "", Time)
	Time<-as.numeric(Time)
	Time[AMPM=='오후']<-Time[AMPM=='오후']+1200

}

Time2<-c()
Time2[12]<-sum(Time>=1200 & Time<1300)+sum(Time>=100 & Time<200)
Time2[11]<-sum(Time>=200 & Time<400)
Time2[10]<-sum(Time>=400 & Time<600)
Time2[9]<-sum(Time>=600 & Time<800)
Time2[8]<-sum(Time>=800 & Time<1000)
Time2[7]<-sum(Time>=1000 & Time<1200)
Time2[6]<-sum(Time>=2400)+sum(Time>=1300 & Time<1400)
Time2[5]<-sum(Time>=1400 & Time<1600)
Time2[4]<-sum(Time>=1600 & Time<1800)
Time2[3]<-sum(Time>=1800 & Time<2000)
Time2[2]<-sum(Time>=2000 & Time<2200)
Time2[1]<-sum(Time>=2200 & Time<2400)
names(Time2)<-c(
'22~00시', '20~22시', '18~20시', '16~18시', '14~16시', '12~14시', 
'10~12시', '08~10시', '06~08시', '04~06시', '02~04시', '00~02시')



	###월별 대화량

if(PC==T){

}else{
	Chat_Month<-c()
	for(i in 1:length(Chat3)){Chat_Month[i]<-Chat3[[i]][2]}	###월만 추출
	Chat_Month2<-table(Chat_Month)		###크기순 정렬
	Chat_Month3<-Chat_Month2[grep("월", names(Chat_Month2))]
}



	###제목

sta<-as.Date(gsub(" ", "", paste(year, "-", month, "-", day)))
fin<-strsplit(Chat[2], " ")
fin<-as.Date(gsub(" ", "", paste(gsub("년", "", fin[[1]][4]), "-", gsub("월", "", fin[[1]][5]), "-", gsub("일", "", fin[[1]][6]))))



	###출력

par(mfrow=c(2, 2))
barplot<-barplot(User, names.arg=names(User), horiz=T, col=colorRampPalette(c("#E7EEF3", "#E2E9EE", "#4895CD"))(length(User)), main="잉여력", las=2)
title(Chat[1], outer=T, line=-1.2, cex.main=1.6)
title(paste(sta, '~', fin), outer=T, line=-2.2, cex.main=1)
barlabels(User+max(User)/11, barplot, labels=User, cex=0.7, prop=1, border="white")
pie3D(User, labels=paste(names(User), round(User/sum(User)*100, 1), "%"), theta=pi/3, explode=0.1, col=colorRampPalette(c("#E7EEF3", "#E2E9EE", "#4895CD"))(length(User)), main='점유율', labelcex=0.8)
barplot_Time<-barplot(Time2, names.arg=names(Time2), las=2, main="잉여 크리 타임", horiz=T, col="#A3BEDA")
barlabels(Time2+max(Time2)/11, barplot_Time, labels=Time2, cex=0.7, prop=1, border="white")
barplot_Month<-barplot(Chat_Month3, names.arg=names(Chat_Month3), las=2, main="월별 잉여 도수", horiz=F, col="#A3BEDA")
barlabels(barplot_Month, Chat_Month3+max(Chat_Month3)/18, labels=Chat_Month3, cex=0.7, prop=1, border="white")

}








Kakao_WordCloud2=function(txt, date=0){


	###함수실행

library(wordcloud)


	###텍스트 파일 불러오기

dat<-paste("C://Users/Minjae/Downloads/", txt)
dat<-gsub(" ", "", dat)
Chat<-readLines(dat)


	###주어진 날짜부터 대화 읽음

if(date==0){
	t<-4
}else{
	year2<-strtrim(date, 4)
	month2<-ifelse(substr(date, 6, 7)<10, substr(date, 7, 7), substr(date, 6, 7))
	day2<-ifelse(substr(date, 9, 10)<10, substr(date, 10, 10), substr(date, 9, 10))
	dw<-format(as.Date(date), "%A")
	init<-gsub(" ", "", paste(year2, "년", "-", month2, "월", "-", day2, "일", "-", dw))
	init<-gsub("-", " ", init)
	t<-which(Chat==paste('---------------',init,'---------------'))
}

Chat<-Chat[-grep("---------------", Chat)]
Chat2<-strsplit(Chat[t:length(Chat)], " ")	

Chat2<-list()
for(i in 1:length(Chat2)){Chat2[[i]]<-Chat2[[i]]}
Chat2<-unlist(Chat2)
Chat2[Chat2=='사진']<-''
Chat2[Chat2=='---------------']<-''
Chat2[Chat2=='photo']<-''
Chat2[Chat2=='음성메시지']<-''
Chat2[Chat2=='2017년']<-''
Chat2[Chat2=='??']<-''
Chat2[grep(":", Chat2)]<-''
Chat2[grep("\\[", Chat2)]<-''
Chat2[grep("\\]", Chat2)]<-''
Chat2[grep("\\(", Chat2)]<-''
Chat2[grep("\\)", Chat2)]<-''
Chat2<-gsub("ㅋ", "", Chat2)
word<-sort(table(Chat2[Chat2!=""]), decreasing=TRUE)


	#색상 : 민트, 옐로우, 보라, 살구색, 올리브, 피콕블루
col_cloud<-c("#69D295", "#F8DB4A", "#968DFF", "#FFAC8D", "#788D4D", "#267180")
wordcloud(names(word[1:300]), scale=c(7, 0.5), random.order=F, random.color=F, freq=word, rot.per=0, col=col_cloud)	
}










Kakao_WordCloud=function(txt, year=0, month=0, day=0){


	###텍스트 파일 불러오기

dat<-paste("C://Users/Minjae/Downloads/", txt)
dat<-gsub(" ", "", dat)
Chat<-readLines(dat)



	###PC버전 판독기

if (length(grep("---------------", Chat[4]))==1){PC=T
}else{PC=F}



	###주어진 날짜부터 대화 읽음

start<-gsub("-", " ", gsub(" ", "", paste(year, "년", "-", month, "월", "-", day, "일")))



	###읽기 시작할 위치 지정

if(PC==T){
	if(year+month+day==0){t<-4
	}else{t<-grep(start, Chat)[1]}
}else{
	if(year+month+day==0){t<-5
	}else{t<-grep(start, Chat)[1]}
}



	###띄어쓰기 분리

Chat2<-strsplit(Chat[t:length(Chat)], " ")



Chat3<-list()
for(i in 1:length(Chat2)){
Chat3[[i]]<-Chat2[[i]][-c(1, 2, 3, 4, 5, 6, 7)]
}
Chat3<-unlist(Chat3)


Chat3[Chat3=='<사진>']<-''
Chat3[Chat3=='<음성메시지>']<-''
Chat3[grep("\\(", Chat3)]<-''
Chat3[grep("\\[", Chat3)]<-''
Chat3<-gsub("ㅋ", "", Chat3)


word<-sort(table(Chat3[Chat3!='']), decreasing=TRUE)
word[1:30]


	#색상 : 민트, 옐로우, 보라, 살구색, 올리브, 피콕블루
col_cloud<-c("#69D295", "#F8DB4A", "#968DFF", "#FFAC8D", "#788D4D", "#267180")
wordcloud(names(word[1:300]), scale=c(7, 0.5), random.order=F, random.color=F, freq=word, rot.per=0, col=col_cloud)	


}







Kakao_Indiv=function(txt, year=0, month=0, day=0, name_ind){

library(RColorBrewer)
library(wordcloud)
library(plotrix)



if(name_ind==name){name_ind<-"회원님"}



	###텍스트 파일 불러오기

dat<-paste("C://Users/Minjae/Downloads/", txt)
dat<-gsub(" ", "", dat)
Chat<-readLines(dat)



	###PC버전 판독기

if (length(grep("---------------", Chat[4]))==1){PC=T
}else{PC=F}



	###주어진 날짜부터 대화 읽음

start<-gsub("-", " ", gsub(" ", "", paste(year, "년", "-", month, "월", "-", day, "일")))



	###읽기 시작할 위치 지정

if(PC==T){
	if(year+month+day==0){t<-4
	}else{t<-grep(start, Chat)[1]}
}else{
	if(year+month+day==0){t<-5
	}else{t<-grep(start, Chat)[1]}
}



	###띄어쓰기 분리

Chat2<-strsplit(Chat[t:length(Chat)], " ")
Chat2[1:10]




	###사용자 추출

Name<-c()
for(i in 1:length(Chat2)){
Name[i]<-Chat2[[i]][6]
}

Chat3<-ifelse(Name==name_ind, Chat2, NA)
Chat4<-Chat3[!is.na(Chat3)]



	###단어 빈도수 

Chat5<-list()
for(i in 1:length(Chat4)){
Chat5[[i]]<-Chat4[[i]][-c(1, 2, 3, 4, 5, 6, 7)]
}

Chat5<-unlist(Chat5)
Chat4[1:100]

Chat5[Chat5=='<사진>']<-''
Chat5[Chat5=='<음성메시지>']<-''
Chat5[grep("\\(", Chat5)]<-''
Chat5[grep("\\[", Chat5)]<-''
Chat5[grep("/", Chat5)]<-''
Chat5<-gsub("ㅋ", "", Chat5)

table(Chat5)

word<-sort(table(Chat5[Chat5!='']), decreasing=TRUE)



	###시간대별 대화

if(PC==T){
	Time<-c()
	for(i in 1:length(Chat2)){Time[i]<-Chat2[[i]][3]}
	AMPM<-c()
	for(i in 1:length(Chat2)){AMPM[i]<-Chat2[[i]][2]}
	Time<-gsub(":", "", Time)
	Time<-gsub("]", "", Time)
	Time<-as.numeric(Time)
	Time[is.na(Time)]<-0
	AMPM[is.na(AMPM)]<-0
	Time[AMPM=='[오후']<-Time[AMPM=='[오후']+1200
}else{
	Time<-c()
	for(i in 1:length(Chat4)){Time[i]<-Chat4[[i]][5]}
	AMPM<-c()
	for(i in 1:length(Chat4)){AMPM[i]<-Chat4[[i]][4]}
	Time<-gsub(":", "", Time)
	Time<-gsub(",", "", Time)
	Time<-as.numeric(Time)
	Time[AMPM=='오후']<-Time[AMPM=='오후']+1200

}

Time2<-c()
Time2[12]<-sum(Time>=1200 & Time<1300)+sum(Time>=100 & Time<200)
Time2[11]<-sum(Time>=200 & Time<400)
Time2[10]<-sum(Time>=400 & Time<600)
Time2[9]<-sum(Time>=600 & Time<800)
Time2[8]<-sum(Time>=800 & Time<1000)
Time2[7]<-sum(Time>=1000 & Time<1200)
Time2[6]<-sum(Time>=2400)+sum(Time>=1300 & Time<1400)
Time2[5]<-sum(Time>=1400 & Time<1600)
Time2[4]<-sum(Time>=1600 & Time<1800)
Time2[3]<-sum(Time>=1800 & Time<2000)
Time2[2]<-sum(Time>=2000 & Time<2200)
Time2[1]<-sum(Time>=2200 & Time<2400)
names(Time2)<-c(
'22~00시', '20~22시', '18~20시', '16~18시', '14~16시', '12~14시', 
'10~12시', '08~10시', '06~08시', '04~06시', '02~04시', '00~02시')



	###월별 대화량

if(PC==T){

}else{
	Chat_Month<-c()
	for(i in 1:length(Chat4)){Chat_Month[i]<-Chat4[[i]][2]}	###월만 추출
	Chat_Month2<-table(Chat_Month)		###크기순 정렬
	Chat_Month3<-Chat_Month2[grep("월", names(Chat_Month2))]
}



	###제목

sta<-as.Date(gsub(" ", "", paste(year, "-", month, "-", day)))
fin<-strsplit(Chat[2], " ")
fin<-as.Date(gsub(" ", "", paste(gsub("년", "", fin[[1]][4]), "-", gsub("월", "", fin[[1]][5]), "-", gsub("일", "", fin[[1]][6]))))

if(name_ind=="회원님"){name_ind<-name}



	###출력

par(mfrow=c(2, 2))
barplot_word<-barplot(sort(word[1:12]), names.arg=names(sort(word[1:12])), las=2, main="사용 단어 빈도", horiz=T, col="#A3BEDA")
title(paste("[", name_ind, "] 사용자 분석"), outer=T, line=-1.2, cex.main=1.6)
title(paste(sta, '~', fin), outer=T, line=-2.2, cex.main=1)
barlabels(sort(word[1:12])+max(word)/15, barplot_word, labels=sort(word[1:12]), cex=0.7, prop=1, border="white")
col_cloud<-c("#69D295", "#F8DB4A", "#968DFF", "#FFAC8D", "#267180")
wordcloud(names(word[1:150]), scale=c(5, 0.5), random.order=F, random.color=F, freq=word, rot.per=0, col=col_cloud)	
barplot_Time<-barplot(Time2, names.arg=names(Time2), las=2, main="시간별 대화량", horiz=T, col="#A3BEDA")
barlabels(Time2+max(Time2)/11, barplot_Time, labels=Time2, cex=0.7, prop=1, border="white")
barplot_Month<-barplot(Chat_Month3, names.arg=names(Chat_Month3), las=2, main="월별 대화량", horiz=F, col="#A3BEDA")
barlabels(barplot_Month, Chat_Month3+max(Chat_Month3)/18, labels=Chat_Month3, cex=0.7, prop=1, border="white")

}
