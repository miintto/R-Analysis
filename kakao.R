###working diresctory 설정 / 패키지 실행
	setwd("C:/Users/Minjae/Downloads")
	library(ggplot2)
	library(gridExtra)
	library(ggrepel)
	library(wordcloud)
	library(dplyr)



###텍스트파일 불러오기
	dat<-readLines("KakaoTalkChats_2017.txt")

###자를 시작점/자르기
	t<-grep('2017년 1월 1일', dat)[1]
	Chat<-dat[t:length(dat)]

###매달 시작하는 행 추출
	mon_1<-grep('2017년 1월 1일', Chat)[1]		
	mon_2<-grep('2017년 2월 1일', Chat)[1]
	mon_3<-grep('2017년 3월 1일', Chat)[1]
	mon_4<-grep('2017년 4월 1일', Chat)[1]
	mon_5<-grep('2017년 5월 1일', Chat)[1]
	mon_6<-grep('2017년 6월 1일', Chat)[1]
	mon_7<-grep('2017년 7월 1일', Chat)[1]
	mon_8<-grep('2017년 8월 1일', Chat)[1]
	mon_9<-grep('2017년 9월 1일', Chat)[1]
	mon_10<-grep('2017년 10월 1일', Chat)[1]
	mon_11<-grep('2017년 11월 1일', Chat)[1]
	mon_12<-grep('2017년 12월 1일', Chat)[1]
	n<-length(Chat)

###띄어쓰기 단위로 자르기
	Chat2<-strsplit(Chat, " ")

###분석할 사용자명 설정
	Name<-c('(알수없음)', '가정현', '김정민', '박병규', '박지성', '타다요시', '이종하', '전현구', '정승원', '한기석', '회원님')
	dim(Name)<-c(11, 1)
	colnames(Name)<-'Name'

###매달마다 사용자별 대화량 추출후 합치기
	name1<-c()
	for(i in 1:(mon_2-1)) name1[i]<-Chat2[[i]][6]
	tab1<-table(name1)
	dat1<-data.frame(Name=names(tab1), Jan=as.numeric(tab1))
	Chat3<-merge(Name, dat1, by='Name', all.x=T)

	name2<-c()
	for(i in mon_2:(mon_3-1)) name2[i]<-Chat2[[i]][6]
	tab2<-table(name2)
	dat2<-data.frame(Name=names(tab2), Feb=as.numeric(tab2))
	Chat3<-merge(Chat3, dat2, by='Name', all.x=T)

	name3<-c()
	for(i in mon_3:(mon_4-1)) name3[i]<-Chat2[[i]][6]
	tab3<-table(name3)
	dat3<-data.frame(Name=names(tab3), Mar=as.numeric(tab3))
	Chat3<-merge(Chat3, dat3, by='Name', all.x=T)

	name4<-c()
	for(i in mon_4:(mon_5-1)) name4[i]<-Chat2[[i]][6]
	tab4<-table(name4)
	dat4<-data.frame(Name=names(tab4), Apr=as.numeric(tab4))
	Chat3<-merge(Chat3, dat4, by='Name', all.x=T)

	name5<-c()
	for(i in mon_5:(mon_6-1)) name5[i]<-Chat2[[i]][6]
	tab5<-table(name5)
	dat5<-data.frame(Name=names(tab5), May=as.numeric(tab5))
	Chat3<-merge(Chat3, dat5, by='Name', all.x=T)

	name6<-c()
	for(i in mon_6:(mon_7-1)) name6[i]<-Chat2[[i]][6]
	tab6<-table(name6)
	dat6<-data.frame(Name=names(tab6), Jun=as.numeric(tab6))
	Chat3<-merge(Chat3, dat6, by='Name', all.x=T)

	name7<-c()
	for(i in mon_7:(mon_8-1)) name7[i]<-Chat2[[i]][6]
	tab7<-table(name7)
	dat7<-data.frame(Name=names(tab7), Jul=as.numeric(tab7))
	Chat3<-merge(Chat3, dat7, by='Name', all.x=T)

	name8<-c()
	for(i in mon_8:(mon_9-1)) name8[i]<-Chat2[[i]][6]
	tab8<-table(name8)
	dat8<-data.frame(Name=names(tab8), Aug=as.numeric(tab8))
	Chat3<-merge(Chat3, dat8, by='Name', all.x=T)

	name9<-c()
	for(i in mon_9:(mon_10-1)) name9[i]<-Chat2[[i]][6]
	tab9<-table(name9)
	dat9<-data.frame(Name=names(tab9), Sep=as.numeric(tab9))
	Chat3<-merge(Chat3, dat9, by='Name', all.x=T)

	name10<-c()
	for(i in mon_10:(mon_11-1)) name10[i]<-Chat2[[i]][6]
	tab10<-table(name10)
	dat10<-data.frame(Name=names(tab10), Oct=as.numeric(tab10))
	Chat3<-merge(Chat3, dat10, by='Name', all.x=T)

	name11<-c()
	for(i in mon_11:(mon_12-1)) name11[i]<-Chat2[[i]][6]
	tab11<-table(name11)
	dat11<-data.frame(Name=names(tab11), Nov=as.numeric(tab11))
	Chat3<-merge(Chat3, dat11, by='Name', all.x=T)

	name12<-c()
	for(i in mon_12:n) name12[i]<-Chat2[[i]][6]
	tab12<-table(name12)
	dat12<-data.frame(Name=names(tab12), Dec=as.numeric(tab12))
	Chat3<-merge(Chat3, dat12, by='Name', all.x=T)

	Chat3[is.na(Chat3)]<-0
	rownames(Chat3)<-Chat3[,1]
	Chat4<-Chat3[,-1]
	rownames(Chat4)[rownames(Chat4)=='회원님']<-'박민재'

###충진이 데이터 병합
	Chat4[9,]<-Chat4[1,]+Chat4[9,]
	Chat4<-Chat4[-1,]

###데이터 완ㅋ성ㅋ
	head(Chat4)





#######################################################################################
###
###	Chat4<-Chat4[, 7:12]



###데이터파일 가공
Dat_Chat4<-expand.grid(Name=rownames(Chat4), Month=colnames(Chat4))
Dat_Chat4$Chat<-as.vector(as.matrix(Chat4))
pie<-arrange(data.frame(Name=rownames(Chat4), Chat=apply(Chat4, 1, sum)), Chat)
pie$Percent<-round(pie$Chat/sum(pie$Chat)*100, digits=1)
pie$mid<-cumsum(pie$Percent)-pie$Percent/2



###차트
chart1<-ggplot(Dat_Chat4, aes(Month, reorder(Name,Chat), fill=Chat))+
geom_tile(col='white')+
scale_fill_gradient2(low="white", high="steelblue")+
geom_text(aes(Month, Name, label=Chat), size=3)+
ggtitle('월별 대화량 (개인별)')

chart2<-ggplot(pie, aes(factor(0), Percent, fill=reorder(Name, -Percent)))+
geom_bar(stat='identity')+
scale_fill_brewer(palette='Set3')+
guides(fill=guide_legend(title=NULL))+
geom_text_repel(aes(x=1, y=mid, label=Percent), segment.size=0, size=3)+
coord_polar(theta='y')+
ggtitle('톡방 점유율')

chart3<-ggplot(Dat_Chat4, aes(Month, Chat, fill=reorder(Name, -Chat)))+
geom_bar(stat='identity')+
scale_fill_brewer(palette='Set3')+
guides(fill=guide_legend(title=NULL))+
ggtitle('월별 대화량 (통합형)')

chart4<-ggplot(Dat_Chat4, aes(Month, Chat, fill=reorder(Name, -Chat)))+
geom_bar(stat='identity', position='fill')+
scale_fill_brewer(palette='Set3')+
guides(fill=guide_legend(title=NULL))+
ggtitle('월별 점유율')

##차트 출력
grid.arrange(chart1, chart2, chart3, chart4, nrow=2, ncol=2)










######################################################################################

	dat<-readLines("KakaoTalkChats_2017.txt")
	t<-grep('2017년 1월 1일', dat)[1]
	Chat<-dat[t:length(dat)]
	Chat2<-strsplit(Chat, " ")



###결측치 없애기
for(i in 1:length(Chat2)){
	if(is.na(Chat2[[i]][6])) Chat2[[i]][6]<-''
}

###공백 없애기
for(i in 1:length(Chat2)){
	if(is.na(Chat2[[i]][1])) Chat2[[i]]<-''
}

###엔터 없애기
for(i in length(Chat2):1){
	if(Chat2[[i]][1]!='2017년'){
		Chat2[[i-1]]<-c(Chat2[[i-1]], Chat2[[i]])
		Chat2[[i]]<-c('', '', '', '', '', '')
	}
}



User<-list()
for(j in 1:10){
	User_1<-list()
	for(i in 1:length(Chat2)) if(Chat2[[i]][6]==Name[j+1]) User_1[[i]]<-Chat2[[i]][-(1:7)]
	User_1<-unlist(User_1)
	User[[j]]<-User_1
}


for(i in 1:10){
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
User[[i]]<-gsub("\\=", "", User[[i]])
User[[i]][grep("\\(", User[[i]])]<-''
User[[i]][grep("\\/", User[[i]])]<-''
}


###
tab_all<-sort(table(unlist(User)), decreasing=T)[-1]
col_cloud<-c("#69D295", "#F8DB4A", "#968DFF", "#FFAC8D", "#788D4D", "#267180")


wordcloud(names(tab_all[1:250]), scale=c(7, 1), random.order=T, random.color=F, freq=tab_all, rot.per=0, col=col_cloud)	




####출력할 길이
N<-30

tab_User<-list()
for(i in 1:10){
tab_User[[i]]<-sort(table(User[[i]]), decreasing=T)[2:(N+1)]
}

Count<-NULL
for(i in 1:10) Count<-c(Count, as.numeric(tab_User[[i]]))
Word_chat<-NULL
for(i in 1:10) Word_chat<-c(Word_chat, names(tab_User[[i]]))

Word<-expand.grid(Chat=1:N, User=c(Name[2:10], '박민재'))
Word$Count<-Count
Word$Words<-Word_chat




ggplot(Word, aes(1, -Chat))+
geom_text_repel(aes(color=User, label=Words, size=Count), segment.alpha=0, force=5)+
scale_size(range = c(4, 15), guide = FALSE)+
scale_y_continuous(breaks = NULL)+
scale_x_continuous(breaks = NULL)+
labs(x = '', y = '')+
facet_grid(.~User)






#