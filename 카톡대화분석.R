###함수실행

source("D:/Minjae/Documents/analyze.r")	#분석함수
library(wordcloud)




###카톡대화 분석기#########################################



###카톡 대화 파일 입력 (반드시 텍스트문서 인코딩을 ANSI로 할것!!)

txt<-"KakaoTalk_20170707_1409_31_627_group.txt"
txt<-"KakaoTalkChats(8).txt"





###분석 시작할 년, 월, 일 입력

name<-'박민재'
year<-2017
month<-9
day<-1










###단톡방 분석

Kakao_Chat(name, txt, year, month, day)



###단어 빈도수 분석(Word Cloud)

Kakao_WordCloud(txt, year, month, day)



###개인별 분석

name_ind<-"가정현"
Kakao_Indiv(txt, year, month, day, name_ind)





########################################################
