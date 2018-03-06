### 패키지 로드
library(maps)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(dplyr)


### 지도 데이터 불러오기 (long=경도, lat=위도 )
map_state<-map_data("state")    ##지도 데이터
map_state_cen<-data.frame(state.center, state.name)   ##지도좌표, 이름


### 텅 빈 지도 출력
ggplot(Dat_map, aes(long, lat, group = group))+  ### x, y값에 경도, 위도
geom_polygon(color = "black", fill = "white")+  ### 흰 배경에 검은 경계
geom_text(data=map_state_cen, aes(x=x, y=y, label=state.abb, group=NULL), size=2)



### 파일 불러오기
setwd("C:/Users/Minjae/Downloads")
Dat_Elc<-read.table("US_Pre.txt", header=T, sep=",")
Dat_Elc[,1]<-as.character(Dat_Elc[,1])
Dat_Elc[,1]<-tolower(Dat_Elc[,1])

### 우세한 후보 설정
Dat_Elc$Portion<-ifelse(Dat_Elc[,11]>Dat_Elc[,12], -Dat_Elc[,11], Dat_Elc[,12])
Dat_Elc$Winner<-ifelse(Dat_Elc[,11]>Dat_Elc[,12], 'Clinton', 'Trump')

### merge하기
Dat_map<-merge(Dat_Elc, map_state, by='region')

### order순으로 다시 배열
Dat_map<-arrange(Dat_map, order)



### 출력
ggplot()+
geom_polygon(data=Dat_map, aes(long, lat, group=group, fill=Portion), col='white')+
scale_fill_gradient2('Trump & \n Clinton', low='#0088ff', high='#ff3333')+
ggtitle("An Approval Rating of Two Candidate in United States")+
theme_void()+
geom_text(data=map_state_cen, aes(x=x, y=y, label=state.name, group=NULL), size=5)








chart1<-ggplot(Dat_map, aes(long, lat, group=group, fill=Per_Trump))+
geom_polygon(col='white')+
coord_map("polyconic")+
scale_fill_gradient2('Trump', low='white', high='#ff6666')+
ggtitle("An Approval Rating of Donald Trump in United States")+
theme_void()

chart2<-ggplot(Dat_map, aes(long, lat, group=group, fill=Per_Clinton))+
geom_polygon(col='white')+
coord_map("polyconic")+
scale_fill_gradient2('Clinton', low='white', high='#0099ff')+
ggtitle("An Approval Rating of Hillary Clinton in United States")+
theme_void()

chart4<-ggplot(Dat_map, aes(long, lat, group=group, fill=Total))+
geom_polygon(col='white')+
coord_map("polyconic")+
scale_fill_gradient2('Trump & \n Clinton', low='#0099ff', mid='white', high='#ff6666')+
ggtitle("An Approval Rating of Two Candidate in United States")+
theme_void()

grid.arrange(chart1, chart2, chart3, chart4, ncol=2, bottom=range)
