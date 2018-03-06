library(ggplot2)

#ggplot layer층

# Data : dataframe 형태의 데이터
# Aesthetics : 색, 모양 등 시각적 디자인
# Geometries : 출력할 데이터의 기하학적 형태
# Facets : 
# Statistics : 
# Coordinates : 
# Theme : 



head(diamonds)
dim(diamonds)

##Data : 데이터 입력 (물론 아무것도 출력되지 않음...)
dat1<-ggplot(diamonds[seq(1, 53940, 100),], aes(x=color))
dat2<-ggplot(diamonds[seq(1, 53940, 100),], aes(x=carat, y=price))


##Aesthetics : 색, 모양 등 원하는 시각적 디자인
##Geometrics : 점, 선, 도형 등의 기하학적 요소

dat1+geom_bar()	#bar plot
dat2+geom_line()	#선형그래프
dat2+geom_point()	#산점도 찍기
dat2+geom_point(aes(col=color, shape=color))	#색, 모양 설정


##Coordinates
dat1+geom_bar()+coord_flip()	##막대 가로로
#