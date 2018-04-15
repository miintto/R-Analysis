[아스키 아트]
==========

# 1. R로 이미지 불러오기

이미지를 불러오려면 따로 패키지가 필요하다.

	install.packages("jpeg")
	library(jpeg)

위 패키지에 내장된 함수로 이미지를 불러올 수 있다.
이제 다음 이미지를 불러오도록 해보자.

![](./images/30ae255638f08ca40e897e5d8e8da414.jpg)

**이미지 불러오기는 readJPEG 함수를 이용해서 불러온다.**
사진을 읽어오면 이미지의 픽셀 단위로 0~1사이의 numeric 데이터 값이 저장된다.

	setwd('C:/Users/Minjae/Desktop')
	img = readJPEG('30ae255638f08ca40e897e5d8e8da414.jpg')

	plot(0:1, 0:1, type='n')
	rasterImage(img, 0, 0, 1, 1)

![](./images/ASCII_Art_1.png)

저장된 이미지를 보는 방법은 plot을 이용해서이다.
x축과 y축을 0부터 1까지의 범위로 출력해 두고 그 위에 rasterImage 함수를 이용해서 사진을 출력하는 방식이다. rasterImage는 기본 내장된 함수다.
**rasterImage(이미지 데이터, x시작값, y시작값, x종료값, y종료값)** 순으로 값을 입력해주면 각 꼭지점에 맞춰서 이미지가 출력되는 것을 확인할 수 있다. 

# 2. 이미지 데이터 구조 파악

	dim(img)
>562	1000	3

데이터의 구조를 살펴보면 3차원으로 되어있는데, 순서대로 **세로, 가로, RGB 값**으로 구성되어 있다.
간단히 562×1000의 행렬이 3겹으로 겹쳐져 있다고 생각하면 편하다.

그래서 이미지를 출력할 때 한 픽셀마다 3개의 RGB값을 모두 조합해서 출력하게 되는데, 만약 R값과 B값을 모두 0으로 만든다면 초록색의 이미지만 출력될 것이다.

	img[, , 1]<-0
	img[, , 3]<-0

	plot(0:1, 0:1, type='n')
	rasterImage(img, 0, 0, 1, 1)

![](./images/ASCII_Art_2.png)

이런 식으로 출력된다.


# 3. 아스키 아트

이제 주어진 이미지를 아스키 코드로 바꾸어 보자.
우선 이미지를 562×1000픽셀의 이미지를 모두 쓸수는 없으니 크기를 조금 줄여야 한다.
가로 길이 100픽셀을 기준으로 하려고 한다.

10:1의 비율이지만 세로의 길이는 56.2으로 잡는게 아니라 다시 반으로 줄인 28.1로 잡아야 한다.
**왜냐햐면 우리가 결과물을 출력할 cmd창에서 픽셀 하나에 대응될 텍스트 하나의 크기가 정사각형이 아니라 세로가 길쭉한 모양이기 때문이다.**
그래서 결론적으로 가로를 100, 세로가 28인 28×100의 행렬을 만들어준다.

	img_mat<-matrix(0, 28, 100)

	for(i in 1:28){
	  for(j in 1:100){
	    img_mat[i, j]<-mean(img[(20*i-19):(20*i), (10*j-9):(10*j), ])
	  }
	}

위의 계산은 for문을 이용해 한 픽셀이 되는 20×10 행렬의 모든 RGB값들의 평균을 구해서 28×100행렬의 한 픽셀값으로 만들어주는 과정이다. 간단히 말해서 **562×1000×3**을 **28×100**으로 줄여주는 과정인 셈이다.
결과를 출력해보면 화질이 다소 줄어든 점을 확인할 수 있다.

![](./images/ASCII_Art_3.png)

>입력해준 데이터가 2차윈일 경우, 즉 세로×가로 행렬로만 이루어진 경우에는 흑백으로 인식을 한다.

이제 각 픽셀마다 해당하는 밝기의 텍스트를 넣어 준다.
난 10종류의 텍스트를 사용했는데, 편의에 따라 가짓수를 늘려도 좋고 줄여도 좋다.

	prob<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
	ran<-quantile(img_mat, probs=prob)

	img_mat2<-img_mat
	img_mat2[img_mat<=ran[1]]<-" "
	img_mat2[ran[1]<=img_mat & img_mat<ran[2]]<-"."
	img_mat2[ran[2]<=img_mat & img_mat<ran[3]]<-":"
	img_mat2[ran[3]<=img_mat & img_mat<ran[4]]<-"i"
	img_mat2[ran[4]<=img_mat & img_mat<ran[5]]<-"7"
	img_mat2[ran[5]<=img_mat & img_mat<ran[6]]<-"X"
	img_mat2[ran[6]<=img_mat & img_mat<ran[7]]<-"8"
	img_mat2[ran[7]<=img_mat & img_mat<ran[8]]<-"N"
	img_mat2[ran[8]<=img_mat & img_mat<ran[9]]<-"M"
	img_mat2[ran[9]<=img_mat]<-"@"

위 코드는 모든 값중 하위 10%는 공백 " "을 집어넣고 하위 20%는 "."을 넣고 하는 식으로 작업한 코드이다.
**여기가 중요한 단계인데 어떤 텍스트를 어떤 비율로 넣어주냐에 따라 결과물이 확 달라질 수 있다.**

다음 단계는 제작된 과정을 텍스트로 내보내는 과정이다.
나는 C의 명령 프롬프트창으로 띄울거기 때문에 편의상 printf까지 합쳐주었다.

	img_txt<-apply(img_mat2, 1, paste0, collapse='')
	img_txt<-paste0("printf(\"", img_txt, "\\n\");")
	write.csv2(img_txt, 'Output.txt', row.names=F, quote=F)

작업한 Output이라는 이름으로 내보내주었다. 열어보면 다음과 같이 여러 텍스트 형태로 출력되어 있을 것이다.

>printf("                       ..8MM@@@@@@@@@MMN@@MMMMNNNNXXX8NMMM@@@@@@@@@@@@
>printf("                      ..iMM@@@@@@MN87iiX77X7ii::::::::::i7778M@@@@@@@@@@
>printf("                       :N@M@@@@MN87:.::::::iiiiiiiii::::::::XNMM@@@@@@@@
>printf("                       :M@@@@@MM8:..:::iiii77777777777iii::::X8MM@@@@@@@
> ...

이제 그대로 복사해서 C에서 실행을 시켜보자!

![](./images/ASCII_Art_4.png)


