library(png)
library(jpeg)
setwd('C:/Users/Minjae/Desktop')


### 이미지 불러오기 (둘 중 아무거나 가져와도 됨)
img = readJPEG('KakaoTalk_20180415_023836491.jpg')   # .jpg
img = readPNG('KakaoTalk_20180415_002937908.png')   # .png


### 사진 확인
plot(0:1, 0:1, type='n')
rasterImage(img, 0, 0, 1, 1)


### 흑백으로 변환
img[, , 1]<-(img[, , 1]+img[, , 2]+img[, , 3])/3


### 가로 100픽셀을 기준으로 변환
n<-100
x_pix<-100
y_pix<-round(50*dim(img)[1]/dim(img)[2])
i_ran = floor(dim(img)[1]/y_pix)
j_ran = floor(dim(img)[2]/x_pix)


### 픽셀 병합
img_mat<-matrix(0, y_pix, x_pix)
for(i in 1:y_pix){
  for(j in 1:x_pix){
    img_mat[i, j]<-mean(img[(i_ran*i-i_ran+1):(i_ran*i), (j_ran*j-j_ran+1):(j_ran*j), 1])
  }
}


### 축소된 이미지 확인
plot(0:1, 0:1, type='n')
rasterImage(img_mat, 0, 0, 1, 1)



#############################################################################
### 색 배분을 잘 조절해야 한다..
### 숫자가 높을수록 밝다

hist(img_mat, breaks = 30)
ran<-quantile(img_mat, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))


### 대입
img_mat2<-img_mat
img_mat2[img_mat<=ran[1]]<-" "
img_mat2[ran[1]<=img_mat & img_mat<ran[2]]<-"."
img_mat2[ran[2]<=img_mat & img_mat<ran[3]]<-":"
img_mat2[ran[3]<=img_mat & img_mat<ran[4]]<-"i"
img_mat2[ran[4]<=img_mat & img_mat<ran[5]]<-"7"
img_mat2[ran[5]<=img_mat & img_mat<ran[6]]<-"Y"
img_mat2[ran[6]<=img_mat & img_mat<ran[7]]<-"8"
img_mat2[ran[7]<=img_mat & img_mat<ran[8]]<-"N"
img_mat2[ran[8]<=img_mat & img_mat<ran[9]]<-"M"
img_mat2[ran[9]<=img_mat]<-"@"


### 바로 뽑을수 있도록 형태 변환
img_txt<-apply(img_mat2, 1, paste0, collapse='')
img_txt<-paste0("printf(\"", img_txt, "\\n\");")


### 이렇게 뽑아서 C로 돌려본다.
write.csv2(img_txt, 'Output.txt', row.names=F, quote=F)

