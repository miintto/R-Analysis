library(png)
library(jpeg)
setwd('C:/Users/Minjae/Desktop')


### 이미지 불러오기
img = readJPEG('IMG_2201.jpg')


dim(img)
### 630*630*3 (가로, 세로, RGB값)
### [0, 1]사이의 데이터로 이루어져 있고 숫자가 커질수록 밝아진다.


### 이미지 출력
plot(0:1, 0:1, type='n')
rasterImage(img, 0, 0, 1, 1)


img[, , 1]   ### red
img[, , 2]   ### green
img[, , 3]   ### blue


###다 섞으면 회색이 된다.
img[, , 1]<-(img[, , 1]+img[, , 2]+img[, , 3])/3

plot(0:1, 0:1, type='n')
rasterImage(img[, , 1], 0, 0, 1, 1)



