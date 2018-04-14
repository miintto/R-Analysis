library(png)
library(jpeg)
setwd('C:/Users/Minjae/Desktop')


### �̹��� �ҷ�����
img = readJPEG('IMG_2201.jpg')


dim(img)
### 630*630*3 (����, ����, RGB��)
### [0, 1]������ �����ͷ� �̷���� �ְ� ���ڰ� Ŀ������ �������.


### �̹��� ���
plot(0:1, 0:1, type='n')
rasterImage(img, 0, 0, 1, 1)


img[, , 1]   ### red
img[, , 2]   ### green
img[, , 3]   ### blue


###�� ������ ȸ���� �ȴ�.
img[, , 1]<-(img[, , 1]+img[, , 2]+img[, , 3])/3

plot(0:1, 0:1, type='n')
rasterImage(img[, , 1], 0, 0, 1, 1)


