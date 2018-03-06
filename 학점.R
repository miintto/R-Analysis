setwd("D:/Minjae/Documents/Statistics/_DATA_SET")
dat<-read.table("Grade.txt", header=T, sep="\t")
head(dat)



names(dat)<-c('sex','grade','part_time_job','score','credits','hours','part_time_job_group','score_group')


#