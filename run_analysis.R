
###STEP 1 READ ALL
#features and labels
lbl<-read.csv("Data/activity_labels.txt",sep="",head=F)
features<-read.csv("Data/features.txt",sep = "",head=F)
#train
train.x<-read.csv("Data/train/X_train.txt",sep = "",head=F)
train.y<-read.csv("Data/train/y_train.txt",sep = "",head=F)
train.subject<-read.csv("Data/train/subject_train.txt",sep = "",head=F)


#test
test.x<-read.csv("Data/test/X_test.txt",sep = "",head=F)
test.y<-read.csv("Data/test/y_test.txt",sep = "",head=F)
test.subject<-read.csv("Data/test/subject_test.txt",sep = "",head=F)



###MERGING and tranforming
library(plyr)

t1<-test.x
t2<-train.x

#changing the names
names(t1)<-as.character(features[,2])
names(t2)<-as.character(features[,2])

#adding the colums of subject id
t1$subject<-test.subject[,1]
t2$subject<-train.subject[,1]

#adding activities colums
activities<-as.character(lbl[,2])

act.t1<-as.factor(test.y[,1])
act.t2<-as.factor(train.y[,1])
levels(act.t1)<-activities
levels(act.t2)<-activities

t1$activity<-act.t1
t2$activity<-act.t2

#create index
t1.index<-seq(1,dim(t1)[1])
t2.index<-seq(dim(t1)[1]+1,dim(t1)[1]+dim(t2)[1])

t1$index<-t1.index
t2$index<-t2.index

#merge datasets

merged<-rbind(t1,t2)

#select ther desired columns

data.mean<-grep("mean",names(merged))
data.sd<-grep("std",names(merged))
activ<-grep("activity",names(merged))
subj<-grep("subject",names(merged))
desired.index<-sort(c(data.sd,data.mean,activ,subj))

m1<-merged[,desired.index]
m1$subject<-as.factor(m1$subject)

splt1<-split(m1,m1$activity)
splt2<-split(m1,m1$subject)

d1<-as.data.frame(sapply(splt1,function(x) colMeans(x[,1:79],na.rm=T)))

d2<-as.data.frame(sapply(splt2,function(x) colMeans(x[1:79],na.rm=T)))

names(d2)<- paste("Subject",names(d2),sep=" ")

new.dataset<-cbind(d1,d2)

write.csv(new.dataset,file="newdata.csv",sep="")
