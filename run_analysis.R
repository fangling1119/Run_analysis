#��չ����ռ�
rm(list=ls(all=TRUE))

##1.Merges the training and the test sets to create one data set
##��ȡtrain����
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt",header = FALSE, sep = "")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt",header = FALSE, sep = "")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",header = FALSE, sep = "")

##��ȡtest����
x_test<-read.table("./UCI HAR Dataset/test/X_test.txt",header = FALSE, sep = "")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt",header = FALSE, sep = "")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",header = FALSE, sep = "")

activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE, sep = "")
features<-read.table("./UCI HAR Dataset/features.txt",header = FALSE, sep = "")

#�ϲ�����
x<-rbind(x_train,x_test)
y<-rbind(y_train,y_test)
xy<-cbind(x,y)                  #xyΪû�мӱ�����ʱ�����ݼ�


##2.Extracts only the measurements on the mean and standard deviation for each measurement
f<-as.character(features[,2])
i<-grep("mean()", f)
i1<-grep("meanFreq()",f)
i<-i[-match(i1,i)]
j<-grep("std()", f)
k<-sort(c(i,j))
xy_mean_std<-xy[,k]        #��ȡmean and standard deviation�Ľ����δ�ӱ�����
f1<-f[k]                   #f1Ϊxy_mean_std���ж�Ӧ��label

##3.Uses descriptive activity names to name the activities in the data set
activity<-match(y$V1,activity_labels$V1)
y1<-activity_labels[activity,2]    #descriptive activity names
y1<-as.data.frame(y1)  
xy1<-cbind(x,y1)               #�滻��activity_labels�����ݼ���δ�ӱ�������

##4.Appropriately labels the data set with descriptive variable names.
colnames(x)<-features$V2
colnames(y)<-"activity_labels"
xy<-cbind(x,y)              #��1�кϲ������ݼӱ�����
colnames(xy_mean_std)<-f1   #��2����ȡ�����ݼӱ�����
colnames(y1)<-"activity_names"
xy1<-cbind(x,y1)            #��3�����ݼӱ�����

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subject<-rbind(subject_train,subject_test)
colnames(subject)<-"subject"
xy2<-cbind(xy1,subject)     
xy2_subject<-data.frame()
for (i in 1:30)
  {t<-subset(xy2,subject==i)
   for (j in 1:561){
     xy2_subject[i,j]<-mean(t[,j])
        }
}
xy2_activity<-data.frame()
for (i in 1:6)
{t<-subset(xy,activity_labels==i)
 for (j in 1:561){
   xy2_activity[i,j]<-mean(t[,j])
 }
}
colnames(xy2_activity)<-features$V2
colnames(xy2_subject)<-features$V2

xy_mean_std1<-cbind(xy_mean_std,subject)     
xy_mean_std1_subject<-data.frame()
for (i in 1:30)
{t<-subset(xy_mean_std1,subject==i)
 for (j in 1:66){
   xy_mean_std1_subject[i,j]<-mean(t[,j])
 }
}
xy_mean_std1_activity<-data.frame()
for (i in 1:6)
{t<-subset(xy,activity_labels==i)
 for (j in 1:66){
   xy_mean_std1_activity[i,j]<-mean(t[,j])
 }
}
colnames(xy_mean_std1_activity)<-f1
colnames(xy_mean_std1_subject)<-f1

##��5���������
write.table(xy2_activity, file = "./UCI HAR Dataset/xy2_activity.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
write.table(xy2_subject, file = "./UCI HAR Dataset/xy2_subject.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
write.table(xy_mean_std1_activity, file = "./UCI HAR Dataset/xy_mean_std1_activity.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
write.table(xy_mean_std1_subject, file = "./UCI HAR Dataset/xy_mean_std1_subject.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")