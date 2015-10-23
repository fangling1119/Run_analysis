#清空工作空间
rm(list=ls(all=TRUE))

##1.Merges the training and the test sets to create one data set
##读取train数据
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt",header = FALSE, sep = "")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt",header = FALSE, sep = "")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",header = FALSE, sep = "")

##读取test数据
x_test<-read.table("./UCI HAR Dataset/test/X_test.txt",header = FALSE, sep = "")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt",header = FALSE, sep = "")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",header = FALSE, sep = "")

activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE, sep = "")
features<-read.table("./UCI HAR Dataset/features.txt",header = FALSE, sep = "")

#合并数据
x<-rbind(x_train,x_test)
y<-rbind(y_train,y_test)
xy<-cbind(x,y)                  #xy为没有加变量名时的数据集


##2.Extracts only the measurements on the mean and standard deviation for each measurement
f<-as.character(features[,2])
i<-grep("mean()", f)
i1<-grep("meanFreq()",f)
i<-i[-match(i1,i)]
j<-grep("std()", f)
k<-sort(c(i,j))
xy_mean_std<-xy[,k]        #提取mean and standard deviation的结果，未加变量名
f1<-f[k]                   #f1为xy_mean_std各列对应的label

##3.Uses descriptive activity names to name the activities in the data set
activity<-match(y$V1,activity_labels$V1)
y1<-activity_labels[activity,2]    #descriptive activity names
y1<-as.data.frame(y1)  
xy1<-cbind(x,y1)               #替换了activity_labels的数据集（未加变量名）

##4.Appropriately labels the data set with descriptive variable names.
colnames(x)<-features$V2
colnames(y)<-"activity_labels"
xy<-cbind(x,y)              #给1中合并的数据加变量名
colnames(xy_mean_std)<-f1   #给2中提取的数据加变量名
colnames(y1)<-"activity_names"
xy1<-cbind(x,y1)            #给3中数据加变量名

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

##第5步数据输出
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
