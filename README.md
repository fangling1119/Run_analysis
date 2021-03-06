# Run_analysis
Getting and Cleaning Data_ course project

==================================================================
 1.describing how the script works 

   step1: Merges the training and the test sets to create one data set.
   First, uses the function read.table() reading the data into R. The data we need to use include 'X_train.txt','X_test.txt','y_train.txt','y_test.txt','subject_train.txt','subject_test.txt','activity_labels.txt' and 'features.txt'.Respectively named as x_train,x_test,y_train,y_test, subject_train,subject_test,activity_labels and features. 
   Second, uses the function rbind() and cbind) to merge the training and the test sets to create one data set named xy.	
 
   step2: Extracts only the measurements on the mean and standard deviation for each measurement. 
   First, uses the function grep() and match() finding out the elements in which contains "mean()" and "std()",then extracts only the measurements on the mean and standard deviation for each measurement in the data set xy, at last, creates a new data set named xy_mean_std. 
 
   step3: Uses descriptive activity names to name the activities in the data set.
  Uses the function match() to match y$v1 and activity_labels$v1, then according to this index - matching results，we can name the activities in the data set xy，create one data set named xy1.
  
   step4: Appropriately labels the data set with descriptive variable names. 
    Uses the function colnames() attaching a label to the data set use the viriable named features.
    
   step5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
   

  ==================================================================                                                        
  2.the code book describing the variables
  
  The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

  Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 
mean(): Mean value
std(): Standard deviation

The complete list of variables of each feature vector as follow:

"1" "tBodyAcc-mean()-X"
"2" "tBodyAcc-mean()-Y"
"3" "tBodyAcc-mean()-Z"
"4" "tBodyAcc-std()-X"
"5" "tBodyAcc-std()-Y"
"6" "tBodyAcc-std()-Z"
"7" "tGravityAcc-mean()-X"
"8" "tGravityAcc-mean()-Y"
"9" "tGravityAcc-mean()-Z"
"10" "tGravityAcc-std()-X"
"11" "tGravityAcc-std()-Y"
"12" "tGravityAcc-std()-Z"
"13" "tBodyAccJerk-mean()-X"
"14" "tBodyAccJerk-mean()-Y"
"15" "tBodyAccJerk-mean()-Z"
"16" "tBodyAccJerk-std()-X"
"17" "tBodyAccJerk-std()-Y"
"18" "tBodyAccJerk-std()-Z"
"19" "tBodyGyro-mean()-X"
"20" "tBodyGyro-mean()-Y"
"21" "tBodyGyro-mean()-Z"
"22" "tBodyGyro-std()-X"
"23" "tBodyGyro-std()-Y"
"24" "tBodyGyro-std()-Z"
"25" "tBodyGyroJerk-mean()-X"
"26" "tBodyGyroJerk-mean()-Y"
"27" "tBodyGyroJerk-mean()-Z"
"28" "tBodyGyroJerk-std()-X"
"29" "tBodyGyroJerk-std()-Y"
"30" "tBodyGyroJerk-std()-Z"
"31" "tBodyAccMag-mean()"
"32" "tBodyAccMag-std()"
"33" "tGravityAccMag-mean()"
"34" "tGravityAccMag-std()"
"35" "tBodyAccJerkMag-mean()"
"36" "tBodyAccJerkMag-std()"
"37" "tBodyGyroMag-mean()"
"38" "tBodyGyroMag-std()"
"39" "tBodyGyroJerkMag-mean()"
"40" "tBodyGyroJerkMag-std()"
"41" "fBodyAcc-mean()-X"
"42" "fBodyAcc-mean()-Y"
"43" "fBodyAcc-mean()-Z"
"44" "fBodyAcc-std()-X"
"45" "fBodyAcc-std()-Y"
"46" "fBodyAcc-std()-Z"
"47" "fBodyAccJerk-mean()-X"
"48" "fBodyAccJerk-mean()-Y"
"49" "fBodyAccJerk-mean()-Z"
"50" "fBodyAccJerk-std()-X"
"51" "fBodyAccJerk-std()-Y"
"52" "fBodyAccJerk-std()-Z"
"53" "fBodyGyro-mean()-X"
"54" "fBodyGyro-mean()-Y"
"55" "fBodyGyro-mean()-Z"
"56" "fBodyGyro-std()-X"
"57" "fBodyGyro-std()-Y"
"58" "fBodyGyro-std()-Z"
"59" "fBodyAccMag-mean()"
"60" "fBodyAccMag-std()"
"61" "fBodyBodyAccJerkMag-mean()"
"62" "fBodyBodyAccJerkMag-std()"
"63" "fBodyBodyGyroMag-mean()"
"64" "fBodyBodyGyroMag-std()"
"65" "fBodyBodyGyroJerkMag-mean()"
"66" "fBodyBodyGyroJerkMag-std()"
