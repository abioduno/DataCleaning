#1.Merges the training and the test sets to create one data set.
library(dplyr)
#test data
X_test<-read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$methods)
Y_test<-read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "id")
Subject_test <-read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

#train data
X_train<-read.table("UCI HAR Dataset/train/X_train.txt", col.names=features$methods)
Y_train<-read.table("UCI HAR Dataset/train/Y_train.txt", col.names = "id")
Subject_train <-read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

#features and activity data
features<-read.table("UCI HAR Dataset/features.txt", col.names = c("N","methods"))
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("id", "activity"))

#concatenation of tables
Xdata <- rbind(X_test, X_train)
Ydata <- rbind(Y_test, Y_train)
Subject <- rbind(Subject_test, Subject_train)

combineddata<- cbind(Subject, Ydata, Xdata)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#Measurements
DataExtracts <- combineddata %>%
  select(subject, id, contains("mean"), contains("std"))
#measurements<-grep("mean\\(\\)|std\\(\\)", features$methods)
#length(measurements)


#3. Uses descriptive activity names to name the activities in the data set
#Name the datast with descriptive activity names
DataExtracts$id <-activity_labels[DataExtracts$id,2]

#4. Appropriately labels the data set with descriptive variable names.
names(DataExtracts)<-gsub("^t", "Time", names(DataExtracts))
names(DataExtracts)<-gsub("^f", "Frequency", names(DataExtracts))
names(DataExtracts)<-gsub("BodyBody", "Body", names(DataExtracts))
names(DataExtracts)<-gsub("^angle.t", "t_Angle", names(DataExtracts)) 
names(DataExtracts)[2]="activity"

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
SecondDataExtract <- DataExtracts %>%
  group_by(subject, activity) %>%
  summarize_all(list(mean=mean, median=median))

#write output to a file
write.table(SecondDataExtract, "analysis_output.txt", row.name=FALSE)