library(data.table)
library(dplyr)
library(tidyr)
data1 <- file.path("./data" , "UCI HAR Dataset")

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)


subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
ytest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
xtest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


#Merge the training and the test sets to create one data set


#combine the respective data in both train and test data sets corresponding
#to subject, activity and features

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(ytrain, ytest)
features <- rbind(xtrain, xtest)

#name the column
colnames(features) <- t(featureNames[2])

#merge

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Extracts only the measurements on the mean and standard deviation for each measurement

columnswithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredcolumns <- c(columnswithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredcolumns]
dim(extractedData)

#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
extractedData$Activity <- factor(extractedData$Activity, labels=c("Walking",
                                                             "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

#From the data set in step 4, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.


#set subject as a factor variable
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#data set with average for each activity and subject.
tidyDataset <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyDataset <- tidyDataset[order(tidyDataset$Subject,tidyDataset$Activity),]
write.table(tidyDataset, file = "Tidy.txt", row.names = FALSE)




