# Step1. Merges the training and the test sets to create one data set.

trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
trainLabel <- read.table("UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("UCI HAR Dataset/test/X_test.txt")

testLabel <- read.table("UCI HAR Dataset/test/y_test.txt") 
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
joinData <- rbind(trainData, testData)

joinLabel <- rbind(trainLabel, testLabel)

joinSubject <- rbind(trainSubject, testSubject)


# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("UCI HAR Dataset/features.txt")

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])

joinData <- joinData[, meanStdIndices]

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)

write.table(cleanedData, "complete_data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
library(reshape2)
cd_mean <- melt(cleanedData, id=c("subject", "activity"))
result <- dcast(cd_mean, activity + subject ~ variable, mean)

write.table(result, "tidied.txt") # write out the 2nd dataset
