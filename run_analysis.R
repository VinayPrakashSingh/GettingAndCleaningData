# Step1. Merges the training and the test sets to create one data set.
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
head(trainData)
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt") 
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainLabel, testLabel)
joinSubject <- rbind(trainSubject, testSubject)

# Step2. Extracts only the measurements on the mean and stdev 
features <- read.table("./UCI HAR Dataset/features.txt")

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])

joinData <- joinData[, meanStdIndices]

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
names(joinData) <- gsub("mean", "Mean", names(joinData)) 
names(joinData) <- gsub("std", "Std", names(joinData)) 
names(joinData) <- gsub("-", "", names(joinData)) 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel)

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject)
cleanedData <- cbind(joinSubject, joinLabel, joinData)
write.table(cleanedData, "dtTidy.txt",row.names=FALSE) 
# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)



write.table(result, "meandata.txt",row.names=FALSE) # write out the 2nd dataset
