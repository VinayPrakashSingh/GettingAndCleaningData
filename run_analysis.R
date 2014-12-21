#nstailled required packages

install.packages("data.table")
install.packages("reshape2")

##Test install packages

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)


##setting the path to local repository folder(Path may change for another system)
setwd("C:\\Users\\admin\\Desktop\\GettingAndCleaningData")

##Setting the variable named path for further use
path <- getwd()

##downloaded file from the zip url and extracted them into local repo directory 

##See all file listing

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

##Reading Training and Test data
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

##Reading the activity files(i.e label files)
dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))

##Merge Training and Test data set
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

##Merge columns

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

##Setting keys

setkey(dt, subject, activityNum)

##Extracting mean and Stdev
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

##Subsetting mean and Stdev
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

##Converting  a vector of variable names matching columns in dt.

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)

##dtFeatures$featureCode

##Subsetting variables using variable names.

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]

##USe descriptive activity names
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

##Merge activity label

dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)

##Add activityName as a key

setkey(dt, subject, activityNum, activityName)

##Reshaping from a short and wide format

dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))

##Merge activity names

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
            all.x = TRUE)

##Create a new variable activity equivalent to activityName as a factor class. Create a new variable, feature that is equivalent to featureName as a factor class

dt$activity <- factor(dt$activityName)

##Seperate features from featureName using the helper function grepthis.

grepthis <- function(regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

##Check to make sure all possible combinations of feature are accounted for by all possible combinations of the factor class variables

r1 <- nrow(dt[, .N, by = c("feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", 
                           "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2


##Part 1) Merges the training and the test sets to create one data set.
##Create a tidy data set

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]

##Part 2. Extracts only the measurements on the mean and standard deviation for each measurement
dtTidy[, .N, by = c(names(dtTidy)[grep("^feat", names(dtTidy))])]


##write.table(dtTidy, "dtTidy.txt", sep="\t", row.names=FALSE, col.names=FALSE) 