#TO RUN, COPY ALL TEXT FILES FROM DATA SOURCE INTO THE FOLDER WHERE Rstudio RUNS FROM


# Part 1 - Merging the training set and the test set to make one data set

temp1 <- read.table("X_train.txt")
temp2 <- read.table("X_test.txt")
X <- rbind(temp1, temp2)

temp1 <- read.table("subject_train.txt")
temp2 <- read.table("subject_test.txt")
S <- rbind(temp1, temp2)

temp1 <- read.table("y_train.txt")
temp2 <- read.table("y_test.txt")
Y <- rbind(temp1, temp2)

#  Part 2 - Extracting only the measurements on the mean and std dev for each measurement

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# Part 3 - Naming the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# Part 4 - Labeling the data set with activity names

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creating a second, independent tidy dataset with the ave of ea var for ea activity and subject

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "DataSetWithAverages.txt", row.names = FALSE)
