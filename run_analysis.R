
dfmean <- function(df){
    df1 <- as.data.frame(df)
    vec <- numeric()
    for (i in 3:ncol(df1)){
        vec = c(vec, mean(df1[, i]))        
    }
    vec   
}

## Read the related data files into workspace
x_train <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt")
x_test <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt")

y_train <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
y_test <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt")

subject_train <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
subject_test <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")

features <- read.table(".\\UCI HAR Dataset\\features.txt", as.is = c(1, 2))
activity_lables <- read.table(".\\UCI HAR Dataset\\activity_labels.txt")

## 1. Merges the training and the test sets to create one data set.
x <- rbind(x_train, x_test)
subject <- rbind(subject_train, subject_test)
y <- rbind(y_train, y_test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 2.1 Appropriately labels the data set with descriptive variable names
names(x) <- features[[2]]

## 2.2 extract the measurements whose name include mean or std(standard deviation)
x_extract <- x[, grep("mean|std", features[[2]])]

## 3. Uses descriptive activity names to name the activities in the data set
## 3.1 Get the descriptive activity vector for data set x
activity_vec <- sapply(y, function(x) activity_lables[x,2])

## 3.2 merge subject and activity lables into x data set
x_merge <- cbind(subject[[1]], activity_vec, x_extract)

## 3.3 update the column names of subject and activity lables to descriptive ones
colnames(x_merge)[1:2] <- c("subject", "activity_lables")

## 4. Appropriately labels the data set with descriptive variable names, has been done in 2.1 and 3.3

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
second_x <- sapply(split(x_merge, list(x_merge[["subject"]], x_merge[["activity_lables"]])), dfmean)

## transpose the result matrix to the correct one which every row is an observation
second_x1 <- as.data.frame(t(second_x))

## add subject and activity_lables to original data frame
x2 <- t(as.data.frame(strsplit(rownames(second_x1), ".", fixed = TRUE)))
result <- cbind(x2, second_x1)

## change to descriptive colnames
colnames(result) <- colnames(x_merge)

write.table(result, file = "result.txt", row.name=FALSE)



