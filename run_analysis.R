library("data.table")
run_analysis <- function(parameter = "BodyAcc", measure = "mean", axis = "X") {
	# Constants
	BASE_DIRECTORY <- "./UCI_HAR_Dataset/"
	TEST_DIRECTORY <- "test"
	TRAIN_DIRECTORY <- "train"
	PARAMETER_VALUES <- c("BodyAcc", "GravityAcc", "BodyAccJerk")
	MEASURE_VALUES <- c("mean", "std")
	AXIS_VALUES <- c("X", "Y", "Z")

	# Validate Input Parameters
	if(!(parameter %in% PARAMETER_VALUES)) stop("Invalid Parameter Provided")
	if(!(measure %in% MEASURE_VALUES)) stop("Invalid Measure Provided")
	if(!(axis %in% AXIS_VALUES)) stop("Invalid Axis Provided")
	
	# Read the Labels
	dataLabels <- read.table("./UCI_HAR_Dataset/features.txt", header=FALSE)

	# Read the Activities
	activityLabels <- read.table("./UCI_HAR_Dataset/activity_labels.txt", header=FALSE)
	
	# Read the Test Data
	print("Test - Read Subjects")
	testdataSubject <- read.table("./UCI_HAR_Dataset/test/subject_test.txt", header=FALSE, col.names="Subject")
	print("Test - Read X Test")
	testdataXTest <- read.table("./UCI_HAR_Dataset/test/X_test.txt", header= FALSE)
	print("Test - Read Y Test")
	testdataYTest <- read.table("./UCI_HAR_Dataset/test/Y_test.txt", header= FALSE, col.names="Test")
	
	print(sprintf("Test - Number of Rows in Subject = %d", nrow(testdataSubject)))
	print(sprintf("Test - Number of Rows in XTest = %d", nrow(testdataXTest)))
	print(sprintf("Test - Number of Rows in YTest = %d", nrow(testdataYTest)))
	
	# Read the Train Data
	print("Train - Read Subjects")
	traindataSubject <- read.table("./UCI_HAR_Dataset/train/subject_train.txt", header= FALSE, col.names="Subject")
	print("Train - Read X Test")
	traindataXTest <- read.table("./UCI_HAR_Dataset/train/X_train.txt", header= FALSE)
	print("Train - Read Y Test")
	traindataYTest <- read.table("./UCI_HAR_Dataset/train/Y_train.txt", header= FALSE, col.names="Test")
	
	print(sprintf("Train - Number of Rows in Subject = %d", nrow(traindataSubject)))
	print(sprintf("Train - Number of Rows in XTrain = %d", nrow(traindataXTest)))
	print(sprintf("Train - Number of Rows in YTrain = %d", nrow(traindataYTest)))
	
	# Assign the Column Headers
	names(testdataXTest) <- dataLabels$V2
	names(traindataXTest) <- dataLabels$V2
	
	# Merge the Columns across Test Data and Training Data
	testData <- data.table(testdataSubject, testdataXTest, testdataYTest)
	write.table(testData, "./C3Project_test.csv", sep=",", row.names=FALSE)
	trainData <- data.table(traindataSubject, traindataXTest, traindataYTest)
	write.table(trainData, "./C3Project_train.csv", sep=",", row.names=FALSE)
	
	print(sprintf("Number of Columns in Test Data = %d", ncol(testData)))
	print(sprintf("Number of Columns in Train Data = %d", ncol(trainData)))

	# Merge the Test Data and the Training Data
	fullData <- testData
	fullData <- rbind(fullData, trainData)

	print(sprintf("Number of Columns in Full Data = %d", ncol(fullData)))
	print(sprintf("Number of Rows in Full Data = %d", nrow(fullData)))
	
	# Write the data to a file
	write.table(fullData, "./C3ProjectFullData.csv", sep=",", row.names=FALSE)

	# Extract "Mean" and "Std" columns only
	dfFullData <- data.frame(fullData)
	extractedData <- dfFullData[, c(1,2,3,4,5,6,7,42,43,44,45,46,47,82,83,84,85,86,87,563)]

	print(sprintf("Number of Columns in Extracted Data = %d", ncol(extractedData)))
	print(sprintf("Number of Rows in Extracted Data = %d", nrow(extractedData)))
	
	# Write the data to a file
	write.table(extractedData, "./C3ProjectExtracted.csv", sep=",", row.names=FALSE)
	
	print("Replacing Activities")
	# Replace the Activity Labels
	extractedData$Test <- replace(extractedData$Test, extractedData$Test == "1", "WALKING")
	extractedData$Test <- replace(extractedData$Test, extractedData$Test == "2", "WALKING_UPSTAIRS")
	extractedData$Test <- replace(extractedData$Test, extractedData$Test == "3", "WALKING_DOWNSTAIRS")
	extractedData$Test <- replace(extractedData$Test, extractedData$Test == "4", "SITTING")
	extractedData$Test <- replace(extractedData$Test, extractedData$Test == "5", "STANDING")
	extractedData$Test <- replace(extractedData$Test, extractedData$Test == "6", "LAYING")

	print(sprintf("Number of Columns in Replaced Data = %d", ncol(extractedData)))
	print(sprintf("Number of Rows in Replaced Data = %d", nrow(extractedData)))
	
	# Write the data to a file
	write.table(extractedData, "./C3ProjectActivity.csv", sep=",", row.names=FALSE)
	
	# Average of each variable for every Activity and for every Subject
	if(parameter == "BodyAcc") {
		if(measure == "mean") {
			if(axis == "X") {
				result <- xtabs(tBodyAcc.mean...X ~ Subject + Test, data = extractedData)
			}
			else if(axis == "Y") {
				result <- xtabs(tBodyAcc.mean...Y ~ Subject + Test, data = extractedData)
			}
			else {
				result <- xtabs(tBodyAcc.mean...Z ~ Subject + Test, data = extractedData)
			}
		}
		else {
			if(axis == "X") {
				result <- xtabs(tBodyAcc.std...X ~ Subject + Test, data = extractedData)
			}
			else if(axis == "Y") {
				result <- xtabs(tBodyAcc.std...Y ~ Subject + Test, data = extractedData)
			}
			else {
				result <- xtabs(tBodyAcc.std...Z ~ Subject + Test, data = extractedData)
			}
		}
	}
	else if(parameter == "GravityAcc") {
		if(measure == "mean") {
			if(axis == "X") {
				result <- xtabs(tGravityAcc.mean...X ~ Subject + Test, data = extractedData)
			}
			else if(axis == "Y") {
				result <- xtabs(tGravityAcc.mean...Y ~ Subject + Test, data = extractedData)
			}
			else {
				result <- xtabs(tGravityAcc.mean...Z ~ Subject + Test, data = extractedData)
			}
		}
		else {
			if(axis == "X") {
				result <- xtabs(tGravityAcc.std...X ~ Subject + Test, data = extractedData)
			}
			else if(axis == "Y") {
				result <- xtabs(tGravityAcc.std...Y ~ Subject + Test, data = extractedData)
			}
			else {
				result <- xtabs(tGravityAcc.std...Z ~ Subject + Test, data = extractedData)
			}
		}		
	}
	else {
		if(measure == "mean") {
			if(axis == "X") {
				result <- xtabs(tBodyAccJerk.mean...X ~ Subject + Test, data = extractedData)
			}
			else if(axis == "Y") {
				result <- xtabs(tBodyAccJerk.mean...Y ~ Subject + Test, data = extractedData)
			}
			else {
				result <- xtabs(tBodyAccJerk.mean...Z ~ Subject + Test, data = extractedData)
			}
		}
		else {
			if(axis == "X") {
				result <- xtabs(tBodyAccJerk.std...X ~ Subject + Test, data = extractedData)
			}
			else if(axis == "Y") {
				result <- xtabs(tBodyAccJerk.std...Y ~ Subject + Test, data = extractedData)
			}
			else {
				result <- xtabs(tBodyAccJerk.std...Z ~ Subject + Test, data = extractedData)
			}
		}
	}
	
	# Write the Output
	write.table(result, "./C3ProjectResult.csv", sep=",", row.names=FALSE)
	result
}