## functions to clean "UCI HAR Dataset"

library(data.table)

loadData <- function(){
    ## load test and training data, combine them to one data Set and add feature names
    filePathTest <- "UCI HAR Dataset/test/"
    filePathTrain <- "UCI HAR Dataset/train/"
    
    featureNames <- fread("UCI HAR Dataset/features.txt", select=2, col.names = "featureNames")[[1]]
    
    featuresTest <- fread(paste(c(filePathTest,"X_test.txt"), collapse=""), col.names = featureNames)
    lablesTest <- fread(paste(c(filePathTest,"y_test.txt"), collapse=""), col.names = "activity")
    idTest <- fread(paste(c(filePathTest,"subject_test.txt"), collapse=""), col.names = "subject_id")
    
    featuresTrain <- fread(paste(c(filePathTrain,"X_train.txt"), collapse=""), col.names = featureNames)
    lablesTrain <- fread(paste(c(filePathTrain,"y_train.txt"), collapse=""), col.names = "activity")
    idTrain <- fread(paste(c(filePathTrain,"subject_train.txt"), collapse=""), col.names = "subject_id")
    
    dataTest <- cbind(idTest,featuresTest,lablesTest)
    dataTrain <- cbind(idTrain,featuresTrain,lablesTrain)
    dataComplete <- rbind(dataTrain,dataTest)
    
    dataComplete
}

getMeanStd <- function(dataSet){
    ## filter dataSet to only contain columns that have the words "mean" or "std"
    idxMeanStd <- grepl("mean|std", names(dataSet))
    idxMeanStd[c(1,length(idxMeanStd))] <- TRUE
    dataMeanStd <- dataSet[, ..idxMeanStd] 
    
    dataMeanStd
}

labelActievities <- function(dataSet, activityLabels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING" ,"STANDING", "LAYING")){
    ## convert numbers representing activities to actual activity names
    dataMeanStd <- dataMeanStd[, activity:=as.character(activity)]
    for (i in seq_along(activityLabels)){
        activityNr <- as.character(i)
        dataMeanStd[activity==activityNr, activity := activityLabels[i]]
    }
    dataMeanStd
}


renameFeatures <- function(dataSet){
    ## delete reserved signs "()" and "-" from column names to make it better readable
    colnames(dataSet) <- gsub("\\()","",colnames(dataSet))
    colnames(dataSet) <- gsub("\\-","_",colnames(dataSet))
    dataSet
}

groupMean <- function(dataSet){
    ## calculate mean of dataset grouped by activity and subject_id
    dataCols <- colnames(dataSet)[2:dim(dataSet)[2]-1]
    groupedMeans <- dataSet[,  lapply(.SD, mean),.(activity, subject_id)]
    groupedMeans
}

saveCleanData <- function(filename, dataSet){
    write.table(dataSet, filename, row.names=FALSE)
}