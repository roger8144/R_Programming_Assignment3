rankhospital <- function(state, outcome, num = "best") {
    library(dplyr)
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    featureNames <- names(data)
    colNumber <- grep("State|(^Hospital+(.*)+Death+(.*)+(Heart.Attack|Heart.Failure|Pneumonia)$)|^H+(.*)+Name$", 
                      featureNames)
    dataUse <- data[, colNumber]
    featureNames <- names(dataUse)
    
    ## Check that state and outcome are valid
    words <- strsplit(tolower(featureNames), split = "from.", fixed = TRUE)
    exist <- sapply(words, function(x) is.element(sub(" ", ".", tolower(outcome)), x))
    if (!is.element(state, dataUse$State)) {
        stop("invalid state")
    } else if (sum(which(exist)) == 0) {
        stop("invalid outcome")
    }
    
    ## convert characters to numerics  
    dataUse[, -2:-1] <- lapply(dataUse[, -2:-1], function (x) as.numeric(x))
    
    ## Return hospital name in that state with the given rank 
    ## 30-day death rate
    result <- filter(dataUse, State == state, !is.na(dataUse[, exist]))
    result <- result[order(result[, exist], result$Hospital.Name), ]
    if (num == "best") {
        result$Hospital.Name[1]
    } else if (num == "worst") {
        result$Hospital.Name[dim(result)[1]]
    } else {
        result$Hospital.Name[num]
    }     
}