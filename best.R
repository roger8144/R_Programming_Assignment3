best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death rate and 
    ## break the tie if there are more than one hospitals with the same lowest
    ## 30-day death rate
    result <- filter(dataUse, State == state, !is.na(dataUse[, exist]))
    result <- result[order(result$Hospital.Name), ]
    result$Hospital.Name[which.min(result[, exist])]
}