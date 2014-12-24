rankall <- function (outcome, num = "best") {
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
    if (sum(which(exist)) == 0) {
        stop("invalid outcome")
    }
    
    ## convert characters to numerics  
    dataUse[, -2:-1] <- lapply(dataUse[, -2:-1], function (x) as.numeric(x))
    
    ## For each state, find the hospital of the given rank
    splitData <- split(dataUse, as.factor(dataUse$State))
    result <- lapply(splitData, function (x) {x <- filter(x, !is.na(x[, exist])); 
                                              x <- x[order(x[, exist], x$Hospital.Name), ];
                                              if (num == "best") {
                                                  c(x$Hospital.Name[1], x$State[1])
                                              } else if (num == "worst") {
                                                  c(tail(x$Hospital.Name, n = 1), x$State[1])
                                              } else {
                                                  c(x$Hospital.Name[num], x$State[1])
                                              }})
    
    ## Return a data frame with the hospital names and the 
    ## (abbreviated) state name
    result <- data.frame(matrix(unlist(result), nrow = length(result), byrow = TRUE))
    names(result) <- c("hospital", "state")
    row.names(result) <- result$state
    result
}