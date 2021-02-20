train_valid_test_split <- function(x, trainSplit, validSplit, testSplit){
  
  if (length(x) < 3) {break("Not sufficient size for x: (x<3)")}
  if (sum(c(trainSplit, validSplit, testSplit))>1) { break("The sum of the splits exceeds 1")}
  
  status <- rep(as.character(NA), times = length(x))
  
  trainSize <- round(trainSplit * length(x))
  validSize <- round(validSplit * length(x))
  testSize  <- round(testSplit * length(x))
  
  status[1:trainSize]                            <- "Train"
  status[(trainSize+1):(trainSize+validSize)]    <- "Validation"
  status[(trainSize+validSize+1):length(status)] <- "Test"
  
  return(status)
}
