library(gtools)
library(stringr)
library(reshape2)
longify <- function(listOfPurchases){
    output <- data.frame(products=listOfPurchases[2:length(listOfPurchases)])
    output$transaction <- listOfPurchases[1]
    return(output)
}
getsupport <- function(line,frame){
    rollingset <- frame[,line[1]]
    for (i in 2:length(line)){
        rollingset <- intersect(rollingset, frame[,line[i]])
    }
    rollingset <- rollingset[!is.na(rollingset)]
    return(length(rollingset))
}
findFrequent <- function (myData, kitemset, minsup){
    itemslength <- ncol(myData)-1
    possible <- permutations(itemslength,kitemset)
    possible <- as.data.frame(possible + 1)
    pos <- apply(possible,1, function(x) getsupport(x,myData))
    possible$support <- pos/nrow(myData)
    return(possible[possible$support >= minsup,])
}


hypotheticalreadLines <- character(5)
hypotheticalreadLines[1] <- c("10, beer, nuts, diaper")
hypotheticalreadLines[2]  <- c("20, beer, coffee, diaper, nuts")
hypotheticalreadLines[3]  <- c("30, beer, diaper, eggs")
hypotheticalreadLines[4]  <- c("40, beer, nuts, eggs, milk")
hypotheticalreadLines[5]  <- c("50, nuts, coffee, diaper, eggs, milk")

transactionHistory <- lapply(hypotheticalreadLines, function(x) str_trim(unlist(strsplit(x,split=","))))
aslong <- do.call(rbind,lapply(transactionHistory, longify))
wideform <- dcast(aslong, transaction ~ products)

freqset <- findFrequent(wideform, 3, .5)
print(freqset)
