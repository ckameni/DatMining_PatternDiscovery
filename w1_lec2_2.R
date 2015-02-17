longify <- function(listOfPurchases){
    output <- data.frame(products=listOfPurchases[2:length(listOfPurchases)])
    output$transaction <- listOfPurchases[1]
    return(output)
}


t10 <- c("10", "b", "n", "d")
t20 <- c("20", "b", "c", "d")
t30 <- c("30", "b", "d", "e")
t40 <- c("40", "n", "e", "m")
t50 <- c("50", "n", "c", "d", "e", "m")

transactionHistory <- list(t10,t20,t30,t40,t50)

aslong <- do.call(rbind,lapply(transactionHistory, longify))
library(reshape2)
wideform <- dcast(aslong, transaction ~ products)

beerDiaper <- intersect(wideform$b, wideform$d)[!is.na(intersect(wideform$b, wideform$d))]
supportBeerDiaper <- length(beerDiaper) / length(transactionHistory)


