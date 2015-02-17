
http://www.ling.upenn.edu/~joseff/rstudy/week4.html
#install.packages( "arules" )
library(arules)



zero_or_one <- function (n, weight){
    nmbrs <- runif(n);nmbrs
    output <- integer(n);output
    output[nmbrs > weight] <- 0;output
    output[nmbrs <= weight] <- 1;output
    return(output)
}


sz= 30
set.seed(1101011)
shopping_history <- data.frame(apple=zero_or_one(sz, .7),
                               banana=zero_or_one(sz, .9), 
                               kiwifruit=zero_or_one(sz,.5),
                               pear=zero_or_one(sz, .6), 
                               orange=zero_or_one(sz, .4), 
                               durian=zero_or_one(sz,.2))



sz= 30
set.seed(1101011)
shopping_history_2 <- data.frame(apple=rbinom(sz, 1, .7), 
                                 banana=rbinom(sz, 1, .9), 
                                 kiwifruit=rbinom(sz, 1, .5), 
                                 pear=rbinom(sz, 1, .6), 
                                 orange=rbinom(sz, 1, .4), 
                                 durian=rbinom(sz, 1, .2))
shopping_history_2 


rules<-apriori(as.matrix(shopping_history),parameter=list(support=0.01,confidence=0.0))
inspect(rules)
str(rules)

#For those of you who want to use data sets built into R..

#install.packages("BayesLCA")
#install.packages("arules")
#library(arules)
library(BayesLCA)


data(Alzheimer)
rules<-apriori(as.matrix(Alzheimer),parameter=list(support=0.01,confidence=0.0))
inspect(rules)
str(rules)
# or..  

data(Groceries)
rules <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01))
inspect(rules)

# if you want to order the rules..  
rules <- sort(rules,by="support")
inspect(rules)




#to convert the rules output into a dataframe for easier export,
rules.dataframe = data.frame(
    lhs = labels(lhs(rules))$elements,
    rhs = labels(rhs(rules))$elements, 
    rules@quality)

head(rules.dataframe)
# order by descending support
rules <- sort(rules, decreasing=T, by="support")
head(rules.dataframe)



install.packages("arulesViz")
library(arulesViz)
#open the arulesViz vignette
vignette("arulesViz")

#Glad to hear about the arules package,  so I don't have to write any more code like this!

items <- factor(sort(c("Beer", "Nuts", "Diaper", "Coffee", "Eggs", "Milk")))

transactions <- list(
    list(Tid=10, itemset=factor(c("Beer", "Nuts",   "Diaper"),                 levels(items))),
    list(Tid=20, itemset=factor(c("Beer", "Coffee", "Diaper"),                 levels(items))),
    list(Tid=30, itemset=factor(c("Beer", "Diaper", "Eggs"),                   levels(items))),
    list(Tid=40, itemset=factor(c("Nuts", "Eggs",   "Milk"),                   levels(items))),
    list(Tid=50, itemset=factor(c("Nuts", "Coffee", "Diaper", "Eggs", "Milk"), levels(items)))
)

association_rules <- function(trans.list, minsup=0.5, minconf=0.5) {
    for (i in 1:length(items)) {   
        supsum  <- 0
        for(k in 1:length(trans.list)) if(items[i] %in% trans.list[[k]]$itemset) supsum<-supsum+1
        for (j in 1:length(items)) {
            if (i == j) next  
            confsum <- 0
            for(k in 1:length(trans.list)) 
                if(items[i] %in% transactions[[k]]$itemset && items[j] %in% transactions[[k]]$itemset) 
                    confsum<-confsum+1 
            if (supsum > 0) {
                sup  <- supsum/length(trans.list)
                conf <- confsum/supsum
                if (sup >= minsup)
                    if (conf >= minconf)
                        cat(levels(items)[i],"->",levels(items)[j],"(",round(sup,2)*100,"%,",round(conf,2)*100,"%)\n")        
            }
        }  
    }
} 


association_rules(transactions, minsup=0.5, minconf=0.5)

