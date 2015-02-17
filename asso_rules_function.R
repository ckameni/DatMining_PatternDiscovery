    items <- factor(sort(c("Beer", "Nuts", "Diaper", "Coffee", "Eggs", "Milk")))
    
    transactions <- list(
        list(Tid=10, itemset = c("Beer", "Nuts", "Diaper")),
        list(Tid=20, itemset = c("Beer", "Coffee", "Diaper","Nuts")),
        list(Tid=30, itemset = c("Beer", "Diaper", "Eggs")),
        list(Tid=40, itemset = c("Beer","Nuts", "Eggs", "Milk")),
        list(Tid=50, itemset = c("Nuts", "Coffee", "Diaper", "Eggs", "Milk"))
    )
    
    association_rules <- function(trans.list, minsup=0.5, minconf=0.5) {
        for (i in 1:length(items)) {
            supsum <- 0
            for(k in 1:length(trans.list)) if(items[i] %in% trans.list[[k]]$itemset) supsum<-supsum+1
            for (j in 1:length(items)) {
                if (i == j) next
                confsum <- 0
                for(k in 1:length(trans.list))
                    if(items[i] %in% transactions[[k]]$itemset && items[j] %in% transactions[[k]]$itemset)
                        confsum<-confsum+1
                if (supsum > 0) {
                    sup <- supsum/length(trans.list)
                    conf <- confsum/supsum
                    if (sup >= minsup)
                        if (conf >= minconf)
                            cat(levels(items)[i],"->",levels(items)[j],"(",round(sup,2)*100,"%,",round(conf,2)*100,"%)\n")
                }
            }
        }
    }
    
    association_rules(transactions, minsup=0.5, minconf=0.5) 