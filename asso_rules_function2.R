#
# association_rules()
# version 2
# for loops replaced by sapply functions
#
items <- sort(c("Beer", "Nuts", "Diaper", "Coffee", "Eggs", "Milk"))

transactions <- list(
    list(Tid=10, itemset = c("Beer", "Nuts", "Diaper")),
    list(Tid=20, itemset = c("Beer", "Coffee", "Diaper","Nuts")),
    list(Tid=30, itemset = c("Beer", "Diaper", "Eggs")),
    list(Tid=40, itemset = c("Beer","Nuts", "Eggs", "Milk")),
    list(Tid=50, itemset = c("Nuts", "Coffee", "Diaper", "Eggs", "Milk"))
)
rules <- data.frame(
    itemA = rep(as.character(""), (length(items)* (length(items)-1))),
    itemB = rep(as.character(""), (length(items)* (length(items)-1))),
    sup = rep(as.integer(0), (length(items)* (length(items)-1))),
    conf = rep(as.integer(0), (length(items)* (length(items)-1))),
    stringsAsFactors = F
)
rule_index <- 0

name_rule <- function(item_a, item_b) {
    if (item_a != item_b) {
        rules[rule_index, 1] <- item_a
        rules[rule_index, 2] <- item_b
        assign("rules", rules, envir = .GlobalEnv)
        assign("rule_index", rule_index + 1, envir = .GlobalEnv)
    } }
set_rule_names <- function(items) {
    assign("rule_index", 1, envir = .GlobalEnv)
    xxx <- sapply (items, function(y) sapply(items, function(x) name_rule(y,x)))
}

print_rules <- function(items, trans, minsup, minconf) {
    ntrans <- length(trans)
    xxx <- sapply(1:nrow(rules), function(i) {
        if ((rules[i,3]/ntrans >= minsup) && (rules[i,4]/rules[i,3] >= minconf))
            cat(rules[i,1],"->",rules[i,2],
                "(",round(rules[i,3]/ntrans,2)*100,"%,",
                round(rules[i,4]/rules[i,3],2)*100,"%)\n")
    } } }

get_support <- function (items, trans.list) {
    xxx <- sapply(items, function(item) sapply(trans.list, function(trans) {
        if (item %in% unlist(trans)) {
            row_vals <- which(rules$itemA == item)
            xxx <- sapply(seq_along(row_vals), function(i) {
                rules[row_vals[i],3] <- rules[row_vals[i],3] + 1
                assign("rules", rules, envir = .GlobalEnv)
            } ) }})) }

get_confidence <- function (trans) {
    xxx <- sapply(1:nrow(rules), function(i) {
        xxx <- sapply(1:length(trans), function(j) {
            if ((rules[i,1] %in% unlist(trans[j])) && (rules[i,2] %in% unlist(trans[j])) ) {
                rules[i,4] <- rules[i,4] + 1
                assign("rules", rules, envir = .GlobalEnv)
            } })}) }
association_rules2<- function(transactions, items, minsup=0.5, minconf=0.5) {
    set_rule_names(items)
    get_support (items, transactions)
    get_confidence(transactions)
    print_rules(items, transactions, minsup, minconf)
}

> association_rules2(transactions, items, minsup=0.5, minconf=0.5) 