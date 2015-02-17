R code with arules

#input.csv
#Beer,Diaper,Eggs
#Beer,Coffee,Diaper,Nuts
#Beer,Nuts,Diaper
#Beer,Nuts,Eggs,Milk
#Nuts,Coffee,Diaper,Eggs,Milk

require(arules)
transactions = read.transactions(file = file("input.csv"), format = "basket", sep = ",")
rules = apriori(transactions, parameter = list(minlen=1, sup = 0.5, conf = 0.5))
WRITE(rules, file = "", sep = ",", quote = TRUE, col.names = NA)