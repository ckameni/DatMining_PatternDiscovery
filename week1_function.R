source('common.R')
df <- data.frame(Tid=c(10,20,30,40,50), B=c(1,1,1,1,0), N=c(1,1,0,1,1),
                 D=c(1,1,1,0,1), C=c(0,1,0,0,1), E=c(0,0,1,1,1), M=c(0,0,0,1,1))
df
sup_rules(df,confmin=0.75)
sup_rules(df,confmin=0.7499999)
sup_rules(df,confmin=0.7499999,supmin=0.5)
count_sup_rules(df,confmin=0.7499999,supmin=0.5)


#=========================================================
 df <- data.frame(Tid=c(10,20,30,40,50), B=c(1,1,1,1,0), N=c(1,1,0,1,1), D=c(1,1,1,0,1), C=c(0,1,0,0,1), E=c(0,0,1,1,1), M=c(0,0,0,1,1))
 sup_rules(df, supmin=0.4999, confmin=0.4999)
count_sup_rules (df, supmin=0.5, confmin=0.6)

sup_rules <- function(transactions, supmin=0.0, confmin=0.0)
{
    df <- transactions
    distinctitems = setdiff(colnames(df), 'Tid')
    p = combn(distinctitems, 2)
    d = as.data.frame(t(p))
    d2 = data.frame(V1=d$V2, V2=d$V1)
    r = rbind(d,d2)
    r$V1 <- as.character(r$V1)
    r$V2 <- as.character(r$V2)
    fsx <- function(x) dim(df[ df[, x]==1,])[1] / dim(df)[1]
    r$sx = sapply(r$V1, fsx)
    fs <- function(x,y) dim(df[ df[,x]==1 & df[,y]==1, ])[1] / dim(df)[1]
    r$s <- mapply(fs, r$V1, r$V2)
    r$c <- r$s / r$sx
    r[ r[,'s']>=supmin & r[,'c']>=confmin, ]
}

count_sup_rules <- function(transactions, supmin=0.0, confmin=0.0)
{
    x = sup_rules(transactions, supmin, confmin)
    dim(x)[1]
}

#=================================================================
x_union_y <- function(transactions, items)
{
    df <- transactions
    for (i in 1:length(items))
    {
        colname <- items[i]
        rowcontainsitem <- df[,colname]==1
        df <- df[ rowcontainsitem, ]
    }
    df[,'Tid']
}


# Works with any number of items.
# In data mining, X union Y means find transactions containing BOTH items X and Y.
# In data mining, X union Y does NOT mean find transactions that contain either item X or Y.
# Usage example:
 source('common.R')
 df <- data.frame(Tid=c(10,20,30,40,50), B=c(1,1,1,0,0), N=c(1,0,0,1,1), D=c(1,1,1,0,1), C=c(0,1,0,0,1), E=c(0,0,1,1,1), M=c(0,0,0,1,1))
 x_union_y(df, items=c('B','D'))

 x_union_y(df, items=c('B','D','E'))

 x_union_y(df, items=c('B','D','E','M'))

 x_union_y(df, items=c('B'))
 x_union_y(df, items=c('D'))

#################################
#======================================

#Here is an example where you can see how to remove explicit loops out of an R program.

old_x_union_y <- function(transactions, items){
    df <- transactions
    for (i in 1:length(items)){
        colname <- items[i]
        rowcontainsitem <- df[,colname]==1
        df <- df[ rowcontainsitem, ]
    }
    df[,'Tid']
}

old2_x_union_y <- function(transactions, items){
    df <- transactions
    f <- function(colname) {
        rowcontainsitem <- df[,colname]==1
        df <- df[ rowcontainsitem, ]
    }
    mapply(f, items)
    df[,'Tid']
}

x_union_y <- function(df, items){
    mapply(function(colname) {rowcontainsitem <- df[,colname]==1; df <- df[ rowcontainsitem, ]}, items)
    df[,'Tid']
}  