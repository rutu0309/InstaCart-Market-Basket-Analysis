install.packages("mice")
install.packages("VIM")
library(mice)
library(VIM)

##########Summary of orders dataset########
summary(is.na(orders))

###########Na pattern##############
md.pattern(orders)

##########Percentage of Missing#########
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(orders,2,pMiss)
#Graphical presentation
aggr_plot <- aggr(orders, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(orders), cex.axis=.5, gap=3, ylab=c("Histogram of missing data","Pattern"))

############Missing value evalution###########
orders[!complete.cases(orders),]

#Missing Value Imputation
orders[is.na(orders)] <- 0


## read in data ##
setwd('C:/Users/Karthik/Downloads/ISDS577_Dataset') # specify the path where data file is placed
orders = read.csv('orders.csv', head=T, stringsAsFactors=F, na.strings='') # StringAsFactor=F, strings wil not be considered as factor; na.strings specifies missing values.
View(orders)
dim(orders)

# check missing with for loop
for (ii in 1:ncol(orders)) {
  print( colnames(orders)[ii] ) # print the column names
  print( table(is.na(orders[,ii])) ) # print the number of columns
}

#Check Missing Values for all variables
for (Var in names(orders)) {
  missing <- sum(is.na(orders[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

#Function to imputate values to 0
na.zero <- function (x) {
  x[is.na(x)] <- 0
}


#imputate null values in orders table to 0
orders[is.na(orders)] <- 0

#Check null values in particular column of order table
table(is.na(orders['days_since_prior_order']))

