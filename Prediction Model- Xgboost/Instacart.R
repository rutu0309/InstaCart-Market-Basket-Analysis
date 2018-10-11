######################################Checking missings############################################################

#################aisles######
dim(aisles)
summary(is.na(aisles))


#################departments######
dim(departments)
summary(is.na(departments))

#################orders######
dim(orders)
summary(is.na(orders))

#################products######
dim(products)
summary(is.na(products))

#################orders_products_prior######
dim(order_products_prior)
summary(is.na(order_products_prior))

#################order_products_train######
dim(order_products_train)
summary(is.na(order_products_prior))

#################sample_submission######
dim(sample_submission)
summary(is.na(sample_submission))


#################How many train and test########
library(plyr)
library(ggplot2)
eval_type_no <- count(orders, "eval_set")
p<-ggplot(eval_type_no, aes(x= eval_set, y=freq)) + 
   geom_bar(stat="identity", fill = "steelblue") +
   geom_text(aes(label=freq), vjust=-0.3, size=3.5) + theme_minimal()
p


library(dplyr)
orders %>%
  group_by(eval_set) %>%
  summarise(users=n_distinct(user_id))

#####################Order###########################
count(orders, "order_number")
tail(orders)

################Pairwise Missing#####################
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(orders,2,pMiss)

# install.packages("VIM")
# library(VIM)
# aggr_plot <- aggr(orders, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
#                   labels=names(orders), cex.axis=.4, gap=3, ylab=c("Histogram of missing data","Pattern"))
 
############## Imputation of Missing values ########
orders[is.na(orders)] <- 0

################### 4 to 100 ###################
# meaning users with total no of orders = 4:100 are only in this dataset

grouped_df <- orders %>%
  group_by(user_id) %>%
  summarise(total_orders= max(order_number))

total_orders <- max(orders$order_number)

ggplot(grouped_df, aes(x = total_orders)) + geom_bar(fill="indianred2") + 
        ggtitle("Frequecy of total orders") + theme(plot.title = element_text(hjust = 0.5))

# The total number of orders are between 4-100 per customer in a decreasing trend with a spike at 100


################## Order frequency by day and hour ######################
library(ggplot2)
library(dplyr)
library(plyr)
grouped_df <- orders %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarize(total = n())                
ggplot(grouped_df, aes(order_dow, order_hour_of_day)) +geom_tile(aes(fill = total), colour = "white") + 
        scale_fill_gradient(low = "lightcyan3",high = "lightcoral") + ggtitle("Frequency of Day of week Vs Hour of day") + 
        theme(plot.title = element_text(hjust = 0.5))

################ Interval between orders ##########################
grouped_df <- orders %>% 
  group_by(days_since_prior_order) %>%
  summarise(total = n())
ggplot(grouped_df, aes(days_since_prior_order, total)) + geom_bar(stat="identity",fill = "yellowgreen") +
      ggtitle("Interval between prior orders") + theme(plot.title = element_text(hjust = 0.5))

############### Joining Datasets ###########################
## aisle and products
Total <- merge(aisles, products, by = "aisle_id")

## Total and departments
Total1 <- merge(departments,Total, by = "department_id" )

## train and prior(all in one/no mapping)
total2 <- merge(order_products_prior, order_products_train, all = TRUE)

## total2 and total1
eda_df <- merge(total2, Total1,by = "product_id")

# eda_df is the dataset which we will use for exploratory data analysis



