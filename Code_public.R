#library(bannerCommenter)
#banner("Title:", "R code","Author: Iverson ZHOU","Date: 2019-10-22","Version 1","Topic: Customer Segementation (initial exploration)","Dataset: instacart market basket", emph = TRUE)
#install.packages("installr"); library(installr) 
#updateR() # updating R.

###########################################################################
###########################################################################
###                                                                     ###
###                                TITLE:                               ###
###                                R CODE                               ###
###                        AUTHOR: IVERSON ZHOU                         ###
###                          DATE: 2019-10-22                           ###
###                              VERSION 1                              ###
###         TOPIC: CUSTOMER SEGEMENTATION (INITIAL EXPLORATION)         ###
###                  DATASET: INSTACART MARKET BASKET                   ###
###                                                                     ###
###########################################################################
###########################################################################



#set work dir
getwd()
setwd("C:/Users/Zing/OneDrive/GitHub/R/Market Basket/instacart-market-basket-analysis/")

#library
Packages <- c("tidyverse"
,"e1071","caret","class","randomForest","RColorBrewer","scales","data.table","readr","plyr","sqldf","ggpcorrplot","ggplot2","cluster","HSAUR","fpc","openxlsx")
lapply(Packages, library, character.only = TRUE)

# import data
orders <- read_csv("./orders.csv")
prior <- read_csv("./order_products__prior.csv")
train <- read_csv("./order_products__train.csv")
products <- read_csv("./products.csv")
aisles <- read_csv("./aisles.csv")

#View(orders) View(order_products_prior) View(order_products_train)


dim(orders)
dim(aisles)
dim(prior)
dim(train)

# Explore data
head(orders,10)
head(prior,10)
head(train,10)


#key info:
#orders csv file: user_id
#Each row =1 order
#order_id=Orders
#order_number when it has been made
#muti-layer, 1 order mutiple products 
#add_to_cart_order= the sequence in which products have been + to the cart

###################################### data wrangling 

summary(prior)
prior <- prior[1:500000,]
order_prior = merge(prior,orders,by='order_id')
head(order_prior,10)

nr = merge(prior,products, by ='product_id')
nr = merge(nr,orders,by='order_id')
nr2 = merge(nr,aisles,by='aisle_id')
###################################### 



###################################### summary

DT <- data.table(nr2)
DT[, .(number_of_distinct_orders = length(unique(order_id))), by = product_name]


#Product 种类 count
ddply(nr2,~product_name,summarise,count=length(product_name))

#Best Selling Product Catergory
ddply(nr2,~aisle,summarise,count=length(aisle))

######################################


###################################### create single view
aqw <- dcast(nr2, user_id  ~ aisle)
utils::View(aqw)
dim(aqw)
#[1] 25831   135


######################################check data corrlation level
shop.cov <- cov(aqw)
shop.eigen <- eigen(shop.cov)

ggcorrplot(round(shop.cov, 2), hc.order = TRUE, type = "upper",
             outline.col = "white")
######################################


######################################PCA
aqw.dim.reduction_test<- princomp(aqw[c(2:135)])
plot(aqw.dim.reduction_test)
plot(aqw.dim.reduction_test,type='l')


aqw.dim.reduction <- prcomp(aqw[c(2:135)]
#, center = TRUE, scale = TRUE
) #Scaling data #Remove USERID

summary(aqw.dim.reduction)
print(aqw.dim.reduction)


screeplot(aqw.dim.reduction, type = "l", npcs =60, main = "Screeplot of the first 60 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#6

prop_varex <- (aqw.dim.reduction$sdev^2 / sum(aqw.dim.reduction$sdev^2))

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

cumpro <- cumsum(aqw.dim.reduction$sdev^2 / sum(aqw.dim.reduction$sdev^2))
plot(cumpro[0:135], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 80, col="blue", lty=5)
abline(h = 0.7, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

#aqw.dim.reduction$x[,1:4]

tocluster <- data.frame(aqw.dim.reduction$x[,1:6])



######################################################Elbow Method for determining optimal number of clusters
ws <- (nrow(aqw)-1)*sum(apply(aqw,2,var))
for (i in 2:15) ws[i] <- sum(kmeans(aqw,
                                     centers=i)$withinss)
plot(1:15, ws, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#set.seed(123)
## Compute and plot wss for k = 2 to k = 15.
#k.max <- 15
#data <- aqw
#wss <- sapply(1:k.max, 
#              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
#wss
#plot(1:k.max, wss,
#     type="b", pch = 19, frame = FALSE, 
#     xlab="Number of clusters K",
#     ylab="Total within-clusters sum of squares")
######################################################



###################################################### attach cluster back to signle ID view dataset

k <- kmeans(tocluster, 6, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(tocluster, col=k$clust, pch=16)

#set.seed(20)
#clusters <- kmeans(tocluster, 6)
#names(clusters)
#clusters$cluster
#aqw['cluster']=clusters$cluster
###################################################### 

# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))
k$clust
aqw['cluster']=k$clust

#dissE <- daisy(tocluster) 
#dE2   <- dissE^2
#sk2   <- silhouette(k$clust, dE2)
#plot(sk2)
plotcluster(tocluster, k$clust)
points(k$centers,col=1:8,pch=16)

#clusplot(tocluster, k$clust, color=TRUE, shade=TRUE, labels=2, lines=0)
#test=aggregate( as.matrix(aqw[,2:136]), as.list(aqw[,1:10]), FUN = mean)
#cluster_mean <- aggregate(aqw[,-1],by = list(k$clust),FUN = mean)

#mydata <- data.frame(aqw,k$cluster)
#cluster_mean <- aggregate(mydata[,-1],by = list(k$clust),FUN = mean)
#x=cluster_mean
clusteravg<- aqw[,-1] %>% 
  group_by(cluster) %>%
  summarise_all("mean")

#new <- cbind(test[,1], sapply(test[,c(2:135)], rank))
clusteravg2 <- cbind(clusteravg[,1], sapply(-clusteravg[,c(2:135)], rank#,ties.method='min'
                                     ))
write.csv(mydata, "./mydata.csv")
#write.xlsx(mydata, "./mydata.xlsx") 

long <- melt(clusteravg2[clusteravg2[,1]=='1',], id.vars = c("cluster"))
long2 <- melt(clusteravg2[clusteravg2[,1]=='2',], id.vars = c("cluster"))
long3 <- melt(clusteravg2[clusteravg2[,1]=='3',], id.vars = c("cluster"))
long4 <- melt(clusteravg2[clusteravg2[,1]=='4',], id.vars = c("cluster"))
long5 <- melt(clusteravg2[clusteravg2[,1]=='5',], id.vars = c("cluster"))
long6 <- melt(clusteravg2[clusteravg2[,1]=='6',], id.vars = c("cluster"))


##Cluster 1 looks like new born baby Household
##Cluster 2 looks like single shopper
##Cluster 3 looks like alcoho lover
##Cluster 4 looks like baking lover
##Cluster 5 looks like random shoppers（junk drawer）
##Cluster 6 looks like heavy Grocery Fresh category dominant
##to be a heavy Grocery and above average Detergents_Paper but low Fresh foods
##Cluster 3 is dominant in the Fresh category
##Cluster 5 might be either the “junk drawer” catch-all cluster or it might represent the small customers




aqw$cluster[aqw$cluster=="4"] <- "newborn"
aqw$cluster[aqw$cluster=="2"] <- "Single"
aqw$cluster[aqw$cluster=="1"] <- "Drinkers"
aqw$cluster[aqw$cluster=="3"] <- "Baking"
aqw$cluster[aqw$cluster=="6"] <- "Random Shoppers"
aqw$cluster[aqw$cluster=="5"] <- "Grocery/Fresh"


levels(as.factor(aqw$cluster))



nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
ran <- sample(1:nrow(aqw), 0.9 * nrow(aqw)) 

#aqw_norm <- as.data.frame(lapply(aqw[,c(2:135)], nor))
aqw_norm <- as.data.frame(aqw[,c(2:135)])

##extract training set
aqw_train <- aqw_norm[ran,] 
##extract testing set
aqw_test <- aqw_norm[-ran,] 
aqw_target_category <- aqw[ran,136]
aqw_test_category <- aqw[-ran,136]


##run knn function
pr <- knn(aqw_train,aqw_test,cl=aqw_target_category,k=1)

##create confusion matrix
tab <- table(pr,aqw_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)



#random forrest
set.seed(1234)
ran_2 = sample(2, nrow(aqw), replace=TRUE, prob=c(0.7,0.3))
aqw_train_2= aqw[ran_2==1,]
aqw_test_2 = aqw[ran_2==2,]



names(aqw_train_2) <- make.names(names(aqw_train_2))
str(aqw_train_2)
sapply(aqw_train_2, function(x) any(is.na(x)))
##aqw_rf = randomForest(cluster~., data=aqw_train_2, ntree=100, proximity=T)

aqw_rf = randomForest(as.factor(cluster)~., data=aqw_train_2[,c(2:136)], ntree=100, type ='class')

table(predict(aqw_rf), aqw_train_2$cluster)
tab_rf<- table(predict(aqw_rf), aqw_train_2$cluster)


plot(aqw_rf)
importance(aqw_rf)



names(aqw_test_2) <- make.names(names(aqw_test_2))
aqwPred <- predict(aqw_rf, aqw_test_2[,c(2:135)])

accuracy_rf <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy_rf(tab_rf)




names(aqw_test_2) <- make.names(names(aqw_test_2))
aqwPred <- predict(aqw_rf, aqw_test_2[,c(2:135)])
table(aqwPred, aqw_test_2$cluster)
plot(margin(aqw_rf, aqw_test_2$cluster))
CM = table(aqwPred, aqw_test_2$cluster)
accuracy = (sum(diag(CM)))/sum(CM)
confusionMatrix(aqwPred,as.factor(aqw_test_2$cluster))

