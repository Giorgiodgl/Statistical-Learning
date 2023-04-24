####Importing dataset####
library(tidyverse)
library(corrplot)
library(factoextra)
library(cluster)

####Importing dataset & preprocessign####
data <- read.table('C:\\Users\\giorg\\Desktop\\google_review_ratings.csv',sep  = ',', header = TRUE)

data_clean <- data[,-1]

data_norm <- scale(data_clean)

summary(data)

head(data)

dim(data)

plt_hist <- function(data) {
  for(i in data) {
    if (is.numeric(i) == TRUE) {
      hist(i)
    }
  }
}

plt_hist(data)

data %>% glimpse()

data_clean %>% 
  DataExplorer::create_report()

corrmatrix <- cor(data_clean)
corrplot(corrmatrix, method = 'shade')

####K-means####

fviz_nbclust(data_clean, kmeans, method='silhouette')
fviz_nbclust(data_clean, kmeans, method='wss')
#no clear division so we can try to analize with 5 and 9

set.seed(42)
fit <- kmeans(data_clean, 9, nstart = 20) # k = 5
fviz_cluster(fit,data_clean)

clusplot(data[,-1], fit$cluster, 
         main='2D representation of the 5 Cluster',
         color=TRUE, shade=TRUE,
         labels=5, lines=0)

centers <- fit$centers
rownames(centers) <- c('Cluster_1','Cluster_2','Cluster_3','Cluster_4','Cluster_5')
centers <- as.data.frame(centers)


clt1 <- as.data.frame(centers[1, , drop = FALSE])
barplot(unlist(clt1),col = rgb(0.8,0.1,0.2,0.9))


clt2 <- as.data.frame(centers[2, , drop = FALSE])
barplot(unlist(clt2),col = rgb(0.1,0.2,0.8,0.9))


clt3 <- as.data.frame(centers[3, , drop = FALSE])
barplot(unlist(clt3),col = rgb(0.2,0.8,0.1,0.9))


clt4 <- as.data.frame(centers[4, , drop = FALSE])
barplot(unlist(clt4),col = rgb(0.3,0.3,0.3,0.9))


clt5 <- as.data.frame(centers[5, , drop = FALSE])
barplot(unlist(clt5),col = rgb(0.8,0.7,0.8,0.9))


####Hierarchical Clustering####

#Ward distance

d_euc <- dist(data_clean, method = "euclidean") # Euclidean distance matrix.
hclust_ward <- hclust(d_euc, method="ward.D2")

cut_ward <- cutree(hclust_ward, k = 5)
plot(hclust_ward)
rect.hclust(hclust_ward , k = 5)

data_cl_w <- mutate(data_clean, cluster = cut_ward)
count(data_cl_w,cluster)

#Average

hclust_avg <- hclust(d_euc, method="average")

cut_avg <- cutree(hclust_avg, k = 3)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 3)

data_cl_a <- mutate(data_clean, cluster = cut_avg)
count(data_cl_a,cluster)

##manhattan distance average method

d_man <- dist(data_clean, method = "manhattan")

hclust_avg <- hclust(d_man, method="average")

cut_avg <- cutree(hclust_avg, k = 4)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 4)

data_cl_a <- mutate(data_clean, cluster = cut_avg)
count(data_cl_a,cluster)
##same prob

##ward dist with manhattan distance

hclust_ward_man <- hclust(d_man, method="ward.D2")

cut_ward_man <- cutree(hclust_ward_man, k = 5)
plot(hclust_ward_man)
rect.hclust(hclust_ward_man , k = 5)

data_cl_w_m <- mutate(data_clean, cluster = cut_ward_man)
count(data_cl_w_m,cluster)

####we notice the most balanced one is the one with euclidian dist and ward method

mean_h <- aggregate(data_cl_w[1:24], list(data_cl_w$cluster), FUN=mean)
rownames(mean_h) <- c('Cluster_1','Cluster_2','Cluster_3','Cluster_4','Cluster_5')
mean_h <- mean_h[,-1]

clt1_h <- as.data.frame(mean_h[1, , drop = FALSE])
barplot(unlist(clt1_h),col = rgb(0.8,0.1,0.2,0.9))


clt2_h <- as.data.frame(mean_h[2, , drop = FALSE])
barplot(unlist(clt2_h),col = rgb(0.1,0.2,0.8,0.9))


clt3_h <- as.data.frame(mean_h[3, , drop = FALSE])
barplot(unlist(clt3_h),col = rgb(0.2,0.8,0.1,0.9))


clt4_h <- as.data.frame(mean_h[4, , drop = FALSE])
barplot(unlist(clt4_h),col = rgb(0.3,0.3,0.3,0.9))


clt5_h <- as.data.frame(mean_h[5, , drop = FALSE])
barplot(unlist(clt5_h),col = rgb(0.8,0.7,0.8,0.9))
