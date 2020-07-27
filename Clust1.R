# Hierarchical Clustering
input <- read.csv(file.choose())
View(input)
crime_data <- input[-1]
View(crime_data)
normalized_data <- scale(crime_data)
View(normalized_data)
d <- dist(normalized_data,method="euclidean")
crime_clust <- hclust(d,method="complete")
crime_clust
plot(crime_clust)
plot(crime_clust,hang=-1)
cluster <- cutree(crime_clust,k=4)
rect.hclust(crime_clust, k=4, border="green")

membership<-as.matrix(cluster)

final <- data.frame(input,membership)
View(final)

final_model <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final_model)

write.csv(final_model,file="Final.csv")

getwd()


# K Means Clustering
library(plyr)
library(animation)
library(kselection)
library(NbClust)
library(factoextra)

input1 <- read.csv(file.choose())
View(input1)
mydata <- input1[-1]
View(mydata)
norm_data <- scale(mydata)
View(norm_data)
fit <- kmeans(norm_data,4)
str(fit)

#elbow curve to decide the k value
cl <- fviz_nbclust(norm_data, kmeans, method = "wss",k.max=10) +
      geom_vline(xintercept = 4, linetype = 2)+
      labs(subtitle = "Elbow method")
plot(cl)

fit <- kmeans.ani(norm_data,4)

final2<- data.frame(input, fit$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)
aggregate(mydata, by=list(fit$cluster), FUN=mean)




