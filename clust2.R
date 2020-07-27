# Hierarchial Clustering Method 
getwd()
library(readxl)
raw_data <- read_excel("EastWestAirlines.xlsx",sheet=2)
View(raw_data)
my_data <- raw_data[-1]
View(my_data)
# Translating categorical data to Numeric data
my_data$cc1_miles = ifelse(my_data$cc1_miles==1,2500,
                              ifelse(my_data$cc1_miles==2,7500,
                                     ifelse(my_data$cc1_miles==3,17500,
                                            ifelse(my_data$cc1_miles==4,32500,
                                                   ifelse(my_data$cc1_miles==5,50000,0)))))

my_data$cc2_miles = ifelse(my_data$cc2_miles==1,2500,
                              ifelse(my_data$cc2_miles==2,7500,
                                     ifelse(my_data$cc2_miles==3,17500,
                                            ifelse(my_data$cc2_miles==4,32500,
                                                   ifelse(my_data$cc2_miles==5,50000,0)))))

my_data$cc3_miles = ifelse(my_data$cc3_miles==1,2500,
                              ifelse(my_data$cc3_miles==2,7500,
                                     ifelse(my_data$cc3_miles==3,17500,
                                            ifelse(my_data$cc3_miles==4,32500,
                                                   ifelse(my_data$cc3_miles==5,50000,0)))))
View(my_data)
# Normalization
x <- scale(my_data)
View(x)

hclustfunc <- function(x) hclust(x, method="complete")
distfunc <- function(x) as.dist((1-cor(t(x)))/2)
d <- distfunc(x)

fit <- hclustfunc(d)
str(fit)
plot(fit)
plot(fit,hang=-1)

airline <- cutree(fit,k=4)
rect.hclust(fit,k=4,border="blue")
table(airline)
membership<-as.matrix(airline)

final <- data.frame(raw_data,membership)
View(final)

final_model <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final_model)
g1 = aggregate(my_data,list(airline),median)
data.frame(Cluster=g1[,1],Freq=as.vector(table(airline)),g1[,-1])

# K Means Clustering
library(readxl)
library(kselection)
library(animation)
library(NbClust)
library(factoextra)
air_data <- read_excel("EastWestAirlines.xlsx",sheet=2)
View(air_data)
Estair_data <- air_data[-1]
View(Estair_data)
# Translating categorical data to Numeric data
Estair_data$cc1_miles = ifelse(Estair_data$cc1_miles==1,2500,
                           ifelse(Estair_data$cc1_miles==2,7500,
                                  ifelse(Estair_data$cc1_miles==3,17500,
                                         ifelse(Estair_data$cc1_miles==4,32500,
                                                ifelse(Estair_data$cc1_miles==5,50000,0)))))

Estair_data$cc2_miles = ifelse(Estair_data$cc2_miles==1,2500,
                           ifelse(Estair_data$cc2_miles==2,7500,
                                  ifelse(Estair_data$cc2_miles==3,17500,
                                         ifelse(Estair_data$cc2_miles==4,32500,
                                                ifelse(Estair_data$cc2_miles==5,50000,0)))))

Estair_data$cc3_miles = ifelse(Estair_data$cc3_miles==1,2500,
                           ifelse(Estair_data$cc3_miles==2,7500,
                                  ifelse(Estair_data$cc3_miles==3,17500,
                                         ifelse(Estair_data$cc3_miles==4,32500,
                                                ifelse(Estair_data$cc3_miles==5,50000,0)))))
# Normalization
nor_data <- scale(Estair_data)
View(nor_data)
airline_cl <- kmeans(nor_data,5)
str(airline_cl)
# Elbow curve to decide the k value
air_cl <- fviz_nbclust(nor_data, kmeans, method = "wss",k.max=8) +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
plot(air_cl)
airline_cl <- kmeans.ani(nor_data,5)

final2<- data.frame(air_data, airline_cl$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)

aggregate(Estair_data, by=list(airline_cl$cluster), FUN=mean)


