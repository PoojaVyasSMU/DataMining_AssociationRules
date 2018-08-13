getwd()
setwd("~/Fall'17/Data mining/Project4/")
load("2012_o.rda")
summary(df)
dato<-df
summary(dato)
dato$PseudoID <- NULL
dato$Name <- NULL
dato$Date <- NULL
dato$Education <- as.numeric(as.character(dato$Education))
dato$Age <- as.numeric(substr(dato$Age, start = 1, stop = 2))
dato$LOS <- as.numeric(sub("-.*|\\+|<", "", dato$LOS))
dato$SupervisoryStatus <- as.numeric(as.character(dato$SupervisoryStatus))
dato$Pay <- as.numeric(dato$Pay)
dat3 <- dato[,c("Age", "LOS", "Education", "SupervisoryStatus", "Pay")]
dim(dat3)
summary(dat3)
take <- complete.cases(dat3)

dat3 <- dat3[take,]
dim(dat3)
summary(dat3)
dat3$SupervisoryStatus <- 8-dat3$SupervisoryStatus
dat3 <- scale(dat3)
summary(dat3)
set.seed(1000)

k <- 3
km <- kmeans(dat3, centers = k)
str(km)
set.seed(1000)


km$centers

library("GGally")
ggparcoord(km$centers)

ggparcoord(cbind(data.frame(km$centers), data.frame(id = as.character(1:k))), 
           columns = 1:ncol(km$centers), groupColumn = 'id')

dat_used <- dato[take,]
dat_cluster_1 <- dat_used[km$cluster == 1,]
dat_cluster_2 <- dat_used[km$cluster == 2,]
dat_cluster_3 <- dat_used[km$cluster == 3,]

summary(dat_cluster_1[,c("Age", "Education", "LOS", "Agency", "Pay")])

summary(dat_cluster_2[,c("Age", "Education", "LOS", "Agency", "Pay")])

summary(dat_cluster_3[,c("Age", "Education", "LOS", "Agency", "Pay")])

#where people in different clusters work
head(sort(table(dat_cluster_1$Agency)/nrow(dat_cluster_1), decreasing = TRUE))

head(sort(table(dat_cluster_2$Agency)/nrow(dat_cluster_2), decreasing = TRUE))

head(sort(table(dat_cluster_3$Agency)/nrow(dat_cluster_3), decreasing = TRUE))


lift_cluster_1 <- sort((table(dat_cluster_1$Agency)/nrow(dat_cluster_1)) /
                         (table(dat_used$Agency)/nrow(dat_used)), 
                       decreasing = TRUE)
head(lift_cluster_1, n = 10)

tail(lift_cluster_1, n = 10)

barplot(rev(head(lift_cluster_1, n=20)), horiz = TRUE, las = 2, xlab = "Lift")

lift_cluster_2 <- sort((table(dat_cluster_2$Agency)/nrow(dat_cluster_2)) /
                         (table(dat_used$Agency)/nrow(dat_used)),
                       decreasing = TRUE)
head(lift_cluster_2, n = 10)

tail(lift_cluster_2, n = 10)

barplot(rev(head(lift_cluster_2, n=20)), horiz = TRUE, las = 2, xlab = "Lift")

lift_cluster_3 <- sort((table(dat_cluster_3$Agency)/nrow(dat_cluster_3)) /
                         (table(dat_used$Agency)/nrow(dat_used)), 
                       decreasing = TRUE)
head(lift_cluster_3, n = 10)

tail(lift_cluster_3, n = 10)

barplot(rev(head(lift_cluster_3, n=20)), horiz = TRUE, las = 2, xlab = "Lift")

#cluster agencies

#error- should add fulltime and seasonal to orignal dataframe
agency_data <-
  aggregate(cbind(Age, Education, Grade, 
                  LOS, Pay, SupervisoryStatus)
            ~ Agency, data = dat_used, FUN = mean)
head(agency_data)

#adding column agency size
agency_size <- as.data.frame(table(dat_used$Agency))
colnames(agency_size) <- c("Agency", "size")
head(agency_size)
summary(agency_size)
agency_data <- merge(agency_data, agency_size)
head(agency_data)

rownames(agency_data) <- agency_data$Agency
agency_data <- agency_data[,-1]

d <- dist(scale(agency_data))
cl <- hclust(d)
plot(cl)

#large agencies only
agency_large <- subset(agency_data, subset = size > 5000)

d <- dist(scale(agency_large))
cl <- hclust(d)
plot(cl)

#ignoring size
d <- dist(scale(agency_large[,colnames(agency_large) != "size"]))
cl <- hclust(d)

plot(cl)

#look at structure of data using pca

pc <- prcomp(scale(agency_large[,colnames(agency_large) != "size"]))
biplot(pc, col = c("grey", "red"))

#optimal number of clusters for k means

plot(dat3)

set.seed(1234)
ks <- 2:30

#within sum of squares - error

WSS <- sapply(ks, FUN=function(k) {
  kmeans(dat3, centers=k, nstart=5)$tot.withinss
})
plot(ks, WSS, type="l")
abline(v=4, col="red", lty=2)

# average width
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(dat3, centers=k, nstart=5)$cluster)$avg.silwidth
})
plot(ks, ASW, type="l")

ks[which.max(ASW)]


