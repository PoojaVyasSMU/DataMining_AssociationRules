#Load for vata
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
summary(dato)
datVATA<- dato[dato$AgencyName =='VETERANS HEALTH ADMINISTRATION',]
sample_ID <- sample(1:nrow(datVATA), size = 10000)
datvs <- datVATA[sample_ID, ]
summary(datvs)
dat3vs <- datvs[,c("Age", "LOS", "Education", "SupervisoryStatus", "Pay")]
dat4vs <- datvs[,c("Age", "LOS", "Education", "SupervisoryStatus", "Pay")]
dim(dat3vs)
summary(dat3vs)
summary(dato)
head(dato$Age)
head(dato$LOS)
head(dato$Pay)
take <- complete.cases(dat3vs)
dat3vs <- dat3vs[take,]
dim(dat3vs)
summary(dat3vs)
dat3vs$SupervisoryStatus <- 8-dat3vs$SupervisoryStatus
dat3vs <- scale(dat3vs)
summary(dat3vs)
set.seed(1000)

d <- dist(dat3vs)
#optimal k
plot(dat3vs)

#set.seed(1234)
ks <- 2:10

#within sum of squares - error

WSS <- sapply(ks, FUN=function(k) {
  kmeans(dat3vs, centers=k, nstart=5)$tot.withinss
})
plot(ks, WSS, type="l", main = 'Optimal K for VATA')
abline(v=4, col="red", lty=2)

# average width
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(dat3vs, centers=k, nstart=5)$cluster)$avg.silwidth
})

plot(ks, ASW, type="l", main='ASW for VATA')

ks[which.max(ASW)]
abline(v=ks[which.max(ASW)], col="red", lty=2)

#kmeans

k <- 4
km <- kmeans(dat3vs, centers = k)
str(km)
set.seed(1000)

km

km$centers

def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(t(1:4)) # 4 plots in one
for(i in 1:4) barplot(km$centers[i,], ylim=c(-2,2), main=paste("Cluster", i))

library("GGally")
ggparcoord(km$centers)

ggparcoord(cbind(data.frame(km$centers), data.frame(id = as.character(1:k))), 
           columns = 1:ncol(km$centers), groupColumn = 'id')

#pca

pc <- prcomp(scale(dat3vs))
biplot(pc, col = c("grey", "red"))

#originnal observation for cluster
dat_used <- dato[take,]
dat_cluster_1 <- dat_used[km$cluster == 1,]
dat_cluster_2 <- dat_used[km$cluster == 2,]
dat_cluster_3 <- dat_used[km$cluster == 3,]
dat_cluster_4 <- dat_used[km$cluster == 4,]

summary(dat_cluster_1[,c("Age", "Education", "LOS", "Agency", "Pay")])
summary(dat_cluster_2[,c("Age", "Education", "LOS", "Agency", "Pay")])
summary(dat_cluster_3[,c("Age", "Education", "LOS", "Agency", "Pay")])

head(sort(table(dat_cluster_1$Pay)/nrow(dat_cluster_1), decreasing = TRUE))
head(sort(table(dat_cluster_2$Pay)/nrow(dat_cluster_2), decreasing = TRUE))
head(sort(table(dat_cluster_3$Pay)/nrow(dat_cluster_3), decreasing = TRUE))
head(sort(table(dat_cluster_4$Pay)/nrow(dat_cluster_4), decreasing = TRUE))

head(sort(table(dat_cluster_1$StationName)/nrow(dat_cluster_1), decreasing = TRUE))
head(sort(table(dat_cluster_2$StationName)/nrow(dat_cluster_2), decreasing = TRUE))
head(sort(table(dat_cluster_3$StationName)/nrow(dat_cluster_3), decreasing = TRUE))
head(sort(table(dat_cluster_4$StationName)/nrow(dat_cluster_4), decreasing = TRUE))


lift_cluster_1 <- sort((table(dat_cluster_1$StationName)/nrow(dat_cluster_1)) /
                         (table(dat_used$StationName)/nrow(dat_used)), decreasing = TRUE)
head(lift_cluster_1, n = 10)
tail(lift_cluster_1, n = 10)



lift_cluster_2 <- sort((table(dat_cluster_2$StationName)/nrow(dat_cluster_2)) /
                         (table(dat_used$StationName)/nrow(dat_used)), decreasing = TRUE)
head(lift_cluster_2, n = 10)
tail(lift_cluster_2, n = 10)


lift_cluster_3 <- sort((table(dat_cluster_3$StationName)/nrow(dat_cluster_3)) /
                         (table(dat_used$StationName)/nrow(dat_used)), decreasing = TRUE)
head(lift_cluster_3, n = 10)
tail(lift_cluster_3, n = 10)


lift_cluster_4 <- sort((table(dat_cluster_4$StationName)/nrow(dat_cluster_4)) /
                         (table(dat_used$StationName)/nrow(dat_used)), decreasing = TRUE)
head(lift_cluster_4, n = 10)
tail(lift_cluster_4, n = 10)

barplot(rev(head(lift_cluster_1, n=20)), horiz = TRUE, las = 2, xlab = "Lift")
barplot(rev(head(lift_cluster_2, n=20)), horiz = TRUE, las = 2, xlab = "Lift")
barplot(rev(head(lift_cluster_3, n=20)), horiz = TRUE, las = 2, xlab = "Lift")
barplot(rev(head(lift_cluster_4, n=20)), horiz = TRUE, las = 2, xlab = "Lift")


#hierarchy

d <- dist(dat3vs)
hc <- hclust(d, method="complete")

plot(as.dendrogram(hc), leaflab="none")
rect.hclust(hc, k=4)

plot(hc, hang=-1)

#single link cluster
hc_single <- hclust(d, method="single")
plot(hc_single)
rect.hclust(hc_single, k=4)

#internal validation
library(cluster)
windows()
plot(silhouette(km$cluster, d))

#visualize distance matrix
d <- dist(dat3vs)

library(seriation)
pimage(d)

#compare cluster qualities
fpc::cluster.stats(d, km$cluster)
cluster_complete <- cutree(hc, k=4)
fpc::cluster.stats(d, cluster_complete)


sapply(list(km=km$cluster,hc_compl=hc),
  FUN=function(x) fpc::cluster.stats(d, x))[c("within.cluster.ss","avg.silwidth"),]

#external validation didn't work
library(mlbench)

summary(dat3vs)

#prepare data
take <- complete.cases(dat4vs)
dat4vs <- dat4vs[take,]
dim(dat4vs)
truth <- dat4vs$Pay
summary(truth)
datt<- subset(dat4vs, select =c("Age","LOS","Education","SupervisoryStatus"))
summary(datt)
plot(datt)
ks <- 2:10
km <- kmeans(datt, centers=4, nstart = 10)
plot(datt, col=km$cluster)

d <- dist(datt)
hc <- hclust(d, method="single")
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, cutree(hc, k))$avg.silwidth
})
windows()
plot(ks, ASW, type="l")


entropy <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  m <- length(cluster)
  mi <- table(cluster)
  
  cnts <- split(truth, cluster)
  cnts <- sapply(cnts, FUN = function(n) table(n))
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  e <- -p * log(p, 2)
  sum(rowSums(e, na.rm = TRUE) * mi/m)
}

purity <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  m <- length(cluster)
  mi <- table(cluster)
  
  cnts <- split(truth, cluster)
  cnts <- sapply(cnts, FUN = function(n) table(n))
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  
  sum(apply(p, 1, max) * mi/m)
}

random4 <- sample(1:4, nrow(datt), replace = TRUE)

hc_4 <- cutree(hc, 4)

r <- rbind(
  kmeans = c(
    unlist(fpc::cluster.stats(d, km$cluster, truth, compareonly = TRUE)),
    entropy = entropy(km$cluster, truth),
    purity = purity(km$cluster, truth)
  ),
  hc = c(
    unlist(fpc::cluster.stats(d, hc_4, truth, compareonly = TRUE)),
    entropy = entropy(hc_4, truth),
    purity = purity(hc_4, truth)
  )
  )

r

