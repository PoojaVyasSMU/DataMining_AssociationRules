load("C:/Users/Owner/Desktop/FAll 2017/DM/p3/2012_o.rda")
dato <- df
dato$PseudoID <- NULL
dato$Name <- NULL
dato$Date <- NULL
dato$Education <- as.numeric(as.character(dato$Education))
dato$Age <- as.numeric(substr(dato$Age, start = 1, stop = 2))
dato$LOS <- as.numeric(sub("-.*|\\+|<", "", dato$LOS))

summary(dato)

library(arules)
which(sapply(dato, FUN = function(i) is.numeric(i)))
for(i in which(sapply(dato, FUN = function(i) is.numeric(i))))
  dato[[i]] <- discretize(dato[[i]], method = "frequency")
dato$Agency <- NULL ### we have AgencyName
dato$Schedule <- NULL ### we have fulltime
dato$NSFTP <- NULL ### ???
dato$Station <- NULL## we have StationID
dato$Degree <- NULL
dato$StatusName <- NULL

summary(dato)

transo <- as(dato, "transactions")
summary(transo)

itemFrequencyPlot(transo, topN = 50)

colnames(transo)

colnames(dato)

ruleso <- apriori(transo, parameter = list(supp = .01, conf = .8))

summary(ruleso)

inspect(head(ruleso, by = "lift"))

library(arulesViz)

plot(ruleso, engine = "html")

plot(ruleso, method = "graph", engine = "html")


#Frequent
2^ncol(transo)
is <- apriori(transo, parameter=list(target="frequent"))
is
5/nrow(transo)

is <- apriori(transo, parameter=list(target="frequent", support=0.05))
is
is <- sort(is, by="support")
inspect(head(is, n=10))

#maximal frequent itemsets

is_max <- is[is.maximal(is)]
inspect(head(sort(is_max, by="support")))

#closed frequent itemsets
is_closed <- is[is.closed(is)]
inspect(head(sort(is_closed, by="support")))

#Mine association rules:
ruleso <- apriori(transo, parameter=list(support=0.05, confidence=.9))
length(ruleso)
inspect(head(ruleso))
quality(head(ruleso))

#rules with highest lift
ruleso <- sort(ruleso, by="lift")
inspect(head(ruleso, n=10))

#additional interest measures:
interestMeasure(ruleso[1:10], measure=c("phi", "gini"),
                transo=transo)

#adding measures to rules;
quality(ruleso) <- cbind(quality(ruleso),
                        interestMeasure(ruleso, measure=c("phi", "gini"),
                                        transo=transo))
# high Phi correlation score:
inspect(head(rules, by="phi"))

#mining using templates:
type <- grep("type=", itemLabels(trans), value = TRUE)
type

barplot(c(
  frequent=length(is),
  closed=length(is_closed),
  maximal=length(is_max)
), ylab="count", xlab="itemsets")

inspectDT(ruleso)

summary(dat2)

dat_set1 <- dat2[dat2$AgencyName == "VETERANS HEALTH ADMINISTRATION",]
dat_set2 <- dat2[dat2$AgencyName == "INTERNAL REVENUE SERVICE",]

dat_o_b <- rbind(dat2, dato) #combine

summary(dat_o_b)

#discretize
dat_o_b$Pay <- discretize(dat_o_b$Pay, method = "frequency")
dat_o_b$LOS <- discretize(dat_o_b$LOS, method = "frequency")
dat_o_b$Education <- discretize(dat_o_b$Education, method = "frequency")
dat_o_b$Age <- discretize(dat_o_b$Age, method = "frequency")

#split
dat_o <- dat_1_2[dat_1_2$AgencyName == "VETERANS HEALTH ADMINISTRATION",]
dat_b <- dat_1_2[dat_1_2$AgencyName == "INTERNAL REVENUE SERVICE",]

nrow(dat_set2)
nrow(dat_set1)

colnames(dat_set1)[12]

summary(dat_set1)

dat_set1$StationName <- NULL
dat_set2$StationName <- NULL
transo <- as(dato, "transactions")
trans2 <- as(dat_set2, "transactions")

rulestry <- apriori(trans2, parameter=list
                    (support=0.05, confidence=.8,minlen=3, maxlen=20))

ruleso <- apriori(transo)
rules2 <- apriori(trans2)

inspect(head(rules1, by = "lift", n=5))
inspect(head(rules2, by = "lift", n=5))

m <- match(rules, ruleso)
sum(!is.na(m))/min(length(rules),length(ruleso))

r <- sample(rules, 100)

q <- interestMeasure(r, measure = c("supp", "confidence", "lift"),
                     transactions = transo, reuse = FALSE)

diff <- (quality(r)[,-4] - q)/quality(r)[,-4]
diff

inspect(r[which(diff$supp > 0.2 & diff$supp!=1)])

inspect(r[which(diff$supp < -0.1)])

inspect(r[which(diff$lift > 0.1)])

inspect(r[which(diff$lift < -0.1)])

ruleso <- apriori(transo, parameter=list
                    (support=0.05, confidence=.8,minlen=3, maxlen=20))

inspectDT(ruleso)

