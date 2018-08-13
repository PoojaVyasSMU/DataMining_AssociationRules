load("C:/Users/Owner/Desktop/FAll 2017/DM/p3/2004_b.rda")
dat2 <- df
dat2$PseudoID <- NULL
dat2$Name <- NULL
dat2$Date <- NULL
dat2$Education <- as.numeric(as.character(dat2$Education))
dat2$Age <- as.numeric(substr(dat2$Age, start = 1, stop = 2))
dat2$LOS <- as.numeric(sub("-.*|\\+|<", "", dat2$LOS))

summary(dat2)

library(arules)
which(sapply(dat2, FUN = function(i) is.numeric(i)))
for(i in which(sapply(dat2, FUN = function(i) is.numeric(i))))
  dat2[[i]] <- discretize(dat2[[i]], method = "frequency")
dat2$Agency <- NULL ### we have AgencyName
dat2$Schedule <- NULL ### we have fulltime
dat2$NSFTP <- NULL ### ???
dat2$Station <- NULL## we have StationID
dat2$Degree <- NULL
dat2$StatusName <- NULL
summary(dat2)

trans <- as(dat2, "transactions")
summary(trans)

itemFrequencyPlot(trans, topN = 50)

colnames(trans)

colnames(dat2)

as(trans, "matrix")[1:3,]

rules <- apriori(trans, parameter = list(supp = .01, conf = .8))

summary(rules)

inspect(head(rules, by = "lift"))

library(arulesViz)

plot(rules, engine = "html")

plot(rules, method = "graph", engine = "html")


#Frequent
2^ncol(trans)
is <- apriori(trans, parameter=list(target="frequent"))
is
5/nrow(trans)

is <- apriori(trans, parameter=list(target="frequent", support=0.05))
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
rules <- apriori(trans, parameter=list(support=0.05, confidence=.9))
length(rules)
inspect(head(rules))
quality(head(rules))

#rules with highest lift
rules <- sort(rules, by="lift")
inspect(head(rules, n=10))

#additional interest measures:
interestMeasure(rules[1:10], measure=c("phi", "gini"),
                trans=trans)

#adding measures to rules;
quality(rules) <- cbind(quality(rules),
                        interestMeasure(rules, measure=c("phi", "gini"),
                                        trans=trans))
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


