set.seed(1234)
#train and test data
births <- read.csv("better2000births.csv")
data <- births
finaldata <- na.omit(data)
sample_size <- floor(0.50*nrow(finaldata))
index <- sample(seq_len(nrow(finaldata)),size = sample_size)
cv_train <- finaldata[index,]
cv_test <- finaldata[-index,]
summary(cv_test$Premie)

thetree2 <- tree(Premie ~ ., data = cv_train)
plot(thetree2)
text(thetree2, pretty = 0)

result1 <- thetree(thetree2, cv_test)
knitr::kable(result1$tab)

cross_prune <- cv.tree(thetree2, FUN = prune.misclass, K = nrow(cv_train))
tab2 <- data.frame(size = cross_prune$size, error_rate = cross_prune$dev)
knitr::kable(tab2)
