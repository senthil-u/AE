names(loandata)
load("C:/Users/nexus/Documents/data/loandata1.rda")
shapiro.test(loandata$interest.rate)
tapply(loandata$interest.rate, loandata$loan.length, median, na.rm=TRUE)
wilcox.test(interest.rate ~ loan.length, alternative="two.sided", data=loandata)
.Z <- scale(loandata[,c("amount.funded.by.investors","amount.requested")])
loandata$Z.amount.funded.by.investors <- .Z[,1]
loandata$Z.amount.requested <- .Z[,2]
remove(.Z)
.PC <- princomp(~Z.amount.funded.by.investors+Z.amount.requested, cor=TRUE, data=loandata)
unclass(loadings(.PC))  # component loadings
.PC$sd^2  # component variances
summary(.PC) # proportions of variance
screeplot(.PC)
loandata$PC1 <- .PC$scores[,1]
remove(.PC)
class(loandata$loan.length)
LinearModel.1 <- lm(interest.rate ~ fico.high +loan.length +PC1, data=loandata)
summary(LinearModel.1)

