library(alr4)
library(olsrr)
library(MASS)
library(caret)

attach(dataset)

# Descriptive Statistics and Scatterplot matrix
summary(dataset)
plot(dataset,col=4)

# Initial Model fitting
testmod <- lm(ccs~cmt+bfs+flya+wtr+spl+ca+fa+age,data = dataset)
summary(testmod)
plot(testmod)

# Box cox transformation
k = boxCox(testmod)
nccs = ccs^0.75

testmod2 <- lm(nccs~cmt+bfs+flya+wtr+spl+ca+fa+age,data = dataset)
summary(testmod2)
plot(testmod2)

# box-cox transformation for ccs and log transformation for predictor variables cmt,wtr,ca,fa,age
testmod4 = lm(nccs~log(cmt)+bfs+flya+log(wtr)+spl+log(ca)+log(fa)+log(age),data = data2)
summary(testmod4)
plot(testmod4)

#WLS method
res4 = testmod4$residuals
z=log(res4^2)

z.lo=loess(z~nccs,degree = 2,span=.75)
loz=predict(z.lo)
yord=order(nccs)

plot(nccs,z)
lines(nccs[yord],loz[yord],col=2)

sig2hat=exp(loz)
sighat=sqrt(sig2hat)

testmod5 = lm(nccs~log(cmt)+bfs+flya+log(wtr)+spl+log(ca)+log(fa)+log(age),data = data2,weights = 1/sighat)
summary(testmod5)
plot(testmod5)

# WLS again (2nd time)
res5 = testmod5$residuals
z2=log(res5^2)

z.lo2=loess(z2~nccs,degree = 2,span=.75)
loz2=predict(z.lo2)
yord=order(nccs)

plot(nccs,z2)
lines(nccs[yord],loz2[yord],col=2)

sig2hat2=exp(loz2)
sighat2=sqrt(sig2hat2)

testmod6 = lm(nccs~log(cmt)+bfs+flya+log(wtr)+spl+log(ca)+log(fa)+log(age),data = data2,weights = 1/sighat2)
summary(testmod6)
plot(testmod6)

#WLS again (3rd time)
res6 = testmod6$residuals
z3=log(res6^2)

z.lo3=loess(z3~nccs,degree = 2,span=.75)
loz3=predict(z.lo3)
yord=order(nccs)

plot(nccs,z3)
lines(nccs[yord],loz3[yord],col=2)

sig2hat3=exp(loz3)
sighat3=sqrt(sig2hat3)

testmod7 = lm(nccs~log(cmt)+bfs+flya+log(wtr)+spl+log(ca)+log(fa)+log(age),data = data2,weights = 1/sighat3)
summary(testmod7)
plot(testmod7)

plot(testmod7$fitted.values,weighted.residuals(testmod7))
lines(lowess(testmod7$fitted.values,weighted.residuals(testmod7)),col=4)

summary(testmod7)

# model selection
low = lm(nccs~1,data = dataset,weights = 1/sig2hat3)
high = lm(nccs~log(cmt)+bfs+flya+log(wtr)+spl+log(ca)+log(fa)+log(age),data = dataset,weights = 1/sighat3)

k = ols_step_forward_p(high);k;k$model # forward selection
summary(k$model)
b=ols_step_backward_p(high);b;b$model # backward selection
summary(b$model)
s= ols_step_both_p(high);s;s$model # stepwise
bs=ols_step_best_subset(high);bs # best subset selection
summary(s$model)

# comparing  AIC and Mallows Cp Values of the two models
AIC(k$model)
AIC(s$model)

k$mallows_cp
s$mallows_cp

# Model Validation
train.control=trainControl(method="cv",number=10)

modval1 <- train(nccs~log(cmt)+bfs+flya+log(wtr)+log(ca)+log(fa)+log(age),data = dataset,weights = 1/sighat3 , method = "lm",
                 trControl = train.control)
print(modval1)
modval1$results
summary(modval1$finalModel)