library("ROCR")
library("nnet")
library("ResourceSelection")


data = read.csv("datasets/CardiacDataAdj.csv",header = T)
data.continous = keep(data, function(x) {!(all(x %in% c(0,1,0.5)))})
data.categorical = keep(data, function(x) {all(x %in% (c(0,1,1.5)))})
for (i in c(14,17:31)){
  data[[i]] = factor(data[[i]])
} #cast to factor
for (i in names(data.continous)){
  outliers[[i]] = boxplot.stats(data.continous[,i])$out
} #ensure that no outliers exist

training = head(data,471)
testing = tail(data,117)

# start with all variables minus collineary variables
start_back_any = glm(any.event ~ . -basedp -pkhr -dp -X.mphr.b. -dpmaxdo -newMI -newCABG - newPTCA - death, data = training, family = 'binomial',control = list(maxit = 50))
end_back_any = step(start_back_any)

start_forw_any = glm(any.event ~ 1 , data = training, family = 'binomial',control = list(maxit = 50))
end_forw_any = step(start_forw_any, direction='forward',scope=formula(start_back_any))

start_both_any = glm(any.event ~ 1 , data = training, family = 'binomial',control = list(maxit = 50))
end_both_any = step(start_forw_any, direction='both',scope=formula(start_back_any))

car::vif(end_back_any) #check collinearity
#check linearity between  independent variables and log odds.
probabilities <- end_back_any %>% predict(training, type = "response")
mydata <- training %>%
  select("dobEF")
predictors <- c("dobEF")
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Mc Fadden Pseudo R^2
ll.null <- end_back_any$null.deviance/-2
ll.proposed <- end_back_any$deviance/-2
(ll.null - ll.proposed) / ll.null
1 - pchisq(2*(ll.proposed - ll.null), df=(length(end_back_any$coefficients)-1)) # pseudo R^2 p-value
hoslem.test(end_back_any$y, fitted(end_back_any), g=10) #we canno't reject H0

#ROC curve training data
pred_back =  prediction(end_back_any$fitted.values,training$any.event)
eval_back = performance(pred_back,"acc")
plot(eval_back)
roc = performance(pred_back, "tpr","fpr")
plot(roc,colorize = T,main="ROC curve, any event, training data",ylab="True Positive",xlab="False Positive")
abline(a=0,b=1)
auc = performance(pred_back,"auc")
auc = unlist(slot(auc,"y.values"))
auc = round(auc,4)
legend(.7,.2,auc,title="AUC")

#ROC curve testing data
pred_back =  predict(end_forw_any,testing,type='response')
pred_back = prediction(pred_back,testing$any.event)
roc = performance(pred_back, "tpr","fpr")
plot(roc,colorize = T,main="ROC curve, any event, testing data",ylab="True Positive",xlab="False Positive")
abline(a=0,b=1)
auc = performance(pred_back,"auc")
auc = unlist(slot(auc,"y.values"))
auc = round(auc,4)
legend(.7,.2,auc,title="AUC")



-basebp -pkhr -dp -dobEF -X.mphr.b. -dpmaxdo