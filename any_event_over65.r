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
data_over60 = data[data$age > 60,]


training_over60 = head(data_over60,331)
testing_over60 = tail(data_over60,83)

# start with all variables minus collineary variables
start_back_any_60 = glm(any.event ~ . -basedp -pkhr -dp -X.mphr.b. -dpmaxdo -newMI -newCABG - newPTCA - death, data = training_over60, family = 'binomial',control = list(maxit = 50))
end_back_any_60 = step(start_back_any_60)

# worse results
start_forw_any_60 = glm(any.event ~ 1 , data = training_over60, family = 'binomial',control = list(maxit = 50))
end_forw_any_60 = step(start_forw_any_60, direction='forward',scope=formula(start_back_any_60))

start_both_any_60 = glm(any.event ~ 1 , data = training_over60, family = 'binomial',control = list(maxit = 50))
end_both_any_60 = step(start_forw_any_60, direction='both',scope=formula(start_back_any_60))

car::vif(end_back_any_60) #check collinearity
#check linearity between  independent variables and log odds.
probabilities <- end_back_any_60 %>% predict(training_over60, type = "response")
mydata <- training_over60 %>%
  select("dobdose","dobEF")
predictors <- c("dobdose","dobEF")
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
#apporximately linear

#Mc Fadden Pseudo R^2
ll.null <- end_back_any_60$null.deviance/-2
ll.proposed <- end_back_any_60$deviance/-2
(ll.null - ll.proposed) / ll.null
1 - pchisq(2*(ll.proposed - ll.null), df=(length(end_back_any_60$coefficients)-1)) # pseudo R^2 p-value
hoslem.test(end_back_any_60$y, fitted(end_back_any_60), g=10) #goodness of fit test

#ROC curve training data
pred_back =  prediction(end_back_any_60$fitted.values,training_over60$any.event)
eval_back = performance(pred_back,"acc")
plot(eval_back)
roc = performance(pred_back, "tpr","fpr")
plot(roc,colorize = T,main="ROC curve, any event, age > 60, training data",ylab="True Positive",xlab="False Positive")
abline(a=0,b=1)
auc = performance(pred_back,"auc")
auc = unlist(slot(auc,"y.values"))
auc = round(auc,4)
legend(.7,.2,auc,title="AUC")


#ROC curve testing data
pred_back =  predict(end_back_any_60,testing_over60,type='response')
pred_back = prediction(pred_back,testing_over60$any.event)
eval_back = performance(pred_back,"acc")
plot(eval_back)
roc = performance(pred_back, "tpr","fpr")
plot(roc,colorize = T,main="ROC curve, age > 60, testing data",ylab="True Positive",xlab="False Positive")
abline(a=0,b=1)
auc = performance(pred_back,"auc")
auc = unlist(slot(auc,"y.values"))
auc = round(auc,4)
legend(.7,.2,auc,title="AUC")


