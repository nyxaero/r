library(MASS)
library(ggplot2)
library(caret)
library(car)
library(pROC)
library(dplyr)
library(corrplot)

data("Boston")
head(Boston)

sum(is.na(Boston))

summary(Boston)

boxplot(Boston$medv, main="boxplot of median values of home")

Boston <- Boston %>% filter(medv<50)

cor_matrix <- cor(Boston)
corrplot(cor_matrix, method="circle")

simple_model <- lm(medv ~ lstat, data=Boston)
summary(simple_model)

multiple_model <- lm(medv ~ lstat * rm, data = Boston)
summary(multiple_model)

adjusted_R2 <- summary(multiple_model)$adj.r.squared
AIC_value <- AIC(multiple_model)
BIC_value <- BIC(multiple_model)

plot(multiple_model, which = 1, main="residuals vs fitted plot")

plot(multiple_model, which = 2, main="normal Q-Q plot")

train_control <- trainControl(method="cv", number=10)
cv_model <- train(
  medv~lstat*rm, 
  data=Boston,
  method="lm",
  trControl = train_control
)
print(cv_model)

Boston$medv_class <- ifelse(Boston$medv >= 25, 1 , 0)

logistic_model <- glm( medv_class ~ lstat * rm, data=Boston, family="binomial")
summary(logistic_model)

pred_probs <- predict(logistic_model, type="response")
roc_curve <- roc(Boston$medv, pred_probs)

plot(roc_curve, main="roc for logistic reg model", col="blue")
abline(a=0,b=1,lty=2,col="red")

cat(auc(roc_curve))