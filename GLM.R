#广义线性模型

#1.随机成分：Y服从某个分布（不一定是正态）
#2.系统成分：X的线性组合η=β₀+β₁X1+...+βpXp
#3.连接函数：g(μ) = η，把μ（Y的均值）变成可以预测的值

#logistic回归 二分类
model <- glm( diabetes ~ age + bmi + glucose,
              data = patient_data,
              family = binomial(link = "logit") ) #family指定分布和连接函数，
                                                  #此处指定二项分布和logit连接函数
summary(model)

pred_probs <- predict(model, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0) #根据概率预测类别

#泊松回归 计数数据
model_poisson <- glm( hospital_days ~ age + severity_score + comorbidity,
                      data = patient_data,
                      family = poisson(link = "log") ) #泊松分布和log连接函数
#检查离散程度
dispersion <- model_poisson$deviance / model_poisson$df.residual
#如果dispersion > 1.5，说明过度离散，可以考虑使用负二项回归
library(MASS)
model_nb <- glm.nb(hospital_days ~ age + severity, data = patient_data)


#实例 二分类问题 diabetes预测
library(tidyverse)
library(car)

diabetes <- read_csv("diabetes.csv")
str(diabetes)

#构建模型
model <- glm( diabetes ~ age + bmi + glucose,
              data = diabetes,
              family = binomial(link = "logit") )
summary(model)
vif(model) #检查多重共线性

diabetes$pred_prob <- predict(model, type = "response")
diabetes$pred_class <- ifelse(diabetes$pred_prob > 0.5, "positive", "negative")

table(diabetes$diabetes, diabetes$pred_class,Actual = diabetes$diabetes, Predicted = diabetes$pred_class)
#ROC曲线
library(pROC)
roc_obj <- roc(diabetes$diabetes, diabetes$pred_prob)
plot(roc_obj, main = "ROC Curve", col = "blue")
auc(roc_obj) #计算AUC值

# Y 是什么类型？
# ├── 连续正态值
# │   └── Gaussian 分布 + Identity 连接（普通线性回归）
# │
# ├── 二分类（0/1）
# │   └── Binomial 分布 + Logit 连接（逻辑回归）
# │
# ├── 计数数据（0, 1, 2, 3, ...）
# │   ├── 方差 ≈ 均值
# │   │   └── Poisson 分布 + Log 连接
# │   └── 方差 > 均值（过度离散）
# │       └── Negative Binomial 分布 + Log 连接
# │
# └── 有序多分类（1 < 2 < 3）
#     └── Ordinal 分布 + Cumulative Logit 连接