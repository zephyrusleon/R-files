#计算效应量
install.packages("pwr")
library(pwr)

#导入
effect_size <- 0.2 #效应量
alpha <- 0.05 #显著性水平
power <- 0.8 #统计功效

#计算样本量
result <- pwr.t.test(d = effect_size,
                     sig.level = alpha,
                     power = power,
                     type = "two.sample",
                     alternative = "two.sided")

#输出结果
ceiling(result$n) #向上取整样本量
