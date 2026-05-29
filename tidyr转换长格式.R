#宽数据转化为长数据使用ggplot2绘图
library(ggplot2)
library(tidyr)

#示例
data <-data.frame(
  name = c("a","b","c"),
  math = c("80","90","100"),
  english = c("89", "90","40")
)
#使用tidyr中pivot_longer转换长格式
long_data = pivot_longer(
  data = data,
  cols = c(math,english), #指定要转换的列
  names_to = "subject", #列名
  values_to = "score" #对应的值列
)
print(long_data)

#应用
#若meta长格式为n1 mean1 sd1 n2 mean2 sd2需要转换为n mean sd 
df <- data.frame(
  study = c("atn1979","jonathon2014","nujso2023"),
  n1   = c(10, 12, 15),
  mean1 = c(5.2, 6.1, 4.8),
  sd1   = c(0.8, 0.9, 1.1),
  n2   = c(9, 11, 14),
  mean2 = c(6.5, 7.0, 5.3),
  sd2   = c(0.7, 1.0, 1.2),
  agent = c("RT","AE","Yoga"),
)

long_data <- df %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "group"),
    names_pattern = "(.*)([0-9]+)$"
  )
print(long_data)