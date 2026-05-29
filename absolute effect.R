# 安装并加载包（第一次运行需要安装）
# install.packages("readxl")
library(readxl)

df <- data.frame(
  study = c("Alejandro_2022", "Taunton_1996", "Tsourlou_2006"),
  n1 = c(17, 16, 12),
  mean1 = c(29.1, 52.5, 32.0),
  sd1 = c(7.25, 9.30, 1.70),
  n2 = c(17, 13, 10),
  mean2 = c(22.50, 48.30, 25.56),
  sd2 = c(3.75, 8.90, 1.80)
)

# 显示数据结构
print(df)
str(df)

# 计算 pooled SD
df$pooledSD <- sqrt(((df$n1 - 1) * df$sd1^2 + (df$n2 - 1) * df$sd2^2) / (df$n1 + df$n2 - 2))

# 计算均值差 (绝对效应, 单位 kg)
df$mean_diff <- df$mean1 - df$mean2

# 计算 SMD (Cohen's d)
df$SMD <- df$mean_diff / df$pooledSD

# 把 SMD 转换为绝对效应 (kg)
df$SMD_to_kg <- df$SMD * df$pooledSD  # 实际上等于 mean_diff

# 打印结果
print(df)

# 额外生成解释表：SMD 对应多少 kg（便于放图注）
SMD_example <- data.frame(
  SMD = c(0.2, 0.5, 0.8),
  Interpretation = c("small", "medium", "large"),
  Kg_equivalent = round(mean(df$pooledSD) * c(0.2, 0.5, 0.8), 2)
)
print(SMD_example)




