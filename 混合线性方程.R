install.packages('lme4')
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("emmeans")
install.packages("performance")
install.packages("effects")
install.packages("lmerTest")
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(emmeans)
library(performance)
library(effects)

#next, we have a wide format dataset
df_wide <- data.frame(
  id = 1:8,
  group = c(0, 0, 0, 0, 1, 1, 1, 1),
  time0 = c(0.35, 0.77, 0.48, 0.63, 0.45, 0.56, 1.08, 0.55),
  time1 = c(1.01, 1.32, 1.18, 1.42, 0.59, 0.86, 1.44, 1.20),
  time2 = c(1.47, 1.60, 1.65, 1.88, 0.64, 1.37, 1.93, 1.68),
  time4 = c(2.46, 2.54, 2.86, 3.13, 0.99, 2.04, 2.63, 2.87)
)

#we need to convert it to long format
df_long <- df_wide %>%
  pivot_longer( #使用pivot_longer函数将数据从宽格式转换为长格式
    cols = starts_with("time"),
    names_to = "time",#names_to指定新列的名称，这里是time
    names_prefix = "time",#移除time前缀
    values_to = "grow_factor") %>%
  mutate(time = as.numeric(gsub("time", "", time))) #使用mutate函数创建一个新的time列，通过gsub函数移除time前缀并将其转换为数值类型
head(df_long)
#接着，我们在建模前，先进行描述性统计分析，看看数据的分布情况
df <- df_long %>%
  group_by(group, time) %>%
  summarise(
    Mean = mean(grow_factor),
    Sd = sd(grow_factor),
    N = n(),
    .groups = "drop")
print(df)
#绘制生长因子随时间变化的趋势图
ggplot(df_long, aes(x = time, y = grow_factor, color = factor(group))) +
  geom_line(aes(group = id), alpha = 0.5) + #添加个体线条
  geom_point(size = 3) + #添加数据点
  stat_summary(fun = mean, geom = "line", size = 1.5) + #添加平均趋势线
  #stats_summary函数用于计算每个时间点的平均生长因子，
  #并使用geom = "line"参数将其绘制为线条，size参数设置线条的粗细
  labs(title = "Growth Factor Over Time by Group",
       x = "Time (days)",
       y = "Growth Factor",
       color = "Group") +
  theme_minimal()

#接下来，我们使用混合线性模型来分析数据
#个体为随机截距，时间和组别为固定效应
model_full <- lmer(
  grow_factor ~ time * group + (1 | id), #公式：因变量 ~ 固定效应 + (随机效应 | 分组变量)
  data = df_long)
summary(model_full)
#检验交互作用的显著性
model_no_interaction <- lmer(grow_factor ~ group + time + (1 | id),
                             data = df_long,
                             REML = FALSE) #REML = FALSE表示使用最大似然估计来拟合
model_full_for_LRT <- lmer(grow_factor ~ group * time + (1 | id),
                           data = df_long,
                           REML = FALSE)
anova(model_no_interaction,
      model_full_for_LRT,
      test = "LRT") # 似然比检验
#(1 | id)表示对每个id估计一个随机截距，这是处理重复测量数据相关性的关键，它允许每个研究对象有自己的基线水平。

#在得到作用后，我们使用emmeans包进行事后分析，比较不同时间点和组别之间的差异
emm_interaction <- emmeans(model_full, 
                           specs= pairwise ~ group | time) #specs参数指定了要比较的组别和时间点，这里是比较不同组别在每个时间点的差异
print(emm_interaction$emmeans) #查看边际均值

plot(emm_interaction$emmeans, comparisons = TRUE) +
  theme_minimal() +
  labs=(title = "interaction plot"
        subtitle = "estimate point and 95% CI") #绘制边际均值图，并显示比较结果
emm_slopes <- emtrends(model_full, specs = "group", var = "time")
print(emm_slopes)
# 配对比较两组的时间斜率是否不同
pairs(emm_slopes)

提取残差和拟合值
df_long$fitted <- fitted(model_full)
df_long$residuals <- residuals(model_full)

# 5.2 绘制诊断图
# 残差vs拟合值图（检查同方差性）
p1 <- ggplot(df_long, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype ="dashed", color ="red") +
  geom_smooth(se = FALSE, method ="loess", color ="blue") +
  labs(title ="残差 vs. 拟合值图", x ="拟合值", y ="残差") +
  theme_minimal()

# Q-Q图（检查残差正态性）
p2 <- ggplot(df_long, aes(sample = residuals)) +
  stat_qq_line(color ="red") +
  stat_qq(alpha = 0.6) +
  labs(title ="残差Q-Q图", x ="理论分位数", y ="样本分位数") +
  theme_minimal()

# 使用patchwork包并排显示两个图
library(patchwork)
p1 + p2

# 5.3 使用performance包进行综合诊断
check_model(model_full)

# 5.3 使用performance包进行综合诊断
diagnostic_plots <- check_model(model_full)
plot(diagnostic_plots)

# 上一步如果出图未成功 可运行下面的code：
if(interactive()) {
  dev.new()# 打开一个新的图形窗口
  plot(diagnostic_plots)
}
