#复现带显著性标记的柱状图
#导入数据
data_long <- read.csv("data_long.csv")
#添加 value列
data_long$value <- data_long$DAI

#确定 x 出现顺序
data_long$Group <- factor(data_long$Group,
                          levels = c("Healthy", "Mild", "Moderate", "Severe"))
#定义误差棒计算方法
ebbottom <- function(x) {return(mean(x) - sd(x))} #误差棒下界
ebtop <- function(x) {return(mean(x) + sd(x))} #误差棒上界

#计算 y 轴上限用于调整显著性标记的位置
index_high <- max(data_long$value) * 1.4

#画图
library(ggplot2)
library(ggsignif)

colors <- c("gray", "#90A4C4", "#DBDDEF", "#FCEDE9")

ggplot(data = data_long, aes(x = Group, y = value)) +
 stat_summary(fun = mean, geom = "bar",
              fill = colors,
              width = 0.7, alpha = 0.6) +
  #误差棒
 stat_summary(fun.min = ebbottom = mean_sdl, fun.max= ebtop,
              geom = "errorbar",
              width = 0.2, color = "black") +
  #散点
 geom_jitter(width = 0.2, size = 2, color = "black") +
  #显著性标记
 geom_signif(comparisons = list(c("Healthy", "Mild"),
                                c("Healthy", "Moderate"),
                                c("Healthy", "Severe")),
             test = "t.test",
             map_signif_level = TRUE,
             y_position = c(index_high,
                            index_high * 1.1,
                            index_high * 1.2),
             tip_length = c(c(0.7,0.1)),
             textsize = 6,
             size = 5) +
  #y轴标签
  scale_y_continuous(expand= c(0,0),
                     limits = c(0, index_high= 1.2)) +
  labs(x = "Group", y = "DAI") +
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))