library(irr)

# 1. 载入数据
# 这里直接构造你提供的数据


# 2. 数据清洗与格式转换
# 你的数据第一列是“专家姓名”，计算时需要去掉，只保留数值
rating_data <- data[, -1] 

# 【关键步骤】
# irr 包的 kendall 函数要求：行(Row)是“被评价的对象(指标)”，列(Column)是“评价者(专家)”
# 你原本的数据：行是专家，列是指标。
# 所以我们需要用 t() 函数进行转置。
matrix_data <- t(rating_data)

# 3. 计算肯德尔协调系数 (Kendall's W)
# correct = TRUE 用于校正结值（Ties，即相同的分数）
kendall_result <- kendall(matrix_data, correct = TRUE)

# 4. 打印结果
print(kendall_result)

# 假设 matrix_data 是你之前代码里的那个转置后的矩阵
# 计算每个指标的均值 (Mean) 和 标准差 (SD)
means <- rowMeans(matrix_data)
sds <- apply(matrix_data, 1, sd)

# 计算变异系数 (CV) = 标准差 / 均值
cv <- sds / means

# 打印变异系数，从大到小排序，看看哪些指标分歧最大
sort(cv, decreasing = TRUE)
