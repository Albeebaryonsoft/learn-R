
# —— 第一步：安装必要的包（首次运行需要）——
install.packages("readr")   # 高效读取 CSV 文件
install.packages("dplyr")   # 数据清洗、变换、筛选必备

# —— 第二步：加载包（每次运行前都要）——
library(readr)     # 用于读取CSV文件
library(dplyr)     # 提供管道语法和数据操作函数
df <- read_csv("data/dirty_phase2_mcl_simulated_data.csv")
head(df)


# -- 第三步：识别问题数据 —— 
str(df)           # 查看数据类型（int, factor, character）
summary(df)       # 查看变量的分布、缺失值等信息
#从summary里面可以看出：多个变量有缺失值，结局的缺失值过多，可能存在treatment的拼写错误，年龄错误，time和event都有错误，只有0和1不可能出现2
#检查 id是否有重复
any(duplicated(df$patient_id))
#哪个重复了，各重复了几次
df %>%
  group_by(patient_id) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  arrange(desc(n))
#查看具体重复的内容，便于人工保留
df %>%
  group_by(patient_id) %>%
  filter(n() > 1) %>%
  arrange(patient_id) %>%
  View()

# --第四步：清洗数据 --
# 4.1 treatment





