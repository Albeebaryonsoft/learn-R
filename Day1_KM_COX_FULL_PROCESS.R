
#使用模拟临床试验数据，对 Treatment vs Control 两组患者的生存时间进行分析：
# 从数据清洗开始 全流程分析

# 如果曾经下载过packages，就不用重复下载。但是我们不知道哪些下载过。所以用这一串代码。

required_packages <- c("readr", "dplyr", "tableone", "ggplot2", "survival", "survminer")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}



# step 0: read data
data <- read.csv("data/simulated_clinical_trial_data_dirty.csv")

head(data)


#step 1: data cleaning
library(dplyr)

data$treatment_group <- factor(data$treatment_group)
data$sex <- factor(data$sex)
#依赖 %>% 出过问题，所以改换直接写清洗逻辑
str(data) # character 需要转换成factor

#step 2: descriptive anaysis

table1 <- CreateTableOne(vars = c("age","sex"), strata = "treatment_group", data = data )

print(table1, showAllLevels = TRUE)


