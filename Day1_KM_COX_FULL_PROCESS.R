#你好 王老板223543
# 能不能行


## 你好王老板的点点滴滴

#使用模拟临床试验数据，对 Treatment vs Control 两组患者的生存时间进行分析：
# 从数据清洗开始 全流程分析

# 如果曾经下载过packages，就不用重复下载。但是我们不知道哪些下载过。所以用这一串代码。

### 测试 pull and push

required_packages <- c("readr", "dplyr", "tableone", "ggplot2", "survival", "survminer")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}



# step 0: read data
data <- read.csv("data/simulated_clinical_trial_data_dirty.csv")


#step 1: data cleaning
library(dplyr)

data$treatment_group <- factor(data$treatment_group)
data$sex <- factor(data$sex)
#依赖 %>% 出过问题，所以改换直接写清洗逻辑
str(data) # character 需要转换成factor

#step 2: descriptive anaysis

table1 <- CreateTableOne(vars = c("age","sex"), strata = "treatment_group", data = data )

print(table1, showAllLevels = TRUE)

#step3: kaplan-meier 生存曲线

surv_obj <- Surv(time = data$time, event = data$status)
fin_km <- survfit(surv_obj ~ treatment_group, data = data)


km_plot <- ggsurvplot(fin_km,
           data = data,
           risk.table = TRUE, # 图下显示在险人数
           risk.table.height = 0.25,
           pval = TRUE, # 显示 log-rank 检验 p 值
           conf.int = TRUE,         # 加置信区间
           legend.labs = c("Control", "Treatment"),
           legend.title = "Group",
           palette = c("#E69F00","#56B4E9"),  #颜色
           surv_median.line = "hv" , #添加中为生存时间参考线
           xlab = "Time (months)",
           ylab = "Survival probability",
           title = "Kaplan-Meier Survival Curve by Treatment Group",
           ggtheme = theme_minimal()) # 还有theme_classic()，ggtheme = theme_bw()


# 调整主标题居中
km_plot$plot <- km_plot$plot +
  theme(plot.title = element_text(hjust = 0.5))

# 显示图
km_plot

#仅仅输出km为PDF
ggsave("km_plot_day1.pdf", km_plot$plot, width = 8, height = 6, dpi = 300)

#输出 km＋risk table 为pdf
install.packages("patchwork")   # 第一次用时安装
library(patchwork)              # 每次使用前加载
combined_plot <- km_plot$plot / km_plot$table + 
  plot_layout(heights = c(3, 1))  # 上下比例
ggsave("km_plot_with_risktable.pdf", plot = combined_plot, width = 10, height = 8, dpi = 300)

#step 4: cox 回归模型
cox_model <- coxph(surv_obj ~ treatment_group + age + sex, data = data)
summary(cox_model)  # 显示模型摘要结果，包括 HR、p 值等

# 🌱 Step 5: 提取风险比（HR）与置信区间
exp(coef(cox_model))       # 提取 HR（风险比）
exp(confint(cox_model))    # 提取 95% 置信区间


# Although the treatment group showed a trend toward improved survival (HR < 1), 
#the effect was not statistically significant (95% CI includes 1). 
# Therefore, we cannot conclude that the treatment has a beneficial effect on survival based on the current data.
