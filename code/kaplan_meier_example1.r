
install.packages("readr")      
library(readr)


df <- read_csv("data/simulated_clinical_trial_data.csv") 
head(df)


install.packages("survival")
install.packages("survminer")

library("survival")
library("survminer") #这里曾需要下载rtools（win），mac则不同

surv_obj <- Surv(time = df$time, event = df$event)
km_fit <- survfit(surv_obj ~ group, data = df)

library("survminer")
ggsurvplot(km_fit,
           data = df,
           pval = TRUE,             # 显示 log-rank 检验 p 值
           conf.int = TRUE,         # 加置信区间
           risk.table = TRUE,       # 图下显示在险人数
           legend.title = "Group",
           legend.labs = c("Control", "Treatment"),
           xlab = "Time (days)",
           ylab = "Survival probability",
           ggtheme = theme_minimal())


