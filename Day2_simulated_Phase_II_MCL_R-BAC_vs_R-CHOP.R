  
  # —— 1：安装必要的包（首次运行需要）——
  install.packages("readr")   # 高效读取 CSV 文件
  install.packages("dplyr")   # 数据清洗、变换、筛选必备
  
  
  # —— 2：加载包（每次运行前都要）——
  library(readr)     # 用于读取CSV文件
  library(dplyr)     # 提供管道语法和数据操作函数
  
  df_raw <- read_csv("data/dirty_phase2_mcl_simulated_data.csv")
  df <- df_raw # 这份可以随便清洗、操作
  
  head(df_raw)
  head(df)
  
  # -- 3：识别问题数据 —— 
  str(df)           # 查看数据类型（int, factor, character）
  summary(df)       # 查看变量的分布、缺失值等信息
  # 多个变量有缺失值，结局的缺失值过多，可能存在treatment的拼写错误，年龄错误，time和event都有错误，只有0和1不可能出现2
  
  #检查 id是否有重复
  any(duplicated(df$patient_id))
  
  #哪个重复了，各重复了几次
  df %>%
    group_by(patient_id) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    arrange(desc(n))
  
  #查看具体重复的内容，便于人工保留. 保留策略？？
  df %>%
    group_by(patient_id) %>%
    filter(n() > 1) %>%
    arrange(patient_id) %>%
    View()
  
  # 清洗前总记录数 120
  n_before <- nrow(df)
  n_before
  
  # 处理原则：保留每个 patient_id 缺失最少的一条
  df <- df %>%
    rowwise() %>%
    mutate(n_missing = sum(is.na(c(age, ecog, time, event, severe_ae, mrd_positive)))) %>%
    ungroup() %>%
    group_by(patient_id) %>%
    arrange(n_missing) %>%
    slice(1) %>%
    ungroup()
  
  # 清洗后记录数67
  n_after <- nrow(df)
  n_after
  View(df)
  
  #确认删除重复值后id都是唯一了
  df %>%
    count(patient_id) %>%
    summarise(
      total_id = n(),
      avg_times = mean(n),
      max_times = max(n)
    )
  
  
  # 4 ：清洗数据 --
# 4.1 清洗treatment
  
  unique(df$treatment)
  # 判单有哪些可能性
  
  # 统一大小写 + 去掉首尾空格
  df <- df %>%
    mutate(treatment = toupper(trimws(treatment)))  # 全部转为大写并去掉空格
  unique(df$treatment)
  # 合并拼写错误 → 标准为 R-BAC 和 R-CHOP，其他全部设为 NA
  df <- df %>%
    mutate(treatment = case_when(
      treatment %in% c("R-BAC", "RBAC") ~ "R-BAC",
      treatment %in% c("R-CHOP", "RCHOP", "R-CHOP", "RCHOP ", "RCHOP") ~ "R-CHOP",
      treatment %in% c("NONE", "", "UNKNOWN") ~ NA_character_,
      TRUE ~ treatment   # 👈 如果已经是标准值，就保留！
    ))
  
  
  # 转换为 factor 类型（两组）并设定顺序
  df$treatment <- factor(df$treatment, levels = c("R-CHOP", "R-BAC"))
  
  # 删除治疗组缺失的记录
  df <- df %>% filter(!is.na(treatment))
  
  # 检查剩余数据分布
  table(df$treatment)
  
  
  # 查看清洗后各组人数，包括 NA
  table(df$treatment, useNA = "always") # 46，55，22
  
  
# 4.2 清洗 age
  summary(df$age)
  df <- df %>%
    mutate(age = ifelse(age < 18 | age > 90, NA, age))
  df <- df %>% filter(!is.na(age))
  summary(df$age)
  
  str(df$age)
  
# 4.3 清洗ecog 只纳入体能状态好的病人 0-2， 排出干扰因素
  
  # 查看原始取值
  table(df$ecog, useNA = "always")
  
  # 清洗：将合法值转为整数型，其余设为 NA
  df <- df %>%
    mutate(ecog = case_when(
      ecog %in% c("0", 0) ~ 0,
      ecog %in% c("1", 1) ~ 1,
      ecog %in% c("2", 2) ~ 2,
      ecog %in% c("3", 3) ~ 3,
      TRUE ~ NA_real_  # 不合法的值（如"unknown"）设为 NA
    ))
  
  # 转为 factor 类型（有序分组）
  df$ecog <- factor(df$ecog, levels = c(0, 1, 2, 3), ordered = TRUE)
  
  # 查看清洗后的频数分布
  table(df$ecog, useNA = "always")
  
# 4.4 清洗time
  # 查看时间分布
  summary(df$time)
  
  # 剔除缺失值和负值
  df <- df %>%
    filter(!is.na(time) & time > 0)
  
  # 设置最小生存时间阈值（例如将 time < 0.1 的统一设为 0.1）
  df <- df %>%
    mutate(time = ifelse(time < 0.1, 0.1, time))
  
  # 查看结果
  summary(df$time)
  
  # 删除“时间非正且事件发生”的错误组合
  df <- df %>%
    filter(!(event == 1 & (is.na(time) | time <= 0)))
  
  summary(df$time)
  
# 4.5 清洗 event
  # 查看原始 event 分布（含非法值）
  table(df$event, useNA = "always")
  
  # 清洗：保留 0 和 1，其他设为 NA
  df <- df %>%
    mutate(event = case_when(
      event %in% c(0, 1) ~ as.numeric(event),
      TRUE ~ NA_real_
    ))
  
  # 删除 NA 的记录
  df <- df %>% filter(!is.na(event))
  
  # 查看清洗后结果
  table(df$event)
  View(df)

  
# 4.6 清洗 AE -- CTCAE（Common Terminology Criteria for Adverse Events）在真实的临床数据中，不仅有分级还有分类
  
  # 检查原始 severe_ae 值分布
  table(df$severe_ae, useNA = "always")
  
  # 清洗：保留 0 和 1，其他设为 NA
  df <- df %>%
    mutate(severe_ae = case_when(
      severe_ae %in% c(0, 1) ~ as.integer(severe_ae),
      TRUE ~ NA_integer_
    ))
  
  # 转换为 factor（用于分组/表格），0代表没有发生 Grade ≥3，1代表发生了，简化版的分级
  df$severe_ae <- factor(df$severe_ae, levels = c(0, 1), labels = c("No", "Yes"))
  
  # 再次查看清洗后分布
  table(df$severe_ae, useNA = "always")
  
  # 去掉 NA（因为我们要用它建模）
  df <- df %>% filter(!is.na(severe_ae))
  
  # 输出当前剩余行数
  cat("清洗后剩余样本数：", nrow(df), "\n")
  View(df)
  
# 4.7 清洗 MRD -- 通常会直接总结为二分类变量，只在某些特殊情况下使用连续变量
  
  # 查看原始取值分布
  table(df$mrd_positive, useNA = "always")
  
  # 清洗：保留 0 和 1，其他设为 NA
  df <- df %>%
    mutate(mrd_positive = case_when(
      mrd_positive %in% c(0, 1) ~ as.integer(mrd_positive),
      TRUE ~ NA_integer_
    ))
  
  # 转换为 factor（带标签）
  df$mrd_positive <- factor(df$mrd_positive, 
                            levels = c(0, 1), 
                            labels = c("Negative", "Positive"))
  
  # 再次查看清洗后分布
  table(df$mrd_positive, useNA = "always")
  View(df)
  
  # 对于存在na的协变量，我们可以用已有变量预测缺失的值，并多次模拟生成多个版本的完整数据集，最后将分析结果合并，考虑不确定性。
  #多重插补（Multiple Imputation）
  
  # 粗暴的方法：建模前统一剔除关键变量的 NA, 洗干净的数命名为df_model
  df_model <- df %>%
    filter(!is.na(time), 
           !is.na(event), 
           !is.na(treatment), 
           !is.na(ecog), 
           !is.na(mrd_positive))
  
  # 强制变为 unordered factor
  df_model$ecog <- factor(df_model$ecog, ordered = FALSE)
  
  # 设置参考组为 ECOG = 0
  df_model$ecog <- relevel(df_model$ecog, ref = "0")
  
  
  # 检查剩余样本数
  nrow(df_model)

  # 5正式分析
  
  # 5.1 cox model
  library(survival)
  surv_obj <- Surv(time = df_model$time, event = df_model$event)
  cox_model <- coxph(surv_obj ~ treatment + age + ecog + mrd_positive, data = df_model)
  
  # 查看模型结果
  summary(cox_model)
  
  # 检查比例风险假设是否成立
  cox.zph(cox_model)
  # n= 36, number of events= 11  这是分析不可靠的原因，样本量远远不够
  # 每拟合一个变量，需要至少10个事件， 5*10 >> 11
  
  # 如果你希望画森林图
  install.packages("forestmodel")  # 只需一次
  
  library(forestmodel)
  
  forest_model(cox_model)
  
  # 5.2 KM plot
  
  library(survival)
  library(survminer)
  
  # 生存对象
  surv_obj <- Surv(time = df_model$time, event = df_model$event)
  
  # 拟合生存曲线
  fit_km <- survfit(surv_obj ~ treatment, data = df_model)
  
  # 绘图
  ggsurvplot(fit_km,
             data = df_model,
             pval = TRUE,                  # 显示 log-rank 检验 p 值
             conf.int = TRUE,              # 显示置信区间
             risk.table = TRUE,            # 风险表：每时间点剩多少人
             legend.title = "Treatment",
             legend.labs = c("R-BAC", "R-CHOP"),
             xlab = "Time (months)",
             ylab = "Progression-Free Survival",
             title = "Kaplan-Meier Curve by Treatment Group",
             palette = c("#E69F00", "#0072B2"))  # 可选配色
  
  
# 5.3 分析AE在两组中的差别
  
  table(df_model$treatment, df_model$severe_ae)
  
    
  
  
  
  
  
  
  
  
  
  
  
  
  