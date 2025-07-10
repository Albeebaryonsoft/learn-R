  
  # —— 第一步：安装必要的包（首次运行需要）——
  install.packages("readr")   # 高效读取 CSV 文件
  install.packages("dplyr")   # 数据清洗、变换、筛选必备
  
  
  # —— 第二步：加载包（每次运行前都要）——
  library(readr)     # 用于读取CSV文件
  library(dplyr)     # 提供管道语法和数据操作函数
  
  df_raw <- read_csv("data/dirty_phase2_mcl_simulated_data.csv")
  df <- df_raw # 这份可以随便清洗、操作
  
  head(df_raw)
  head(df)
  
  # -- 第三步：识别问题数据 —— 
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
  
  
  # --第四步：清洗数据 --
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
  
  # 查看清洗后各组人数，包括 NA
  table(df$treatment, useNA = "always") # 46，55，22
  
  
  # 4.2 清洗 age
  summary(df$age)
  df <- df %>%
    mutate(age = ifelse(age < 18 | age > 90, NA, age))
  df <- df %>% filter(!is.na(age))
  summary(df$age)
  
  str(df$age)
  
  