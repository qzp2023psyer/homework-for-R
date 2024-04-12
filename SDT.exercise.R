setwd("C:/Users/Cesare/Desktop/R4psybook/R4Psy")
library(tidyverse)
# 获取所有的.out文件名
files <- list.files("data/match", pattern = "data_exp7_rep_match_.*\\.out$", full.names = TRUE)

convert_data_types = function(df) {
  df <- df %>%
    dplyr::mutate(Date = as.character(Date),
                  Prac = as.character(Prac),
                  Sub = as.numeric(Sub),
                  Age = as.numeric(Age),
                  Sex = as.character(Sex),
                  Hand = as.character(Hand),
                  Block = as.numeric(Block),
                  Bin = as.numeric(Bin),
                  Trial = as.numeric(Trial),
                  Shape = as.character(Shape),
                  Label = as.character(Label),
                  Match = as.character(Match),
                  CorrResp = as.character(CorrResp),
                  Resp = as.character(Resp),
                  ACC = as.numeric(ACC),
                  RT = as.numeric(RT))
  return(df)
}

# 读取每个.out文件，并进行数据清洗
matching.data <- lapply(files, function(file) {
  df <- read.table(file, header = TRUE)
  df <- dplyr::filter(df, Date != "Date") # 过滤掉 Date 列值为 "Date" 的行
  df <- mutate(df, 
               convert_data_types(df)
  ) # 进行数据类型转换和数据清洗
  return(df)
}) %>% 
  bind_rows()

# 清除中间变量
rm(files)

matching.result <- matching.data %>% 
  select(Sub, Hand, Block, Bin, Shape, Match, ACC, RT) %>% 
  drop_na() %>% 
  filter(Hand == "R", # 选择右利手被试
         RT >= 0.2 & RT <= 1.5 # 选择RT属于[200,1500]
  ) %>%
  group_by(Sub, Shape) %>%
  summarise(
    hit = length(ACC[Match == "match" & ACC == 1]),
    fa = length(ACC[Match == "mismatch" & ACC == 0]),
    miss = length(ACC[Match == "match" & ACC == 0]),
    cr = length(ACC[Match == "mismatch" & ACC == 1]),
    Dprime = qnorm(
      ifelse(hit / (hit + miss) < 1,
             hit / (hit + miss),
             1 - 1 / (2 * (hit + miss))
      )
    ) 
    - qnorm(
      ifelse(fa / (fa + cr) > 0,
             fa / (fa + cr),
             1 / (2 * (fa + cr))
      )
    )) %>%
  select(-hit, -fa, -miss, -cr)%>%
  pivot_wider(names_from = "Shape", 
              values_from = "Dprime")


