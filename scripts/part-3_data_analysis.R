# Data analysis 

# loading in prepared data: 
  X_mat <- read.csv(here::here("X_mat_processed.csv"))
  X_mat <- X_mat %>% 
    select(CLE, Forest_Type, PRECU, RainFall,rf_index) 
  
# Analysis 1 ----
# Simple regression between annual rainfall and mean relative abundance (%)
  summary(lm(Y_mat$V_CAL~X_mat$PRECU))
  plot(Y_mat$V_CAL~X_mat$PRECU)
  
  library(ggplot2)
  CAL <- data.frame(Y_mat$V_CAL,X_mat$PRECU, X_mat$rf_index)
  colnames(CAL) <- c("V_CAL","PRECU","rf_index")
  ggplot(CAL,aes(PRECU, V_CAL)) +
    geom_point() 
    geom_smooth(method='lm', formula= y~x)
  
  RHG <- data.frame(Y_mat$V_RHG,X_mat$PRECU)
  colnames(RHG) <- c("V_RHG","PRECU")
  ggplot(RHG,aes(PRECU, V_RHG)) +
    stat_summary(fun.data=mean_cl_normal) + 
    geom_smooth(method='lm', formula= y~x)

  RAA <- data.frame(Y_mat$V_KAA,X_mat$PRECU)
  colnames(RAA) <- c("V_KAA","PRECU")
  ggplot(RAA,aes(PRECU, V_KAA)) +
    geom_point()+
    geom_smooth(method='lm', formula= y~x)
  
# Analysis 2 - bar charts ---- 
  
  ggplot(data=CAL, aes(x=rf_index, y=log(V_CAL))) +
    geom_boxplot()
    