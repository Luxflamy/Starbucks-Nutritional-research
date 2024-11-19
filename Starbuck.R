# Get the Data
rm(list = ls()) # initialization
# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest
# Either ISO-8601 date or year/week works!
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
tuesdata <- tidytuesdayR::tt_load('2021-12-21')
tuesdata <- tidytuesdayR::tt_load(2021, week = 52)

starbucks <- tuesdata$starbucks;starbucks

table(starbucks $product_name)

starbucks_2 <- starbucks %>% group_by(product_name)

df <- starbucks;
summary(df)
df$trans_fat_g <- as.numeric(df$trans_fat_g)
df$fiber_g <- as.numeric(df$fiber_g)
df$milk <- as.character(df$milk)
df$whip <- as.character(df$whip)
summary(df)

is_outlier <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x < lower_bound | x > upper_bound
}


clean_data <- function(df) {
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) {  
      df <- df[!is.na(df[[col]]), ]  
      df <- df[!is_outlier(df[[col]]), ] 
    }
  }
  return(df)
}

df_clean <- clean_data(df);df_clean

dim(df);dim(df_clean)

## visualize
summary(df_clean[,c(5:15)])

boxplot(df_clean[,c(7,8,9,12,13,14)],ylab = "(g)")
boxplot(df_clean[,c(10,11,15)],ylab = "(mg)")
boxplot(df_clean[,c(5,6)],ylab = "(ml)")
par(new=TRUE)
axis(side = 4, at = pretty(range(df_clean[,c(5,6)])), labels = sprintf(paste0("%s\n(KCal)"), pretty(range(df_clean[,c(5,6)]))))
mtext("Additional Value", side = 4, line = 3)

corrplot::corrplot(cor(df_clean_numeric), method = "color", col.lab = "black",tl.col = "black")
heatmap(correlation_matrix, col = colorRampPalette(c("blue", "white", "red"))(100))

## PCA 

colnames(df)

# 选择除了 "product_name" 以外的变量
df_pca <- df[,3:15]
# 如果有缺失值，你可以选择处理或者删除含有缺失值的行
# 例如：
df_pca <- na.omit(df_pca)
sapply(df_pca, class)
df_pca$milk <- as.numeric(as.character(df_pca$milk))
df_pca$whip <- as.numeric(as.character(df_pca$whip))

# 标准化数据
df_scaled <- scale(df_pca)

# 计算主成分
pca_result <- prcomp(df_scaled)
# 查看主成分的方差解释比例
summary(pca_result)
pca_result$rotation[,1:4]
# 绘制累积方差解释比例的图
plot(cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2), xlab = "Number of Components", ylab = "Cumulative Proportion of Variance Explained", type = "b")

# 根据需要选择保留的主成分数量
# 例如，保留解释总方差的80%：
cumulative_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
n_components <- which.max(cumulative_variance >= 0.8)


## MDS
df_scaled

## FA
library(psych)
library(corrplot)

df_cor <- cor(df_scaled)
corrplot(df_cor)
fa_result <- factanal(df_cor, factors = 4)
fa_result <- fa(df_cor, nfactors = 3, rotate = "none", fm = "ml",scores = "Anderson")

# 查看因子分析结果摘要
summary(fa_result)

# 打印因子载荷
print(fa_result$loadings)

# 绘制因子载荷图
fa.diagram(fa_result)




######################################################## calories ~ size/caffeine

starbucks_HotCh <- starbucks %>% filter(product_name  == "Hot Chocolate")
starbucks_WhiChoMoc <- starbucks %>% filter(product_name  == "White Chocolate Mocha")
starbucks_LatteMac <- starbucks %>% filter(product_name == "Latte Macchiato")
starbucks_CinnamnonDL <- starbucks %>% filter(product_name == "Cinnamon Dolce Latte")
starbucks_LatteMac

plot.size_calories_HotC <- ggplot(starbucks_HotCh, aes(x = size, y = calories, group = product_name)) +
  geom_col()
plot.size_calories_WhiteCM <- ggplot(starbucks_WhiChoMoc, aes(x = size, y = calories, group = product_name)) +
  geom_col()
plot.size_calories_LatteMac <- ggplot(starbucks_LatteMac, aes(x = size, y = calories, group = product_name)) +
  geom_col()
plot.size_calories_CinnamnonDL <- ggplot(starbucks_CinnamnonDL, aes(x = size, y = calories, group = product_name)) +
  geom_col()

plot.size_calories_HC
plot.size_calories_WhiteCM

plot_grid(plot.size_calories_HC, plot.size_calories_WhiteCM,
          plot.size_calories_LatteMac, plot.size_calories_CinnamnonDL,
          labels = c("Hot Chocolate","White Chocolate Mocha","starbucks_Latte_Mac","Cinnamon Dolce Latte"))


starbucks_whip0 <- starbucks %>% filter(whip == 0)
starbucks_whip1 <- starbucks %>% filter(whip == 1)
starbucks_whip0

plot.starbucks_whip0 <- ggplot(starbucks_whip0, aes( x = calories, color = size))+
  geom_density()
plot.starbucks_whip1 <- ggplot(starbucks_whip1, aes(  x= calories, color = size))+
  geom_density()

plot_grid(plot.starbucks_whip1, labels = c("whip=1"),scale = 1)


plot.whip0_calories <- ggplot(starbucks_whip0, aes( x = calories, color = size, fill = size )) +
  geom_histogram(size=.2, alpha = 0.5)+
  ggtitle("whip = 0")

plot.whip1_calories <- ggplot(starbucks_whip1, aes( x = calories, color = size, fill = size ))+
  geom_histogram(size = .2, alpha = 0.5)+
  ggtitle("whip = 0")

plot_grid(plot.whip0_calories,plot.whip1_calories,nrow = 2)

plot.whip0_calories_bymilk <- ggplot(starbucks_whip0, aes( x = calories, color = as.character(milk), fill = as.character(milk) ))+
  geom_density(alpha = 0.25, adjust = 1, lwd = 0 )+
  ggtitle("whip = 0")
plot.whip1_calories_bymilk <- ggplot(starbucks_whip1, aes( x = calories, color = as.character(milk), fill = as.character(milk) ))+
  geom_density(alpha = 0.25, adjust = 1, lwd = 0 )+
  ggtitle("whip = 1")
plot.whip0_calories_bymilk
plot_grid(plot.whip0_calories_bymilk, plot.whip1_calories_bymilk, nrow = 2)


plot.whip0_calories_2 <- ggplot(starbucks_whip0, aes( x = calories, color = size, fill = size ))+
  geom_density(alpha = 0.5, adjust = 1, lwd = 0 )+
  ggtitle("whip = 0")

plot.whip1_calories_2 <- ggplot(starbucks_whip1, aes( x = calories, color = size, fill = size ))+
  geom_density(alpha = 0.5, adjust = 1, lwd = 0 )+
  ggtitle("whip = 1")

plot_grid(plot.whip0_calories_2, plot.whip1_calories_2,nrow = 2)

plot.starbucks_whip0_ServSize <- ggplot(starbucks_whip0, aes(x = serv_size_m_l, y = calories, color = milk))+
  geom_jitter()
plot.starbucks_whip1_ServSize <- ggplot(starbucks_whip1, aes(x = serv_size_m_l, y = calories, color = milk))+
  geom_jitter()

plot_grid(plot.starbucks_whip0_ServSize, plot.starbucks_whip1_ServSize, labels = c("whip=0","whip=1"),scale = 1)



ggplot(starbucks, aes(size, calories, colour=product_name)) + 
  geom_point(aes(y=calories))

cal_size_ml <- lm(calories ~ serv_size_m_l, starbucks)
cal_size_ml_gg <- ggplot(starbucks, aes(x = serv_size_m_l, y = calories))
ggplot(starbucks, aes(x = serv_size_m_l, y = calories)) + 
  geom_smooth(method = "lm")+
  geom_point() 

cal_size_ml <- lm(calories ~ serv_size_m_l, starbucks_Caf_M)
cal_size_ml_gg <- ggplot(starbucks_Caf_M, aes(x = serv_size_m_l, y = calories))
ggplot(starbucks_Caf_M, aes(x = serv_size_m_l, y = calories)) + 
  geom_smooth(method = "lm")+
  geom_point() 

ggplot(starbucks, aes( x = serv_size_m_l , y = caffeine_mg)) + 
  geom_count()

########################################################  calories ~ whip
starbucks_short <- starbucks %>% filter(size== "short")
starbucks_tall <- starbucks %>% filter(size== "tall")
starbucks_grande <- starbucks %>% filter(size== "grande")
starbucks_venti <- starbucks %>% filter(size== "venti")

par(mfrow = c(2,2))

plot.star_short <- ggplot(starbucks_short, aes(x = whip, y = calories)) + 
  geom_count()

plot.star_tall <- ggplot(starbucks_tall, aes(x = whip, y = calories)) + 
  geom_count()

plot.star_grande <- ggplot(starbucks_grande, aes(x = whip, y = calories)) + 
  geom_count()

plot.star_venti <- ggplot(starbucks_venti, aes(x = whip, y = calories)) + 
  geom_count()

plot_grid(plot.star_short, plot.star_tall, plot.star_grande, plot.star_venti,
          labels = c("short","tall","grande","venti"))

plot.star_whip <- ggplot(starbucks, aes(x = whip, y = calories, color = size))+
  geom_count()

ggplot(starbucks , aes(x = whip, y = calories, color = size)) +
  geom_jitter(width = 0.1)

ggplot(starbucks , aes(x = size, y = calories, color = whip)) +
  geom_bin_2d()


########################################################  total_fat ~ milk ,calories and whip
plot.star_whip


plot.starbucks_whip0_totalf <- ggplot(starbucks_whip0, aes(x = total_fat_g, y = calories, color = milk))+
  geom_jitter()
plot.starbucks_whip1_totalf <- ggplot(starbucks_whip1, aes(x = total_fat_g, y = calories, color = milk))+
  geom_jitter()

plot_grid(plot.starbucks_whip0_totalf, plot.starbucks_whip1_totalf, labels = c("whip=0","whip=1"),scale = 1)

plot.starbucks_totalf <- ggplot(starbucks, aes(x = total_fat_g, y = calories, color = whip))+
  geom_count()+
  geom_smooth(method = "lm")
plot_grid(plot.starbucks_totalf)


plot.starbucks_whip1_transf <- ggplot(starbucks_whip1, aes(x = trans_fat_g, y = calories, color = milk))+
  geom_jitter()
plot.starbucks_whip1_saturf <- ggplot(starbucks_whip1, aes(x = saturated_fat_g, y = calories, color = milk))+
  geom_jitter()
plot_grid(plot.starbucks_whip1_transf, plot.starbucks_whip1_saturf, labels = c("trans_fat","saturated_fat"),nrow = 2)


starbucks_milk <- starbucks %>% group_by(milk)
starbucks_milk

plot.fat_total <- ggplot(starbucks, aes(x = milk, y = total_fat_g ))+
  geom_jitter(width = 0.15)

plot.fat_satu <- ggplot(starbucks, aes(x = milk, y = saturated_fat_g ))+
  geom_jitter(width = 0.15)

plot.fat_tran <- ggplot(starbucks, aes(x = milk, y = trans_fat_g ))+
  geom_jitter(width = 0.15)

plot.milk_chole <- ggplot(starbucks, aes(x = milk, y = cholesterol_mg ))+
  geom_jitter(width = 0.15)


plot.milk_sodi <- ggplot(starbucks, aes(x = milk, y = sodium_mg ))+
  geom_jitter(width = 0.15)
plot.milk_sodi
plot.milk_carbs <- ggplot(starbucks, aes(x = milk, y = total_carbs_g ,color = size))+
  geom_jitter(width = 0.15)
plot.milk_carbs 
plot.milk_fiber <- ggplot(starbucks, aes(x = milk, y = fiber_g ))+
  geom_jitter(width = 0.15)
plot.milk_fiber
plot.milk_sugar <- ggplot(starbucks, aes(x = milk, y = sugar_g ,color = whip))+
  geom_jitter(width = 0.15)
plot.milk_sugar
plot.size_caffeine <- ggplot(starbucks %>% filter(serv_size_m_l !=0),
                             aes(x = serv_size_m_l, y = caffeine_mg , color = milk))+
  geom_density()

plot_grid(plot.fat_total, plot.fat_satu, plot.fat_tran, plot.milk_chole, scale = 1)
plot_grid(plot.size_caffeine)
plot_grid(plot.milk_sodi, plot.milk_carbs )

summary(cal_size_ml)
par(mfrow = c(1,1))
plot(cal_size_ml)
################################################################# sodi ~ whip/fat/caffeine /sugar


plot.sodi_whip0 <- ggplot(starbucks_whip0, aes( x = sodium_mg, color = size, fill = size ))+
  geom_density(alpha = 0.5, adjust = 1, lwd = 0 )+
  ggtitle("whip = 0")

plot.sodi_whip1 <- ggplot(starbucks_whip1, aes( x = sodium_mg, color = size, fill = size ))+
  geom_density(alpha = 0.5, adjust = 1, lwd = 0 )+
  ggtitle("whip = 0")

plot_grid(plot.sodi_whip0,plot.sodi_whip1, nrow = 2)



plot.sodi_totalf <- ggplot(starbucks, aes( x = total_fat_g, y = sodium_mg)) +
  geom_count()
plot.sodi_totalf

plot.sodi_totalf_bywhip <- ggplot(starbucks, aes( x = total_fat_g, y = sodium_mg, color = whip)) +
  geom_count()
plot.sodi_totalf_bymilk <- ggplot(starbucks, aes( x = total_fat_g, y = sodium_mg, color = milk)) +
  geom_count()

plot_grid(plot.sodi_totalf_bymilk, plot.sodi_totalf_bywhip, nrow = 1)

plot.sodi_caffeine_bywhip <- ggplot(starbucks, aes( x = caffeine_mg, y = sodium_mg, color = whip)) +
  geom_jitter()

plot.sodi_caffeine_bymilk <- ggplot(starbucks, aes( x = caffeine_mg, y = sodium_mg, color = milk)) +
  geom_jitter()

plot_grid(plot.sodi_caffeine_bywhip, plot.sodi_caffeine_bymilk, nrow = 1)


plot.sodi_sugar_bywhip <- ggplot(starbucks_milk_0, aes( x = sugar_g, y = sodium_mg, color = whip)) +
  geom_count()

plot.sodi_sugar_bymilk <- ggplot(starbucks_milk_0, aes( x = sugar_g, y = sodium_mg, color = milk)) +
  geom_count()

plot_grid(plot.sodi_sugar_bywhip, plot.sodi_sugar_bymilk, nrow = 1)



plot.sodi_sugar_bywhip_1 <- ggplot(starbucks_milk, aes( x = sugar_g, y = sodium_mg, color = whip)) +
  geom_count()

plot.sodi_sugar_bymilk_1 <- ggplot(starbucks_milk, aes( x = sugar_g, y = sodium_mg, color = milk)) +
  geom_count()

plot_grid(plot.sodi_sugar_bywhip_1, plot.sodi_sugar_bymilk_1, nrow = 1)

################################################################# sugar ~ calories

plot.sugar_calories_bywhip <- ggplot( starbucks, aes( x = sugar_g, y = calories, color = whip)) +
  geom_jitter()

plot.sugar_calories_bymilk <- ggplot( starbucks, aes( x = sugar_g, y = calories, color = milk)) +
  geom_jitter()

plot_grid( plot.sugar_calories_bywhip, plot.sugar_calories_bymilk )


################################################################# fiber ~ calories/sugar/sodi

starbucks_grande


plot.fiber_calories_bywhip <- ggplot( starbucks_grande, aes( x = fiber_g, y = calories, color = whip)) +
  geom_jitter()

plot.fiber_calories_bymilk <- ggplot( starbucks_grande, aes( x = fiber_g, y = calories, color = milk)) +
  geom_jitter()

plot_grid( plot.fiber_calories_bywhip, plot.fiber_calories_bymilk )

################################################################# lm analyze

library(gt)
library(car)
library(pixiedust)
library(kableExtra)

mydata1 <- starbucks[,5:15]
cup1 <- mydata1
mydata1[,1] <- cup1[,2]
mydata1[,2] <- cup1[,1]
colnames(mydata1)[1] <- "calories"
colnames(mydata1)[2] <- "serv_size_m_l"
mydata1[,5:11] <- sapply( mydata1[,5:11], as.numeric)



mydata1 %>% ncol()
c=cor(mydata1[,1:3],use = "pairwise.complete.obs")
c
scatterplotMatrix(mydata1[,1:5],spread = F,lty.smooth=2,main='相关性矩阵图')

scatterplotMatrix(mydata1[,1:11],spread = F,lty.smooth=2,main='相关性矩阵图')


lm_cal <- lm( calories ~ , mydata1)
summary(lm_cal)

mydata1
mean(mydata1$calories)
mean(mydata1$total_fat_g)


summary(mydata1[,9:11])

vif(lm_cal)

lm_cal_pro <- lm( calories ~ saturated_fat_g + trans_fat_g + sodium_mg + sugar_g + cholesterol_mg + fiber_g, mydata1 )

summary(lm_cal_pro)

vif(lm_cal_pro)
