
library(pwr)
pwr.r.test(r = 0.1, power = 0.8) #r=.1をターゲット
pwr.r.test(r = 0.2, power = 0.8) #r=.2をターゲット
pwr.r.test(r = 0.3, power = 0.8) #r=.3をターゲット
pwr.r.test(r = 0.4, power = 0.8) #r=.4をターゲット

#（1）前処理 ----
library(tidyverse)

#メモ：欠損値の処理はリストワイズにする

##1-1. ローデータ読み込み ----
lowdata <- read_csv("lowdata_4timepoints.csv", na = c(".", ""))
lowdata$gender_T1 <- factor(lowdata$gender_T1) #性別をfactor型に変換
lowdata$gender_T2 <- factor(lowdata$gender_T2) #性別をfactor型に変換
lowdata$gender_T3 <- factor(lowdata$gender_T3) #性別をfactor型に変換
lowdata$gender_T4 <- factor(lowdata$gender_T4) #性別をfactor型に変換
names(lowdata) #変数名確認
head(lowdata) #先頭6行確認



##1-2. 下位尺度得点の算出 ----
data <- lowdata %>% 
  dplyr::mutate(eoe_T1 = (hsc4_T1 + hsc6_T1 + hsc8_T1 + hsc9_T1 + hsc12_T1)/5, na.rm = TRUE) %>% #EOE_T1の平均
  dplyr::mutate(lst_T1 = (hsc2_T1 + hsc11_T1)/2, na.rm = TRUE) %>% #LST_T1の平均
  dplyr::mutate(aes_T1 = (hsc5_T1 + hsc10_T1 + hsc1_T1 + hsc3_T1)/4, na.rm = TRUE) %>% #AES_T1の平均
  dplyr::mutate(hsc_T1 = (eoe_T1 + lst_T1 + aes_T1)/3, na.rm = TRUE) %>% #HSC_T1の平均
  dplyr::mutate(eoe_T2 = (hsc4_T2 + hsc6_T2 + hsc8_T2 + hsc9_T2 + hsc12_T2)/5, na.rm = TRUE) %>% #EOE_T2の平均
  dplyr::mutate(lst_T2 = (hsc2_T2 + hsc11_T2)/2, na.rm = TRUE) %>% #LST_T2の平均
  dplyr::mutate(aes_T2 = (hsc5_T2 + hsc10_T2 + hsc1_T2 + hsc3_T2)/4, na.rm = TRUE) %>% #AES_T2の平均
  dplyr::mutate(hsc_T2 = (eoe_T2 + lst_T2 + aes_T2)/3, na.rm = TRUE)  %>% #HSC_T2の平均
  dplyr::mutate(eoe_T3 = (hsc4_T3 + hsc6_T3 + hsc8_T3 + hsc9_T3 + hsc12_T3)/5, na.rm = TRUE) %>% #EOE_T3の平均
  dplyr::mutate(lst_T3 = (hsc2_T3 + hsc11_T3)/2, na.rm = TRUE) %>% #LST_T3の平均
  dplyr::mutate(aes_T3 = (hsc5_T3 + hsc10_T3 + hsc1_T3 + hsc3_T3)/4, na.rm = TRUE) %>% #AES_T3の平均
  dplyr::mutate(hsc_T3 = (eoe_T3 + lst_T3 + aes_T3)/3, na.rm = TRUE) %>% #HSC_T3の平均
  dplyr::mutate(eoe_T4 = (hsc4_T4 + hsc6_T4 + hsc8_T4 + hsc9_T4 + hsc12_T4)/5, na.rm = TRUE) %>% #EOE_T4の平均
  dplyr::mutate(lst_T4 = (hsc2_T4 + hsc11_T4)/2, na.rm = TRUE) %>% #LST_T4の平均
  dplyr::mutate(aes_T4 = (hsc5_T4 + hsc10_T4 + hsc1_T4 + hsc3_T4)/4, na.rm = TRUE) %>% #AES_T4の平均
  dplyr::mutate(hsc_T4 = (eoe_T4 + lst_T4 + aes_T4)/3, na.rm = TRUE) %>% #HSC_T4の平均
  dplyr::mutate(hsc_onemonth = (hsc_T1 + hsc_T2 + hsc_T3 + hsc_T4)/4, na.rm = TRUE) %>% #1ヵ月間のHSC平均
  dplyr::mutate(wb_T1 = (wb1_T1 + wb2_T1 + wb3_T1 + wb4_T1 + wb5_T1)/5, na.rm = TRUE) %>% #wb_T1の平均
  dplyr::mutate(wb_T2 = (wb1_T2 + wb2_T2 + wb3_T2 + wb4_T2 + wb5_T2)/5, na.rm = TRUE) %>% #wb_T2の平均
  dplyr::mutate(wb_T3 = (wb1_T3 + wb2_T3 + wb3_T3 + wb4_T3 + wb5_T3)/5, na.rm = TRUE) %>% #wb_T3の平均
  dplyr::mutate(wb_T4 = (wb1_T4 + wb2_T4 + wb3_T4 + wb4_T4 + wb5_T4)/5, na.rm = TRUE) %>% #wb_T4の平均
  dplyr::mutate(wb_onemonth = (wb_T1 + wb_T2 + wb_T3 + wb_T4)/4, na.rm = TRUE) %>% #1か月間のwb平均
  dplyr::mutate(ev_T1 = (as.numeric(ev1_T1) + as.numeric(ev2_T1))/2, na.rm = TRUE) %>% #event_T1の平均 #as.numericにしないとエラーが出る
  dplyr::mutate(ev_T2 = (as.numeric(ev1_T2) + as.numeric(ev2_T2))/2, na.rm = TRUE) %>% #event_T2の平均
  dplyr::mutate(ev_T3 = (as.numeric(ev1_T3) + as.numeric(ev2_T3))/2, na.rm = TRUE) %>% #event_T3の平均
  dplyr::mutate(ev_T4 = (as.numeric(ev1_T4) + as.numeric(ev2_T4))/2, na.rm = TRUE) %>% #event_T4の平均
  dplyr::mutate(ev_onemonth = (ev_T1 + ev_T2 + ev_T3 + ev_T4)/4, na.rm = TRUE) %>% #1か月間のev平均
  dplyr::select(-na.rm) #謎にna.rmという変数が勝手に作成されてしまうのでそれを除外

head(data) #先頭6行確認
names(data) #変数名確認
write.csv(data, file = "data_for_analysis.csv", na = ".") #csvで書き出し

##1-3. ヒストグラム ----

#性別T1の度数分布とヒストグラム
gender_T1_count <- dplyr::count(data, gender_T1)
knitr::kable(gender_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = gender_T1, fill = factor(gender_T1))) + geom_bar() #視覚化

#性別T2の度数分布とヒストグラム
gender_T2_count <- dplyr::count(data, gender_T2)
knitr::kable(gender_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = gender_T2, fill = factor(gender_T2))) + geom_bar() #視覚化

#性別T3の度数分布とヒストグラム
gender_T3_count <- dplyr::count(data, gender_T3)
knitr::kable(gender_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = gender_T3, fill = factor(gender_T3))) + geom_bar() #視覚化

#性別T4の度数分布とヒストグラム
gender_T4_count <- dplyr::count(data, gender_T4)
knitr::kable(gender_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = gender_T4, fill = factor(gender_T4))) + geom_bar() #視覚化

#hsc1_T1の度数分布とヒストグラム
hsc1_T1_count <- dplyr::count(data, hsc1_T1)
knitr::kable(hsc1_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc1_T1, fill = factor(hsc1_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc2_T1の度数分布とヒストグラム
hsc2_T1_count <- dplyr::count(data, hsc2_T1)
knitr::kable(hsc2_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc2_T1, fill = factor(hsc2_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc3_T1の度数分布とヒストグラム
hsc3_T1_count <- dplyr::count(data, hsc3_T1)
knitr::kable(hsc3_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc3_T1, fill = factor(hsc3_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc4_T1の度数分布とヒストグラム
hsc4_T1_count <- dplyr::count(data, hsc4_T1)
knitr::kable(hsc4_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc4_T1, fill = factor(hsc4_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc5_T1の度数分布とヒストグラム
hsc5_T1_count <- dplyr::count(data, hsc5_T1)
knitr::kable(hsc5_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc5_T1, fill = factor(hsc5_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc6_T1の度数分布とヒストグラム
hsc6_T1_count <- dplyr::count(data, hsc6_T1)
knitr::kable(hsc6_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc6_T1, fill = factor(hsc6_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc7_T1の度数分布とヒストグラム
hsc7_T1_count <- dplyr::count(data, hsc7_T1)
knitr::kable(hsc7_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc7_T1, fill = factor(hsc7_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc8_T1の度数分布とヒストグラム
hsc8_T1_count <- dplyr::count(data, hsc8_T1)
knitr::kable(hsc8_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc8_T1, fill = factor(hsc8_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc9_T1の度数分布とヒストグラム
hsc9_T1_count <- dplyr::count(data, hsc9_T1)
knitr::kable(hsc9_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc9_T1, fill = factor(hsc9_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc10_T1の度数分布とヒストグラム
hsc10_T1_count <- dplyr::count(data, hsc10_T1)
knitr::kable(hsc10_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc10_T1, fill = factor(hsc10_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc11_T1の度数分布とヒストグラム
hsc11_T1_count <- dplyr::count(data, hsc11_T1)
knitr::kable(hsc11_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc11_T1, fill = factor(hsc11_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc12_T1の度数分布とヒストグラム
hsc12_T1_count <- dplyr::count(data, hsc12_T1)
knitr::kable(hsc12_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc12_T1, fill = factor(hsc12_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc1_T2の度数分布とヒストグラム
hsc1_T2_count <- dplyr::count(data, hsc1_T2)
knitr::kable(hsc1_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc1_T2, fill = factor(hsc1_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc2_T2の度数分布とヒストグラム
hsc2_T2_count <- dplyr::count(data, hsc2_T2)
knitr::kable(hsc2_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc2_T2, fill = factor(hsc2_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc3_T2の度数分布とヒストグラム
hsc3_T2_count <- dplyr::count(data, hsc3_T2)
knitr::kable(hsc3_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc3_T2, fill = factor(hsc3_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc4_T2の度数分布とヒストグラム
hsc4_T2_count <- dplyr::count(data, hsc4_T2)
knitr::kable(hsc4_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc4_T2, fill = factor(hsc4_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc5_T2の度数分布とヒストグラム
hsc5_T2_count <- dplyr::count(data, hsc5_T2)
knitr::kable(hsc5_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc5_T2, fill = factor(hsc5_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc6_T2の度数分布とヒストグラム
hsc6_T2_count <- dplyr::count(data, hsc6_T2)
knitr::kable(hsc6_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc6_T2, fill = factor(hsc6_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc7_T2の度数分布とヒストグラム
hsc7_T2_count <- dplyr::count(data, hsc7_T2)
knitr::kable(hsc7_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc7_T2, fill = factor(hsc7_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc8_T2の度数分布とヒストグラム
hsc8_T2_count <- dplyr::count(data, hsc8_T2)
knitr::kable(hsc8_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc8_T2, fill = factor(hsc8_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc9_T2の度数分布とヒストグラム
hsc9_T2_count <- dplyr::count(data, hsc9_T2)
knitr::kable(hsc9_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc9_T2, fill = factor(hsc9_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc10_T2の度数分布とヒストグラム
hsc10_T2_count <- dplyr::count(data, hsc10_T2)
knitr::kable(hsc10_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc10_T2, fill = factor(hsc10_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc11_T2の度数分布とヒストグラム
hsc11_T2_count <- dplyr::count(data, hsc11_T2)
knitr::kable(hsc11_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc11_T2, fill = factor(hsc11_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc12_T2の度数分布とヒストグラム
hsc12_T2_count <- dplyr::count(data, hsc12_T2)
knitr::kable(hsc12_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc12_T2, fill = factor(hsc12_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc1_T3の度数分布とヒストグラム
hsc1_T3_count <- dplyr::count(data, hsc1_T3)
knitr::kable(hsc1_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc1_T3, fill = factor(hsc1_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc2_T3の度数分布とヒストグラム
hsc2_T3_count <- dplyr::count(data, hsc2_T3)
knitr::kable(hsc2_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc2_T3, fill = factor(hsc2_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc3_T3の度数分布とヒストグラム
hsc3_T3_count <- dplyr::count(data, hsc3_T3)
knitr::kable(hsc3_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc3_T3, fill = factor(hsc3_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc4_T3の度数分布とヒストグラム
hsc4_T3_count <- dplyr::count(data, hsc4_T3)
knitr::kable(hsc4_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc4_T3, fill = factor(hsc4_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc5_T3の度数分布とヒストグラム
hsc5_T3_count <- dplyr::count(data, hsc5_T3)
knitr::kable(hsc5_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc5_T3, fill = factor(hsc5_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc6_T3の度数分布とヒストグラム
hsc6_T3_count <- dplyr::count(data, hsc6_T3)
knitr::kable(hsc6_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc6_T3, fill = factor(hsc6_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc7_T3の度数分布とヒストグラム
hsc7_T3_count <- dplyr::count(data, hsc7_T3)
knitr::kable(hsc7_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc7_T3, fill = factor(hsc7_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc8_T3の度数分布とヒストグラム
hsc8_T3_count <- dplyr::count(data, hsc8_T3)
knitr::kable(hsc8_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc8_T3, fill = factor(hsc8_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc9_T3の度数分布とヒストグラム
hsc9_T3_count <- dplyr::count(data, hsc9_T3)
knitr::kable(hsc9_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc9_T3, fill = factor(hsc9_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc10_T3の度数分布とヒストグラム
hsc10_T3_count <- dplyr::count(data, hsc10_T3)
knitr::kable(hsc10_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc10_T3, fill = factor(hsc10_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc11_T3の度数分布とヒストグラム
hsc11_T3_count <- dplyr::count(data, hsc11_T3)
knitr::kable(hsc11_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc11_T3, fill = factor(hsc11_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc12_T3の度数分布とヒストグラム
hsc12_T3_count <- dplyr::count(data, hsc12_T3)
knitr::kable(hsc12_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc12_T3, fill = factor(hsc12_T3))) + geom_histogram(binwidth = 1) #視覚化

#hsc1_T4の度数分布とヒストグラム
hsc1_T4_count <- dplyr::count(data, hsc1_T4)
knitr::kable(hsc1_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc1_T4, fill = factor(hsc1_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc2_T4の度数分布とヒストグラム
hsc2_T4_count <- dplyr::count(data, hsc2_T4)
knitr::kable(hsc2_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc2_T4, fill = factor(hsc2_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc3_T4の度数分布とヒストグラム
hsc3_T4_count <- dplyr::count(data, hsc3_T4)
knitr::kable(hsc3_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc3_T4, fill = factor(hsc3_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc4_T4の度数分布とヒストグラム
hsc4_T4_count <- dplyr::count(data, hsc4_T4)
knitr::kable(hsc4_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc4_T4, fill = factor(hsc4_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc5_T4の度数分布とヒストグラム
hsc5_T4_count <- dplyr::count(data, hsc5_T4)
knitr::kable(hsc5_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc5_T4, fill = factor(hsc5_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc6_T4の度数分布とヒストグラム
hsc6_T4_count <- dplyr::count(data, hsc6_T4)
knitr::kable(hsc6_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc6_T4, fill = factor(hsc6_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc7_T4の度数分布とヒストグラム
hsc7_T4_count <- dplyr::count(data, hsc7_T4)
knitr::kable(hsc7_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc7_T4, fill = factor(hsc7_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc8_T4の度数分布とヒストグラム
hsc8_T4_count <- dplyr::count(data, hsc8_T4)
knitr::kable(hsc8_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc8_T4, fill = factor(hsc8_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc9_T4の度数分布とヒストグラム
hsc9_T4_count <- dplyr::count(data, hsc9_T4)
knitr::kable(hsc9_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc9_T4, fill = factor(hsc9_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc10_T4の度数分布とヒストグラム
hsc10_T4_count <- dplyr::count(data, hsc10_T4)
knitr::kable(hsc10_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc10_T4, fill = factor(hsc10_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc11_T4の度数分布とヒストグラム
hsc11_T4_count <- dplyr::count(data, hsc11_T4)
knitr::kable(hsc11_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc11_T4, fill = factor(hsc11_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc12_T4の度数分布とヒストグラム
hsc12_T4_count <- dplyr::count(data, hsc12_T4)
knitr::kable(hsc12_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc12_T4, fill = factor(hsc12_T4))) + geom_histogram(binwidth = 1) #視覚化

#wb1_T1の度数分布とヒストグラム
wb1_T1_count <- dplyr::count(data, wb1_T1)
knitr::kable(wb1_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb1_T1, fill = factor(wb1_T1))) + geom_histogram(binwidth = 1) #視覚化

#wb2_T1の度数分布とヒストグラム
wb2_T1_count <- dplyr::count(data, wb2_T1)
knitr::kable(wb2_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb2_T1, fill = factor(wb2_T1))) + geom_histogram(binwidth = 1) #視覚化

#wb3_T1の度数分布とヒストグラム
wb3_T1_count <- dplyr::count(data, wb3_T1)
knitr::kable(wb3_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb3_T1, fill = factor(wb3_T1))) + geom_histogram(binwidth = 1) #視覚化

#wb4_T1の度数分布とヒストグラム
wb4_T1_count <- dplyr::count(data, wb4_T1)
knitr::kable(wb4_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb4_T1, fill = factor(wb4_T1))) + geom_histogram(binwidth = 1) #視覚化

#wb5_T1の度数分布とヒストグラム
wb5_T1_count <- dplyr::count(data, wb5_T1)
knitr::kable(wb5_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb5_T1, fill = factor(wb5_T1))) + geom_histogram(binwidth = 1) #視覚化

#wb1_T2の度数分布とヒストグラム
wb1_T2_count <- dplyr::count(data, wb1_T2)
knitr::kable(wb1_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb1_T2, fill = factor(wb1_T2))) + geom_histogram(binwidth = 1) #視覚化

#wb2_T2の度数分布とヒストグラム
wb2_T2_count <- dplyr::count(data, wb2_T2)
knitr::kable(wb2_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb2_T2, fill = factor(wb2_T2))) + geom_histogram(binwidth = 1) #視覚化

#wb3_T2の度数分布とヒストグラム
wb3_T2_count <- dplyr::count(data, wb3_T2)
knitr::kable(wb3_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb3_T2, fill = factor(wb3_T2))) + geom_histogram(binwidth = 1) #視覚化

#wb4_T2の度数分布とヒストグラム
wb4_T2_count <- dplyr::count(data, wb4_T2)
knitr::kable(wb4_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb4_T2, fill = factor(wb4_T2))) + geom_histogram(binwidth = 1) #視覚化

#wb5_T2の度数分布とヒストグラム
wb5_T2_count <- dplyr::count(data, wb5_T2)
knitr::kable(wb5_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb5_T2, fill = factor(wb5_T2))) + geom_histogram(binwidth = 1) #視覚化

#wb1_T3の度数分布とヒストグラム
wb1_T3_count <- dplyr::count(data, wb1_T3)
knitr::kable(wb1_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb1_T3, fill = factor(wb1_T3))) + geom_histogram(binwidth = 1) #視覚化

#wb2_T3の度数分布とヒストグラム
wb2_T3_count <- dplyr::count(data, wb2_T3)
knitr::kable(wb2_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb2_T3, fill = factor(wb2_T3))) + geom_histogram(binwidth = 1) #視覚化

#wb3_T3の度数分布とヒストグラム
wb3_T3_count <- dplyr::count(data, wb3_T3)
knitr::kable(wb1_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb3_T3, fill = factor(wb3_T3))) + geom_histogram(binwidth = 1) #視覚化

#wb4_T3の度数分布とヒストグラム
wb4_T3_count <- dplyr::count(data, wb4_T3)
knitr::kable(wb4_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb4_T3, fill = factor(wb4_T3))) + geom_histogram(binwidth = 1) #視覚化

#wb5_T3の度数分布とヒストグラム
wb5_T3_count <- dplyr::count(data, wb5_T3)
knitr::kable(wb5_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb5_T3, fill = factor(wb5_T3))) + geom_histogram(binwidth = 1) #視覚化

#wb1_T4の度数分布とヒストグラム
wb1_T4_count <- dplyr::count(data, wb1_T4)
knitr::kable(wb1_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb1_T4, fill = factor(wb1_T4))) + geom_histogram(binwidth = 1) #視覚化

#wb2_T4の度数分布とヒストグラム
wb2_T4_count <- dplyr::count(data, wb2_T4)
knitr::kable(wb2_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb2_T4, fill = factor(wb2_T4))) + geom_histogram(binwidth = 1) #視覚化

#wb3_T4の度数分布とヒストグラム
wb3_T4_count <- dplyr::count(data, wb3_T4)
knitr::kable(wb3_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb3_T4, fill = factor(wb3_T4))) + geom_histogram(binwidth = 1) #視覚化

#wb5_T4の度数分布とヒストグラム
wb5_T4_count <- dplyr::count(data, wb5_T4)
knitr::kable(wb5_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb5_T4, fill = factor(wb5_T4))) + geom_histogram(binwidth = 1) #視覚化

#event1_T1の度数分布とヒストグラム
event1_T1_count <- dplyr::count(data, ev1_T1)
knitr::kable(event1_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev1_T1, fill = factor(ev1_T1))) + geom_histogram(binwidth = 1) #視覚化

#event2_T1の度数分布とヒストグラム
event2_T1_count <- dplyr::count(data, ev2_T1)
knitr::kable(event2_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev2_T1, fill = factor(ev2_T1))) + geom_histogram(binwidth = 1) #視覚化

#event1_T2の度数分布とヒストグラム
event1_T2_count <- dplyr::count(data, ev1_T2)
knitr::kable(event1_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev1_T2, fill = factor(ev1_T2))) + geom_histogram(binwidth = 1) #視覚化

#event2_T2の度数分布とヒストグラム
event2_T2_count <- dplyr::count(data, ev2_T2)
knitr::kable(event2_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev2_T2, fill = factor(ev2_T2))) + geom_histogram(binwidth = 1) #視覚化

#event1_T3の度数分布とヒストグラム
event1_T3_count <- dplyr::count(data, ev1_T3)
knitr::kable(event1_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev1_T3, fill = factor(ev1_T3))) + geom_histogram(binwidth = 1) #視覚化

#event2_T3の度数分布とヒストグラム
event2_T3_count <- dplyr::count(data, ev2_T3)
knitr::kable(event2_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev2_T3, fill = factor(ev2_T3))) + geom_histogram(binwidth = 1) #視覚化

#event1_T4の度数分布とヒストグラム
event1_T4_count <- dplyr::count(data, ev1_T4)
knitr::kable(event1_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev1_T4, fill = factor(ev1_T4))) + geom_histogram(binwidth = 1) #視覚化

#event2_T4の度数分布とヒストグラム
event2_T4_count <- dplyr::count(data, ev2_T4)
knitr::kable(event2_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev2_T4, fill = factor(ev2_T4))) + geom_histogram(binwidth = 1) #視覚化

#eoe_T1の度数分布とヒストグラム
eoe_T1_count <- dplyr::count(data, eoe_T1)
knitr::kable(eoe_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = eoe_T1, fill = factor(eoe_T1))) + geom_histogram(binwidth = 1) #視覚化

#eoe_T2の度数分布とヒストグラム
eoe_T2_count <- dplyr::count(data, eoe_T2)
knitr::kable(eoe_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = eoe_T2, fill = factor(eoe_T2))) + geom_histogram(binwidth = 1) #視覚化

#eoe_T3の度数分布とヒストグラム
eoe_T3_count <- dplyr::count(data, eoe_T3)
knitr::kable(eoe_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = eoe_T3, fill = factor(eoe_T3))) + geom_histogram(binwidth = 1) #視覚化

#eoe_T4の度数分布とヒストグラム
eoe_T4_count <- dplyr::count(data, eoe_T4)
knitr::kable(eoe_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = eoe_T4, fill = factor(eoe_T4))) + geom_histogram(binwidth = 1) #視覚化

#lst_T1の度数分布とヒストグラム
lst_T1_count <- dplyr::count(data, lst_T1)
knitr::kable(lst_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = lst_T1, fill = factor(lst_T1))) + geom_histogram(binwidth = 1) #視覚化

#lst_T2の度数分布とヒストグラム
lst_T2_count <- dplyr::count(data, lst_T2)
knitr::kable(lst_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = lst_T2, fill = factor(lst_T2))) + geom_histogram(binwidth = 1) #視覚化

#lst_T3の度数分布とヒストグラム
lst_T3_count <- dplyr::count(data, lst_T3)
knitr::kable(lst_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = lst_T3, fill = factor(lst_T3))) + geom_histogram(binwidth = 1) #視覚化

#lst_T4の度数分布とヒストグラム
lst_T4_count <- dplyr::count(data, lst_T4)
knitr::kable(lst_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = lst_T4, fill = factor(lst_T4))) + geom_histogram(binwidth = 1) #視覚化

#aes_T1の度数分布とヒストグラム
aes_T1_count <- dplyr::count(data, aes_T1)
knitr::kable(aes_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = aes_T1, fill = factor(aes_T1))) + geom_histogram(binwidth = 1) #視覚化

#aes_T2の度数分布とヒストグラム
aes_T2_count <- dplyr::count(data, aes_T2)
knitr::kable(aes_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = aes_T2, fill = factor(aes_T2))) + geom_histogram(binwidth = 1) #視覚化

#aes_T3の度数分布とヒストグラム
aes_T3_count <- dplyr::count(data, aes_T3)
knitr::kable(aes_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = aes_T3, fill = factor(aes_T3))) + geom_histogram(binwidth = 1) #視覚化

#aes_T4の度数分布とヒストグラム
aes_T4_count <- dplyr::count(data, aes_T4)
knitr::kable(aes_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = aes_T4, fill = factor(aes_T4))) + geom_histogram(binwidth = 1) #視覚化

#hsc_T1の度数分布とヒストグラム
hsc_T1_count <- dplyr::count(data, hsc_T1)
knitr::kable(hsc_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc_T1, fill = factor(hsc_T1))) + geom_histogram(binwidth = 0.1) #視覚化

#hsc_T2の度数分布とヒストグラム
hsc_T2_count <- dplyr::count(data, hsc_T2)
knitr::kable(hsc_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc_T2, fill = factor(hsc_T2))) + geom_histogram(binwidth = 0.1) #視覚化

#hsc_T3の度数分布とヒストグラム
hsc_T3_count <- dplyr::count(data, hsc_T3)
knitr::kable(hsc_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc_T3, fill = factor(hsc_T3))) + geom_histogram(binwidth = 0.1) #視覚化

#hsc_T4の度数分布とヒストグラム
hsc_T4_count <- dplyr::count(data, hsc_T4)
knitr::kable(hsc_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc_T4, fill = factor(hsc_T4))) + geom_histogram(binwidth = 0.1) #視覚化

#wb_T1の度数分布とヒストグラム
wb_T1_count <- dplyr::count(data, wb_T1)
knitr::kable(wb_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb_T1, fill = factor(wb_T1))) + geom_histogram(binwidth = 0.5) #視覚化

#wb_T2の度数分布とヒストグラム
wb_T2_count <- dplyr::count(data, wb_T2)
knitr::kable(wb_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb_T2, fill = factor(wb_T2))) + geom_histogram(binwidth = 0.5) #視覚化

#wb_T3の度数分布とヒストグラム
wb_T3_count <- dplyr::count(data, wb_T3)
knitr::kable(wb_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb_T3, fill = factor(wb_T3))) + geom_histogram(binwidth = 0.5) #視覚化

#wb_T4の度数分布とヒストグラム
wb_T4_count <- dplyr::count(data, wb_T4)
knitr::kable(wb_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb_T4, fill = factor(wb_T4))) + geom_histogram(binwidth = 0.5) #視覚化

#ev_T1の度数分布とヒストグラム
ev_T1_count <- dplyr::count(data, ev_T1)
knitr::kable(ev_T1_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev_T1, fill = factor(ev_T1))) + geom_histogram(binwidth = 0.5) #視覚化

#ev_T2の度数分布とヒストグラム
ev_T2_count <- dplyr::count(data, ev_T2)
knitr::kable(ev_T2_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev_T2, fill = factor(ev_T2))) + geom_histogram(binwidth = 0.5) #視覚化

#ev_T3の度数分布とヒストグラム
ev_T3_count <- dplyr::count(data, ev_T3)
knitr::kable(ev_T3_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev_T3, fill = factor(ev_T3))) + geom_histogram(binwidth = 0.5) #視覚化

#ev_T4の度数分布とヒストグラム
ev_T4_count <- dplyr::count(data, ev_T4)
knitr::kable(ev_T4_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev_T4, fill = factor(ev_T4))) + geom_histogram(binwidth = 0.5) #視覚化

#hsc_onemonthの度数分布とヒストグラム
hsc_onemonth_count <- dplyr::count(data, hsc_onemonth)
knitr::kable(hsc_onemonth_count) #テーブル化
ggplot(data = data, mapping = aes(x = hsc_onemonth, fill = factor(hsc_onemonth))) + geom_histogram(binwidth = 0.3) + guides(fill = "none") #視覚化

#wb_onemonthの度数分布とヒストグラム
wb_onemonth_count <- dplyr::count(data, wb_onemonth)
knitr::kable(wb_onemonth_count) #テーブル化
ggplot(data = data, mapping = aes(x = wb_onemonth, fill = factor(wb_onemonth))) + geom_histogram(binwidth = 0.3) + guides(fill = "none") #視覚化

#wb_onemonthの度数分布とヒストグラム
ev_onemonth_count <- dplyr::count(data, ev_onemonth)
knitr::kable(ev_onemonth_count) #テーブル化
ggplot(data = data, mapping = aes(x = ev_onemonth, fill = factor(ev_onemonth))) + geom_histogram(binwidth = 0.3) + guides(fill = "none") #視覚化

##1-4. 記述統計量 ----

#hsc_T1
hsc_T1_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc1.T1.mean = mean (hsc1_T1), #hsc1_T1の平均
                   hsc1.T1.sd = sd (hsc1_T1), #hsc1_T1のSD
                   hsc2.T1.mean = mean (hsc2_T1), 
                   hsc2.T1.sd = sd (hsc2_T1),
                   hsc3.T1.mean = mean (hsc3_T1), 
                   hsc3.T1.sd = sd (hsc3_T1),
                   hsc4.T1.mean = mean (hsc4_T1), 
                   hsc4.T1.sd = sd (hsc4_T1),
                   hsc5.T1.mean = mean (hsc5_T1), 
                   hsc5.T1.sd = sd (hsc5_T1),
                   hsc6.T1.mean = mean (hsc6_T1), 
                   hsc6.T1.sd = sd (hsc6_T1),
                   hsc7.T1.mean = mean (hsc7_T1), 
                   hsc7.T1.sd = sd (hsc7_T1),
                   hsc8.T1.mean = mean (hsc8_T1), 
                   hsc8.T1.sd = sd (hsc8_T1),
                   hsc9.T1.mean = mean (hsc9_T1), 
                   hsc9.T1.sd = sd (hsc9_T1),
                   hsc10.T1.mean = mean (hsc10_T1), 
                   hsc10.T1.sd = sd (hsc10_T1),
                   hsc11.T1.mean = mean (hsc11_T1), 
                   hsc11.T1.sd = sd (hsc11_T1),
                   hsc12.T1.mean = mean (hsc4_T1), 
                   hsc12.T1.sd = sd (hsc4_T1),
                   eoe.mean.T1 = mean (eoe_T1),
                   eoe.sd.T1 = sd (eoe_T1),
                   lst.mean.T1 = mean (lst_T1),
                   lst.sd.T1 = sd (lst_T1),
                   aes.mean.T1 = mean (aes_T1),
                   aes.sd.T1 = sd (aes_T1),
                   hsc.mean.T1 = mean (hsc_T1),
                   hsc.sd.T1 = sd (hsc_T1))
knitr::kable(hsc_T1_discriptive, digits = 2) #出力

#hsc_T2
hsc_T2_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc1.T2.mean = mean (hsc1_T2), #hsc1_T2の平均
                   hsc1.T2.sd = sd (hsc1_T2), #hsc1_T2のSD
                   hsc2.T2.mean = mean (hsc2_T2), 
                   hsc2.T2.sd = sd (hsc2_T2),
                   hsc3.T2.mean = mean (hsc3_T2), 
                   hsc3.T2.sd = sd (hsc3_T2),
                   hsc4.T2.mean = mean (hsc4_T2), 
                   hsc4.T2.sd = sd (hsc4_T2),
                   hsc5.T2.mean = mean (hsc5_T2), 
                   hsc5.T2.sd = sd (hsc5_T2),
                   hsc6.T2.mean = mean (hsc6_T2), 
                   hsc6.T2.sd = sd (hsc6_T2),
                   hsc7.T2.mean = mean (hsc7_T2), 
                   hsc7.T2.sd = sd (hsc7_T2),
                   hsc8.T2.mean = mean (hsc8_T2), 
                   hsc8.T2.sd = sd (hsc8_T2),
                   hsc9.T2.mean = mean (hsc9_T2), 
                   hsc9.T2.sd = sd (hsc9_T2),
                   hsc10.T2.mean = mean (hsc10_T2), 
                   hsc10.T2.sd = sd (hsc10_T2),
                   hsc11.T2.mean = mean (hsc11_T2), 
                   hsc11.T2.sd = sd (hsc11_T2),
                   hsc12.T2.mean = mean (hsc4_T2), 
                   hsc12.T2.sd = sd (hsc4_T2),
                   eoe.mean.T2 = mean (eoe_T2),
                   eoe.sd.T2 = sd (eoe_T2),
                   lst.mean.T2 = mean (lst_T2),
                   lst.sd.T2 = sd (lst_T2),
                   aes.mean.T2 = mean (aes_T2),
                   aes.sd.T2 = sd (aes_T2),
                   hsc.mean.T2 = mean (hsc_T2),
                   hsc.sd.T2 = sd (hsc_T2)) 
knitr::kable(hsc_T2_discriptive, digits = 2) #出力

#hsc_T3
hsc_T3_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc1.T3.mean = mean (hsc1_T3), #hsc1_T3の平均
                   hsc1.T3.sd = sd (hsc1_T3), #hsc1_T3のSD
                   hsc2.T3.mean = mean (hsc2_T3), 
                   hsc2.T3.sd = sd (hsc2_T3),
                   hsc3.T3.mean = mean (hsc3_T3), 
                   hsc3.T3.sd = sd (hsc3_T3),
                   hsc4.T3.mean = mean (hsc4_T3), 
                   hsc4.T3.sd = sd (hsc4_T3),
                   hsc5.T3.mean = mean (hsc5_T3), 
                   hsc5.T3.sd = sd (hsc5_T3),
                   hsc6.T3.mean = mean (hsc6_T3), 
                   hsc6.T3.sd = sd (hsc6_T3),
                   hsc7.T3.mean = mean (hsc7_T3), 
                   hsc7.T3.sd = sd (hsc7_T3),
                   hsc8.T3.mean = mean (hsc8_T3), 
                   hsc8.T3.sd = sd (hsc8_T3),
                   hsc9.T3.mean = mean (hsc9_T3), 
                   hsc9.T3.sd = sd (hsc9_T3),
                   hsc10.T3.mean = mean (hsc10_T3), 
                   hsc10.T3.sd = sd (hsc10_T3),
                   hsc11.T3.mean = mean (hsc11_T3), 
                   hsc11.T3.sd = sd (hsc11_T3),
                   hsc12.T3.mean = mean (hsc4_T3), 
                   hsc12.T3.sd = sd (hsc4_T3),
                   eoe.mean.T3 = mean (eoe_T3),
                   eoe.sd.T3 = sd (eoe_T3),
                   lst.mean.T3 = mean (lst_T3),
                   lst.sd.T3 = sd (lst_T3),
                   aes.mean.T3 = mean (aes_T3),
                   aes.sd.T3 = sd (aes_T3),
                   hsc.mean.T3 = mean (hsc_T3),
                   hsc.sd.T3 = sd (hsc_T3)) 
knitr::kable(hsc_T3_discriptive, digits = 2) #出力

#hsc_T4
hsc_T4_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc1.T4.mean = mean (hsc1_T4), #hsc1_T4の平均
                   hsc1.T4.sd = sd (hsc1_T4), #hsc1_T4のSD
                   hsc2.T4.mean = mean (hsc2_T4), 
                   hsc2.T4.sd = sd (hsc2_T4),
                   hsc3.T4.mean = mean (hsc3_T4), 
                   hsc3.T4.sd = sd (hsc3_T4),
                   hsc4.T4.mean = mean (hsc4_T4), 
                   hsc4.T4.sd = sd (hsc4_T4),
                   hsc5.T4.mean = mean (hsc5_T4), 
                   hsc5.T4.sd = sd (hsc5_T4),
                   hsc6.T4.mean = mean (hsc6_T4), 
                   hsc6.T4.sd = sd (hsc6_T4),
                   hsc7.T4.mean = mean (hsc7_T4), 
                   hsc7.T4.sd = sd (hsc7_T4),
                   hsc8.T4.mean = mean (hsc8_T4), 
                   hsc8.T4.sd = sd (hsc8_T4),
                   hsc9.T4.mean = mean (hsc9_T4), 
                   hsc9.T4.sd = sd (hsc9_T4),
                   hsc10.T4.mean = mean (hsc10_T4), 
                   hsc10.T4.sd = sd (hsc10_T4),
                   hsc11.T4.mean = mean (hsc11_T4), 
                   hsc11.T4.sd = sd (hsc11_T4),
                   hsc12.T4.mean = mean (hsc4_T4), 
                   hsc12.T4.sd = sd (hsc4_T4),
                   eoe.mean.T4 = mean (eoe_T4),
                   eoe.sd.T4 = sd (eoe_T4),
                   lst.mean.T4 = mean (lst_T4),
                   lst.sd.T4 = sd (lst_T4),
                   aes.mean.T4 = mean (aes_T4),
                   aes.sd.T4 = sd (aes_T4),
                   hsc.mean.T4 = mean (hsc_T4),
                   hsc.sd.T4 = sd (hsc_T4)) 
knitr::kable(hsc_T4_discriptive, digits = 2) #出力

#hsc_onemonth
hsc_onemonth_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc1.onemonth.mean = mean (hsc_onemonth), #hsc_onemonthの平均
                   hsc1.onemonth.sd = sd (hsc_onemonth)) #hsc_onemonthのSD
knitr::kable(hsc_onemonth_discriptive, digits = 2) #出力

#wb_T1
wb_T1_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   wb1.T1.mean = mean (wb1_T1), #hsc1_T10の平均
                   wb1.T1.sd = sd (wb1_T1), #hsc1_T10のSD
                   wb2.T1.mean = mean (wb2_T1), 
                   wb2.T1.sd = sd (wb2_T1),
                   wb3.T1.mean = mean (wb3_T1), 
                   wb3.T1.sd = sd (wb3_T1),
                   wb4.T1.mean = mean (wb4_T1), 
                   wb4.T1.sd = sd (wb4_T1),
                   wb5.T1.mean = mean (wb5_T1), 
                   wb5.T1.sd = sd (wb5_T1),
                   wb.T1.mean = mean (wb_T1),
                   wb.T1.sd = sd (wb_T1))
knitr::kable(wb_T1_discriptive, digits = 2) #出力

#wb_T2
wb_T2_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   wb1.T2.mean = mean (wb1_T2), #hsc1_T2の平均
                   wb1.T2.sd = sd (wb1_T2), #hsc1_T2のSD
                   wb2.T2.mean = mean (wb2_T2), 
                   wb2.T2.sd = sd (wb2_T2),
                   wb3.T2.mean = mean (wb3_T2), 
                   wb3.T2.sd = sd (wb3_T2),
                   wb4.T2.mean = mean (wb4_T2), 
                   wb4.T2.sd = sd (wb4_T2),
                   wb5.T2.mean = mean (wb5_T2), 
                   wb5.T2.sd = sd (wb5_T2),
                   wb.T2.mean = mean (wb_T2),
                   wb.T2.sd = sd (wb_T2))
knitr::kable(wb_T2_discriptive, digits = 2) #出力

#wb_T3
wb_T3_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   wb1.T3.mean = mean (wb1_T3), #hsc1_T3の平均
                   wb1.T3.sd = sd (wb1_T3), #hsc1_T3のSD
                   wb2.T3.mean = mean (wb2_T3), 
                   wb2.T3.sd = sd (wb2_T3),
                   wb3.T3.mean = mean (wb3_T3), 
                   wb3.T3.sd = sd (wb3_T3),
                   wb4.T3.mean = mean (wb4_T3), 
                   wb4.T3.sd = sd (wb4_T3),
                   wb5.T3.mean = mean (wb5_T3), 
                   wb5.T3.sd = sd (wb5_T3),
                   wb.T3.mean = mean (wb_T3),
                   wb.T3.sd = sd (wb_T3))
knitr::kable(wb_T3_discriptive, digits = 2) #出力

#wb_T4
wb_T4_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   wb1.T4.mean = mean (wb1_T4), #hsc1_T4の平均
                   wb1.T4.sd = sd (wb1_T4), #hsc1_T4のSD
                   wb2.T4.mean = mean (wb2_T4), 
                   wb2.T4.sd = sd (wb2_T4),
                   wb3.T4.mean = mean (wb3_T4), 
                   wb3.T4.sd = sd (wb3_T4),
                   wb4.T4.mean = mean (wb4_T4), 
                   wb4.T4.sd = sd (wb4_T4),
                   wb5.T4.mean = mean (wb5_T4), 
                   wb5.T4.sd = sd (wb5_T4),
                   wb.T4.mean = mean (wb_T4),
                   wb.T4.sd = sd (wb_T4))
knitr::kable(wb_T4_discriptive, digits = 2) #出力

#wb_onemonth
wb_onemonth_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   wb.onemonth.mean = mean (wb_onemonth), #wb_onemonthの平均
                   wb.onemonth.sd = sd (wb_onemonth)) #wb_onemonthのSD
knitr::kable(wb_onemonth_discriptive, digits = 2) #出力

#ev_T1
ev_T1_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   ev1.T1.mean = mean (ev1_T1), #ev1_T1の平均
                   ev1.T1.sd = sd (ev1_T1), #ev1_T1のSD
                   ev2.T1.mean = mean (ev2_T1), 
                   ev2.T1.sd = sd (ev2_T1),
                   ev.T1.mean = mean (ev_T1),
                   ev.T1.sd = sd (ev_T1))
knitr::kable(ev_T1_discriptive, digits = 2) #出力

#ev_T2
ev_T2_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   ev1.T2.mean = mean (ev1_T2), #ev1_T2の平均
                   ev1.T2.sd = sd (ev1_T2), #ev1_T2のSD
                   ev2.T2.mean = mean (ev2_T2), 
                   ev2.T2.sd = sd (ev2_T2),
                   ev.T2.mean = mean (ev_T2),
                   ev.T2.sd = sd (ev_T2))
knitr::kable(ev_T2_discriptive, digits = 2) #出力

#ev_T3
ev_T3_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   ev1.T3.mean = mean (ev_T3), #ev1_T3の平均
                   ev1.T3.sd = sd (ev1_T3), #ev1_T3のSD
                   ev2.T3.mean = mean (ev2_T3), 
                   ev2.T3.sd = sd (ev2_T3),
                   ev.T3.mean = mean (ev_T3),
                   ev.T3.sd = sd (ev_T3))
knitr::kable(ev_T3_discriptive, digits = 2) #出力

#ev_T4
ev_T4_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   ev1.T4.mean = mean (ev1_T4), #ev1_T4の平均
                   ev1.T4.sd = sd (ev1_T4), #ev1_T4のSD
                   ev2.T4.mean = mean (ev2_T4), 
                   ev2.T4.sd = sd (ev2_T4),
                   ev.T4.mean = mean (ev_T4),
                   ev.T4.sd = sd (ev_T4))
knitr::kable(ev_T4_discriptive, digits = 2) #出力

#ev_onemonth
ev_onemonth_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   ev.onemonth.mean = mean (ev_onemonth), #ev_onemonthの平均
                   ev.onemonth.sd = sd (ev_onemonth)) #ev_onemonthのSD
knitr::kable(ev_onemonth_discriptive, digits = 2) #出力

#age_T1
age_T1_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   age.T1.mean = mean (age_T1), #age_T1の平均
                   age.T1.sd = sd (age_T1)) #age_T1のSD
knitr::kable(age_T1_discriptive, digits = 2) #出力

#age_T2
age_T2_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   age.T2.mean = mean (age_T2), #age_T2の平均
                   age.T2.sd = sd (age_T2)) #age_T2のSD
knitr::kable(age_T2_discriptive, digits = 2) #出力

#age_T3
age_T3_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   age.T3.mean = mean (age_T3), #age_T3の平均
                   age.T3.sd = sd (age_T3)) #age_T3のSD
knitr::kable(age_T3_discriptive, digits = 2) #出力

#age_T4
age_T4_discriptive <- 
  data %>%
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   age.T4.mean = mean (age_T4), #age_T4の平均
                   age.T4.sd = sd (age_T4)) #age_T4のSD
knitr::kable(age_T4_discriptive, digits = 2) #出力

##1-5. 内的一貫性の算出 ----
library(psych)
library(GPArotation)

#hsc_T1
alpha(data[, c(6,7,8,9,11,12,13,14,15,16,17)]) #alpha .63
omega(data[, c(6,7,8,9,11,12,13,14,15,16,17)],3,fm="ml") #omega hierarchical=.41, omega total=.74

#hsc_T2
alpha(data[, c(27,28,29,30,32,33,34,35,36,37,38)]) #alpha .78
omega(data[, c(27,28,29,30,32,33,34,35,36,37,38)],3,fm="ml") #omega hierarchical=.64, omega total=.84

#hsc_T3
alpha(data[, c(48,49,50,51,53,54,55,56,57,58,59)]) #alpha .75
omega(data[, c(48,49,50,51,53,54,55,56,57,58,59)],3,fm="ml") #omega hierarchical=.59, omega total=.83

#hsc_T4
alpha(data[, c(69,70,71,72,74,75,76,77,78,79,80)]) #alpha .79
omega(data[, c(69,70,71,72,74,75,76,77,78,79,80)],3,fm="ml") #omega hierarchical=.71, omega total=.86

#wb_T1
alpha(data[, c(18:22)]) #alpha .78
omega(data[, c(18:22)],1,fm="ml") #omega hierarchical=.78, omega total=.79

#wb_T2
alpha(data[, c(39:43)]) #alpha .85
omega(data[, c(39:43)],1,fm="ml") #omega hierarchical=.83, omega total=.85

#wb_T3
alpha(data[, c(60:64)]) #alpha .85
omega(data[, c(60:64)],1,fm="ml") #omega hierarchical=.85, omega total=.85

#wb_T4
alpha(data[, c(81:85)]) #alpha .90
omega(data[, c(81:85)],1,fm="ml") #omega hierarchical=.90, omega total=.90

#1-6. 級内相関係数 -----
#irrパッケージ読み込み
library(irr)

#ICCに必要な変数だけのデータセットを作成
icc_hsc <- data %>% dplyr::select("hsc_T1", "hsc_T2", "hsc_T3", "hsc_T4")
icc_wb <- data %>% dplyr::select("wb_T1", "wb_T2", "wb_T3", "wb_T4")
icc_ev <- data %>% dplyr::select("ev_T1", "ev_T2", "ev_T3", "ev_T4")

#ICC算出
icc(icc_hsc, "twoway", "agreement") #ICC = 0.70 [0.61 < ICC < 0.77]
icc(icc_wb, "twoway", "agreement") #ICC = 0.66 [0.57 < ICC < 0.74]
icc(icc_ev, "twoway", "agreement") #ICC = 0.18 [0.10 < ICC < 0.30]

#4時点にわたる自己相関
#HSC
cor.test(icc_hsc$hsc_T1, icc_hsc$hsc_T2)
cor.test(icc_hsc$hsc_T1, icc_hsc$hsc_T3)
cor.test(icc_hsc$hsc_T1, icc_hsc$hsc_T4)
cor.test(icc_hsc$hsc_T2, icc_hsc$hsc_T3)
cor.test(icc_hsc$hsc_T3, icc_hsc$hsc_T4)
#WB
cor.test(icc_wb$wb_T1, icc_wb$wb_T2)
cor.test(icc_wb$wb_T1, icc_wb$wb_T3)
cor.test(icc_wb$wb_T1, icc_wb$wb_T4)
cor.test(icc_wb$wb_T2, icc_wb$wb_T3)
cor.test(icc_wb$wb_T3, icc_wb$wb_T4)
#LE
cor.test(icc_ev$ev_T1, icc_ev$ev_T2)
cor.test(icc_ev$ev_T1, icc_ev$ev_T3)
cor.test(icc_ev$ev_T1, icc_ev$ev_T4)
cor.test(icc_ev$ev_T2, icc_ev$ev_T3)
cor.test(icc_ev$ev_T3, icc_ev$ev_T4)

## 1-7. 欠損値分析 ----
library(BaylorEdPsych)
missingdata <- data %>% dplyr::select(hsc_T1,hsc_T2,hsc_T3,hsc_T4,wb_T1,wb_T2,wb_T3,wb_T4,ev_T1,ev_T2,ev_T3,ev_T4,hsc_onemonth,wb_onemonth,ev_onemonth)
LittleMCAR(missingdata)


#（2）相関係数 ※具体的な数値や係数はHADファイルを参照してください----

cordata <- data %>% dplyr::select(hsc_T1,hsc_T2,hsc_T3,hsc_T4,wb_T1,wb_T2,wb_T3,wb_T4,ev_T1,ev_T2,ev_T3,ev_T4,hsc_onemonth,wb_onemonth,ev_onemonth) %>% drop_na()
head(cordata)
names(cordata)
cormat <- round(cor(cordata),2)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# （3）HSCS確認的因子分析（補足的位置づけ）-----
library(lavaan)
library(semPlot)
library(semTools)

## 3因子モデル（1時点目）
model_t1 <-'
EOE =~ hsc4_T1 + hsc6_T1 + hsc8_T1 + hsc9_T1 + hsc12_T1
LST =~ hsc2_T1 + hsc11_T1
AES =~ hsc1_T1 + hsc3_T1 + hsc5_T1 + hsc10_T1 
'
cfa_t1 <- cfa(model_t1, data = data, missing = "fiml")
summary(cfa_t1, fit.measures = TRUE, standardized = TRUE)

## 3因子モデル（2時点目）
model_t2 <-'
EOE =~ hsc4_T2 + hsc6_T2 + hsc8_T2 + hsc9_T2 + hsc12_T2
LST =~ hsc2_T2 + hsc11_T2
AES =~ hsc1_T2 + hsc3_T2 + hsc5_T2 + hsc10_T2
'
cfa_t2 <- cfa(model_t2, data = data, missing = "fiml")
summary(cfa_t2, fit.measures = TRUE, standardized = TRUE)

## 3因子モデル（3時点目）
model_t3 <-'
EOE =~ hsc4_T3 + hsc6_T3 + hsc8_T3 + hsc9_T3 + hsc12_T3
LST =~ hsc2_T3 + hsc11_T3
AES =~ hsc1_T3 + hsc3_T3 + hsc5_T3 + hsc10_T3
'
cfa_t3 <- cfa(model_t3, data = data, missing = "fiml")
summary(cfa_t3, fit.measures = TRUE, standardized = TRUE)

## 3因子モデル（4時点目）
model_t4 <-'
EOE =~ hsc4_T4 + hsc6_T4 + hsc8_T4 + hsc9_T4 + hsc12_T4
LST =~ hsc2_T4 + hsc11_T4
AES =~ hsc1_T4 + hsc3_T4 + hsc5_T4 + hsc10_T4
'
cfa_t4 <- cfa(model_t4, data = data, missing = "fiml")
summary(cfa_t4, fit.measures = TRUE, standardized = TRUE)

## Bifactorモデル（1時点目）
bi_t1 <-'
EOE =~ hsc4_T1 + hsc6_T1 + hsc8_T1 + hsc9_T1 + hsc12_T1
LST =~ hsc2_T1 + hsc11_T1
AES =~ hsc1_T1 + hsc3_T1 + hsc5_T1 + hsc10_T1
SPS =~ hsc1_T1 + hsc2_T1 + hsc3_T1 + hsc4_T1 + hsc5_T1 + hsc6_T1 + hsc8_T1 + hsc9_T1 + hsc10_T1 + hsc11_T1 + hsc12_T1
SPS ~~ 0*EOE
SPS ~~ 0*LST
SPS ~~ 0*AES
EOE ~~ 0*LST
EOE ~~ 0*AES
LST ~~ 0*AES
'
bifac_t1 <- cfa(bi_t1, data = data, missing = "fiml")
summary(bifac_t1, fit.measures = TRUE, standardized = TRUE)

## Bifactorモデル（2時点目）
bi_t2 <-'
EOE =~ hsc4_T2 + hsc6_T2 + hsc8_T2 + hsc9_T2 + hsc12_T2
LST =~ hsc2_T2 + hsc11_T2
AES =~ hsc1_T2 + hsc3_T2 + hsc5_T2 + hsc10_T2
SPS =~ hsc1_T2 + hsc2_T2 + hsc3_T2 + hsc4_T2 + hsc5_T2 + hsc6_T2 + hsc8_T2 + hsc9_T2 + hsc10_T2 + hsc11_T2 + hsc12_T2
SPS ~~ 0*EOE
SPS ~~ 0*LST
SPS ~~ 0*AES
EOE ~~ 0*LST
EOE ~~ 0*AES
LST ~~ 0*AES
'
bifac_t2 <- cfa(bi_t2, data = data, missing = "fiml")
summary(bifac_t2, fit.measures = TRUE, standardized = TRUE)

## Bifactorモデル（3時点目）
bi_t3 <-'
EOE =~ hsc4_T3 + hsc6_T3 + hsc8_T3 + hsc9_T3 + hsc12_T3
LST =~ hsc2_T3 + hsc11_T3
AES =~ hsc1_T3 + hsc3_T3 + hsc5_T3 + hsc10_T3
SPS =~ hsc1_T3 + hsc2_T3 + hsc3_T3 + hsc4_T3 + hsc5_T3 + hsc6_T3 + hsc8_T3 + hsc9_T3 + hsc10_T3 + hsc11_T3 + hsc12_T3
SPS ~~ 0*EOE
SPS ~~ 0*LST
SPS ~~ 0*AES
EOE ~~ 0*LST
EOE ~~ 0*AES
LST ~~ 0*AES
'
bifac_t3 <- cfa(bi_t3, data = data, missing = "fiml")
summary(bifac_t3, fit.measures = TRUE, standardized = TRUE)

## Bifactorモデル（4時点目）
bi_t4 <-'
EOE =~ hsc4_T4 + hsc6_T4 + hsc8_T4 + hsc9_T4 + hsc12_T4
LST =~ hsc2_T4 + hsc11_T4
AES =~ hsc1_T4 + hsc3_T4 + hsc5_T4 + hsc10_T4
SPS =~ hsc1_T4 + hsc2_T4 + hsc3_T4 + hsc4_T4 + hsc5_T4 + hsc6_T4 + hsc8_T4 + hsc9_T4 + hsc10_T4 + hsc11_T4 + hsc12_T4
SPS ~~ 0*EOE
SPS ~~ 0*LST
SPS ~~ 0*AES
EOE ~~ 0*LST
EOE ~~ 0*AES
LST ~~ 0*AES
'
bifac_t4 <- cfa(bi_t4, data = data, missing = "fiml")
summary(bifac_t4, fit.measures = TRUE, standardized = TRUE)

## 縦断的確認的因子モデル
# 複数因子の場合は、次のURLを参考に自力でコードを書くこと：https://groups.google.com/forum/#!topic/lavaan/nfdatPgLLhc

### 時点ごとに因子モデルを描く
model_hsc <-'
HSC_t1 =~ hsc1_T1 + hsc2_T1 + hsc3_T1 + hsc4_T1 + hsc5_T1 + hsc6_T1 + hsc8_T1 + hsc9_T1 + hsc10_T1 + hsc11_T1 + hsc12_T1
HSC_t2 =~ hsc1_T2 + hsc2_T2 + hsc3_T2 + hsc4_T2 + hsc5_T2 + hsc6_T2 + hsc8_T2 + hsc9_T2 + hsc10_T2 + hsc11_T2 + hsc12_T2
HSC_t3 =~ hsc1_T3 + hsc2_T3 + hsc3_T3 + hsc4_T3 + hsc5_T3 + hsc6_T3 + hsc8_T3 + hsc9_T3 + hsc10_T3 + hsc11_T3 + hsc12_T3 
HSC_t4 =~ hsc1_T4 + hsc2_T4 + hsc3_T4 + hsc4_T4 + hsc5_T4 + hsc6_T4 + hsc8_T4 + hsc9_T4 + hsc10_T4 + hsc11_T4 + hsc12_T4
'

### Create list of variables（時間ごとの変数名のリストをつくる）
var1 <- c("hsc1_T1", "hsc2_T1", "hsc3_T1", "hsc4_T1", "hsc5_T1", "hsc6_T1", "hsc8_T1", "hsc9_T1", "hsc10_T1", "hsc11_T1", "hsc12_T1")#T1
var2 <- c("hsc1_T2", "hsc2_T2", "hsc3_T2", "hsc4_T2", "hsc5_T2", "hsc6_T2", "hsc8_T2", "hsc9_T2", "hsc10_T2", "hsc11_T2", "hsc12_T2")#T2
var3 <- c("hsc1_T3", "hsc2_T3", "hsc3_T3", "hsc4_T3", "hsc5_T3", "hsc6_T3", "hsc8_T3", "hsc9_T3", "hsc10_T3", "hsc11_T3", "hsc12_T3")#T3
var4 <- c("hsc1_T4", "hsc2_T4", "hsc3_T4", "hsc4_T4", "hsc5_T4", "hsc6_T4", "hsc8_T4", "hsc9_T4", "hsc10_T4", "hsc11_T4", "hsc12_T4")#T4
varlist <- list(var1, var2, var3, var4)
longInvariance(model = model_hsc, auto = 1, data = data, varList = varlist, constrainAuto = TRUE, missing = "fiml")


# （4）時点ごとのRoisman's exploretory Approach----

## 独立変数の中心化
data_c <- data %>% drop_na() %>% select_("hsc_T1", "hsc_T2", "hsc_T3", "hsc_T4", "wb_T1", "wb_T2", "wb_T3", "wb_T4", "ev_T1", "ev_T2", "ev_T3", "ev_T4", "hsc_onemonth", "wb_onemonth", "ev_onemonth") #na削除したうえで必要な変数抽出
data_c$hsc_T1_c <- data_c$hsc_T1 - mean(data_c$hsc_T1) #hsc_T1の中心化
data_c$hsc_T2_c <- data_c$hsc_T2 - mean(data_c$hsc_T2) #hsc_T2の中心化
data_c$hsc_T3_c <- data_c$hsc_T3 - mean(data_c$hsc_T3) #hsc_T3の中心化
data_c$hsc_T4_c <- data_c$hsc_T4 - mean(data_c$hsc_T4) #hsc_T4の中心化
data_c$ev_T1_c <- data_c$ev_T1 - mean(data_c$ev_T1) #ev_T1の中心化
data_c$ev_T2_c <- data_c$ev_T2 - mean(data_c$ev_T2) #ev_T2の中心化
data_c$ev_T3_c <- data_c$ev_T3 - mean(data_c$ev_T3) #ev_T3の中心化
data_c$ev_T4_c <- data_c$ev_T4 - mean(data_c$ev_T4) #ev_T4の中心化
data_c$hsc_onemonth_c  <- data_c$hsc_onemonth - mean(data_c$hsc_onemonth) #hsc_onemonthの中心化
data_c$ev_onemonth_c  <- data_c$ev_onemonth - mean(data_c$ev_onemonth) #ev_onemonthの中心化

## 中心化できたか確認（中心化前と中心化後の相関係数をみる）
round(cor(data.frame(data_c$hsc_T1, data_c$hsc_T2, data_c$hsc_T3, data_c$hsc_T4, data_c$ev_T1, data_c$ev_T2, data_c$ev_T3, data_c$ev_T4, data_c$hsc_onemonth, data_c$ev_onemonth)), digits = 2)#中心化前の変数
round(cor(data.frame(hsc_T1_c, hsc_T2_c, hsc_T3_c, hsc_T4_c, ev_T1_c, ev_T2_c, ev_T3_c, ev_T4_c, hsc_onemonth_c, ev_onemonth_c)), digits = 2)#中心化後の変数

## 事前に中心化しなくてもpequodパッケージで一発で検定できる
library(pequod)

### 1時点目の分析 ----

#pequodで分析版
model_t1 <- lmres(wb_T1 ~ ev_T1 + hsc_T1  + ev_T1:hsc_T1, centered = c("wb_T1", "ev_T1", "hsc_T1"), data = data) #交互作用の検討
summary(model_t1)

#通常のlmで分析版（ステップ1：主効果モデル）
model_t1s1 <- lm(data_c$wb_T1 ~ data_c$ev_T1_c + data_c$hsc_T1_c)
summary(model_t1s1)
AIC(model_t1s1)
BIC(model_t1s1)

#通常のlmで分析版（ステップ2：交互作用モデル）
model_t1s2 <- lm(data_c$wb_T1 ~ data_c$ev_T1_c + data_c$hsc_T1_c + data_c$ev_T1_c:data_c$hsc_T1_c)
summary(model_t1s2)
AIC(model_t1s2)
BIC(model_t1s2)
anova(model_t1s1, model_t1s2) #R^2の増加量の検定

### 2時点目の分析 ----

#pequodで分析版
model_t2 <- lmres(wb_T2 ~ ev_T2 + hsc_T2  + ev_T2:hsc_T2, centered = c("wb_T2", "ev_T2", "hsc_T2"), data = data) #交互作用の検討
summary(model_t2)

#### p<.10で交互作用が有意だったので単純傾斜検定 ----
model_ss <- simpleSlope(model_t2, pred ="ev_T2", mod1 = "hsc_T2")
summary(model_ss) #High HSCだけ係数が有意 b = 0.26, p<.001
PlotSlope(model_ss)

#### RoS testをRoismanのアプリで行うために共分散を算出 ----
reg <- lm(data_c$wb_T2_c ~ data_c$ev_T2_c + data_c$hsc_T2_c + data_c$ev_T2_c:data_c$hsc_T2_c)
summary(reg)
round(hccm(reg, type = "hc0"), digits = 3)
#必要なパラメタ
#* Intercept (b0) = -0.024 →切片
#* Variable X (b1) = 0.31 →環境変数（独立変数）の回帰係数
#* Variable Z (b2) = -0.04 →感受性変数（調整変数）の回帰係数
#* Interaction XZ (b3) = 0.17 →交互作用項の回帰係数
#* Variance parameter b1 = 0.05^2 = 0.003 →たぶんstand.errorの2乗のことだと思う
#* Variance parameter b2 = 0.11^2 = 0.012 →たぶんstand.errorの2乗のことだと思う
#* Variance parameter b3 = 0.06^2 = 0.004 →たぶんstand.errorの2乗のことだと思う
#* Covariance parameters b1 b3 = -0.001
#* Covariance parameters b2 b3 = -0.001
#* Degress of freedom (df) = 75 →アプリの説明によればN - 独立変数の数k - 1で計算される（この場合、79 - 3 - 1 = 75）

#通常のlmで分析版（ステップ1：主効果モデル）
model_t2s1 <- lm(data_c$wb_T2 ~ data_c$ev_T2_c + data_c$hsc_T2_c)
summary(model_t2s1)
AIC(model_t2s1)
BIC(model_t2s1)

#通常のlmで分析版（ステップ2：交互作用モデル）
model_t2s2 <- lm(data_c$wb_T2 ~ data_c$ev_T2_c + data_c$hsc_T2_c + data_c$ev_T2_c:data_c$hsc_T2_c)
summary(model_t2s2)
AIC(model_t2s2)
BIC(model_t2s2)
anova(model_t2s1, model_t2s2) #R^2の増加量の検定

### 3時点目の分析 ----

#pequodで分析版
model_t3 <- lmres(wb_T3 ~ ev_T3 + hsc_T3  + ev_T3:hsc_T3, centered = c("wb_T3", "ev_T3", "hsc_T3"), data = data) #交互作用の検討
summary(model_t3)

#通常のlmで分析版（ステップ1：主効果モデル）
model_t3s1 <- lm(data_c$wb_T3 ~ data_c$ev_T3_c + data_c$hsc_T3_c)
summary(model_t3s1)
AIC(model_t3s1)
BIC(model_t3s1)

#通常のlmで分析版（ステップ2：交互作用モデル）
model_t3s2 <- lm(data_c$wb_T3 ~ data_c$ev_T3_c + data_c$hsc_T3_c + data_c$ev_T3_c:data_c$hsc_T3_c)
summary(model_t3s2)
AIC(model_t3s2)
BIC(model_t3s2)
anova(model_t3s1, model_t3s2) #R^2の増加量の検定

### 4時点目の分析 ----

#pequodで分析版
model_t4 <- lmres(wb_T4 ~ ev_T4 + hsc_T4  + ev_T4:hsc_T4, centered = c("wb_T4", "ev_T4", "hsc_T4"), data = data) #交互作用の検討
summary(model_t4)

#通常のlmで分析版（ステップ1：主効果モデル）
model_t4s1 <- lm(data_c$wb_T4 ~ data_c$ev_T4_c + data_c$hsc_T4_c)
summary(model_t4s1)
AIC(model_t4s1)
BIC(model_t4s1)

#通常のlmで分析版（ステップ2：交互作用モデル）
model_t4s2 <- lm(data_c$wb_T4 ~ data_c$ev_T4_c + data_c$hsc_T4_c + data_c$ev_T4_c:data_c$hsc_T4_c)
summary(model_t4s2)
AIC(model_t4s2)
BIC(model_t4s2)
anova(model_t4s1, model_t4s2) #R^2の増加量の検定

### 1ヵ月全体の分析 ----

#pequodで分析版
model_onemonth <- lmres(wb_onemonth ~ ev_onemonth + hsc_onemonth  + ev_onemonth:hsc_onemonth, centered = c("wb_onemonth", "ev_onemonth", "hsc_onemonth"), data = data) #交互作用の検討
summary(model_onemonth)

#通常のlmで分析版（ステップ1：主効果モデル）
model_1ms1 <- lm(data_c$wb_onemonth ~ data_c$ev_onemonth_c + data_c$hsc_onemonth_c)
summary(model_1ms1)
AIC(model_1ms1)
BIC(model_1ms1)

#通常のlmで分析版（ステップ2：交互作用モデル）
model_1ms2 <- lm(data_c$wb_onemonth ~ data_c$ev_onemonth_c + data_c$hsc_onemonth_c + data_c$ev_onemonth_c:data_c$hsc_onemonth_c)
summary(model_1ms2)
AIC(model_1ms2)
BIC(model_1ms2)
anova(model_1ms1, model_1ms2) #R^2の増加量の検定

# （5）時点ごとのWidaman's Approach----
library(soilphysics) #非線形モデルで準R2を算出するため使用

## 1時点目：弱い差次感受性 ---- 
weak_diff_T1 <- nls(wb_T1 ~ B0 + B1*(ev_T1 - C) + B3*((ev_T1 - C)*hsc_T1), 
                 data = data,
                 start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff_T1) 

AIC(weak_diff_T1)
BIC(weak_diff_T1)
pred <- predict(weak_diff_T1) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diff_T1)
w <- weights(weak_diff_T1)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diff_T1)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1時点目：強い差次感受性 ----
strong_diff_T1 <- nls(wb_T1 ~ B0 + 0*(ev_T1 - C) + B3*((ev_T1 - C)*hsc_T1),
                   data = data,
                   start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff_T1)
AIC(strong_diff_T1)
BIC(strong_diff_T1)
pred <- predict(strong_diff_T1) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diff_T1)
w <- weights(strong_diff_T1)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diff_T1)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1時点目：弱い素因ストレス ----
weak_diathesis_T1 <- nls(wb_T1 ~ B0 + B1*(ev_T1 + 3) + B3*((ev_T1 + 3)*hsc_T1), #+3は環境変数の最大値(C on X)
                      data = data,
                      start = list(B0 = 90, B1 = 0, B3 = -1)) #Cの初期値は設定不要
summary(weak_diathesis_T1)

AIC(weak_diathesis_T1)
BIC(weak_diathesis_T1)
pred <- predict(weak_diathesis_T1) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diathesis_T1)
w <- weights(weak_diathesis_T1)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diathesis_T1)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1時点目：強い素因ストレス ---- 
strong_diathesis_T1 <- nls(wb_T1 ~ B0 + 0*(ev_T1 + 3) + B3*((ev_T1 + 3)*hsc_T1),
                        data = data,
                        start = list(B0 = 90, B3 = -1))
summary(strong_diathesis_T1)
AIC(strong_diathesis_T1)
BIC(strong_diathesis_T1)
pred <- predict(strong_diathesis_T1) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diathesis_T1)
w <- weights(strong_diathesis_T1)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diathesis_T1)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1時点目：弱いヴァンテージ感受性 ----
weak_vs_T1 <- nls(wb_T1 ~ B0 + B1*(ev_T1 - 3) + B3*((ev_T1 - 3)*hsc_T1), #-3は環境変数の最小値（C on X）
               data = data,
               start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs_T1)
AIC(weak_vs_T1)
BIC(weak_vs_T1)
pred <- predict(weak_vs_T1) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_vs_T1)
w <- weights(weak_vs_T1)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_vs_T1)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1時点目：強いヴァンテージ感受性 ----
strong_vs_T1 <- nls(wb_T1 ~ B0 + 0*(ev_T1 - 3) + B3*((ev_T1 - 3)*hsc_T1),
                 data = data,
                 start = list(B0 = 90, B3 = -1))
summary(strong_vs_T1)
AIC(strong_vs_T1)
BIC(strong_vs_T1)
pred <- predict(strong_vs_T1) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_vs_T1)
w <- weights(strong_vs_T1)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_vs_T1)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 2時点目：弱い差次感受性 ---- 
weak_diff_T2 <- nls(wb_T2 ~ B0 + B1*(ev_T2 - C) + B3*((ev_T2 - C)*hsc_T2), 
                    data = data,
                    start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff_T2) 

AIC(weak_diff_T2)
BIC(weak_diff_T2)
pred <- predict(weak_diff_T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diff_T2)
w <- weights(weak_diff_T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diff_T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 2時点目：強い差次感受性 ----
strong_diff_T2 <- nls(wb_T2 ~ B0 + 0*(ev_T2 - C) + B3*((ev_T2 - C)*hsc_T2),
                      data = data,
                      start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff_T2)
AIC(strong_diff_T2)
BIC(strong_diff_T2)
pred <- predict(strong_diff_T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diff_T2)
w <- weights(strong_diff_T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diff_T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 2時点目：弱い素因ストレス ----
weak_diathesis_T2 <- nls(wb_T2 ~ B0 + B1*(ev_T2 + 3) + B3*((ev_T2 + 3)*hsc_T2), #+3は環境変数の最大値(C on X)
                         data = data,
                         start = list(B0 = 90, B1 = 0, B3 = -1)) #Cの初期値は設定不要
summary(weak_diathesis_T2)
AIC(weak_diathesis_T2)
BIC(weak_diathesis_T2)
pred <- predict(weak_diathesis_T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diathesis_T2)
w <- weights(weak_diathesis_T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diathesis_T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 2時点目：強い素因ストレス ---- 
strong_diathesis_T2 <- nls(wb_T2 ~ B0 + 0*(ev_T2 + 3) + B3*((ev_T2 + 3)*hsc_T2),
                           data = data,
                           start = list(B0 = 90, B3 = -1))
summary(strong_diathesis_T2)
AIC(strong_diathesis_T2)
BIC(strong_diathesis_T2)
pred <- predict(strong_diathesis_T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diathesis_T2)
w <- weights(strong_diathesis_T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diathesis_T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 2時点目：弱いヴァンテージ感受性 ----
weak_vs_T2 <- nls(wb_T2 ~ B0 + B1*(ev_T2 - 3) + B3*((ev_T2 - 3)*hsc_T2), #-3は環境変数の最小値（C on X）
                  data = data,
                  start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs_T2)
AIC(weak_vs_T2)
BIC(weak_vs_T2)
pred <- predict(weak_vs_T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_vs_T2)
w <- weights(weak_vs_T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_vs_T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 2時点目：強いヴァンテージ感受性 ----
strong_vs_T2 <- nls(wb_T2 ~ B0 + 0*(ev_T2 - 3) + B3*((ev_T2 - 3)*hsc_T2),
                    data = data,
                    start = list(B0 = 90, B3 = -1))
summary(strong_vs_T2)
AIC(strong_vs_T2)
BIC(strong_vs_T2)
pred <- predict(strong_vs_T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_vs_T2)
w <- weights(strong_vs_T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_vs_T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 3時点目：弱い差次感受性 ---- 
weak_diff_T3 <- nls(wb_T3 ~ B0 + B1*(ev_T3 - C) + B3*((ev_T3 - C)*hsc_T3), 
                    data = data,
                    start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff_T3) 

AIC(weak_diff_T3)
BIC(weak_diff_T3)
pred <- predict(weak_diff_T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diff_T3)
w <- weights(weak_diff_T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diff_T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 3時点目：強い差次感受性 ----
strong_diff_T3 <- nls(wb_T3 ~ B0 + 0*(ev_T3 - C) + B3*((ev_T3 - C)*hsc_T3),
                      data = data,
                      start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff_T3)
AIC(strong_diff_T3)
BIC(strong_diff_T3)
pred <- predict(strong_diff_T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diff_T3)
w <- weights(strong_diff_T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diff_T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 3時点目：弱い素因ストレス ----
weak_diathesis_T3 <- nls(wb_T3 ~ B0 + B1*(ev_T3 + 3) + B3*((ev_T3 + 3)*hsc_T3), #+3は環境変数の最大値(C on X)
                         data = data,
                         start = list(B0 = 90, B1 = 0, B3 = -1)) #Cの初期値は設定不要
summary(weak_diathesis_T3)

AIC(weak_diathesis_T3)
BIC(weak_diathesis_T3)
pred <- predict(weak_diathesis_T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diathesis_T3)
w <- weights(weak_diathesis_T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diathesis_T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 3時点目：強い素因ストレス ---- 
strong_diathesis_T3 <- nls(wb_T3 ~ B0 + 0*(ev_T3 + 3) + B3*((ev_T3 + 3)*hsc_T3),
                           data = data,
                           start = list(B0 = 90, B3 = -1))
summary(strong_diathesis_T3)
AIC(strong_diathesis_T3)
BIC(strong_diathesis_T3)
pred <- predict(strong_diathesis_T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diathesis_T3)
w <- weights(strong_diathesis_T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diathesis_T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 3時点目：弱いヴァンテージ感受性 ----
weak_vs_T3 <- nls(wb_T3 ~ B0 + B1*(ev_T3 - 3) + B3*((ev_T3 - 3)*hsc_T3), #-3は環境変数の最小値（C on X）
                  data = data,
                  start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs_T3)
AIC(weak_vs_T3)
BIC(weak_vs_T3)
pred <- predict(weak_vs_T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_vs_T3)
w <- weights(weak_vs_T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_vs_T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 3時点目：強いヴァンテージ感受性 ----
strong_vs_T3 <- nls(wb_T3 ~ B0 + 0*(ev_T3 - 3) + B3*((ev_T3 - 3)*hsc_T3),
                    data = data,
                    start = list(B0 = 90, B3 = -1))
summary(strong_vs_T3)
AIC(strong_vs_T3)
BIC(strong_vs_T3)
pred <- predict(strong_vs_T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_vs_T3)
w <- weights(strong_vs_T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_vs_T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 4時点目：弱い差次感受性 ---- 
weak_diff_T4 <- nls(wb_T4 ~ B0 + B1*(ev_T4 - C) + B3*((ev_T4 - C)*hsc_T4), 
                    data = data,
                    start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff_T4) 

AIC(weak_diff_T4)
BIC(weak_diff_T4)
pred <- predict(weak_diff_T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diff_T4)
w <- weights(weak_diff_T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diff_T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 4時点目：強い差次感受性 ----
strong_diff_T4 <- nls(wb_T4 ~ B0 + 0*(ev_T4 - C) + B3*((ev_T4 - C)*hsc_T4),
                      data = data,
                      start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff_T4)
AIC(strong_diff_T4)
BIC(strong_diff_T4)
pred <- predict(strong_diff_T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diff_T4)
w <- weights(strong_diff_T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diff_T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 4時点目：弱い素因ストレス ----
weak_diathesis_T4 <- nls(wb_T4 ~ B0 + B1*(ev_T4 + 3) + B3*((ev_T4 + 3)*hsc_T4), #+3は環境変数の最大値(C on X)
                         data = data,
                         start = list(B0 = 90, B1 = 0, B3 = -1)) #Cの初期値は設定不要
summary(weak_diathesis_T4)

AIC(weak_diathesis_T4)
BIC(weak_diathesis_T4)
pred <- predict(weak_diathesis_T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diathesis_T4)
w <- weights(weak_diathesis_T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diathesis_T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 4時点目：強い素因ストレス ---- 
strong_diathesis_T4 <- nls(wb_T4 ~ B0 + 0*(ev_T4 + 3) + B3*((ev_T4 + 3)*hsc_T4),
                           data = data,
                           start = list(B0 = 90, B3 = -1))
summary(strong_diathesis_T4)
AIC(strong_diathesis_T4)
BIC(strong_diathesis_T4)
pred <- predict(strong_diathesis_T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diathesis_T4)
w <- weights(strong_diathesis_T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diathesis_T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 4時点目：弱いヴァンテージ感受性 ----
weak_vs_T4 <- nls(wb_T4 ~ B0 + B1*(ev_T4 - 3) + B3*((ev_T4 - 3)*hsc_T4), #-3は環境変数の最小値（C on X）
                  data = data,
                  start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs_T4)
AIC(weak_vs_T4)
BIC(weak_vs_T4)
pred <- predict(weak_vs_T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_vs_T4)
w <- weights(weak_vs_T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_vs_T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 4時点目：強いヴァンテージ感受性 ----
strong_vs_T4 <- nls(wb_T4 ~ B0 + 0*(ev_T4 - 3) + B3*((ev_T4 - 3)*hsc_T4),
                    data = data,
                    start = list(B0 = 90, B3 = -1))
summary(strong_vs_T4)
AIC(strong_vs_T4)
BIC(strong_vs_T4)
pred <- predict(strong_vs_T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_vs_T4)
w <- weights(strong_vs_T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_vs_T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

# （6）1ヵ月間のWidaman's Approach ----

## 1か月間：弱い差次感受性 ---- 
weak_diff_om <- nls(wb_onemonth ~ B0 + B1*(ev_onemonth - C) + B3*((ev_onemonth - C)*hsc_onemonth), 
                    data = data,
                    start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff_om) 

AIC(weak_diff_om)
BIC(weak_diff_om)
pred <- predict(weak_diff_om) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diff_om)
w <- weights(weak_diff_om)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diff_om)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1か月間：強い差次感受性 ----
strong_diff_om <- nls(wb_onemonth ~ B0 + 0*(ev_onemonth - C) + B3*((ev_onemonth - C)*hsc_onemonth),
                      data = data,
                      start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff_om)
AIC(strong_diff_om)
BIC(strong_diff_om)
pred <- predict(strong_diff_om) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diff_om)
w <- weights(strong_diff_om)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diff_om)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1か月間：弱い素因ストレス ----
weak_diathesis_om <- nls(wb_onemonth ~ B0 + B1*(ev_onemonth + 3) + B3*((ev_onemonth + 3)*hsc_onemonth), #+3は環境変数の最大値(C on X)
                         data = data,
                         start = list(B0 = 90, B1 = 0, B3 = -1)) #Cの初期値は設定不要
summary(weak_diathesis_om)

AIC(weak_diathesis_om)
BIC(weak_diathesis_om)
pred <- predict(weak_diathesis_om) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diathesis_om)
w <- weights(weak_diathesis_om)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diathesis_om)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1ヵ月間：強い素因ストレス ---- 
strong_diathesis_om <- nls(wb_onemonth ~ B0 + 0*(ev_onemonth + 3) + B3*((ev_onemonth + 3)*hsc_onemonth),
                           data = data,
                           start = list(B0 = 90, B3 = -1))
summary(strong_diathesis_om)
AIC(strong_diathesis_om)
BIC(strong_diathesis_om)
pred <- predict(strong_diathesis_om) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diathesis_om)
w <- weights(strong_diathesis_om)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diathesis_om)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1か月間：弱いヴァンテージ感受性 ----
weak_vs_om <- nls(wb_onemonth ~ B0 + B1*(ev_onemonth - 3) + B3*((ev_onemonth - 3)*hsc_onemonth), #-3は環境変数の最小値（C on X）
                  data = data,
                  start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs_om)
AIC(weak_vs_om)
BIC(weak_vs_om)
pred <- predict(weak_vs_om) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_vs_om)
w <- weights(weak_vs_om)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_vs_om)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## 1か月間：強いヴァンテージ感受性 ----
strong_vs_om <- nls(wb_onemonth ~ B0 + 0*(ev_onemonth - 3) + B3*((ev_onemonth - 3)*hsc_onemonth),
                    data = data,
                    start = list(B0 = 90, B3 = -1))
summary(strong_vs_om)
AIC(strong_vs_om)
BIC(strong_vs_om)
pred <- predict(strong_vs_om) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_vs_om)
w <- weights(strong_vs_om)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_vs_om)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out


#（7）Widaman's Approachの作図 ----

## 1時点目の作図 ----
# 1時点目は強い素因ストレスモデルが支持された

# B0（切片）=2.12
# B1（傾き：低感受性群）= 0.00
# C（交差点）=3.00
# B3（傾き：高感受性群）= 0.03

png("figure/week1.png", width = 1200, height = 1200)
p <- ggplot(data, aes(ev_T1, wb_T1)) + 
  geom_abline(intercept = 2.03, slope = 0.03, size = 1) +  
  geom_abline(intercept = 2.12, slope = 0.00, size = 1, linetype = 2) + 
  ylim(1.5, 2.5) + xlim(-3, 3)
p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), axis.ticks = element_line(size = 1,linetype = "blank")) 
dev.off()

## 2時点目の作図 ----
# 2時点目は強いヴァンテージ感受性モデルが支持された

# B0（切片）= 3.36
# B1（傾き：低感受性群）= 0.00
# C（交差点）= -3.00
# B3（傾き：高感受性群）= 0.04

png("figure/week2.png", width = 1200, height = 1200)
p <- ggplot(data, aes(ev_T2, wb_T2)) + 
  geom_abline(intercept = 3.48, slope = 0.04, size = 1) +  
  geom_abline(intercept = 3.36, slope = 0.00, size = 1, linetype = 2) + 
  ylim(3.0, 4.0) + xlim(-3, 3)
p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), axis.ticks = element_line(size = 1,linetype = "blank")) 
dev.off()


## 3時点目の作図 ----
# 3時点目は強いヴァンテージ感受性モデルが支持された

# B0（切片）= 3.36
# B1（傾き：低感受性群）= 0.00
# C（交差点）= -3.00
# B3（傾き：高感受性群）= 0.04

png("figure/week3.png", width = 1200, height = 1200)
p <- ggplot(data, aes(ev_T3, wb_T3)) + 
  geom_abline(intercept = 3.48, slope = 0.04, size = 1) +  
  geom_abline(intercept = 3.36, slope = 0.00, size = 1, linetype = 2) + 
  ylim(3.0, 4.0) + xlim(-3, 3)
p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), axis.ticks = element_line(size = 1,linetype = "blank")) 
dev.off()


## 4時点目の作図 ----
# 4時点目は弱い素因ストレスモデルが支持された

# B0（切片）= 2.20
# B1（傾き：低感受性群）= 0.64
# C（交差点）= 3.00
# B3（傾き：高感受性群）= -0.09

png("figure/week4.png", width = 1200, height = 1200)
p <- ggplot(data, aes(ev_T4, wb_T4)) + 
  geom_abline(intercept = 2.47, slope = -0.09, size = 1) +  
  geom_abline(intercept = 0.28, slope = 0.64, size = 1, linetype = 2) +  
  ylim(0.0, 3.0) + xlim(-3, 3)
p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), axis.ticks = element_line(size = 1,linetype = "blank"))
dev.off()


## 1か月全体の作図 ----
# 1か月全体では強いヴァンテージ感受性モデルが支持された

# B0（切片）= 3.49
# B1（傾き：低感受性群）= 0.00
# C（交差点）= 3.00
# B3（傾き：高感受性群）= 0.05

png("figure/week3.png", width = 1200, height = 1200)
p <- ggplot(data, aes(ev_onemonth, wb_onemonth)) + 
  geom_abline(intercept = 3.64, slope = 0.05, size = 1) +  
  geom_abline(intercept = 3.49, slope = 0.00, size = 1, linetype = 2) +  
  ylim(3.0, 4.0) + xlim(-3, 3)
p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), axis.ticks = element_line(size = 1,linetype = "blank")) 
dev.off()


# (8) Additinal Analysis----
## Journal of Youth and Adolescenceに投稿するときに必要

## 分析案: t-1時点のGxEがt時点の従属変数を予測するかどうか検討して、上記のWidamanモデルの頑健性を確認する

## Widaman's Approachのコードの従属変数の時点を書き換えればOK!!


## Roisoman Approach 


### T1 -> T2

#通常のlmで分析版（ステップ1：主効果モデル）
model_addt2s1 <- lm(data_c$wb_T2 ~ data_c$ev_T1_c + data_c$hsc_T1_c)
summary(model_addt2s1)
AIC(model_addt2s1)
BIC(model_addt2s1)

#通常のlmで分析版（ステップ2：交互作用モデル）
model_addt2s2 <- lm(data_c$wb_T2 ~ data_c$ev_T1_c + data_c$hsc_T1_c + data_c$ev_T1_c:data_c$hsc_T1_c)
summary(model_addt2s2)
AIC(model_addt2s2)
BIC(model_addt2s2)
anova(model_addt2s1, model_addt2s2) #R^2の増加量の検定


### T2 -> T3

#通常のlmで分析版（ステップ1：主効果モデル）
model_addt3s1 <- lm(data_c$wb_T3 ~ data_c$ev_T2_c + data_c$hsc_T2_c)
summary(model_addt3s1)
AIC(model_addt3s1)
BIC(model_addt3s1)

#通常のlmで分析版（ステップ2：交互作用モデル）
model_addt3s2 <- lm(data_c$wb_T3 ~ data_c$ev_T2_c + data_c$hsc_T2_c + data_c$ev_T2_c:data_c$hsc_T2_c)
summary(model_addt3s2)
AIC(model_addt3s2)
BIC(model_addt3s2)
anova(model_addt3s1, model_addt3s2) #R^2の増加量の検定


### T3 -> T4

#通常のlmで分析版（ステップ1：主効果モデル）
model_addt4s1 <- lm(data_c$wb_T4 ~ data_c$ev_T3_c + data_c$hsc_T3_c)
summary(model_addt4s1)
AIC(model_addt4s1)
BIC(model_addt4s1)

#通常のlmで分析版（ステップ2：交互作用モデル）
model_addt4s2 <- lm(data_c$wb_T4 ~ data_c$ev_T3_c + data_c$hsc_T3_c + data_c$ev_T3_c:data_c$hsc_T3_c)
summary(model_addt4s2)
AIC(model_addt4s2)
BIC(model_addt4s2)
anova(model_addt4s1, model_addt4s2) #R^2の増加量の検定



## T1->T2：弱い差次感受性 ---- 
weak_diff_T1T2 <- nls(wb_T2 ~ B0 + B1*(ev_T1 - C) + B3*((ev_T1 - C)*hsc_T1), 
                    data = data,
                    start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff_T1T2) 

AIC(weak_diff_T1T2)
BIC(weak_diff_T1T2)
pred <- predict(weak_diff_T1T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diff_T1T2)
w <- weights(weak_diff_T1T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diff_T1T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T1->T2：強い差次感受性 ----
strong_diff_T1T2 <- nls(wb_T2 ~ B0 + 0*(ev_T1 - C) + B3*((ev_T1 - C)*hsc_T1),
                      data = data,
                      start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff_T1T2)
AIC(strong_diff_T1T2)
BIC(strong_diff_T1T2)
pred <- predict(strong_diff_T1T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diff_T1T2)
w <- weights(strong_diff_T1T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diff_T1T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T1->T2：弱い素因ストレス ----
weak_diathesis_T1T2 <- nls(wb_T2 ~ B0 + B1*(ev_T1 + 3) + B3*((ev_T1 + 3)*hsc_T1), #+3は環境変数の最大値(C on X)
                         data = data,
                         start = list(B0 = 90, B1 = 0, B3 = -1)) #Cの初期値は設定不要
summary(weak_diathesis_T1T2)

AIC(weak_diathesis_T1T2)
BIC(weak_diathesis_T1T2)
pred <- predict(weak_diathesis_T1T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diathesis_T1T2)
w <- weights(weak_diathesis_T1T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diathesis_T1T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T1->T2：強い素因ストレス ---- 
strong_diathesis_T1T2 <- nls(wb_T2 ~ B0 + 0*(ev_T1 + 3) + B3*((ev_T1 + 3)*hsc_T1),
                           data = data,
                           start = list(B0 = 90, B3 = -1))
summary(strong_diathesis_T1T2)
AIC(strong_diathesis_T1T2)
BIC(strong_diathesis_T1T2)
pred <- predict(strong_diathesis_T1T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diathesis_T1T2)
w <- weights(strong_diathesis_T1T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diathesis_T1T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T1->T2：弱いヴァンテージ感受性 ----
weak_vs_T1T2 <- nls(wb_T2 ~ B0 + B1*(ev_T1 - 3) + B3*((ev_T1 - 3)*hsc_T1), #-3は環境変数の最小値（C on X）
                  data = data,
                  start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs_T1T2)
AIC(weak_vs_T1T2)
BIC(weak_vs_T1T2)
pred <- predict(weak_vs_T1T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_vs_T1T2)
w <- weights(weak_vs_T1T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_vs_T1T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T1->T2：強いヴァンテージ感受性 ----
strong_vs_T1T2 <- nls(wb_T2 ~ B0 + 0*(ev_T1 - 3) + B3*((ev_T1 - 3)*hsc_T1),
                    data = data,
                    start = list(B0 = 90, B3 = -1))
summary(strong_vs_T1T2)
AIC(strong_vs_T1T2)
BIC(strong_vs_T1T2)
pred <- predict(strong_vs_T1T2) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_vs_T1T2)
w <- weights(strong_vs_T1T2)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_vs_T1T2)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out


## T2->T3：弱い差次感受性 ---- 
weak_diff_T2T3 <- nls(wb_T3 ~ B0 + B1*(ev_T2 - C) + B3*((ev_T2 - C)*hsc_T2), 
                      data = data,
                      start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff_T2T3) 

AIC(weak_diff_T2T3)
BIC(weak_diff_T2T3)
pred <- predict(weak_diff_T2T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diff_T2T3)
w <- weights(weak_diff_T2T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diff_T2T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T2->T3：強い差次感受性 ----
strong_diff_T2T3 <- nls(wb_T3 ~ B0 + 0*(ev_T2 - C) + B3*((ev_T2 - C)*hsc_T2),
                        data = data,
                        start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff_T2T3)
AIC(strong_diff_T2T3)
BIC(strong_diff_T2T3)
pred <- predict(strong_diff_T2T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diff_T2T3)
w <- weights(strong_diff_T2T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diff_T2T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T2->T3：弱い素因ストレス ----
weak_diathesis_T2T3 <- nls(wb_T3 ~ B0 + B1*(ev_T2 + 3) + B3*((ev_T2 + 3)*hsc_T2), #+3は環境変数の最大値(C on X)
                           data = data,
                           start = list(B0 = 90, B1 = 0, B3 = -1)) #Cの初期値は設定不要
summary(weak_diathesis_T2T3)

AIC(weak_diathesis_T2T3)
BIC(weak_diathesis_T2T3)
pred <- predict(weak_diathesis_T2T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diathesis_T2T3)
w <- weights(weak_diathesis_T2T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diathesis_T2T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T2->T3：強い素因ストレス ---- 
strong_diathesis_T2T3 <- nls(wb_T3 ~ B0 + 0*(ev_T2 + 3) + B3*((ev_T2 + 3)*hsc_T2),
                             data = data,
                             start = list(B0 = 90, B3 = -1))
summary(strong_diathesis_T2T3)
AIC(strong_diathesis_T2T3)
BIC(strong_diathesis_T2T3)
pred <- predict(strong_diathesis_T2T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diathesis_T2T3)
w <- weights(strong_diathesis_T2T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diathesis_T2T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T2->T3：弱いヴァンテージ感受性 ----
weak_vs_T2T3 <- nls(wb_T3 ~ B0 + B1*(ev_T2 - 3) + B3*((ev_T2 - 3)*hsc_T2), #-3は環境変数の最小値（C on X）
                    data = data,
                    start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs_T2T3)
AIC(weak_vs_T2T3)
BIC(weak_vs_T2T3)
pred <- predict(weak_vs_T2T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_vs_T2T3)
w <- weights(weak_vs_T2T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_vs_T2T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T2->T3：強いヴァンテージ感受性 ----
strong_vs_T2T3 <- nls(wb_T3 ~ B0 + 0*(ev_T2 - 3) + B3*((ev_T2 - 3)*hsc_T2),
                      data = data,
                      start = list(B0 = 90, B3 = -1))
summary(strong_vs_T2T3)
AIC(strong_vs_T2T3)
BIC(strong_vs_T2T3)
pred <- predict(strong_vs_T2T3) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_vs_T2T3)
w <- weights(strong_vs_T2T3)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_vs_T2T3)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T3->T4：弱い差次感受性 ---- 
weak_diff_T3T4 <- nls(wb_T4 ~ B0 + B1*(ev_T3 - C) + B3*((ev_T3 - C)*hsc_T3), 
                      data = data,
                      start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff_T3T4) 

AIC(weak_diff_T3T4)
BIC(weak_diff_T3T4)
pred <- predict(weak_diff_T3T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diff_T3T4)
w <- weights(weak_diff_T3T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diff_T3T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T3->T4：強い差次感受性 ----
strong_diff_T3T4 <- nls(wb_T4 ~ B0 + 0*(ev_T3 - C) + B3*((ev_T3 - C)*hsc_T3),
                        data = data,
                        start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff_T3T4)
AIC(strong_diff_T3T4)
BIC(strong_diff_T3T4)
pred <- predict(strong_diff_T3T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diff_T3T4)
w <- weights(strong_diff_T3T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diff_T3T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T3->T4：弱い素因ストレス ----
weak_diathesis_T3T4 <- nls(wb_T4 ~ B0 + B1*(ev_T3 + 3) + B3*((ev_T3 + 3)*hsc_T3), #+3は環境変数の最大値(C on X)
                           data = data,
                           start = list(B0 = 90, B1 = 0, B3 = -1)) #Cの初期値は設定不要
summary(weak_diathesis_T3T4)

AIC(weak_diathesis_T3T4)
BIC(weak_diathesis_T3T4)
pred <- predict(weak_diathesis_T3T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_diathesis_T3T4)
w <- weights(weak_diathesis_T3T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_diathesis_T3T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T3->T4：強い素因ストレス ---- 
strong_diathesis_T3T4 <- nls(wb_T4 ~ B0 + 0*(ev_T3 + 3) + B3*((ev_T3 + 3)*hsc_T3),
                             data = data,
                             start = list(B0 = 90, B3 = -1))
summary(strong_diathesis_T3T4)
AIC(strong_diathesis_T3T4)
BIC(strong_diathesis_T3T4)
pred <- predict(strong_diathesis_T3T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_diathesis_T3T4)
w <- weights(strong_diathesis_T3T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_diathesis_T3T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T3->T4：弱いヴァンテージ感受性 ----
weak_vs_T3T4 <- nls(wb_T4 ~ B0 + B1*(ev_T3 - 3) + B3*((ev_T3 - 3)*hsc_T3), #-3は環境変数の最小値（C on X）
                    data = data,
                    start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs_T3T4)
AIC(weak_vs_T3T4)
BIC(weak_vs_T3T4)
pred <- predict(weak_vs_T3T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(weak_vs_T3T4)
w <- weights(weak_vs_T3T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(weak_vs_T3T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

## T3->T4：強いヴァンテージ感受性 ----
strong_vs_T3T4 <- nls(wb_T4 ~ B0 + 0*(ev_T3 - 3) + B3*((ev_T3 - 3)*hsc_T3),
                      data = data,
                      start = list(B0 = 90, B3 = -1))
summary(strong_vs_T3T4)
AIC(strong_vs_T3T4)
BIC(strong_vs_T3T4)
pred <- predict(strong_vs_T3T4) #非線形モデルで準R2を算出する
n <- length(pred)
res <- resid(strong_vs_T3T4)
w <- weights(strong_vs_T3T4)
if (is.null(w)) w <- rep(1, n)
rss <- sum(w * res ^ 2)
resp <- pred + res
center <- weighted.mean(resp, w)
r.df <- summary(strong_vs_T3T4)$df[2]
int.df <- 1
tss <- sum(w * (resp - center)^2)
r.sq <- 1 - rss/tss
adj.r.sq <- 1 - (1 - r.sq) * (n - int.df) / r.df
out <- list(pseudo.R.squared = r.sq,
            adj.R.squared = adj.r.sq)
out

# Additional Analysisの作図 ----

# T1->T2は強いヴァンテージ感受性モデルが支持された ----

# B0（切片）=3.23
# B1（傾き：低感受性群）= 0.00
# C（交差点）=3.00
# B3（傾き：高感受性群）= 0.03

png("figure/ad1.png", width = 1200, height = 1200)
p <- ggplot(data, aes(ev_T1, wb_T2)) + 
  geom_abline(intercept = 3.32, slope = 0.03, size = 1) +  
  geom_abline(intercept = 3.23, slope = 0.00, size = 1, linetype = 2) + 
  ylim(3.00, 4.00) + xlim(-3, 3)
p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), axis.ticks = element_line(size = 1,linetype = "blank")) 
dev.off()

# T2->T3は強いヴァンテージ感受性モデルが支持された ----

# B0（切片）=3.24
# B1（傾き：低感受性群）= 0.00
# C（交差点）=3.00
# B3（傾き：高感受性群）= 0.03

png("figure/ad2.png", width = 1200, height = 1200)
p <- ggplot(data, aes(ev_T2, wb_T3)) + 
  geom_abline(intercept = 3.33, slope = 0.03, size = 1) +  
  geom_abline(intercept = 3.24, slope = 0.00, size = 1, linetype = 2) + 
  ylim(3.00, 4.00) + xlim(-3, 3)
p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), axis.ticks = element_line(size = 1,linetype = "blank")) 
dev.off()

# T3->T4は強いヴァンテージ感受性モデルが支持された ----

# B0（切片）=3.31
# B1（傾き：低感受性群）= 0.00
# C（交差点）=3.00
# B3（傾き：高感受性群）= 0.03

png("figure/ad3.png", width = 1200, height = 1200)
p <- ggplot(data, aes(ev_T3, wb_T4)) + 
  geom_abline(intercept = 3.43, slope = 0.04, size = 1) +  
  geom_abline(intercept = 3.31, slope = 0.00, size = 1, linetype = 2) + 
  ylim(3.00, 4.00) + xlim(-3, 3)
p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), axis.ticks = element_line(size = 1,linetype = "blank")) 
dev.off()


# LEGITパッケージで検算＆作図 ----（最終的にこの結果を論文に記述する）

library(LEGIT) #https://cran.r-project.org/web/packages/LEGIT/vignettes/GxE_testing.html
df <- as.data.frame(data) #データフレームとして明示


# 1時点目の強いヴァンテージ感受性モデル ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"hsc_T1", drop=FALSE], env=df[,"ev_T1", drop = FALSE], formula_noGxE = wb_T1 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits$vantage_sensitivity_STRONG, xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 2時点目の強い素因ストレスモデル ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"hsc_T2", drop=FALSE], env=df[,"ev_T2", drop = FALSE], formula_noGxE = wb_T2 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 3時点目の強い素因ストレスモデル ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"hsc_T3", drop=FALSE], env=df[,"ev_T3", drop = FALSE], formula_noGxE = wb_T3 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
summary(GxE_test_BIC$fits$diathesis_stress_STRONG) 
summary(GxE_test_BIC$fits$diathesis_stress_WEAK) 
summary(GxE_test_BIC$fits$vantage_sensitivity_STRONG) 
summary(GxE_test_BIC$fits$vantage_sensitivity_WEAK) 
summary(GxE_test_BIC$fits$diff_suscept_STRONG) 
summary(GxE_test_BIC$fits$diff_suscept_WEAK) 

plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 4時点目の弱いヴァンテージ感受性モデル ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"hsc_T4", drop=FALSE], env=df[,"ev_T4", drop = FALSE], formula_noGxE = wb_T4 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 1か月間の強い素因ストレスモデル ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"hsc_onemonth", drop=FALSE], env=df[,"ev_onemonth", drop = FALSE], formula_noGxE = wb_onemonth ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
summary(GxE_test_BIC$fits$diathesis_stress_STRONG) 
summary(GxE_test_BIC$fits$diathesis_stress_WEAK) 
summary(GxE_test_BIC$fits$vantage_sensitivity_STRONG) 
summary(GxE_test_BIC$fits$vantage_sensitivity_WEAK) 

plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 追加分析の2時点目：強い素因ストレスモデル ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"hsc_T1", drop=FALSE], env=df[,"ev_T1", drop = FALSE], formula_noGxE = wb_T2 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 追加分析の3時点目：強い素因ストレスモデル ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"hsc_T2", drop=FALSE], env=df[,"ev_T2", drop = FALSE], formula_noGxE = wb_T3 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 追加分析の4時点目：強い素因ストレスモデル ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"hsc_T3", drop=FALSE], env=df[,"ev_T3", drop = FALSE], formula_noGxE = wb_T4 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")



# AESだけで追加分析 ----（考察の論拠として）

library(LEGIT) #https://cran.r-project.org/web/packages/LEGIT/vignettes/GxE_testing.html
df <- as.data.frame(data) #データフレームとして明示


# 1時点目のモデル：VS支持 ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"aes_T1", drop=FALSE], env=df[,"ev_T1", drop = FALSE], formula_noGxE = wb_T1 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits$vantage_sensitivity_STRONG, xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 2時点目のモデル：VS支持 ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"aes_T2", drop=FALSE], env=df[,"ev_T2", drop = FALSE], formula_noGxE = wb_T2 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 3時点目のモデル：VS支持 ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"aes_T3", drop=FALSE], env=df[,"ev_T3", drop = FALSE], formula_noGxE = wb_T3 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 4時点目のモデル：VS支持----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"aes_T4", drop=FALSE], env=df[,"ev_T4", drop = FALSE], formula_noGxE = wb_T4 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 追加分析の2時点目：VS支持 ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"aes_T1", drop=FALSE], env=df[,"ev_T1", drop = FALSE], formula_noGxE = wb_T2 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 追加分析の3時点目：VS支持 ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"aes_T2", drop=FALSE], env=df[,"ev_T2", drop = FALSE], formula_noGxE = wb_T3 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

# 追加分析の4時点目：VS支持 ----
GxE_test_BIC = GxE_interaction_test(data=df, genes=df[,"aes_T3", drop=FALSE], env=df[,"ev_T3", drop = FALSE], formula_noGxE = wb_T4 ~ 1, crossover = c("min","max"), criterion="BIC")
GxE_test_BIC
# fits[[1]] is the best model (based on BIC)
summary(GxE_test_BIC$fits[[1]]) 
plot(GxE_test_BIC$fits[[1]], xlim=c(-3,3), ylim=c(1,4), legend = "aa")

