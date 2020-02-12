
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



#（2）Autoregressive latent trajectory model ----

#コードはこのgithubサイトが参考になる：https://github.com/cddesja/lavaan-reproducible/blob/master/bollen2004-autoregressive.R

library(lavaan)
library(RAMpath)

#分析に必要な変数のデータセット作成
lcm.data <- data %>% dplyr::select(hsc_T1,hsc_T2,hsc_T3,hsc_T4,hsc_T5,hsc_T6,hsc_T7,hsc_T8,hsc_T9,hsc_T10,wb_T1,wb_T2,wb_T3,wb_T4,wb_T5,wb_T6,wb_T7,wb_T8,wb_T9,wb_T10,ev_T1,ev_T2,ev_T3,ev_T4,ev_T5,ev_T6,ev_T7,ev_T8,ev_T9,ev_T10)
names(lcm.data)

#hscの最適な成長モデルの比較 ----
hsc.fit <- ramLCM(data = lcm.data, 
                         outcome = 1:10, 
                         model= "all", 
                         missing = "fiml") #推定
  #quadratic modelの当てはまりが最適

hsc.fit1 <- ramLCM(data = lcm.data, 
                  outcome = 1:10, 
                  model= "quadratic", 
                  missing = "fiml") #推定
cat(hsc.fit1$model$quadratic)

#with autregression制約なし ----
hsc.fit.sr <-'
level =~ 1* hsc_T1 +1* hsc_T2 +1* hsc_T3 +1* hsc_T4 +1* hsc_T5 +1* hsc_T6 +1* hsc_T7 +1* hsc_T8 +1* hsc_T9 +1* hsc_T10 
slope =~  0 * hsc_T1 + 1 * hsc_T2 + 2 * hsc_T3 + 3 * hsc_T4 + 4 * hsc_T5 + 5 * hsc_T6 + 6 * hsc_T7 + 7 * hsc_T8 + 8 * hsc_T9 + 9 * hsc_T10 
quadratic =~  0 * hsc_T1 + 1 * hsc_T2 + 4 * hsc_T3 + 9 * hsc_T4 + 16 * hsc_T5 + 25 * hsc_T6 + 36 * hsc_T7 + 49 * hsc_T8 + 64 * hsc_T9 + 81 * hsc_T10 
hsc_T1 ~~(vare)* hsc_T1 
hsc_T2 ~~(vare)* hsc_T2 
hsc_T3 ~~(vare)* hsc_T3 
hsc_T4 ~~(vare)* hsc_T4 
hsc_T5 ~~(vare)* hsc_T5 
hsc_T6 ~~(vare)* hsc_T6 
hsc_T7 ~~(vare)* hsc_T7 
hsc_T8 ~~(vare)* hsc_T8 
hsc_T9 ~~(vare)* hsc_T9 
hsc_T10 ~~(vare)* hsc_T10 

#structured residual
hsc_T2 ~ hsc_T1
hsc_T3 ~ hsc_T2
hsc_T4 ~ hsc_T3
hsc_T5 ~ hsc_T4
hsc_T6 ~ hsc_T5
hsc_T7 ~ hsc_T6
hsc_T8 ~ hsc_T7
hsc_T9 ~ hsc_T8
hsc_T10 ~ hsc_T9
'
fit.sr <- growth(hsc.fit.sr, data = data, missing = "fiml")
summary(fit.sr, fit.measures = TRUE, standardized = TRUE)


#制約あり ------
#with autregression制約なし ----
hsc.fit.sr1 <-'
level =~ 1* hsc_T1 +1* hsc_T2 +1* hsc_T3 +1* hsc_T4 +1* hsc_T5 +1* hsc_T6 +1* hsc_T7 +1* hsc_T8 +1* hsc_T9 +1* hsc_T10 
slope =~  0 * hsc_T1 + 1 * hsc_T2 + 2 * hsc_T3 + 3 * hsc_T4 + 4 * hsc_T5 + 5 * hsc_T6 + 6 * hsc_T7 + 7 * hsc_T8 + 8 * hsc_T9 + 9 * hsc_T10 
quadratic =~  0 * hsc_T1 + 1 * hsc_T2 + 4 * hsc_T3 + 9 * hsc_T4 + 16 * hsc_T5 + 25 * hsc_T6 + 36 * hsc_T7 + 49 * hsc_T8 + 64 * hsc_T9 + 81 * hsc_T10 
hsc_T1 ~~(vare)* hsc_T1 
hsc_T2 ~~(vare)* hsc_T2 
hsc_T3 ~~(vare)* hsc_T3 
hsc_T4 ~~(vare)* hsc_T4 
hsc_T5 ~~(vare)* hsc_T5 
hsc_T6 ~~(vare)* hsc_T6 
hsc_T7 ~~(vare)* hsc_T7 
hsc_T8 ~~(vare)* hsc_T8 
hsc_T9 ~~(vare)* hsc_T9 
hsc_T10 ~~(vare)* hsc_T10 

#structured residual
hsc_T2 ~ (a)*hsc_T1
hsc_T3 ~ (a)*hsc_T2
hsc_T4 ~ (a)*hsc_T3
hsc_T5 ~ (a)*hsc_T4
hsc_T6 ~ (a)*hsc_T5
hsc_T7 ~ (a)*hsc_T6
hsc_T8 ~ (a)*hsc_T7
hsc_T9 ~ (a)*hsc_T8
hsc_T10 ~ (a)*hsc_T9
'
fit.sr1 <- growth(hsc.fit.sr1, data = data, missing = "fiml")
summary(fit.sr1, fit.measures = TRUE, standardized = TRUE)




#latent growth model

lt.model <- '
hsc.i =~ 1*hsc_T1 + 1*hsc_T2 + 1*hsc_T3 + 1*hsc_T4 + 1*hsc_T5 + 1*hsc_T6 + 1*hsc_T7 + 1*hsc_T8 + 1*hsc_T9 + 1*hsc_T10
hsc.s =~ 0*hsc_T1 + 1*hsc_T2 + 2*hsc_T3 + 3*hsc_T4 + 4*hsc_T5 + 5*hsc_T6 + 6*hsc_T7 + 7*hsc_T8 + 8*hsc_T9 + 9*hsc_T10
wb.i =~ 1*wb_T1 + 1*wb_T2 + 1*wb_T3 + 1*wb_T4 + 1*wb_T5 + 1*wb_T6 + 1*wb_T7 + 1*wb_T8 + 1*wb_T9 + 1*wb_T10
wb.s =~ 0*wb_T1 + 1*wb_T2 + 2*wb_T3 + 3*wb_T4 + 4*wb_T5 + 5*wb_T6 + 6*wb_T7 + 7*wb_T8 + 8*wb_T9 + 9*wb_T10
ev.i =~ 1*ev_T1 + 1*ev_T2 + 1*ev_T3 + 1*ev_T4 + 1*ev_T5 + 1*ev_T6 + 1*ev_T7 + 1*ev_T8 + 1*ev_T9 + 1*ev_T10
ev.s =~ 0*ev_T1 + 1*ev_T2 + 2*ev_T3 + 3*ev_T4 + 4*ev_T5 + 5*ev_T6 + 6*ev_T7 + 7*ev_T8 + 8*ev_T9 + 9*ev_T10

# estimate the means
hsc.i ~ 1
hsc.s ~ 1
wb.i ~ 1
wb.s ~ 1
ev.i ~ 1
ev.s ~ 1

# estimate the variances/covariances
hsc.i ~~ hsc.i #hscの切片分散
hsc.s ~~ hsc.s #hscの傾き分散
hsc.i ~~ hsc.s #hscの切片と傾きの共分散
wb.i ~~ wb.i #wbの切片分散
wb.s ~~ wb.s #wbの傾き分散
wb.i ~~ wb.s #wbの切片と傾きの共分散
ev.i ~~ ev.i #evの切片分散
ev.s ~~ ev.s #evの傾き分散
ev.i ~~ ev.s #evの切片と傾きの共分散
hsc.i ~~ wb.i #hscの切片とwbの切片の共分散
hsc.i ~~ ev.i #hscの切片とevの切片の共分散
wb.i ~~ ev.i #wbの切片とevの切片の共分散
hsc.s ~~ wb.s #hscの傾きとwbの傾きの共分散
hsc.s ~~ ev.s #hscの傾きとevの傾きの共分散
wb.s ~~ ev.s #wbの傾きとevの傾きの共分散
hsc.i ~~ wb.s #hscの切片とwbの傾きの共分散
hsc.i ~~ ev.s #hscの切片とevの傾きの共分散
wb.i ~~ hsc.s #wbの切片とhscの傾きの共分散
wb.i ~~ ev.s #wbの切片とevの傾きの共分散
ev.i ~~ hsc.s #evの切片とhscの傾きの共分散
ev.i ~~ wb.s #evの切片とwbの傾きの共分散

# estimate the residual variances
hsc_T1 ~~ hsc_T1
hsc_T2 ~~ hsc_T2
hsc_T3 ~~ hsc_T3
hsc_T4 ~~ hsc_T4
hsc_T5 ~~ hsc_T5
hsc_T6 ~~ hsc_T6
hsc_T7 ~~ hsc_T7
hsc_T8 ~~ hsc_T8
hsc_T9 ~~ hsc_T9
hsc_T10 ~~ hsc_T10
wb_T1 ~~ wb_T1
wb_T2 ~~ wb_T2
wb_T3 ~~ wb_T3
wb_T4 ~~ wb_T4
wb_T5 ~~ wb_T5
wb_T6 ~~ wb_T6
wb_T7 ~~ wb_T7
wb_T8 ~~ wb_T8
wb_T9 ~~ wb_T9
wb_T10 ~~ wb_T10
ev_T1 ~~ ev_T1
ev_T2 ~~ ev_T2
ev_T3 ~~ ev_T3
ev_T4 ~~ ev_T4
ev_T5 ~~ ev_T5
ev_T6 ~~ ev_T6
ev_T7 ~~ ev_T7
ev_T8 ~~ ev_T8
ev_T9 ~~ ev_T9
ev_T10 ~~ ev_T10
'
lt.fit <- growth(lt.model, data = data, missing = "fiml")
summary(lt.fit, fit.measures = TRUE)


## Autoregressive model

#制約なしモデル -----
ar.model <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + ev_T3
hsc_T5 ~ hsc_T4 + wb_T4 + ev_T4
hsc_T6 ~ hsc_T5 + wb_T5 + ev_T5
hsc_T7 ~ hsc_T6 + wb_T6 + ev_T6
hsc_T8 ~ hsc_T7 + wb_T7 + ev_T7
hsc_T9 ~ hsc_T8 + wb_T8 + ev_T8
hsc_T10 ~ hsc_T9 + wb_T9 + ev_T9
wb_T2 ~ wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + ev_T3
wb_T5 ~ wb_T4 + hsc_T4 + ev_T4
wb_T6 ~ wb_T5 + hsc_T5 + ev_T5
wb_T7 ~ wb_T6 + hsc_T6 + ev_T6
wb_T8 ~ wb_T7 + hsc_T7 + ev_T7
wb_T9 ~ wb_T8 + hsc_T8 + ev_T8
wb_T10 ~ wb_T9 + hsc_T9 + ev_T9
ev_T2 ~ ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + wb_T3
ev_T5 ~ ev_T4 + hsc_T4 + wb_T4
ev_T6 ~ ev_T5 + hsc_T5 + wb_T5
ev_T7 ~ ev_T6 + hsc_T6 + wb_T6
ev_T8 ~ ev_T7 + hsc_T7 + wb_T7
ev_T9 ~ ev_T8 + hsc_T8 + wb_T8
ev_T10 ~ ev_T9 + hsc_T9 + wb_T9

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
hsc_T9 ~~ wb_T9
hsc_T9 ~~ ev_T9
wb_T9 ~~ ev_T9
hsc_T10 ~~ wb_T10
hsc_T10 ~~ ev_T10
wb_T10 ~~ ev_T10
'

fit <- sem(ar.model, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#自己回帰を等値制約するモデル ------
ar.model.1 <- '
#autregressive plus cross-lagged
hsc_T2 ~ (a)*hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ (a)*hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ (a)*hsc_T3 + wb_T3 + ev_T3
hsc_T5 ~ (a)*hsc_T4 + wb_T4 + ev_T4
hsc_T6 ~ (a)*hsc_T5 + wb_T5 + ev_T5
hsc_T7 ~ (a)*hsc_T6 + wb_T6 + ev_T6
hsc_T8 ~ (a)*hsc_T7 + wb_T7 + ev_T7
hsc_T9 ~ (a)*hsc_T8 + wb_T8 + ev_T8
hsc_T10 ~ (a)*hsc_T9 + wb_T9 + ev_T9
wb_T2 ~ (b)*wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ (b)*wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ (b)*wb_T3 + hsc_T3 + ev_T3
wb_T5 ~ (b)*wb_T4 + hsc_T4 + ev_T4
wb_T6 ~ (b)*wb_T5 + hsc_T5 + ev_T5
wb_T7 ~ (b)*wb_T6 + hsc_T6 + ev_T6
wb_T8 ~ (b)*wb_T7 + hsc_T7 + ev_T7
wb_T9 ~ (b)*wb_T8 + hsc_T8 + ev_T8
wb_T10 ~ (b)*wb_T9 + hsc_T9 + ev_T9
ev_T2 ~ (c)*ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ (c)*ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ (c)*ev_T3 + hsc_T3 + wb_T3
ev_T5 ~ (c)*ev_T4 + hsc_T4 + wb_T4
ev_T6 ~ (c)*ev_T5 + hsc_T5 + wb_T5
ev_T7 ~ (c)*ev_T6 + hsc_T6 + wb_T6
ev_T8 ~ (c)*ev_T7 + hsc_T7 + wb_T7
ev_T9 ~ (c)*ev_T8 + hsc_T8 + wb_T8
ev_T10 ~ (c)*ev_T9 + hsc_T9 + wb_T9

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
hsc_T9 ~~ wb_T9
hsc_T9 ~~ ev_T9
wb_T9 ~~ ev_T9
hsc_T10 ~~ wb_T10
hsc_T10 ~~ ev_T10
wb_T10 ~~ ev_T10
'
fit <- sem(ar.model.1, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)

#すべての自己回帰と交差遅延を等値制約するモデル ------
ar.model.all <- '
#autregressive plus cross-lagged
hsc_T2 ~ (a)*hsc_T1 + (d)*wb_T1 + (e)*ev_T1
hsc_T3 ~ (a)*hsc_T2 + (d)*wb_T2 + (e)*ev_T2
hsc_T4 ~ (a)*hsc_T3 + (d)*wb_T3 + (e)*ev_T3
hsc_T5 ~ (a)*hsc_T4 + (d)*wb_T4 + (e)*ev_T4
hsc_T6 ~ (a)*hsc_T5 + (d)*wb_T5 + (e)*ev_T5
hsc_T7 ~ (a)*hsc_T6 + (d)*wb_T6 + (e)*ev_T6
hsc_T8 ~ (a)*hsc_T7 + (d)*wb_T7 + (e)*ev_T7
hsc_T9 ~ (a)*hsc_T8 + (d)*wb_T8 + (e)*ev_T8
hsc_T10 ~ (a)*hsc_T9 + (d)*wb_T9 + (e)*ev_T9
wb_T2 ~ (b)*wb_T1 + (f)*hsc_T1 + (g)*ev_T1
wb_T3 ~ (b)*wb_T2 + (f)*hsc_T2 + (g)*ev_T2
wb_T4 ~ (b)*wb_T3 + (f)*hsc_T3 + (g)*ev_T3
wb_T5 ~ (b)*wb_T4 + (f)*hsc_T4 + (g)*ev_T4
wb_T6 ~ (b)*wb_T5 + (f)*hsc_T5 + (g)*ev_T5
wb_T7 ~ (b)*wb_T6 + (f)*hsc_T6 + (g)*ev_T6
wb_T8 ~ (b)*wb_T7 + (f)*hsc_T7 + (g)*ev_T7
wb_T9 ~ (b)*wb_T8 + (f)*hsc_T8 + (g)*ev_T8
wb_T10 ~ (b)*wb_T9 + (f)*hsc_T9 + (g)*ev_T9
ev_T2 ~ (c)*ev_T1 + (h)*hsc_T1 + (i)*wb_T1
ev_T3 ~ (c)*ev_T2 + (h)*hsc_T2 + (i)*wb_T2
ev_T4 ~ (c)*ev_T3 + (h)*hsc_T3 + (i)*wb_T3
ev_T5 ~ (c)*ev_T4 + (h)*hsc_T4 + (i)*wb_T4
ev_T6 ~ (c)*ev_T5 + (h)*hsc_T5 + (i)*wb_T5
ev_T7 ~ (c)*ev_T6 + (h)*hsc_T6 + (i)*wb_T6
ev_T8 ~ (c)*ev_T7 + (h)*hsc_T7 + (i)*wb_T7
ev_T9 ~ (c)*ev_T8 + (h)*hsc_T8 + (i)*wb_T8
ev_T10 ~ (c)*ev_T9 + (h)*hsc_T9 + (i)*wb_T9

#residual covariance
hsc_T1 ~~ (j)*wb_T1
hsc_T1 ~~ (k)*ev_T1
wb_T1 ~~ (l)*ev_T1
hsc_T2 ~~ (j)*wb_T2
hsc_T2 ~~ (k)*ev_T2
wb_T2 ~~ (l)*ev_T2
hsc_T3 ~~ (j)*wb_T3
hsc_T3 ~~ (k)*ev_T3
wb_T3 ~~ (l)*ev_T3
hsc_T4 ~~ (j)*wb_T4
hsc_T4 ~~ (k)*ev_T4
wb_T4 ~~ (l)*ev_T4
hsc_T5 ~~ (j)*wb_T5
hsc_T5 ~~ (k)*ev_T5
wb_T5 ~~ (l)*ev_T5
hsc_T6 ~~ (j)*wb_T6
hsc_T6 ~~ (k)*ev_T6
wb_T6 ~~ (l)*ev_T6
hsc_T7 ~~ (j)*wb_T7
hsc_T7 ~~ (k)*ev_T7
wb_T7 ~~ (l)*ev_T7
hsc_T8 ~~ (j)*wb_T8
hsc_T8 ~~ (k)*ev_T8
wb_T8 ~~ (l)*ev_T8
hsc_T9 ~~ (j)*wb_T9
hsc_T9 ~~ (k)*ev_T9
wb_T9 ~~ (l)*ev_T9
hsc_T10 ~~ (j)*wb_T10
hsc_T10 ~~ (k)*ev_T10
wb_T10 ~~ (l)*ev_T10
'
fit <- sem(ar.model.all, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#ｗｂの交差遅延を等値制約するモデル -----
ar.model.wb <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + (a)*wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + (a)*wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + (a)*wb_T3 + ev_T3
hsc_T5 ~ hsc_T4 + (a)*wb_T4 + ev_T4
hsc_T6 ~ hsc_T5 + (a)*wb_T5 + ev_T5
hsc_T7 ~ hsc_T6 + (a)*wb_T6 + ev_T6
hsc_T8 ~ hsc_T7 + (a)*wb_T7 + ev_T7
hsc_T9 ~ hsc_T8 + (a)*wb_T8 + ev_T8
hsc_T10 ~ hsc_T9 + (a)*wb_T9 + ev_T9
wb_T2 ~ wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + ev_T3
wb_T5 ~ wb_T4 + hsc_T4 + ev_T4
wb_T6 ~ wb_T5 + hsc_T5 + ev_T5
wb_T7 ~ wb_T6 + hsc_T6 + ev_T6
wb_T8 ~ wb_T7 + hsc_T7 + ev_T7
wb_T9 ~ wb_T8 + hsc_T8 + ev_T8
wb_T10 ~ wb_T9 + hsc_T9 + ev_T9
ev_T2 ~ ev_T1 + hsc_T1 + (a)*wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + (a)*wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + (a)*wb_T3
ev_T5 ~ ev_T4 + hsc_T4 + (a)*wb_T4
ev_T6 ~ ev_T5 + hsc_T5 + (a)*wb_T5
ev_T7 ~ ev_T6 + hsc_T6 + (a)*wb_T6
ev_T8 ~ ev_T7 + hsc_T7 + (a)*wb_T7
ev_T9 ~ ev_T8 + hsc_T8 + (a)*wb_T8
ev_T10 ~ ev_T9 + hsc_T9 + (a)*wb_T9

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
hsc_T9 ~~ wb_T9
hsc_T9 ~~ ev_T9
wb_T9 ~~ ev_T9
hsc_T10 ~~ wb_T10
hsc_T10 ~~ ev_T10
wb_T10 ~~ ev_T10
'
fit <- sem(ar.model.wb, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#HSCの交差遅延を等値制約するモデル ----
ar.model.hsc <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + ev_T3
hsc_T5 ~ hsc_T4 + wb_T4 + ev_T4
hsc_T6 ~ hsc_T5 + wb_T5 + ev_T5
hsc_T7 ~ hsc_T6 + wb_T6 + ev_T6
hsc_T8 ~ hsc_T7 + wb_T7 + ev_T7
hsc_T9 ~ hsc_T8 + wb_T8 + ev_T8
hsc_T10 ~ hsc_T9 + wb_T9 + ev_T9
wb_T2 ~ wb_T1 + (a)*hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + (a)*hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + (a)*hsc_T3 + ev_T3
wb_T5 ~ wb_T4 + (a)*hsc_T4 + ev_T4
wb_T6 ~ wb_T5 + (a)*hsc_T5 + ev_T5
wb_T7 ~ wb_T6 + (a)*hsc_T6 + ev_T6
wb_T8 ~ wb_T7 + (a)*hsc_T7 + ev_T7
wb_T9 ~ wb_T8 + (a)*hsc_T8 + ev_T8
wb_T10 ~ wb_T9 + (a)*hsc_T9 + ev_T9
ev_T2 ~ ev_T1 + (a)*hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + (a)*hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + (a)*hsc_T3 + wb_T3
ev_T5 ~ ev_T4 + (a)*hsc_T4 + wb_T4
ev_T6 ~ ev_T5 + (a)*hsc_T5 + wb_T5
ev_T7 ~ ev_T6 + (a)*hsc_T6 + wb_T6
ev_T8 ~ ev_T7 + (a)*hsc_T7 + wb_T7
ev_T9 ~ ev_T8 + (a)*hsc_T8 + wb_T8
ev_T10 ~ ev_T9 + (a)*hsc_T9 + wb_T9

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
hsc_T9 ~~ wb_T9
hsc_T9 ~~ ev_T9
wb_T9 ~~ ev_T9
hsc_T10 ~~ wb_T10
hsc_T10 ~~ ev_T10
wb_T10 ~~ ev_T10
'

fit <- sem(ar.model.hsc, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)
 
#EVENTの交差遅延を等値制約するモデル -----
ar.model.ev <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + (a)*ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + (a)*ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + (a)*ev_T3
hsc_T5 ~ hsc_T4 + wb_T4 + (a)*ev_T4
hsc_T6 ~ hsc_T5 + wb_T5 + (a)*ev_T5
hsc_T7 ~ hsc_T6 + wb_T6 + (a)*ev_T6
hsc_T8 ~ hsc_T7 + wb_T7 + (a)*ev_T7
hsc_T9 ~ hsc_T8 + wb_T8 + (a)*ev_T8
hsc_T10 ~ hsc_T9 + wb_T9 + (a)*ev_T9
wb_T2 ~ wb_T1 + hsc_T1 + (a)*ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + (a)*ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + (a)*ev_T3
wb_T5 ~ wb_T4 + hsc_T4 + (a)*ev_T4
wb_T6 ~ wb_T5 + hsc_T5 + (a)*ev_T5
wb_T7 ~ wb_T6 + hsc_T6 + (a)*ev_T6
wb_T8 ~ wb_T7 + hsc_T7 + (a)*ev_T7
wb_T9 ~ wb_T8 + hsc_T8 + (a)*ev_T8
wb_T10 ~ wb_T9 + hsc_T9 + (a)*ev_T9
ev_T2 ~ ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + wb_T3
ev_T5 ~ ev_T4 + hsc_T4 + wb_T4
ev_T6 ~ ev_T5 + hsc_T5 + wb_T5
ev_T7 ~ ev_T6 + hsc_T6 + wb_T6
ev_T8 ~ ev_T7 + hsc_T7 + wb_T7
ev_T9 ~ ev_T8 + hsc_T8 + wb_T8
ev_T10 ~ ev_T9 + hsc_T9 + wb_T9

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
hsc_T9 ~~ wb_T9
hsc_T9 ~~ ev_T9
wb_T9 ~~ ev_T9
hsc_T10 ~~ wb_T10
hsc_T10 ~~ ev_T10
wb_T10 ~~ ev_T10
'

fit <- sem(ar.model.ev, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#HSCとWBの交差遅延を等値制約するモデル ----
ar.model.hscwb <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + (b)*wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + (b)*wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + (b)*wb_T3 + ev_T3
hsc_T5 ~ hsc_T4 + (b)*wb_T4 + ev_T4
hsc_T6 ~ hsc_T5 + (b)*wb_T5 + ev_T5
hsc_T7 ~ hsc_T6 + (b)*wb_T6 + ev_T6
hsc_T8 ~ hsc_T7 + (b)*wb_T7 + ev_T7
hsc_T9 ~ hsc_T8 + (b)*wb_T8 + ev_T8
hsc_T10 ~ hsc_T9 + (b)*wb_T9 + ev_T9
wb_T2 ~ wb_T1 + (a)*hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + (a)*hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + (a)*hsc_T3 + ev_T3
wb_T5 ~ wb_T4 + (a)*hsc_T4 + ev_T4
wb_T6 ~ wb_T5 + (a)*hsc_T5 + ev_T5
wb_T7 ~ wb_T6 + (a)*hsc_T6 + ev_T6
wb_T8 ~ wb_T7 + (a)*hsc_T7 + ev_T7
wb_T9 ~ wb_T8 + (a)*hsc_T8 + ev_T8
wb_T10 ~ wb_T9 + (a)*hsc_T9 + ev_T9
ev_T2 ~ ev_T1 + (a)*hsc_T1 + (b)*wb_T1
ev_T3 ~ ev_T2 + (a)*hsc_T2 + (b)*wb_T2
ev_T4 ~ ev_T3 + (a)*hsc_T3 + (b)*wb_T3
ev_T5 ~ ev_T4 + (a)*hsc_T4 + (b)*wb_T4
ev_T6 ~ ev_T5 + (a)*hsc_T5 + (b)*wb_T5
ev_T7 ~ ev_T6 + (a)*hsc_T6 + (b)*wb_T6
ev_T8 ~ ev_T7 + (a)*hsc_T7 + (b)*wb_T7
ev_T9 ~ ev_T8 + (a)*hsc_T8 + (b)*wb_T8
ev_T10 ~ ev_T9 + (a)*hsc_T9 + (b)*wb_T9

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
hsc_T9 ~~ wb_T9
hsc_T9 ~~ ev_T9
wb_T9 ~~ ev_T9
hsc_T10 ~~ wb_T10
hsc_T10 ~~ ev_T10
wb_T10 ~~ ev_T10
'

fit <- sem(ar.model.hscwb, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#HSCとEVの交差遅延を等値制約するモデル ------
ar.model.hscev <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + (a)*ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + (a)*ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + (a)*ev_T3
hsc_T5 ~ hsc_T4 + wb_T4 + (a)*ev_T4
hsc_T6 ~ hsc_T5 + wb_T5 + (a)*ev_T5
hsc_T7 ~ hsc_T6 + wb_T6 + (a)*ev_T6
hsc_T8 ~ hsc_T7 + wb_T7 + (a)*ev_T7
hsc_T9 ~ hsc_T8 + wb_T8 + (a)*ev_T8
hsc_T10 ~ hsc_T9 + wb_T9 + (a)*ev_T9
wb_T2 ~ wb_T1 + (b)*hsc_T1 + (a)*ev_T1
wb_T3 ~ wb_T2 + (b)*hsc_T2 + (a)*ev_T2
wb_T4 ~ wb_T3 + (b)*hsc_T3 + (a)*ev_T3
wb_T5 ~ wb_T4 + (b)*hsc_T4 + (a)*ev_T4
wb_T6 ~ wb_T5 + (b)*hsc_T5 + (a)*ev_T5
wb_T7 ~ wb_T6 + (b)*hsc_T6 + (a)*ev_T6
wb_T8 ~ wb_T7 + (b)*hsc_T7 + (a)*ev_T7
wb_T9 ~ wb_T8 + (b)*hsc_T8 + (a)*ev_T8
wb_T10 ~ wb_T9 + (b)*hsc_T9 + (a)*ev_T9
ev_T2 ~ ev_T1 + (b)*hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + (b)*hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + (b)*hsc_T3 + wb_T3
ev_T5 ~ ev_T4 + (b)*hsc_T4 + wb_T4
ev_T6 ~ ev_T5 + (b)*hsc_T5 + wb_T5
ev_T7 ~ ev_T6 + (b)*hsc_T6 + wb_T6
ev_T8 ~ ev_T7 + (b)*hsc_T7 + wb_T7
ev_T9 ~ ev_T8 + (b)*hsc_T8 + wb_T8
ev_T10 ~ ev_T9 + (b)*hsc_T9 + wb_T9

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
hsc_T9 ~~ wb_T9
hsc_T9 ~~ ev_T9
wb_T9 ~~ ev_T9
hsc_T10 ~~ wb_T10
hsc_T10 ~~ ev_T10
wb_T10 ~~ ev_T10
'

fit <- sem(ar.model.hscev, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)

#すべての交差遅延を等値制約するモデル ------
ar.model.hscwbev <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + (c)*wb_T1 + (a)*ev_T1
hsc_T3 ~ hsc_T2 + (c)*wb_T2 + (a)*ev_T2
hsc_T4 ~ hsc_T3 + (c)*wb_T3 + (a)*ev_T3
hsc_T5 ~ hsc_T4 + (c)*wb_T4 + (a)*ev_T4
hsc_T6 ~ hsc_T5 + (c)*wb_T5 + (a)*ev_T5
hsc_T7 ~ hsc_T6 + (c)*wb_T6 + (a)*ev_T6
hsc_T8 ~ hsc_T7 + (c)*wb_T7 + (a)*ev_T7
hsc_T9 ~ hsc_T8 + (c)*wb_T8 + (a)*ev_T8
hsc_T10 ~ hsc_T9 + (c)*wb_T9 + (a)*ev_T9
wb_T2 ~ wb_T1 + (b)*hsc_T1 + (a)*ev_T1
wb_T3 ~ wb_T2 + (b)*hsc_T2 + (a)*ev_T2
wb_T4 ~ wb_T3 + (b)*hsc_T3 + (a)*ev_T3
wb_T5 ~ wb_T4 + (b)*hsc_T4 + (a)*ev_T4
wb_T6 ~ wb_T5 + (b)*hsc_T5 + (a)*ev_T5
wb_T7 ~ wb_T6 + (b)*hsc_T6 + (a)*ev_T6
wb_T8 ~ wb_T7 + (b)*hsc_T7 + (a)*ev_T7
wb_T9 ~ wb_T8 + (b)*hsc_T8 + (a)*ev_T8
wb_T10 ~ wb_T9 + (b)*hsc_T9 + (a)*ev_T9
ev_T2 ~ ev_T1 + (b)*hsc_T1 + (c)*wb_T1
ev_T3 ~ ev_T2 + (b)*hsc_T2 + (c)*wb_T2
ev_T4 ~ ev_T3 + (b)*hsc_T3 + (c)*wb_T3
ev_T5 ~ ev_T4 + (b)*hsc_T4 + (c)*wb_T4
ev_T6 ~ ev_T5 + (b)*hsc_T5 + (c)*wb_T5
ev_T7 ~ ev_T6 + (b)*hsc_T6 + (c)*wb_T6
ev_T8 ~ ev_T7 + (b)*hsc_T7 + (c)*wb_T7
ev_T9 ~ ev_T8 + (b)*hsc_T8 + (c)*wb_T8
ev_T10 ~ ev_T9 + (b)*hsc_T9 + (c)*wb_T9

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
hsc_T9 ~~ wb_T9
hsc_T9 ~~ ev_T9
wb_T9 ~~ ev_T9
hsc_T10 ~~ wb_T10
hsc_T10 ~~ ev_T10
wb_T10 ~~ ev_T10
'

fit <- sem(ar.model.hscwbev, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#（2）1か月内でのモデル-----

#制約なし
ar.model <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + ev_T3
wb_T2 ~ wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + ev_T3
ev_T2 ~ ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)
#Akaike (AIC)                                1625.967
#Bayesian (BIC)                              1755.775

#すべての変数の自己回帰に等値制約
ar.model.1 <- '
#autregressive plus cross-lagged
hsc_T2 ~ (a)*hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ (a)*hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ (a)*hsc_T3 + wb_T3 + ev_T3
wb_T2 ~ (b)*wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ (b)*wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ (b)*wb_T3 + hsc_T3 + ev_T3
ev_T2 ~ (c)*ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ (c)*ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ (c)*ev_T3 + hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.1, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)
#Akaike (AIC)                                1619.447
#Bayesian (BIC)                              1736.892

#hscの自己回帰だけに等値制約
ar.model.2 <- '
#autregressive plus cross-lagged
hsc_T2 ~ (a)*hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ (a)*hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ (a)*hsc_T3 + wb_T3 + ev_T3
wb_T2 ~ wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + ev_T3
ev_T2 ~ ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.2, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)

#wbの自己回帰だけに等値制約
ar.model.3 <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + ev_T3
wb_T2 ~ (a)*wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ (a)*wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ (a)*wb_T3 + hsc_T3 + ev_T3
ev_T2 ~ ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.3, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)

#evの自己回帰だけに等値制約
ar.model.4 <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + ev_T3
wb_T2 ~ wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + ev_T3
ev_T2 ~ (a)*ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ (a)*ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ (a)*ev_T3 + hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.4, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#hscとwbの自己回帰だけに等値制約
ar.model.5 <- '
#autregressive plus cross-lagged
hsc_T2 ~ (a)*hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ (a)*hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ (a)*hsc_T3 + wb_T3 + ev_T3
wb_T2 ~ (b)*wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ (b)*wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ (b)*wb_T3 + hsc_T3 + ev_T3
ev_T2 ~ ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.5, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)

#hscとevの自己回帰だけに等値制約
ar.model.6 <- '
#autregressive plus cross-lagged
hsc_T2 ~ (a)*hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ (a)*hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ (a)*hsc_T3 + wb_T3 + ev_T3
wb_T2 ~ wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + ev_T3
ev_T2 ~ (b)*ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ (b)*ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ (b)*ev_T3 + hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.6, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#すべての変数の交差遅延に等値制約
ar.model.7 <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + (a)*wb_T1 + (b)*ev_T1
hsc_T3 ~ hsc_T2 + (a)*wb_T2 + (b)*ev_T2
hsc_T4 ~ hsc_T3 + (a)*wb_T3 + (b)*ev_T3
wb_T2 ~ wb_T1 + (c)*hsc_T1 + (d)*ev_T1
wb_T3 ~ wb_T2 + (c)*hsc_T2 + (d)*ev_T2
wb_T4 ~ wb_T3 + (c)*hsc_T3 + (d)*ev_T3
ev_T2 ~ ev_T1 + (e)*hsc_T1 + (f)*wb_T1
ev_T3 ~ ev_T2 + (e)*hsc_T2 + (f)*wb_T2
ev_T4 ~ ev_T3 + (e)*hsc_T3 + (f)*wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.7, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)
#Akaike (AIC)                                1608.710
#Bayesian (BIC)                              1713.793

#hscの交差遅延だけに等値制約
ar.model.8 <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + ev_T3
wb_T2 ~ wb_T1 + (c)*hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + (c)*hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + (c)*hsc_T3 + ev_T3
ev_T2 ~ ev_T1 + (e)*hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + (e)*hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + (e)*hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.8, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)

#wbの交差遅延だけに等値制約
ar.model.9 <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + (c)*wb_T1 + ev_T1
hsc_T3 ~ hsc_T2 + (c)*wb_T2 + ev_T2
hsc_T4 ~ hsc_T3 + (c)*wb_T3 + ev_T3
wb_T2 ~ wb_T1 + hsc_T1 + ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + ev_T3
ev_T2 ~ ev_T1 + hsc_T1 + (d)*wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + (d)*wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + (d)*wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.8, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#evの交差遅延だけに等値制約
ar.model.10 <- '
#autregressive plus cross-lagged
hsc_T2 ~ hsc_T1 + wb_T1 + (c)*ev_T1
hsc_T3 ~ hsc_T2 + wb_T2 + (c)*ev_T2
hsc_T4 ~ hsc_T3 + wb_T3 + (c)*ev_T3
wb_T2 ~ wb_T1 + hsc_T1 + (c)*ev_T1
wb_T3 ~ wb_T2 + hsc_T2 + (c)*ev_T2
wb_T4 ~ wb_T3 + hsc_T3 + (c)*ev_T3
ev_T2 ~ ev_T1 + hsc_T1 + wb_T1
ev_T3 ~ ev_T2 + hsc_T2 + wb_T2
ev_T4 ~ ev_T3 + hsc_T3 + wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.10, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#すべての変数の自己回帰と交差遅延に等値制約
ar.model.11 <- '
#autregressive plus cross-lagged
hsc_T2 ~ (g)*hsc_T1 + (a)*wb_T1 + (b)*ev_T1
hsc_T3 ~ (g)*hsc_T2 + (a)*wb_T2 + (b)*ev_T2
hsc_T4 ~ (g)*hsc_T3 + (a)*wb_T3 + (b)*ev_T3
wb_T2 ~ (h)*wb_T1 + (c)*hsc_T1 + (d)*ev_T1
wb_T3 ~ (h)*wb_T2 + (c)*hsc_T2 + (d)*ev_T2
wb_T4 ~ (h)*wb_T3 + (c)*hsc_T3 + (d)*ev_T3
ev_T2 ~ (i)*ev_T1 + (e)*hsc_T1 + (f)*wb_T1
ev_T3 ~ (i)*ev_T2 + (e)*hsc_T2 + (f)*wb_T2
ev_T4 ~ (i)*ev_T3 + (e)*hsc_T3 + (f)*wb_T3

#residual covariance
hsc_T1 ~~ wb_T1
hsc_T1 ~~ ev_T1
wb_T1 ~~ ev_T1
hsc_T2 ~~ wb_T2
hsc_T2 ~~ ev_T2
wb_T2 ~~ ev_T2
hsc_T3 ~~ wb_T3
hsc_T3 ~~ ev_T3
wb_T3 ~~ ev_T3
hsc_T4 ~~ wb_T4
hsc_T4 ~~ ev_T4
wb_T4 ~~ ev_T4
'

fit <- sem(ar.model.11, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE, standardized = TRUE)
#Akaike (AIC)                                1600.867
#Bayesian (BIC)                              1693.587

#T5-T8で同じモデルで上記と同じモデル
ar.model.all <- '
#autregressive plus cross-lagged
hsc_T5 ~ (a)*hsc_T4 + (d)*wb_T4 + (e)*ev_T4
hsc_T6 ~ (a)*hsc_T5 + (d)*wb_T5 + (e)*ev_T5
hsc_T7 ~ (a)*hsc_T6 + (d)*wb_T6 + (e)*ev_T6
hsc_T8 ~ (a)*hsc_T7 + (d)*wb_T7 + (e)*ev_T7
wb_T5 ~ (b)*wb_T4 + (f)*hsc_T4 + (g)*ev_T4
wb_T6 ~ (b)*wb_T5 + (f)*hsc_T5 + (g)*ev_T5
wb_T7 ~ (b)*wb_T6 + (f)*hsc_T6 + (g)*ev_T6
wb_T8 ~ (b)*wb_T7 + (f)*hsc_T7 + (g)*ev_T7
ev_T5 ~ (c)*ev_T4 + (h)*hsc_T4 + (i)*wb_T4
ev_T6 ~ (c)*ev_T5 + (h)*hsc_T5 + (i)*wb_T5
ev_T7 ~ (c)*ev_T6 + (h)*hsc_T6 + (i)*wb_T6
ev_T8 ~ (c)*ev_T7 + (h)*hsc_T7 + (i)*wb_T7

#residual covariance
hsc_T5 ~~ wb_T5
hsc_T5 ~~ ev_T5
wb_T5 ~~ ev_T5
hsc_T6 ~~ wb_T6
hsc_T6 ~~ ev_T6
wb_T6 ~~ ev_T6
hsc_T7 ~~ wb_T7
hsc_T7 ~~ ev_T7
wb_T7 ~~ ev_T7
hsc_T8 ~~ wb_T8
hsc_T8 ~~ ev_T8
wb_T8 ~~ ev_T8
'

fit <- sem(ar.model.all, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE)


#すべての変数の自己回帰と交差遅延、残差分散に等値制約
ar.model.12 <- '
#autregressive plus cross-lagged
hsc_T2 ~ (g)*hsc_T1 + (a)*wb_T1 + (b)*ev_T1
hsc_T3 ~ (g)*hsc_T2 + (a)*wb_T2 + (b)*ev_T2
hsc_T4 ~ (g)*hsc_T3 + (a)*wb_T3 + (b)*ev_T3
wb_T2 ~ (h)*wb_T1 + (c)*hsc_T1 + (d)*ev_T1
wb_T3 ~ (h)*wb_T2 + (c)*hsc_T2 + (d)*ev_T2
wb_T4 ~ (h)*wb_T3 + (c)*hsc_T3 + (d)*ev_T3
ev_T2 ~ (i)*ev_T1 + (e)*hsc_T1 + (f)*wb_T1
ev_T3 ~ (i)*ev_T2 + (e)*hsc_T2 + (f)*wb_T2
ev_T4 ~ (i)*ev_T3 + (e)*hsc_T3 + (f)*wb_T3

#residual covariance
hsc_T1 ~~ (j)*wb_T1
hsc_T1 ~~ (k)*ev_T1
wb_T1 ~~ (l)*ev_T1
hsc_T2 ~~ (j)*wb_T2
hsc_T2 ~~ (k)*ev_T2
wb_T2 ~~ (l)*ev_T2
hsc_T3 ~~ (j)*wb_T3
hsc_T3 ~~ (k)*ev_T3
wb_T3 ~~ (l)*ev_T3
hsc_T4 ~~ (j)*wb_T4
hsc_T4 ~~ (k)*ev_T4
wb_T4 ~~ (l)*ev_T4
'

fit <- sem(ar.model.12, data = data, fixed.x = FALSE, missing = "fiml")
summary(fit,  fit.measures = TRUE, standardized = TRUE)
#Akaike (AIC)                                1592.565
#Bayesian (BIC)                              1666.741


#感受性-環境相互作用の検討 ----
#4時点全体の平均値を変数として用いる
data.int <- data %>% 
  dplyr::mutate(hsc_mean = (hsc_T1 + hsc_T2 + hsc_T3 + hsc_T4)/4, na.rm = TRUE) %>% #HSCの1ヵ月平均
  dplyr::mutate(wb_mean = (wb_T1 + wb_T2 + wb_T3 + wb_T4)/4, na.rm = TRUE) %>% #wbの1か月平均
  dplyr::mutate(ev_mean = (ev_T1 + ev_T2 + ev_T3 + ev_T4)/4, na.rm = TRUE) #evの1か月平均
names(data.int)
mean(data.int$hsc_mean, na.rm = TRUE)
sd(data.int$hsc_mean, na.rm = TRUE)
mean(data.int$wb_mean, na.rm = TRUE)
sd(data.int$wb_mean, na.rm = TRUE)
mean(data.int$ev_mean, na.rm = TRUE)
sd(data.int$ev_mean, na.rm = TRUE)

#weak diff
weak_diff <- nls(wb_mean ~ B0 + B1*(ev_mean - C) +
                   B3*((ev_mean - C)*hsc_mean),
                 data = data.int,
                 start = list(B0 = 90, B1 = 0, C = 20, B3 = -1))
summary(weak_diff)
AIC(weak_diff) #111.0736
BIC(weak_diff) #120.2169

#strong diff
strong_diff <- nls(wb_mean ~ B0 + 0*(ev_mean - C) + B3*((ev_mean - C)*hsc_mean),
                   data = data.int,
                   start = list(B0 = 90, C = 20, B3 = -1))
summary(strong_diff)
AIC(strong_diff) #111.0023
BIC(strong_diff) #118.3169


#環境変数の最大値最小値
max(data.int$ev_mean, na.rm = TRUE)
#最大値は2.88

min(data.int$ev_mean, na.rm = TRUE)
#最小値は-0.75


#weak diathesis
weak_diathesis <- nls(wb_mean ~ B0 + B1*(ev_mean - 3) +
                        B3*((ev_mean - 3)*hsc_mean),
                      data = data.int,
                      start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_diathesis)
AIC(weak_diathesis) #111.0782
BIC(weak_diathesis) #118.3927

#strong disthesis
strong_diathesis <- nls(wb_mean ~ B0 + 0*(ev_mean - 3) +
                          B3*((ev_mean - 3)*hsc_mean),
                        data = data.int,
                        start = list(B0 = 90, B3 = -1))
summary(strong_diathesis)
AIC(strong_diathesis) #109.1823
BIC(strong_diathesis) #114.6683

#weak vs
weak_vs <- nls(wb_mean ~ B0 + B1*(ev_mean - (-3.00)) +
                 B3*((ev_mean - (-3.00))*hsc_mean),
               data = data.int,
               start = list(B0 = 90, B1 = 0, B3 = -1))
summary(weak_vs)
AIC(weak_vs) #110.941
BIC(weak_vs) #118.2556

#strong vs
strong_vs <- nls(wb_mean ~ B0 + 0*(ev_mean - (-3.00)) +
                   B3*((ev_mean - (-3.00))*hsc_mean),
                 data = data.int,
                 start = list(B0 = 90, B3 = -1))
summary(strong_vs)
AIC(strong_vs) #109.0124
BIC(strong_vs) #114.4984

#強いVSの作図
p <- ggplot(data, aes(ev_mean, wb_mean)) + 
  geom_abline(intercept = 2.90, slope = 0.01, size = 1) + #切片を0.01の300%(0.03)＋にずらす 
  geom_abline(intercept = 2.87, slope = 0.00, size = 1, linetype = 2) + 
  ylim(2.75, 3.10) + xlim(-3.00, 3.00)
p <- p + theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.line = element_line(colour = "azure4", 
                                   linetype = "solid"), 
          axis.ticks = element_line(size = 1, linetype = "blank"))
p

mean(data.int$wb_mean, na.rm = TRUE)
sd(data.int$wb_mean, na.rm = TRUE)


#級内相関係数 -----
#irrパッケージ読み込み
library(irr)

#ICCに必要な変数だけのデータセットを作成
library(tidyverse)
icc_hsc <- data %>% dplyr::select("hsc_T1", "hsc_T2", "hsc_T3", "hsc_T4")
icc_wb <- data %>% dplyr::select("wb_T1", "wb_T2", "wb_T3", "wb_T4")
icc_ev <- data %>% dplyr::select("ev_T1", "ev_T2", "ev_T3", "ev_T4")

#ICC算出
icc(icc_hsc, "twoway", "agreement") #ICC = 0.777 [0.679 < ICC < 0.856]
icc(icc_wb, "twoway", "agreement") #ICC = 0.638 [0.513 < ICC < 0.751]
icc(icc_ev, "twoway", "agreement") #ICC = 0.223 [0.090 < ICC < 0.384]

#4時点にわたる自己相関
cor.test(icc_hsc$hsc_T1, icc_hsc$hsc_T2)
cor.test(icc_hsc$hsc_T1, icc_hsc$hsc_T3)
cor.test(icc_hsc$hsc_T1, icc_hsc$hsc_T4)
cor.test(icc_hsc$hsc_T2, icc_hsc$hsc_T3)
cor.test(icc_hsc$hsc_T3, icc_hsc$hsc_T4)
