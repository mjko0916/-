library(tidyverse)
library(ranger)
library(caret)
library(ModelMetrics)
library(survey)
library(jtools)
library(ggstance)
library(InformationValue)

# -----------------------------------------------------------------------
#             DATA  처리
# ------------------------------------------------------------------------
setwd("C:\\Users\\User\\Desktop\\graduate\\보건의료빅데이터\\프로젝트")
data2016 <- read.csv("data2016.csv")
data2017 <- read.csv("data2017.csv")

# 만나이 19세 이상
data2016 <- data2016 %>% filter(age>=19) %>% filter(DI1_dg!=9 | DI2_dg!=9 | DI3_dg!=9 | DI4_dg!=9) %>% filter(DI3_dg!=9)
data2017 <- data2017 %>% filter(age>=19) %>% filter(DI1_dg!=9 | DI2_dg!=9 | DI3_dg!=9 | DI4_dg!=9)%>% filter(DI3_dg!=9)
data <- rbind(data2016, data2017) 
write.csv(data, file="data.csv", row.names = FALSE)

# 데이터 전처리
# NA 많은 변수 HE_Pb HE_Hg HE_Cd(중금속)
data <- data %>% select(-c(HE_Pb, HE_Hg, HE_Cd))

## 파생변수

### 순환계 질환 전부
data$DI_dg <- as.factor(ifelse(data$DI1_dg==1 | data$DI2_dg==1 | data$DI3_dg==1 | data$DI4_dg==1, 1, 0))

### 유전
data <- data %>% 
  mutate(HE_HPfh = ifelse(data$HE_HPfh1 == 1 | data$HE_HPfh2 == 1 | data$HE_HPfh3 == 1, 1, 0),
         HE_HLfh = ifelse(data$HE_HLfh1 == 1 | data$HE_HLfh2 == 1 | data$HE_HLfh3 == 1, 1, 0),
         HE_IHDfh = ifelse(data$HE_IHDfh1 == 1 | data$HE_IHDfh2 == 1 | data$HE_IHDfh3 == 1, 1, 0),
         HE_STRfh = ifelse(data$HE_STRfh1 == 1 | data$HE_STRfh2 == 1 | data$HE_STRfh3 == 1, 1, 0)) %>%
  select(-c(HE_HPfh1, HE_HPfh2, HE_HPfh3, HE_HLfh1, HE_HLfh2, HE_HLfh3, HE_IHDfh1, HE_IHDfh2, HE_IHDfh3,
            HE_STRfh1, HE_STRfh2, HE_STRfh3))

### 체중변화 - 연속형
data$BO <- ifelse(data$BO1_2 == 1, -4, ifelse(data$BO1_2 == 2, -8, ifelse(data$BO1_2 == 3, -12, 0)))
data$BO <- ifelse(data$BO1_3 == 1, 4, ifelse(data$BO1_3 == 2, 8, ifelse(data$BO1_3 == 3, 12, data$BO)))

# 음주 - 연속형
data$BD <- ifelse(data$BD1_11 == 8, 0, ifelse(data$BD1_11 == 9, NA, 2*data$BD1_11))

# 수면시간
data$Total_slp_wd <- ifelse(data$Total_slp_wd == 9999, NA, data$Total_slp_wd)
data$Total_slp_wk <- ifelse(data$Total_slp_wk == 9999, NA, data$Total_slp_wk)

# 평생 흡연
data$BS <- as.factor(ifelse(data$BS1_1 == 1 | data$BS1_1 == 2, 0, ifelse(data$BS1_1 == 9, NA, 1)))

# 걷기
data$BE <- ifelse(data$BE3_33 == 88, 0, ifelse(data$BE3_33 == 99, NA, data$BE3_33))

# age2
data$age2 <- as.factor(ifelse(data$age >= 65, 1, 0))

# 비만
data$HE_obe <- as.factor(ifelse(data$HE_BMI<18.5, 1, ifelse(data$HE_BMI<25, 2, 3)))


# factor 변환
cols <- c("sex", "edu", "DI1_dg", "DI2_dg", "DI3_dg", "DI4_dg", "mh_stress", "pa_aerobic", "HE_fh",
          "HE_rPLS", "HE_DM", "HE_HCHOL", "DI_dg", "HE_HPfh", "HE_HLfh", "HE_IHDfh",
          "HE_STRfh", "HE_hepaB", "HE_hepaC")
data[cols] <- lapply(data[cols], factor)


# -----------------------------------------------------------------------
#             최종 데이터
# ------------------------------------------------------------------------

# 순환기계질환 전체
data_all <- data %>% select(sex, age, age2, edu, BO, BD, Total_slp_wd, Total_slp_wk, mh_stress, BS, HE_wc, 
                            HE_BMI, HE_DM, HE_obe, HE_fh, pa_aerobic, BE, HE_HBsAg, HE_ast, HE_alt, HE_hepaB, 
                            HE_hcv, HE_hepaC, HE_BUN, HE_crea, HE_Uacid, psu, wt_itvex, kstrata, DI_dg) %>% na.omit()

# 고혈압 HE_chol, HE_HCHOL, HE_HPfh
data_hp <- data %>% select(sex, age, age2, edu, BO, BD, Total_slp_wd, Total_slp_wk, mh_stress, BS, HE_wc, 
                           HE_BMI, HE_DM, HE_obe, pa_aerobic, BE, HE_HBsAg, HE_ast, HE_alt, HE_hepaB, 
                           HE_hcv, HE_hepaC, HE_BUN, HE_crea, HE_Uacid, HE_chol, HE_HCHOL, HE_HPfh, 
                           psu, wt_itvex, kstrata, DI1_dg) %>% na.omit()

# 고지혈증 HE_sbp, HE_dbp, HE_HLfh
data_hl <- data %>% select(sex, age, age2, edu, BO, BD, Total_slp_wd, Total_slp_wk, mh_stress, BS, HE_wc, 
                           HE_BMI, HE_DM, HE_obe, pa_aerobic, BE, HE_HBsAg, HE_ast, HE_alt, HE_hepaB, 
                           HE_hcv, HE_hepaC, HE_BUN, HE_crea, HE_Uacid, HE_sbp, HE_dbp, HE_HLfh, 
                           psu, wt_itvex, kstrata, DI2_dg) %>% na.omit()

# 허혈성심장질환 HE_sbp, HE_dbp, HE_chol, HE_HCHOL, HE_IHDfh
data_ihd <- data %>% select(sex, age, age2, edu, BO, BD, Total_slp_wd, Total_slp_wk, mh_stress, BS, HE_wc, 
                            HE_BMI, HE_DM, HE_obe, pa_aerobic, BE, HE_HBsAg, HE_ast, HE_alt, HE_hepaB, 
                            HE_hcv, HE_hepaC, HE_BUN, HE_crea, HE_Uacid, HE_sbp, HE_dbp, HE_IHDfh, 
                            HE_chol, HE_HCHOL, psu, wt_itvex, kstrata, DI4_dg) %>% na.omit()

# 뇌졸중 HE_sbp, HE_dbp, HE_chol, HE_HCHOL, HE_STRfh
data_str <- data %>% select(sex, age, age2, edu, BO, BD, Total_slp_wd, Total_slp_wk, mh_stress, BS, HE_wc, 
                           HE_BMI, HE_DM, HE_obe, pa_aerobic, BE, HE_HBsAg, HE_ast, HE_alt, HE_hepaB, 
                           HE_hcv, HE_hepaC, HE_BUN, HE_crea, HE_Uacid, HE_sbp, HE_dbp, HE_STRfh, 
                           HE_chol, HE_HCHOL, psu, wt_itvex, kstrata, DI3_dg) %>% na.omit()

# -----------------------------------------------------------------------
#             MODELING
# ------------------------------------------------------------------------

## Confusion Matrix result
result<-function(a,b,c,d){
  accur<-(a+d)/(a+b+c+d)
  recall<-d/(b+d)
  precision<-d/(c+d)
  F1<-2*(precision*recall)/(precision+recall)
  return (c(accur,recall,precision, F1))
}

## svyglm
svyglm_fun <- function(dat, dep_var1, indep_variables){
  set.seed(100)
  trainRowNumbers <- as.vector(createDataPartition(eval(parse(text=paste0(expr(dat), "$", dep_var1))), p=0.8, list=FALSE))
  trainData <- dat[trainRowNumbers,]
  testData <- dat[-trainRowNumbers,]
  assign("trainData", trainData, envir = .GlobalEnv)
  assign("testData", testData, envir = .GlobalEnv)
  
  data_we <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_itvex, data=trainData)
  assign("data_we", data_we, envir = .GlobalEnv)
  
  myexp <- paste0(dep_var1, "~", paste(indep_variables, collapse="+"))
  assign("myexp", myexp, envir = .GlobalEnv)
  svyglm.fit <- step(survey::svyglm(formula = as.formula(myexp),
                            design = eval(data_we),
                            data = trainData,
                            family = quasibinomial(link="logit")), direction="both")
  assign("svyglm.fit", svyglm.fit, envir = .GlobalEnv)
  
  pred<-predict(svyglm.fit, newdata=eval(testData), type="response")
  optCutOff <- optimalCutoff(eval(parse(text=paste0("testData$", dep_var1))), pred)[1]
  pred<-ifelse(pred>optCutOff,1,0)
  assign("pred", pred, envir = .GlobalEnv)
  return(table(pred, eval(parse(text=paste0("testData$", dep_var1)))))
}

# randomforest
rf <- function(dat, dep_var1, indep_variables){
  set.seed(100)
  trainRowNumbers <- as.vector(createDataPartition(eval(parse(text=paste0(expr(dat), "$", dep_var1))), p=0.8, list=FALSE))
  trainData <- dat[trainRowNumbers,]
  testData <- dat[-trainRowNumbers,]
  assign("trainData", trainData, envir = .GlobalEnv)
  assign("testData", testData, envir = .GlobalEnv)
  
  myexp <- paste0(dep_var1, "~", paste(indep_variables, collapse="+"))
  assign("myexp", myexp, envir = .GlobalEnv)
  
  ranger.fit <- ranger(as.formula(myexp), data = trainData)
  ranger.pred <- predict(ranger.fit, testData, type="response")
  pred <- ranger.pred[[1]]
  return(table(pred, eval(parse(text=paste0("testData$", dep_var1)))))
}



# 순환기계질환 전체
svyglm_fun(data_all, "DI_dg", names(data_all)[-c(27:30)])
svyglm_all <- svyglm.fit
plotROC(testData$DI_dg, pred)
plot_summs(svyglm_all, scale=TRUE) +
  ggtitle("순혈기계질환 전체") +
  theme(axis.title.x = NULL,
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 18),
        plot.title = element_text(size=20, hjust=0.5))
result(1227, 241, 214, 472)
rf(data_all, "DI_dg", names(data_all)[-c(27:30)])
result(1219, 265, 222, 448)
rm(trainData);rm(testData);rm(myexp);rm(data_we)


# 고혈압 HE_chol, HE_HCHOL, HE_HPfh
svyglm_fun(data_hp, "DI1_dg", names(data_hp)[-c(29:32)])
svyglm_hp <- svyglm(formula = DI1_dg ~ age + age2 + edu + BO + BD + HE_wc + HE_BMI + HE_DM + 
                      pa_aerobic + HE_HBsAg + HE_alt + HE_hepaB + HE_BUN + HE_crea + 
                      HE_Uacid + HE_chol + HE_HCHOL + HE_HPfh, data = trainData,
                    family = quasibinomial(link="logit"))
result(1510, 213, 127, 303)

rf(data_hp, "DI1_dg", names(data_hp)[-c(29:32)])
result(1516, 224, 121, 292)
rm(trainData);rm(testData);rm(myexp);rm(data_we)

# 고지혈증 HE_sbp, HE_dbp, HE_HLfh
## svyglm
svyglm_fun(data_hl, "DI2_dg", names(data_hl)[-c(29:32)])
svyglm_hl <- svyglm(formula = DI2_dg ~ sex + age + age2 + edu + BO + BD + mh_stress + HE_wc + 
                      HE_BMI + HE_DM + HE_obe + pa_aerobic + HE_HBsAg + HE_alt + 
                      HE_hepaB + HE_hcv + HE_hepaC + HE_BUN + HE_dbp + HE_HLfh + 
                      Total_slp_wd, data = trainData,
                    family = quasibinomial(link="logit"))


lda.fit <- lda(ormula = DI2_dg ~ sex + age + age2 + edu + BO + BD + mh_stress + HE_wc + 
      HE_BMI + HE_DM + HE_obe + pa_aerobic + HE_HBsAg + HE_alt + 
      HE_hepaB + HE_hcv + HE_hepaC + HE_BUN + HE_dbp + HE_HLfh + 
      Total_slp_wd, data = trainData)
pred <- predict(lda.fit)

result(1692, 318, 77, 67)

## randomforest
rf(data_hl, "DI2_dg", names(data_hl)[-c(29:32)])
result(1738, 343, 31, 42)
rm(trainData);rm(testData);rm(myexp);rm(data_we)

# 허혈성심장질환 HE_sbp, HE_dbp, HE_chol, HE_HCHOL, HE_IHDfh
svyglm_fun(data_ihd, "DI4_dg", names(data_ihd)[-c(31:34)])
svyglm_ihd <- svyglm(formula = DI3_dg ~ sex + age + age2 + edu + BO + BD + Total_slp_wk + mh_stress + 
                       HE_BMI + HE_DM + pa_aerobic + BE + HE_ast + HE_alt + HE_hcv + 
                       HE_hepaC + HE_crea + HE_dbp + HE_chol + HE_HCHOL + HE_BUN, data = trainData,
                    family = quasibinomial(link="logit"))
result(2091, 61, 1, 0)

rf(data_ihd, "DI4_dg", names(data_ihd)[-c(31:34)])
result(2108,45,0,0)

rm(trainData);rm(testData);rm(myexp);rm(data_we)

# 뇌졸중 HE_sbp, HE_dbp, HE_chol, HE_HCHOL, HE_STRfh
svyglm_fun(data_str, "DI3_dg", names(data_str)[-c(31:34)])
svyglm_str <- svyglm(formula = DI4_dg ~ sex + age + age2 + BO + Total_slp_wd + Total_slp_wk + 
                       HE_wc + HE_DM + pa_aerobic + HE_hepaC + HE_BUN + HE_crea + 
                       HE_Uacid + HE_sbp + HE_STRfh + HE_chol + HE_HCHOL + mh_stress, data = trainData,
                     family = quasibinomial(link="logit"), design=data_we)
predicted <- predict(svyglm_str, testData, type="response")

result(2017, 45, 1, 0)
rf(data_str, "DI3_dg", names(data_str)[-c(31:34)])
result(2092, 61, 0, 0)
rm(trainData);rm(testData);rm(myexp);rm(data_we)

# -----------------------------------------------------------------------
#             EDA
# ------------------------------------------------------------------------

cat <- function(dat, indep_var1, dep_var1){
  indep_var <- enquo(indep_var1)
  dep_var <- enquo(dep_var1)
  
  percentData <- dat %>% 
    select(!!indep_var, !!dep_var) %>% 
    group_by(!!indep_var) %>% count(!!dep_var) %>%
    mutate(ratio=scales::percent(n/sum(n)))
  ggplot(percentData, aes(x = !!indep_var, y = n, fill = !!dep_var)) + 
    geom_bar(data = percentData, position = "fill", width = 0.7, stat = 'identity') +
    geom_text(data = percentData, aes(y = n, label = ratio), 
              position = position_fill(vjust = 0.5), size = 6)  +
    theme_bw() +
    coord_flip() + 
    scale_fill_brewer() + 
    ggtitle(deparse(substitute(indep_var1))) +
    labs(x = "" , y = "") +
    theme(plot.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.position = "NULL")
}

amount <- function(dat, indep_var1, dep_var1){
  indep_var <- enquo(indep_var1)
  dep_var <- enquo(dep_var1)
  
  ggplot(dat, aes(x = !!dep_var, y = !!indep_var, fill=!!dep_var)) + 
    geom_boxplot(width=0.5) +
    theme_bw() +
    scale_fill_brewer() +
    ggtitle(deparse(substitute(indep_var1))) +
    ylab("") +
    theme(plot.title = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.position = "NULL") 
}


gg1 <- cat(data_all, edu, DI_dg)
gg2 <- cat(data_all %>% filter(HE_fh!=9), HE_fh, DI_dg)
gg3 <- cat(data_all, HE_DM, DI_dg)
gg4 <- amount(data_all, age, DI_dg)
gg5 <- amount(data_all, HE_BMI, DI_dg)
gg6 <- amount(data_all, HE_wc, DI_dg)

library(gridExtra)
grid.arrange(gg1,gg2,gg3,gg4,gg5,gg6, nrow=2)

