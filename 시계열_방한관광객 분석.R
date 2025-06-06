getwd()
setwd("C:/Users/김보성/Documents/time series")

library(lmtest)           
library(TSA)
library(itsmr)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)
library(forecast)
library(tseries)
library(dplyr)
library(TSlibrary)
library(patchwork)  # 여러 그래프 붙이기용
library(tseries)
library(vars)
library(corrplot)


# import data
tot_df <- read_csv("total variables.csv")

## 날짜 변환
tot_df <- tot_df %>%
  mutate(month = ymd(month))


# STEP 0: 데이터 불러오기
tot_df_full <- read_csv("total variables.csv") %>%
  mutate(month = ymd(month))

# STEP 1: 마지막 5개 행 제거 (존재하지 않았던 것처럼 처리)
h <- 5
n <- nrow(tot_df_full)

# 보관용: 마지막 5개의 실제 값 (나중에 예측 정확도 평가에 사용)
future_td <- tot_df_full$TD[(n - h + 1):n]
future_q2 <- tot_df_full$Q2[(n - h + 1):n]

# 학습용 데이터: 처음 n - 5개까지만 사용
tot_df <- tot_df_full[1:(n - h), ]  # 이후부터 이걸 기준으로 분석

# STEP 2: 시계열 객체 생성 (잘린 데이터 기준)
ts_td <- ts(tot_df$TD, frequency = 12, start = c(2015, 1))

## 시계열화
ts_trc <- ts(tot_df$TRC, frequency = 12, start = c(2015, 1))
ts_ko <- ts(tot_df$KO, frequency = 12, start = c(2015, 1))
ts_co <- ts(tot_df$CO, frequency = 12, start = c(2015, 1))
ts_exr <- ts(tot_df$EXR, frequency = 12, start = c(2015, 1))
ts_rp <- ts(tot_df$RP, frequency = 12, start = c(2015, 1))
ts_sp <- ts(tot_df$SP, frequency = 12, start = c(2015, 1))
ts_erp <- ts(tot_df$ERP, frequency = 12, start = c(2015, 1))
ts_esp <- ts(tot_df$ESP, frequency = 12, start = c(2015, 1))

# 정상성 확인
## List of all variables to check (excluding date and dummies)
vars_to_check <- c("TD", "KO", "CO", "EXR", "RP", "SP", "TRC", "ERP", "ESP")

## Run ADF test on each variable
adf_results <- lapply(vars_to_check, function(var) {
  test_result <- adf.test(tot_df[[var]])
  list(variable = var,
       p_value = test_result$p.value,
       stationary = ifelse(test_result$p.value < 0.05, "Yes", "No"))
})

## Display results
stationarity_df <- do.call(rbind, lapply(adf_results, as.data.frame))
print(stationarity_df)


#plot
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(ts_td, type='l')
title("TD")
acf(ts_td, lag=50)
pacf(ts_td, lag=50)

adf.test(ts_td)

## 1차 차분
ts_td_diff <- diff(ts_td)

## 차분된 시계열과 ACF/PACF 그리기
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(ts_td_diff, type='l', main = "Differenced TD")
acf(ts_td_diff, lag.max = 50)
pacf(ts_td_diff, lag.max = 50)

## 차분된 시계열에 ADF 테스트 수행
adf.test(ts_td_diff)


# 단변량 분석
## STEP 1: ARIMA (비계절성) 적합
fit_arima <- auto.arima(ts_td, seasonal = FALSE)
summary(fit_arima)

## STEP 2: SARIMA (계절성 포함) 적합
fit_sarima <- auto.arima(ts_td, seasonal = TRUE)
summary(fit_sarima)

## STEP 3: ARIMAX (COVID 더미 포함) 적합
fit_arimax <- auto.arima(ts_td, xreg = tot_df$CD, seasonal = FALSE)
summary(fit_arimax)

## STEP 4: ARIMAX (COVID, 계절 변수 더미 포함) 적합
# xreg에 넣을 외생변수들을 데이터프레임 또는 매트릭스로 묶기
xreg_mat <- cbind(tot_df$Q2)

fit_arimax_Q <- auto.arima(ts_td, 
                           xreg = xreg_mat, 
                           seasonal = FALSE)
summary(fit_arimax_Q)


## STEP 5: 모델 AIC 비교
cat("\n--- 모델 비교 ---\n")
cat("ARIMA AIC:", AIC(fit_arima), "\n")
cat("SARIMA AIC:", AIC(fit_sarima), "\n")
cat("ARIMAX AIC:", AIC(fit_arimax), "\n")
cat("ARIMAX AIC:", AIC(fit_arimax_Q), "\n")

## 변수 유의미한지, 잔차는 어떤지 확인
coeftest(fit_arimax_Q)
test(resid(fit_arimax_Q))

## Optional: Ljung-Box test directly
Box.test(residuals(fit_arimax_Q), lag = 12, type = "Ljung-Box")

#예측 정확도 평가
forecast_result <- forecast::forecast(fit_arimax_Q, xreg = future_q2, h = h)

# 예측 결과 확인
print(forecast_result)
accuracy(forecast_result$mean, future_td)



# 시간 벡터 (yearmon)
start_time <- start(ts_td)
freq <- frequency(ts_td)
n <- length(ts_td)

time_actual <- seq(as.yearmon(paste(start_time[1], start_time[2], sep = "-")),
                   by = 1/freq, length.out = n)
time_forecast <- seq(tail(time_actual, 1) + 1/freq,
                     by = 1/freq, length.out = h)

# 하나의 전체 시계열로 연결
df_all <- data.frame(
  Date = c(time_actual, time_forecast),
  Value = c(as.numeric(ts_td), as.numeric(forecast_result$mean)),
  Segment = c(rep("Actual", n), rep("Forecast", h))
)

# 🔧 하나의 선으로 그리되 색상만 구간별로 지정
ggplot(df_all, aes(x = Date, y = Value)) +
  geom_line(color = "grey40", linewidth = 1) +
  geom_line(data = subset(df_all, Segment == "Forecast"),
            aes(x = Date, y = Value), color = "red", linewidth = 1.2) +
  labs(title = "ARIMAX Forecast", x = "Time", y = "TD") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )


##### 다변량 분석
# 예시: 2015년 1월부터 시작, 월별 데이터라고 가정
ts_df <- ts(tot_df[ , -1], start = c(2015, 1), frequency = 12)  # 첫 번째 열이 날짜면 제외


# 제외할 변수 지정
excluded_vars <- c("CD", "Q2", "Q3", "Q4")

# 분석 대상 변수 선택
target <- "TD"
explanatory_vars <- setdiff(colnames(ts_df), c(target, excluded_vars))

# 그래프 배치 및 여백 조정
par(mfrow = c(2, 4),             # 2행 4열 배치
    mar = c(4.5, 4.5, 4, 2),     # 하, 좌, 상, 우 여백 (기본보다 넉넉하게)
    oma = c(1, 1, 2, 1))         # 바깥 여백 (전체 제목 등을 위해)

# CCF 그래프 그리기
for (var in explanatory_vars) {
  ccf(ts_df[, var], ts_df[, target],
      main = paste("CCF:", var, "vs", target))
}


VARselect(ts_df, lag.max = 10, type = "const")

# # TD 제외한 설명변수만 추출
# xvars <- ts_df[, colnames(ts_df) != "TD"]
# 
# # 상관계수 행렬 계산
# cor_mat <- cor(xvars, use = "pairwise.complete.obs")
# 
# dev.off()
# 
# # 시각화
# corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8,
#          addCoef.col = "black", number.cex = 0.7, diag = FALSE)
# 
# cor_mat <- cor(ts_df[, colnames(ts_df) != "TD"], use = "pairwise.complete.obs")
# 
# # 0.8 이상인 상관계수 쌍 (자기 자신 제외)
# high_corr_pairs <- which(abs(cor_mat) >= 0.8 & abs(cor_mat) < 1, arr.ind = TRUE)
# high_corr_vars <- unique(rownames(cor_mat)[high_corr_pairs[,1]])
# print(high_corr_vars)
# 
# library(stats)
# 
# # PCA에 사용된 변수들
# high_corr_vars <- c("TRC", "SP",  "ERP", "ESP", "RP",  "KO")  # 예시
# 
# # TD 제외하고 나머지 중에서 PCA 안 한 변수만 선택
# remaining_vars <- setdiff(colnames(ts_df), c("TD", high_corr_vars))
# print(remaining_vars)
# 
# # 2. PCA 수행 (high_corr_vars 기준)
# pca_input <- scale(ts_df[, high_corr_vars])
# pca_result <- prcomp(pca_input)
# pc1 <- pca_result$x[, 1]
# 
# # 3. lag 1 적용: PC1, CO, EXR
# pc1_lag1 <- c(NA, pc1[-length(pc1)])
# exr_lag1 <- c(NA, ts_df[-nrow(ts_df), "EXR"])
# 
# # 4. 더미 변수: CD, Q2, Q3, Q4
# dummy_vars <- as.matrix(ts_df[, c("Q2", "Q4")])
# 
# # 5. xreg 결합
# xreg_all <- cbind(exr_lag1, dummy_vars)
# colnames(xreg_all)[1] <- c("EXR")
# 
# # 6. NA 제거 및 TD 정렬
# valid_index <- which(!is.na(rowSums(xreg_all)))
# xreg_trim <- xreg_all[valid_index, ]
# td_trim <- ts_df[valid_index, "TD"]
# 
# # 7. ARIMAX 모델 적합
# fit_arimax_final <- auto.arima(td_trim, xreg = xreg_trim, seasonal = FALSE)
# summary(fit_arimax_final)
# 
# ## 변수 유의미한지, 잔차는 어떤지 확인
# coeftest(fit_arimax_final)
# test(resid(fit_arimax_final))


### 그래 뭣하러 지랄하냐 그냥 VARX 써라 대희야
# 내생 변수
endog_vars <- c("TD", "ERP", "ESP", "KO", "CO", "TRC")
Y <- ts_df[, endog_vars]

# 외생 변수: 나머지 변수들
exog_vars <- setdiff(colnames(ts_df), endog_vars)
X <- ts_df[, exog_vars]

# lag.max = 6 정도로 설정
varx_lag_selection <- VARselect(Y, lag.max = 8, type = "const", exogen = X)
print(varx_lag_selection$selection)

fit_varx <- VAR(y = Y, p = 2, type = "const", exogen = X)
summary(fit_varx)

## 변수 유의미한지, 잔차는 어떤지 확인
install.packages("nortest")
library(nortest)  # ad.test()를 위해

# TD 변수에 대한 잔차 추출
resid_td <- resid(fit_varx)[, "TD"]

# # 1. 잔차 시계열 plot
# plot(resid_td, type = "l", main = "Residuals: TD", ylab = "Residuals", xlab = "Time")
# 
# # 2. ACF plot
# acf(resid_td, main = "ACF of Residuals: TD")
# 
# # 3. PACF plot
# pacf(resid_td, main = "PACF of Residuals: TD")
# 
# # 4. Normal Q-Q plot
# qqnorm(resid_td, main = "Normal Q-Q Plot: TD")
# qqline(resid_td, col = "black")

# 5. 정규성 검정
test(resid(fit_varx)[, "TD"])


# ERP변수에 대한 잔차 추출
resid_td <- resid(fit_varx)[, "ERP"]

# 정규성 검정
test(resid(fit_varx)[, "ERP"])

# ESP변수에 대한 잔차 추출
resid_td <- resid(fit_varx)[, "ESP"]

# 정규성 검정  (CO, TRC)
test(resid(fit_varx)[, "ESP"])

# CO변수에 대한 잔차 추출
resid_td <- resid(fit_varx)[, "TRC"]

# 정규성 검정  (CO, TRC)
test(resid(fit_varx)[, "TRC"])


# h-step forecast
# [1] 미래 외생변수 구성
X_future <- as.matrix(tot_df_full[(n - h + 1):n, exog_vars])
forecast_varx <- predict(fit_varx, n.ahead = h, dumvar = X_future)  # X_future: 미래 외생 변수

# TD 예측 추출
pred_td <- forecast_varx$fcst$TD[, "fcst"]

accuracy(pred_td, future_td)
print(pred_td)


# 시간 벡터 (yearmon)
start_time <- start(ts_td)
freq <- frequency(ts_td)
n <- length(ts_td)

time_actual <- seq(as.yearmon(paste(start_time[1], start_time[2], sep = "-")),
                   by = 1/freq, length.out = n)
time_forecast <- seq(tail(time_actual, 1) + 1/freq,
                     by = 1/freq, length.out = h)

# 하나의 전체 시계열로 연결
df_all <- data.frame(
  Date = c(time_actual, time_forecast),
  Value = c(as.numeric(ts_td), as.numeric(pred_td)),
  Segment = c(rep("Actual", n), rep("Forecast", h))
)

# 🔧 하나의 선으로 그리되 색상만 구간별로 지정
ggplot(df_all, aes(x = Date, y = Value)) +
  geom_line(color = "grey40", linewidth = 1) +
  geom_line(data = subset(df_all, Segment == "Forecast"),
            aes(x = Date, y = Value), color = "red", linewidth = 1.2) +
  labs(title = "VAR Forecast", x = "Time", y = "TD") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )


####################분량이 부족하다면 다른 모델도 사용한다
