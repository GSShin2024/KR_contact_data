# 필요한 라이브러리 로드
library(tidyverse)

# 데이터 준비 (실제 데이터를 입력해야 합니다)
data <- data.frame(
  Country = c("United Kingdom", "Sweden", "Italy", "France", "Spain", "Germany", "Japan", "Korea"),
  Average_Time = c(3.67, 4.67, 5, 6, 8.33, 6.33, 34, 20),
  Financial_Incentives = c(2, 2, 2, 2, 1, 2, 0, 0),
  Prescribing_Guidelines = c(2, 2, 2, 2, 2, 2, 0, 0),
  Prescription_Budget = c(2, 2, 2, 0, 1, 1, 0, 0),
  Prescription_Quota = c(0, 2, 2, 1, 1, 1, 0, 0),
  Information_Education = c(2, 2, 2, 2, 2, 2, 1, 0)
)

# NA 값 제거
data_clean <- na.omit(data)

# 회귀 모델 생성
model <- lm(Average_Time ~ Financial_Incentives + Prescribing_Guidelines + 
              Prescription_Budget + Prescription_Quota + Information_Education, 
            data = data_clean)

# 모델 요약
summary(model)

# 결과 시각화
plot(model)



# Demand-side Policies 변수 생성
data$Demand_side_Policies <- rowSums(data[, c("Financial_Incentives", "Prescribing_Guidelines", 
                                              "Prescription_Budget", "Prescription_Quota", 
                                              "Information_Education")])

# 회귀 모델 생성
model <- lm(Average_Time ~ Demand_side_Policies, data = data)

# 모델 요약
summary(model)

# 산점도 생성
plot(data$Demand_side_Policies, data$Average_Time, 
     xlab = "Demand-side Policies Score", ylab = "Average Time",
     main = "Relationship between Policies and Market Penetration Time")
abline(model, col = "red")