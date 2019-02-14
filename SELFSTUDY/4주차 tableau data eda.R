#======================#
# superstore DR 10 EDA #
#======================#
library(tidyverse)
library(dplyr)
library(data.table)

# 데이터 읽기
getwd()
setwd("C:\\Users\\USER\\Documents\\Github\\2019-1-winter-BOAZ\\TABLEAU\\input")
orders_raw <- fread("orders.csv", encoding = "UTF-8")     
orders <- orders_raw # 원데이터 복사

# 데이터 살펴보기
orders <-tbl_df(orders)
head(orders) %>% print(width=Inf)
dim(orders)  
summary(orders)
orders <- 
  orders %>% 
  select(-"Sales % Difference from Previous", -"MTD Sales % Difference",
                  -"MTD Profit % Difference", -"YTD Sales % Difference")
names(orders)    # 36개의 변수 

##########
# 전처리 #
##########
str(orders)

# 변수형태 
# Sales, Ship Date, Order Date : chr -> nummeric  
# Sales: "$", "," 사인제거
names(orders)
x1 <- orders[29]
x1 <- as.vector(sapply(x1, function(x1) if(is.character(x1)) gsub(",", "", x1) else(x1)))
orders[29] <- as.vector(sapply(x1, function(x1) if(is.character(x1)) gsub("\\$", "", x1) else(x1)))
orders$Sales <- as.numeric(orders$Sales)
class(orders$Sales)
# number, date parsing
orders[16] <- as.tibble(lapply(orders[16], parse_date))

orders %>% print(width = Inf)
orders %>% select()
# # 구현사항
# - MAIN TARGET 고객(VIP) 설정 기준(두가지이상 변수 활용 ex구매수량, 구매금액 등)을 정하고 매개변수를 활용해 시각화하시오…
# - 날짜를 매개변수로 사용해서 (세부적인 기준을 각자 설정하여) 판매가치가 높은 상품 분석 및 시각화
# - 할인정책을 세우고자 할때, 할인율을 얼마로 하면 좋을지에 대해 결정할 때 가장 효과적이라 생각되는 그래프
# - 계산된 필드를 이용하여 2014년도와 2015년도를 비교할 수 있도록 효율적으로 시각화(단, 1년 내에 기간 설정은 자유롭게(ex)월별로, 분기별로))

# 4년간의 고객구매데이터
range(orders$`Order Date`)

# 고객별 일별 구매량
orders %>% group_by(`Order Date`, `Customer Name`) %>% 
  summarise(n=sum(`Order Quantity`))
# 고객별 년간 구매건수
freq_year<- 
  orders %>% 
  separate(`Order Date`, into = c("year", "month", "day"), sep ="-", convert=TRUE) %>%
  group_by(year, `Customer Name`) %>%
  summarise(n=sum(`Order Quantity`)) 
# segment당 고객 수
orders %>% count(`Customer Name`)
orders %>% count(`Customer Segment`)
# 개당 마진 변수 생성 
orders <- orders %>% mutate(Unit_profit_margin = `Product Base Margin`*`Unit Price`) 
# 어느 품목이 가장 많은 수익을 내는가
orders %>% group_by(`Product Category`,`Product Sub-Category`,`Product Name`) %>% 
  count(Unit_profit_margin) %>% arrange(desc(Unit_profit_margin)) 
orders %>% group_by(`Product Sub-Category`) %>% 
  count(Unit_profit_margin) %>% arrange(desc(Unit_profit_margin)) 
# total margin per row
orders <- orders %>% transmute(tot_margin_per_purchase = Unit_profit_margin*`Order Quantity`)
# 할인율 범위
range(orders$discount)
# 할인율 - 구매수량 상관관계 = 없다, 유의하지 않다
orders %>% group_by(customer name) 
orders %>% ggplot(aes(Discount, Profit)) + geom_point()
reg.dq<- lm(orders$Profit~orders$Discount)
summary(reg.dq)

names(orders)
# 할인율 - 가격(Unit Price) 상관관계 = 없
orders %>% ggplot(aes(Discount, `Unit Price`)) + geom_jitter()
reg.dq<- lm(orders$`Unit Price`~orders$Discount)
summary(reg.dq)
# Order Quantity * Unit Price * (1-Discount)  = Sale? 아니다...
check<- orders %>% transmute(New_sale = `Order Quantity`*`Unit Price`*(1-Discount)) 
check$New_sale == orders$Sales
# product 종류 1253가지
orders %>% count(`Product Name`)
# 구매빈도
orders %>% group_by(year)

# main target - 이윤을 많이 내는 고객
# 구매수량 * 구매품

count(`Order Date`, `Customer Name`)

####
# 1. 스택막대: position="stack" 

###
# bar chart using geom_bar()
# 1. above vs below 사람 수 bar chart 
# 데이터상 기본 최저시급 15 달러를 기준으로 below/above 구분
wage %>% 
  group_by(Below_Minimum_Wage) %>%      # 두개의 그룹
  count() %>%                           # 그룹별 사람 수
  ggplot(aes(Below_Minimum_Wage, n)) +  # count의 결과값 n - y축 stat="identity" 옵션 
  geom_bar(stat="identity")

# bar chart using geom_col()
wage %>% 
  group_by(Below_Minimum_Wage) %>%      # 두개의 그룹
  count() %>%                           # 그룹별 사람 수
  ggplot(aes(Below_Minimum_Wage, n)) +  # count의 결과값 n - y축 stat="identity" 옵션 
  geom_col()

# 2. 평균근속연수별 시급
# 연속형 x : Service_Time_in_Year (평균근속연수)
# 연속형 y : Hrly_Rate (시급)
wage %>% group_by(Below_Minimum_Wage) %>% count()    # 그룹당 사람 수
# 평균근속연수와 시급 산점도
wage %>% ggplot(aes(Service_Time_in_Years, Hrly_Rate)) + geom_point()  

# 최저임금보다 적게받는지 많이 받는지 (below/above) 색상 추가
# aes(color = Below_Minimum_Wage) 옵션
wage %>% 
  ggplot(aes(Service_Time_in_Years, Hrly_Rate, color = Below_Minimum_Wage)) + 
  geom_point()

# color 옵션을 사용자 지정 
# geom 내에서 aes(colour=) 옵션 지정 후 scale_color_manual(values = c())
# 파란색은 최저임금 인상시 영향을 받을 사람
wage %>% 
  ggplot(aes(Service_Time_in_Years, Hrly_Rate, color = Below_Minimum_Wage)) + 
  geom_point() +
  scale_color_manual(values = c("grey60", "blue"))  # color 옵션을 사용자 지정

# 산점도
# 평균근속연수와 시급 산점도에 기준 선 추가 
# geom_vline(xintercept=) 가로선 / geom_hline(yintercept=) 세로선
# 텍스트 주석  annotate("text", label=)
wage %>% 
  ggplot(aes(Service_Time_in_Years, Hrly_Rate, color = Below_Minimum_Wage)) + 
  geom_point() +
  geom_vline(xintercept = 0, linetype = "longdash", color="grey50", size=0.7) +   # 세로선  geom_vline
  geom_hline(yintercept = 15, linetype = "longdash", color="grey50", size=0.7) +  # 가로선  geom_hline
  annotate("text", x = 0, y = 0, label = "Required Service : 0 year", hjust = -0.1, vjust = -0.1) +  # 텍스트 주석  annotate("text", label=)
  annotate("text", x = 0, y = 15, label = "Minimum Wage : $15", hjust = 0.2, vjust = -0.5) +  # 텍스트 주석  annotate("text", label=)
  lims(x = c(0,40), y = c(0, 35)) +  # x축, y축 범위 조정 lims(x, y), xlim(), ylim()
  scale_color_manual(values = c("grey60", "blue"))  # color 옵션을 사용자지정


####
# 부서의 영향을 보여주는 막대 차트
# 이중막대 차트는 부서별로 세분화된 금액, 인력 수의 영향을 모두 표시한다
# 막대는 금액을 기준으로 내림차순으로 정렬된다.

# 최저임금 조정시 부서별 영향 집계
b <- wage %>% 
  filter(Below_Minimum_Wage == "Below Minimum Wage") %>%   # 최저임금 이하의 직원
  group_by(Budget_Area) %>%    # 부서별 집계
  summarise(sum_Wage_Correction = sum(Wage_Correction), # 최저임금의 금전적 영향
            sum_Record = sum(`레코드_수`)) %>%          # 최저임금 미만의 직원
  arrange(desc(sum_Wage_Correction))   # 내림차순으로 정렬
b

# gather() 함수로 measure 변수 안에 
# "sum_Wage_Correction", "sum_Record" 을 범주로 할당
# facet_warp 을 사용하기 위함
endatalong <- gather(b, 
                     key = "measure", value = "value", 
                     c("sum_Wage_Correction", "sum_Record"))
endatalong %>% print(n = Inf)

# transform()
endatalong %>% 
  transform( measure = factor(measure, levels = c("sum_Wage_Correction", "sum_Record"))) %>%  # facet 상 순서를 바꾸기 위해 levels 범주 순서 바꿈
  ggplot(aes(reorder(Budget_Area, value), value)) +  # 부서별 value 를 내림차순으로 정렬
  geom_bar(stat= "identity") +                       # bar 
  geom_text(aes(label = value), vjust = 0, hjust = 0, color = "grey50") + # value 값을 label로 생성
  # 데이터를 여러 개의 부분집합으로 나누고( measure 범주별 ) 작은 여러 개의 그래프 생성
  facet_wrap(~measure, scale = "free_x") +  
  coord_flip()   # coord_flip()은 x축과 y축의 구성을 뒤집어 표현하라는 명령어
                 # coord_flip은 데이터를 기반으로 하지 않고, bar그래프에 대한 함수도 아니기 때문에 ggplot과 geom_bar 함수 뒤에 레이어 추가 형식으로 지정

####
# 현재의 분포와 그 전망치를 보여주는 히스토그램
# 히스토그램과 연관된 정규 분포는 현재 분포, 분포 전망을 나타낸다

wage %>% group_by(Hrly_Rate_Bin_Size) %>% count() %>% print(n = Inf)

# 3. 현재 임금과 인력 수의 분포 
# 히스토그램
wage %>% 
  ggplot(aes(Hrly_Rate_Bin_Size)) + geom_histogram(binwidth = 1) 

# 최저임금보다 적게받는지 많이 받는지 (below/above) 색상 추가
# aes(fill = Below_Minimum_Wage) 옵션
wage %>% 
  ggplot(aes(x = Hrly_Rate_Bin_Size)) + 
  geom_histogram(aes(fill = Below_Minimum_Wage), binwidth = binwidth)

# fill 옵션 사용자 지정 scale_fill_manual()
# 파란색은 최저임금 인상시 영향을 받을 사람
wage %>% 
  ggplot(aes(x = Hrly_Rate_Bin_Size)) + 
  geom_histogram(aes(fill = Below_Minimum_Wage), binwidth = binwidth) + 
  scale_fill_manual(values = c("grey60", "blue"))  # fill 옵션 사용자 지정


# 정규분포그래프
ggplot(data = wage, aes(Hrly_Rate_Bin_Size)) + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(wage$Hrly_Rate), sd= sd(wage$Hrly_Rate)), 
                color = "darkblue", size = 2)

# 한번에 그려보기! 하지만....
# y축 스케일 범위가 달라서 사용하지 못한다
wage %>% 
  ggplot(aes(Hrly_Rate_Bin_Size)) + 
  geom_histogram(binwidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(wage$Hrly_Rate), sd= sd(wage$Hrly_Rate)), 
                color = "darkblue", size = 2)


####
# 이중축 그리기
# 정규분포 곡선이 있는 히스토그램 그래프
# parameters that will be passed to ``stat_function``

n = nrow(wage)  # 917
mean = mean(wage$Hrly_Rate)  # 16.70716
sd = sd(wage$Hrly_Rate)  #3.642373
binwidth = 1  # 막대사이즈 
# passed to geom_histogram and stat_function

# wage 데이터에 정규분포 데이터 추가
set.seed(1)
wage <- wage %>% mutate(x = rnorm(n, mean, sd))
names(wage)

## 정규분포 
wage %>% 
  ggplot(aes(x = Hrly_Rate_Bin_Size)) + 
  geom_histogram(aes(fill = Below_Minimum_Wage), binwidth = binwidth) + 
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth, 
                color = "darkblue", size = 1) +
  scale_fill_manual(values = c("grey60", "blue"))



####
# 4. 

wage %>% 
  ggplot(aes(Hrly_Rate_Correction_Bin_Size)) + 
  geom_histogram(aes(fill = Below_Minimum_Wage), binwidth = 1) +
  scale_fill_manual(values = c("grey60", "blue"))

# 정규분포곡선1
# y 축 스케일 안맞음
ggplot(data = wage, aes(Hrly_Rate_Correction_Bin_Size)) + 
  stat_function(fun = dnorm, args = list(mean = mean(wage$Hrly_Rate), sd= sd(wage$Hrly_Rate)), 
                color = "darkblue", size = 1)
# 정규분포곡선2
# passed to geom_histogram and stat_function
ggplot(data = wage, aes(Hrly_Rate_Correction_Bin_Size)) + 
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth, 
                color = "darkblue", size = 1)

ggplot(wage, aes(x = Hrly_Rate_Correction_Bin_Size)) + 
  geom_histogram(aes(fill = Below_Minimum_Wage), binwidth = binwidth) + 
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth, 
                color = "darkblue", size = 1) +
  scale_fill_manual(values = c("grey60", "blue"))



# 추가
# 정규분포: 평균과 분산 지정
# (1/(sqrt([STDEV]^2*2*pi()))) * exp(-( (attr([Hrly Rate Bin Size])-[Mean])^2  /  (2*[STDEV]^2)  )) * 1 * TOTAL(SUM([레코드 수]))

ggplot(data.frame(x = c(-5, 5)), aes(x=x)) +
  stat_function(fun=dnorm, args=list(mean=2, sd=1), colour="black", size=1.5) +
  geom_vline(xintercept=2, colour="grey", linetype="dashed", size=1) + # 평균에 세로 직선 추가
  geom_text(x=0, y=0.3, label="x = N(2, 1)") +
  ggtitle("Normal Distribution of x~N(2,1)")

# 출처: http://rfriend.tistory.com/95 [R, Python 분석과 프로그래밍 (by R Friend)]

# 이중축 그리기

ggplot(wage, aes(x = Hrly_Rate_Bin_Size)) + 
  geom_histogram(binwidth = binwidth) + 
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd)*n*binwidth, 
                color = "darkblue", size = 1)
# https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve

# bar 
# bar chart
# 그리는 3가지 방법
wage %>% group_by(Hrly_Rate_Bin_Size) %>% count() %>% 
  ggplot(aes(Hrly_Rate_Bin_Size, n)) + geom_bar(stat = "identity") 

wage %>% group_by(Hrly_Rate_Bin_Size) %>% count() %>% 
  ggplot(aes(Hrly_Rate_Bin_Size, n)) + geom_col() 

wage %>% 
  ggplot(aes(Hrly_Rate_Bin_Size)) + geom_bar(stat = "count")

wage %>% 
  ggplot(aes(Hrly_Rate_Correction_Bin_Size)) + geom_bar(stat = "count")





# (1) 산포도
# (2)부서별bar
# (3) 히스토그램과 정규분포1
# (4) 히스토그램과 정규분포2
