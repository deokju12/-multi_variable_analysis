
# 1번

#Houseprice.csv는 주택판매가격(price)과 이에 영향을 줄 것으로 판단되는 4가지 설명변수인 세금(tax; 만원), 대지평수(ground; 평), 건물평수(floor; 평), 주택연령(year; 년)을 27개 주택에 대해 조사한 것이다.
# 1. 5개 변수들에 대한 산점도 행렬을 작성하고 변수들 간의 관계를 설명하여라. 특히 판매가격과 세금의 산점도를 보면 특이값 3개가 나타난다. 이들 값의 특성은 무엇인가?
houseprice <- read.csv("houseprice.csv")
head(Houseprice)
pairs(Houseprice[,c("price","tax","ground",	"floor","year")])

library(corrplot) 
a <- cor(Houseprice)
corrplot(a,method="ellipse")

#시각화

library(ggplot2)
ggplot(data=Houseprice,
       aes(x=Houseprice$tax,
           y=Houseprice$price)) + geom_point() + geom_smooth(method="lm", se=FALSE)

# 9번 10번 자료는 세금에 비해 집값이 더 비싸다고 볼수 있고 27번 자료는 낸 세금에
# 비해 집값이 더 싸다고 볼수 있다.

par(mfrow = c(2,2))

#2번

#주택 판매가격을 종속변수로, 나머지 4개 변수를 설명변수로 하는 선형회귀모형을 적합하여라.

fit2 =lm(price~tax+ground+floor+year,data=Houseprice)
fit2


# 3번 결정계수는 얼마인가? 주택 판매가격에 유의한 영향을 주는 변수들은 무엇인가? 세금과 floor(건물평수)

summary(fit2)
'결정계수(multiple R-squared) 값은 0.9313 즉 이 다중선형회귀모형의 적합값은 데이터의 실제값
를 아주 잘 설명한다고 볼 수 있다. 표를 보면 p값 이 아주 낮은 ground와 floor(각각 0.00109,8.41e-05)를 볼 수 있는 데 이 두 변수가 다중선형회귀모형에 주택가격에 비교적 많은 영향을 주는 변수라고 할 수 있다.

# 4번
세금이 150만원, 대지평수가 50평, 건물평수가 30평, 주택연령이 3년인 주택의 평균판매
가격에 대한 추정값과 90% 신뢰구간을 구하여라'

fit2 =lm(price~tax+ground+floor+year,data=Houseprice)
predict(fit2, data.frame(tax=100, ground=50, floor=30, year=3), 
        interval="confidence", level=0.90)
# 5번
# 회귀진단을 위한 그래프들과 잔차 대 설명변수들의 산점도를 그리고 설명하여라
plot(fit2)

par(mfrow= c(2,2))
rs
res <-resid(fit2)
plot(res ~ tax, Houseprice, ylab= "Residuals")
abline(h=0, lty=3)
plot(res ~ ground, Houseprice, ylab= "Residuals")
abline(h=0, lty=3)
plot(res ~ floor, Houseprice, ylab= "Residuals")
abline(h=0, lty=3)
plot(res ~ year, Houseprice, ylab= "Residuals")
abline(h=0, lty=3)

# 판매가격과 세금의 산점도에서 관측된 우측 상단의 특이값 2개를 제외하면 회귀분석 결
# 과에 영향을 줄 것으로 예상되는가?
  

fit_orin <- lm(price~tax,data=Houseprice)
plot(fit_orin)



