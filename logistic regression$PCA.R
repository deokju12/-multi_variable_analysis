1.Dengue.csv는 Aedes 모기에 의해 전염되는 뎅기열과 관련된 자료이다.
- ID: subject ID
- DENGUE: Dengue fever status: 1=yes, 2=no
- AGE in years
- MOSNET: Use of mosquito netting: 0=yes, 1=no
- SECTOR: Geographical sector in which the subject lived: 1,2,3,4, or 5
AGE, MOSNET, SECTOR를 사용하여 DENGUE를 예측하는 로지스틱 회귀모형을 적합하여라. 

Dengue <- read.csv("Dengue.csv")

library(ISLR)
summary(Dengue)
Dengue["DENGUE"] <- ifelse(Dengue["DENGUE"]==2, 0,1)
Dengue$SECTOR <- as.character(Dengue$SECTOR)
Dengue$MOSNET <- as.character(Dengue$MOSNET)
glm.fit=glm(DENGUE~AGE+MOSNET+SECTOR, data=Dengue, family=binomial)
summary(glm.fit)
각 회귀계수가 유의한지 검정하시오.
AGE는 MOSNET과 SECTOR가 고정되었을 때 P값이 0.00738이므로 유의미하다고 볼 수 있다. 한
편 MOSNET은 AGE와 SECTOR가 고정되었을 때 P값이 0.79314이므로 유의미하지 않다. 또한
SECTOR2는 AGE와 MOSNET1을 고정했을 때 P값이 0.16238이므로 유의미하지 않다. 나머지
SECTOR3,4,5 AGE와 MOSTNET1을 고정했을 때 모두 P값이 0.05보다 작기 때문에 유의미하다


나이와 sector가 동일하다면 모기망을 사용할 때에 비해 사용하지 않을 때 뎅기열에 전염될 odds
는 몇 배 증가하는가? 이 odds ratio에 대한 95% 신뢰구간을 계산하고 MOSNET 변수의 유의성에
대해 판단하시오. (DENGUE YES가 1이고 NO가 0임 )

exp(0.333525)
exp(confint(glm.fit))
MOSNET1의 계수는 0.333525이고 exp(0.333525)를 구하면 1.39588가 나온다. 즉 AGE와 SECTOR
가 동일할 때 MOSNET1(모기장 안 함)은 MOSNET0(모기장 함)보다 뎅기열이 걸릴 확률이 1.39배
(39%) 늘어난다는 것이다.
하지만 MOSNET1의 신뢰구간을 확인해 봤을 때 오즈비가 1인값을 포함하고 있으므로 유의미하
다고 볼 수 없다.
exp(confint(glm.fit)*5)

# 1을 포함하냐 안하냐가 중요 - 1포함하면 유의미하지 않음

다른 설명변수의 값이 동일하다면 나이가 1살 증가하면 뎅기열에 걸릴 odds가 몇 배 증가하는가?
5살 증가하면 몇 배 증가하는가? 이 때 odds ratio에 대한 신뢰구간은 무엇인가?

exp(0.024263)

exp(0.024263*5)

다른 설명변수의 값이 동일할 때 나이가 1살증가하면 뎅기열에 걸릴 odds는 1.02456배 증가한다.
즉 나이가 1살 먹을 때 마다 2.46% 정도 증가한다.

Bulls <- read.csv("Bulls.csv")
Bulls7 <- Bulls[, -c(1,2)] 
head(Bulls7)

2. Bulls.csv 는 경매시장에서 거래된 76마리의 어린(2살 이하) 황소의 특성과 거래가격(SalePr)
에 관한 자료이다. 변수 설명은 아래와 같다.
- Breed=1 if Angus, 5 if Hereford, 8 if Simental
- FtFrBody=fat free body (pounds)
- Frame=Scale from 1(small) to 8 (large)
- SaleHt=Sale height at shoulder (inches)
- YrHgt=Yearling height at shoulder (inches)
- PrctFFB=Percent fat-free body
- BkFat=Back fat (inches)
- SaleWt=Sale weight (pounds)
SalePr와 Breed 변수를 제외한 7개의 변수를 사용해 주성분분석을 시행하여 아래의
질문에 답하시오. 공분산 행렬과 상관계수 행렬을 사용하여 각각 분석하고 비교하시
오.

# 1번

# 공분산행렬
pca_covariance <- prcomp(Bulls7)
pca_covariance

# 상관계수행렬
pca_corr <- prcomp(Bulls7, scale = T)
pca_corr
library(HSAUR2)
library(psych)
pairs.panels(Bulls7)
idx=Bulls7$FtFrBody>1300 | Bulls7$BkFat>0.4 
which(idx)
idx = 44
pairs(Bulls7,col=(idx+1))
pairs(Bulls7)
plot(Bulls7)
pairs.panels(Bulls7)
#2번
# 공분산
plot(pca_covariance, type = "l")
summary(pca_corr)

# 상관계수
plot(pca_corr, type = "l")
summary(pca_corr)

# plot에서 제3 주성분부터 기울기가 완만하게 변하고 그 이후로는 거의 의미가 없기 때문에
# 제3 주성분까지 3개의 주성분을 선택한다.
# 각 주성분의 중요도 누적합인 Cumulative Proportion을 보면
# 제3 주성분까지 선택할 경우 전체 데이터 88.56% 에 대한 설명력을 갖는다.
# 또한 제4 주성분의 고유값(Standard deviation)이 0.7 보다 작기 때문에 제3 주성분까지 3개의 주성분을 선택한다.

#3번
pca_corr$rotation
pca_covariance$rotation
par(mfrow=c(2,4))
for(i in 1:7){
barplot(pca_corr$rotation[,i], col = rainbow(8), las = 2, main = i)
abline(h = -0.3, col="blue")}
for(i in 1:7){
barplot(pca_covariance$rotation[,i], col = rainbow(8), las = 2, main = i)
abline(col="blue")}
#4번
#공분ㅅ

biplot(pca_covariance)
head(Bulls7)
biplot(pca_corr)
Bulls[c(26, 44), ]   # BkFat 값이 큰 소
Bulls[c(63, 57), ]   # PrctFFB 값이 큰 소

# 제1, 제2 주성분 만으로도 전체 데이터의 99.96%를 설명할 수 있지만
# 변수들의 단위가 다르기 때문에 결국 큰 값을 가진 FtFrBody와 SaleWt 두 변수가 주성분을 좌우하게 된다.
# 그러므로 변수간 단위가 크게 차이나는 경우 scaling이 적용되는 상관계수 행렬을 사용해야 한다.

# (2) 상관계수 행렬 이용

# BkFat, SaleWt를 제외한 모든 변수가 제1 주성분에 대해 비슷한 정도로 기여를 하고 있다.
# BkFat, SaleWt는 제2 주성분을 특징짓는 변수이다.
# BkFat 변수는 제1 주성분의 특징과 반대되는 성향을 보인다. 
# 즉 BkFat 값이 큰 경우 SaleHt, YrHgt, Frame 등의 값은 작은 경향을 보인다.

# 특이 값

# 5번
par(mfcol = c(1,2))
biplot(pca_corr)
plot(pca_corr$x[,1], pca_corr$x[,2], xlab = "PC1", ylab = "PC2",
     pch = as.numeric(Bulls$Breed), col = as.numeric(Bulls$Breed))
text(pca_corr$x[,1], pca_corr$x[,2], labels = as.character(Bulls$Breed),
     cex = 0.5, pos = 3, col = as.numeric(Bulls$Breed))
# 6번

qqnorm(pca_covariance$x[,1])
qqline(pca_covariance$x[,1],col="red")
