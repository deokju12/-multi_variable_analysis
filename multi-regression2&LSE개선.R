Infection.csv는 병원에서의 감염위험에 대한 데이터이다. 아래의 변수들을 사용하여 회귀
분석을 시행하고자 한다. 
Y = infection risk in hospital
X1 = average length of patient’s stay (in days)
X2 = a measure of frequency of giving X-rays
X3 = indication in which of 4 U.S. regions the hospital is located: north-east(1), northcentral(2), south(3), west(4).

a. 고려하고 있는 네 개의 변수 간의 산점도행렬을 확인하여라. 이상치가 발견되는가?
Stay 변수를 기준으로 가장 큰 두 관측치를 제외하고 아래의 분석을 진행하기로 한다.

infection <- read.csv("infection.csv")
infection1<-infection[,c("InfctRsk","Stay","Xray","Region")]
infection1
idx=infection$Stay>17
which(idx)
pairs(infection1,col=(idx+1))

stay변수가 17보다 큰 47번, 112번 자료에서 이상치가 나타난다

b. Infection risk를 나머지 세 변수로 셜명하는 모형을 적합하여라. 각 회귀계수의 유의성
을 테스트 하고 유의한 계수의 의미를 해석하시오. 

infection1$Region <- as.character(infection1$Region)
model2=lm(InfctRsk~Stay+Xray+Region, data=infection1[!idx,])
summary(model2)

적합했을 때 Stay는 P값이 1.11e-08으로 아주 유의미하다고 볼 수 있다. Stay가 1늘어나고
Xray,Region이 고정될 때 infctRsk(감염될위험)가 0.5를 상승한다는 것이다. Xray도 pr값이
0.00238로 유의미하다고 볼 수 있다. Xray 횟수가 1번 늘어나고 다른 변수(Stay,Region)가 고정될
때 InfctRsk가 0.01정도 상승한다. Region4도 유의미한 p값을 가지고 있다고 볼 수 있다. 여기서
Region4(west)는 다른 변수(Stay,Xray)들이 고정될 때 Region1(north-east)보다 infctRsk가 1.05정도
상승한다.

c.Region이 west인 지역을 reference level로 사용하여 모형을 다시 적합하고 유의한 회귀계
수의 의미를 해석하시오. B의 결과와 어떻게 다른가?

infection1$Region <- as.factor(infection1$Region)
infection1$Region <- relevel(infection1$Region, ref="4")
model3=lm(InfctRsk~Stay+Xray+Region, data=infection1[!idx,])
summary(model3)

Region1,2,3이 모두 유의미(p값이 모두 0.05보다 낮음)하며 다른 변수들이 고정 될 때
Region4(West) 보다 감염될 확률이 north-east(1)은 1.05 낮고, north-central(2)는 0.88, south(3)은
0.96이 낮다. Stay,Xray 변수와 P값은 변화가 없다

# set.seed의 숫자는 첫번째로 넘버링할 숫자를 의미한다.

3. ISLR 패키지의 College 자료를 사용하여 접수된 지원서의 수(Apps)를 예측할 것이다.
a. 자료를 random하게 70%의 train set과 test set으로 분할하여라
l
ibrary(ISLR)
summary(College)
sum(is.na(College))
set.seed(1)
train=sample(1:nrow(College), nrow(College)*0.7)
train
test=(-train)

b. 최상의 부분집합 선택 방법을 사용하여 train set으로 선형모델을 적합하고 adjusted rsquare기준으로 최적의 모형을 선택하여라. 최적의 모형에 포함되는 설명변수의 수는
몇인가? 최적의 모형을 적합하고 test set에 대한 MSE를 계산하여라.

library(leaps)
fit=regsubsets(Apps~., data=College[train,], nvmax=17)
fit.summary=summary(fit)
fit.summary
plot(fit.summary$adjr2, type='l')
which.max(fit.summary$adjr2)
points(11,fit.summary$adjr2[11], col="red",cex=2,pch=20)

coef=coef(fit, id=11)
coef
test.mat = model.matrix(~ Private+Accept+Enroll+Top10perc+Top25perc+
                          F.Undergrad+Outstate+Room.Board+Terminal
                        +Expend+Grad.Rate , data=College[test,])
pred = test.mat %*% coef

MSE = mean((College$Apps[test] - pred)^2)

test.mat

library(glmnet)
x=model.matrix(Apps~., College)[,-1]
y=College$Apps

c. 10-fold cross-validation으로 선택된 lambda값을 가지고 train set에 ridge regression을
적합하고 test set에 대한 MSE를 계산하여라.

grid=10^seq(10, -2, length=100)
set.seed(1)
train=sample(1:nrow(x), nrow(x)*0.7)
test=(-train)
# k-fold cross validation 하는 함수 cv.glmnet)
cv.glmnet
ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid) -그리드 나눈거 집어 넣기
cv.out=cv.glmnet(x[train,], y[train], alpha=0) - k fold cross validation
cv.out
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
log(bestlam)
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y[test])^2)

d. 10-fold cross-validation으로 선택된 lambda값을 가지고 train set에 lasso regression을
적합하고 test set에 대한 MSE를 계산하여라

lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda=grid)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
log(bestlam)
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mean((lasso.pred-y[test])^2)


