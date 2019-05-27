# multi_variable_analysis

# multi-regression
중회귀분석을 이용해 주택판매가격과 이에 영향을 주는 설명변수들로 분석해 보았다. 산점도행렬, 상관관계 등을 기본적으로
분석하고 특이값을 찾아냈다 이후 유의한 설명변수를 찾고 이상치가 있는지 확인해 봤다. 

# multi-regression2&LSE개선
multi-regression
첫번째부분 코드는 병원에서 감염위험 데이터를 이용해 중회귀분석을 했다. 병원에서 감영위험을 종속변수로 두고 여러가지
설명변수를 이용해 적합해보고 유의미한 변수를 뽑아내고 결과를 해석했다.

LSE개선 
이 부분은 현재까지 계속해서 OLS가정에서 분석을 해왔다면 이번에는 bias를 조금 들리더라도 variance를 많이 줄이는
방향을 찾아 분석해 보았다. 그중에서 best subset과 정규화중 lasso와 ridge을 이용해 분석해 보았다.

# logistic regression$PCA
logistic regression
종속변수가 이진 값으로 구성된 자료를 로지스틱회귀분석을 이용해 분석해 보았다. 


PCA
lse를 개선할 수 있는 방법 중 주성분분석을 이용해 자료를 재구성하고 적합하였다.
