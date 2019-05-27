1.Dengue.csv�� Aedes ��⿡ ���� �����Ǵ� ���⿭�� ���õ� �ڷ��̴�.
- ID: subject ID
- DENGUE: Dengue fever status: 1=yes, 2=no
- AGE in years
- MOSNET: Use of mosquito netting: 0=yes, 1=no
- SECTOR: Geographical sector in which the subject lived: 1,2,3,4, or 5
AGE, MOSNET, SECTOR�� ����Ͽ� DENGUE�� �����ϴ� ������ƽ ȸ�͸����� �����Ͽ���. 

Dengue <- read.csv("Dengue.csv")

library(ISLR)
summary(Dengue)
Dengue["DENGUE"] <- ifelse(Dengue["DENGUE"]==2, 0,1)
Dengue$SECTOR <- as.character(Dengue$SECTOR)
Dengue$MOSNET <- as.character(Dengue$MOSNET)
glm.fit=glm(DENGUE~AGE+MOSNET+SECTOR, data=Dengue, family=binomial)
summary(glm.fit)
�� ȸ�Ͱ���� �������� �����Ͻÿ�.
AGE�� MOSNET�� SECTOR�� �����Ǿ��� �� P���� 0.00738�̹Ƿ� ���ǹ��ϴٰ� �� �� �ִ�. ��
�� MOSNET�� AGE�� SECTOR�� �����Ǿ��� �� P���� 0.79314�̹Ƿ� ���ǹ����� �ʴ�. ����
SECTOR2�� AGE�� MOSNET1�� �������� �� P���� 0.16238�̹Ƿ� ���ǹ����� �ʴ�. ������
SECTOR3,4,5 AGE�� MOSTNET1�� �������� �� ��� P���� 0.05���� �۱� ������ ���ǹ��ϴ�


���̿� sector�� �����ϴٸ� ������ ����� ���� ���� ������� ���� �� ���⿭�� ������ odds
�� �� �� �����ϴ°�? �� odds ratio�� ���� 95% �ŷڱ����� ����ϰ� MOSNET ������ ���Ǽ���
���� �Ǵ��Ͻÿ�. (DENGUE YES�� 1�̰� NO�� 0�� )

exp(0.333525)
exp(confint(glm.fit))
MOSNET1�� ����� 0.333525�̰� exp(0.333525)�� ���ϸ� 1.39588�� ���´�. �� AGE�� SECTOR
�� ������ �� MOSNET1(����� �� ��)�� MOSNET0(����� ��)���� ���⿭�� �ɸ� Ȯ���� 1.39��
(39%) �þ�ٴ� ���̴�.
������ MOSNET1�� �ŷڱ����� Ȯ���� ���� �� ����� 1�ΰ��� �����ϰ� �����Ƿ� ���ǹ���
�ٰ� �� �� ����.
exp(confint(glm.fit)*5)

# 1�� �����ϳ� ���ϳİ� �߿� - 1�����ϸ� ���ǹ����� ����

�ٸ� ���������� ���� �����ϴٸ� ���̰� 1�� �����ϸ� ���⿭�� �ɸ� odds�� �� �� �����ϴ°�?
5�� �����ϸ� �� �� �����ϴ°�? �� �� odds ratio�� ���� �ŷڱ����� �����ΰ�?

exp(0.024263)

exp(0.024263*5)

�ٸ� ���������� ���� ������ �� ���̰� 1�������ϸ� ���⿭�� �ɸ� odds�� 1.02456�� �����Ѵ�.
�� ���̰� 1�� ���� �� ���� 2.46% ���� �����Ѵ�.

Bulls <- read.csv("Bulls.csv")
Bulls7 <- Bulls[, -c(1,2)] 
head(Bulls7)

2. Bulls.csv �� ��Ž��忡�� �ŷ��� 76������ �(2�� ����) Ȳ���� Ư���� �ŷ�����(SalePr)
�� ���� �ڷ��̴�. ���� ������ �Ʒ��� ����.
- Breed=1 if Angus, 5 if Hereford, 8 if Simental
- FtFrBody=fat free body (pounds)
- Frame=Scale from 1(small) to 8 (large)
- SaleHt=Sale height at shoulder (inches)
- YrHgt=Yearling height at shoulder (inches)
- PrctFFB=Percent fat-free body
- BkFat=Back fat (inches)
- SaleWt=Sale weight (pounds)
SalePr�� Breed ������ ������ 7���� ������ ����� �ּ��км��� �����Ͽ� �Ʒ���
������ ���Ͻÿ�. ���л� ��İ� ������ ����� ����Ͽ� ���� �м��ϰ� ���Ͻ�
��.

# 1��

# ���л����
pca_covariance <- prcomp(Bulls7)
pca_covariance

# ���������
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
#2��
# ���л�
plot(pca_covariance, type = "l")
summary(pca_corr)

# ������
plot(pca_corr, type = "l")
summary(pca_corr)

# plot���� ��3 �ּ��к��� ���Ⱑ �ϸ��ϰ� ���ϰ� �� ���ķδ� ���� �ǹ̰� ���� ������
# ��3 �ּ��б��� 3���� �ּ����� �����Ѵ�.
# �� �ּ����� �߿䵵 �������� Cumulative Proportion�� ����
# ��3 �ּ��б��� ������ ��� ��ü ������ 88.56% �� ���� �������� ���´�.
# ���� ��4 �ּ����� ������(Standard deviation)�� 0.7 ���� �۱� ������ ��3 �ּ��б��� 3���� �ּ����� �����Ѵ�.

#3��
pca_corr$rotation
pca_covariance$rotation
par(mfrow=c(2,4))
for(i in 1:7){
barplot(pca_corr$rotation[,i], col = rainbow(8), las = 2, main = i)
abline(h = -0.3, col="blue")}
for(i in 1:7){
barplot(pca_covariance$rotation[,i], col = rainbow(8), las = 2, main = i)
abline(col="blue")}
#4��
#���Ф�

biplot(pca_covariance)
head(Bulls7)
biplot(pca_corr)
Bulls[c(26, 44), ]   # BkFat ���� ū ��
Bulls[c(63, 57), ]   # PrctFFB ���� ū ��

# ��1, ��2 �ּ��� �����ε� ��ü �������� 99.96%�� ������ �� ������
# �������� ������ �ٸ��� ������ �ᱹ ū ���� ���� FtFrBody�� SaleWt �� ������ �ּ����� �¿��ϰ� �ȴ�.
# �׷��Ƿ� ������ ������ ũ�� ���̳��� ��� scaling�� ����Ǵ� ������ ����� ����ؾ� �Ѵ�.

# (2) ������ ��� �̿�

# BkFat, SaleWt�� ������ ��� ������ ��1 �ּ��п� ���� ����� ������ �⿩�� �ϰ� �ִ�.
# BkFat, SaleWt�� ��2 �ּ����� Ư¡���� �����̴�.
# BkFat ������ ��1 �ּ����� Ư¡�� �ݴ�Ǵ� ������ ���δ�. 
# �� BkFat ���� ū ��� SaleHt, YrHgt, Frame ���� ���� ���� ������ ���δ�.

# Ư�� ��

# 5��
par(mfcol = c(1,2))
biplot(pca_corr)
plot(pca_corr$x[,1], pca_corr$x[,2], xlab = "PC1", ylab = "PC2",
     pch = as.numeric(Bulls$Breed), col = as.numeric(Bulls$Breed))
text(pca_corr$x[,1], pca_corr$x[,2], labels = as.character(Bulls$Breed),
     cex = 0.5, pos = 3, col = as.numeric(Bulls$Breed))
# 6��

qqnorm(pca_covariance$x[,1])
qqline(pca_covariance$x[,1],col="red")