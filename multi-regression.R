
# 1��

#Houseprice.csv�� �����ǸŰ���(price)�� �̿� ������ �� ������ �ǴܵǴ� 4���� ���������� ����(tax; ����), �������(ground; ��), �ǹ����(floor; ��), ���ÿ���(year; ��)�� 27�� ���ÿ� ���� ������ ���̴�.
# 1. 5�� �����鿡 ���� ������ ����� �ۼ��ϰ� ������ ���� ���踦 �����Ͽ���. Ư�� �ǸŰ��ݰ� ������ �������� ���� Ư�̰� 3���� ��Ÿ����. �̵� ���� Ư���� �����ΰ�?
houseprice <- read.csv("houseprice.csv")
head(Houseprice)
pairs(Houseprice[,c("price","tax","ground",	"floor","year")])

library(corrplot) 
a <- cor(Houseprice)
corrplot(a,method="ellipse")

#�ð�ȭ

library(ggplot2)
ggplot(data=Houseprice,
       aes(x=Houseprice$tax,
           y=Houseprice$price)) + geom_point() + geom_smooth(method="lm", se=FALSE)

# 9�� 10�� �ڷ�� ���ݿ� ���� ������ �� ��δٰ� ���� �ְ� 27�� �ڷ�� �� ���ݿ�
# ���� ������ �� �δٰ� ���� �ִ�.

par(mfrow = c(2,2))

#2��

#���� �ǸŰ����� ���Ӻ�����, ������ 4�� ������ ���������� �ϴ� ����ȸ�͸����� �����Ͽ���.

fit2 =lm(price~tax+ground+floor+year,data=Houseprice)
fit2


# 3�� ��������� ���ΰ�? ���� �ǸŰ��ݿ� ������ ������ �ִ� �������� �����ΰ�? ���ݰ� floor(�ǹ����)

summary(fit2)
'�������(multiple R-squared) ���� 0.9313 �� �� ���߼���ȸ�͸����� ���հ��� �������� ������
�� ���� �� �����Ѵٰ� �� �� �ִ�. ǥ�� ���� p�� �� ���� ���� ground�� floor(���� 0.00109,8.41e-05)�� �� �� �ִ� �� �� �� ������ ���߼���ȸ�͸����� ���ð��ݿ� ���� ���� ������ �ִ� ������� �� �� �ִ�.

# 4��
������ 150����, ��������� 50��, �ǹ������ 30��, ���ÿ����� 3���� ������ ����Ǹ�
���ݿ� ���� �������� 90% �ŷڱ����� ���Ͽ���'

fit2 =lm(price~tax+ground+floor+year,data=Houseprice)
predict(fit2, data.frame(tax=100, ground=50, floor=30, year=3), 
        interval="confidence", level=0.90)
# 5��
# ȸ�������� ���� �׷������ ���� �� ������������ �������� �׸��� �����Ͽ���
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

# �ǸŰ��ݰ� ������ ���������� ������ ���� ����� Ư�̰� 2���� �����ϸ� ȸ�ͺм� ��
# ���� ������ �� ������ ����Ǵ°�?
  

fit_orin <- lm(price~tax,data=Houseprice)
plot(fit_orin)


