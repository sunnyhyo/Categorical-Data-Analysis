#Cartegorical data analysis
#lab2- ch2

#2x2 tables

tab<-  matrix(c(60,41,681,692),2,2)
tab

n1<- rowSums(tab)[1]
n2<- rowSums(tab)[2]
n11<- tab[1,1];n21<- tab[2,1]; n12<- tab[1,2];n22<- tab[2,2]

p1<- n11/n1; p2<- n21/n2  
#위약을 먹었는데 죽었을 확률, 치료제를 먹었는데 죽었을 확률

odds.1<- p1/(1-p1); odds.2<- p2/(1-p2)
odds.1  #위약그룹의 odds
odds.2  #치료제그룹의 odds

OR<- odds.1/odds.2
OR
#1.4870

logOR<- log(OR)
SE<- sqrt(1/n11+ 1/n12+ 1/n21+ 1/n22)

L<- logOR-1.96*SE
U<- logOR+1.96*SE

c(L,U)
c(exp(L), exp(U))