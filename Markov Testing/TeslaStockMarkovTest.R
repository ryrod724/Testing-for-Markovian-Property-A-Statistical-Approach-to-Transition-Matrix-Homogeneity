library(markovchain)
library(Hmisc)
library(gtools)

stock<-read.csv("TeslaStockData.csv",header=TRUE,sep=",")
tsla<-stock$Close

results<-c()

Bulls<-0 #Over 1.5% gain in stock price
Bears<-0 #Over 1.5% loss in stock price
Stagnants<-0 #in between

for (i in 1:365){
	if (tsla[i+1]>tsla[i]*1.015){
		results[i]<-"Bull"
		Bulls<-Bulls+1
	} else if (tsla[i+1]<tsla[i]*0.985) {
		results[i]<-"Bear"
		Bears<-Bears+1
	} else {
		results[i]<-"Stagnant"
		Stagnants<-Stagnants+1
	}
}

print(results)
print(Bulls)
print(Bears)
print(Stagnants)

BuBu1<-0
BuBe1<-0
BuSt1<-0
BeBu1<-0
BeBe1<-0
BeSt1<-0
StBu1<-0
StBe1<-0
StSt1<-0

BuBu2<-0
BuBe2<-0
BuSt2<-0
BeBu2<-0
BeBe2<-0
BeSt2<-0
StBu2<-0
StBe2<-0
StSt2<-0

for(i in 1:73) {
	if(results[i]=="Bull"&results[i+1]=="Bull"){
		BuBu1<-BuBu1+1
	} else if(results[i]=="Bull"&results[i+1]=="Bear"){
		BuBe1<-BuBe1+1
	} else if(results[i]=="Bull"&results[i+1]=="Stagnant"){
		BuSt1<-BuSt1+1
	} else if(results[i]=="Bear"&results[i+1]=="Bull"){
		BeBu1<-BeBu1+1
	} else if(results[i]=="Bear"&results[i+1]=="Bear"){
		BeBe1<-BeBe1+1
	} else if(results[i]=="Bear"&results[i+1]=="Stagnant"){
		BeSt1<-BeSt1+1
	} else if(results[i]=="Stagnant"&results[i+1]=="Bull"){
		StBu1<-StBu1+1
	} else if(results[i]=="Stagnant"&results[i+1]=="Bear"){
		StBe1<-StBe1+1
	} else if(results[i]=="Stagnant"&results[i+1]=="Stagnant"){
		StSt1<-StSt1+1
	} 
}

Bu1<-BuBe1+BuBu1+BuSt1
Be1<-BeBe1+BeBu1+BeSt1
St1<-StBe1+StBu1+StSt1

for(i in 74:365) {
	if(results[i]=="Bull"&results[i+1]=="Bull"){
		BuBu2<-BuBu2+1
	} else if(results[i]=="Bull"&results[i+1]=="Bear"){
		BuBe2<-BuBe2+1
	} else if(results[i]=="Bull"&results[i+1]=="Stagnant"){
		BuSt2<-BuSt2+1
	} else if(results[i]=="Bear"&results[i+1]=="Bull"){
		BeBu2<-BeBu2+1
	} else if(results[i]=="Bear"&results[i+1]=="Bear"){
		BeBe2<-BeBe2+1
	} else if(results[i]=="Bear"&results[i+1]=="Stagnant"){
		BeSt2<-BeSt2+1
	} else if(results[i]=="Stagnant"&results[i+1]=="Bull"){
		StBu2<-StBu2+1
	} else if(results[i]=="Stagnant"&results[i+1]=="Bear"){
		StBe2<-StBe2+1
	} else if(results[i]=="Stagnant"&results[i+1]=="Stagnant"){
		StSt2<-StSt2+1
	} 
}

Bu2<-BuBe2+BuBu2+BuSt2
Be2<-BeBe2+BeBu2+BeSt2
St2<-StBe2+StBu2+StSt2

expected<-matrix(c(BuBu1*Bu2/Bu1,BuBe1*Bu2/Bu1,BuSt1*Bu2/Bu1,BeBu1*Be2/Be1,BeBe1*Be2/Be1,BeSt1*Be2/Be1,StBu1*St2/St1,StBe1*St2/St1,StSt1*St2/St1),nrow=3,ncol=3,byrow=TRUE)
print(expected)

observed<-matrix(c(BuBu2,BuBe2,BuSt2,BeBu2,BeBe2,BeSt2,StBu2,StBe2,StSt2),nrow=3,ncol=3,byrow=TRUE)
print(observed)

#begining the actual test now
chisq1<- 0

for(i in 1:3) {
  for (j in 1:3) {
    if (expected[i,j] > 0) {
      valueadded<- (observed[i,j]-expected[i,j])^2/expected[i,j]
      print(valueadded)
      chisq1<- chisq1 + valueadded
    }
  }
}

df<- 3*(3-1)-0 
p.value1<-pchisq(chisq1, df=df, lower.tail=FALSE)

df
chisq1
p.value1

