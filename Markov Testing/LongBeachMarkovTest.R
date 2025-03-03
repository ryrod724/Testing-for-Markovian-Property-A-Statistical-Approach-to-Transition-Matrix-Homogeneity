library(markovchain)
library(Hmisc)
library(gtools)


weather.data<- read.csv("LongBeachWeatherData.csv", header=TRUE, sep=",")

LB<- weather.data$Weather

tomorrow.all<- ifelse(LB=="ms", "A", ifelse(LB %in% c("mc","pc", "c", "t"),"B", ifelse(LB %in% c("f"),"C","D")))
#A - sun, B - cloud, C - fog, D - rain/snow

tomorrow.all<-tomorrow.all[1067:1432] #05/01/2024-04/30/2024

p<-0.2 #proportion taken for matrix
upper<- round(p*length(tomorrow.all),0)

aa1<-0
ab1<-0
ac1<-0
ad1<-0
ba1<-0
bb1<-0
bc1<-0
bd1<-0
ca1<-0
cb1<-0
cc1<-0
cd1<-0
da1<-0
db1<-0
dc1<-0
dd1<-0

aa2<-0
ab2<-0
ac2<-0
ad2<-0
ba2<-0
bb2<-0
bc2<-0
bd2<-0
ca2<-0
cb2<-0
cc2<-0
cd2<-0
da2<-0
db2<-0
dc2<-0
dd2<-0

for (i in 1:upper) {
  	if (tomorrow.all[i]=="A"&tomorrow.all[i+1]=="A") {
  		aa1=aa1+1
  	} else if (tomorrow.all[i]=="A"& tomorrow.all[i+1]=="B") {
  		ab1=ab1+1
  	} else if (tomorrow.all[i]=="A"& tomorrow.all[i+1]=="C") {
  		ac1=ac1+1
  	} else if (tomorrow.all[i]=="A"& tomorrow.all[i+1]=="D") {
  		ad1=ad1+1
  	} else if (tomorrow.all[i]=="B"& tomorrow.all[i+1]=="A") {
  		ba1=ba1+1
  	} else if (tomorrow.all[i]=="B"& tomorrow.all[i+1]=="B") {
  		bb1=bb1+1
  	} else if (tomorrow.all[i]=="B"& tomorrow.all[i+1]=="C") {
  		bc1=bc1+1
  	} else if (tomorrow.all[i]=="B"& tomorrow.all[i+1]=="D") {
  		bd1=bd1+1
  	} else if (tomorrow.all[i]=="C"& tomorrow.all[i+1]=="A") {
  		ca1=ca1+1
  	} else if (tomorrow.all[i]=="C"& tomorrow.all[i+1]=="B") {
  		cb1=cb1+1
  	} else if (tomorrow.all[i]=="C"& tomorrow.all[i+1]=="C") {
  		cc1=cc1+1
  	} else if (tomorrow.all[i]=="C"& tomorrow.all[i+1]=="D") {
  		cd1=cd1+1
  	} else if (tomorrow.all[i]=="D"& tomorrow.all[i+1]=="A") {
  		da1=da1+1
  	} else if (tomorrow.all[i]=="D"& tomorrow.all[i+1]=="B") {
  		db1=db1+1
  	} else if (tomorrow.all[i]=="D"& tomorrow.all[i+1]=="C") {
  		dc1=dc1+1
  	} else if (tomorrow.all[i]=="D"& tomorrow.all[i+1]=="D") {
  		dd1=dd1+1
  	} 
  } 
  
for (i in upper+1:length(tomorrow.all)) {
  	if (tomorrow.all[i]=="A" & tomorrow.all[i+1]=="A") {
  		aa2=aa2+1
  	} else if (tomorrow.all[i]=="A" & tomorrow.all[i+1]=="B") {
  		ab2=ab2+1
  	} else if (tomorrow.all[i]=="A" & tomorrow.all[i+1]=="C") {
  		ac2=ac2+1
  	} else if (tomorrow.all[i]=="A" & tomorrow.all[i+1]=="D") {
  		ad2=ad2+1
  	} else if (tomorrow.all[i]=="B" & tomorrow.all[i+1]=="A") {
  		ba2=ba2+1
  	} else if (tomorrow.all[i]=="B" & tomorrow.all[i+1]=="B") {
  		bb2=bb2+1
  	} else if (tomorrow.all[i]=="B" & tomorrow.all[i+1]=="C") {
  		bc2=bc2+1
  	} else if (tomorrow.all[i]=="B" & tomorrow.all[i+1]=="D") {
  		bd2=bd2+1
  	} else if (tomorrow.all[i]=="C" & tomorrow.all[i+1]=="A") {
  		ca2=ca2+1
  	} else if (tomorrow.all[i]=="C" & tomorrow.all[i+1]=="B") {
  		cb2=cb2+1
  	} else if (tomorrow.all[i]=="C" & tomorrow.all[i+1]=="C") {
  		cc2=cc2+1
  	} else if (tomorrow.all[i]=="C" & tomorrow.all[i+1]=="D") {
  		cd2=cd2+1
  	} else if (tomorrow.all[i]=="D" & tomorrow.all[i+1]=="A") {
  		da2=da2+1
  	} else if (tomorrow.all[i]=="D" & tomorrow.all[i+1]=="B") {
  		db2=db2+1
  	} else if (tomorrow.all[i]=="D" & tomorrow.all[i+1]=="C") {
  		dc2=dc2+1
  	} else if (tomorrow.all[i]=="D" & tomorrow.all[i+1]=="D") {
  		dd2=dd2+1
  	} 
 }

a1<-aa1+ab1+ac1+ad1
b1<-ba1+bb1+bc1+bd1
c1<-ca1+cb1+cc1+cd1
d1<-da1+db1+dc1+dd1

a2<-aa2+ab2+ac2+ad2
b2<-ba2+bb2+bc2+bd2
c2<-ca2+cb2+cc2+cd2
d2<-da2+db2+dc2+dd2

if (a1!=0){
	aa1<-aa1*a2/a1
 	ab1<-ab1*a2/a1
 	ac1<-ac1*a2/a1
 	ad1<-ad1*a2/a1
} else {
	aa1<-0
	ab1<-0
	ac1<-0
	ad1<-0
}

if (b1!=0){
	ba1<-ba1*b2/b1
 	bb1<-bb1*b2/b1
 	bc1<-bc1*b2/b1
 	bd1<-bd1*b2/b1
} else {
	ba1<-0
	bb1<-0
	bc1<-0
	bd1<-0
}

if (c1!=0){
	ca1<-ca1*c2/c1
 	cb1<-cb1*c2/c1
 	cc1<-cc1*c2/c1
 	cd1<-cd1*c2/c1
} else {
	ca1<-0
	cb1<-0
	cc1<-0
	cd1<-0
}

if (d1!=0){
	da1<-da1*d2/d1
 	db1<-db1*d2/d1
 	dc1<-dc1*d2/d1
 	dd1<-dd1*d2/d1
} else {
	da1<-0
	db1<-0
	dc1<-0
	dd1<-0
}
 	
#expected
expected<-c(aa1,ab1,ac1,ad1,ba1,bb1,bc1,bd1,ca1,cb1,cc1,cd1,da1,db1,dc1,dd1)
print(expected)

#observed
observed<-c(aa2,ab2,ac2,ad2,ba2,bb2,bc2,bd2,ca2,cb2,cc2,cd2,da2,db2,dc2,dd2)
print(observed)

chisq.val<-0
for(i in 1:16) {
	if(expected[i]!=0)
	chisq.val<-chisq.val+(observed[i]-expected[i])^2/expected[i]
	}
	
zeros.count<-0
for (i in 1:16){
	if(expected[i]==0){
		zeros.count<-zeros.count+1
		}
		print(zeros.count)
  }

df<- 4*(4-1)-zeros.count #zeros.count expected entries of 0
p.value<- pchisq(chisq.val, df=df, lower.tail=FALSE)

chisq.val
df
p.value