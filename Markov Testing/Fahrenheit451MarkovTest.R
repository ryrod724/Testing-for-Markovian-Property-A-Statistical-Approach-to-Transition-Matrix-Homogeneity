library(tidyverse)
library(Hmisc)
library(gtools)

clean.string <- read_file("Fahrenheit451.cleaned.txt")

#splitting the string into characters
x2<- strsplit(clean.string, "")

#shifting the text by one place
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")

#making string splits into matrices
x1Matrix=matrix(unlist(x1),ncol=1,byrow=TRUE)
x2Matrix=matrix(unlist(x2),ncol=1,byrow=TRUE)

vowels<-c("a","e","i","o","u")
consonants<- c("b","c","d","f","g","h","j","k","l","m","n","p","q", "r","s","t","v","w","x","y","z")
 
total<-192330
p<-0.2 #proportion taken for matrix
upper<- total*p

#setting all variables to be 0 before adding
v1<-0
v2<-0
c1<-0
c2<-0
vv1<-0
vv2<-0
cc1<-0
cc2<-0
cv1<-0
cv2<-0
vc1<-0
vc2<-0

for (counter in 1:upper){
v1<-ifelse(x2Matrix[[counter]] %in% vowels,v1+1,v1)
c1<-ifelse(x2Matrix[[counter]] %in% consonants,c1+1,c1)
vv1<-ifelse(x1Matrix[[counter]] %in% vowels & x2Matrix[[counter]] %in% vowels,vv1+1,vv1)
vc1<-ifelse(x1Matrix[[counter]] %in% vowels & x2Matrix[[counter]] %in% consonants,vc1+1,vc1)
cv1<- ifelse(x1Matrix[[counter]] %in% consonants & x2Matrix[[counter]] %in% vowels,cv1+1,cv1)
cc1<- ifelse(x1Matrix[[counter]] %in% consonants & x2Matrix[[counter]] %in% consonants,cc1+1,cc1)
}


for (counter in upper+1:total-1){
v2<- ifelse(x2Matrix[[counter]] %in% vowels,v2+1,v2)
c2<- ifelse(x2Matrix[[counter]] %in% consonants,c2+1,c2)
vv2<- ifelse(x1Matrix[[counter]] %in% vowels & x2Matrix[[counter]] %in% vowels,vv2+1,vv2)
vc2<- ifelse(x1Matrix[[counter]] %in% vowels & x2Matrix[[counter]] %in% consonants,vc2+1,vc2)
cv2<- ifelse(x1Matrix[[counter]] %in% consonants & x2Matrix[[counter]] %in% vowels,cv2+1,cv2)
cc2<- ifelse(x1Matrix[[counter]] %in% consonants & x2Matrix[[counter]] %in% consonants,cc2+1,cc2)
}

evv<-sum(vv1)*sum(v2)/sum(v1)
evc<-sum(vc1)*sum(v2)/sum(v1)
ecv<-sum(cv1)*sum(c2)/sum(c1)
ecc<-sum(cc1)*sum(c2)/sum(c1)

#specifying the transition probability matrix
expected<- matrix(c(evv, evc, ecv, ecc), nrow=2, ncol=2, byrow=TRUE)
print(expected)

observed<-matrix(c(sum(vv2), sum(vc2), sum(cv2), sum(cc2)), nrow=2, ncol=2, byrow=TRUE)
print(observed)

chisq1<- 0

for(i in 1:2) {
  for (j in 1:2) {
    if (expected[i,j] > 0) {
      valueadded<- (observed[i,j]-expected[i,j])^2/expected[i,j]
      print(valueadded)
      chisq1<- chisq1 + valueadded
    }
  }
}

df<- 2*(2-1)-0
p.value1<-pchisq(chisq1, df=df, lower.tail=FALSE)

df
chisq1
p.value1



