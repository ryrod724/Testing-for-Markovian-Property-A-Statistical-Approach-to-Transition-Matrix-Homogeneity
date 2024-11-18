library(tidyverse)
library(Hmisc)
library(gtools)

clean.string <- read_file("GRCh38Data.txt")

##clean.string.df<- data.frame(clean.string)

#splitting the string into characters
x2<- strsplit(clean.string, "")

#shifting the text by one place
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")

#making string splits into matrices
x1Matrix=matrix(unlist(x1),ncol=1,byrow=TRUE)
x2Matrix=matrix(unlist(x2),ncol=1,byrow=TRUE)

a<-c("A")
t<-c("T")
g<-c("G")
c<-c("C")
 
total<-2189
p<-0.2 #proportion taken for matrix
upper<-round(total*p, digits=0)

#setting all variables to be 0 before adding
A1<-0
A2<-0
T1<-0
T2<-0
G1<-0
G2<-0
C1<-0
C2<-0
AA1<-0
AT1<-0
AG1<-0
AC1<-0
TA1<-0
TT1<-0
TG1<-0
TC1<-0
GA1<-0
GT1<-0
GG1<-0
GC1<-0
CA1<-0
CT1<-0
CG1<-0
CC1<-0
AA2<-0
AT2<-0
AG2<-0
AC2<-0
TA2<-0
TT2<-0
TG2<-0
TC2<-0
GA2<-0
GT2<-0
GG2<-0
GC2<-0
CA2<-0
CT2<-0
CG2<-0
CC2<-0

for (counter in 1:upper){
A1<-ifelse(x2Matrix[[counter]] %in% a,A1+1,A1)
T1<-ifelse(x2Matrix[[counter]] %in% t,T1+1,T1)
G1<-ifelse(x2Matrix[[counter]] %in% g,G1+1,G1)
C1<-ifelse(x2Matrix[[counter]] %in% c,C1+1,C1)
AA1<-ifelse(x1Matrix[[counter]] %in% a & x2Matrix[[counter]] %in% a,AA1+1,AA1)
AT1<-ifelse(x1Matrix[[counter]] %in% a & x2Matrix[[counter]] %in% t,AT1+1,AT1)
AG1<-ifelse(x1Matrix[[counter]] %in% a & x2Matrix[[counter]] %in% g,AG1+1,AG1)
AC1<-ifelse(x1Matrix[[counter]] %in% a & x2Matrix[[counter]] %in% c,AC1+1,AC1)
TA1<-ifelse(x1Matrix[[counter]] %in% t & x2Matrix[[counter]] %in% a,TA1+1,TA1)
TT1<-ifelse(x1Matrix[[counter]] %in% t & x2Matrix[[counter]] %in% t,TT1+1,TT1)
TG1<-ifelse(x1Matrix[[counter]] %in% t & x2Matrix[[counter]] %in% g,TG1+1,TG1)
TC1<-ifelse(x1Matrix[[counter]] %in% t & x2Matrix[[counter]] %in% c,TC1+1,TC1)
GA1<-ifelse(x1Matrix[[counter]] %in% g & x2Matrix[[counter]] %in% a,GA1+1,GA1)
GT1<-ifelse(x1Matrix[[counter]] %in% g & x2Matrix[[counter]] %in% t,GT1+1,GT1)
GG1<-ifelse(x1Matrix[[counter]] %in% g & x2Matrix[[counter]] %in% g,GG1+1,GG1)
GC1<-ifelse(x1Matrix[[counter]] %in% g & x2Matrix[[counter]] %in% c,GC1+1,GC1)
CA1<-ifelse(x1Matrix[[counter]] %in% c & x2Matrix[[counter]] %in% a,CA1+1,CA1)
CT1<-ifelse(x1Matrix[[counter]] %in% c & x2Matrix[[counter]] %in% t,CT1+1,CT1)
CG1<-ifelse(x1Matrix[[counter]] %in% c & x2Matrix[[counter]] %in% g,CG1+1,CG1)
CC1<-ifelse(x1Matrix[[counter]] %in% c & x2Matrix[[counter]] %in% c,CC1+1,CC1)
}

for (counter in upper+1:total){
A2<-ifelse(x2Matrix[[counter]] %in% a,A2+1,A2)
T2<-ifelse(x2Matrix[[counter]] %in% t,T2+1,T2)
G2<-ifelse(x2Matrix[[counter]] %in% g,G2+1,G2)
C2<-ifelse(x2Matrix[[counter]] %in% c,C2+1,C2)
AA2<-ifelse(x1Matrix[[counter]] %in% a & x2Matrix[[counter]] %in% a,AA2+1,AA2)
AT2<-ifelse(x1Matrix[[counter]] %in% a & x2Matrix[[counter]] %in% t,AT2+1,AT2)
AG2<-ifelse(x1Matrix[[counter]] %in% a & x2Matrix[[counter]] %in% g,AG2+1,AG2)
AC2<-ifelse(x1Matrix[[counter]] %in% a & x2Matrix[[counter]] %in% c,AC2+1,AC2)
TA2<-ifelse(x1Matrix[[counter]] %in% t & x2Matrix[[counter]] %in% a,TA2+1,TA2)
TT2<-ifelse(x1Matrix[[counter]] %in% t & x2Matrix[[counter]] %in% t,TT2+1,TT2)
TG2<-ifelse(x1Matrix[[counter]] %in% t & x2Matrix[[counter]] %in% g,TG2+1,TG2)
TC2<-ifelse(x1Matrix[[counter]] %in% t & x2Matrix[[counter]] %in% c,TC2+1,TC2)
GA2<-ifelse(x1Matrix[[counter]] %in% g & x2Matrix[[counter]] %in% a,GA2+1,GA2)
GT2<-ifelse(x1Matrix[[counter]] %in% g & x2Matrix[[counter]] %in% t,GT2+1,GT2)
GG2<-ifelse(x1Matrix[[counter]] %in% g & x2Matrix[[counter]] %in% g,GG2+1,GG2)
GC2<-ifelse(x1Matrix[[counter]] %in% g & x2Matrix[[counter]] %in% c,GC2+1,GC2)
CA2<-ifelse(x1Matrix[[counter]] %in% c & x2Matrix[[counter]] %in% a,CA2+1,CA2)
CT2<-ifelse(x1Matrix[[counter]] %in% c & x2Matrix[[counter]] %in% t,CT2+1,CT2)
CG2<-ifelse(x1Matrix[[counter]] %in% c & x2Matrix[[counter]] %in% g,CG2+1,CG2)
CC2<-ifelse(x1Matrix[[counter]] %in% c & x2Matrix[[counter]] %in% c,CC2+1,CC2)
}

observed<-matrix(c(sum(AA2), sum(AT2), sum(AG2), sum(AC2),sum(TA2),sum(TT2),sum(TG2),sum(TC2),sum(GA2),sum(GT2),sum(GG2),sum(GC2),sum(CA2),sum(CT2),sum(CG2),sum(CC2)), nrow=4, ncol=4, byrow=TRUE)
print(observed)

##Note, ex: vc denotes vowel first, then constant
eAA<-sum(AA1)*sum(A2)/sum(A1)
eAT<-sum(AT1)*sum(A2)/sum(A1)
eAG<-sum(AG1)*sum(A2)/sum(A1)
eAC<-sum(AC1)*sum(A2)/sum(A1)
eTA<-sum(TA1)*sum(T2)/sum(T1)
eTT<-sum(TT1)*sum(T2)/sum(T1)
eTG<-sum(TG1)*sum(T2)/sum(T1)
eTC<-sum(TC1)*sum(T2)/sum(T1)
eGA<-sum(GA1)*sum(G2)/sum(G1)
eGT<-sum(GT1)*sum(G2)/sum(G1)
eGG<-sum(GG1)*sum(G2)/sum(G1)
eGC<-sum(GC1)*sum(G2)/sum(G1)
eCA<-sum(CA1)*sum(C2)/sum(C1)
eCT<-sum(CT1)*sum(C2)/sum(C1)
eCG<-sum(CG1)*sum(C2)/sum(C1)
eCC<-sum(CC1)*sum(C2)/sum(C1)

#specifying the transition probability matrix
expected<- matrix(c(eAA, eAT, eAG, eAC,eTA,eTT,eTG,eTC,eGA,eGT,eGG,eGC,eCA,eCT,eCG,eCC), nrow=4, ncol=4, byrow=TRUE)
print(expected)


chisq1<- 0

for(i in 1:4) {
  for (j in 1:4) {
    if (expected[i,j] > 0) {
      valueadded<- (observed[i,j]-expected[i,j])^2/expected[i,j]
      print(valueadded)
      chisq1<- chisq1 + valueadded
    }
  }
}

df<- 4*(4-1)-0
p.value1<-pchisq(chisq1, df=df, lower.tail=FALSE)

df
chisq1
p.value1



