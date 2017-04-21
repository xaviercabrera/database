load("../MiData/mid.R")
#setwd("C:\\Users\\cabrcvi5212d02\\Documents\\Javier\\Orthopedic Surgery\\other\\fracture\\compeeting risk")
setwd("Users/jc/Documents/Kostis/mid")
mdead = m[,1:4]
mid = m[,5:8]
mdemo = m[,9:16]
mdx1 = m[,17]
mdx2to9 = m[,18:25]
mpro= m[,26:33]
summary(m[,48])
save(mdead,file="mdead.R")
save(mid,file="mid.R")
save(mdemo,file="mdemo.R")
save(mdx1,file="mdx1.R")
save(mdx2to9,file="mdx2to9.R")
save(mpro,file="mpro.R")

load("../MiData/mid.R")
load("../MiData/midas.R")
load("../MiData/mdemo.R")
load("../MiData/mdx1.R")
load("../MiData/mdx2to9.R")

dim(mid)


#%%%
sum(mdx1)
m0<-as.character(mdx1)
n0<-nchar(m0)
m0 = as.numeric(m0)
m0[is.na(m0)]=99999
m0 = m0* 10^( 5-n0)

fextr = function(rango=NULL,lista=NULL,excl=NULL,m=m0 ) {
#m = as.numeric(m1<-as.character(m))
#m = m* 10^( 5-nchar(m1))
#m[is.na(m)]=99999
for(i in excl) m[m==i]=99999
if(!is.null(rango)) m1=1*(m>=rango[1] & m <=rango[2]) 
else  m1=m*0
if(!is.null(lista)) for(i in lista)m1[m==i]=1 
m1}

  
## Diabetes 250-250.90
diab = fextr(rango=c(25000,25090))
## HTN ( 401.00-405.99), Excludes 402.01, 402.11, 402.91
htn = fextr(rango=c(40100,40599),excl=c(40201, 40211, 40291)) 
#chronic obstructive pulmonary disease (490.00-496.99)
copd = fextr(rango=c(49000,49699))
#chronic liver disease (571.00-571.99), 
cliver = fextr(rango=c(57100,57199))
#chronic renal disease (580.00-589.99), 
crenal = fextr(rango=c(58000,58999))
# anemia (280.00-281.30, 281.90, 282.00, 283.00-285.99), 
anemia = fextr(rango=c(28000,28130),lista=c(28190,28200))+fextr(rango=c(283.00,285.99))
# cerebrovascular disease : (430-438.99, 362.3,) Excludes stroke
cerebro =  fextr(rango=c(43000,43899), lista=36230) 
# stroke: (430, 431, 432.9, 434.01, 434.11, 434.91, 438, 997.02)
stroke = fextr(lista=c(43000, 43100, 43290,43401,43411,43491,43800,99702))
# cancer fextr(rango=c(14000,20900)) + fextr(rango=c(23500,23900)) 
cancer= fextr(rango=c(14000,20900)) + fextr(rango=c( 23500,23900))
# cardiac dysrhythmias (427.00) Excludes  AF & AF flutter 
cdysrhyth= fextr(lista=42700)
# conduction disorders : left bundle branch block (426.20), other left bundle branch block (426.30),  
# right bundle branch block (426.40), other and unspecified bundle branch block (426.50), 
# other heart block (426.6),  or other specified and unspecified conduction disorders (426.90), 
# complete atrio-ventricular block (426.00), unspecified atrio-ventricular block (426.10), 
# Mobitz Type II atrio-ventricular block (426.12), 
conductdis = fextr(lista=c(42620,42630,42640,42650,42660,42690,42600,42610,42612))
#Acute MI:  (site of infarction, location and Q-wave vs. non-Q-wave MI), Transmural (Anterior ICD9 410.0, 410.1; 
#Inferior ICD9 410.2, 410.4; Lateral ICD9 410.3, 410.5; Posterior ICD9410.6), Subendocardial (ICD9 410.7), 
#Other/unspecified (410.8, 410.9). 
ami = fextr(lista=c(41000, 41010, 41020, 41040,41030, 41050,41060,41070,41080, 41090))
#CHD : 411-414 Excludes acute MI 
chd = fextr(rango=c(41100,41400))
#rheumatic heart disease (393, 394, 395, 396, 397, 398), 
rheumatic = fextr(lista=c(39300, 39400, 39500, 39600, 39700, 39800))
#cardiomyopathy (425), 
cardiomyopathy= fextr(lista=42500)
#endocarditis (421), 
endocar =fextr(lista=42100)
#obesity (278), 
obese = fextr(lista=27800)
#valve disease (424.0 -424.2),
valve = fextr(rango=c(42400 ,42420))
#Sleep Apnea (327.23)
slapnea = fextr(lista=32723)
#Hyperthyroidism (242)
hyperthyro = fextr(lista=24200)
#Hypothyroidism (243, 244)
hypothyro= fextr(lista=c(24300, 24400))
#Thyroiditis (245)
thyro= fextr(lista=24500)
# AFIV
afiv= fextr(lista=c(42731))
#CHF 4280:4284,42820,42823:42830,42833,42840:42843
chf = fextr(rango=c(4280,4284)) +fextr(rango=c(42820,42823)) +
  fextr(rango=c(42830,42833)) +fextr(rango=c(42840,42843)) 
#
a = ls()
a = a[-c(16,20:22)]
save(list=a,file="../MiData/comorbin.R")

# DX2-DX9
m1<-sapply(mdx2to9,as.character)
n1<-nchar(m1)
m1 = as.numeric(m1)
dim(m1) = dim(n1)
m1[n1==0] = 99999
m1[is.na(m1)]=99999
n1[n1==0] = 5
m1 = m1* 10^( 5-n1)

# DX2-DX9
fextr2 = function(rango=NULL,lista=NULL,excl=NULL,m=m1) {
#  m = as.numeric(m1<-as.character(m))
#  m = m* 10^( 5-nchar(m1))
#  m[is.na(m)]=99999
  for(i in excl) m[m==i]=99999
  if(!is.null(rango)) m1 = 1*(m>=rango[1] & m <=rango[2]) 
  else m1=m*0
  if(!is.null(lista)) for(i in lista)m1[m==i]=1 ; 
#  apply(m1,1,sum,na.rm=T)
  c(m1%*%rep(1,8))}

## Diabetes 250-250.90
diab2 = fextr2(rango=c(25000,25090))
## HTN ( 401.00-405.99), Excludes 402.01, 402.11, 402.91
htn2 = fextr2(rango=c(40100,40599),excl=c(40201, 40211, 40291)) 
#chronic  pulmonary disease (490.00-496.99)
copd2 = fextr2(rango=c(49000,49699))
#chronic liver disease (571.00-571.99), 
cliver2 = fextr2(rango=c(57100,57199))
#chronic renal disease (580.00-589.99), 
crenal2 = fextr2(rango=c(58000,58999))
# anemia (280.00-281.30, 281.90, 282.00, 283.00-285.99), 
anemia2 = fextr2(rango=c(28000,28130),lista=c(28190,28200))+fextr2(rango=c(283.00,285.99))
# cerebrovascular disease : (430-438.99, 362.3,) Excludes stroke
cerebro2 =  fextr2(rango=c(43000,43899), lista=36230) 
# stroke: (430, 431, 432.9, 434.01, 434.11, 434.91, 438, 997.02)
stroke2 = fextr2(lista=c(43000, 43100, 43290,43401,43411,43491,43800,99702))
# cancer fextr(rango=c(14000,20900)) + fextr(rango=c(23500,23900)) 
cancer2= fextr2(rango=c(14000,20900)) + fextr2(rango=c( 23500,23900))
# cardiac dysrhythmias (427.00) Excludes  AF & AF flutter 
cdysrhyth2= fextr2(lista=42700)
# conduction disorders : left bundle branch block (426.20), other left bundle branch block (426.30),  
# right bundle branch block (426.40), other and unspecified bundle branch block (426.50), 
# other heart block (426.6),  or other specified and unspecified conduction disorders (426.90), 
# complete atrio-ventricular block (426.00), unspecified atrio-ventricular block (426.10), 
# Mobitz Type II atrio-ventricular block (426.12), 
conductdis2 = fextr2(lista=c(42620,42630,42640,42650,42660,42690,42600,42610,42612))
#Acute MI:  (site of infarction, location and Q-wave vs. non-Q-wave MI), Transmural (Anterior ICD9 410.0, 410.1; 
#Inferior ICD9 410.2, 410.4; Lateral ICD9 410.3, 410.5; Posterior ICD9410.6), Subendocardial (ICD9 410.7), 
#Other/unspecified (410.8, 410.9). 
ami2 = fextr2(lista=c(41000, 41010, 41020, 41040,41030, 41050,41060,41070,41080, 41090))
#CHD : 411-414 Excludes acute MI 
chd2 = fextr(rango=c(41100,41400))
#rheumatic heart disease (393, 394, 395, 396, 397, 398), 
rheumatic2 = fextr2(lista=c(39300, 39400, 39500, 39600, 39700, 39800))
#cardiomyopathy (425), 
cardiomyopathy2= fextr2(lista=42500)
#endocarditis (421), 
endocar2 =fextr2(lista=42100)
#obesity (278), 
obese2 = fextr2(lista=27800)
#valve disease (424.0 -424.2),
valve2 = fextr2(rango=c(42400 ,42420))
#Sleep Apnea (327.23)
slapnea2 = fextr2(lista=32723)
#Hyperthyroidism (242)
hyperthyro2 = fextr2(lista=24200)
#Hypothyroidism (243, 244)
hypothyro2= fextr2(lista=c(24300, 24400))
#Thyroiditis (245)
thyro2= fextr2(lista=24500)
# AFIV
afiv2= fextr2(lista=c(42731))
#CHF 4280:4284,42820,42823:42830,42833,42840:42843
chf2 = fextr2(rango=c(4280,4284)) +fextr2(rango=c(42820,42823)) +
  fextr2(rango=c(42830,42833)) +fextr2(rango=c(42840,42843)) 

a = ls()
a = a[-c(1,20:22)]
save(list=a,file="../MiData/comor2bin.R")
load(file="../MiData/comor2bin.R")
### first time
num=1:nrow(mid)
id = as.numeric(mid[,1])
admdate= mid[,2]
max(admdate)
i = sort.list(id*10^5+admdate)
#####
#####
sex = mdemo$SEX
race = mdemo$RACE

j = afiv[i]==1
jump = c(1,1*(diff(id[i][j]) > 0))
afivtot= cbind(mid[i,][j,][jump==1,],as.numeric(sex[i][j][jump==1]),
               as.numeric(race[i][j][jump==1]), id[i][j][jump==1])
jj = chf[i]>=1
jjump = c(1,1*(diff(id[i][jj]) > 0))
chftot= cbind(mid[i,][jj,][jjump==1,],as.numeric(sex[i][jj][jjump==1]),
               as.numeric(race[i][jj][jjump==1]), id[i][jj][jjump==1])
dim(chftot)
dim(afivtot)
save(chftot,afivtot,file="afiv_chf.R")

tot=merge(chftot,afivtot,by=names(chftot)[1])
load(file="afiv_chf.R")

plot(1960+tot[,2]/365,1960+tot[,7]/365,pch=".",cex=2,xlab="HF",ylab="AFIV")
abline(v=2007+9/12,col=2)
CHF= 1960+tot[,2]/365
hist(CHF,50,col=7,xlab="CHF",main="CHF")
AFIV= 1960+tot[,7]/365
hist(AFIV,50,col=7,xlab="AFIV",main="AFIV")
i = CHF > 2008.75 & AFIV> 2008.75 
plot(density(CHF-AFIV),main="HF - AFIV" )
plot(density(CHF[i]-AFIV[i]),main="HF - AFIV after09/2007" )

     > names(m)
 [1] "LOCATION"   "CAUSE"      "NEWDTD"     "DeathRNUM"  "Patient_ID"
 [6] "ADMDAT"     "DSCHDAT"    "patbdte"    "HOSP"       "ZIP"       
[11] "STATUS"     "SEX"        "RACE"       "SOURCE"     "TOTBIL"    
[16] "DRG"        "DX1"        "DX2"        "DX3"        "DX4"       
[21] "DX5"        "DX6"        "DX7"        "DX8"        "DX9"       
[26] "PROC1"      "PROC2"      "PROC3"      "PROC4"      "PROC5"     
[31] "PROC6"      "PROC7"      "PROC8"      "HISPAN"     "RECDID"    
[36] "DIV"        "DSHYR"      "PRDTE1"     "PRDTE2"     "PRDTE3"    
[41] "PRDTE4"     "PRDTE5"     "PRDTE6"     "PRDTE7"     "PRDTE8"    
[46] "SECOND"     "THIRD"      "PRIME"     
power.anova.test(groups=4,n=NULL,between.var=0.05^2, within.var=0.18^2,power=.8,sig.level=0.05)
power.anova.test(groups=4,n=NULL,between.var=0.5^2, within.var=1.5^2,power=.8,sig.level=0.05)
power.t.test(n=NULL,delta=1, sd=1.5,power=.8,sig.level=0.05/12,type="one.sample", alternative="one.sided")

 
j = afiv2[i]==1
jump = c(1,1*(diff(id[i][j]) > 0))
afivtot2= cbind(mid[i,][j,][jump==1,],id[i][j][jump==1]);
,as.numeric(sex[i][j][jump==1]),
               as.numeric(race[i][j][jump==1]), id[i][j][jump==1])
jj = chf2[i]>=1
jjump = c(1,1*(diff(id[i][jj]) > 0))
chftot2= cbind(mid[i,][jj,][jjump==1,],#as.numeric(sex[i][jj][jjump==1]),
              #as.numeric(race[i][jj][jjump==1]), 
        id[i][jj][jjump==1])
 
tot2=merge(chftot2,afivtot2,by=names(chftot2)[1])
plot(1960+tot2[,2]/365,1960+tot2[,6]/365,pch=".",cex=2,xlab="HF",ylab="AFIV")
plot(density(tot2[,2]-tot2[,6]),main="HF - AFIV" )

