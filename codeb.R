load("midas.R")
setwd("C:\\Users\\cabrcvi5212d02\\Documents\\Javier\\Orthopedic Surgery\\other\\fracture\\compeeting risk")
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
load("../MiData/mdx1.R")
load("../MiData/mdx2to9.R")

dim(mid)


#%%%
sum(mdx1)

fextr = function(rango=c(25000,25090),lista=NULL,excl=NULL,m=mdx1 ) {
m = as.numeric(m1<-as.character(m))
m = m* 10^( 5-nchar(m1))
m[is.na(m)]=99999
for(i in excl) m[m==i]=99999
if(is.null(lista)) 1*(m>=rango[1] & m <=rango[2]) 
else {
 m1=rep(0,length(m))
 for(i in lista)m1[m==i]=1 ;m1}
}
  
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


# DX2-DX9
fextr2 = function(rango=c(25000,25090),lista=NULL,excl=NULL,m=mdx2to9) {
  m = as.numeric(m1<-as.character(m))
  m = m* 10^( 5-nchar(m1))
  m[is.na(m)]=99999
  for(i in excl) m[m==i]=99999
  if(is.null(lista)) 1*(m>=rango[1] & m <=rango[2]) 
  else {
    m1=rep(0,length(m))
    for(i in lista)m1[m==i]=1 ;m1}
}
m1<-as.character(m)

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
> 
