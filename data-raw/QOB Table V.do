***********************************
**** QOB Table V
**** 
**** Yuqiao Huang
**** Date: May 5th 2008
***********************************
clear

log using "C:\Documents and Settings\yhuang7\Desktop\2008 Spring Angrist\QOB_V.log", replace 

set mem 500m
use "C:\Documents and Settings\yhuang7\Desktop\2008 Spring Angrist\QOB_stata_data.dta"
rename v1 AGE
rename v2 AGEQ
rename v4 EDUC
rename v5 ENOCENT
rename v6 ESOCENT
rename v9 LWKLYWGE
rename v10 MARRIED
rename v11 MIDATL
rename v12 MT
rename v13 NEWENG
rename v16 CENSUS
rename v18 QOB
rename v19 RACE
rename v20 SMSA
rename v21 SOATL
rename v24 WNOCENT
rename v25 WSOCENT
rename v27 YOB
drop v8
gen COHORT=20.29
replace COHORT=30.39 if YOB<=39 & YOB >=30
replace COHORT=40.49 if YOB<=49 & YOB >=40
replace AGEQ=AGEQ-1900 if CENSUS==80
gen AGEQSQ= AGEQ*AGEQ

** Generate YOB dummies **********
gen YR20=0  
replace YR20=1 if YOB==1920  
replace YR20=1 if YOB==30 
replace YR20=1 if YOB==40 
gen YR21=0  
replace YR21=1 if YOB==1921  
replace YR21=1 if YOB==31
replace YR21=1 if YOB==41 
gen YR22=0  
replace YR22=1 if YOB==1922  
replace YR22=1 if YOB==32 
replace YR22=1 if YOB==42 
gen YR23=0  
replace YR23=1 if YOB==1923 
replace YR23=1 if YOB==33 
replace YR23=1 if YOB==43 
gen YR24=0  
replace YR24=1 if YOB==1924  
replace YR24=1 if YOB==34 
replace YR24=1 if YOB==44 
gen YR25=0  
replace YR25=1 if YOB==1925  
replace YR25=1 if YOB==35 
replace YR25=1 if YOB==45 
gen YR26=0  
replace YR26=1 if YOB==1926  
replace YR26=1 if YOB==36 
replace YR26=1 if YOB==46 
gen YR27=0  
replace YR27=1 if YOB==1927  
replace YR27=1 if YOB==37 
replace YR27=1 if YOB==47 
gen YR28=0  
replace YR28=1 if YOB==1928  
replace YR28=1 if YOB==38 
replace YR28=1 if YOB==48 
gen YR29=0  
replace YR29=1 if YOB==1929  
replace YR29=1 if YOB==39 
replace YR29=1 if YOB==49 

** Generate QOB dummies ***********
gen QTR1=0
replace QTR1=1 if QOB==1
gen QTR2=0
replace QTR2=1 if QOB==2
gen QTR3=0
replace QTR3=1 if QOB==3
gen QTR4=0
replace QTR4=1 if QOB==4

** Generate YOB*QOB dummies ********
gen QTR120= QTR1*YR20
gen QTR121= QTR1*YR21
gen QTR122= QTR1*YR22
gen QTR123= QTR1*YR23
gen QTR124= QTR1*YR24
gen QTR125= QTR1*YR25
gen QTR126= QTR1*YR26
gen QTR127= QTR1*YR27
gen QTR128= QTR1*YR28
gen QTR129= QTR1*YR29

gen QTR220= QTR2*YR20
gen QTR221= QTR2*YR21
gen QTR222= QTR2*YR22
gen QTR223= QTR2*YR23
gen QTR224= QTR2*YR24
gen QTR225= QTR2*YR25
gen QTR226= QTR2*YR26
gen QTR227= QTR2*YR27
gen QTR228= QTR2*YR28
gen QTR229= QTR2*YR29

gen QTR320= QTR3*YR20
gen QTR321= QTR3*YR21
gen QTR322= QTR3*YR22
gen QTR323= QTR3*YR23
gen QTR324= QTR3*YR24
gen QTR325= QTR3*YR25
gen QTR326= QTR3*YR26
gen QTR327= QTR3*YR27
gen QTR328= QTR3*YR28
gen QTR329= QTR3*YR29

keep if COHORT>30.00 & COHORT <30.40


** Col 1 3 5 7 ***
reg  LWKLYWGE EDUC  YR20-YR28 
reg  LWKLYWGE EDUC  YR20-YR28 AGEQ AGEQSQ 
reg  LWKLYWGE EDUC  RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT YR20-YR28  
reg  LWKLYWGE EDUC  RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT YR20-YR28 AGEQ AGEQSQ 


** Col 2 4 6 8 ***
ivregress 2sls LWKLYWGE YR20-YR28 (EDUC = QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28)
ivregress 2sls LWKLYWGE YR20-YR28 AGEQ AGEQSQ (EDUC = QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28)
ivregress 2sls LWKLYWGE YR20-YR28 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT  (EDUC = QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28)
ivregress 2sls LWKLYWGE YR20-YR28 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT AGEQ AGEQSQ (EDUC = QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28)



