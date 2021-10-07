*Questions: failure variable, adding id - difference no of subjects/records, psick==9999

clear
use "C:\Users\Ilse\Dropbox\Alles\Alles Dropbox\TI\2.1\Applied Microeconometrics\Assignment_3\Assignment\FlowSpells.dta", clear
drop if sptype==1
egen id=group(schoolid teachid)
order id schoolid teachid dest rcensor

stset splength, failure(dest==1)
stdes

g female=0
replace female=1 if gender==2
g age = styr-birthyr
g agesq = age^2
g femage = female*age

breakkk

*Listing of the survivor function:
sts list
*Plot survivor: the plot shows the coefficients given by list command. We add net lost to number to beg total (numerator).
sts graph, survival
sts graph, survival tmax(14)
sts graph, survival tmax(365)
*Plot hazard:
sts graph, hazard
sts graph, hazard width(1)
sts graph, hazard width(1) tmax(365)
sts graph, hazard tmax(14) width(1)
sts graph, hazard tmax(365) width(1)
sts graph, hazard tmax(365) by(gender) //2=female
sts graph, hazard tmax(365) width(1 1) by(gender) //2=female
sts graph, hazard by(year) width(1 1 1 1 1)
sts graph, hazard by(marstat)
sts graph, hazard by(catholic) width(1 1) tmax(365)
sts graph, hazard by(public) width(1 1) tmax(365)
sts graph, hazard by(contract) width(1 1) tmax(365)

sts test gender
sts test year
sts test marstat
sts test catholic
sts test public, detail
sts test contract

*Q2:
g female=0
replace female=1 if gender==2
g age = styr-birthyr
g agesq = age^2
g femage = female*age

streg, distribution(weibull) cl(schoolid) nohr
streg female, distribution(weibull) cl(schoolid) nohr
streg female age, distribution(weibull) cl(schoolid) nohr
streg female age femage, distribution(weibull) cl(schoolid) nohr
streg female age agesq, distribution(weibull) cl(schoolid) nohr
streg female age femage agesq, distribution(weibull) cl(schoolid) nohr

streg, distribution(exponential) cl(schoolid) nohr
streg female, distribution(exponential) cl(schoolid) nohr
streg female age, distribution(exponential) cl(schoolid) nohr
streg female age femage, distribution(exponential) cl(schoolid) nohr
streg female age agesq, distribution(exponential) cl(schoolid) nohr
streg female age femage agesq, distribution(exponential) cl(schoolid) nohr

streg age agesq female femage marstat contract lowgroup classize, distribution(weibull) cl(schoolid) nohr
stcurve, hazard
sts graph, hazard by(female) width(1 1)

* add teachnr, urban, maybe drop femage and agesq

drop if female==0

drop if female==1

*Q3:
stset splength, failure(dest==1) id(id)
stsplit sickdur, at(2 3 7 10 100)

gen dur0=0
replace dur0=1 if sickdur==0
gen dur1=0
replace dur1 = 1 if sickdur==2
gen dur2=0
replace dur2 = 1 if sickdur==3
gen dur3=0
replace dur3 = 1 if sickdur==7
gen dur4=0
replace dur4 = 1 if sickdur==10
gen dur5=0
replace dur5 = 1 if sickdur==100

streg dur0 dur1 dur2 dur3 dur4 dur5 age agesq female femage marstat contract lowgroup classize, distribution(exponential) cl(schoolid) nohr nocon
sts graph, hazard width(1) tmax(15)

*now for many splits in the data:
stset splength, failure(dest==1) id(id)
stsplit sickdur, at(2 3 7 10 110 120 130 140 150 160 170 180 190 200)

gen dur0=0
replace dur0=1 if sickdur==0
gen dur1=0
replace dur1 = 1 if sickdur==2
gen dur2=0
replace dur2 = 1 if sickdur==3
gen dur3=0
replace dur3 = 1 if sickdur==7
gen dur4=0
replace dur4 = 1 if sickdur==10
gen dur5=0
replace dur5 = 1 if sickdur==110
gen dur6=0
replace dur6 = 1 if sickdur==120
gen dur7=0
replace dur7 = 1 if sickdur==130
gen dur8=0
replace dur8 = 1 if sickdur==140
gen dur9=0
replace dur9 = 1 if sickdur==150
gen dur10=0
replace dur10 = 1 if sickdur==160
gen dur11=0
replace dur11 = 1 if sickdur==170
gen dur12=0
replace dur12 = 1 if sickdur==180
gen dur13=0
replace dur13 = 1 if sickdur==190
gen dur14=0
replace dur14 = 1 if sickdur==200

streg dur0 dur1 dur2 dur3 dur4 dur5 dur6 dur7 dur8 dur9 dur10 dur11 dur12 dur13 dur14 ///
age agesq female femage marstat lowgroup classize, ///
distribution(exponential) cl(schoolid) nohr nocon

*THE graph
clear
set obs 200
gen coeff=0
gen time=_n
replace coeff=1 if time<=10
replace coeff=2 if time>10 & time<=20
replace coeff=3 if time>20 & time<=40
replace coeff=4 if time>40 & time<=60
replace coeff=5 if time>60
twoway connect coeff time
br
gen coeff2=0
replace coeff2=2 if time<=10
replace coeff2=3 if time>10 & time<=30
replace coeff2=4 if time>30 & time<=40
replace coeff2=5 if time>40 & time<=80
replace coeff2=4 if time>80 & time<=90
replace coeff2=6 if time>90 & time<=120
replace coeff2=3 if time>120 & time<=160
replace coeff2=4 if time>160

twoway connect coeff2 time
twoway connect coeff coeff2 time

*Q4:
stcox age agesq female femage marstat contract lowgroup classize, cl(schoolid) basehc(haz1) nohr
stcurve, hazard

*Q5: Sindri?
streg, distribution(weibull) frailty(gamma) cl(schoolid) nohr
streg female, distribution(weibull) frailty(gamma) cl(schoolid) nohr
streg age agesq female femage marstat contract lowgroup classize, distribution(weibull) frailty(gamma) cl(schoolid) nohr








