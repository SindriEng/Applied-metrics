*Questions: failure variable, adding id - difference no of subjects/records, psick==9999

use "C:\Users\sindr\Desktop\Tinbergen\2nd-year\Block 1\Applied-Microeconometrics\Applied-metrics\data\FlowSpells.dta", clear
egen id=group(schoolid teachid)
order schoolid teachid id sptype dest rcensor

* Q1: Sickness absence at the individual level: Descriptives
drop if sptype==1
drop psick

* Tell Stata that we have duration data
stset splength, failure(dest==1)
stdes
* Note that the no. of subjects=the number of sickness spells, i.e., 1 id can be counted multiple times.
* Furthermore, we see that the mean is not representative of the population due to some large outliers.

* Now we do a simple listing of the survivor function:
sts list

bbreak

* This allows us to plot the survivor function for two weeks and the first year:
sts graph, survival tmax(14)
sts graph, survival tmax(365)


* and the hazard rate, where we also try different smoothing and compare different subgroups:
sts graph, hazard tmax(14) width(3)
sts graph, hazard tmax(14) width(1)
sts graph, hazard tmax(365) width(1)
sts graph, hazard tmax(365) width(3)
sts graph, hazard tmax(365) width(7)
sts graph, hazard tmax(365) width(3 3) by(gender)
sts graph, hazard tmax(365) width(3 3 3) by(contract)
tab contract
* Contract does not seem useful due to few observations for temps and 50/50
sts graph, hazard tmax(365) width(3 3) by(protest)
sts graph, hazard tmax(365) width(3 3) by(public)
sts graph, hazard tmax(365) width(3 3 3 3 3) by(year)
sts graph, hazard tmax(365) width(3 3 3 3) by(marstat)

* We test for significant differences in the hazard rates for those subgroups:
sts test gender
sts test protest
sts test public
sts test year
sts test marstat
sts test contract
sts test urban
g season = 0
replace season = 1 if stmonth == 12 | stmonth <= 2
replace season = 2 if stmonth == 3 | stmonth ==4 | stmonth == 5
replace season = 3 if stmonth == 6 | stmonth == 7 | stmonth == 8
sts test season

* Significant differences for gender, urban, protest and year

* Q2: Parametric models - Exponential and Weibull 
g age = styr - birthyr
g agesq = age^2
g female = 0
replace female = 1 if gender==2
g femage = female*age

streg age, distribution(weibull) cl(schoolid) nohr
stcurve, hazard


streg age gender protest lowgroup teachnr urban,distribution(weibull) cl(schoolid) nohr

drop if gender==1
drop if gender==2

*Q3: Parametric models – Piece Wise constant
stset splength, failure(dest==1) id(id)
stsplit sickdur, at(2 3 7 8 10)

gen dur0=0
replace dur0 = 1 if sickdur==0
gen dur1=0
replace dur1 = 1 if sickdur==2
gen dur2=0
replace dur2= 1 if sickdur==3
gen dur3=0
replace dur3 = 1 if sickdur==7
gen dur4=0
replace dur4 = 1 if sickdur==8
gen dur5=0
replace dur5 = 1 if sickdur==10
br

streg gender age urban dur0 dur1 dur2 dur3 dur4 dur5,distribution(exponential) cl(schoolid) nohr noconstant

g bhaz = 0
replace bhaz = exp(-.7057141) if dur0==1
replace bhaz = exp(-.9758476) if dur1==1
replace bhaz = exp(-2.414727) if dur2==1
replace bhaz = exp(-3.953066) if dur3==1
replace bhaz = exp(-4.619006) if dur4==1
replace bhaz = exp(-4.870597) if dur5==1
br
graph, bhz tmax(100)
scatter bhaz splength

*Q4:


*Q5:

*Q6: 