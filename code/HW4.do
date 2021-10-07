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
*sts graph, hazard tmax(365) width(3 3 3) by(contract)
*tab contract
* Contract does not seem useful due to few observations for temps and 50/50
sts graph, hazard tmax(365) width(3 3) by(protest)
sts graph, hazard tmax(365) width(3 3) by(public)
sts graph, hazard tmax(365) width(3 3 3 3 3) by(year)
sts graph, hazard tmax(365) width(3 3 3 3) by(marstat)

* We test for significant differences in the hazard rates for those subgroups:
sts test gender
* Men seem to be sick for longer than women.
sts test protest
* Protestant schools have teachers that are sick for longer.
sts test catholic
* Teachers at catholic schools are sick for shorter periods.
sts test public
sts test year
* 1989 had longer sickspells than expected.
sts test marstat
*sts test contract
sts test hours
* Full time workers are sick for longer
sts test merged
sts test urban
* Rural teachers are sick for shorter periods.
sts test lowgroup
* Teachers for higher classes are sick for longer.

g season = 0
replace season = 1 if stmonth == 12 | stmonth <= 2
replace season = 2 if stmonth == 3 | stmonth ==4 | stmonth == 5
replace season = 3 if stmonth == 6 | stmonth == 7 | stmonth == 8
sts test season

g flu = 0
replace flu = 1 if stmonth == 12 | stmonth <= 3
sts test flu

* Significant differences for gender, protest, catholic, year and urban.

* Q2: Parametric models - Exponential and Weibull 
g age = styr - birthyr
g age2 = age^2

tab age
sts test age
* Interesting, younger teachers have more sick days than expected, while older teachers have fewer.

streg age,  distribution(weibull) cl(schoolid) nohr

streg age age2, distribution(weibull) cl(schoolid) nohr
stcurve, hazard
* Why does including age2 kill the effect? there is going to be collinearity between the two unless we center.
* Let's try to first center "age" (make mean=0) before squaring it.
summarize age, meanonly
g agec = age-r(mean)
g agec2 = agec^2

streg agec agec2, distribution(weibull) cl(schoolid) nohr
stcurve, hazard

*Let's do the same for a gender*age interaction term
summarize gender, meanonly
g genderc = gender-r(mean)
g femage = genderc*agec

* The two Weibull regressions:
streg agec,  distribution(weibull) cl(schoolid) nohr

streg agec agec2 genderc femage protest lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr

* Two exponential regression:
streg agec,  distribution(exponential) cl(schoolid) nohr

streg agec agec2 genderc femage protest lowgroup classize urban hours avgten marstat, distribution(exponential) cl(schoolid) nohr

* Separate Weibull for males and females:
frame put if gender == 1, into(males)
frame males: streg agec,  distribution(weibull) cl(schoolid) nohr

frame males: streg agec agec2 protest lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr

frame put if gender == 2, into(females)
frame females: streg agec,  distribution(weibull) cl(schoolid) nohr

frame females: streg agec agec2 protest lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr
* Do the same for other subgroups, base it on the sts tests above?
* Age:
frame put if agec >= 0, into(older)
frame older: streg genderc,  distribution(weibull) cl(schoolid) nohr

frame older: streg genderc femage protest lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr

frame put if agec < 0, into(younger)
frame younger: streg genderc,  distribution(weibull) cl(schoolid) nohr

frame younger: streg genderc femage protest lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr

* Flu:
frame put if flu == 1, into(Flu)
frame Flu: streg agec,  distribution(weibull) cl(schoolid) nohr

frame Flu: streg agec agec2 genderc femage protest lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr

frame put if flu == 0, into(NoFlu)
frame NoFlu: streg agec,  distribution(weibull) cl(schoolid) nohr

frame NoFlu: streg agec agec2 genderc femage protest lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr

* Protestant:
frame put if protest == 1, into(Protestant)
frame Protestant: streg agec,  distribution(weibull) cl(schoolid) nohr

frame Protestant: streg agec agec2 genderc femage lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr

frame put if protest == 0, into(NotProtestant)
frame NotProtestant: streg agec,  distribution(weibull) cl(schoolid) nohr

frame NotProtestant: streg agec agec2 genderc femage lowgroup classize urban hours avgten marstat, distribution(weibull) cl(schoolid) nohr

* Urban:
frame put if urban <= 2, into(Rural)
frame Rural: streg agec,  distribution(weibull) cl(schoolid) nohr

frame Rural: streg agec agec2 genderc femage lowgroup classize protest hours avgten marstat, distribution(weibull) cl(schoolid) nohr

frame put if urban > 2, into(City)
frame City: streg agec,  distribution(weibull) cl(schoolid) nohr

frame City: streg agec agec2 genderc femage lowgroup classize protest hours avgten marstat, distribution(weibull) cl(schoolid) nohr

*Q3: Parametric models – Piece Wise constant
* Few steps
stset splength, failure(dest==1) id(id)
stsplit sickdur, at(2 3 7 8)

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

streg agec agec2 genderc femage protest lowgroup classize urban hours avgten marstat dur0 dur1 dur2 dur3 dur4, distribution(exponential) cl(schoolid) nohr noconstant
* Here, not only gender and age but also hours and urban significant
* Many steps
stset splength, failure(dest==1) id(id)
stsplit Manystep, at(2 3 4 5 7 8 10 13 14 15 21 30 50 60 90 120 200)
br
gen step0=0
replace step0 = 1 if Manystep==0
gen step1=0
replace step1 = 1 if Manystep==2
gen step2=0
replace step2= 1 if Manystep==3
gen step3=0
replace step3 = 1 if Manystep==4
gen step4=0
replace step4 = 1 if Manystep==5
gen step5=0
replace step5 = 1 if Manystep==7
gen step6=0
replace step6 = 1 if Manystep==8
gen step7=0
replace step7= 1 if Manystep==10
gen step8=0
replace step8 = 1 if Manystep==13
gen step9=0
replace step9 = 1 if Manystep==14
gen step10=0
replace step10 = 1 if Manystep==15
gen step11=0
replace step11 = 1 if Manystep==21
gen step12=0
replace step12 = 1 if Manystep==30
gen step13=0
replace step13 = 1 if Manystep==50
gen step14=0
replace step14 = 1 if Manystep==60
gen step15=0
replace step15 = 1 if Manystep==90
gen step16=0
replace step16 = 1 if Manystep==120
gen step17=0
replace step17 = 1 if Manystep==200

streg agec agec2 genderc femage protest lowgroup classize urban hours avgten marstat step0 step1 step2 step3 step4 step5 step6 step7 step8 step9 step10 step11 step12 step13 step14 step15 step16 step17, distribution(exponential) cl(schoolid) nohr noconstant

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