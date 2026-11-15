capture log close
log using _results/mocc06_data, replace text

//  Program:    mocc06_data
//  Task:       Prepare data for analysis
//  Project:    Motherhood Over Cohorts and Cognition
//  Author:     Wolfe 08/20/24, 09/23/2024, 04/15/2026, 9/10/2026

//  #0
//  Program Setup

version 19.5
clear all
macro drop _all
set linesize 80
set scheme s1mono
set more off


//  #1
//  Age at kid's Birth

use _RawData/randhrsfamk1992_2022v1.dta, clear

merge m:1 hhidpn using _RawData/randhrs1992_2022v1.dta, ///	
	keepusing(rabyear raevbrn ragender rabmonth) nogen

*kid's relationship to R
tab karel, nolabel m
tab karel, m

gen     kid = karel == 1  // biological kid of respondent]
lab var kid "Biological kid =1, else 0"
tab     kid, m
tab 	karel kid, m

gen 	step  = karel == 2 // step
lab var step "Step kid =1, else 0"
tab 	step, m
tab 	karel step, m

egen 	anykid = rowtotal(kid step), missing	
lab var anykid "Any kid =1, else 0"
tab 	anykid, m
tab 	karel anykid, m

***Age at first birth
*Continuous age at each kid's birth with adjustments because the HRS
*gives the respondent's birth month and year (no day) and each kid's birth year only
*Midpoint dating (mid-month/mid-year) avoids systematic over/under age at first births
* - Respondent's birth date is set to mid-month or to mid-year when missing rabmonth
* - Each kid's birth date is set to mid-year
gen double rbirth = rabyear + (rabmonth - 0.5)/12 if !mi(rabmonth) // respondent mid-month
replace    rbirth = rabyear + 0.5                 if  mi(rabmonth) // fallback: mid-year
gen        rbfall = mi(rabmonth)

tab kabyearbg, m // best-guess birth year of each kid

*Create age at each kid's birth (bage) 
gen double agebirth = (kabyearbg + 0.5) - rbirth if kid

*Create flags for exceptionally early or late births (not necessarily 1st birth)
gen bearly = agebirth < 13 if !mi(agebirth) // flag exceptionally early birth
gen blate  = agebirth > 50 if !mi(agebirth) // flag exceptionally late birth

recode     agebirth  min/5 = . // Ages <= 5 completely implausible (negative ages to 5)
lab var    agebirth "Age at Biokids Births"
sum        agebirth

***Collapse data to household level
collapse (sum) kidn=kid stepn=step ///
		 (min) agebirth raevbrn    ///
		 (max) anykid bearly blate ///
		 , by(hhidpn)
 
tab1 bearly blate, m
tab  bearly blate, m

*Merge with main data
merge 1:1 hhidpn using _RawData/randhrs1992_2022v1.dta, nogen

sum hhidpn

save _results/mocc06_datakids.dta, replace


//  #2
//  Upload HRS Data & Merge with Cognition Data

use _RawData/cogimp9222a_r.dta, clear

*Convert strings to numeric variables
destring hhid, gen(hhid_num)
destring pn  , gen(pn_num)

gen double hhidpn = (hhid_num * 1000) + pn_num
format     hhidpn %09.0f
sum        hhidpn

***Merge with Main Rand Data File & Cognitive Health
merge 1:1 hhidpn using _results/mocc06_datakids.dta, nogen

sum hhidpn

lab language mocc, new

lab var hhidpn   "Respondent ID"
lab var raestrat "Std Error Stratum"
lab var rawtsamp "Sampling Weight"
lab var raehsamp "Stratum Half-Sample Code Sampling Weight"
lab var racohbyr "Cohort based on birth yr"
lab var raevbrn  "Number of children Ever Born"

lab var anykid   "Any bio/step kid =1"		
lab var kidn     "#Bio kids"
lab var stepn    "#Step kids"
lab var agebirth "Age at 1st Birth"
lab var bearly   "Reported a birth before 13" // for sensitivity analysis
lab var blate    "Reported a birth after 50"  // for sensitivity analysis

*create real numbers from string household id
destring hhid, replace
lab var  hhid "Household ID"
order    hhidpn pn hhid

*** identify base-year weight
*** appropriate for survival analysis -- see HRS Staff. Sampling Weights:
*** Revised for Tracker 2.0 & Beyond. Ann Arbor, MI: Survey Research Center,
*** Institute for Social Research, University of Michigan; 2019.
gen     rwgtTI = .
lab var rwgtTI "Survey Weights (Time Invariant)"

forval w = 1/16 {
	clonevar rwgt`w' = r`w'wtresp
	replace  rwgtTI  = r`w'wtresp if mi(rwgtTI) & r`w'wtresp > 0 & !mi(r`w'wtresp)
	lab var  rwgt`w' "Survey Weights"
}

***In Survey?
* 0.Inap. 
* 1.Resp,alive 
* 4.NR,alive 
* 5.NR,died this wv 
* 6.NR,died prev wv 
* 7.NR,dropped from samp
forval w = 1/16 {
	gen insurv`w' = r`w'iwstat == 1
	tab r`w'iwstat insurv`w', m

	lab var insurv`w' "In Survey =1"
}


//	#2
//	kid Biographies & Sociodemographics 

***Parity
gen      parityC = kidn if kidn > 0 & !mi(kidn)  // bio children
recode   parityC . = 0  if raevbrn == 0          // reported none
recode   parityC 0=3 1=0 2/3=1 4/max=2 if !mi(parityC)
lab var  parityC "#BioKid Categories"
lab def  parityC 3 "None(3)" 2 "4+(2)" 1 "2-3(1)" 0 "One(0)", replace
lab val  parityC parityC
tab      parityC        , m
tab      raevbrn parityC, m

***Age of 1st Birth Categories
gen     agebirthC  = .
replace agebirthC  = 0 if agebirth <  19                     
replace agebirthC  = 1 if agebirth >= 19 & agebirth <23    
replace agebirthC  = 2 if agebirth >= 23 & agebirth < .    
recode  agebirthC .= 3 if parityC  ==  3
lab var agebirthC "Age 1st Birth (Categories)"
lab def agebirthC 0 "<=18(0)" 1 "19-22(1)" 2 "23+(2)" 3 "None(3)", replace
lab val agebirthC agebirthC
bysort  agebirthC: sum agebirth

recode bearly blate (.=0) if parityC  ==  3 // early/late births .=0 if no child

***Year of Birth
gen 	byear = rabyear
lab var byear "Year of R's Birth"
recode  byear .m =.
tab 	byear, m

***HRS Cohort
tab1    hacohort, m
lab var hacohort "HRS Cohorts"
lab def hacohort        ///
		0 "0.Overlap"   ///
		1 "1.Ahead"     ///
		2 "2.Coda"      ///
		3 "3.HRS"       ///
		4 "4.WarBaby"   ///
		5 "5.EarlyBoom" ///
		6 "6.MidBoom"   ///
		7 "7.LateBoom"  ///
		8 "8.GenX" , replace
lab val hacohort hacohort
tab     hacohort, m

***Cohort based on Zheng JGSS 2020
gen     cohort = racohbyr - 1 // center on 0 (making -1 missing, was 0)
lab var cohort "Birth Cohort"
recode  cohort -1=.
lab def cohort 0 "1890/1923(0)" /// Greatest Generation
               1 "1924/1930(1)" /// Early children of Depression
               2 "1931/1941(2)" /// Late children of Depression
               3 "1942/1947(3)" /// War Babies
               4 "1948/1953(4)" /// Early-Baby Boomers
               5 "1954/1959(5)" /// Mid-Baby Boomers
			   6 "1960/1965(6)" /// Late-Baby Boomers
			   7 "1966/1971(7)" /// Generation X
			   , replace
lab val cohort cohort
tab byear cohort

***Age by wave
forvalues w = 1/16 {
	*age at typical interview date
	egen    idate`w'  = median(r`w'iwend), by(hacohort)
	egen    idateA`w' = median(r`w'iwend)
	replace idate`w'  = idateA`w' if mi(idate`w')
	lab var idate`w' "Typical Interview Date"
	format  idate`w' %td

	gen     ageyI`w' = floor((idate`w' - rabdate)/365.2425) // age at typical interview date

	*Age at end of wave
	gen     agey`w'    = r`w'agey_e
	replace agey`w'    = ageyI`w' if mi(agey`w') // if missing age use ageyI
	lab var agey`w'    "Age by Year (No Missing)"
	sum     agey`w'
}
drop ageyI*

***Age of Entry, Last Observed Age
gen     ageyE = .
lab var ageyE "Age of Entry"

gen     ageyL = .
lab var ageyL "Age Last Observed"

forvalues w = 1/16 {
	replace ageyE = r`w'agey_e if !mi(r`w'agey_e) & mi(ageyE)
	replace ageyL = r`w'agey_e if !mi(r`w'agey_e)
}
tab1 ageyE ageyL, m

***Age at Death
gen     agedead  = radage_y
lab var agedead "Age at Death"
tab     agedead, m

gen 	dead = agedead
recode  dead missing = 0 nonmissing = 1 
lab var dead "Deceased = 1, else 0"
tab     dead

***Death by wave
*Find the wave in which each respondent died, using two sources:
* dwave5 = first wave with rWiwstat == 5 (nonresponse, died this wave)
* dwaveA = first wave where age exceeds age at death (used only as a backup)
gen dwave5 = .
gen dwaveA = .

forvalues w = 1/16 {
	replace dwave5 = `w' if r`w'iwstat == 5 & mi(dwave5)
	replace dwaveA = `w' if dead == 1 & agedead < agey`w' & !mi(agey`w') & mi(dwaveA)
}

*Use iwstat when available; otherwise use the age-based wave
gen     dwave = dwave5
replace dwave = dwaveA if mi(dwave) & dead == 1
lab var dwave "Wave of death"

*Checks
count if dead == 1 & mi(dwave)   // decedents with no death wave (want 0)
tab dwave5 if dead == 0          // iwstat says died, but no age at death

*Death indicator for each wave: 0 before death, 1 in the death wave, missing after
forvalues w = 1/16 {
	gen     death`w' = 0
	replace death`w' = 1 if `w' == dwave
	replace death`w' = . if `w' > dwave & !mi(dwave)
	lab var death`w' "death =1, else 0"
	tab    death`w', m
}

*NOTE: radage_y (age at death) is missing for some respondents whose interview
*status is "died this wave" (rWiwstat == 5), so dead = 0 for them. Those
*respondents have a death wave (dwave) and death# = 1 in that wave. Reset dead
*so it agrees with death#. agedead stays missing for them.
replace dead = 1 if !mi(dwave)

***Approximate age at death
*Age at the death wave minus 1 (midpoint of the 2-year interval)
gen agedeadA = .
forvalues w = 1/16 {
	replace agedeadA = agey`w' - 1 if dwave == `w'
}
*Test the rule where the reported age at death is known
gen deadgap = agedead - agedeadA
sum deadgap

*Final variable: reported age when available, approximation otherwise
gen     agedeadF = agedead
replace agedeadF = agedeadA if mi(agedead) & !mi(agedeadA)
lab var agedeadF "Age at Death (approx. if missing)"

gen     agedeadI = mi(agedead) & !mi(agedeadA)
lab var agedeadI "Age at death approximated = 1"
tab     agedeadI, m

***Gender
gen     female=1 if ragender==2
replace female=0 if ragender==1
lab var female "Female =1, male =0"
tab     female ragender, m

***Race/ethnicity
gen     race = raracem - 1
lab var race "Race/ethnicity"
recode  race 2=3
replace race = 2 if rahispan==1
lab def race 0 "White_0" 1 "Black_1" 2 "Hisp_2" 3 "Other_3", replace
lab val race race
tab     race raracem , m
tab     race rahispan, m

***Marital status
lab def mstat 0 "Mar_0" 1 "SepDiv_1" 2 "Wid_2" 3 "Nev_3" , replace

forvalues w = 1/16 {
	gen     mstat`w' = r`w'mstat
	recode  mstat`w' 1/3=0 4/6=1 7=2 8=3 
	lab var mstat`w' "Marital Status"
	lab val mstat`w' mstat

	tab r`w'mstat mstat`w' , m
}

***Foreign born
gen     fborn = 0 if rabplace< .
replace fborn = 1 if rabplace==11
lab var fborn "Foreign born =1, else 0"
tab     rabplace fborn


//  #3
//	Cognition 

forvalues w = 3/16 {
	gen     cogscore`w' = R`w'COG27 
	lab var cogscore`w' "Cognition Score (0-27)"	
}


//  #4
//  Education, Wealth, Income, & Occupation

*Respondent
gen 	reduc = raedyrs
recode  reduc (.a/.z = .)
lab var reduc "Respondent Education"
tab     reduc, m

*Max Mother & Father Education
egen 	peduc = rowmax(rameduc rafeduc)
lab var peduc "Parents' Max Education"
sum  	peduc

*Categorical Education
gen     reducC = reduc  
lab def reducC 0 "<=12(0)" 1 "13-15(1)" 2 "16+(2)", replace
lab val reducC reducC
recode  reducC (min/12=0) (13/15=1) (16/max=2) 
lab var reducC "Ed Categories"

***Occupation label for recoded variable
lab def occ 0 "White(0)" 1 "Skilled(1)" 2 "Manual(2)" 3 "None(3)", replace

forvalues w = 1/16 {
	***Net Worth & Components
	clonevar wealth`w'  = h`w'atotb
	lab var  wealth`w'  "Total Net Worth"

	egen 	 hwealth`w' = rowtotal(h`w'ahous h`w'ahoub), missing
	lab var  hwealth`w' "Total Housing Wealth"

	egen     hdebt`w'   = rowtotal(h`w'amrtb h`w'amort h`w'ahmln), missing
	lab var  hdebt`w'   "Total Housing debt"

	egen     savings`w' = rowtotal(	h`w'aira  h`w'astck h`w'abond ///
									h`w'achck h`w'acd   h`w'aothr ), missing
	lab var  savings`w' "Total Savings/Investments/Retirement"

	clonevar udebt`w'  = h`w'adebt
	lab var  udebt`w'  "Total Unsecured debt"

	clonevar income`w' = h`w'itot
	lab var  income`w' "Total Income"

	*Occupational categories below taken from Montez and Zajacova (2013)
	*white-collar (executive, admin, managerial or professional specialty),
	*skilled (tech and related support, sales, admin support, protective
	*services, or the military), and manual (private household, service,
	*precision production, operators, transportation, or handlers and laborers)

	*HRS occupational codes in rWjlocc:
		*01.Managerial specialty ope
		*02.Prof specialty opr/tech
		*03.Sales
		*04.Clerical/admin supp
		*05.Svc:prv hhld/clean/bldg
		*06.Svc:protection
		*07.Svc:food prep
		*08.Health svc
		*09.Personal svc
		*10.Farming/forestry/fishing
		*11.Mechanics/repair
		*12.Constr trade/extractors
		*13.Precision production
		*14.Operators: machine
		*15.Operators: transport, et
		*16.Operators: handlers, etc
		*17.Member of Armed Forces

	clonevar occ`w' = r`w'jlocc
	lab var  occ`w' "Occupation of Longest Job"
	recode   occ`w' (.a/.z = .) (1/2 = 0) (3 4 6 17 = 1) (5 7/16 = 2)

	*Not in labor force = 3 in rWlbrfh
		*.a = 65+,presumed retired 
		*.q = Not asked,Lt65,not working 
		*.t = Worked last 2 yr,now not 
		*1.Works FT
		*2.Works PT
		*3.Unemployed
		*4.Partly retired
		*5.Retired
		*6.Disabled
		*7.Not in Lbr
	recode   occ`w' . = 3 if inlist(r`w'lbrfh, 3, 5, 6, 7, .a, .q, .t) // Not working/OLF
	lab val  occ`w' occ

	tab r`w'jlocc occ`w', m
}

sum wealth* hwealth* hdebt* savings* udebt* income* occ*

save _results/mocc06_data.dta, replace


//  #5
//  Reshape & Save Data

use _results/mocc06_data.dta, replace

keep 	hhidpn hhid byear cohort hacohort racohbyr agey*   ///
        insurv* raehsamp raestrat rwgtTI rwgt* raevbrn     ///	
		cogscore* death* dead agedeadF agedeadI            /// 
		parityC agebirth agebirthC bearly blate kid* step* ///
		female race mstat* reduc reducC peduc udebt*       ///
		income*  occ* wealth* hwealth* hdebt* savings* 

label language temp, new copy

***Reshape data
reshape long  ///
		agey  cogscore insurv death ///
		mstat wealth hwealth hdebt savings ///
		udebt income occ rwgt, i(hhidpn) j(wave)

label language mocc

foreach v in female race reducC cohort hacohort occ mstat {
	lab val `v' `v'
}

***Time
gen     year = wave
lab var year "Year of HRS Survey"
lab var wave "HRS Survey Wave"

local w = 1
forvalues yr = 1992(2)2022 {
	replace year=`yr' if wave==`w'
	local ++w
}

***Age top coded
gen      ageT = agey
recode   ageT 95/max = 95
lab var  ageT "Age (top coded at 95)"

***Order and Save
order 	hhidpn hhid hacohort raehsamp raestrat rwgt rwgtTI ///
		insurv wave wave year agebirthC parityC

notes: "mocc06.dta ; HRS Data"

save _results/mocc06.dta, replace

capture log close
exit
