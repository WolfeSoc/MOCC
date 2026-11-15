capture log close
log using _results/mocc06_models, replace text

//  Program:   mocc06_models
//  Task:      Descriptives
//  Project:   Motherhood Over Cohorts and Cognition
//  Author:    Wolfe 08/20/24, 09/23/2024, 04/15/2026, 9/10/2026


//  #0
//  Program Setup + Prepare Data

version 19.5
clear all
macro drop _all
set linesize 80
set scheme s1mono
set more off

adopath ++ "_commands" 

***Set locals for models 
local dep  cogSTD                // dependent variable
local est  reg `dep'  
local cdb  agebirthC parityC     // childbearing bios
local cnt  race mstat agey wave  // controls
local lpo  lc(black) mc(black) lc(black) lw(thin)
local opt  "[pweight=rwgt],  vce(cluster hhidpn)"

*Open Data
use   rwgt raehsamp raestrat rwgtTI hhidpn hhid    ///
      racohbyr hacohort insurv cohort wave wave    ///
      year ageyE ageyL agey agey race female mstat ///
      parityC agebirthC bearly blate cogscore      ///     
      using _results/mocc06.dta, clear

misstable sum 

sort hhidpn wave

***Select Sample for Analysis & Grab Missing Information 
statsto, clear

sort hhidpn wave
sum hhidpn if hhidpn[_n]!=hhidpn[_n-1]  // no missing dropped 
local n0 = r(N)
statsto, u(Start=`n0' ) row(RawN) list

tab cohort if hhidpn[_n]!=hhidpn[_n-1] , m

***Baseline Samp
qui drop if  agey < 50  | mi(rwgtTI) | rwgt==0 | year<1996 // baseline sample
sum hhidpn if hhidpn[_n]!=hhidpn[_n-1] 
local n1 = r(N)
local dn = `n0' - `n1'
local pn = (100*`dn')/`n0'
statsto, u(Start=`n0' Keep=`n1' Drop=`dn' PDrop=`pn') row(Baseline) 

***Drop extra cohorts/no cohort
sort hhidpn wave
drop       if cohort==6 | cohort==7 | mi(cohort) 
sum hhidpn if hhidpn[_n]!=hhidpn[_n-1] 
local n2  = r(N)
local dn1 = `n1' - `n2'
local pn1 = (100*`dn1')/`n1'

statsto, u(Start=`n1' Keep=`n2' Drop=`dn1' PDrop=`pn1') row(CohortDrop) 

preserve 
  drop if insurv == 0

  sum hhidpn if hhidpn[_n]!=hhidpn[_n-1] 
  local n3  = r(N)
  local dn2 = `n2' - `n3'
  local pn2 = (100*`dn2')/`n2'

  statsto, u(Start=`n2' Keep=`n3' Drop=`dn2' PDrop=`pn2') row(NoSurv) 

  mark    insamp
  markout insamp _all
  drop if insamp == 0 
  drop    insamp

  sum hhidpn if hhidpn[_n]!=hhidpn[_n-1] 
  local n4  = r(N)
  local dn3 = `n3' - `n4'
  local pn3 = (100*`dn3')/`n3'

  statsto, u(Start=`n3' Keep=`n4' Drop=`dn3' PDrop=`pn3') row(Missing) 
restore

***Missing by variable
statsto mvar , clear

foreach v in cogscore `cdb' `cnt' {
  sort hhidpn wave 

  preserve 
    qui drop if insurv == 0 
    qui drop if mi(`v')

    qui sum  hhidpn if hhidpn[_n]!=hhidpn[_n-1] 
    local  n5 = r(N)
    local dn4 = `n3' - `n5'
    local pn4 = (100*`dn4')/`n3'

    statsto mvar, u(Start=`n4' Keep=`n5' Drop=`dn4' PDrop=`pn4') row(`v') 
  restore
}

***Unique missing by variable
statsto uvar , clear

local vlist "cogscore `cdb' `cnt' "

foreach v in cogscore `cdb' `cnt' {
  sort hhidpn wave 

  local mdrop : subinstr local vlist "`v'" "", all word

  preserve 
    qui drop if insurv == 0 
    qui sum hhidpn if hhidpn[_n]!=hhidpn[_n-1] 
    local n3  = r(N)

    foreach m of local mdrop {
      qui drop if mi(`m')
    }
    qui sum  hhidpn if hhidpn[_n]!=hhidpn[_n-1] 
    local n4 = r(N)

    qui drop if mi(`v')

    qui sum hhidpn if hhidpn[_n]!=hhidpn[_n-1] 
    local  n5 = r(N)
    local dn4 = `n4' - `n5'
    local pn4 = (100*(`dn4'))/(`n3'-`n5')

    statsto uvar, u(Start=`n4' Keep=`n5' Drop=`dn4' PDrop=`pn4') row(`v') 
  restore
}

statsto     , list title(Sample drops)
statsto mvar, list title(Missing By Variable)
statsto uvar, list title(Unieque Missing By Variable)

***DROP ALL MISSING
mark    insamp
markout insamp _all
tab     insamp
drop if insamp == 0
drop    insamp


***Set longitudinal data
xtset hhidpn wave 

***Set survey data
**Code for setting weights is in chapt 8 of:
**https://hrsonline.isr.umich.edu/sitedocs/dmgt/IntroUserGuide.pdf
svyset raehsamp [pweight=rwgt], strata(raestrat) vce(linearized) singleunit(centered)

**Sample size info 
sort hhidpn
sum  hhidpn 
sum  hhidpn if hhidpn[_n]!=hhidpn[_n-1]
sum  hhidpn if female==1 & hhidpn[_n]!=hhidpn[_n-1] // female 
sum  hhidpn if female==0 & hhidpn[_n]!=hhidpn[_n-1] // male  

tab agey cohort

**Standardize by cohort and gender 
gen cogSTD = .

forvalues c = 0/5 {
  **Women
  qui sum cogscore [aweight=rwgt] if cohort==`c' & female==1
  local cmean = r(mean)
  local csd   = r(sd)
  qui gen double cogSTDw = (cogscore - `cmean')/`csd' if cohort==`c' & female==1

  **Men
  qui sum cogscore [aweight=rwgt] if cohort==`c' & female==0
  local cmean = r(mean)
  local csd   = r(sd)
  qui gen double cogSTDm = (cogscore - `cmean')/`csd' if cohort==`c' & female==0

  qui replace cogSTD = cogSTDw if cohort==`c' & female==1
  qui replace cogSTD = cogSTDm if cohort==`c' & female==0
 
  qui drop cogSTDw cogSTDm
}

forvalues c = 0/5 {
  sum cogSTD [aweight=rwgt] if cohort==`c' & female==1
  sum cogSTD [aweight=rwgt] if cohort==`c' & female==0
}

sort hhidpn

***Look at frequency of categories by cohort and gender
*Men
forvalues c = 0/5 {
  tab agebirthC if female==0 & hhidpn[_n]!=hhidpn[_n-1] & cohort==`c'
}

*Women
forvalues c = 0/5 {
  di "Women"
  tab agebirthC if female==1 & hhidpn[_n]!=hhidpn[_n-1] & cohort==`c'
}


//  #1
//  Descriptive Info (Tables 1 and 2)

tab  agey cohort

bysort wave cohort: sum  agey 

***Reviewer wants Table 1 with STD cognition
forvalues c = 0/5 { 
  qui sum cogscore [aweight=rwgt] if cohort==`c' 
  local cmean = r(mean)
  local csd   = r(sd)
  qui gen double cogSTDc`c' = (cogscore - `cmean')/`csd' if cohort==`c' 
}

qui egen cogSTDc = rowmax(cogSTDc*)

sum cogSTDc

statsto cog1 , clear

forvalues c = 5(-1)0 {
  if `c'==0 local aRange "75(5)95"
  if `c'==1 local aRange "70(5)90"
  if `c'==2 local aRange "55(5)85"
  if `c'==3 local aRange "50(5)80"
  if `c'==4 local aRange "50(5)70"
  if `c'==5 local aRange "50(5)65"

  forvalues a = `aRange' {
    di _newline
    di "cohort==`c' : Range = `aRange'" 
    di _newline

    qui svy: mean cogSTDc if cohort==`c' & agey==`a'
    local mn = (el( r(table),1,1))

    di "Mean = `mn' | Cohort==`c'| Age==`a'"
    statsto cogSTD , u(cog`c'=`mn') row(Age`a')
  }
}

forvalues c = 5(-1)0 {
  qui svy: mean cogSTDc if cohort==`c'
  local mn = (el( r(table),1,1))
  statsto cogSTD , u(cog`c'=`mn') row(Total)

  sort hhidpn

  qui sum hhidpn if hhidpn[_n]!=hhidpn[_n-1] & cohort==`c'
  statsto cogSTD , r(cog`c'=N) row(N) list 

  qui sum hhidpn if cohort==`c'
  statsto cogSTD , r(cog`c'=N) row(Observations) list
}

forvalues a = 50(5)95 {
  qui svy: mean cogSTDc if  agey==`a'
  local mn = (el( r(table),1,1))
  statsto cogSTD , u(Total=`mn') row(Age`a')
}

sum hhidpn if hhidpn[_n]!=hhidpn[_n-1]
statsto cogSTD , r(Total=N) row(N)

sum hhidpn
statsto cogSTD , r(Total=N) row(Observations)

svy: mean cogSTDc
local mn = (el( r(table),1,1))
statsto cogSTD , u(Total=`mn') row(Total)

statsto cogSTD , list


***By Cohort & Wave (Table 1)
forvalues c = 5(-1)0 {
  if `c'==0 local aRange "75(5)95"
  if `c'==1 local aRange "70(5)90"
  if `c'==2 local aRange "55(5)85"
  if `c'==3 local aRange "50(5)80"
  if `c'==4 local aRange "50(5)70"
  if `c'==5 local aRange "50(5)65"

  forvalues a = `aRange' {
    di _newline
    di "cohort==`c' : Range = `aRange'" 
    di _newline

    qui svy: mean cogscore if cohort==`c' &  agey==`a'
    local mn = (el( r(table),1,1))

    di "Mean = `mn' | Cohort==`c'| Age==`a'"
    statsto cog1 , u(cog`c'=`mn') row(Age`a')
  }
}

forvalues c = 5(-1)0 {
  qui svy: mean cogscore if cohort==`c'
  local mn = (el( r(table),1,1))
  statsto cog1 , u(cog`c'=`mn') row(Total)

  sort hhidpn

  qui sum hhidpn if hhidpn[_n]!=hhidpn[_n-1] & cohort==`c'
  statsto cog1 , r(cog`c'=N) row(N) list 

  qui sum hhidpn if cohort==`c'
  statsto cog1 , r(cog`c'=N) row(Observations) list
}

forvalues a = 50(5)95 {
  qui svy: mean cogscore if  agey==`a'
  local mn = (el( r(table),1,1))
  statsto cog1 , u(Total=`mn') row(Age`a')
}

sum hhidpn if hhidpn[_n]!=hhidpn[_n-1]
statsto cog1 , r(Total=N) row(N)

sum hhidpn
statsto cog1 , r(Total=N) row(Observations)

svy: mean cogscore
local mn = (el( r(table),1,1))
statsto cog1 , u(Total=`mn') row(Total)

statsto cog1 , list

******By Cohort & Gender (Table 2)
statsto cog2, clear

*** Flag each respondent's first remaining wave 
bysort hhidpn (wave): gen first = (_n==1)
tab    first, m

gen one = 1 // constant, used to count respondents with valid design information

foreach v in  i.agebirthC i.parityC i.race i.mstat c.agey {
  gettoken trash rownm : v     , parse(.)
  gettoken trash rownm : rownm , parse(.)

  forvalues c = 0/5 {
    qui tab `rownm' if first==1
    local rn = `r(r)'

    ***Women
    qui svy, subpop(if cohort==`c' & female==1 & first==1): mean `v'
    mat wtable = r(table)

    ***Men
    qui svy, subpop(if cohort==`c' & female==0 & first==1): mean `v'
    mat mtable = r(table)

    if `rn' < 10 {
      forvalues n = 1/`rn' {
        local mn = (el( wtable,1,`n'))
        statsto cog2 , u(Women=`mn') row(`rownm'`n')  coleq(Cohort`c')

        local mn = (el( mtable,1,`n'))
        statsto cog2 , u(Men=`mn')   row(`rownm'`n')  coleq(Cohort`c')
      }
    }

    if `rn' > 5 {
      local mn = (el( wtable,1,1))
      statsto cog2 , u(Women=`mn') row(`rownm') coleq(Cohort`c')

      local mn = (el( mtable,1,1))
      statsto cog2 , u(Men=`mn')   row(`rownm') coleq(Cohort`c')
    }
  }
}

***Sample size row: respondents (first==1) by cohort and gender
local Nsum = 0
forvalues c = 0/5 {
  foreach g in 1 0 {
    local gl = cond(`g'==1, "Women", "Men")

    qui count if cohort==`c' & female==`g' & first==1
    local nraw = r(N)

    qui svy, subpop(if cohort==`c' & female==`g' & first==1): mean one
    local nsvy = e(N_sub)

    statsto cog2, u(`gl'=`nraw') row(SampleSize) coleq(Cohort`c')
    local Nsum = `Nsum' + `nraw'
  }
}

statout cog1 cog2 using _results/mocc06_models-descriptives.csv, list replace dec(2)


//  #2
//  Birth Age and Parity: Test Baseline and Cohort Differences (Table 3) and Gender

`est' i.agebirthC i.cohort i.female i.(`cnt') `opt'
 estimates store modat

 forvalues c = 0/5 {
  `est' i.agebirthC i.(`cnt') i.female if cohort==`c' `opt'
  estimates store moda`c'
 }

`est' ib3.parityC i.cohort i.female i.(`cnt') `opt'
 estimates store modpt

 forvalues c = 0/5 {
  `est' ib3.parityC i.female i.(`cnt') if cohort==`c' `opt'
  estimates store modp`c'
 }

esttab modat moda0 moda1 moda2 moda3 moda4 moda5  ///
       using _results/mocc06_models-results.csv, replace se label b(2) se(2) ///
       star(* 0.05 ** 0.01 *** 0.001) ///
       keep(1.agebirthC 2.agebirthC 3.agebirthC) nonum noobs compress  nogaps ///
       mtitle("All Cohorts" "Greatest Gen" "Early Children of Depression"     ///
       "Late Children of Depression" "War Babies" "Early Baby Boomers" "Mid Baby Boomers") 

esttab modpt modp0 modp1 modp2 modp3 modp4 modp5 ///
       using _results/mocc06_models-results.csv, append se label b(2) se(2) ///
       star(* 0.05 ** 0.01 *** 0.001) ///
       keep(0.parityC 1.parityC 2.parityC) nonum noobs  compress nogaps   ///
       mtitle("All Cohorts" "Greatest Gen" "Early Children of Depression" ///
       "Late Children of Depression" "War Babies" "Early Baby Boomers" "Mid Baby Boomers") 

***Formally Test Gender & Cohort Differences using interaction

***Test Gender Differences (not included in table)
reg cogSTD i.agebirthC##i.female i.female#i.(cohort `cnt') i.(cohort `cnt') `opt'
testparm i.agebirthC#i.female
lincom _b[1.agebirthC#1.female] - _b[2.agebirthC#1.female]   // young adult vs 23+

reg cogSTD ib3.parityC##i.female i.female#i.(cohort `cnt') i.(cohort `cnt') `opt'
testparm i.parityC#i.female


***Cohort Differences
**Age at First Birth
qui `est' i.agebirthC##i.cohort i.cohort#i.(female `cnt') i.(female `cnt') `opt'
estimates store moda_int


*Omnibus: do cohorts differ in the age-at-first-birth association? (15 df)
testparm i.agebirthC#i.cohort

**Cohort Differences with p values
*Cohort abbreviations (j minus i direction: left vs right)
local cname0 "GG"
local cname1 "ECD"
local cname2 "LCD"
local cname3 "WB"
local cname4 "EBB"
local cname5 "MBB"

*Create column names
local colnms ""
forvalues c2 = 1/5 {
  forvalues c1 = 0/`=`c2'-1' {
    local colnms "`colnms' `cname`c2''v`cname`c1'' `cname`c2''v`cname`c1''p"
  }
}

matrix A = J(3, 30, .)
matrix rownames A = EarlyAdult Adult NoChild
matrix colnames A = `colnms'

local col 0
forvalues c2 = 1/5 {
  forvalues c1 = 0/`=`c2'-1' {
    local col = `col' + 2
    local row 0
 
    forvalues cat = 1/3 {
      local row = `row' + 1
      if `c1' == 0 {
        qui lincom `cat'.agebirthC#`c2'.cohort
      }
      else {
        qui lincom `cat'.agebirthC#`c2'.cohort - `cat'.agebirthC#`c1'.cohort
      }
      matrix A[`row', `col'-1] = r(estimate)
      matrix A[`row', `col']   = r(p)
    }
  }
}

di _newline(2) "Age at First Birth: Cohort Differences and P-Values"
matrix list A, format(%7.4f)


***Parity
qui `est' ib3.parity##i.cohort i.cohort#i.(female `cnt') i.(female `cnt') `opt'
estimates store modp_int

* Omnibus: do cohorts differ in the age-at-first-birth association? (15 df)
testparm ib3.parityC#i.cohort

matrix P = J(3, 30, .)
matrix rownames P = One TwoThree FourPlus 
matrix colnames P = `colnms'

local col 0
forvalues c2 = 1/5 {
  
  forvalues c1 = 0/`=`c2'-1' {
    local col = `col' + 2
    local row 0
    
    forvalues cat = 0/2 {
      local row = `row' + 1
     
      if `c1' == 0 qui lincom `cat'.parityC#`c2'.cohort
      else         qui lincom `cat'.parityC#`c2'.cohort - `cat'.parityC#`c1'.cohort
  
      matrix P[`row', `col'-1] = r(estimate)
      matrix P[`row', `col']   = r(p)
    }
  }
}

di _newline(2) "Parity"
matrix list P, format(%7.4f)

matlist A
matlist P

***Export
putexcel set _results/mocc06_models-results-H2AgeBirth.xlsx, replace
putexcel A1 = "Age at First Birth: coefficient differences and p-values"
putexcel A2 = matrix(A), names

putexcel set _results/mocc06_models-results-H2Parity.xlsx, modify
putexcel A7 = "Parity: coefficient differences and p-values"
putexcel A8 = matrix(P), names


//  #3
//  Age at first birth: Figures for Fully Interactive 

***Fully Interactive Model
qui `est' i.cohort##i.female##i.(agebirthC `cnt') `opt'
estimates store pooledA

estimates restore pooledA
qui margins agebirthC, over(cohort female) atmeans post

matrix T = r(table)
local cn : colfullnames T

local cats "t e a n" // agebirthC = 0 1 2 3
local cols "Xaxis"

foreach s of local cats {
	foreach g in f m {
		local cols `cols' `s'`g'B `s'`g'Ub `s'`g'Lb `s'`g'p
	}
}
local ncol : word count `cols'
matrix preda = J(6, `ncol', .)
matrix colnames preda = `cols'
matrix rownames preda = Cohort0 Cohort1 Cohort2 Cohort3 Cohort4 Cohort5

forvalues c = 0/5 {
	matrix preda[`c'+1, 1] = `c' // Xaxis
}

local rb = rownumb(T, "b")
local rl = rownumb(T, "ll")
local ru = rownumb(T, "ul")
local rp = rownumb(T, "pvalue")

local j = 0
foreach nm of local cn { // loop through column names
	local ++j
	if !regexm("`nm'", "([0-9]+)[a-z]*\.agebirthC") continue
	local k = real(regexs(1))
	if !regexm("`nm'", "([0-9]+)[a-z]*\.cohort") continue
	local c = real(regexs(1))
	if !regexm("`nm'", "([0-9]+)[a-z]*\.female") continue
	local g = real(regexs(1))

	local s  : word `=`k'+1' of `cats'  // grab category info
	local gl = cond(`g'==1, "f", "m")
	local r  = `c' + 1

	matrix preda[`r', colnumb(preda, "`s'`gl'B")]  = T[`rb', `j']
	matrix preda[`r', colnumb(preda, "`s'`gl'Ub")] = T[`ru', `j']
	matrix preda[`r', colnumb(preda, "`s'`gl'Lb")] = T[`rl', `j']
	matrix preda[`r', colnumb(preda, "`s'`gl'p")]  = T[`rp', `j']
}

matlist preda, format(%6.3f)

***Variables from the matrix, column names as before
svmat double preda, names(col)

*Create xaxis for different childbearing groups
gen tXaxis = Xaxis - .15
gen eXaxis = Xaxis - .05
gen aXaxis = Xaxis + .05
gen nXaxis = Xaxis + .15

****Graph Age at first birth
graph twoway /// Women
  (rspike  tfUb tfLb  tXaxis, lw(thin) lcolor(black)) ///
  (scatter tfB        tXaxis, m(O) mfc(white) mlc(black) lc(black) lw(thin)) ///
  (rspike  efUb efLb  eXaxis, lw(thin) lcolor(black)) ///
  (scatter efB        eXaxis, m(O) mc(black) lc(black) lw(thin)) ///
  (rspike  afUb afLb  aXaxis, lw(thin) lcolor(black)) ///
  (scatter afB        aXaxis, m(S) mc(black) lc(black) lw(thin)) ///
  (rspike  nfUb nfLb  nXaxis, lw(thin) lcolor(black)) ///
  (scatter nfB        nXaxis, m(S) mfc(white) mlc(black) lc(black) lw(thin)) ///
    , ///
  plotregion(style(none) margin(0))                                   ///
  yline(0 , lp(line) lw(vthin) lc(black))                             ///
  xline(-.5 .5 1.5 2.5 3.5 4.5 5.5,                                   ///
        lp(solid) lc(black) lw(vthin) noextend)                       ///
  xscale(range(-.51 5.5) lcolor(white))                               ///
  yscale(range(-.8 .7) lcolor(white))                                 ///
  title("Women", size(medium))                                        ///
  xtitle("") xsize(6.5)                                               ///
  ytitle("Cognitive Score" , size(medsmall) margin(0 5 0 0))          ///
  ylab(-.7(.1).7, angle(horizontal) labsize(small)                    ///
       grid gmin gmax glp(solid) tlc(gs12) tlw(vvthin) glcolor(gs12)  ///
       glw(vvthin) nogextend)                                         ///
  xlab(  0 `""Greatest" "Generation""'                                ///
         1 `""Early Children" "of Depression""'                       ///
         2 `""Late Children" "of Depression""'                        ///
				 3 "War Babies"                                               ///
				 4 `""Early" "Baby Boomers""'                                 ///
				 5 `""Mid" "Baby Boomers""' ,                                 ///
         angle(horizontal) labsize(small) noticks nogrid   )          ///
    legend( size(small) order(2 6 4 8)  row(2) col(2)                 ///
            label(2 "18 and Younger")                                 ///
      			label(4 "19 to 22 years old")                             ///
      			label(6 "23 and older")                                   ///
      			label(8 "No Children")                                    ///
      			subtitle(Age at First Birth, position(12)                 ///
      			margin(0 0 1 0) size(medsmall)) bmargin(0 0 0 5)          ///
      			lcolor(white) region(fcolor(white) lcolor(white)) ) 

graph export _results/mocc06_models-Fig1f.eps, replace


graph twoway /// Men
  (rspike  tmUb tmLb tXaxis, lw(thin) lcolor(black))  ///
  (scatter tmB       tXaxis, m(O) mfc(white) mlc(black) lc(black) lw(thin)) ///
  (rspike  emUb emLb eXaxis, lw(thin) lcolor(black))  ///
  (scatter emB       eXaxis, m(O) mc(black) lc(black) lw(thin)) ///
  (rspike  amUb amLb aXaxis, lw(thin) lcolor(black))  ///
  (scatter amB       aXaxis, m(S) mc(black) lc(black) lw(thin)) ///
  (rspike  nmUb nmLb nXaxis, lw(thin) lcolor(black))  ///
  (scatter nmB       nXaxis, m(S) mfc(white) mlc(black) lc(black) lw(thin)) ///
       , ///
  plotregion(style(none) margin(0))                                   ///
  yline(0 , lp(line) lw(vthin) lc(black))                             ///
  xline(-.5 .5 1.5 2.5 3.5 4.5 5.5,                                   ///
        lp(solid) lc(black) lw(vthin) noextend)                       ///
  xscale(range(-.51 5.5) lcolor(white))                               ///
  yscale(range(-.8 .7) lcolor(white))                                 ///
  title("Men", size(medium))                                          ///
  xtitle("") xsize(6.5)                                               ///
  ytitle("Cognitive Score" , size(medsmall) margin(0 5 0 0))          ///
  ylab(-.7(.1).7, angle(horizontal) labsize(small)                    ///
       grid gmin gmax glp(solid) tlc(gs12) tlw(vvthin) glcolor(gs12)  ///
       glw(vvthin) nogextend)                                         ///
  xlab(  0 `""Greatest" "Generation""'                                ///
         1 `""Early Children" "of Depression""'                       ///
         2 `""Late Children" "of Depression""'                        ///
				 3 "War Babies"                                               ///
				 4 `""Early" "Baby Boomers""'                                 ///
				 5 `""Mid" "Baby Boomers""' ,                                 ///
         angle(horizontal) labsize(small) noticks nogrid   )          ///
    legend( size(small) order(2 6 4 8)  row(2) col(2)                 ///
            label(2 "18 and Younger")                                 ///
      			label(4 "19 to 22 years old")                             ///
      			label(6 "23 and older")                                   ///
      			label(8 "No Children")                                    ///
      			subtitle(Age at First Birth, position(12)                 ///
      			margin(0 0 1 0) size(medsmall)) bmargin(0 0 0 5)          ///
      			lcolor(white) region(fcolor(white) lcolor(white)) ) 

graph export _results/mocc06_models-Fig1m.eps, replace


//  #4
//  Parity: Figures for Fully Interactive 
             
***Fully interactive model
qui `est' i.cohort##i.female##i.(i.parityC `cnt') `opt'
estimates store pooledP

foreach c in 0 1 2 {
  lincom _b[5.cohort#1.female#2.parityC] - _b[`c'.cohort#1.female#2.parityC]
}
lincom _b[5.cohort#1.female#2.parityC] - (_b[0.cohort#1.female#2.parityC] + ///
       _b[1.cohort#1.female#2.parityC] + _b[2.cohort#1.female#2.parityC])/3

margins parityC, over(cohort female) atmeans // predictions
 
matrix T = r(table)
local cn : colfullnames T
display "Columns in r(table): " colsof(T) "  (expect 48 if no empty cells)"
display "Example names: " word("`cn'", 1) "  |  " word("`cn'", 2)

*Stubs by parityC level (0=One, 1=2-3, 2=4+, 3=None): women, then men
local pf "of ttf ff ncf"
local pm "om ttm fm ncm"

local pcols ""
foreach s in `pf' `pm' {
	local pcols `pcols' `s'B `s'Ub `s'Lb `s'p
}
local pncol : word count `pcols'
matrix predp = J(6, `pncol', .)
matrix colnames predp = `pcols'
matrix rownames predp = Cohort0 Cohort1 Cohort2 Cohort3 Cohort4 Cohort5

local rb = rownumb(T, "b")
local rl = rownumb(T, "ll")
local ru = rownumb(T, "ul")
local rp = rownumb(T, "pvalue")

local j = 0
foreach nm of local cn {
	local ++j
	if !regexm("`nm'", "([0-9]+)[a-z]*\.parityC") continue
	local k = real(regexs(1))
	if !regexm("`nm'", "([0-9]+)[a-z]*\.cohort") continue
	local c = real(regexs(1))
	if !regexm("`nm'", "([0-9]+)[a-z]*\.female") continue
	local g = real(regexs(1))

	if `g'==1 {
		local s : word `=`k'+1' of `pf'
	} 

  else {
		local s : word `=`k'+1' of `pm'
	}
	
  local r = `c' + 1

	matrix predp[`r', colnumb(predp, "`s'B")]  = T[`rb', `j']
	matrix predp[`r', colnumb(predp, "`s'Ub")] = T[`ru', `j']
	matrix predp[`r', colnumb(predp, "`s'Lb")] = T[`rl', `j']
	matrix predp[`r', colnumb(predp, "`s'p")]  = T[`rp', `j']
}

matlist predp, format(%6.3f)

***Variables from the matrix
svmat double predp, names(col)

graph twoway /// Women's Parity
  (rspike  ofUb ofLb   tXaxis, lw(thin) lcolor(black)) ///
  (scatter ofB         tXaxis, m(O) mfc(white) mlc(black) lc(black) lw(thin)) ///
  (rspike  ttfUb ttfLb eXaxis, lw(thin) lcolor(black)) ///
  (scatter ttfB        eXaxis, m(O) mc(black) lc(black) lw(thin)) ///
  (rspike  ffUb ffLb   aXaxis, lw(thin) lcolor(black)) ///
  (scatter ffB         aXaxis, m(S) mc(black) lc(black) lw(thin)) ///
  (rspike  ncfUb ncfLb nXaxis, lw(thin) lcolor(black)) ///
  (scatter ncfB        nXaxis, m(S) mfc(white) mlc(black) lc(black) lw(thin)) ///
   , ///
  plotregion(style(none) margin(0))                                   ///
  yline(0 , lp(line) lw(vthin) lc(black))                             ///
  xline(-.5 .5 1.5 2.5 3.5 4.5 5.5,                                   ///
        lp(solid) lc(black) lw(vthin) noextend)                       ///
  xscale(range(-.51 5.5) lcolor(white))                               ///
  yscale(range(-.8 .7) lcolor(white))                                 ///
  title("Women", size(medium))                                        ///
  xtitle("") xsize(6.5)                                               ///
  ytitle("Cognitive Score" , size(medsmall) margin(0 5 0 0))          ///
  ylab(-.7(.1).7, angle(horizontal) labsize(small)                    ///
       grid gmin gmax glp(solid) tlc(gs12) tlw(vvthin) glcolor(gs12)  ///
       glw(vvthin) nogextend)                                         ///
  xlab(  0 `""Greatest" "Generation""'                                ///
         1 `""Early Children" "of Depression""'                       ///
         2 `""Late Children" "of Depression""'                        ///
				 3 "War Babies"                                               ///
				 4 `""Early" "Baby Boomers""'                                 ///
				 5 `""Mid" "Baby Boomers""' ,                                 ///
         angle(horizontal) labsize(small) noticks nogrid   )          ///
    legend( size(small) order(2 6 4 8)  row(2) col(2)                 ///
            label(2 "1 Child")                                        ///
      			label(4 "2-3 Children")                                   ///
      			label(6 "4+ Children")                                    ///
      			label(8 "No Children")                                    ///
      			subtitle(Parity, position(12)                             ///
      			margin(0 0 1 0) size(medsmall)) bmargin(0 0 0 5)          ///
      			lcolor(white) region(fcolor(white) lcolor(white)) ) 

graph export _results/mocc06_models-Fig2f.eps, replace


graph twoway /// Men's Parity
  (rspike  omUb omLb   tXaxis, lw(thin) lcolor(black)) ///
  (scatter omB         tXaxis, m(O) mfc(white) mlc(black) lc(black) lw(thin)) ///
  (rspike  ttmUb ttmLb eXaxis, lw(thin) lcolor(black)) ///
  (scatter ttmB        eXaxis, m(O) mc(black) lc(black) lw(thin)) ///
  (rspike  fmUb fmLb   aXaxis, lw(thin) lcolor(black)) ///
  (scatter fmB         aXaxis, m(S) mc(black) lc(black) lw(thin)) ///
  (rspike  ncmUb ncmLb nXaxis, lw(thin) lcolor(black)) ///
  (scatter ncmB        nXaxis, m(S) mfc(white) mlc(black) lc(black) lw(thin)) ///
    , ///
  plotregion(style(none) margin(0))                                   ///
  yline(0 , lp(line) lw(vthin) lc(black))                             ///
  xline(-.5 .5 1.5 2.5 3.5 4.5 5.5,                                   ///
        lp(solid) lc(black) lw(vthin) noextend)                       ///
  xscale(range(-.51 5.5) lcolor(white))                               ///
  yscale(range(-.8 .7) lcolor(white))                                 ///
  title("Men ", size(medium))                                         ///
  xtitle("") xsize(6.5)                                               ///
  ytitle("Cognitive Score" , size(medsmall) margin(0 5 0 0))          ///
  ylab(-.7(.1).7, angle(horizontal) labsize(small)                    ///
       grid gmin gmax glp(solid) tlc(gs12) tlw(vvthin) glcolor(gs12)  ///
       glw(vvthin) nogextend)                                         ///
  xlab(  0 `""Greatest" "Generation""'                                ///
         1 `""Early Children" "of Depression""'                       ///
         2 `""Late Children" "of Depression""'                        ///
				 3 "War Babies"                                               ///
				 4 `""Early" "Baby Boomers""'                                 ///
				 5 `""Mid" "Baby Boomers""' ,                                 ///
         angle(horizontal) labsize(small) noticks nogrid   )          ///
    legend( size(small) order(2 6 4 8)  row(2) col(2)                 ///
            label(2 "1 Child")                                        ///
      			label(4 "2-3 Children")                                   ///
      			label(6 "4+ Children")                                    ///
      			label(8 "No Children")                                    ///
      			subtitle(Parity, position(12)                             ///
      			margin(0 0 1 0) size(medsmall)) bmargin(0 0 0 5)          ///
      			lcolor(white) region(fcolor(white) lcolor(white)) ) 

graph export _results/mocc06_models-Fig2m.eps, replace


//  #5
//  Age at first birth: Test Cohort, Gender, Childbearing from Interactions

*Test whether cohort variation in the association between age at first birth
*and cognition differs for women and men (H4), then locate where the
*difference comes from. The omnibus test is the formal test; Tables A and B
*and the joint tests are descriptive follow-ups to nail down why we see any sig results

*What's saved in mocc06_models-results-H4AgeBirth.xlsx:
* Sheet GenderGaps: GapTab (Cat, Cohort, Women, Men, Gap, SE, p, LB, UB).
* Sheet CohortTransitions: TransTab (Cat, From, To, dWomen, dMen, Diff, SE, p,
* LB, UB). Row labels are r1, r2, ...; use the Cat and Cohort (or From, To)
* columns to identify rows. Cat is the category code (0, 1, 3), not a count.

***omnibus test of H4
estimates restore pooledA
testparm i.agebirthC#i.cohort#i.female

*Decomposing the age-at-first-birth omnibus test of H4 (descriptive)
*Cell-specific associations (cohort x gender x category vs 23+), covariates
*fully interacted. Names are cohort#female#agebirthC, as typed here.
qui `est' ibn.cohort#ibn.female#ib2.agebirthC i.cohort##i.female##i.(`cnt') `opt'

*Table A: associations and gender gaps by cohort 
matrix GapTab = J(18,9,.)
matrix colnames GapTab = Cat Cohort Women Men Gap SE p LB UB
local r 0

foreach k in 0 1 3 {
  forvalues c = 0/5 {
    local ++r
    matrix GapTab[`r',1] = `k'
    matrix GapTab[`r',2] = `c'
    matrix GapTab[`r',3] = _b[`c'.cohort#1.female#`k'.agebirthC]
    matrix GapTab[`r',4] = _b[`c'.cohort#0.female#`k'.agebirthC]

    qui lincom `c'.cohort#1.female#`k'.agebirthC - `c'.cohort#0.female#`k'.agebirthC
    matrix GapTab[`r',5] = r(estimate)
    matrix GapTab[`r',6] = r(se)
    matrix GapTab[`r',7] = r(p)
    matrix GapTab[`r',8] = r(lb)
    matrix GapTab[`r',9] = r(ub)
  }
}
matlist GapTab, format(%7.3f) ///
  title("Table A: association vs 23+ (Cat 0 = <=18, 1 = 19-22, 3 = none)")

*Table B: adjacent-cohort changes, women vs men
matrix TransTab = J(15,10,.)
matrix colnames TransTab = Cat From To dWomen dMen Diff SE p LB UB
local r 0

foreach k in 0 1 3 {
  forvalues c = 1/5 {
    local ++r
    local prev = `c' - 1

    local W1 "`c'.cohort#1.female#`k'.agebirthC"
    local W0 "`prev'.cohort#1.female#`k'.agebirthC"
    local M1 "`c'.cohort#0.female#`k'.agebirthC"
    local M0 "`prev'.cohort#0.female#`k'.agebirthC"
    
    matrix TransTab[`r',1] = `k'
    matrix TransTab[`r',2] = `prev'
    matrix TransTab[`r',3] = `c'

    qui lincom `W1' - `W0'
    matrix TransTab[`r',4] = r(estimate)

    qui lincom `M1' - `M0'
    matrix TransTab[`r',5] = r(estimate)

    qui lincom `W1' - `W0' - `M1' + `M0'
    matrix TransTab[`r',6] = r(estimate)
    matrix TransTab[`r',7] = r(se)
    matrix TransTab[`r',8] = r(p)
    matrix TransTab[`r',9] = r(lb)
    matrix TransTab[`r',10] = r(ub)
  }
}
matlist TransTab, format(%7.3f) title("Table B: change from cohort From to To, women vs men")

*Joint tests: is the gender gap constant across cohorts? 
*Per category (5 restrictions each)
foreach k in 0 1 3 {
  local acc ""
  forvalues c = 1/5 {
    qui test _b[`c'.cohort#1.female#`k'.agebirthC] - _b[`c'.cohort#0.female#`k'.agebirthC] ///
           = _b[0.cohort#1.female#`k'.agebirthC]   - _b[0.cohort#0.female#`k'.agebirthC], `acc'
    local acc "accumulate"
  }
  di as txt "Category `k': F(" r(df) "," r(df_r) ") = " %6.2f r(F) "  p = " %6.4f r(p)
}

*overall (15 restrictions); should reproduce the testparm F 
local acc ""
foreach k in 0 1 3 {
  forvalues c = 1/5 {
    qui test _b[`c'.cohort#1.female#`k'.agebirthC] - _b[`c'.cohort#0.female#`k'.agebirthC] ///
           = _b[0.cohort#1.female#`k'.agebirthC]   - _b[0.cohort#0.female#`k'.agebirthC], `acc'
    local acc "accumulate"
  }
}
di as txt "Overall: F(" r(df) "," r(df_r) ") = " %6.2f r(F) "  p = " %6.4f r(p)

matlist GapTab
matlist TransTab

*Export  
putexcel set _results/mocc06_models-results-H4AgeBirth.xlsx, sheet("GenderGaps") replace
putexcel A1 = matrix(GapTab), names
putexcel set _results/mocc06_models-results-H4AgeBirth.xlsx, sheet("CohortTransitions") modify
putexcel A1 = matrix(TransTab), names


//  #6
//  Parity: Test Cohort, Gender, Childbearing from Interactions

***Parity: omnibus test of H4
estimates restore pooledP
testparm i.parityC#i.cohort#i.female

*Unpacking the parity omnibus test of H4 
qui `est' ibn.cohort#ibn.female#ib3.parityC i.cohort##i.female##i.(`cnt') `opt'

*Table A: associations and gender gaps by cohort 
matrix GapTabP = J(18,9,.)
matrix colnames GapTabP = Cat Cohort Women Men Gap SE p LB UB
local r 0

foreach k in 0 1 2 {
  forvalues c = 0/5 {
    local ++r
    matrix GapTabP[`r',1] = `k'
    matrix GapTabP[`r',2] = `c'
    matrix GapTabP[`r',3] = _b[`c'.cohort#1.female#`k'.parityC]
    matrix GapTabP[`r',4] = _b[`c'.cohort#0.female#`k'.parityC]

    qui lincom `c'.cohort#1.female#`k'.parityC - `c'.cohort#0.female#`k'.parityC
    matrix GapTabP[`r',5] = r(estimate)
    matrix GapTabP[`r',6] = r(se)
    matrix GapTabP[`r',7] = r(p)
    matrix GapTabP[`r',8] = r(lb)
    matrix GapTabP[`r',9] = r(ub)
  }
}
matlist GapTabP, format(%7.3f) ///
  title("Table A: association vs No children (Cat 0 = One, 1 = 2-3, 2 = 4+)")

*Table B: adjacent-cohort changes, women vs men
matrix TransTabP = J(15,10,.)
matrix colnames TransTabP = Cat From To dWomen dMen Diff SE p LB UB
local r 0

foreach k in 0 1 2 {
  forvalues c = 1/5 {
    local ++r
    local prev = `c' - 1

    local W1 "`c'.cohort#1.female#`k'.parityC"
    local W0 "`prev'.cohort#1.female#`k'.parityC"
    local M1 "`c'.cohort#0.female#`k'.parityC"
    local M0 "`prev'.cohort#0.female#`k'.parityC"
    
    matrix TransTabP[`r',1] = `k'
    matrix TransTabP[`r',2] = `prev'
    matrix TransTabP[`r',3] = `c'

    qui lincom `W1' - `W0'
    matrix TransTabP[`r',4] = r(estimate)

    qui lincom `M1' - `M0'
    matrix TransTabP[`r',5] = r(estimate)

    qui lincom `W1' - `W0' - `M1' + `M0'
    matrix TransTabP[`r',6] = r(estimate)
    matrix TransTabP[`r',7] = r(se)
    matrix TransTabP[`r',8] = r(p)
    matrix TransTabP[`r',9] = r(lb)
    matrix TransTabP[`r',10] = r(ub)
  }
}
matlist TransTabP, format(%7.3f) ///
  title("Table B: change from cohort From to To, women vs men (Parity)")

*Joint tests: is the gender gap constant across cohorts? 
*Per category (5 restrictions each)
foreach k in 0 1 2 {
  local acc ""
  forvalues c = 1/5 {
    qui test _b[`c'.cohort#1.female#`k'.parityC] - _b[`c'.cohort#0.female#`k'.parityC] ///
           = _b[0.cohort#1.female#`k'.parityC]   - _b[0.cohort#0.female#`k'.parityC], `acc'
    local acc "accumulate"
  }
  di as txt "Category `k': F(" r(df) "," r(df_r) ") = " %6.2f r(F) "  p = " %6.4f r(p)
}

*overall (15 restrictions); should reproduce the testparm F
local acc ""
foreach k in 0 1 2 {
  forvalues c = 1/5 {
    qui test _b[`c'.cohort#1.female#`k'.parityC] - _b[`c'.cohort#0.female#`k'.parityC] ///
           = _b[0.cohort#1.female#`k'.parityC]   - _b[0.cohort#0.female#`k'.parityC], `acc'
    local acc "accumulate"
  }
}
di as txt "Overall: F(" r(df) "," r(df_r) ") = " %6.2f r(F) "  p = " %6.4f r(p)

matlist GapTab
matlist TransTab

*Export 
putexcel set _results/mocc06_models-results-H4Parity.xlsx, sheet("GenderGaps") replace
putexcel A1 = matrix(GapTabP), names
putexcel set _results/mocc06_models-results-H4Parity.xlsx, sheet("CohortTransitions") modify
putexcel A1 = matrix(TransTabP), names


//  #7
//  Test Cohort Differences in Predicted Values

***Age at Birth
foreach g in 1 0 {                       // 1 = women, 0 = men
  foreach k in 0 1 2 3 {                 // age-at-first-birth category
    estimates restore pooledA            // restore each time: -post- overwrites results
    qui margins agebirthC, over(cohort female) atmeans post

    di _newline "female==`g'  agebirthC==`k'"

    * 15 pairwise cohort differences
    forvalues c2 = 1/5 {
      forvalues c1 = 0/`=`c2'-1' {
        qui lincom _b[`k'.agebirthC#`c2'.cohort#`g'.female] - ///
                   _b[`k'.agebirthC#`c1'.cohort#`g'.female]
        di "  cohort `c2' - cohort `c1': " %6.3f r(estimate) "  p=" %6.4f r(p)
      }
    }

    * joint test: predicted score equal in all six cohorts
    local eq ""
    forvalues c = 1/5 {
      local eq "`eq' (_b[`k'.agebirthC#0.cohort#`g'.female] = _b[`k'.agebirthC#`c'.cohort#`g'.female])"
    }
    qui test `eq'
    di "  joint test: F=" %5.2f r(F) "  p=" %6.4f r(p)

    * Boomers (cohorts 4-5) vs. earlier four cohorts (0-3), simple averages
    qui lincom (_b[`k'.agebirthC#4.cohort#`g'.female] + _b[`k'.agebirthC#5.cohort#`g'.female])/2 ///
             - (_b[`k'.agebirthC#0.cohort#`g'.female] + _b[`k'.agebirthC#1.cohort#`g'.female] ///
              + _b[`k'.agebirthC#2.cohort#`g'.female] + _b[`k'.agebirthC#3.cohort#`g'.female])/4
    di "  Boomers vs earlier: " %6.3f r(estimate) "  p=" %6.4f r(p)
  }
}


***Parity
foreach g in 1 0 {                       // 1 = women, 0 = men
  foreach k in 0 1 2 3 {                 // parity category
    estimates restore pooledP            // restore each time: -post- overwrites results
    qui margins parityC, over(cohort female) atmeans post

    di _newline "female==`g'  parityC==`k'"

    * 15 pairwise cohort differences
    forvalues c2 = 1/5 {
      forvalues c1 = 0/`=`c2'-1' {
        qui lincom _b[`k'.parityC#`c2'.cohort#`g'.female] - _b[`k'.parityC#`c1'.cohort#`g'.female]
        di "  cohort `c2' - cohort `c1': " %6.3f r(estimate) "  p=" %6.4f r(p)
      }
    }

    * joint test: predicted score equal in all six cohorts
    local eq ""
    forvalues c = 1/5 {
      local eq "`eq' (_b[`k'.parityC#0.cohort#`g'.female] = _b[`k'.parityC#`c'.cohort#`g'.female])"
    }
    qui test `eq'
    di "  joint test: F=" %5.2f r(F) "  p=" %6.4f r(p)

    * Boomers (cohorts 4-5) vs. earlier four cohorts (0-3), simple averages
    qui lincom (_b[`k'.parityC#4.cohort#`g'.female] + _b[`k'.parityC#5.cohort#`g'.female])/2 ///
             - (_b[`k'.parityC#0.cohort#`g'.female] + _b[`k'.parityC#1.cohort#`g'.female] ///
              + _b[`k'.parityC#2.cohort#`g'.female] + _b[`k'.parityC#3.cohort#`g'.female])/4
    di "  Boomers vs earlier: " %6.3f r(estimate) "  p=" %6.4f r(p)
  }
}


//  #8
//  Close

capture log close
exit
