*!version 2.0.4 :: 06/20/12 2:43 PM :: fix bug with at() returning 1x1

//  Program     : statsto
//  Author      : Joseph D. Wolfe
//  Description : Format and Export Matrices
//  Requirements: >=Stata 10 (untested with previous versions)

capture clear programs
program define statsto

syntax [anything] ,               ///
	[                             ///
                                  /// Row specification
	ROWs(string asis)             /// Default is _all
	ROWSelect(string asis)        /// V2.0.0 new name for rows()
                                  ///
		                          /// Column specification
	COLumns(string asis)          /// only for matrix returns
	COLSelect(string asis)        /// V2.0.0 new name for columns()
	Ereturns(string asis)         /// specify/rename e() returns
	EBreturns(string asis)        /// specify/rename e(b) returns
	Rreturns(string asis)         /// specify/rename r() returns
	RBreturns(string asis)        /// specify/rename r(b) returns
	Uservalues(string asis)       /// specify/rename user created scalars
	ATreturns(string asis)        /// specify/rename margins' r(at)
                                  ///
								  /// Matrix specification
	MATrix(string)                /// incoming matrix
                                  ///
								  /// Equation specification
	roweq(string asis)            /// specify/rename row equations
	coleq(string asis)            /// specify/rename row equations
	roweqstub(string asis)        /// V2.0.0 new name for roweq()
	coleqstub(string asis)        /// V2.0.0 new name for coleq()
								  ///
								  /// Prefixes
	colstub(string)               /// add stub to colnames
	rowstub(string)               /// add stub to rownames
	RStub(string)                 /// stub to column names for r()
	RBStub(string)                /// stub for column names of r(b)
	UStub(string)                 /// stub to column names for u() collections
	ATStub(string)                /// stub to column names for at()
	EStub(string)                 /// stub to column names for e()
	EBStub(string)                /// stub to column names for e(b)
	MATROWStub(string)            /// stub to row names for m()
	MATCOLStub(string)            /// stub to column names for m()
	MOUTcome(string)              /// add outcome# to mat name
	OUTcome(string)               /// add outcome# to col name
                                  ///
								  /// Viewing options
	list                          /// list matrix
	LISTNew                       /// list incoming matrix
	PREView                       /// preview changes but don't change matrix
	WIDth(numlist >0 integer)     /// width when listing matrix
	DECimals(numlist >-1 integer) /// digits when list matrix
	FORMat(string)                /// format for matrix
	title(string)                 /// title added when listing
	drop                          /// drop names $statsto_stored_matrices
	DIRectory                     /// display names in $statsto_stored_matrices
                                  ///
					              /// Matrix utilities
	clear                         /// reset matrix to null
	save(string)                  /// save matrix to new matrix
	TRANspose                     /// list/save transposed matrix
	blank                         /// add a blank row
	mv(string)                    /// default missing value = .z
	GENerate(string)              /// create variables from columns
	ATLABel(string)               /// use r(at) to create labels
                                  ///
                                  /// Statistics
	level(numlist >=10 <+99.999 ) /// significance level of p and CI
                                  ///
								  /// Advanced options
	NOStrip						  /// Don't remove omitted information
	MATROW1(string)               /// specify start row for incoming matrices
	MATCOL1(string)               /// specify start col for incoming matrices
	rowstart(string)              /// specify start row for incoming matrices
	colstart(string)              /// specify start col for incoming matrices
	unstack                       /// row eq in eb & rb become col eq
	]

//  #2
//  Create basics, macros, & temporary names

tempname INmat OUTmat INtemp rreturn VARmat TRANmat ATlabel
tempname missmat INmatrNUM INmatcNUM INmatrowNM INmatcolNM tmpmat

*name of matrix for results
if "`anything'"=="" local anything "StatSto"

*clear previously held returns
capture _return drop `rreturn'

*V2.0.0: new option names
if `"`rows'"'!="" & `"`rowselect'"'!= "" {
	di as error ///
	`"row() and rowselect() cannot be used together"'
	exit 198
}
if `"`rowselect'"'!= "" local rows `"`rowselect'"'

if `"`roweqstub'"'!="" & `"`roweq'"'!= "" {
	di as error ///
	`"roweqstub() and roweq() cannot be used together"'
	exit 198
}
if `"`roweqstub'"'!= "" local roweq `"`roweqstub'"'

if `"`coleqstub'"'!="" & `"`coleq'"'!= "" {
	di as error ///
	`"coleqstub() and coleq() cannot be used together"'
	exit 198
}
if `"`coleqstub'"'!= "" local coleq `"`coleqstub'"'

if `"`columns'"'!="" & `"`colselect'"'!= "" {
	di as error ///
	`"columns() and colselect() cannot be used together"'
	exit 198
}
if `"`colselect'"'!= "" local columns `"`colselect'"'

*check if matrix in statsto exists
capture confirm matrix `anything'
if !_rc local mat_exist   = 1
if  _rc local mat_exist   = 0
if "`clear'" !="" & `mat_exist'{
	matrix drop `anything'
	local mat_exist    = 0
}

*if drop or dir specified as mat name
if (`"`1'"' == "drop" & `"`2'"'=="") | "`drop'"!="" {
	local sendtoclear = 1
}
else local sendtoclear = 0
if (`"`1'"' == "dir" & `"`2'"'=="") | "`directory'"!=""{
	local sendtodir   = 1
}
else local sendtodir  = 0

*replace _rest with _all so new syntax will work with old
local options "rows columns matrix generate ereturns ebreturns rreturns rbreturns atreturns roweq coleq"

*loop through options that can contain _rest and replace with _all
foreach opt of local options {
	local tmpopt `"``opt''"'
	local `opt' ""
	while `"`tmpopt'"'!= "" {
		gettoken optchk   tmpopt : tmpopt , parse(" ") quote
		if `"`optchk'"'==`"_rest"' {
			local optchk "_all"
		}

		*if, e.g., row(Row=_rest) specified
		if regexm(`"`optchk'"', "=") {
			gettoken optchk1 optchk2 : optchk , parse("=") quotes
			local optchk1 : subinstr local optchk1 "=" ""
			local optchk2 : subinstr local optchk2 "=" ""

			if `"`optchk2'"'==`"_rest"' {
				local optchk `"`optchk1'=_all"'
			}
		}
		local `opt' `"``opt'' `optchk'"'
	}
}

*check if col(name=_all) is specified
if regexm(`"`columns'"', `"["a-zA-Z1-10":]*[a-zA-Z1-10]+[ ]*=[ ]*_all"') {
	local colbackup `"`columns'"'
}

*check if row(name=_all) is specified
if regexm(`"`rows'"', `"["a-zA-Z1-10":]*[a-zA-Z1-10]+[ ]*=[ ]*_all"') {
	local rowbackup `"`rows'"'
}

*make row/colstart compatibile with previous statsto versions
if "`rowstart'"!="" local matrow1 "`rowstart'"
if "`colstart'"!="" local matcol1 "`colstart'"

*need to check if columns specified
if `"`ebreturns'"'=="" & `"`rbreturns'"'=="" & `"`atreturns'"'=="" & `"`matrix'"'=="" & `"`ereturns'"'=="" & `"`rreturns'"'=="" & `"`uservalues'"'=="" local ret_type "empty"

*need to do 1 error before replacing empty row local with _all
*this gives error is row() is the only option with statsto
if `"`rows'"'!="" & `"`ret_type'"'=="empty" {
	di as error ///
	"row() cannot be used without a return specified, e.g., e()"
	exit 198
}

*need below to check for an error when adding uservalues in appendRET
*is matrix from e or r return?
local rmat = regexm(`"`matrix'"',"r\([a-zA-Z1-9]+[_a-zA-Z1-9]*\)")
local emat = regexm(`"`matrix'"',"e\([a-zA-Z1-9]+[_a-zA-Z1-9]*\)")
if  (`"`ereturns'"'!="" | `"`ebreturns'"'!="" |  `emat') & ///
	(`"`rreturns'"'=="" & `"`rbreturns'"'=="" & !`rmat')   ///
	local er_ret = 1 // just e returns
if  (`"`ereturns'"'=="" & `"`ebreturns'"'=="" & !`emat') & ///
    (`"`rreturns'"'!="" | `"`rbreturns'"'!="" |  `rmat')   ///
	local er_ret = 2 // just r
if  (`"`ereturns'"'!="" | `"`ebreturns'"'!="" | `emat')  & ///
	(`"`rreturns'"'!="" | `"`rbreturns'"'!="" | `rmat')    ///
	local er_ret = 3 // both r and e
else	local er_ret = 0

//  #3
//  defaults

*matlist replaces .z with a blank space
if "`mv'" =="" local mv ".z"
if "`mv'"!="" & "`mv'"!="." {
	if length("`mv'")>2 {
		di as error "mv() must be a period or a period with a single letter."
		exit 198
	}
	local mv = lower("`mv'")
	local mv "`mv'"
}

*default level
if "`level'" ==""  local level = 95

*grab all rows if nothing specified
if `"`rows'"'=="" local rows "_all"

*adjust outcome and moutcome
if `"`outcom'"'   != "" local colstub  = `"`colstub'`outcome'"'
if `"`moutcome'"' != "" local anything = `"`anything'`moutcome'"'

*default formatting
if `"`format'"'=="" & `"`decimals'"'=="" & `"`width'"'=="" {
	local format "%9.3g"
}

*find row if matrow1 is specified
if `"`matrow1'"'!="" {
	capture confirm integer number `matrow1'
	local eqparse = regexm(`"`matrow1'"', ":")

	*make sure that row names has quotes around it
	if `eqparse' {
		gettoken eqnm rnm: matrow1 , parse(":")
		local matrow1 `""`eqnm'"`rnm'"'
	}

	if `mat_exist' & _rc {
		local rowstart = 0
		local inrownames : rowfullnames `anything', quoted
		foreach rnm of local inrownames {
			if `"`matrow1'"'==`"`rnm'"' {
                local rowstart: list posof `"`rnm'"' in inrownames
			}
		}
		if !`rowstart' local rowstart = rowsof(`anything') + 1
	}

	if !`mat_exist' & _rc {
		local rowstart = 1
	}

	*if matrow1 is a number just make rowstart that number
	if !_rc {
		local rowstart = `matrow1'
	}
}

*find column if matrow1 is specified
if `"`matcol1'"'!="" {
	capture confirm integer number `matcol1'
	local eqparse = regexm(`"`matcol1'"', ":")

	*make sure that row names has quotes around it
	if `eqparse' {
		gettoken eqnm cnm: matcol1 , parse(":")
		local matcol1 `""`eqnm'"`cnm'"'
	}

	if `mat_exist' & _rc {
		local colstart = 0
		local incolnames : colfullnames `anything'  , quoted
		foreach cnm of local incolnames {

			if `"`matcol1'"'==`"`cnm'"' {
                local colstart: list posof `"`cnm'"' in incolnames
			}
		}
		if !`colstart' local colstart = colsof(`anything') + 1
	}

	if !`mat_exist' & _rc {
		local colstart = 1
	}

	*if matcol1 is a number just make colstart that number
	if !_rc {
		local colstart = `matcol1'
	}
}

*grab at matrix
local aterror = 0
if `"`atreturns'"' != "" {
	capture confirm matrix r(at)
	if !_rc local atmatrix "r(at)"
	if  _rc local aterror = 1
}

//  #4
//  Errors

*v202
if `"`atlabel'"'!="" {
	local cmd = r(cmd)
	capture confirm matrix e(at)
	local e_at = _rc
	capture confirm matrix r(at)
	local r_at = _rc

	if `"`cmd'"'!="margins" {
		di as error ///
		"statsto must immediately follow margins when using atlabel()"
		exit 198
	}
	if (`"`ereturns'"'!="" | `"`rreturns'"'!="" | `"`uservalues'"'!="" | `"`matrix'"'!="") {
		di as error ///
		"atlabel() can only be used with rbreturns() or ebreturns() returned by margins"
		exit 198
	}
	if (`"`ereturns'"'=="" & `"`rreturns'"'=="" & `"`uservalues'"'=="" & `"`matrix'"'=="" & `"`ebreturns'"'!="" & `"`rbreturns'"'!="") {
		di as error ///
		"atlabel() can only be used with rbreturns() or ebreturns() returned by margins"
		exit 198
	}
	if (`"`ereturns'"'=="" & `"`rreturns'"'=="" & `"`uservalues'"'=="" & `"`matrix'"'=="" & `"`ebreturns'"'=="" & `"`rbreturns'"'=="" & `"`atreturns'"'=="") {
		di as error ///
		"atlabel() can only be used with rbreturns() or ebreturns() returned by margins"
		exit 198
	}
	if `"`ebreturns'"'!="" & `e_at' {
		di as error ///
		"atlabel() can only be used with ebreturns() returned by using the post with margins"
		exit 198
	}
	if  `e_at' & `r_at' {
		di as error ///
		"atlabel() can only be used when r(at) or e(at) is returned by margins"
		exit 198
	}
}

*v200: scalars with mat not allowed
local serror = 0
if (`"`ereturns'"'!="" | `"`rreturns'"'!="" | `"`uservalues'"'!="") &  ///
(`"`ebreturns'"'!="" | `"`rbreturns'"'!="" | `"`matrix'"'!="") {
	di as error ///
	"scalar returns and matrix returns must be collected separately"
	exit 198
}

if "`decimals'"!= "" & "`format'"!="" {
	di as error ///
	"decimals() and format() cannot be used together"
	exit 198
}

if "`width'"!= "" & "`format'"!="" {
	di as error ///
	"width() and format() cannot be used together"
	exit 198
}

if `aterror' {
	di as error ///
	"r(at) is not in memory"
	exit 198
}

*adjust where incoming are being placed in collect matrix
if `"`matrow1'"' != "" {
	local matrow1num : list sizeof local matrow1
	if `matrow1num' >= 2 {
		if `matrow1num'==2 local eqcheck = regexm(`"`matrow1'"', `"\"[a-zA-Z1-9]+\":[a-zA-Z1-9]+"')
		else local eqcheck = 0
		if !`eqcheck' {
			di as error ///
			"matrow1() may only specify 1 row or eq:row"
			exit 198
		}
 	}
}
if `"`matcol1'"' != "" {
	local matcol1num : list sizeof local matcol1
	if `matcol1num' >= 2 {
		if `matcol1num'==2 local eqcheck = regexm(`"`matcol1'"', `"\"[a-zA-Z1-9]+\":[a-zA-Z1-9]+"')
		else local eqcheck = 0
		if !`eqcheck' {
			di as error ///
			"matcol1() may only specify 1 col or eq:col"
			exit 198
		}
 	}
}

*ckeck if eb or rb exists if specified
*I also use the eb rb locals later
if `"`ebreturns'"'!="" {
	capture confirm matrix e(b)
	if  _rc {
		di as error ///
		"e(b) matrix is not in memory"
		exit 198
	}
}
if `"`rbreturns'"'!="" {
	capture confirm matrix r(b)
	if  _rc {
		di as error ///
		"r(b) matrix is not in memory"
		exit 198
	}
}

*check for stub errors
if (`"`colstub'"' !="" | `"`rowstub'"'!="") & !`mat_exist' & "`ret_type'"=="empty" {
	di as error ///
	"No columns exist to add stubs"
	exit 198
}
if `"`RStub'"'  != "" & `"`rreturns'"' ==""  {
	di as error ///
	"You must specify rreturns to add rstub()"
	exit 198
}
if `"`RBStub'"' != "" & `"`rbreturns'"' ==""   {
	di as error ///
	"You must specify rbreturns to add rbstub()"
	exit 198
}
if `"`UStub'"'  != "" & `"`ureturns'"' ==""  {
	di as error ///
	"You must specify ureturns to add ustub()"
	exit 198
}
if `"`ATStub'"' != "" & `"`atreturns'"' =="" {
	di as error ///
	"You must specify atreturns to add atstub()"
	exit 198
}
if `"`EStub'"'  != "" & `"`ereturns'"' ==""    {
	di as error ///
	"You must specify ereturns to add estub()"
	exit 198
}
if `"`EBStub'"' != "" & `"`ebreturns'"' ==""  {
	di as error ///
	"You must specify ebreturns to add ebstub()"
	exit 198
}

* matrix errors
if `"`columns'"'!="" & `"`matrix'"'=="" {
	di as error ///
	"columns() can only be used with matrix()"
	exit 198
}

if `"`matrix'"'!="" {
	capture confirm matrix `matrix'
	if _rc {
		di as error ///
		"Matrix `matrix' is not in memory."
		exit 198
	}
}

//  #5
//  ebreturns

if `"`ebreturns'"'!="" {
	local ret "e"
	local ret_type "matrix"

	*need to store r-returns b/c _cllct_removeOMITTED wipes them out
	qui capture _return hold    `rreturn'
	qui capture _return restore `rreturn', hold

	*remove omitted rows/cols before preceeding, new matrix saved in returns
	_cllct_removeOMITTED, matrix(e(b)) `nostrip'

	*column name parse
	_cllct_parseNMS, innames(`ebreturns') roweq(`roweq') coleq(`coleq')
		local  INcolNMS  `"`s(IN_NMS)'"'
		local  OUTcolNMS `"`s(OUT_NMS)'"'
		local  parseCOL= `"`s(parseNMS)'"'

	*row name parse
	_cllct_parseNMS, innames(`rows')  rownames(1) roweq(`roweq') ///
	coleq(`coleq')
		local  INrowNMS   `"`s(IN_NMS)'"'
		local  OUTrowNMS  `"`s(OUT_NMS)'"'
		local  parseROW = `"`s(parseNMS)'"'

	if "`unstack'"!="" {
		_cllct_parseEQNMS, incolnms(`"`INcolNMS'"')         ///
		outcolnms(`"`OUTcolNMS'"') inrownms(`"`INrowNMS'"') ///
		outrownms(`"`OUTrowNMS'"')  ret(`"`ret'"')          ///
		parseCOLnms(`parseCOL')  parseROWnms(`parseROW')    ///
		rettype(eb)

		*add/replace equations
		_cllct_addEQNMS, innames(`ebreturns') newincol(`s(newINcolNMS)') ///
		newoutcol(`s(newOUTcolNMS)') newinrow(`s(newINrowNMS)')          ///
		newoutrow(`s(newOUTrowNMS)') roweq(`roweq') coleq(`coleq') `unstack'
	}

	if "`unstack'"=="" {
		_cllct_parseMATeq, matrix(`matrix') columns(`columns') ///
		rows(`rows')  incol(`INcolNMS') outcol(`OUTcolNMS')    ///
		inrow(`INrowNMS') outrow(`OUTrowNMS') bstats(1)

		*add/replace equations
		_cllct_addEQNMS, innames(`ebreturns') newincol(`s(newINcolNMS)') ///
		newoutcol(`s(newOUTcolNMS)') newinrow(`s(newINrowNMS)')          ///
		newoutrow(`s(newOUTrowNMS)') roweq(`roweq') coleq(`coleq') bstats(1)
	}
		local ebOUTcolNMS   `"`s(newOUTcolNMS)'"'
		local ebINcolNMS    `"`s(newINcolNMS)'"'
		local ebINrowNMS    `"`s(newINrowNMS)'"'
		local ebOUTrowNMS   `"`s(newOUTrowNMS)'"'
		local ebsingleRowEQ `"`s(singleRowEQ)'"'

	*append returns to matrix
	_cllct_appendRET, cllctmat(`"`anything'"') ret(`"`ret'"')             ///
	incolnms(`"`ebINcolNMS'"')  outcolnms(`"`ebOUTcolNMS'"')              ///
	outrownms(`"`ebOUTrowNMS'"') inrownms(`"`ebINrowNMS'"')               ///
	level(`"`level'"') mv(`"`mv'"') `preview' `listnew'                   ///
	ret_type(`"`ret_type'"') bstats(1) uvalues(0) er_ret(`er_ret')        ///
	retstub(`"`ebstub'"') colstub(`"`colstub'"') rowstub(`"`rowstub'"')   ///
	`rowmiss' `colmiss' singleroweq(`ebsingleRowEQ') rowstart(`rowstart') ///
	colstart(`colstart')  `unstack' rowbackup(`"`rowbackup'"')            ///
	warninfo(`WARNinfo') _retrest(`rreturn') atlabel(`atlabel')

	*Was user already warned about overwriting info?
	local WARNinfo `s(WARNinfo)'

	*restore rreturns
	qui capture _return restore `rreturn'
}

//  #6
//  rbreturns

if `"`rbreturns'"'!="" {
	local ret      "r"
	local ret_type "matrix"

	*need to store r-returns b/c _cllct_removeOMITTED wipes them out
	qui capture _return hold `rreturn'
	qui capture _return restore `rreturn', hold

	*remove omitted rows/cols before preceeding
	*new matrix saved in returns
	_cllct_removeOMITTED, matrix(r(b)) `nostrip'

	*col name parse
	_cllct_parseNMS, innames(`rbreturns') roweq(`roweq') coleq(`coleq')
		local  INcolNMS  `"`s(IN_NMS)'"'
		local  OUTcolNMS `"`s(OUT_NMS)'"'
		local  parseCOL =`"`s(parseNMS)'"'

	*row name parse
	_cllct_parseNMS, innames(`rows') rownames(1) roweq(`roweq') ///
	coleq(`coleq')
		local  INrowNMS   `"`s(IN_NMS)'"'
		local  OUTrowNMS  `"`s(OUT_NMS)'"'
		local  parseROW = `"`s(parseNMS)'"'

	if "`unstack'"!="" {
		_cllct_parseEQNMS, incolnms(`"`INcolNMS'"')         ///
		outcolnms(`"`OUTcolNMS'"') inrownms(`"`INrowNMS'"') ///
		outrownms(`"`OUTrowNMS'"')  ret(`"`ret'"')          ///
		parseCOLnms(`parseCOL')  parseROWnms(`parseROW')    ///
		rettype(eb)

		*add/replace equations
		_cllct_addEQNMS, innames(`rbreturns') newincol(`s(newINcolNMS)') ///
		newoutcol(`s(newOUTcolNMS)') newinrow(`s(newINrowNMS)')          ///
		newoutrow(`s(newOUTrowNMS)') roweq(`roweq') coleq(`coleq') `unstack'
	}

	if "`unstack'"=="" {
		_cllct_parseMATeq, matrix(`matrix') columns(`columns') ///
		rows(`rows')  incol(`INcolNMS') outcol(`OUTcolNMS')    ///
		inrow(`INrowNMS') outrow(`OUTrowNMS') bstats(1)

		*add/replace equations
		_cllct_addEQNMS, innames(`rbreturns') newincol(`s(newINcolNMS)') ///
		newoutcol(`s(newOUTcolNMS)') newinrow(`s(newINrowNMS)')          ///
		newoutrow(`s(newOUTrowNMS)') roweq(`roweq') coleq(`coleq') bstats(1)
	}
		local rbOUTcolNMS   `"`s(newOUTcolNMS)'"'
		local rbINcolNMS    `"`s(newINcolNMS)'"'
		local rbINrowNMS    `"`s(newINrowNMS)'"'
		local rbOUTrowNMS   `"`s(newOUTrowNMS)'"'
		local rbsingleRowEQ `"`s(singleRowEQ)'"'

	*append returns to matrix
	_cllct_appendRET, cllctmat(`"`anything'"') ret(`"`ret'"')             ///
	incolnms(`"`rbINcolNMS'"')  outcolnms(`"`rbOUTcolNMS'"')              ///
	outrownms(`"`rbOUTrowNMS'"') inrownms(`"`rbINrowNMS'"')               ///
	level(`"`level'"') mv(`"`mv'"') `preview' `listnew'                   ///
	ret_type(`"`ret_type'"') bstats(1) uvalues(0)  er_ret(`er_ret')       ///
	retstub(`"`rbstub'"') colstub(`"`colstub'"') rowstub(`"`rowstub'"')   ///
	`rowmiss' `colmiss' singleroweq(`rbsingleRowEQ') rowstart(`rowstart') ///
	colstart(`colstart') `unstack' rowbackup(`"`rowbackup'"')             ///
	warninfo(`WARNinfo') _retrest(`rreturn') atlabel(`atlabel')

	*Was user already warned about overwriting info?
	local WARNinfo `s(WARNinfo)'

	*restore rreturns
	qui capture _return restore `rreturn'
}

//  #7
//  atmatrix

if "`atreturns'"!="" {
	local ret      "matrix"
	local ret_type "atmatrix"
	local er_ret =0

	*need to store r-returns b/c _cllct_removeOMITTED wipes them out
	qui capture _return hold `rreturn'
	qui capture _return restore `rreturn', hold

	*remove omitted rows/cols before preceeding
	*new matrix saved in returns
	_cllct_removeOMITTED, matrix(r(b)) `nostrip'

	_cllct_parseNMS, innames(`atreturns') roweq(`roweq') coleq(`coleq')
		local  INcolNMS  `"`s(IN_NMS)'"'
		local  OUTcolNMS `"`s(OUT_NMS)'"'

	*row name parse
	_cllct_parseNMS, innames(`rows')  rownames(1) roweq(`roweq') coleq(`coleq')
		local  INrowNMS   `"`s(IN_NMS)'"'
		local  OUTrowNMS  `"`s(OUT_NMS)'"'

	_cllct_parseMATeq,  matrix(r(INatm)) columns(`atreturns')              ///
					    incol(`INcolNMS') outcol(`OUTcolNMS') bstats(0)    ///
					    inrow(`INrowNMS') outrow(`OUTrowNMS') rows(`rows') ///
						ret_type(`ret_type')
 		local cstub    `"`s(newOUTcolSTBx)'"'
		local cadjust  `"`s(cadjust)'"'
 		local rstub    `"`s(newOUTrowSTB)'"'
		local radjust  `"`s(radjust)'"'

	_cllct_addEQNMS, innames(`atreturns') newincol(`s(newINcolNMS)')     ///
	newoutcol(`s(newOUTcolNMS)') newinrow(`s(newINrowNMS)')              ///
	newoutrow(`s(newOUTrowNMS)') roweq(`roweq') coleq(`coleq') bstats(0)
		local atmOUTcolNMS   `"`s(newOUTcolNMS)'"'
		local atmINcolNMS    `"`s(newINcolNMS)'"'
		local atmINrowNMS    `"`s(newINrowNMS)'"'
		local atmOUTrowNMS   `"`s(newOUTrowNMS)'"'
		local atsingleRowEQ  `"`s(singleRowEQ)'"'

	_cllct_appendRET, cllctmat(`"`anything'"') ret(`"`ret'"')              ///
	incolnms(`"`atmINcolNMS'"')  outcolnms(`"`atmOUTcolNMS'"')             ///
	outrownms(`"`atmOUTrowNMS'"') inrownms(`"`atmINrowNMS'"')              ///
	mv(`"`mv'"') `preview' `listnew' ret_type(`"`ret_type'"') bstats(0)    ///
	uvalues(0)  er_ret(`er_ret') retstub(`"`atstub'"')                     ///
	colstub(`"`colstub'`cstub'"') rowstub(`"`rowstub'`rstub'"')            ///
	matrix(r(INatm)) `rowmiss' `colmiss'                                   ///
	level(`"`level'"') singleroweq(`atsingleRowEQ') rowstart(`rowstart')   ///
	colstart(`colstart') cadjust(`cadjust') radjust(`radjust')             ///
	rowbackup(`"`rowbackup'"') warninfo(`WARNinfo') _retrest(`rreturn')    ///
	atlabel(`atlabel')

	*Was user already warned about overwriting info?
	local WARNinfo `s(WARNinfo)'

	*restore rreturns
	qui capture _return restore `rreturn'
}

//  #8
//  Matrix

if "`matrix'"!="" {
	local ret      "matrix"
	local ret_type "matrix"
	local er_ret =0

	*need to store r-returns b/c _cllct_removeOMITTED wipes them out
	qui capture _return hold `rreturn'
	qui capture _return restore `rreturn', hold

	*remove omitted rows/cols before preceeding
	*new matrix saved in returns
	_cllct_removeOMITTED, matrix(`matrix') `nostrip'

	_cllct_parseNMS, innames(`columns') roweq(`roweq') coleq(`coleq')
		local  INcolNMS  `"`s(IN_NMS)'"'
		local  OUTcolNMS `"`s(OUT_NMS)'"'

	*row name parse
	_cllct_parseNMS, innames(`rows')  rownames(1) roweq(`roweq') coleq(`coleq')
		local  INrowNMS   `"`s(IN_NMS)'"'
		local  OUTrowNMS  `"`s(OUT_NMS)'"'

	_cllct_parseMATeq, matrix(`matrix') columns(`columns') rows(`rows')  ///
					    incol(`INcolNMS') outcol(`OUTcolNMS')            ///
					    inrow(`INrowNMS') outrow(`OUTrowNMS') bstats(0)
 		local cstub    `"`s(newOUTcolSTB)'"'
		local cadjust  `"`s(cadjust)'"'
 		local rstub    `"`s(newOUTrowSTB)'"'
		local radjust  `"`s(radjust)'"'

	_cllct_addEQNMS, innames(`columns') newincol(`s(newINcolNMS)')       ///
	newoutcol(`s(newOUTcolNMS)') newinrow(`s(newINrowNMS)')              ///
	newoutrow(`s(newOUTrowNMS)') roweq(`roweq') coleq(`coleq') bstats(0)
		local mOUTcolNMS   `"`s(newOUTcolNMS)'"'
		local mINcolNMS    `"`s(newINcolNMS)'"'
		local mINrowNMS    `"`s(newINrowNMS)'"'
		local mOUTrowNMS   `"`s(newOUTrowNMS)'"'
		local msingleRowEQ `"`s(singleRowEQ)'"'

	_cllct_appendRET, cllctmat(`"`anything'"') ret(`"`rel_type'"')         ///
	incolnms(`"`mINcolNMS'"')  outcolnms(`"`mOUTcolNMS'"')                 ///
	outrownms(`"`mOUTrowNMS'"') inrownms(`"`mINrowNMS'"')                  ///
	mv(`"`mv'"') `preview' `listnew' ret_type(`"`ret_type'"') bstats(0)    ///
	uvalues(0)  er_ret(`er_ret')                                           ///
	colstub(`"`matcolstub'`cstub'"') rowstub(`"`matrowstub'`rstub'"')      ///
	matrix(`"`matrix'"')  `rowmiss' `colmiss'                              ///
	level(`"`level'"') singleroweq(`msingleRowEQ') rowstart(`rowstart')    ///
	colstart(`colstart') cadjust(`cadjust') radjust(`radjust')             ///
	rowbackup(`"`rowbackup'"') colbackup(`"`colbackup'"')                  ///
	warninfo(`WARNinfo') _retrest(`rreturn')

	*Was user already warned about overwriting info?
	local WARNinfo `s(WARNinfo)'

	*restore rreturns
	qui capture _return restore `rreturn'
}

//  #9
//  ereturns

if `"`ereturns'"'!="" {
	local ret      "e"
	local ret_type "scalar"

	*need to store r-returns b/c _cllct_removeOMITTED wipes them out
	qui capture _return hold `rreturn'
	qui capture _return restore `rreturn', hold

	_cllct_parseNMS, innames(`ereturns') roweq(`roweq') coleq(`coleq')
		local  INcolNMS  `"`s(IN_NMS)'"'
		local  OUTcolNMS `"`s(OUT_NMS)'"'

	_cllct_parseNMS, innames(`rows')  rownames(1) roweq(`roweq')  coleq(`coleq')
		local  INrowNMS   `"`s(IN_NMS)'"'
		local  OUTrowNMS  `"`s(OUT_NMS)'"'

	_cllct_addEQNMS, innames(`ereturns') newincol(`INcolNMS') ///
	newoutcol(`OUTcolNMS') newinrow(`INrowNMS')               ///
	newoutrow(`OUTrowNMS') roweq(`roweq') coleq(`coleq') bstats(0)
		local eOUTcolNMS   `"`s(newOUTcolNMS)'"'
		local eINcolNMS    `"`s(newINcolNMS)'"'
		local eINrowNMS    `"`s(newINrowNMS)'"'
		local eOUTrowNMS   `"`s(newOUTrowNMS)'"'
		local esingleRowEQ `"`s(singleRowEQ)'"'

	*append returns to matrix
	_cllct_appendRET, cllctmat(`"`anything'"') ret(`"`ret'"')              ///
	incolnms(`"`eINcolNMS'"')  outcolnms(`"`eOUTcolNMS'"')                 ///
	outrownms(`"`eOUTrowNMS'"') inrownms(`"`eINrowNMS'"')                  ///
	mv(`"`mv'"') `preview' `listnew' ret_type(`"`ret_type'"') bstats(0)    ///
	uvalues(0)  er_ret(`er_ret') retstub(`"`estub'"')                      ///
	colstub(`"`colstub'"') rowstub(`"`rowstub'"') `rowmiss' `colmiss'      ///
	level(`"`level'"') singleroweq(`esingleRowEQ') rowstart(`rowstart')    ///
	colstart(`colstart') rowbackup(`"`rowbackup'"') warninfo(`WARNinfo')   ///
	_retrest(`rreturn')

	*Was user already warned about overwriting info?
	local WARNinfo `s(WARNinfo)'

	*Get row number for scalars
	local srowname `"`s(srowname)'"'

	*restore rreturns
	qui capture _return restore `rreturn'
}

//  #10
//  rreturns

if `"`rreturns'"'!="" {
	local ret      "r"
	local ret_type "scalar"

	*need to store r-returns b/c _cllct_removeOMITTED wipes them out
	qui capture _return hold `rreturn'
	qui capture _return restore `rreturn', hold

	_cllct_parseNMS, innames(`rreturns') roweq(`roweq') coleq(`coleq')
		local  INcolNMS  `"`s(IN_NMS)'"'
		local  OUTcolNMS `"`s(OUT_NMS)'"'

	_cllct_parseNMS, innames(`rows')  rownames(1) roweq(`roweq') coleq(`coleq')
		local  INrowNMS   `"`s(IN_NMS)'"'
		local  OUTrowNMS  `"`s(OUT_NMS)'"'

	_cllct_addEQNMS, innames(`rreturns') newincol(`INcolNMS') ///
	newoutcol(`OUTcolNMS') newinrow(`INrowNMS')               ///
	newoutrow(`OUTrowNMS') roweq(`roweq') coleq(`coleq') bstats(0)
		local rOUTcolNMS   `"`s(newOUTcolNMS)'"'
		local rINcolNMS    `"`s(newINcolNMS)'"'
		local rINrowNMS    `"`s(newINrowNMS)'"'
		local rOUTrowNMS   `"`s(newOUTrowNMS)'"'
		local rsingleRowEQ `"`s(singleRowEQ)'"'

	_cllct_appendRET, cllctmat(`"`anything'"') ret(`"`ret'"')            ///
	incolnms(`"`rINcolNMS'"')  outcolnms(`"`rOUTcolNMS'"')               ///
	outrownms(`"`rOUTrowNMS'"') inrownms(`"`rINrowNMS'"')                ///
	level(`"`level'"') mv(`"`mv'"') `preview' `listnew'                  ///
	ret_type(`"`ret_type'"') bstats(0) uvalues(0)  er_ret(`er_ret')      ///
	retstub(`"`rstub'"') colstub(`"`colstub'"') rowstub(`"`rowstub'"')   ///
	`rowmiss' `colmiss' singleroweq(`rsingleRowEQ') rowstart(`rowstart') ///
	colstart(`colstart') rowbackup(`"`rowbackup'"')                      ///
	scalarRow(`"`srowname'"') warninfo(`WARNinfo') _retrest(`rreturn')

	*Was user already warned about overwriting info?
	local WARNinfo `s(WARNinfo)'

	*Get row number for scalars
	local srowname `"`s(srowname)'"'

	*restore rreturns
	qui capture _return restore `rreturn'
}

//  #11
//  uservalues

if `"`uservalues'"'!="" {
	local ret_type "scalar"

	*need to store r-returns b/c _cllct_removeOMITTED wipes them out
	qui capture _return hold `rreturn'
	qui capture _return restore `rreturn', hold

	_cllct_parseNMS, innames(`uservalues') roweq(`roweq') coleq(`coleq')
		local  INcolNMS  `"`s(IN_NMS)'"'
		local  OUTcolNMS `"`s(OUT_NMS)'"'

	_cllct_parseNMS, innames(`rows') rownames(1) roweq(`roweq') coleq(`coleq')
		local  INrowNMS   `"`s(IN_NMS)'"'
		local  OUTrowNMS  `"`s(OUT_NMS)'"'

	_cllct_addEQNMS, innames(`ureturns') newincol(`INcolNMS') ///
	newoutcol(`OUTcolNMS') newinrow(`INrowNMS')  bstats(0)    ///
	newoutrow(`OUTrowNMS') roweq(`roweq') coleq(`coleq') u_exist(1)
		local uOUTcolNMS   `"`s(newOUTcolNMS)'"'
		local uINcolNMS    `"`s(newINcolNMS)'"'
		local uINrowNMS    `"`s(newINrowNMS)'"'
		local uOUTrowNMS   `"`s(newOUTrowNMS)'"'
		local usingleRowEQ `"`s(singleRowEQ)'"'

	_cllct_appendRET, cllctmat(`"`anything'"') ret(`"`ret'"')              ///
	incolnms(`"`uINcolNMS'"')  outcolnms(`"`uOUTcolNMS'"')                 ///
	outrownms(`"`uOUTrowNMS'"') inrownms(`"`uINrowNMS'"')                  ///
	 mv(`"`mv'"') `preview' `listnew' ret_type(`"`ret_type'"') bstats(0)   ///
	uvalues(1)  er_ret(`er_ret') retstub(`"`ustub'"')                      ///
	colstub(`"`colstub'"') rowstub(`"`rowstub'"') `rowmiss' `colmiss'      ///
	level(`"`level'"') singleroweq(`usingleRowEQ') rowstart(`rowstart')    ///
	colstart(`colstart') rowbackup(`"`rowbackup'"')                        ///
	scalarRow(`"`srowname'"') warninfo(`WARNinfo') _retrest(`rreturn')

	*Was user already warned about overwriting info?
	local WARNinfo `s(WARNinfo)'

	*Get row number for scalars
	local srowname `"`s(srowname)'"'

	*restore rreturns
	qui capture _return restore `rreturn'
}

//  #12
//  Add blank row

if "`blank'" != "" {
	local cnum               = colsof(`anything')
	matrix `INtemp'          = J(1,`cnum',`mv')
	matrix rownames `INtemp' = "----"
	mat `anything' = `anything' \ `INtemp'
}

//  #13
//  Adjust formatting options

*check if matrix in statsto exists now
capture confirm matrix `anything'
if !_rc local mat_exist   = 1
if  _rc local mat_exist   = 0

if `"`format'"'=="" & `"`decimals'"'=="" & `"`width'"'!="" {
	local format "%`width'.3g"
}
if `"`width'"'=="" local twidth = 12    // row title width
if `"`width'"'!="" local twidth = `width'

if "`decimals'"!= "" {
	if `"`width'"'=="" local width = 9
	local decnum : list sizeof local decimals
	local colnum = colsof(`anything')
	local rownum = rowsof(`anything')

	*error if too many decimals specified
	if `decnum' > `colnum' {
		di as error ///
		"You have specified too many columns in decimals()"
		exit 198
	}

	*get equations for loops below
	local ceq : coleq `anything', quoted

	*more than 1 decimal specified
	if `decnum' > 1 {
		local and     ""
		forvalues dn = 1(1)`decnum' {
			local dec : word `dn' of `decimals'
			local eq  : word `dn' of `ceq'
			local colline ""

			*add lines for equations
			if `dn'==1 & `"`eq'"'!="_" local colline "|"
			if `dn'>1 & `dn'<=`colnum'{
				local and "&"
				local prevnum = `dn'-1
				local preveq : word `prevnum' of `ceq'
				local preseq : word `dn'      of `ceq'

				if `"`preveq'"'!=`"`preseq'"' {
					local colline "|"
					local and ""
				}
				if `dn' ==`colnum' {
					local colline ""
				}
			}
			local newformat `"`newformat' `and' %`width'.`dec'f`colline'"'
		}

		if `decnum' < `colnum' {
			local start   = `decnum' + 1
			local prevnum = `decnum'

			forvalues x = `start'(1)`colnum' {
				local preveq  : word `prevnum' of `ceq'
				local preseq  : word `x'      of `ceq'
				local colline ""
				local and "&"

				if `"`preveq'"'!=`"`preseq'"' & `x'<`colnum'{
					local colline "|"
					local and ""
				}

				local addcfmt "`addcfmt' `and' %`width'.7g`colline'"
			}
		}
		local newformat `"cspec(&%`twidth's| `newformat' `addcfmt' o1&)"'

		*create row formating
		forvalues x = 1(1)`rownum' {
				if `x' >1 local addrfmt "`addrfmt' &"
			}
		local newformat `"`newformat' rspec(||`addrfmt'|)"'
	}
	else {
		local newformat `"format(%`width'.`decimals'f)"'
	}
} // end decimals

*format options
if "`format'"!= "" &  `mat_exist' {
	local formnum : list sizeof local format
	local colnum = colsof(`anything')
	local rownum = rowsof(`anything')

	*error if too many formimals specified
	if `formnum' > `colnum' {
		di as error ///
		"You have specified too many columns in format()"
	}

	*more than 1 formimal specified
	if `formnum' > 1 {
		forvalues dn = 1(1)`formnum' {
			local form : word `dn' of `format'
			if `dn'>1 local and "&"
			local newformat `"`newformat' `and' `form'"'
		}

		if `formnum' < `colnum' {
			local start = `formnum' + 1
			forvalues x = `start'(1)`colnum' {
				local addcfmt "`addcfmt' & %9.7g"
			}
		}

		local newformat `"cspec(& %`twidth's| `newformat' `addcfmt' o1&)"'

		*create row formating
		forvalues x = 1(1)`rownum' {
				if `x' >1 local addrfmt "`addrfmt' &"
			}
		local newformat `"`newformat' rspec(||`addrfmt'|)"'
	}
	else {
		local newformat `"format(`format')"'
	}
} // end format

if !regexm(`"`newformat'"', "rspec") & !regexm(`"`newformat'"', "cspec") {
	local newformat `"twidth(`twidth') `newformat'"'
}

//  #14
//  List, Save, Transpose Matrix

if `mat_exist' matrix `OUTmat' = `anything'

if !`mat_exist' & !`sendtodir' & !`sendtoclear' & "`clear'"=="" {
	di as error ///
	"Matrix `anything' is not in memory"
	exit 198
}

if "`transpose'"!="" & ("`list'"!="" | "`list'"=="") {
	matrix  `TRANmat' = (`OUTmat')'
	matlist `TRANmat', nodotz border(top bottom) title("`title'") `newformat'
	if "`save'"!="" matrix  `save' = `TRANmat'
}

if "`list'"!="" & "`transpose'"=="" {
	matlist `OUTmat', nodotz border(top bottom) title("`title'") `newformat'
	if "`save'"!="" matrix  `save' = `OUTmat'
}

//  #15
//  Create Variables from Matrix Columns

if `"`generate'"' != "" {
	*check if all columns are desired
	foreach col of local generate {
		local _allcol = strmatch(`"`col'"', "_all")
		if !`_allcol' local newgen `"`newgen' `col'"'
	}

	local wdcount : length local newgen

	*error: _all and other col-names exist
	if `_allcol' & `wdcount' {
		di as error ///
		"generate() cannot contain _all along with column names"
		exit 198
	}

	if `_allcol' {
		svmat `OUTmat', names(col)
	}

	if !`_allcol' {
		local colnms : colnames `OUTmat'
		local eqnms  : coleq `OUTmat'

		*error: need to specify col# if equations are present
		foreach eq of local eqnms {
			if `"`eq'"' != "_" {
				di as error ///
				"You must use column number in generate() if matrix contains equations"
				exit 198
			}
		}

		local colnum = 0
		foreach nm of local generate  {
			capture confirm integer number `nm'
			if !_rc mat `VARmat' = `OUTmat'[1...,`nm']
			if  _rc mat `VARmat' = `OUTmat'[1...,"`nm'"]
			svmat `VARmat', names(col)
		}
	}
}

//  #16
//  Drop returns in hold

capture _return drop `rreturn'

//  #17
//  Collect matrix name - remove duplicates

if !`sendtoclear' & !`sendtodir' {
	local  tabstored_tables `"$statsto_stored_matrices"'
	local  tabstored_tables  `"`tabstored_tables' `anything' `save'"'
	local  tabstored_tables : list uniq tabstored_tables

	*remove matrix name if it no longer exists
	if !`mat_exist' {
		local  tabstored_tables : subinstr local tabstored_tables `"`anything'"' ""
	}
	global statsto_stored_matrices  `"`tabstored_tables'"'
}

//  #18
//  Display or remove matrix names from global

if `sendtoclear' {
	capture macro drop statsto_stored_matrices
	di as text "(note: matrix names removed)"
}

if `sendtodir' {
	foreach tab of global statsto_stored_matrices {
		di as text `"`tab'"'
	}
	if "$statsto_stored_matrices" == "" {
		di as text "(note: no matrix names in directory)"
	}
}

//  #18
//  end statsto

end


// subroutines ---------------------------------------------------------------

//  #0
//  Program     : _cllct_appendRET
//  Author      : Joseph D. Wolfe
//  Description : Append matrix_name with e or r Return (was appendERret)
//  Requirements: >=Stata 10 (currently untested on previous versions)

* version 0.2.2 :: 2012-05-04 :: bug with or stats, add orpct
* version 0.2.3 :: 2012-05-05 :: bug rowstart colstart
* version 0.2.4 :: 2012-05-08 :: uservalues row#

//  #1
//  Program Define

program _cllct_appendRET, sclass

syntax ,                       ///
	[                          ///
	cllctmat(string)           ///  collect matrix
	incolnms(string)           ///  incoming column names
	inrownms(string)           ///  incoming row names
	outcolnms(string)          ///  outgoing columns
	outrownms(string)          ///  outgoing rows
	ret(string)                ///  current return; e, r, u
	ret_type(string)           ///  matrix, scalar, empty, atmatrix
	er_ret(numlist)            ///  contains combo of returns user wants
	bstats(numlist)            ///  1 for rb or eb, 0 otherwise
	matrix(string)             ///  name of incoming matrix
    uvalues(numlist)           ///  user values are being used
							   ///
	singleroweq(string asis)   ///  add equations to rows
	rowbackup(string)          ///  word=_all syntax used
	colbackup(string)          ///  word=_all syntax used
	scalarRow(string)          ///  Row name for scalars without row name
							   ///
	rowstart(numlist)          ///  specifies where to start incoming mat
	colstart(numlist)          ///
                               ///
	allrows(string)            ///  Collect all rows of matrix
					           ///
	cadjust(numlist)           ///  adjust col/row names in matrix?
	radjust(numlist)           ///
							   ///
	colstub(string)            ///  various stub options
	rowstub(string)            ///
	retstub(string)            ///
                               ///
	mv(string)	               ///  missing values
	level(numlist)             ///  level for ci
	title(string)              ///  title to display
	preview                    ///  show changes before making them
	listnew                    ///  list changes made without saving
							   ///
	unstack					   ///  move incoming row eq to col eqs
							   ///
	WARNinfo(numlist)          ///  Warn User They're Overwriting Information
	_retrest(string)           ///  Restore r-return
	atlabel(string)            ///  If atlabel() option is specified
	]

//  #2
//  Set macros, defaults

if "`cadjust'" =="" local cadjust  = 0
if "`radjust'" =="" local radjust  = 0
if "`everycol'"=="" local everycol = 0

*temporary names for matrix/return manipulation
tempname INmat INatm INtemp INstats INstatsMAT INscalar INeq INcov INmatR INmatC REMrow
tempname INtempmat INtempCNM INtempRNM MAThold
tempname INmatrNUM INmatcNUM INmatrowNM INmatcolNM tmpmat
tempname OUTmat OUTtemp OUTtempmat OUTtempCNM OUTtempRNM
tempname rreturn ereturn ATlabel missmat

*temporary names for caculating statistics
tempname estval estimateval bval seval Fval tval chi2val zval
tempname p levelpval cifactorval lbval ubval

*check if omitted variables have been removed
capture confirm matrix r(INmat)
if !_rc matrix `INmat' = r(INmat)
if  _rc & "`matrix'"!="" matrix `INmat' = `matrix'
if  _rc & `bstats'       matrix `INmat' = `ret'(b)
capture confirm matrix r(INcov)
if !_rc matrix `INcov' = r(INcov)
if  _rc & `bstats' matrix `INcov' = `ret'(V)
capture confirm matrix r(INatm)
if !_rc matrix `INatm' = r(INatm)
if  _rc matrix `INatm' = r(at)

*convert any #s to row or column names
if `"`ret_type'"'=="matrix" | `"`ret_type'"'=="atmatrix" {
	if `"`ret_type'"'=="atmatrix" local inmat "INatm"
	else local inmat "INmat"

	_cllct_rowcolNUM, innm(`incolnms') inmat(`inmat') outnm(`outcolnms') col(1) eb(`bstats')
	local OUTcolNMS  `"`s(OUT_NMS)'"'
	local INcolNMS   `"`s(IN_NMS)'"'

	_cllct_rowcolNUM, innm(`inrownms') inmat(`inmat') outnm(`outrownms') col(0) eb(`bstats')
	local INrowNMS   `"`s(IN_NMS)'"'
	local OUTrowNMS  `"`s(OUT_NMS)'"'
}
if `"`ret_type'"'=="scalar" {
	local INcolNMS  `"`incolnms'"'
	local OUTcolNMS `"`outcolnms'"'
	local INrowNMS  `"`inrownms'"'
	local OUTrowNMS `"`outrownms'"'
}

*restore r-returns // v179
qui capture _return restore `_retrest', hold

*get names for incoming and outcoming columns and rows
local newOUTcolNMS  `"`OUTcolNMS'"'
local newINcolNMS   `"`INcolNMS'"'

*need this for margins related commands
local ver = c(stata_version)

*need this for displaying warning for overwriting
if `"`warninfo'"'!="1" local WARNinfo = 0
if `"`warninfo'"'=="1" local WARNinfo = 1

*need to set pval
scalar `levelpval'  = (1 - (`level'/100) )/2

*this local allows me to drop columns where no rows are specified
*I use it in loops below to remove unwanted columns
local new_matnum =  0

*need to reconfirm if matrix exists - it has possibly been added
capture confirm matrix `cllctmat'
if !_rc local mat_exist = 1
if  _rc local mat_exist = 0

*check if z or t-values: t-values are indicated by (df_r) return
capture confirm scalar `ret'(df_r)

if !_rc { // df_r exists
	local isT = 1
	local isZ = 0
	local df  = `ret'(df_r)
}
if _rc { // df_r does not exist
	capture confirm scalar `ret'(df) // ned to check if df is less than N
	local N = e(N)

	if !_rc  {
		local df = `ret'(df)
		if `df'<`N' {     // if so then it's a t-test
			local isT = 1
			local isZ = 0
		}
		if `df'==`N' {     // if equal it's a z-test
			local df  = 0
			local isT = 0
			local isZ = 1
		}
	}
	if _rc {
		local isT = 0
		local isZ = 1
		local df  = 0
	}
}

*need to get a count of how many ":" tells me how many row/cols
local trash: subinstr local newINcolNMS ":" "", all count(local colcount)
local trash: subinstr local INrowNMS    ":" "", all count(local inrowcount)
local trash: subinstr local OUTrowNMS   ":" "", all count(local outrowcount)

if `"`colbackup'"'!="" & `"`matrix'"'!="" {
	local colcount = colsof(`INmat')
	local newOUTcolNMS  ""
	local newINcolNMS   ""
	local OUTcolNMS     ""
	local INcolNMS      ""
	local colrename     ""
	local newcolnms     ""
	local newcolnms     ""
	gettoken newout : colbackup, parse("=")

	forvalues rc = 1/`colcount' {
		if regexm(`"`colbackup'"', ":") {
			gettoken newouteq temp: colbackup, parse(":")
			gettoken newoutnm : temp, parse("=")
			local    newoutnm : subinstr local newoutnm ":" ""
			local INcolNMS  `"`INcolNMS' "`newouteq'":`newoutnm'`rc'"'
			local colrename `"`colrename'`""`newouteq'":`newoutnm'`rc'"'"'
		}
		else {
			gettoken newout : colbackup, parse("=")
			local INcolNMS `"`INcolNMS' "_":`newout'`rc'"'
			local colrename `"`colrename' `newout'`rc'"'
		}
	}
	local newOUTcolNMS `"`INcolNMS'"'
	local newINcolNMS  `"`INcolNMS'"'
	local OUTcolNMS    `"`INcolNMS'"'

	mat colnames `INmat' = `colrename'
}

//  #4
//  Split Matrix into Column Vectors

local num   = 0
local kpnum = 0

forvalues matcol = 1(1)`colcount' {
	local ++num
	local instop  = 0
	local outstop = 0
	local ineqnm  ""
	local eqnm    ""
	local outeqnm ""
	local eqnmnew ""
	local stat    ""
	local statnew ""
	tempname INmat`num' INtemp`num' INcov`num'

	*need row numbers for uvalues
	if  `mat_exist' local tmpoutrownum = rowsof(`cllctmat')
	if !`mat_exist' local tmpoutrownum = 0

	*get equation and column names and what they should be changed to
	*below trims equation name off and then gets column name (stat)
	while !`instop' {
		gettoken ineqnm newINcolNMS : newINcolNMS, parse(":")
		local eqnm "`eqnm' `ineqnm'"
		local instop = regexm("`eqnm'", ":")
		local eqnm : subinstr local eqnm ":" ""
		local eqnm = trim(`"`eqnm'"')
		if `"`newINcolNMS'"' == "" local instop = 1
	}
	gettoken stat newINcolNMS: newINcolNMS

	while !`outstop' {
		gettoken outeqnm newOUTcolNMS : newOUTcolNMS, parse(":")
		local eqnmnew `"`eqnmnew' `outeqnm'"'
		local outstop = regexm(`"`eqnmnew'"', ":")
		local eqnmnew : subinstr local eqnmnew ":" ""
		local eqnmnew = trim(`"`eqnmnew'"')
		if `"`newOUTcolNMS'"' == "" local outstop = 1
	}
	gettoken statnew newOUTcolNMS: newOUTcolNMS

	*if user wants column name adjusted
	if `cadjust' local statnew = `num'

	*check if null column is specified; create if scalar
	local nullmat = 0
	if regexm(`"`stat'"', "^\.[a-z]*")  {
		if !`bstats' & `"`matrix'"'=="" {
			matrix `INtemp`num'' = J(1,1,`stat')
			matrix rownames `INtemp`num'' = "----"
			matrix colnames `INtemp`num'' = `"`statnew'"'
		}
		local nullmat = 1
	}

	*Set row names for scalars // v172
	*row name = Row# & colname = Return
	*assign row# to row name if not specified
	if  "`ret_type'"=="scalar" {
		local rowname      `"`statnew'"'
		local tmpoutrownms `"`outrownms'"'

		foreach rnm of local tmpoutrownms {
/*v175  if `"`rnm'"' == `"_:_all"' {*/
				if "`rowstart'"!="" local addrow = `rowstart'
				else local addrow = `tmpoutrownum' + 1
				local rowname "Row`addrow'"
				local outrownms `"`outrownms' `rowname'"'

				if `"`scalarRow'"'!="" local rowname `"`scalarRow'"'

				*set sreturn for future scalars
				sreturn local srowname `"`rowname'"'
/*			} */
		}
	}

	*get scalar returns
	if !`nullmat' & "`ret_type'"=="scalar" & !`uvalues' {
		*does return exist?
		capture confirm matrix `ret'(`stat')
		if  _rc local ismat = 0
		if !_rc local ismat = 1

		*allow r() to grab 1x1 matrices, e.g., from nlcom: v172
		local toobig = 0
		if `ismat'              local toobig = rowsof(`ret'(`stat'))
		if `ismat' & `toobig'<2 local toobig = colsof(`ret'(`stat'))
		if `ismat' & `toobig'<2 local toobig = 0

		capture local numtest = `ret'(`stat')
		capture confirm number `numtest'
		if  _rc local retexists = 0
		if !_rc local retexists = 1

		*create statistics for various test commands
		local rtest = 0
		if inlist(`"`stat'"', "b", "est", "estimate", "or", "expb", "level") |       inlist(`"`stat'"', "lb", "p", "t", "df", "ub","z") {
			_cllct_addRTESTS, isT(`isT') isZ(`isZ') df(`df')        ///
                              level(`level') retexists(`retexists') ///
							  return(`ret'(`stat')) er_ret(`ret')
			local rtest = 1
		}

		*error if r() or e() is matrix or doesn't exist
		if `ismat' & `toobig' {
			di as error ///
			"`ret'(`stat') is a matrix: Please use ebreturns(), rbreturns(), or matrix()."
			exit 198
		}
		if !`ismat' & !`retexists' & !`rtest' {
			di as error ///
			"`ret'(`stat') is not in returns"
			exit 198
		}

		if  `rtest' matrix `INtemp`num'' =    `s(`stat')'
		if !`rtest' matrix `INtemp`num'' = `ret'(`stat')
		if  "`stat'"=="df" | "`stat'"=="df_r" {
			matrix `INtemp`num'' = `ret'(`stat')
		}

		matrix colnames `INtemp`num'' = `stat'
		matrix rownames `INtemp`num'' = `rowname'
	} // End Scalar Returns

	*Prepare User-defined values
	if `uvalues' & !`nullmat'{
		capture confirm number `stat'
		if  _rc local unum = 0
		if !_rc local unum = 1

		if !`unum' capture confirm scalar `stat'
		if !_rc local snum = 1
		if  _rc local snum = 0

		if !`unum' & !`snum' {
			di as error ///
			"Uservalues() can only contain numbers"
			exit 198
		}
		else {
			matrix `INtemp`num''          = `stat'
			matrix colnames `INtemp`num'' = `statnew'
			matrix rownames `INtemp`num'' = `rowname'
		}
	}

	*check if row name is a single number
	*need to grab that row number but not name the row with number
	if  "`ret_type'"=="scalar" {
		if `num' == 1 {
			local rwcheck: subinstr local INrowNMS "_:" ""
			local rwcheck: subinstr local rwcheck "_:_all" ""
			capture confirm integer number `rwcheck'
			if !_rc local rwskip =1
			if  _rc local rwskip =0
		}

		if `inrowcount'==1 & `outrowcount'==1 & `"`INrowNMS'"'==`"`OUTrowNMS'"' & `"`INrowNMS'"'!=" _:_all" & `"`OUTrowNMS'"'!=" _:_all" /* & !`rwskip' v174*/ {
			*trim space at the end
			local OUTrowNMS = trim(`"`OUTrowNMS'"')
			local INrowNMS  = trim(`"`INrowNMS'"')
			matrix colnames `INtemp`num'' = `"`INrowNMS'"'
			matrix rownames `INtemp`num'' = `"`OUTrowNMS'"'
		}

	/* v174: fix u() row(4) error
		*if row name is a number I need to replace with rownames
		if `rwskip' {
			if `num' == 1 {
				local INrowNMS  ""
				local OUTrowNMS ""
			}
			local INrowNMS  "`INrowNMS' _:`rowname'"
			local OUTrowNMS "`INrowNMS' _:`rowname'"
		} */
	}

	*Prepare matrices
	if `"`matrix'"'!= "" &  !`nullmat' {
		*check if stat is the col number
		*if it is a # and not equal to stat then skip column
		capture confirm number `stat'
		if !_rc {
			local grabcol `stat'
			if `stat'!=`stat' continue
		}
		else local grabcol `""`eqnm':`stat'""'

		*change rownames for at matrix to colnames for b matrix
		if "`ret_type'"=="atmatrix" & `matcol'==1 {
			_cllct_removeOMITTED, matrix(r(b)) `nostrip'
			local ATrowNMS : colnames `INmat'
			matrix rownames `INatm' = `ATrowNMS'
			matrix `INmat'          = `INatm'

			*v2.0.4: if at() is 1 row then need to replace row name with one
			*that can be combined with r(b)
			local ChkRowNum = rowsof(`INmat')
			if `ChkRowNum'==1 {
				local INrowNMS     `"_:`ATrowNMS'"'
				local newINrowNMS  `"_:`ATrowNMS'"'
			}
		}

		matrix `INtemp`num''  = `INmat'[1...,`grabcol']
	} // end if matrix

	*get B and V matrices for each equation
	if `bstats' {
		matrix `INtemp`num'' = `INmat'
		matrix `INcov`num''  = `INcov'

		if "`unstack'"!="" {
			matrix `INcov`num''  = `INcov'["`eqnm':","`eqnm':"]
			matrix `INtemp`num'' = `INmat'[1...,"`eqnm':"]
		}

		*check if eb() is specified correctly
		if !inlist("`stat'", "b", "est", "estimate","expb", "or", "level") &  !inlist("`stat'","lb", "ub", "p", "se", "t", "z", "orpct","_all") & !regexm("`stat'","\.[a-z]*") {
			di as error "`stat' is not allowed"
				exit 198
		}
	}

	*use mata to change equation, column, & row names to new names
	if !`bstats' {
		mata: `INtempCNM'          = st_matrixcolstripe("`INtemp`num''")
		mata: `INtempCNM'[1,(1,2)] = ("`eqnmnew'","`statnew'")
		mata: st_matrixcolstripe("`INtemp`num''", `INtempCNM')
		matrix `INmat`num'' = `INtemp`num''
	}

	*create stats specified by eb or rb option
	if `bstats' {
		if `nullmat' {
			local trash : colnames `INtemp`num'', quoted
			local rnum  : word count `trash'
			forvalues i  = 1/`rnum' {
				mat `INtemp`num''[1,`i'] = `stat'
			}
		}

		*if unstack, remove equations from column and add to row
		if "`unstack'"!="" {
			mat coleq `INtemp`num'' = ""
			mat roweq `INtemp`num'' = `"`eqnm'"'
		}

		mata: `INtempCNM'          = st_matrixcolstripe("`INtemp`num''")
		mata: `INtempRNM'          = st_matrixrowstripe("`INtemp`num''")
		mata: `INtempRNM'[1,(1,2)] = ("`eqnmnew'","`statnew'")
		mata: `INmat'              = st_matrix("`INtemp`num''")
		mata: `INcov'              = st_matrix("`INcov`num''")

		*get column numbers of incoming matrix
		mata: st_numscalar("`INscalar'", cols(`INmat'))
		local tmpcolnum        = `INscalar'

		*calculate statistics and add to matrix
		forvalues i = 1(1)`tmpcolnum' {
			if "`stat'"== "z" | "`stat'"=="t" {
				mata: `INmat'[,`i']=`INmat'[1,`i']/sqrt(`INcov'[`i',`i'])
				mata:  st_numscalar("`INscalar'", (`INmat'[1,`i']/sqrt(`INcov'[`i',`i'])))
			}
			if "`stat'"== "ub" | "`stat'"=="lb" {
					if `isT' scalar `cifactorval' = abs(invttail(`df',`levelpval'))
					if !`isT' scalar `cifactorval' = abs(invnormal(`levelpval'))
				if "`stat'"== "lb" {
					mata: `INmat'[,`i']=`INmat'[1,`i'] - (st_numscalar("`cifactorval'")*sqrt(`INcov'[`i',`i']))
				}
				if "`stat'"== "ub" {
					mata: `INmat'[,`i']=`INmat'[1,`i'] + (st_numscalar("`cifactorval'")*sqrt(`INcov'[`i',`i']))
				}
			}
			if "`stat'"=="p" {
				if `isT' {
					mata:  st_numscalar("`INscalar'", (`INmat'[1,`i']/sqrt(`INcov'[`i',`i'])))
					scalar `cifactorval' = abs(invttail(`df',`levelpval'))
					scalar `p' = 2*ttail(`df',abs(`INscalar'))
				}
				if !`isT' {
					mata:  st_numscalar("`INscalar'", (`INmat'[1,`i']/sqrt(`INcov'[`i',`i'])))
					scalar `cifactorval' = abs(invnormal(`levelpval'))
					scalar `p' = 2*(1-normal(abs(`INscalar')))
				}
				mata: `INmat'[,`i']=st_numscalar("`p'")
			}
			*get actual stats
			if "`stat'"== "se" {
				mata: `INmat'[,`i'] = sqrt(`INcov'[`i',`i'])
			}
			if "`stat'"== "exp" | "`stat'"=="or" {
				mata: `INmat'[,`i'] = exp(`INmat'[1,`i'])
			}
			if "`stat'"== "orpct" {
				mata: `INmat'[,`i'] = 100*(exp(`INmat'[1,`i'])-1)
			}
			if "`stat'"== "level" {
				mata: `INmat'[,`i'] = `level'
			}

			*replace matrix in stata with new stats column
			if `i' == `tmpcolnum' {
				mata: st_matrix("`INtemp'", `INmat')
			}
		} // end forvalues loop

		*Add column/row names and equations and transpose
		mata: st_matrixcolstripe("`INtemp'", `INtempCNM')
		mata: st_matrixrowstripe("`INtemp'", `INtempRNM')

		*transpose matrices from b returns
		matrix define `INmat`num'' = (`INtemp')'
	} // end bstats

	local matnum "`matnum' `num'"

} // end returns loop

//  #5
//  Prep For Incoming Rows

*check if all rows wanted
local newINrowNMS: subinstr local INrowNMS "_:_all" "", count(local _allrows)
local _allrows = 0
local newOUTrowNMS: subinstr local OUTrowNMS "_:_all" "", count(local _allrows)

*check if eq:row=_all is specified
*use this local when adjusting rows
local nmrcount : list sizeof local INrowNMS

*count the number of ":"s, to find out how many rows user wants
local trash : subinstr local newINrowNMS ":" "", all count(local rowcount)

*check to see if more than 1 row specified
local inrownums : list sizeof local newINrowNMS

if `inrownums'>1 {
	local eq2names  = regexm(`"`INrowNMS'"',`"^"[ ]*[a-zA-Z1-9]+[ ][a-zA-Z1-9]+[ ]*":[a-zA-Z1-9]+[ ]*$"')
	if `eq2names' local inrownums = 1
}

if `inrownums' {
	forvalues mat_num = 1(1)`rowcount' {
		local reqnm    ""
		local inreqnm  ""
		local reqnmnew ""
		local outreqnm ""
		local instop  = 0
		local outstop = 0

		while !`instop' {
			gettoken inreqnm newINrowNMS : newINrowNMS, parse(":")
			local reqnm "`reqnm' `inreqnm'"
			local instop = regexm("`reqnm'", ":")
			local reqnm  : subinstr local reqnm ":" ""
			local reqnm  = trim(`"`reqnm'"')
			if `"`newINrowNMS'"' == "" local instop = 1
		}

		while !`outstop'  {
			gettoken outreqnm newOUTrowNMS: newOUTrowNMS, parse(":")
			local reqnmnew "`reqnmnew' `outreqnm'"
			local outstop  = regexm("`reqnmnew'", ":")
			local reqnmnew : subinstr local reqnmnew ":" ""
			local reqnmnew = trim(`"`reqnmnew'"')
			if `"`newOUTrowNMS'"' == `""' local outstop = 1
		}

		*compile parsed row names and equations
		gettoken rnm newINrowNMS: newINrowNMS
		local    rnm   : subinstr local rnm   `":"' "", all
		local    rnm   : subinstr local rnm   `"""' "", all
		local    reqnm : subinstr local reqnm `":"' "", all
		local    reqnm : subinstr local reqnm `"""' "", all
		local    inreq `"`inreq' "`reqnm'""'
		local    inrnm `"`inrnm' "`rnm'""'

		gettoken rnmnew newOUTrowNMS: newOUTrowNMS
		local    rnmnew   : subinstr local rnmnew   `":"' "", all
		local    rnmnew   : subinstr local rnmnew   `"""' "", all
		local    reqnmnew : subinstr local reqnmnew `":"' "", all
		local    reqnmnew : subinstr local reqnmnew `"""' "", all
		local    outreq `"`outreq' "`reqnmnew'""'
		local    outrnm `"`outrnm' "`rnmnew'""'
	}
} // End get row and new row information

//  #6
//  revise rows of incoming matrices

foreach i of local matnum {
	*if user has specified eq:row=_all, need to make special adjustments
	*so that eq:row becomes a stub for row# ; `radjust' is for matrices
	if `"`rowbackup'"'!="" | `radjust' {
		local newrownms ""
		local inrownum  = rowsof(`INmat`i'')
		gettoken newout : rowbackup, parse("=")

		forvalues rn = 1(1)`inrownum' {
			if regexm(`"`rowbackup'"', ":") {
				gettoken newouteq temp: rowbackup, parse(":")
				gettoken newoutnm : temp, parse("=")
				local    newoutnm : subinstr local newoutnm ":" ""
				if !`radjust'  local newrownms `"`newrownms' `""`newouteq'":`newoutnm'`rn'"'"'
				if  `radjust'  local newrownms `"`newrownms' `rn'"'
			}
			else {
				gettoken newout : rowbackup, parse("=")
				if !`radjust'  local newrownms `"`newrownms' `newout'`rn'"'
				if  `radjust'  local newrownms `"`newrownms' `rn'"'
			}
		}
		mat rownames `INmat`i'' = `newrownms'
	}

	if `"`rowbackup'"'=="" {
		mata: `INmat`i''   = st_matrix("`INmat`i''")
		mata: `INtemp'     = J(rows(`INmat`i''),cols(`INmat`i''),0)
		mata: `INtempRNM'  = st_matrixrowstripe("`INmat`i''")
		mata: `INtempCNM'  = st_matrixcolstripe("`INmat`i''")

		local roweq        : roweq    `INmat`i'', quoted
		local rownames     : rownames `INmat`i'', quoted
		local rownums      = rowsof(`INmat`i'')
		local keeprow      = 0
		local anykeep      = 0

		*Need to loop through all row names to see which to keep & drop
		*Also, compare user-specified names to names in incoming matrix
		forvalues innum = 1(1)`rowcount' {

/* v172: comment out
			*need to make some adjustments to add scalar returns
			if `inrownums'==1  & "`ret_type'"=="scalar" {
				local inreq : roweq    `INmat`i'', quoted
				local inrnm : rownames `INmat`i'', quoted
				local _allrows = 1
			}
*/

			local innm  : word `innum' of `inrnm'
			local ineq  : word `innum' of `inreq'
			local outnm : word `innum' of `outrnm'
			local outeq : word `innum' of `outreq'

			*find the correct row and give new name
			forvalues outnum = 1(1)`rownums' {
				local rowname   : word `outnum' of `rownames'
				local eqrowname : word `outnum' of `roweq'

  		if `"`ineq':`innm'==`eqrowname':`rowname'"'==`"_:1._at==_:1bn._at"' {
				local innm "1bn._at"
			}

				*adjust if user does not specify equation name
				if (`"`ineq'"'=="_" & `"`innm'"'!="") | "`noINeq'"=="1" {
					local ineq  `"`eqrowname'"'
					local noINeq = 1
					if `"`outeq'"'=="_" {
						local outeq   `"`eqrowname'"'
						local noOUTeq = 1
					}
				}

				if `"`ineq':`innm'"'== `"`eqrowname':`rowname'"' {
					mata: `INtempRNM'[`outnum',1] = "`outeq'"
					mata: `INtempRNM'[`outnum',2] = "`outnm'"
					mata: `INtemp'[`outnum',1]    = 1

					local keeprow = 1
				}
				else {
					if !`_allrows' local keeprow = 0
					if  `_allrows' local keeprow = 1
				}
				local anykeep = `anykeep'+`keeprow'
			}
		}

		mata: st_matrixrowstripe("`INmat`i''",`INtempRNM')

		*drop column if no row is specified
		if !`anykeep' & !`_allrows' {
			local ++ kpnum
			if `kpnum'==1 local new_matnum "`matnum'"
			local new_matnum : subinstr local new_matnum "`i'" ""
		}
		if  `anykeep' & !`_allrows' {
			mata: st_matrix("`INmat`i''", select(`INmat`i'', `INtemp'))


			mata: st_matrixrowstripe("`INmat`i''", select(`INtempRNM', `INtemp'))
			mata: st_matrixcolstripe("`INmat`i''", `INtempCNM')
		}
	} // end else statement
} // append rows of incoming matrix

*if every row of incoming column is dropped, display error
if "`new_matnum'"!="0" local matnum "`new_matnum'"
local anymatrices : list sizeof local matnum
if "`anymatrices'"=="0" {
	di as error ///
	"You are dropping every incoming row of a return. Check row()."
	exit 198
}

//  #7
//  Add Labels from r(at) Values If Specified

//  #5
//  Remove missing columns from at matrx

if `"`ret_type'"'=="atmatrix" | "`atlabel'"!="" {
	local new_matnum `"`matnum'"'
	foreach i of local matnum {
		mata: `INmat' = st_matrix("`INmat`i''")

		*find out where missing columns are located
		tempname missmat`i'
		mata:    `missmat`i''   = colmissing(`INmat')

		*get matrix size
		local  INmatrNUM  = rowsof(`INmat')
		local  INmatcNUM  = colsof(`INmat')

		*create a matrix in stata to stripe mata matrix
		mata:  st_matrix("`missmat'", `missmat`i'')
		mat   `tmpmat'  = J(1,`INmatcNUM',1)

		local colmiss = `missmat'[1,1]

		if `colmiss' == `INmatrNUM' local new_matnum : subinstr local new_matnum "`i'" ""
	}

	*update matnum
	local matnum `"`new_matnum'"'

	*get at matrix size
	local  INmatrNUM  = rowsof(`INatm')
	local  INmatcNUM  = colsof(`INatm')

	foreach i of local matnum {
		* Create ATlable Matrix to Add Labels from r(at)
		*if r(at) exists remove missing columns

		local matcolnms   : colnames `INatm'

		*Create matrix row labels from at values
		local labpos ""
		foreach lab of local atlabel {
			local pos : list posof "`lab'" in matcolnms

			*give error if atlabel name isn't in r(at) column names
			if "`pos'" == "" {
				di as error ///
				"Column `lab' is not in r(at) matrix"
				exit 198
			}

			local labpos "`labpos' `pos'"
		}


		mata: `ATlabel' = J(`INmatrNUM', 2,"")

		forvalues j = 1/`INmatrNUM' {
			local labhold `"`atlabel'"'
			local lpnum = 0
			foreach l of local labpos {
				local ++lpnum
				gettoken labnm labhold : labhold, parse(" ")
				local b = `INatm'[`j',`l']
				local b : di %9.3g `b'

				*error if . in label
				if regexm("`b'", "\.") {
					di as error ///
					`"Label `labnm' contains "." and is not allowed in matrix row names"'
					exit 198
				}

				local b = trim("`b'")
				if `lpnum'==1 local a `"`labnm'_`b'"'
				if `lpnum' >1 local a `"`a' `labnm'_`b'"'
			}

			local lablgth : length local a
			if `lablgth'<12 local lablgth=12
			mata: `ATlabel'[`j',2] = `"`a'"'
		}
	}
}

*update labels
if `"`atlabel'"'!= "" {
	mata: `ATlabel' = select(`ATlabel', `INtemp') // v202
	foreach i of local matnum {
		mata: st_matrixrowstripe("`INmat`i''", `ATlabel')
	}
}

//  #8
//  Add stubs

if "`rowstub'"!= "" | "`colstub'"!= "" | "`retstub'"!=""  {
	foreach i of local matnum {
		local rownames : rownames `INmat`i''
		local cnm      : colnames `INmat`i''
		mata: `INtempRNM' = st_matrixrowstripe("`INmat`i''")
		mata: `INtempCNM' = st_matrixcolstripe("`INmat`i''")

		*parse out equation names
		if regexm(`"`rowstub'"', ":") {
			gettoken erstub kpparsing : rowstub  , parse(":")
			gettoken trash  rowstub   : kpparsing, parse(":")
		}

		if regexm(`"`colstub'"', ":") {
			gettoken ecstub kpparsing : colstub  , parse(":")
			gettoken trash  colstub   : kpparsing, parse(":")
		}

		*add row stubs
		local rnum = 0
		foreach rnm  of local rownames {
			local ++rnum
			if `"`erstub'"'!="" | "`erstub'"'!="_" {
				mata: `INtempRNM'[`rnum',1] = "`erstub'"
			}
			mata: `INtempRNM'[`rnum',2] = "`rowstub'`rnm'"
		}

		*add column stubs
		if `"`ecstub'"'!="" | "`ecstub'"'!="_" {
			mata: `INtempCNM'[1,1] = "`ecstub'"
		}
		mata: `INtempCNM'[1,2] = "`colstub'`retstub'`cnm'"

		*replace old names in stata with new names in mata
		mata: st_matrixrowstripe("`INmat`i''", `INtempRNM')
		mata: st_matrixcolstripe("`INmat`i''", `INtempCNM')
	}
}

//  #9
//  Add new row equations to each incoming matrix

if `"`singleroweq'"' != "" {
	foreach i of local matnum {
		local rownum = rowsof(`INmat`i'')
		mata: `INtempRNM' = st_matrixrowstripe("`INmat`i''")

		forvalues r = 1(1)`rownum' {
			mata: `INtempRNM'[`r',1] = `"`singleroweq'"'
		}
		mata: st_matrixrowstripe("`INmat`i''", `INtempRNM')
	}
}

//  #10
//  Combine and List the new columns if listnew specified

if "`listnew'" != "" {
	foreach i of local matnum {
		local insertCOL = 0

		if `i' == 1 matrix `INmat' =`INmat`i''
		else {
			local colNM`i'   : colnames `INmat`i''
			local rowNM`i'   : rownames `INmat`i''
			*since incoming is col vector, only 1 eq, so no quotes needed
			local  eqNM`i'   : coleq    `INmat`i''
			local rowNUM`i'  : list sizeof local rowNM`i'

			local INmatcolNMS : colnames `INmat'
			*possible for mult eq so quotes needed; removed below
			local INmateqNMS  : coleq    `INmat', quoted
			local INmatcolNUM : list sizeof local INmatcolNMS

			forvalues colnum = 1(1)`INmatcolNUM' {
				local incolnm : word `colnum' of `macval(INmatcolNMS)'
				local ineqnm  : word `colnum' of `macval(INmateqNMS)'
				local inmatnm  "`ineqnm':`incolnm'"
				local tmpmatnm "`eqNM`i'':`colNM`i''"
				if  `"`inmatnm'"' == `"`tmpmatnm'"' {
					local insertCOL = `colnum'
				}
			}

			if `insertCOL' {
				*create matrix with correct dimensions
				mata: `INtemp'    = st_matrix("`INmat`i''")
				mata: `INtempRNM' = st_matrixrowstripe("`INmat`i''")
				mata: `INmat`i''     = J(rows(`INtemp'), `INmatcolNUM',`mv')
				forvalues rownum = 1(1)`rowNUM`i'' {
					mata:`INmat`i''[`rownum',`insertCOL']=`INtemp'[`rownum',1]
				}
				mata: st_matrix("`INmat`i''",`INmat`i'')
				mata: st_matrixrowstripe("`INmat`i''", `INtempRNM')
				matrix `INmat' = `INmat' \ `INmat`i''
			}
			if !`insertCOL' {
				_cllct_mat_capp `INmat' : `INmat' `INmat`i'', miss(`mv')
			}
		} // end else
	} // forvalues i = 1(1)`num'

	matlist `INmat', nodotz border(top bottom) title("`title'") format(%9.4g)

} // End listnew

//  #11
//  Adjust INmat & OUTmat if rowstart or colstart specified

*adjust rowstart/colstart if only 1 specified
if "`rowstart'"!="" & "`colstart'"== ""  {
	if `mat_exist' {
		local colstart = colsof(`cllctmat') + 1
		local newcolstart = `colstart'
	}
	if !`mat_exist' {
		local colstart = 1
		local newcolstart = `colstart'
	}
}
if "`rowstart'"=="" & "`colstart'"!="" {
	if `mat_exist' {
		local rowstart = rowsof(`cllctmat')+1
	}
	if !`mat_exist' {
		local rowstart = 1
	}
}

if "`colstart'"!="" | "`rowstart'"!="" {
	if "`colstart'"!="" local newcolstart = `colstart'-1

	*recombine incoming col vectors into INmat
	local mcount = 0
	foreach i of local matnum {
		local ++mcount
		*set cllctmat to OUTmat if it exists
		if `mcount'==1   {
			if `mat_exist' matrix  `OUTmat' = `cllctmat'
			matrix  `INmat' = `INmat`i''
		}

		if `mcount' > 1 {
			_cllct_mat_capp `INmat' : `INmat' `INmat`i'', miss(`mv')
		}

	}

	*add col# and row# if row|colstart> outmat row|col
	local rowstop = `rowstart'-1  // where OUTmat stops
	local colstop = `colstart'-1

	if !`mat_exist' {
		local incolfnms : colfullnames `INmat', quoted
		local inrowfnms : rowfullnames `INmat', quoted

		local rnum = rowsof(`INmat')
		local cnum = colsof(`INmat')

		if `rowstart' > 1 {
			forvalues r = 1(1)`rowstop' {
				matrix `INtemp'          = J(1,`cnum',`mv')
				matrix colnames `INtemp' = `incolfnms'
				matrix rownames `INtemp' = Row`r'

				if `r'==1 matrix `INtempmat'    = `INtemp'
 				if `r'> 1 matrix `INtempmat'    = `INtempmat' \ `INtemp'
			}
			matrix `INmat'  = `INtempmat' \ `INmat'
			matrix `OUTmat' = `INmat'

			*update locals
			local rnum = rowsof(`INmat')
			local cnum = colsof(`INmat')
			local incolfnms : colfullnames `INmat', quoted
			local inrowfnms : rowfullnames `INmat', quoted
		}

		if `colstart' > 1 {
			forvalues c = 1(1)`colstop' {
				matrix `INtemp' = J(`rnum',1,`mv')
				matrix rownames `INtemp' = `inrowfnms'
				matrix colnames `INtemp' = Col`c'

				if `c'==1 matrix `INtempmat'    = `INtemp'
 				if `c'> 1 matrix `INtempmat'    = `INtempmat' , `INtemp'
			}
			matrix `OUTmat' = `INtempmat' , `INmat'
		}

		if `rowstart'==1 {
			local rowstart ""
		}
		if `colstart'==1 {
			local colstart ""
		}

	} // end if !`mat_exist'

	if `mat_exist' {
		local outrownum = rowsof(`OUTmat')
		local outcolnum = colsof(`OUTmat')
		local inrownum  = rowsof(`INmat')
		local incolnum  = colsof(`INmat')
		local matover   = 0

		*create error for trying to write over cells with info
		if `colstart'<=`outcolnum' & `rowstart'<=`outrownum' {
			local coladd = `colstart' - `outcolnum' + `incolnum' - 1
			local rowadd = `rowstart' - `outrownum' + `inrownum' - 1

			if `coladd'<0 local coladd = 0
			if `rowadd'<0 local rowadd = 0

			local totcol = `outcolnum' + `coladd'
			local totrow = `outrownum' + `rowadd'

			*create matrix to populate
			matrix `INtemp' = J(`totrow',`totcol',`mv')

			*create row/col names
			local outrownames : rowfullnames `OUTmat'
			local outcolnames : colfullnames `OUTmat'
			local inrownames  : rowfullnames `INmat'
			local incolnames  : colfullnames `INmat'
			local cnnum = 0
			local chold  ""
			local cnhold ""
			local rnnum = 0
			local rhold  ""
			local rnhold ""

			forvalues j = 1/`outcolnum' {
				forvalues i = 1/`outrownum'  {
						mat `INtemp'[`i',`j'] = `OUTmat'[`i',`j']
				}
			}

			forvalues i = `colstart'/`totcol' {
				local ++cnnum
				if `i'>`outcolnum' {
					local chold : word `i' of `incolnames'
					local cnhold `"`cnhold' `"`chold'"'"'
				}
			}
			forvalues i = `rowstart'/`totrow' {
				local ++cnnum
				if `i'>`outrownum' {
					local rhold : word `i' of `inrownames'
					local rnhold `"`rnhold' `"`rhold'"'"'
				}
			}

			*populate matrix
			local csnum = 0
			forvalues j = `colstart'/`totcol' {
				local ++csnum
				local rsnum = 0

				forvalues i = `rowstart'/`totrow'  {
					local ++rsnum
					local cellchk = `INtemp'[`i',`j']

					capture confirm number `cellchk'
					if !_rc & !`WARNinfo' {
						di as error ///
						"Warning: You are replacing information in your matrix"
						local ++WARNinfo
					}

					if `rsnum'<=`inrownum' & `csnum'<=`incolnum' {
						mat `INtemp'[`i',`j'] = `INmat'[`rsnum',`csnum']
					}
				}
			}

			*update where columns will start for next set of matrices
			local colstart  = `colstart' + `incolnum' + 1

			matrix `OUTmat' = `INtemp'
			matrix rownames `OUTmat' = `outrownames' `rnhold'
			matrix colnames `OUTmat' = `outcolnames' `cnhold'

			local matover = 1
		}  // end if `colstart'<=`outcolnum' & `rowstart'<=`outrownum'

		if !`matover' {
			* start value of loops below
			local rstart  = `outrownum'+1
			local cstart  = `outcolnum'+1

			*if adding more rows/columns than exists, add "Row#" or "Col#"
			if `rowstop' > `outrownum' {
				local outcolnum  = colsof(`OUTmat')
				local outcolfnms : colfullnames `OUTmat', quoted

				forvalues r = `rstart'(1)`rowstop' {
					matrix `INtemp' = J(1,`outcolnum',`mv')
					matrix colnames `INtemp' = `outcolfnms'
					matrix rownames `INtemp' = Row`r'
					matrix `OUTmat' = `OUTmat' \ `INtemp'
				}
			}
			if `colstop' > `outcolnum' {
				local outrownum  = rowsof(`OUTmat')
				local outrowfnms : rowfullnames `OUTmat', quoted
					matrix `INtemp' = J(`outrownum',1,`mv')
					matrix rownames `INtemp' = `outrowfnms'

				forvalues c = `cstart'(1)`colstop' {
					matrix colnames `INtemp' = Col`c'
				}
					matrix `OUTmat' = `OUTmat' , `INtemp'
			}

			*update locals
			local outrownum = rowsof(`OUTmat')
			local outcolnum = colsof(`OUTmat')
			local inrownum  = rowsof(`INmat')
			local incolnum  = colsof(`INmat')

			*prep mata
			mata: `INmat'      = st_matrix("`INmat'")
			mata: `OUTmat'     = st_matrix("`OUTmat'")
			mata: `OUTtempRNM' = st_matrixrowstripe("`OUTmat'")
			mata: `OUTtempCNM' = st_matrixcolstripe("`OUTmat'")
			mata: `INtempRNM'  = st_matrixrowstripe("`INmat'")
			mata: `INtempCNM'  = st_matrixcolstripe("`INmat'")

			if `rowstart' > `outrownum'	& `colstart'>`outcolnum' {
				local rowtot    = `outrownum' + `inrownum'
				local coltot    = `outcolnum' + `incolnum'
				mata: `INtemp'  = J(`inrownum',`outcolnum',`mv')
				mata: `INmat'   = `INtemp' , `INmat'
				mata: `INtemp'  = J(`outrownum',`coltot',`mv')
				mata: `INmat'   = `INtemp' \ `INmat'
			}

			*need locals for below
			local rplus    = `inrownum'  -  (`outrownum' - `rowstop')
			local cplus    = `incolnum'  -  (`outcolnum' - `colstop')
			local incstart = `incolnum'  - `cplus' + 1
			local inrstart = `inrownum'  - `rplus' + 1
			local rowtot   = (`outrownum' + `rplus')
			local rowadd   = `rowtot'    - `inrownum'
			local coltot   = `outcolnum' + `cplus'
			local coladd   = `coltot'    - `incolnum'

			if `rowstart' <= `outrownum' {
				if `rplus' > 0 {
					mata: `INtemp'     = J(`inrownum', `outcolnum',`mv')
					mata: `INmat'      = `INtemp' , `INmat'
					mata: `INtemp'     = J(`rowadd',`coltot',`mv')
					mata: `INmat'      = `INtemp' \ `INmat'
					mata: `INtempRNM'  = `INtempRNM'[`inrstart'::`inrownum',]
					mata: `INtempCNM'  = `INtempCNM'[`incstart'::`incolnum',]
				}

				if `rplus' <= 0 {
					local rowbot   = `outrownum'-(`rowstop' + `inrownum')
					local rowtot   = `outrownum'
					local coltot   = `outcolnum' + `incolnum'
					mata: `INtemp' = J(`inrownum', `outcolnum',`mv')
					mata: `INmat'  = `INtemp' , `INmat'
					mata: `INtemp' = J(`rowstop', `coltot',`mv')
					mata: `INmat'  = `INtemp' \ `INmat'

					if `rowbot' > 0 {
						mata: `INtemp' = J(`rowbot', `coltot',`mv')
						mata: `INmat'  =  `INmat' \ `INtemp'
					}
				}
			} // end if rowstart' <= `outrownum'

			if `colstart' <= `outcolnum' {
				if `cplus' > 0 {
					mata: `INtemp'     = J(`outrownum', `incolnum',`mv')
					mata: `INmat'      = `INtemp' \ `INmat'
					mata: `INtemp'     = J(`rowtot',`colstop',`mv')
					mata: `INmat'      = `INtemp' , `INmat'
					mata: `INtempRNM'  = `INtempRNM'[`inrstart'::`inrownum',]
					mata: `INtempCNM'  = `INtempCNM'[`incstart'::`incolnum',]
				}

				if `cplus' <= 0 {
					local colbot   = `outcolnum'-(`colstop' + `incolnum')
					local coltot   = `outcolnum'
					local rowtot   = (`outrownum' + `rplus')
					mata: `INtemp' = J(`outrownum', `incolnum',`mv')
					mata: `INmat'  = `INtemp' \ `INmat'

					mata: `INtemp' = J(`rowtot', `colstop',`mv')
					mata: `INmat'  = `INtemp' , `INmat'

					if `colbot' > 0 {
						mata: `INtemp' = J(`rowtot', `colbot',`mv')
						mata: `INmat'  =  `INmat' , `INtemp'
					}
				}
			} // end if `colstart' <= `outcolnum'

			*fill in mata INmat with OUTmat values
			forvalues c = 1(1)`outcolnum' {
				forvalues r  = 1(1)`outrownum' {
					mata: `INmat'[`r',`c'] = `OUTmat'[`r',`c']
				}
			}

			*append col/row names
			if `rplus' > 0 & `cplus'>0 {
				mata: `OUTtempRNM'  = `OUTtempRNM' \ `INtempRNM'
				mata: `OUTtempCNM'  = `OUTtempCNM' \ `INtempCNM'
			}

			if `rplus' <= 0 {
				mata: `OUTtempRNM'  = `OUTtempRNM'
				mata: `OUTtempCNM'  = `OUTtempCNM' \ `INtempCNM'
			}

			if `cplus' <= 0 {
				mata: `OUTtempRNM'  = `OUTtempRNM' \ `INtempRNM'
				mata: `OUTtempCNM'  = `OUTtempCNM'
			}

			*send back to Stata
			mata: st_matrix("`OUTmat'",`INmat')
			mata: st_matrixrowstripe("`OUTmat'",`OUTtempRNM')
			mata: st_matrixcolstripe("`OUTmat'",`OUTtempCNM')
		}
	} // end if `mat_exist'
} // end row/col placement

//  #12
//  compile matrices to OUTmat (if rowstart & colstart not specified)

local skip     = 0

if "`colstart'"=="" & "`rowstart'"=="" {
	local mcount = 0
	foreach i of local matnum {
		local ++mcount
		matrix `INmat' =`INmat`i''

		*if cllctmat does not exist, first INmat matrix becomes OUTmat
		*if cllctmat exists, cllctmat becomes OUTMat
		if `mcount'==1 & `mat_exist'  {
				matrix  `OUTmat' = `cllctmat'
		}
		if `mcount'==1 &  !`mat_exist' {
			matrix  `OUTmat' = `INmat'
		}
		mata: `INmat'  = st_matrix("`INmat'")
		mata: `OUTmat' = st_matrix("`OUTmat'")

		if (`mcount'>1 | `mat_exist') {
			local inrow        ""
			local outrow       ""
			local outcol       ""
			local incolnm      : colnames `INmat'
			local inceqnm      : coleq    `INmat'
			local INmatrowNMS  : rownames `INmat', quoted
			local INmatreqNMS  : roweq    `INmat', quoted
			local OUTmatcolNMS : colnames `OUTmat', quoted
			local OUTmatrowNMS : rownames `OUTmat', quoted
			local OUTmatceqNMS : coleq    `OUTmat', quoted
			local OUTmatreqNMS : roweq    `OUTmat', quoted
			local incolnum     : list sizeof local incolnm
			local INmatrowNUM  : list sizeof local INmatrowNMS
			local OUTmatrowNUM : list sizeof local OUTmatrowNMS
			local OUTmatcolNUM : list sizeof local OUTmatcolNMS
			local incmatnm    `"`inceqnm':`incolnm'"'
			local rowcount     ""

			*check to see if column name already exists in OUTmat
			*if so, place contents in that column
			forvalues outcolN = 1(1)`OUTmatcolNUM' {
				local outcolnm  : word `outcolN' of `OUTmatcolNMS'
				local outceqnm  : word `outcolN' of `OUTmatceqNMS'
				local outcmatnm  `"`outceqnm':`outcolnm'"'

				if  `"`outcmatnm'"' == `"`incmatnm'"' {
						local outcol = `outcolN'

						*remove duplicates // v179
						local outcol : list uniq outcol
				}
			}

			*check to see if row name already exists in OUTmat
			*if so place contents in that row
			forvalues outrowN = 1(1)`OUTmatrowNUM' {
				local outrownm : word `outrowN' of `OUTmatrowNMS'
				local outreqnm : word `outrowN' of `OUTmatreqNMS'
				local outrmatnm `"`outreqnm':`outrownm'"'

				forvalues inrowN = 1(1)`INmatrowNUM' {
					local inrownm : word `inrowN' of `INmatrowNMS'
					local inreqnm : word `inrowN' of `INmatreqNMS'
					local inrmatnm `"`inreqnm':`inrownm'"'

					if  `"`outrmatnm'"' == `"`inrmatnm'"' {
						local outrow "`outrow' `outrowN'"
						local inrow  "`inrow' `inrowN'"

						*remove duplicates // v179
						local outrow : list uniq outrow
						local inrow  : list uniq inrow
					}
				}
			}

			*if INmat column is in OUTmat
			if "`outcol'"!="" {
				local innum      = 0
				local infoexists = 0
				local newrnum : list sizeof outrow

				*if INmat row = OUTmat row
				while `"`outrow'"'!="" {
					local ++innum
					gettoken rownum outrow : outrow, parse(" ")

					*I have to adjust newin by rows removed
					local rowcount `"`rowcount' `newin'"'
					local newin  : word `innum' of `inrow'
					local previn ""
					foreach x of local rowcount {
						if `x'<`newin' local previn `"`previn' `x'"'
					}
					local previnnum : list sizeof previn

					*does cell contain a number?
					local rowfull = `OUTmat'[`rownum',`outcol']
					capture confirm number `rowfull'
					if !_rc  {
						local colin = `newin'-`previnnum'
						mata: `OUTmat'[`rownum',`outcol'] = `INmat'[`colin',1]


						if !`WARNinfo' {
							di as error ///
								"Warning: You are replacing information in your matrix"
								local ++WARNinfo
						}

						if `newrnum'>`OUTmatrowNUM'  {
							mata: `REMrow' = J(rows(`INmat'), cols(`INmat'),1)
							mata: `REMrow'[`newin',] = 0
							mata: st_matrix("`INmat'", select(`INmat', `REMrow'))

							mata: st_matrixrowstripe("`INmat'", select(`INtempRNM', `REMrow'))
							mata: st_matrixcolstripe("`INmat'", `INtempCNM')
							mata: `INmat' = st_matrix("`INmat'")
							local NEWrowNUM = rowsof(`INmat')
						}
					}
/* v173: fixed row error
if _rc {
*/
					mata: `OUTmat'[`rownum',`outcol'] = `INmat'[`newin',1]

/*	}*/

					mata: `OUTtempRNM' = st_matrixrowstripe("`OUTmat'")
					mata: `OUTtempCNM' = st_matrixcolstripe("`OUTmat'")
					mata: st_matrix("`OUTmat'", `OUTmat')
					mata: st_matrixrowstripe("`OUTmat'",`OUTtempRNM')
					mata: st_matrixcolstripe("`OUTmat'",`OUTtempCNM')
				} // end while `outrow'

			*if column the same but different rows
			if `INmatrowNUM'>`newrnum'  {

				mata: `INtempRNM'  = st_matrixrowstripe("`INmat'")
				mata: `INtempCNM'  = st_matrixcolstripe("`OUTmat'")

				*if no rows are the same
				if `"`inrow'"'=="" {
					mata: `INtemp'  = J(rows(`INmat'), cols(`OUTmat'),`mv')
					if "`NEWrowNUM'"=="" local NEWrowNUM `"`INmatrowNUM'"'

					forvalues rownum = 1(1)`NEWrowNUM' {
						mata:`INtemp'[`rownum',`outcol']=`INmat'[`rownum',1]
					}

					mata: st_matrix("`INtemp'",`INtemp')
				}

				*if some rows are the same
				if `"`inrow'"'!="" {
					mata: `INtemp' = st_matrix("`INmat'")
					mata: `REMrow' = J(rows(`INtemp'), cols(`INtemp'),1)

					foreach r of local inrow {
						mata: `REMrow'[`r',] = 0
					}

					mata: st_matrix("`INtemp'", select(`INtemp', `REMrow'))
					mata: `INtemp' = st_matrix("`INtemp'")

					local r = rowsof(`INtemp')
					local c = colsof(`OUTmat')

					mata: `INtempmat' = J(`r',`c',.z)

					forvalues r = 1/`r' {
						mata: `INtempmat'[`r',`outcol'] = `INtemp'[`r',1]
					}

					mata: st_matrix("`INtemp'", `INtempmat')

					mata: `INtempRNM' = select(`INtempRNM', `REMrow')
					mata: st_matrixcolstripe("`INtemp'", `INtempCNM')
				}

				mata: st_matrixcolstripe("`INtemp'", `INtempCNM')
				mata: st_matrixrowstripe("`INtemp'", `INtempRNM')

				matrix `OUTmat' = `OUTmat' \ `INtemp'
			}
		}     // end if `outcol'

			*if col is diff than all cols of OUTmat just use _cllct_mat_capp
			if "`outcol'"=="" {
				_cllct_mat_capp `OUTmat' : `OUTmat' `INmat', miss(`mv')
			}
		} // end if (`i'>1 | `mat_exist')
	} // end foreach i of local matnum
} // end if colstart & rowstart == ""

//  #13
//  remove "_" equation names that were used for parsing

local outeqcolnms : coleq `OUTmat', quoted
local outeqrownms : roweq `OUTmat', quoted
foreach eqcol of local outeqcolnms {
	if inlist(`"`eqcol'"', "_") local newouteqcolnms `"`newouteqcolnms' """'
	else local newouteqcolnms `"`newouteqcolnms' "`eqcol'""'
}
foreach eqrow of local outeqrownms {
	if inlist(`"`eqrow'"', "_") local newouteqrownms `"`newouteqrownms' """'
	else local newouteqrownms `"`newouteqrownms' "`eqrow'""'
}

matrix coleq `OUTmat' = `newouteqcolnms'
matrix roweq `OUTmat' = `newouteqrownms'

//  #14
//  create matrix & preview

if "`preview'" == "" matrix `cllctmat' = `OUTmat'
if "`preview'" != "" matlist `OUTmat', nodotz border(top bottom) title("`title'")

*return whether warning was displayed
sreturn local WARNinfo `"`WARNinfo'"'

//  #15
//  End _cllct_appendRET

end

//  #0
//  Program     : _cllct_addEQNMS
//  Author      : Joseph D. Wolfe
//  Description : create sreturns with equation names added
//  Requirements: >=Stata 10 (currently untested on previous versions)

* version 0.0.1 :: 06/26/11 6:22 PM
* version 0.0.2 :: 10/25/11 9:47 AM
* version 0.0.3 :: 10/25/11 9:47 AM; fix coleq bug

//  #1
//  Program define

program _cllct_addEQNMS, sclass

syntax ,                     ///
	[                        ///
	innames(string asis)     ///
	newincol(string asis)    ///
	newoutcol(string asis)   ///
	newinrow(string asis)    ///
	newoutrow(string asis)   ///
	roweq(string asis)       ///
	coleq(string asis)       ///
	u_exist(numlist)         ///
	bstats(numlist)          ///
	unstack					 ///
	]

//  #2
//  Set defaults

if "`rownm'"  =="" local rownm = 0

*to use to make adjustments for uservalues
if "`u_exist'"=="" local u_exist = 0

*check if equations are present in e(b) or r(b) matrix
if "`bstats'"=="1"  {
	local coleqcheck : coleq r(INmat)
	local coleqcheck : word 1 of `coleqcheck'
	if "`coleqcheck'"!="_" local reqparse = 1
}

*see if equations exist in return
if regexm(`"`coleq'"', "=") local ceqparse = 1
if regexm(`"`roweq'"', "=") local reqparse = 1
if "`ceqparse'"=="" local ceqparse = 0
if "`reqparse'"=="" local reqparse = 0

*check if _all is specified for column names and then row names
if `"`coleq'"'!="" & `ceqparse' {
	local _allceq = 0
	local tmpcoleq  : subinstr local coleq "=" " = ", all
	local tmpcolnum : list sizeof local tmpcoleq
	local coleq ""

	*loop through and remove _all if it exists
	forvalues i = 1(1)`tmpcolnum' {
		local nm      : word `i' of `tmpcoleq'
		local _allceq  = strmatch(`"`nm'"', "_all")
		if   !`_allceq' local _allceq  = strmatch(`"`nm'"', "_:_all")
		if   !`_allceq' local coleq `"`coleq' "`nm'""'
	}
}
if `"`roweq'"'!="" & `reqparse' {
	local _allreq = 0
	local tmproweq  : subinstr local roweq "=" " = ", all
	local tmprownum : list sizeof local tmproweq
	local roweq ""
	forvalues i = 1(1)`tmprownum' {
		local nm      : word `i' of `tmproweq'
		local _allreq = strmatch(`"`nm'"', "_all")
		if   !`_allreq' local _allreq  = strmatch(`"`nm'"', "_:_all")
		if   !`_allreq' local roweq `"`roweq' "`nm'""'
	}
}

//  #3
//  Add Error Messages

*Errors for trying to add more than 1 equation name at a time
if `"`coleq'"'!=""  {
	local wdcount : list sizeof coleq

	if !`ceqparse' & `wdcount'>1 {
		di as error ///
		"Invalid syntax in coleq()."

		di as error ///
		"NOTE:"
		di as error ///
		"- You may only add 1 equation at a time in coleq()."
		di as error ///
		"- Use quotes for equation names with 2 or more words."
		exit 198
	}
}
if `"`roweq'"'!=""  {
	local wdcount : list sizeof roweq

	if !`reqparse' & `wdcount'>1 {
		di as error ///
		"You can only add 1 equation at a time in roweq()."
		di as error ///
		"Use quotes for equation names with 2 or more words."
		exit 198
	}
}

*check if too few or too many "=" in column and rows
if `"`coleq'"'!="" & `ceqparse' {
	if regex(`"`coleq'"', "=[ ]*=")  {
		di as error ///
		`"Too many "=" in coleq()"'
		exit 198
	}
	*if correct dividing # words by # equal signs should be even
	local rest    : subinstr local coleq `""=""' " ", all count(local enum)
	local restnum : list sizeof local rest
	local eqratio = `restnum'/`enum'
	local odd = mod(`eqratio',2)  // if 1 then odd number of words
	if `odd'  & ("`coleqcheck'"=="_" | "`coleqcheck'"=="" ) {
		di as error ///
		`"Incorrect use of "=" in coleq()"'
		exit 198
	}
}
if `"`roweq'"'!="" & `reqparse' {
	if regex(`"`roweq'"', "=[ ]*=")  {
		di as error ///
		`"Too  many "=" in roweq()"'
		exit 198
	}
	*if correct dividing # words by # equal signs should be even
	local rest    : subinstr local roweq `""=""' " ", all count(local enum)
	local restnum : list sizeof local rest
	local eqratio = `restnum'/`enum'
	local odd = mod(`eqratio',2)  // if 1 then odd number of words
	if `odd' & ("`coleqcheck'"=="_" | "`coleqcheck'"=="" ) {
		di as error ///
		`"Incorrect use of "=" in roweq()"'
		exit 198
	}
}

//  #4
//  If No Equation Names Added Use Previous Col/Row names

if `"`coleq'"'==""  {
	local newINcolNMS  `"`newincol'"'
	local newOUTcolNMS `"`newoutcol'"'
}
if `"`roweq'"'=="" {
	local newINrowNMS  `"`newinrow'"'
	local newOUTrowNMS `"`newoutrow'"'
}

//  #5
//  If equation name added or specified

*have to make a small adjustment for uservalues or there's an error
if `"`coleq'"'!="" & !`ceqparse' {
		local newOUTcolNMS : subinstr local newoutcol `""_":"' `"`coleq':"', all
		local newOUTcolNMS : subinstr local newOUTcolNMS `"_:"' `"`coleq':"', all
		local newINcolNMS  `"`newincol'"'
}

*if equation name added for rows add eq name in _cllct_appendRET
if `"`roweq'"'!="" & !`reqparse' {
	local newINrowNMS  `"`newinrow'"'
	local newOUTrowNMS `"`newoutrow'"'
	local singleRowEQ  `"`roweq'"'
}

//  #6
//  If equation names replaced

if `"`coleq'"'!="" & `ceqparse' {
	local tmpcoleqnm `"`coleq'"'
	local tmpcoleqnm : subinstr local tmpcoleqnm `""=""' " ", all

	while `"`tmpcoleqnm'"'!="" {
		gettoken outnm tmpcoleqnm : tmpcoleqnm, quotes
		gettoken innm  tmpcoleqnm : tmpcoleqnm, quotes
		if `"`innm'"'  != "" local innm      = itrim(`innm')
		if `"`outnm'"' != "" local outnm     = itrim(`outnm')
		if `"`innm'"'  != "" local tmpin `"`tmpin' "`innm'""'
		if `"`outnm'"' != "" local tmpout `"`tmpout' "`outnm'""'
	}

	local newOUTcolNMS `"`newoutcol'"'
	local newINcolNMS  `"`newincol'"'
	local num = 0
	foreach innm of local tmpin {
		local ++num
		local outnm        : word `num' of `tmpout'
		local newOUTcolNMS : subinstr local newOUTcolNMS ///
							 `""`innm'""' `""`outnm'""', all
	}

	if !`_allceq' {
		local tmpincol  `"`newincol'"'
		local tmpoutcol `"`newOUTcolNMS'"'
		local newOUTcolNMS ""
		local newINcolNMS  ""

		while `"`tmpoutcol'"' !="" {
			gettoken eqnm  tmpoutcol : tmpoutcol, parse(":")
			gettoken colnm tmpoutcol : tmpoutcol
			local nm `""`eqnm'"`colnm'"'
			local num = 0
			foreach outnm of local tmpout {
				if regexm(`"`nm'"',`""`outnm'":"') & `"`tmpoutcol'"' != "" {
					local newOUTcolNMS `"`newOUTcolNMS' `nm'"'
				}
			}
		}
		while `"`tmpincol'"' !="" {
			gettoken eqnm  tmpincol : tmpincol, parse(":")
			gettoken colnm tmpincol : tmpincol
			local nm `""`eqnm'"`colnm'"'
			local num = 0
			foreach innm of local tmpin {
				if regexm(`"`nm'"',`""`innm'":"') & `"`tmpincol'"' != "" {
					local newINcolNMS `"`newINcolNMS' `nm'"'
				}
			}
		}
	} // end if !`_allceq'
} // end replace col equation names

//  #7
//  Repeat For rows

if `"`roweq'"'!="" & `reqparse' {
	local tmproweqnm `"`roweq'"'
	local tmproweqnm : subinstr local tmproweqnm `""=""' " ", all

	while `"`tmproweqnm'"'!="" {
		gettoken outnm tmproweqnm : tmproweqnm, quotes
		gettoken innm  tmproweqnm : tmproweqnm, quotes
		if `"`innm'"'  != "" local innm      = itrim(`innm')
		if `"`outnm'"' != "" local outnm     = itrim(`outnm')
		if `"`innm'"'  != "" local tmpin `"`tmpin' "`innm'""'
		if `"`outnm'"' != "" local tmpout `"`tmpout' "`outnm'""'
	}

	local newOUTrowNMS `"`newoutrow'"'
	local newINrowNMS  `"`newinrow'"'
	local num = 0

	foreach innm of local tmpin {
		local ++num
		local outnm        : word `num' of `tmpout'
		local newOUTrowNMS : subinstr local newOUTrowNMS `""`innm'""' `""`outnm'""', all
	}

	if !`_allreq' {
		local tmpinrow  `"`newinrow'"'
		local tmpoutrow `"`newOUTrowNMS'"'
		local newOUTrowNMS ""
		local newINrowNMS  ""

		while `"`tmpoutrow'"' !="" {
			gettoken eqnm  tmpoutrow : tmpoutrow, parse(":")
			gettoken rownm tmpoutrow : tmpoutrow
			local nm `""`eqnm'"`rownm'"'
			local num = 0
			foreach outnm of local tmpout {
				if regexm(`"`nm'"',`""`outnm'":"') & `"`tmpoutrow'"' != "" {
					local newOUTrowNMS `"`newOUTrowNMS' `nm'"'
				}
			}
		}
		while `"`tmpinrow'"' !="" {
			gettoken eqnm  tmpinrow : tmpinrow, parse(":")
			gettoken rownm tmpinrow : tmpinrow
			local nm `""`eqnm'"`rownm'"'
			local num = 0
			foreach innm of local tmpin {
				if regexm(`"`nm'"',`""`innm'":"') & `"`tmpinrow'"' != "" {
					local newINrowNMS `"`newINrowNMS' `nm'"'
				}
			}
		}
	} // end if !`_allreq'
} // end replace row equation names

//  #8
//  Check locals

if `"`newINcolNMS'"' == `""' local newINcolNMS `"`newOUTcolNMS'"'
if `"`newINrowNMS'"' == `""' local newINrowNMS `"`newOUTrowNMS'"'

//  #9
//  create s-returns

sreturn local newINcolNMS  `"`newINcolNMS'"'
sreturn local newOUTcolNMS `"`newOUTcolNMS'"'
sreturn local newOUTrowNMS `"`newOUTrowNMS'"'
sreturn local newINrowNMS  `"`newINrowNMS'"'
sreturn local singleRowEQ  `"`singleRowEQ'"'

if "`debug'"!="" {
	di `"Names        = `innames'"'
	di `"anyeq        = `anyeq'"'
	di `"ceqparse     = `ceqparse'"'
	di `"reqparse     = `reqparse'"'
	di `"newincol     = `newincol'"'
	di `"newINcolNMS  = `newINcolNMS'"'
	di `"newoutcol    = `newoutcol'"'
	di `"newOUTcolNMS = `newOUTcolNMS'"'
	di `"newinrow     = `newinrow'"'
	di `"newINrowNMS  = `newINrowNMS'"'
	di `"newoutrow    = `newoutrow'"'
	di `"newOUTrowNMS = `newOUTrowNMS'"'
}

//  #9
//  end _cllct_addEQNMS program

end

//  #0
//  Program     : _cllct_addRTESTS
//  Author      : Joseph D. Wolfe
//  Description : adjust tests results from rreturns & return with sreturns
//  Requirements: >=Stata 10 (currently untested on previous versions)

* version 0.0.1 :: 07/15/11 02:31 PM ; parts from statstoV034 (ParseRreturns)
* version 0.0.2 :: 10/25/11 09:47 AM
* version 0.0.3 :: 12/06/11 01:20 PM
* version 0.0.4 :: 04/17/12 10:13 AM ; allow for e-return scalars

//  #1
//  Program define

program _cllct_addRTESTS, sclass

syntax [anything] ,    ///
	[                  ///
	isT(numlist)       ///
	isZ(numlist)       ///
	df(numlist)        ///
	level(numlist)     ///
	retexists(numlist) ///
	return(string)     ///
	er_ret(string)     ///
	debug              ///
	]                  ///

//  #2
//  set macros, defaults

tempname estval estimateval bval seval tval zval
tempname pval   levelpval cifactorval lbval ubval

*set pval
scalar `levelpval'  = (1 - (`level'/100) )/2

*set scalars to missing
scalar `bval'         =.z
scalar `estimateval'  =.z
scalar `estval'       =.z
scalar `pval'         =.z
scalar `tval'         =.z
scalar `zval'         =.z
scalar `lbval'        =.z
scalar `ubval'        =.z

//  #3
//  if test or comb get information

*retrieve all possible returns, even if they are not in memory
*Note: no error is generated if, e.g., r(se) does not exist
scalar `bval'  = `er_ret'(estimate)
scalar `seval' = `er_ret'(se)

*get from r(b) and r(V) if available
local getmatrix  = 0
local bexist     = 1

if `bval'==. {
	local bexist = 0
	capture scalar `bval' = el(r(b),1,1)
	capture confirm matrix r(V)
	if !_rc {
		local getmatrix  = 1
		local bexist     = 1
	}
}

*error if estimates do not exist
if !`retexists' & !`bexist' {
	di as error ///
	`"Neither `return' nor the information needed to calculate it is in memory"'
	exit 198
}

if `seval'==. & `getmatrix' {
	capture scalar `seval' = sqrt(el(r(V),1,1) )
}

* adjust for t or z distribution
if `isT' {
	scalar `cifactorval' =  abs(invttail(`df',`levelpval'))
}
if `isZ'  {
	scalar `cifactorval' =  abs(invnormal(`levelpval'))
}

* calcluate z, t, and pval
if `isT' {
	scalar `zval' = .z
	scalar `tval' = `bval'/`seval'
	scalar `pval' = 2*ttail(`df',abs(`tval'))

}
if `isZ' {
	scalar `tval' = .z
	scalar `zval' = `bval'/`seval'
	scalar `pval' = 2*(1-normal(abs(`zval')))
}

* bounds
scalar `lbval' = `bval' - (`cifactorval'*`seval')
scalar `ubval' = `bval' + (`cifactorval'*`seval')

*if return exists
if `retexists' & !`bexist' {
	if "`return'"=="`er_ret'(t)"  {
		scalar `tval'  = `return'
		local isT      = 1
		local isZ      = 0
	}
	if "`return'"=="`er_ret'(z)"  {
		scalar `zval'  = `return'
		local isZ      = 1
		local isT      = 0
	}
	if "`return'"=="`er_ret'(p)"  scalar `pval'  = `return'
	if "`return'"=="`er_ret'(lb)" scalar `lbval' = `return'
	if "`return'"=="`er_ret'(ub)" scalar `ubval' = `return'
}

*display warning if t/z specified but other is calculated
if "`stat'" == "t" & `isZ' {
	di as error ///
	"A z-statistic, not a t, has been calculated."
}
if "`stat'" == "z" & `isT' {
	di as error ///
	"A t-statistic, not a z, has been calculated."
}

//  #4
//  create returns

*create sreturn
sreturn local b         =`bval'
sreturn local estimate  =`bval'
sreturn local est       =`bval'
sreturn local level     =`level'
sreturn local p         =`pval'
sreturn local lb        =`lbval'
sreturn local ub        =`ubval'
if `isZ' {
	sreturn local z         =`zval'
	sreturn local t         =`zval'
}
if `isT' {
	sreturn local t         =`tval'
	sreturn local z         =`tval'
}

if !`df' sreturn local df = .z
if  `df' sreturn local df = `df'

if "`debug'"!="" {
	di "b"
	di `bval'
	di "estimate
	di `estimateval'
	di "est"
	di `estval'
	di "level"
	di `level'
	di "p"
	di `pval'
	di "t"
	di `tval'
	di "z"
	di `zval'
	di "lb"
	di `lbval'
	di "ub"
	di `ubval'
	di "df"
	di `df'
}

//  #5
//  end _cllct_addRTESTS

end

//  #0
//  Program     : _cllct_mat_capp
//  Author      : Statacorp
//  Description : slight adjustment of mat_capp
//  Requirements: >=Stata 10 (currently untested on previous versions)


* version 1.1.0 06jun2009

//  #1
//  program define

program _cllct_mat_capp
version 8

syntax anything [, miss(str) cons ts]

local b12   : word 1 of `anything'
local colon : word 2 of `anything'
local b1    : word 3 of `anything'
local b2    : word 4 of `anything'

if `"`colon'"' != ":" {
	di as err `"colon expected, `colon' found"'
	exit 198
}
confirm matrix `b1'
confirm matrix `b2'

local Qmatch = ("`miss'" == "")
if `Qmatch' {
	local miss .
}
if `Qmatch' & rowsof(`b1') != rowsof(`b2') {
	error 503
}

//  #2
//  if same names and same ordering, then do it fast !

mata: st_local("rnameseq", strofreal(	///
	st_matrixrowstripe("`b1'") == st_matrixrowstripe("`b2'") ))
if `rnameseq' {
	mat `b12' = `b1', `b2'
	exit
}

//  #3
//  check whether time-series insertion is requested and needed

if "`ts'" != "" {
	local rfullb1 : rowfullnames `b1', quoted
	local rfullb2 : rowfullnames `b2', quoted
	local tmp : subinstr local rfullb1 "." "", count(local n1)
	local tmp : subinstr local rfullb2 "." "", count(local n2)
	if `n1'==0 & `n2'==0 {
		local ts
	}
}

//  #4
//  match

tempname bb12 bb bb2 bbtemp newrow

local eq1 : roweq `b1', quoted
local eq2 : roweq `b2', quoted
local eq1 : list uniq eq1
local eq2 : list uniq eq2

foreach eq of local eq1 {
	mat `bb' = `b1'["`eq':",1...]
	mat `bb' = `bb' , J(rowsof(`bb'), colsof(`b2'), `miss')

	// eq is removed from eq2 if it exists
	local eq2 : subinstr local eq2 `""`eq'""' "", ///
					word count(local eqinb2)

	if `eqinb2' == 1 {

		// eq occurs also in b2
		mat `bb2' = `b2'["`eq':", 1...]
		local nbb2 = rowsof(`bb2')
		local namesbb2 : rownames `bb2', quoted

		tokenize `"`namesbb2'"'

		forvalues j = 1 / `nbb2' {
			_ms_parse_parts `"``j''"'
			local isvar = r(type) == "variable"
			local ii = rownumb(`bb',"``j''")
			if `ii' != . {
				// found
				mat subst `bb'[`ii',colsof(`b1')+1] ///
						= `bb2'[`j',1...]
			}
			else {
				// append a row to bb
				if `Qmatch' {
					di as err "rowname mismatch"
					exit 503
				}
				mat `newrow' = ///
					J(1,colsof(`b1'),`miss') , ///
					`bb2'[`j',1...]
				version 11: ///
				mat rownames `newrow' = `"`eq':``j''"'
				if "`ts'" != "" & `isvar' {
					TS_Insert `bb' `newrow'
				}
				else {
					mat `bb' = `bb' \ `newrow'
				}
			}
		}

		if "`cons'" != "" {
			ConsLast `bb'
		}
	}

	mat `bb12' = nullmat(`bb12') \ `bb'
}

// all equations now left in eq2 cannot exist in eq1/b1

if `Qmatch' & `"`eq2'"' != "" {
	di as err "rowname mismatch"
	exit 503
}

// append remaining equations in eq2

foreach eq of local eq2 {
	mat `bbtemp' = `b2'["`eq':",1...]
	mat `bb' = J(rowsof(`bbtemp'),colsof(`b1'),`miss') , `bbtemp'
	mata : st_matrixrowstripe("`bb'",		///
		(J(rows(st_matrix("`bb'")),1,"`eq'"),	///
		 (st_matrixrowstripe("`bbtemp'")[.,2])	///
		))
	mat `bb12' = `bb12' \ `bb'
}

mata : st_matrixcolstripe("`bb12'", ///
	st_matrixcolstripe("`b1'") \ st_matrixcolstripe("`b2'"))

mat `b12' = `bb12'
end

//  #5
//  ConsLast b: Moves the row with name _cons to the bottom

program ConsLast
args b

local i = rownumb(`b',"_cons")
local n = rowsof(`b')
if inlist(`i',.,`n') {
	exit
}

if `i' == 1 {
	mat `b' = `b'[2...,1...] \ `b'[1,1...]
	exit
}

local im1 = `i'-1
local ip1 = `i'+1
mat `b' = `b'[1..`im1',1...] \ (`b'[`ip1'..`n',1...] \ `b'[`i',1...])
end

//  #6
//  TS_Insert b newrow

*Inserts newrow in b directly after all rows of b with matching varname
*(i.e., with different ts operators), or at the bottom of b if no such
*row is found.

program TS_Insert
args b newrow

local vname : rownames `newrow', quoted
local ip = index("`vname'", ".")
if `ip' > 0 {
	local vname = substr("`vname'", `ip'+1, .)
}

local bnames : rownames `b', quoted
local nb = rowsof(`b')
tokenize `"`bnames'"'
forvalues i = `nb'(-1)1 {
	if index("``i'' ",".`vname' ") > 0 {
		// row should be inserted directly after i
		local ii = `i'
		continue, break
	}
}

if "`ii'" == "" | "`ii'" == "`nb'" {
	matrix `b' = `b' \ `newrow'
}
else {
	local ip1 = `ii'+1
	matrix `b' = `b'[1..`ii',1...] \ (`newrow' \ `b'[`ip1'...,1...])
}

//  #7
//  end _cllct_mat_capp

end

//  #0
//  Program     : _cllct_parseEQNMS
//  Author      : Joseph D. Wolfe
//  Description : create sreturns with new and orginal col/row/eq names
//  Requirements: >=Stata 10 (currently untested on previous versions)

* version 0.0.1 :: 06/07/11 12:05 PM
* version 0.0.2 :: 06/08/11 03:24 PM
* version 0.0.3 :: 07/26/11 03:24 PM
* version 0.0.4 :: 10/25/11 09:49 AM
* version 0.0.5 :: 11/24/11 06:43 PM

//  #1
//  Program define

program _cllct_parseEQNMS, sclass

syntax ,                       ///
	[                          ///
	rettype(string)            ///
	incolnms(string)           ///
	outcolnms(string)          ///
	inrownms(string)           ///
	outrownms(string)          ///
	matname(string asis)       ///
	parseCOLnms(numlist)       ///
	parseROWnms(numlist)       ///
	ret(string)                ///
	debug                      ///
	]                          ///

//  #2
//  defaults, locals

*tempnames
tempname INmat

*check if omitted variables have been removed
capture confirm matrix r(INmat)
if !_rc matrix `INmat' = r(INmat)
if  _rc matrix `INmat' = `ret'(b)

local INcolNMS `"`incolnms'"'
local OUTcolNMS `"`outcolnms'"'
local INcolNUM  : list sizeof local INcolNMS

local INrowNMS  `"`inrownms'"'
local OUTrowNMS `"`outrownms'"'
local INcolNUM  : list sizeof local INrowNMS

local eqcolchk : colfullnames `INmat'
local eqcolchk = regexm(`"`eqcolchk'"', ":")

local eqrowchk : rowfullnames `INmat'
local eqrowchk = regexm(`"`eqrowchk'"', ":")

//  #3
//  add equation names to cols

*if user specifies equation then this subroutine is skipped
*if equations exist but user has not specified add to col names
if !`parseCOLnms' & `eqcolchk' {
	local INceqNMS : coleq `INmat',  quoted
	local INceqNMS : list uniq local INceqNMS
	local INceqNUM : list sizeof local INceqNMS

	foreach eq of local INceqNMS {
		foreach innm of local INcolNMS {
			local newnm : subinstr local innm "_:" ""
			local tmpINcolNMS  `"`tmpINcolNMS'  "`eq'":`newnm'"'
		}
		foreach outnm of local OUTcolNMS {
			local newnm : subinstr local outnm "_:" ""
			local tmpOUTcolNMS `"`tmpOUTcolNMS' "`eq'":`newnm'"'
		}
	}
	sreturn local newINcolNMS  `"`tmpINcolNMS'"'
	sreturn local newOUTcolNMS `"`tmpOUTcolNMS'"'
}

if `parseCOLnms' | !`eqcolchk' {
	sreturn local newINcolNMS  `"`INcolNMS'"'
	sreturn local newOUTcolNMS `"`OUTcolNMS'"'
}

//  #4
//  add equation names to rows

if !`parseROWnms' & `eqrowchk' {
	local INreqNMS : roweq `INmat',  quoted
	local INreqNMS : list uniq local INreqNMS
	local INreqNUM : list sizeof local INreqNMS

	foreach eq of local INreqNMS {
		foreach innm of local INrowNMS {
			local newnm : subinstr local innm "_:" ""
			local tmpINrowNMS  `"`tmpINrowNMS'  "`eq'":`newnm'"'
		}
		foreach outnm of local OUTrowNMS {
			local newnm : subinstr local outnm "_:" ""
			local tmpOUTrowNMS `"`tmpOUTrowNMS' "`eq'":`newnm'"'
		}
	}
	sreturn local newINrowNMS  `"`tmpINrowNMS'"'
	sreturn local newOUTrowNMS `"`tmpOUTrowNMS'"'
}

if `parseROWnms' | !`eqrowchk' {
	sreturn local newINrowNMS  `"`INrowNMS'"'
	sreturn local newOUTrowNMS `"`OUTrowNMS'"'
}

if "`debug'"!="" {
di "parseEQNMS******Columns"
	di `"INcolNMS `INcolNMS'"'
	di `"Parse = `parseCOLnms'"'
	di `"eqcolchk = `eqcolchk'"'
	di `"INceqNMS = `INceqNMS'"'
	di `"newINcolNMS =  `tmpINcolNMS'"'
	di `"newOUTcolNMS =  `tmpOUTcolNMS'"'
di "parseEQNMS******Rows"
	di `"INrowNMS `INrowNMS'"'
	di `"Parse = `parseROWnms'"'
	di `"eqrowchk = `eqrowchk'"'
	di `"INreqNMS = `INReqNMS'"'
	di `"newINrowNMS =  `tmpINrowNMS'"'
	di `"newOUTrowNMS =  `tmpOUTrowNMS'"'
}

//  #5
//  end _cllct_parseEQNMS

end

//  #0
//  Program     : _cllct_parseMATeq
//  Author      : Joseph D. Wolfe
//  Description : parse matrix equation information
//  Requirements: >=Stata 10 (currently untested on previous versions)

* version 0.0.1 :: 06/26/11 6:22 PM
* version 0.0.2 :: 10/25/11 9:49 AM
* version 0.0.3 :: 01/30/12 3:32 PM :: parsing issue with newOUTrowNMS
* version 0.0.4 :: 03/30/12 11:35 AM

//  #1
//  Program define

program _cllct_parseMATeq, sclass

syntax ,                  ///
	[                     ///
	matrix(string asis)   ///
	columns(string asis)  ///
	rows(string asis)     ///
	incol(string asis)    ///
	outcol(string asis)   ///
	inrow(string asis)    ///
	outrow(string asis)   ///
	ret_type(string)      ///
	bstats(numlist)       ///
	trace                 ///
	debug                 ///
	]                     ///

set trace off
if "`trace'" != "" set trace on

//  #2
//  set up defaults, temporary files, locals

tempname INtempmat INmat INmathold
local newINcolNMS  `"`incol'"'
local newOUTcolNMS `"`outcol'"'
local newINrowNMS  `"`inrow'"'
local newOUTrowNMS `"`outrow'"'

*check if omitted variables have been removed
capture confirm matrix r(INmat)
if !_rc matrix `INtempmat' = r(INmat)
if  _rc matrix `INtempmat' = `matrix'

if "`matrix'" == "r(INatm)" & !_rc {
	matrix `INtempmat' = r(INatm) // atmatrix
	matrix `INmat'     = r(INmat)
}

if `bstats' & !_rc {
	matrix `INmat'     = r(INmat)'
	matrix `INtempmat' = r(INmat)'
	local check   : roweq `INmat', quoted
	local check   : word 1 of `check'
	if `"`check'"'==`"_"' local eqexist = 0
	if `"`check'"'!=`"_"' local eqexist = 1
}

//  #3
//  check if all columns of matrix needed

*check if, e.g., eq:col=_all
local trash : subinstr local newINcolNMS ":" "", count(local nmccount)

*chekc if _all is specfied
local _allcols = 0
if `"`columns'"'!="" {
	local newINcolNMS: subinstr local newINcolNMS "_:_all" "", all  count(local _allcap)

	local newOUTcolNMS: subinstr local newOUTcolNMS "_:_all" "", all  count(local _allcap)

	if   `_allcap' local _allcols = 1
}

*anything left in INcolNMS after _all removed?
local anycols : list sizeof local newINcolNMS

*if nothing left add _:_all check if,e.g., eq:col=_all
*if so, i'll have to make adjustments later
local cadjust = 0
if !`anycols' & `_allcols' & `"`newINcolNMS'"'=="" & `"`newOUTcolNMS'"'!="" {
	local cadjust = 1
}

*if cadjust then I need to move what's in newOUTcolNMS b/c
*it contains the column stub
if `cadjust' {
	local newOUTcolSTB `"`newOUTcolNMS'"'
	local newOUTcolNMS ""
}

//  #4
//  check for col(#) and change # to column name

*need to get a count of how many ":" which will tell me how many columns
local trash : subinstr local newINcolNMS ":" "", all count(local colcount)

forvalues matcol = 1(1)`colcount' {
	local instop  = 0
	local outstop = 0
	local ineqnm  ""
	local eqnm    ""
	local outeqnm ""
	local eqnmnew ""
	local stat    ""
	local statnew ""

	*get equation and column names and what they should be changed to
	*below trims equation name off and then gets column name (stat)
	while !`instop' {
		gettoken ineqnm newINcolNMS : newINcolNMS, parse(":")
		local eqnm "`eqnm' `ineqnm'"
		local instop = regexm("`eqnm'", ":")
		local eqnm : subinstr local eqnm ":" ""
		local eqnm = trim(`"`eqnm'"')
		if `"`newINcolNMS'"' == "" local instop = 1
	}
	gettoken stat newINcolNMS: newINcolNMS

	while !`outstop' {
		gettoken outeqnm newOUTcolNMS : newOUTcolNMS, parse(":")
		local eqnmnew "`eqnmnew' `outeqnm'"
		local outstop = regexm("`eqnmnew'", ":")
		local eqnmnew : subinstr local eqnmnew ":" ""
		local eqnmnew = trim(`"`eqnmnew'"')
		if `"`newOUTcolNMS'"' == "" local outstop = 1
	}
	gettoken statnew newOUTcolNMS: newOUTcolNMS

	capture confirm number `stat'

	if !_rc {
		*error if equation name exists
		if `"`eqnm'"'!=`"_"' {
			di as error ///
			"You cannot specify equations when using column(#) or at(#)"
			exit 198
		}
		*error if specified number is large number of rows
		local colnum = colsof(`INtempmat')
		if `colnum'< `stat' {
			di as error ///
			"column(#) or at(#) must be < or = to number of incoming columns"
			exit 198
		}

		capture confirm number `statnew'
		local repnew=0
		if !_rc {
			if `"`eqnmnew'`statnew'"'!=`"`eqnm'`stat'"' {
				di as error ///
				"column(#=#) or at(#=#) is not allowed"
				exit 198
			}
			local repnew = 1
		}

		mat `INmathold' = `INtempmat'[1...,`stat']
		local eqnm : coleq    `INmathold'
		local stat : colnames `INmathold'

		if `repnew' {
			local eqnmnew "`eqnm'"
			local statnew "`stat'"
		}
	} // end if _rc

	local INholdNM  `"`INholdNM' "`eqnm'":"`stat'""'
	local OUTholdNM `"`OUTholdNM' "`eqnmnew'":"`statnew'""'
}

local newINcolNMS  `"`INholdNM'"'
local newOUTcolNMS `"`OUTholdNM'"'

//  #4
//  grab all columns if none specified or "_all" specified

if !`bstats' & (`"`columns'"'=="" | ("`_allcols'"=="1" & !`anycols')) {
	local cnum = colsof(`INtempmat')
	local ineq : coleq `INtempmat', quoted
	local innm : colnames `INtempmat'

	forvalues c = 1(1)`cnum' {
		local newineq : word `c' of `ineq'
		local newinnm : word `c' of `innm'
		local newINcolNMS  `"`newINcolNMS' "`newineq'":"`newinnm'""'
		local newOUTcolNMS `"`newOUTcolNMS' "`newineq'":"`newinnm'""'
	}
}

//  #5
//  if _allcols adjust eq & col names so all columns are included

if "`_allcols'"=="1" & `anycols' {
	local tmpINcolNMS  `"`newINcolNMS'"'
	local tmpOUTcolNMS `"`newOUTcolNMS'"'
	local col_num : list sizeof local tmpINcolNMS
	local mcnms   : colnames `matrix'
	local meqcnms : coleq    `matrix'

	forvalues columns = 1(1)`col_num' {
		local instop  = 0
		local outstop = 0

		while !`instop' {
			gettoken ineqnm tmpINcolNMS : tmpINcolNMS, parse(":") quotes
			gettoken nms tmpINcolNMS : tmpINcolNMS, parse(" ") quotes
			local eqnms `"`eqnms' `ineqnm'"'
			local instop = regexm("`eqnms'", ":")
			local eqnms : subinstr local eqnms ":" ""
			local eqnms = trim(`"`eqnms'"')
			local innms `"`innms' `nms'"'
			local innms : subinstr local innms ":" ""
			local innms = trim(`"`innms'"')
			if `"`tmpINcolNMS'"' == "" local instop = 1
		}

		while !`outstop'  {
			gettoken outeqnms tmpOUTcolNMS: tmpOUTcolNMS, parse(":") quotes
			gettoken nms tmpOUTcolNMS: tmpOUTcolNMS, parse(" ") quotes
			local eqnmsnew `"`eqnmsnew' `outeqnms'"'
			local outstop = regexm("`eqnmsnew'", ":")
			local eqnmsnew : subinstr local eqnmsnew ":" ""
			local eqnmsnew = trim(`"`eqnmsnew'"')
			local outnms `"`outnms' `nms'"'
			local outnms : subinstr local outnms ":" ""
			local outnms = trim(`"`outnms'"')
			if `"`tmpOUTcolNMS'"' == "" local outstop = 1
		}
	} // end parse user specified equation & column names

	*compare user-specified names to matrix col names
	local cnum        = 0
	local mcolexist   = 0
	local newINcolNMS  ""
	local newOUTcolNMS ""
	local colnums : list sizeof local innms

	foreach mcnm of local mcnms {
		local ++cnum
		local meqcnm   : word `cnum' of `meqcnms'
		local colexist = 0

		local incnum = 0
		foreach nm of local innms {
			local ++incnum
			local eqnm    : word `incnum' of `eqnms'
			local outnm   : word `incnum' of `outnms'
			local eqnmnew : word `incnum' of `eqnmsnew'

			if `"`eqnm':`nm'"'==`"`meqcnm':`mcnm'"' {
				local newINcolNMS  `"`newINcolNMS' "`eqnm'":"`nm'""'
				local newOUTcolNMS `"`newOUTcolNMS' "`eqnmnew'":"`outnm'""'
				local ++colexist
				local ++mcolexist
			}
		}
		if !`colexist' {
			local newINcolNMS  `"`newINcolNMS' "`meqcnm'":"`mcnm'""'
			local newOUTcolNMS `"`newOUTcolNMS' "`meqcnm'":"`mcnm'""'
		}
	}

	*error if column not found
	if !`mcolexist' {
		di as error ///
		"The names specified in column() do not exist in `matrix'"
		exit 198
	}
} // end grab all columns

//  #6
//  repeat for rows

*count number of rows user has specified
local trash : subinstr local newINrowNMS ":" "", count(local nmrcount)

*check if all rows of matrix needed
local _allrows = 0
if `"`rows'"'!="" {
	local newINrowNMS: subinstr local newINrowNMS "_:_all" "", all  count(local _allcap)

	local newOUTrowNMS: subinstr local newOUTrowNMS "_:_all" "", all  count(local _allcap)

	if   `_allcap' local _allrows = 1
}

*anything left in INrowNMS after _all removed?
local anyrows : list sizeof local newINrowNMS

*if nothing left add _:_all check if,e.g., eq:row=_all
*if so, i'll have to make adjustments later
local radjust = 0
if !`anyrows' & `_allrows' & `"`newINrowNMS'"'=="" & `"`newOUTrowNMS'"'!="" {
	local radjust = 1
}

*if cadjust then I need to move what's in newOUTrowNMS b/c
*it contains the row stub
if `radjust' {
	local newOUTrowSTB `"`newOUTrowNMS'"'
	local newOUTrowNMS ""
}

//  #7
//  grab row information

if  `"`rows'"'=="" | ("`_allrows'"=="1" & !`anyrows') {
	if "`ret_type'"=="atmatrix" {
		*change rownames for at matrix to colnames for b matrix
		local rnum  = rowsof(`INtempmat')
		local innm  : colnames `INmat'
		local wdnum = 0

		*if nothing is being changed then out names become in names
		if `"`OUTrowNMS'"'==`"`INrowNMS'"' local outchng = 1
		else                               local outchng = 0
		local newINrowNMS ""

		*reformat names to be comparable to other return names
		forvalues x = 1(1)`rnum' {
			local ++wdnum
			local nm : word `wdnum' of `innm'
			local newINrowNMS `"`newINrowNMS' "_":"`nm'""'
		}
		if `outchng' local newOUTrowNMS `"`newINrowNMS'"'
	}

	if "`ret_type'"!="atmatrix" {
		local rnum = rowsof(`INtempmat')
		local ineq : roweq `INtempmat', quoted
		local innm : rownames `INtempmat'

		forvalues r = 1(1)`rnum' {
			local newineq : word `r' of `ineq'
			local newinnm : word `r' of `innm'
			local newINrowNMS  `"`newINrowNMS' "`newineq'":"`newinnm'""'
			local newOUTrowNMS `"`newOUTrowNMS' "`newineq'":"`newinnm'""'
		}
	}
}

//  #8
//  if _allrows adjust eq & row names so all are included

if `anyrows' {
	local eqnms     ""
	local innms     ""
	local eqnmsnew  ""
	local outnms    ""
	local mrnms     ""
	local meqrnms   ""
	local tmpINrowNMS  `"`newINrowNMS'"'
	local tmpOUTrowNMS `"`newOUTrowNMS'"'
	local row_num : list sizeof local tmpINrowNMS
	local mrnms   : rownames `INtempmat'
	local meqrnms : roweq    `INtempmat'

	forvalues rowumns = 1(1)`row_num' {
		local instop  = 0
		local outstop = 0

		while !`instop' {
			gettoken ineqnm tmpINrowNMS : tmpINrowNMS, parse(":") quotes
			gettoken nms tmpINrowNMS : tmpINrowNMS, parse(" ") quotes
			local eqnms `"`eqnms' `ineqnm'"'
			local instop = regexm("`eqnms'", ":")
			local eqnms : subinstr local eqnms ":" ""
			local eqnms = trim(`"`eqnms'"')
			local innms `"`innms' `nms'"'
			local innms : subinstr local innms ":" ""
			local innms = trim(`"`innms'"')
			if `"`tmpINrowNMS'"' == "" local instop = 1
		}

		while !`outstop'  {
			gettoken outeqnms tmpOUTrowNMS: tmpOUTrowNMS, parse(":") quotes
			gettoken nms tmpOUTrowNMS: tmpOUTrowNMS, parse(" ") quotes
			local eqnmsnew `"`eqnmsnew' `outeqnms'"'
			local outstop = regexm("`eqnmsnew'", ":")
			local eqnmsnew : subinstr local eqnmsnew ":" ""
			local eqnmsnew = trim(`"`eqnmsnew'"')
			local outnms `"`outnms' `nms'"'
			local outnms : subinstr local outnms ":" ""
			local outnms = trim(`"`outnms'"')
			if `"`tmpOUTrowNMS'"' == "" local outstop = 1
		}
	} // end parse user specified equation & row names

	*compare user-specified names to matrix row names
	local rnum        = 0
	local mrowexist   = 0
	local newINrowNMS  ""
	local newOUTrowNMS ""
	local rownums : list sizeof local innms

	foreach mrnm of local mrnms {
		local ++rnum
		local meqrnm   : word `rnum' of `meqrnms'
		local rowexist = 0
		local inrnum   = 0

		foreach nm of local innms {
			local ++inrnum
			local eqnm    : word `inrnum' of `eqnms'
			local outnm   : word `inrnum' of `outnms'
			local eqnmnew : word `inrnum' of `eqnmsnew'

			*if a number, skip it - dealt with in appendRET.ado
			capture confirm number `nm'
			if !_rc & `rnum'==1 {
				local mrowexist = 1
				if `_allrows' local all "_:_all"
				local newINrowNMS  `"`newINrowNMS' _:`nm' `all'"'
				local newOUTrowNMS `"`newOUTrowNMS' "`eqnmnew'":"`outnm'" `all'"'
			}

			*check if number range might exist
			if regexm(`"`nm'"', "-") & `rnum'==1  {
				local mrowexist = 1
				if `_allrows' local all "_:_all"
				local newINrowNMS  `"`newINrowNMS' _:`nm' `all'"'
				local newOUTrowNMS `"`newOUTrowNMS' "`eqnmnew'":"`outnm'" `all'"'
			}


			*adjust for at rownames not appearing as in matrix
  			if `"`eqnm':`nm'==`meqrnm':`mrnm'"' == `"_:1._at==_:1bn._at"' {
				local nm "1bn._at"
			}

			*if no equation specified but exists grab rows for each eq
			if `"`eqnm'"'==`"_"' & "`eqexist'"=="1" {
				if `"`nm'"'==`"`mrnm'"' {
					local newINrowNMS  `"`newINrowNMS' "`meqrnm'":"`nm'""'
					local newOUTrowNMS `"`newOUTrowNMS' "`meqrnm'":"`outnm'""'
					local ++rowexist
					local ++mrowexist
				}
			}

			if `"`eqnm':`nm'"'==`"`meqrnm':`mrnm'"' {
				local newINrowNMS  `"`newINrowNMS' "`eqnm'":"`nm'""'
				local newOUTrowNMS `"`newOUTrowNMS' "`eqnmnew'":"`outnm'""'
				local ++rowexist
				local ++mrowexist
			}
		}

		if !`rowexist' & "`_allrows'"=="1" {
			local newINrowNMS  `"`newINrowNMS' "`meqrnm'":"`mrnm'""'
			local newOUTrowNMS `"`newOUTrowNMS' "`meqrnm'":"`mrnm'""'
		}
	}
} // end grab all rowumns

//  #9
//  errors

*error if row not found
if "`mrowexist'"=="0" & !`_allrows' {
	di as error ///
	"The names specified in row() do not exist"
	exit 198
}

//  #10
//  create s-returns

sreturn local newINcolNMS  `"`newINcolNMS'"'
sreturn local newOUTcolNMS `"`newOUTcolNMS'"'
sreturn local newINrowNMS  `"`newINrowNMS'"'
sreturn local newOUTrowNMS `"`newOUTrowNMS'"'
sreturn local cadjust      `"`cadjust'"'
sreturn local newOUTcolSTB `"`newOUTcolSTB'"'
sreturn local radjust      `"`radjust'"'
sreturn local newOUTrowSTB `"`newOUTrowSTB'"'

if "`debug'" != "" {
	di `"newINcolNMS   = `newINcolNMS'"'
	di `"newOUTcolNMS  = `newOUTcolNMS'"'
	di `"newINrowNMS   = `newINrowNMS'"'
	di `"newOUTrowNMS  = `newOUTrowNMS'"'
}

//  #11
//  end _cllct_parseMATeq

end

//  #0
//  Program     : _cllct_removeOMITTED
//  Author      : Joseph D. Wolfe
//  Description : create sreturns with equation names added
//  Requirements: >=Stata 10 (currently untested on previous versions)

* version 0.0.1 :: 07/26/11 11:11 AM
* version 0.0.2 :: 10/25/11 09:51 AM
* version 0.0.3 :: 04/03/12 07:19 PM
* version 0.0.4 :: 05/11/12 01:46 PM :: nostrip option added

//  #1
//  Program define

program _cllct_removeOMITTED, rclass

syntax ,                     ///
	[                        ///
	matrix(string)           ///
	NOStrip	                 ///
	trace                    ///
	debug					 ///
	]                        ///

set trace off
if "`trace'" != "" set trace on

//  #2
//  Set defaults

*temporary names for matrix/return manipulation
tempname INmat INcov INatm MAThold INtempmat INtempCNM INtempRNM

*need this for margins related commands
local ver = c(stata_version)

*check if matrix exists before trying to remove omitted
capture confirm matrix `matrix'
if  _rc local stop = 1
if !_rc local stop = 0

//  #3
//  remove omitted

if `ver'>=11 & !`stop' & "`nostrip'"=="" {

	local cnum = 0

	*create matrix
	matrix `INmat' = `matrix'

	if "`matrix'" == "r(b)" {
		matrix `INcov' = r(V)
		matrix `INatm' = r(at)
	}
	if "`matrix'" == "e(b)" {
			matrix `INcov' = e(V)
			matrix `INatm' = e(at)
	}

	local inmatrices `"`inmatrices' INmat"'
	capture confirm matrix `INcov'
	if !_rc local inmatrices `"`inmatrices' INcov"'
	capture confirm matrix `INatm'
	if !_rc local inmatrices `"`inmatrices' INatm"'

	foreach mat of local inmatrices {
		*check missing
		_ms_omit_info ``mat''
		matrix `MAThold' = r(omit)
		mata:  `MAThold' = st_matrix("`MAThold'")
		local colnum     = colsof(`MAThold')

		*turn 0s to 1s and 1s to 0s so mata can be used to remove omitted
		forvalues col = 1(1)`colnum' {
			mata: `MAThold'[1,`col'] = abs(`MAThold'[1,`col']-1)
		}

		*get matrix info from stata
		mata: `INtempmat' = st_matrix("``mat''")
		mata: `INtempCNM' = st_matrixcolstripe("``mat''")
		mata: `INtempRNM' = st_matrixrowstripe("``mat''")

		*remove rows that are missing and restor matrix
		mata: st_matrix("``mat''", select(`INtempmat', `MAThold'))
		mata: `INtempCNM' = select(`INtempCNM', (`MAThold')')
		mata: st_matrixcolstripe("``mat''", `INtempCNM')
		mata: st_matrixrowstripe("``mat''",`INtempRNM')

		*transpose and repeat
		matrix ``mat'' = (``mat'')'

		*check missing
		_ms_omit_info ``mat''
		matrix `MAThold' = r(omit)
		mata:  `MAThold' = st_matrix("`MAThold'")
		local colnum     = colsof(`MAThold')

		*turn 0s to 1s and 1s to 0s so mata can be used to remove omitted
		forvalues col = 1(1)`colnum' {
			mata: `MAThold'[1,`col'] = abs(`MAThold'[1,`col']-1)
		}

		*get matrix info from stata
		mata: `INtempmat' = st_matrix("``mat''")
		mata: `INtempCNM' = st_matrixcolstripe("``mat''")
		mata: `INtempRNM' = st_matrixrowstripe("``mat''")

		*remove rows that are missing and restor matrix
		mata: st_matrix("``mat''", select(`INtempmat', `MAThold'))
		mata: `INtempCNM' = select(`INtempCNM', (`MAThold')')
		mata: st_matrixcolstripe("``mat''", `INtempCNM')
		mata: st_matrixrowstripe("``mat''",`INtempRNM')

		*transpose to orginal
		matrix ``mat'' = (``mat'')'
		}
} // end remove omitted

//  #4
//  set returns

capture confirm matrix `INmat'
if !_rc return matrix INmat = `INmat'
capture confirm matrix `INcov'
if !_rc return matrix INcov = `INcov'
capture confirm matrix `INatm'
if !_rc return matrix INatm = `INatm'

//  #5
//  end _cllct_removeOMITTED

end

//  Program     : _cllct_appendFILE
//  Author      : Joseph D. Wolfe & J. Scott Long
//  Description : Save graphs in single file (rtf, html, tex)
//  Requirements: >= Stata 10 (untested on previous versions)

* version 0.0.1 :: 01/28/12 11:09 AM :: Created separate ado for part
*!version 0.0.2 :: 03/30/12 11:37 AM

//  #1
//  Program define graphout

program define _cllct_appendFILE, sclass

syntax,                         ///
    [                           ///
								/// Necessary options
    ahandle(string asis)        /// temp name of handle
    bhandle(string asis)        /// temp name of handle
    appfile(string asis)        /// temp name of file
    savefile(string asis)   	/// name of file name & loc if avail
								///
								/// Graphout options
	file_ext(string)            /// file extension (e.g., .rtf)
	fignum(numlist)             /// figure number
	tabnum(numlist)             /// figure number
	nocount                     /// don't count graphs
	fbarrier(numlist)           /// tex files
							    ///
								/// Statout options
	tabnum(numlist)             /// table number
    ]

//  #2
//  Set defaults

local SaveFileAs `"`savefile'"'

if "`fbarrier'" !="" {
	local brpkg  "\usepackage{placeins}"
	local break  "\FloatBarrier"
	local brnum  "`fbarrier'"
}

//  #2
//  append file

* locals to find end of file
local endflcxv   `"="""'
local endfltex   `"\end{document}"'
local endflrtf   `"}"'
local endflhtml  `"</body></html>"'
local endfl      `"`endfl`file_ext''"'

* open file with graphs
qui file open `ahandle'  using `appfile'   , read write
qui file open `bhandle'  using `macval(SaveFileAs)', read write
file seek `bhandle' query
local loc    = r(loc)
local end    = 0

* copy file to temporary file
while (`end'==0) {
	file seek  `bhandle' `loc'
	file read  `bhandle' line
	local end = r(eof)
	file seek  `bhandle' query
	local loc = r(loc)

	*check if file ends
	local checkend = 1
	if (`"`line'"' == `"`endfl'"') & (`"`file_ext'"'!="csv") {
		local  checkend   = 0
		while `checkend' == 0 {
			file seek  `bhandle' `loc'
			file read  `bhandle' newline
			local newend  = r(eof)
			file seek  `bhandle' query
			local newloc  = r(loc)
			local newline `"`line'"'
			if `"`newline'"'  != "" local checkend = 1
			if   `newend'     != 0  local checkend = 2
		}
	}
	if `checkend' == 2 continue, break

	*count graphs in tex file too
	  *1) add break to avoid "too many floats"
	  *2) add figure # before graph
	if "`nocount'"=="" & regexm(`"`line'"',"Figure [1-9]+") & "`float'"=="" {
		local fignum = `fignum'+1
	}
	if "`nocount'" == "" & regexm(`"`line'"', "Table [1-9]+")  {
		local tabnum = `tabnum'+1
	}
	if "`file_ext'" == "tex" & "`break'" != "" {
		if regexm(`"`line'"', `"`endfig'"') {
			local numgraph = `numgraph'+1
		}
		if regexm(`"`line'"', `"`break'"') {
			local numgraph =  0
		}
	if `numgraph' == `brnum' {
		local line `"`line' `break'"'
		local numgraph = 0
	}
	}
	*write line to append file
	file write `ahandle' `"`line'"' _n
 }

//  #3
//  Add sreturns

sreturn local fignum `fignum'
sreturn local tabnum `tabnum'

//  #4
//  Close
end

//  #0
//  Program     : _cllct_getFileNameAndPath.ado
//  Author      : Joseph D. Wolfe
//  Description : get file name and path to file if it exists
//  Requirements: >=Stata 10 (currently untested on previous versions)

*! version 0.0.1 :: 03/01/11 12:31 PM

//  #1
//  Program define

program  _cllct_getFileNameAndPath, sclass
	syntax, fileinfo(string)
	version 10

//  #2
//  Parse path and set sreturns

if regexm(`"`fileinfo'"', "[\]|[/]")  {
	local more `"`fileinfo'"'
	local stop = 1

	while `stop' != 0 {
		if regexm(`"`more'"', "[\]") {
			gettoken hold more : more, parse("\")
			local PathToFile `"`macval(PathToFile)'`hold'"'
			local trash : subinstr local more "\" "", all count(local stop)
		}

		if regexm(`"`more'"', "[/]") {
			gettoken hold more : more, parse("/")
			local PathToFile "`macval(PathToFile)'`hold'"
			local trash : subinstr local more "/" "", all count(local stop)
		}
	}
	sreturn local file_name  `"`more'"'
	sreturn local PathToFile `"`PathToFile'"'
}

else {
	sreturn local file_name `"`fileinfo'"'
	sreturn local PathToFile ""
}

//  #3
//  end _getFileNameAndPath
end

//  #0
//  Program     : _cllct_parseNMS
//  Author      : Joseph D. Wolfe
//  Description : create sreturns with new and orginal col/row/eq names
//  Requirements: >=Stata 10 (currently untested on previous versions)

* version 0.0.1 :: 06/07/11 12:05 PM
* version 0.0.2 :: 07/26/11 12:05 PM
* version 0.0.3 :: 10/25/11 09:50 AM
*!version 0.0.4 :: 05/24/12 06:17 PM

//  #1
//  Program define

program define _cllct_parseNMS, sclass

syntax ,                     ///
	[                        ///
	innames(string asis)     ///
	rownames(numlist)        ///
	coleq(string asis)       ///
	roweq(string asis)       ///
	trace                    ///
	debug					 ///
	]                        ///

set trace off
if "`trace'"    != "" set trace on
if "`rownames'" == "" local rownames = 0

//  #2
//  add defaults

*check if equation names are already specified
local parseNMS = regexm(`"`innames'"', ":")

*see if equations are specified in incoming names
local anyceq = regexm(`"`newoutcol'"', ":")
local anyreq = regexm(`"`newoutrow'"', ":")

//  #3
//  add errors

if `parseNMS' & `"`roweq'"'!="" & `rownames' {
	di as error ///
	"You cannot use roweq() when equations are specified in row names"
	exit 198
}

if `parseNMS' & `"`coleq'"'!=""  & !`rownames' {
	di as error ///
	"You cannot use coleq() when equations are specified in column names"
	exit 198
}

//  #4
//  Prepare for parsing

*add spaces around = and : to assist in parsing
local crnames : subinstr local innames "=" " = ", all
local crnames : subinstr local crnames ":" " : ", all
// local crnames = itrim(`"`crnames'"')
local crnum   : list sizeof local crnames
tokenize `"`crnames'"'

*n1wd-n3wd stands for next # word
*p1wd-p3wd stands for prev # word
local n1wd =  1
local n2wd =  2
local n3wd =  3
local p1wd = -1
local p2wd = -2
local p3wd = -3

//  #5
//  loop through each word of tokenized crnames and label as in or out names

forvalues wd = 1(1)`crnum' {
	local ++n1wd
	local ++n2wd
	local ++n3wd
	local ++p1wd
	local ++p2wd
	local ++p3wd
	local innm  = 0  // if 1 then send to INcolNMS
	local outnm = 0  // if 1 then send to OUTcolNMS
	local stop  = 0

	*the next word can be "=", ":", or neither
	*figure out which and then determine where name should be
	if `"``n1wd''"' == "=" {
		if  `"``wd''"'   == "="  | `"``wd''"' == ":"   local stop = 1
		if  `"``n2wd''"' == "="  | `"``n2wd''"'==":"   local stop = 1
		if (`"``p1wd''"' == "=")                       |               ///
           (`"``p1wd''"' == ":" & `"``p3wd''"' == "=") |               ///
           (`"``p1wd''"' == ":" & `"``p3wd''"' == "=") local stop = 1

		if !`stop' local outnm = 1
	}

	if (`"``n1wd''"' == ":") {
		if  `"``wd''"'   == "="  | `"``wd''"'  ==":"    local stop  =1
		if  `"``n2wd''"' == "="  | `"``n2wd''"'==":"    local stop  =1
		if (`"``p1wd''"' == "=") &                                      ///
           (`"``p2wd''"' == "="  | `"``p2wd''"' == ":") local stop  =1
		if  `"``p1wd''"' == "="                         local innm  =1
		if  `"``n2wd''"' == "="                         local outnm =1
		if  `"``n1wd''"' == ":" & `"``n2wd''"' != "=" &                 ///
            `"``n2wd''"' != ":" & `"``n3wd''"' == "="   local outnm =1
		if  `"``p1wd''"' != "=" & `"``n3wd''"'!="=" {
			local innm  = 1
			local outnm = 1
		}
	}

	if (`"``n1wd''"' != "=" & `"``n1wd''"' !=":")	{
		if `"``p1wd''"'  == "=" & `"``p2wd''"' == "="  local stop  =1
		if `"``p1wd''"'  == ":" & `"``p2wd''"' == ":"  local stop  =1
		if `"``p1wd''"'  == "="                        local innm  =1
		if `"``p1wd''"'  == ":" & `"``p3wd''"'== "="   local innm  =1
		if  `"``p1wd''"' == ":" & `"``p2wd''"'!= "="                    ///
                                & `"``p2wd''"'!= ":"   local innm  =1
		if `"``wd''"'    ==":"  & `"``n2wd''"'== "="   local outnm =1
		if `"``wd''"'    ==":"  & `"``p2wd''"'== "="   local innm  =1
		if `"``p1wd''"'  ==":"  & `"``n2wd''"'!= "="                    ///
         & `"``p3wd''"'  =="=" 		                 	local innm =1
		if `"``p1wd''"'  ==":"  & `"``n2wd''"'!= "=" & `"``p3wd''"'!="=" {
			local innm  = 1
			local outnm = 1
		}
		if `"``wd''"'==":" & `"``p2wd''"'!="=" & `"``n2wd''"'!="=" {
			local innm  = 1
			local outnm = 1
		}
    	if `"``p1wd''"'!=":" & `"``p1wd''"'!="=" & `"``wd''"'!=":" &    ///
           `"``n2wd''"'!="=" {
			local innm  = 1
			local outnm = 1
		}
    	if `"``p1wd''"'!=":" & `"``p1wd''"'!="=" & `"``wd''"'!=":" &   ////
           `"``n1wd''"'!=":" & `"``n1wd''"'!="=" {
			local innm  = 1
			local outnm = 1
		}
	}

	*never include equal signs in locals
	if `"``wd''"' == "=" {
		local innm  = 0
		local outnm = 0
	}

if "`debug'"!="" {
	di "`wd'*******************************"
	di `"Names = `innames'"'
	di `"IN `innm' / OUT `outnm'"'
	di `"wd - ``wd''"'
	di `"n1wd - ``n1wd''"'
	di `"n2wd - ``n2wd''"'
	di `"n3wd - ``n3wd''"'
	di `"p1wd - ``p1wd''"'
	di `"p2wd - ``p2wd''"'
	di `"p3wd - ``p3wd''"'
	di `"Error - `stop'"'
}

	*put 2+ word column/equation names into quotes
	local quotes : list sizeof local `wd'
	if `quotes'>1 local wd `""``wd''""'
	else local wd `"``wd''"'

	if `innm' {
		*make sure all in names have equation name
		local inwd `"`wd'"'
		local inwd = itrim(`"`inwd'"')
		local inwd =  trim(`"`inwd'"')

		if (!regexm(`"`wd'"',":")     &  !regexm(`"``p1wd''"',":") &       ///
           !regexm(`"``n1wd''"',":")) | (!regexm(`"`wd'"',":")     &       ///
            regexm(`"``p1wd''"',"innames\(") & !regexm(`"``n1wd''"',":"))  {
            	local inwd `"_:`inwd'"'
				local IN_NMS `"`IN_NMS' `inwd'"'
         }

		else if `"`inwd'"'==":" |  `"``p1wd''"'==":" { //v004 fix
			local IN_NMS `"`IN_NMS'`inwd'"'
		}

		else if `"`inwd'"'!=":" & `"``p1wd''"'!=":" {
			local IN_NMS `"`IN_NMS' `inwd'"'
		}
	}
	if `outnm' {
		*make sure all out names have equation name
		local outwd `"`wd'"'
		local outwd = itrim(`"`outwd'"')
		local outwd =  trim(`"`outwd'"')

		if (!regexm(`"`wd'"',":")      & !regexm(`"``p1wd''"',":") &       ///
			!regexm(`"``n1wd''"',":")) | (!regexm(`"`wd'"',":")    &       ///
         	 regexm(`"``p1wd''"',"innames\(") & !regexm(`"``n1wd''"',":")) {
			 	local outwd `"_:`outwd'"'
				local OUT_NMS `"`OUT_NMS' `outwd'"'
		}

		else if `"`outwd'"'==":" | `"``p1wd''"'==":" {
			local OUT_NMS `"`OUT_NMS'`outwd'"'
		}

		else if `"`outwd'"'!=":" & `"``p1wd''"'!=":" {
			local OUT_NMS `"`OUT_NMS' `outwd'"'
		}
	}

	*Syntax error
	if "`stop'" == "" local stop = 0
	if "`stop'" == "1" {
		di as error "Column or Row specification contains invalid syntax"
		exit 198
	}
}

/* v004: no longer necessary
//  #6
//  Remove Unnecessary Spaces

local IN_NMS = itrim(`"`IN_NMS'"')
local IN_NMS : subinstr local IN_NMS " = " "=", all
local IN_NMS : subinstr local IN_NMS  " =" "=", all
local IN_NMS : subinstr local IN_NMS  "= " "=", all

local IN_NMS : subinstr local IN_NMS " : " ":", all
local IN_NMS : subinstr local IN_NMS  " :" ":", all
local IN_NMS : subinstr local IN_NMS  ": " ":", all

local OUT_NMS = itrim(`"`OUT_NMS'"')
local OUT_NMS : subinstr local OUT_NMS " = " "=", all
local OUT_NMS : subinstr local OUT_NMS  " =" "=", all
local OUT_NMS : subinstr local OUT_NMS  "= " "=", all

local OUT_NMS : subinstr local OUT_NMS " : " ":", all
local OUT_NMS : subinstr local OUT_NMS  " :" ":", all
local OUT_NMS : subinstr local OUT_NMS  ": " ":", all
*/

//  #7
//  Set s-returns

sreturn local parseNMS = `parseNMS'  // to use in parseEQNMS
sreturn local IN_NMS  `"`IN_NMS'"'
sreturn local OUT_NMS `"`OUT_NMS'"'

if "`debug'"!="" {
	di `"Names = `innames'"'
	di `"IN_NMS = `IN_NMS'"'
	di `"OUT_NMS = `OUT_NMS'"'
}

//  #8
//  end _cllct_parseNMS program
end

//  #0
//  Program     : _cllct_rowcolNUM
//  Author      : Joseph D. Wolfe
//  Description : convert # to column or row names
//  Requirements: >=Stata 10 (currently untested on previous versions)

* version 0.0.2 :: 03/28/12 9:47 AM
* version 0.0.1 :: 10/27/11 1:04 PM

//  #1
//  Program define

program _cllct_rowcolNUM, sclass

syntax ,                     ///
	[                        ///
	inmat(string asis)       ///
	innm(string asis)        ///
	outnm(string asis)       ///
	col(numlist)             ///
	eb(string asis)          ///
	]                        ///

//  #2
//  Set defaults

tempname INmat
if  "`inmat'"=="INmat" mat `INmat' = r(INmat)
if  "`inmat'"=="INatm" mat `INmat' = r(INatm)

*need to transpose matrix if eb or rb return
if `eb' mat `INmat'= `INmat''

*get matrix row/col names
if `col' {
	local mateq  : coleq    `INmat', quoted
	local matnm  : colnames `INmat'
	local matnum = colsof(`INmat')
}
if !`col' {
	local mateq  : roweq    `INmat', quoted
	local matnm  : rownames `INmat'
	local matnum = rowsof(`INmat')
}

*hold names
local tempIN  `"`innm'"'
local tempOUT `"`outnm'"'
local newin    ""
local newout   ""

//  #3
//  any incoming or outgoing names numbers?

local count = 0
local tempNMS `"`innm'"'
local tempOUT `"`outnm'"'
local hold     ""
local outhold  ""

while `"`tempIN'"'!="" {
	gettoken ineq tempIN : tempIN, parse(":")
	gettoken in tempIN   : tempIN, parse(" ")
	local    in          : subinstr local in ":" ""
	local    in          : subinstr local in `"""' "", all

	gettoken outeq tempOUT : tempOUT, parse(":")
	gettoken out   tempOUT : tempOUT, parse(" ")
	local    out           : subinstr local out ":" ""
	local    out           : subinstr local out `"""' "", all

	capture confirm integer number `in'
	local numchk = _rc
	if `"`in'"'==`"`out'"' local ineqout = 1
	if `"`in'"'!=`"`out'"' local ineqout = 0

	local parse = regexm(`"`in'"', "-")

	*if `in' is empty exit loop
	if `"`in'"'=="" continue, break

	*is a range of numbers included?
	*make sure "-" isn't inside a name
	if `parse' & `ineqout' {
		foreach nm of local inname {
			if `"`in'"'==`"`nm'"' {
				local parse = 0
			}
		}
		if `parse' {
			local    strt : subinstr local in "-" " "
			gettoken strt  stp : strt , parse(" ")
			local    strt = trim("`strt'")
			local    stp  = trim("`stp'")

			capture confirm integer number `strt'
			if !_rc local start = `strt'
			if  _rc local start : list posof "`strt'" in matnm

			capture confirm integer number `stp'
			if !_rc local stop  = `stp'
			if  _rc local stop  : list posof "`stp'" in matnm

			*error message if stop is bigger or = to start of range
			if (`start'>=`stop') | (`stop' > `matnum')  {
				di as error "`in' is an invalid range"
				exit 198
			}

			*get names of col/row in range specified
			forvalues num = `start'/`stop' {
				local eq : word `num' of `mateq'
				local nm : word `num' of `matnm'
				local eqnm `"`eqnm' "`eq'":"`nm'""'
			}

			*replace range with names in range
			local newin  `"`newin' `eqnm'"'
			local newout `"`newout' `eqnm'"'
		} // end if `parse'
	} // end if regexm()

	* see if in is a single number; if so replace with name
	if !`numchk' & !`parse' {
		local repeq : word `in' of `mateq'
		local repin : word `in' of `matnm'

		local newin `"`newin' "`repeq'":`repin'"'
		if  `ineqout' local newout `"`newout' "`repeq'":"`repin'""'
		if !`ineqout' local newout `"`newout' "`outeq'":"`out'""'

	}

	if `numchk' & !`parse' {
		local newin  `"`newin'  "`ineq'":"`in'""'
		local newout `"`newout' "`outeq'":"`out'""'
	}
} // end foreach

*change "_":_all to _:_all for future code to work correctly
local newin  : subinstr local newin  `""_":_all"' "_:_all", all
local newout : subinstr local newout `""_":_all"' "_:_all", all
local newin  : subinstr local newin  `""_":"_all""' "_:_all", all
local newout : subinstr local newout `""_":"_all""' "_:_all", all

//  #4
//  set resturns

sreturn local IN_NMS  `"`newin'"'
sreturn local OUT_NMS `"`newout'"'

//  #5
//  end _cllct_rowcolNUM

end

// end subroutines -----------------------------------------------------------

exit  // exit statsto




// statsto history -----------------------------------------------------------

version 0.0.6c 2011-02-07 get b - need to populate collect yet...
version 0.0.5c 2011-02-06 start populating
version 0.0.4b 2011-02-05 clean code
version 0.0.3 2011-02-05
version 0.0.2 2011-02-03
version 0.0.1 2011-02-02 - i/o and basic syntax
version 0.0.8 2011-02-08 pre major cleanup
version 0.0.7a 2011-02-07 ParseAT
version 0.1.0 2011-02-08 rationalize names
version 0.1.6 rows(_all)
version 0.1.5 clear alone didn't change matrix in memory
version 0.1.4 b doesn't add new names adds extra column
version 0.1.3 allow no row
version 0.1.2 2011-02-09 only rows()
version 0.1.1 2011-02-08 populate b and at
version 0.2.6 rowstub() added; needs more debugging
version 0.2.5 auto add row if no row(); don't drop _N_u_l_l_ if rows added
version 0.2.4 mout and outcome
version 0.2.3 fix z vs t in testnl and test
version 0.2.2 fix when r(t) is not a test...
version 0.2.1 matrix
version 0.2.0 version 10 compatable
version 0.1.8 rows(xx=_all)
version 0.1.7 all margins and most factor seems functional
version 0.3.0 2011-02-19 mrowstub to matrowstub; mcolstub matcolstub
version 0.2.9 2011-02-18
version 0.2.8 i.wc in dydx() for margins; c(stata_version) not c(version)!
version 0.2.7 debug at() options; allow at(3)
version 0.3.1 matrowstub matcolstub matrow1 matcol1 again
version 0.3.2 2011-03-01 allow upper case for derivative r() returns
version 0.3.3 2011-03-25 fix matrixrow1() where 1st row dropped
version 0.3.4 2011-04-04 add version control for factor variables
version 0.3.4 and rename _clct_ListMatrix
version 0.3.5 2011-05-26 bug with regress p-values
version 0.4.0 05/28/11 07:18 PM; small clean up
version 0.5.0 05/28/11 07:23 PM; JDW complete rewrite
version 0.5.1 06/10/11 01:04 PM; JDW add row options, clean up
version 0.5.2 07/27/11 01:04 PM;
version 0.6.0 10/25/11 09:52 AM; bug fixes; updates to r tests; generate
version 0.6.1 11/03/11 09:50 AM; Minor clean
version 0.6.2 11/11/11 09:06 AM; coleq bug fix
version 0.6.3 11/28/11 07:14 AM; unstack option added
version 0.6.4 12/06/11 01:18 PM ; expb & or added, bug fixes
version 1.6.4 12/12/11 11:40 AM ; Live Version
version 1.6.5 01/25/12 8:31 AM :: save matname to global
version 1.6.6 03/27/12 4:28 PM :: bugs - formatting, uvals, overwriting
version 1.6.7 04/03/12 8:42 PM
version 1.6.8 04/03/12 8:42 PM :: fixed error adding matrices
version 1.6.9 :: 2012-05-04 :: error with or option, add orpct; combine subroutines
version 1.7.0 :: 2012-05-04 :: add _rest; col/rowstart update; update width for row titles
version 1.7.1 :: 2012-05-08 :: uservalues row#; bug in colstart/rowstart; error if r() or e() is matrix
version 1.7.2 :: 05/11/12 11:18 AM  :: scalar row = row# by default; nostrip option added; e() and r() allow 1x1 matrix
version 1.7.3 :: 05/17/12 11:18 AM  :: fix level error
version 1.7.4 :: 05/17/12 11:18 AM  :: fix row bug; fix u() row() bug
version 1.7.5 :: 05/22/12 02:42 PM  :: fix row bug with scalars rename
version 1.7.6 :: 05/24/12 6:00 PM :: fix equation bug
version 1.7.7 :: 05/24/12 6:00 PM :: fix col(stub=_all) bug; fix at() row(#) bug
version 1.7.8 :: 05/27/12 1:21 PM :: change global name to statsto_stored_matrices
version 1.7.9 :: 05/30/12 9:27 AM :: fix mat() bug ; restore rreturns sooner
version 2.0.0 :: 05/31/12 11:13 AM :: option rename
version 2.0.1 :: 05/31/12 1:52 PM :: option rename fix
version 2.0.2 :: 06/14/12 12:26 PM :: add atlabel option
version 2.0.3 :: 06/19/12 4:42 PM :: fix bug with rb() at() when r(b) is 1x1
