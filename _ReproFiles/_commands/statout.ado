*! version 0.4.2 :: 06/08/12 3:06 PM :: fix rrename bug

//  Program     : statout
//  Author      : Joseph D. Wolfe
//  Description : format matrices
//  Requirements: Stata 10.1+ (untested on previous versions)

capture clear programs
program define statout, sclass

syntax [anything] [using/],       ///
	[                             ///
								  /// Viewing options
	list                          /// List matrix
	LISTNew                       /// List incoming matrix
	PREView                       /// Preview changes but don't change matrix
	WIDth(numlist >0 integer)     /// Width when listing matrix
	DECimals(numlist >-1 integer) /// Digits when list matrix
	FORMat(string)                /// Format for matrix
	title(string)                 /// title added when listing
    note(string)                  /// add note below matrix
	ROWNames                      /// List rownames of matrix
	COLNames                      /// List colnames of matrix
								  ///
					              /// Matrix utilities
	clear                         /// Reset matrix to null
	save(string)                  /// Save matrix to new matrix
	TRANspose                     /// List/save transposed matrix
	blank                         /// Add a blank row
	mv(string)                    /// Default missing value = .z
	GENerate(string)              /// Create variables from columns
	GENStub(string)               /// Create variables from columns with stub
	basecount(numlist >-1 integer) ///
								  ///
					              /// Edit matrix
	RREName(string asis)          /// Rename rows
	CREName(string asis)          /// Rename columns
	CDROp(string asis)            /// Drop specified columns
	RDROp(string asis)            /// Drop specified rows
	CKEEp(string asis)            /// Keep specified columns
	RKEEp(string asis)            /// Keep specified rows
	CORDer(string asis)           /// Reorder columns to match
	RORDer(string asis)           /// Reorder rows to match
	CNUMber                       /// Add numbers to column name
	RNUMber                       /// Add numbers to row name
	CALPha                        /// Alphabetize columns
	RALPha                        /// Alphabetize rows
								  ///
								  /// File Editing
	REPlace	                      /// Replace previous file (default)
    APPend                        /// Add graphs to existing file
	NOCount                       /// Don't count tables
    DOCument                      /// TEX file is self-contained
	ALIGNment(string)             /// Matrix alignment in file
    PLACEment(string)             /// Position of tex table
    LABel(string)                 /// Add \label{} to tex table
	]

//  #2
//  Set macros and defaults before looping through matrices

*temporary names for matrix/return manipulation
tempname INmat  INtemp INtempmat INtempCNM INtempRNM MAThold tempmat
tempname OUTmat OUTtemp OUTtempmat OUTtempCNM OUTtempRNM TRANmat

* macros for handles and files
tempname ahandle bhandle
tempfile repfile appfile

*some convenient name changes
if `"`using'"'=="" local SaveFile = 0
if `"`using'"'!="" local SaveFile = 1

if `SaveFile' {
	local SaveFileAs `"`using'"'

	* get path and file info for file to save graphs
	_cllct_getFileNameAndPath, fileinfo(`"`SaveFileAs'"')
	local path          `"`s(PathToFile)'"'
	local file_name     `"`s(file_name)'"'
	if `"`path'"'       == "" local path "."

	* get file extension (Ben Jann is the author of _getfilesuffix)
	_getfilesuffix      `"`file_name'"'
	local file_ext      `"`s(suffix)'"'
	local file_dispext   = upper(`"`file_ext'"')

	* if append specified and file not found, add replace option
	qui capture findfile `"`file_name'"', path("`path'")
	local file_exist = _rc
	if `file_exist' == 601 & "`replace'"==""{
		loc replace "replace"
		loc append ""
	}
}

* check if _ststo global has matrix names
if `"`anything'"' == "" {
	if "$_ststo_stored_matrices" == "" {
		di as error ///
		"no stored or specified matrix names"
		exit
	}
	else local matrixlist `"$_ststo_stored_matrices"'
}
if `"`anything'"' != "" local matrixlist `"`anything'"'

* basecount
if "`basecount'" != "" local inmatnum = `basecount'-1
if "`basecount'" == "" local inmatnum =  0

* matlist replaces .z with a blank space - default missing value
if "`mv'" =="" local mv ".z"

* set alignment of title and note (default is differs by file type)
if "`alignment'" == "" {
	if "`file_ext'" == "rtf"  local mtxalign "\ql"
	if "`file_ext'" == "tex"  local mtxalign "center"
	if "`file_ext'" == "html" local mtxalign "left"
}
if "`alignment'" == "center" | "`alignment'" == "c" {
	if "`file_ext'" == "rtf"  local mtxalign "\qc"
	if "`file_ext'" == "tex"  local mtxalign "center"
	if "`file_ext'" == "html" local mtxalign "center"
}
if "`alignment'" == "right"  | "`alignment'" == "r" {
	if "`file_ext'" == "rtf"  local mtxalign "\qr"
	if "`file_ext'" == "tex"  local mtxalign "flushright"
	if "`file_ext'" == "html" local mtxalign "right"
}
if "`alignment'" == "left"   | "`alignment'" == "l"  {
	if "`file_ext'" == "rtf"  local mtxalign "\ql"
	if "`file_ext'" == "tex"  local mtxalign "flushleft"
	if "`file_ext'" == "html" local mtxalign "left"
}

//  #3
//  Begin looping through each matrix listed

local inmatcount : list sizeof matrixlist

while `"`matrixlist'"'!="" & `"`matrixlist'"'!=" " {
	local ++inmatnum
	if `inmatcount'>1 {
		gettoken incomingmatrix matrixlist: matrixlist, parse(" ")
	}
	if `inmatcount'==1 {
		local incomingmatrix `"`matrixlist'"'
		local matrixlist ""
	}
	if `inmatnum'>1 {
		local append "append"
		local replace ""
	}

//  #4
//  Get defaults for each incoming matrix

*if title & nocount not specified make the title the name of the matrix
if "`nocount'"=="" {
	local table "Table"
	local colon ":"
    if `"`title'"' == "" {
    	local ftitle `"`incomingmatrix'"'
	}
}
if "`nocount'"!="" local colon ""

*check if matrix in _ststo exists
capture confirm matrix `incomingmatrix'
if !_rc local mat_exist = 1
if  _rc local mat_exist = 0
if "`clear'" !="" & `mat_exist'{
	capture matrix drop `incomingmatrix'
	local mat_exist     = 0
}

if `mat_exist' {
	matrix `INmat'  = `incomingmatrix'
	matrix `OUTmat' = `incomingmatrix'
}

if !`mat_exist' {
	di as error ///
	"Matrix `incomingmatrix' not found"
	exit 198
}

*get row and column info of matrix
local INmatrowNMS : rowfullnames `INmat', quoted
local INmatcolNMS : colfullnames `INmat', quoted
local INmatrowNUM = rowsof(`INmat')
local INmatcolNUM = colsof(`INmat')

*check to see if drop, keep, order contain numbers or ranges
local count = 0
foreach macval in drop keep order {
	foreach rc in r c {
		local wdrep ""
		local temp         `"``rc'`macval''"'
		local `rc'`macval' `"``rc'`macval''"'
		if "`rc'"=="r" local rowcol "row"
		if "`rc'"=="c" local rowcol "col"

		while `"`temp'"'!="" {
			gettoken mac1  temp : temp , parse(" ")

			*is a list included?
			if regexm(`"`mac1'"', "-") {
				*make sure "-" isn't inside a name
				local parse = 1
				foreach nm of local INmat`rowcol'NMS {
					if `"`mac1'"'==`"`nm'"' {
						local parse = 0
					}
				}

				if `parse' {
					local    strt : subinstr local mac1 "-" " "
					gettoken strt  stp : strt , parse(" ")
					local    strt = trim("`strt'")
					local    stp  = trim("`stp'")

					capture confirm integer number `strt'
					if !_rc local start = `strt'
					if  _rc local start : list posof "`strt'" in INmat`rowcol'NMS
					capture confirm integer number `stp'
					if !_rc local stop  = `stp'
					if  _rc local stop  : list posof "`stp'" in INmat`rowcol'NMS

					*error message if stop is bigger or = to start of range
					if (`start'>=`stop') | (`stop' > `INmat`rowcol'NUM')  {
						di as error "`macval'() contains an invalid range"
						exit 198
					}

					*get names of col/row in range specified
					forvalues num = `start'/`stop' {
						local wd : word `num' of `INmat`rowcol'NMS'
						local wdrep `"`wdrep' `wd'"'
					}

					*replace range with names in range
					local `rc'`macval' : subinstr local `rc'`macval' `"`mac1'"' `"`wdrep'"'
				} // end if `parse'
			} // end if regexm()

			* see if mac1 is a single number; if so replace with name
			capture confirm integer number `mac1'
			if !_rc {
				local tempNMS `"``rc'`macval''"'
				local hold     ""


				*error message if stop is bigger or = to start of range
				if (`mac1' > `INmat`rowcol'NUM')  {
					di as error "`macval'() contains an invalid range"
					exit 198
				}

				while `"`tempNMS'"'!="" {
					local wdrep : word `mac1' of `INmat`rowcol'NMS'
					local eq ""
					local nm ""

					if regexm(`"`tempNMS'"', ":") {
						gettoken eq tempNMS : tempNMS, parse(":")
					}
					gettoken nm tempNMS : tempNMS, parse(" ")

					if `"`eq'"'=="" & `"`nm'"'==`"`mac1'"' {
						local hold `"`hold' `wdrep'"'
					}
					if `"`eq'"'=="" & `"`nm'"'!=`"`mac1'"' {
						local hold `"`hold' `nm'"'
					}
					if `"`eq'"'!="" & `"`nm'"'!=`"`mac1'"' {
						local hold `"`hold' "`eq'"`nm'"'
					}
				}
				local `rc'`macval' `"`hold'"'
			}
		} // end while
	}  // end foreach rc
} // end foreach macval

//  #5
//  Decode Syntax

* preview turns on list
if ("`preview'"=="preview") local list "list"

* if transpose this implies list
if ("`transpose'"=="transpose") local list "list"

* doLIST: list or list new specified?
local doLIST = 0
if ("`list'"=="list" | "`listnew'"=="listnew") local doLIST = 1

* doROWCOL: row column modifications?
local doROWCOL = 0
if `"`cnumber'"'=="cnumber" | `"`corder'"'!="" | `"`cdrop'"'!="" ///
  |`"`rnumber'"'=="rnumber" | `"`rorder'"'!="" | `"`rdrop'"'!="" ///
  |`"`rrename'"'!=""        | `"`calpha'"'!="" | `"`ckeep'"'!="" ///
  |`"`crename'"'!=""        | `"`ralpha'"'!="" | `"`rkeep'"'!="" {
	local doROWCOL = 1
}

//  #6
//  Trap errors

if "`decimals'"!= "" & "`format'"!="" {
	di as error ///
	"decimals() and format() cannot be used together"
	exit 198
}

if `"`nocount'"'!="" & `"`file_ext'"'=="tex" {
	di as error ///
	"nocount cannot be used with TEX files"
	exit 198
}

if !`mat_exist' & "`clear'"=="" {
	di as error ///
	"Matrix `incomingmatrix' is not in memory"
	exit 198
}

if `"`corder'"'!="" & `"`cdrop'"'!="" {
	di as error ///
	"Options corder() and cdrop() cannot be in the same command."
	exit 198
}

if `"`rorder'"'!="" & `"`rdrop'"'!="" {
	di as error ///
	"Options rorder() and rdrop() cannot be in the same command."
	exit 198
}

if `"`corder'"'!="" & `"`ckeep'"'!="" {
	di as error ///
	"Options corder() and ckeep() cannot be in the same command."
	exit 198
}

if `"`cdrop'"'!="" & `"`ckeep'"'!="" {
	di as error ///
	"Options cdrop() and ckeep() cannot be in the same command."
	exit 198
}
if `"`rdrop'"'!="" & `"`rkeep'"'!="" {
	di as error ///
	"Options rdrop() and rkeep() cannot be in the same command."
	exit 198
}

if `"`rorder'"'!="" & `"`rkeep'"'!="" {
	di as error ///
	"You cannot used rorder() and rkeep() in the same command."
	exit 198
}

if `"`ralpha'"'!="" & regexm(`"`INmatrowNMS'"', ":") {
	di as error ///
	"You cannot used raplpha if matrix contains equations."
	 exit 198
}

if `"`calpha'"'!="" & regexm(`"`INmatcolNMS'"', ":") {
	di as error ///
	"You cannot used calpha if matrix contains equations."
	 exit 198
}

if "`mv'"!="" & "`mv'"!="." {
	if length("`mv'")>2 {
		di as error "mv() must be a period or a period with a single letter."
		exit 198
	}
	local mv = lower("`mv'")
	local mv "`mv'"
}

*if user tries to keep,drop, or reorder more rows/cols than mat contains
foreach macval in drop keep order {
	foreach rc in r c {
		if `"``rc'`macval''"'!="" {
			if regexm(`"``rc'`macval''"', ":") {
				local trash : subinstr local `rc'`macval' ":" "", count(local `rc'`macval'num )
			}
			else local `rc'`macval'num : list sizeof `rc'`macval'
			if "`rc'"=="c" & `INmatcolNUM'<``rc'`macval'num' {
				di as error ///
				"`rc'`macval'() contains too many columns"
				exit 198
			}
			if "`rc'"=="r" & `INmatrowNUM'<``rc'`macval'num' {
				di as error ///
				"`rc'`macval'() contains too many rows"
				exit 198
			}
			if "`rc'"=="c" & `INmatcolNUM'==``rc'`macval'num' & "`macval'"=="drop"  {
				di as error ///
				"`rc'`macval'() cannot be used to drop every column"
				exit 198
			}
			if "`rc'"=="r" & `INmatrowNUM'==``rc'`macval'num' & "`macval'"=="drop" {
				di as error ///
				"`rc'`macval'() cannot be used to drop every row"
				exit 198
			}
		}
	}
}

* file found but append and replace not specified
if `SaveFile' {
	if !`file_exist' & "`replace'"=="" & "`append'"=="" {
		di as error ///
		`"`file_name' already exists"'
		exit 602
	}

	* no file extension found
	if `:list sizeof file_ext'==0 {
		di as error ///
		"file extension is missing (rtf, html, csv, or tex)"
		exit 198
	}

	* file creation: no file extension found
	if "`file_ext'" != "rtf" & "`file_ext'" != "html" & "`file_ext'" != "tex" & "`file_ext'" != "csv"{
		di as error ///
		"file extension must be rtf, html, or tex"
		exit 198
	}

	* file creation: can't specify append and replace at same time
	if "`append'" != "" & "`replace'" != "" {
		di as error ///
		"append and replace cannot be used together"
		exit 198
	}
}

* error for placement
if "`placement'"!= "" {
	if "`file_ext'" != "tex" {
		di as error ///
			"placement is only available for TEX files"
			exit 198
	}
	if inlist("`placement'", "h","t","b","p","H") local stop = 0
	else local stop = 1
	if `stop'== 1 {
			di as error ///
			`"`placement' invalid specification for placement"'
			exit 198
	}
}

* error for document option for nontex files
if "`document'" != "" & "`file_ext'" != "tex"  {
	di as error ///
	"document only allowed for TEX files"
	exit 198
}

* label errors
if "`label'" != "" & "`file_ext'"!="tex" {
	di as error ///
	"label() option is only available for TEX files"
	exit 198
}

//  #7
//  Rename rows and columns

* rename
if `"`rrename'"'!="" & `MATexists' {
	RowColRename, names(`rrename') rowcol(row) mat(`OUTmat') ///
				 	inmat(`"`INmatrowNMS'"') rename(yes)
}
if `"`crename'"'!="" & `MATexists' {
	RowColRename, names(`crename') rowcol(col) mat(`OUTmat') ///
				 	inmat(`"`INmatcolNMS'"') rename(yes)
}

//  #8
//  Number columns or rows

if ("`rnumber'"=="rnumber" | "`cnumber'"=="cnumber") ///
	 & `mat_exist' {
	local rowNMS : rownames `OUTmat'
	local colNMS : colnames `OUTmat'
	if "`rnumber'"=="rnumber" {
		local newrowNMS ""
		local i = 0
		foreach nm in `rowNMS' {
			local ++i
			local newrowNMS "`newrowNMS' `i'_`nm'"
		}
		matrix rownames `OUTmat' = `newrowNMS'
	}
	if "`cnumber'"=="cnumber" {
		local newcolNMS ""
		local i = 0
		foreach nm in `colNMS' {
			local ++i
			local newcolNMS "`newcolNMS' `i'_`nm'"
		}
		matrix colnames `OUTmat' = `newcolNMS'
	}
}

//  #9
//  Drop/Keep column or rows

*get matrix in mata
mata: `INtemp'    = st_matrix("`OUTmat'")
mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'")
mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'")

if `"`rdrop'"'!="" & `mat_exist' {
	RowColRename, names(`rdrop') rowcol(row) mat(`OUTmat') ///
				  inmat(`"`INmatrowNMS'"')
	local cdroplist `"`s(cdrop)'"'
	local rdroplist `"`s(rdrop)'"'
	mata: `INtemp'    = select(`INtemp', (`rdroplist'))
	mata: `INtempRNM' = select(`INtempRNM', (`rdroplist'))
	mata: st_matrix("`OUTmat'",`INtemp')
	mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'",`INtempCNM')
	mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'",`INtempRNM')

	*update mata
	mata: `INtemp'    = st_matrix("`OUTmat'")
	mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'")
	mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'")
}

if `"`cdrop'"'!="" & `mat_exist' {
	RowColRename, names(`cdrop') rowcol(col) mat(`OUTmat') ///
				  inmat(`"`INmatcolNMS'"')
	local cdroplist `"`s(cdrop)'"'
	local rdroplist `"`s(rdrop)'"'
	mata: `INtemp'    = select(`INtemp', (`cdroplist'))
	mata: `INtempCNM' = select(`INtempCNM', (`rdroplist'))
	mata: st_matrix("`OUTmat'",`INtemp')
	mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'",`INtempCNM')
	mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'",`INtempRNM')

	*update mata
	mata: `INtemp'    = st_matrix("`OUTmat'")
	mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'")
	mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'")
}

if `"`ckeep'"'!="" & `mat_exist' {
	RowColRename, names(`ckeep') rowcol(col) mat(`OUTmat') ///
				  inmat(`"`INmatcolNMS'"')
	local ckeeplist `"`s(ckeep)'"'
	local rkeeplist `"`s(rkeep)'"'
	mata: `INtemp'    = select(`INtemp', (`ckeeplist'))
	mata: `INtempCNM' = select(`INtempCNM', (`rkeeplist'))
	mata: st_matrix("`OUTmat'",`INtemp')
	mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'",`INtempCNM')
	mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'",`INtempRNM')

	*update mata
	mata: `INtemp'    = st_matrix("`OUTmat'")
	mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'")
	mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'")
}

if `"`rkeep'"'!="" & `mat_exist' {
	RowColRename, names(`rkeep') rowcol(row) mat(`OUTmat') ///
				 	inmat(`"`INmatrowNMS'"')
	local ckeeplist `"`s(rkeep)'"'
	local rkeeplist `"`s(rkeep)'"'
	mata: `INtemp'    = select(`INtemp', (`rkeeplist'))
	mata: `INtempRNM' = select(`INtempRNM', (`rkeeplist'))
	mata: st_matrix("`OUTmat'",`INtemp')
	mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'",`INtempCNM')
	mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'",`INtempRNM')

	*update mata
	mata: `INtemp'    = st_matrix("`OUTmat'")
	mata: `INtempCNM' = st_matrixcolstripe("`OUTmat'")
	mata: `INtempRNM' = st_matrixrowstripe("`OUTmat'")
}

//  #10
//  Rearrange Matrix

*reorder columns or rows
if `"`corder'"'!="" & `mat_exist'==1 {
	RowColReorder, names(`corder') rowcol(col) mat(`OUTmat') ///
				 	inmat(`"`INmatcolNMS'"')
}
if `"`rorder'"'!="" & `mat_exist'==1 {
	RowColReorder, names(`rorder') rowcol(row) mat(`OUTmat') ///
				 	inmat(`"`INmatrowNMS'"')
}

* alphabetize
if "`ralpha'"!="" & `mat_exist'==1 {
	local rowalpha : rownames `OUTmat'
	local rowalpha : list sort rowalpha
	foreach nm in `rowalpha' {
		GetRowColIndex, name(`nm') matrix(`OUTmat') rowcol(row)
		local MATrownum `s(MATrownum)'
		matrix `tempmat' = nullmat(`tempmat') \ `OUTmat'[`MATrownum',1...]
	}
	matrix `OUTmat' = `tempmat'
	capture matrix drop `tempmat'
}

if "`calpha'"!="" & `mat_exist'==1 {
	local colalpha : colnames `OUTmat'
	local colalpha : list sort colalpha
	foreach nm in `colalpha' {
		GetRowColIndex, name(`nm') matrix(`OUTmat') rowcol(col)
		local MATcolnum `s(MATcolnum)'
		matrix `tempmat' = nullmat(`tempmat') , `OUTmat'[1...,`MATcolnum']
	}
	matrix `OUTmat' = `tempmat'
}

//  #11
//  Add Blank Row

if "`blank'" != "" {
	local cnum               = colsof(`OUTmat')
	matrix `INtemp'          = J(1,`cnum',`mv')
	matrix rownames `INtemp' = "----"
	mat `OUTmat' = `OUTmat' \ `INtemp'
}

//  #12
//  Adjust formatting

*error if too many decimals specified
local colnum = colsof(`OUTmat')
local rownum = rowsof(`OUTmat')

local fmtnum : list sizeof format
local decnum : list sizeof decimals
local wthnum : list sizeof width

local fmtloc "format decimals width"

foreach mloc in fmtnum decnum wthnum {
	gettoken fmtlocnm fmtloc : fmtloc

	if ``mloc'' != `colnum' & ``mloc''>1 {
		di as error ///
		"You have specified an incorrect number of columns in `fmtlocnm'()"
		exit 198
	}
}

if `"`inmatnum'"'=="1" {
	if "`width'"!= "" & "`format'"!="" {
		di as error ///
		"width() and format() cannot be used together"
		exit 198
	}
	if `"`width'"'=="" local width "9"

	if `"`format'"'=="" & `"`decimals'"'=="" & `"`width'"'!="" {
		local format "%`width'.0g"
	}

	if `"`width'"'=="" local twidth = 12    // row title width
	if `"`width'"'!="" local twidth = `width'

	if "`decimals'"!= "" {
		if `"`width'"'=="" local width = 9
		local decnum : list sizeof local decimals

		*get equations for loops below
		local ceq : coleq `OUTmat', quoted

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
	if "`format'"!= "" & `mat_exist' {
		local formnum : list sizeof local format
		local colnum = colsof(`OUTmat')
		local rownum = rowsof(`OUTmat')

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
} // end if inmatnum=1

//  #13
//  List, save, transpose matrix

*update title
if "`nocount'"=="" & `"`title'"'=="" {
	local ltitle `"Table `inmatnum': `incomingmatrix'"'
}
if "`nocount'"=="" & `"`title'"'!="" {
	local ltitle `"Table `inmatnum': `title'"'
}
if "`nocount'"!="" {
	local ltitle `"`title'"'
}

*display transposed matrix - list is assumed
if "`transpose'"!="" & ("`list'"!="" | "`list'"=="") {
	matrix  `TRANmat' = (`OUTmat')'
	matlist `TRANmat', nodotz border(top bottom) title("`title'") ///
					  `newformat'
	if "`save'"!="" matrix  `save' = `TRANmat'
}

if "`list'"!="" & "`transpose'"=="" {
	matlist `OUTmat', nodotz border(top bottom) title("`ltitle'") `newformat'
	if "`save'"!="" matrix  `save' = `OUTmat'
}

if "`preview'"!="" matrix `OUTmat' = `INmat'
matrix `incomingmatrix' = `OUTmat'

//  #14
//  Generate columns from Matrix

if `"`genstub'"'!="" & `"`generate'"'=="" {
	local generate "_all"
}

if `"`generate'"' != "" {
	*set temp matrix to work with
	mat `INtemp' = `OUTmat'

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

	if `"`genstub'"'!="" {
		local colnms : colnames `INtemp'
		local newcolnms ""
		foreach nm of local colnms {
			local newcolnms `"`newcolnms' `genstub'`nm'"'
		}
		mat colnames `INtemp' = `newcolnms'
	}

	if `_allcol'  {
		svmat `INtemp', names(col)
	}

	if !`_allcol' {
		local colnms : colnames `INtemp'
		local eqnms  : coleq `INtemp'

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
			if !_rc mat `tempmat' = `OUTmat'[1...,`nm']
			if  _rc mat `tempmat' = `OUTmat'[1...,"`nm'"]

			if `"`genstub'"'!="" {
				local cnm : colnames `tempmat'
				local cname `"`genstub'`cnm'"'
				mat colnames `tempmat' = `cname'
			}
			svmat `tempmat', names(col)
		}
	}
} // end if generate

//  #15
//  Display col/row names

capture confirm matrix `OUTmat'
if !_rc {
//	if "`preview'"=="" matrix `OUTmat'=(`OUTmat')'

	*clear s returns before adding row and column names
	sreturn clear
	local rfullnames : rowfullnames `OUTmat', quoted
	local cfullnames : colfullnames `OUTmat', quoted
	sreturn local colnames `"`cfullnames'"'
	sreturn local rownames `"`rfullnames'"'

	if `"`colnames'"'!="" {
		di as text `"Col Names: `cfullnames'"'
	}

	if `"`rownames'"'!="" {
		di as text `"Row Names: `rfullnames'"'
	}
}

//  #16
//  Save matrix/matrices to specified file

if `SaveFile' {

//  #17
//  Update locals before sending to file

*get row and column info of matrix
local INmatrowNMS   : rownames `INmat', quoted
local INmatcolNMS   : colnames `INmat', quoted
local INmatroweqNMS : roweq `INmat', quoted
local INmatcoleqNMS : coleq `INmat', quoted
local INmatrowNUM   = rowsof(`INmat')
local INmatcolNUM   = colsof(`INmat')

* check if row equations exist
local reqexist = 0
local tmproweq  `"`INmatroweqNMS'"'
while `"`tmproweq'"'!="" {
	gettoken reqchk tmproweq : tmproweq, parse(" ")
	local    reqchk : subinstr local reqchk " " "", all
	if `"`reqchk'"'!=`"_"'	local ++reqexist
}

* check if col equations exist
local ceqexist = 0
local tmpcoleq  `"`INmatcoleqNMS'"'
while `"`tmpcoleq'"'!="" {
	gettoken ceqchk tmpcoleq : tmpcoleq, parse(" ")
	local    ceqchk : subinstr local ceqchk " " "", all
	if `"`ceqchk'"'!=`"_"'	local ++ceqexist
}

* Get total number of columns that will be in table
local TOTcols = `INmatcolNUM' + 1 // add an empty cell for (1,1)
if `reqexist' local TOTcols = `TOTcols' + 1

//  #18
//  RTF: locals to create file

if "`file_ext'" == "rtf" {
	*clear locals
	local cellsize  ""
	local rowline0  ""
	local rowline00 ""
	local rowcell0  ""
	local rowcell00 ""
	local bborder   ""
	local tborder   ""
	local tbborder  ""
	local open      ""
	local body1     ""
	local body2     ""
	local body3     ""
	local close     ""
	local reqspace  ""
	forvalues i = 1/`INmatrowNUM' {
		local rowline`i' ""
	}

	* prepare options
	if `inmatnum'==1 {
		local open   "{\rtf"
		if `"`note'"' != "" local note "`note' \line"
	}
	* Add code that's necessary before each row
	forvalues j = 1/`TOTcols' {
		if `j'==1 {
			local cellsize `"\trowd\trgaph180\cellx1440"'
			local size = 1440
			local bborder `"\trowd\trgaph180\clbrdrb\brdrw15\brdrs \cellx1440"'
			local tborder `"\trowd\trgaph180\clbrdrt\brdrw15\brdrs \cellx1440"'
			local tbborder `"\trowd\trgaph180\clbrdrt\brdrw15\brdrs\clbrdrb \brdrw15\brdrs\cellx1440"'
		}
		if `j'>1  {
			local size = `size' + 1440
			local cellsize `"`cellsize'\cellx`size'"'
			local bborder  `"`bborder'\clbrdrb\brdrw15\brdrs\cellx`size'"'
			local tborder  `"`tborder'\clbrdrt\brdrw15\brdrs\cellx`size'"'
			local tbborder `"`tbborder'\clbrdrt\brdrw15\brdrs\clbrdrb \brdrw15\brdrs\cellx`size'"'
		}
	}

	* create large part of body2 macro with matrix info
	forvalues i = 1/`INmatrowNUM' {
		* Add column names and column equation names
		if `i'==1 {
			local prevceq ""
			local prevreq ""

			* Add empty column for row equation
			if `reqexist' local reqspace "\pard\intbl\cell"

			* Add column equations if they exist
			if `ceqexist' {
				while `"`INmatcoleqNMS'"'!=`""' {
					gettoken ceq1 INmatcoleqNMS : INmatcoleqNMS, parse(" ")
					local    ceq2 : subinstr local ceq1 " " "", all
					if `"`ceq2'"'==`"`prevceq'"' local cellceq `"`'"'
					if `"`ceq2'"'!=`"`prevceq'"' local cellceq `"{`ceq1'}"'
					local rowline0 `"`rowline0'\pard\intbl `cellceq'\cell"'
					local prevceq `"`ceq2'"'
				}
				local rowline0 `"{`tborder' `reqspace'  \pard\intbl\cell `rowline0'\row}"'
			}

			*Add column names next
			while `"`INmatcolNMS'"'!=`""' {
				gettoken cnm INmatcolNMS : INmatcolNMS, parse(" ")
				local rowline00 `"`rowline00' \pard\intbl {`cnm'}\cell"'
			}
			if  `ceqexist' local border `"`bborder'"'
			if !`ceqexist' local border `"`tbborder'"'

			local rowline00 `"{`border' `reqspace' \pard\intbl\cell `rowline00'\row}"'
		}
		* Get rows of matrix
		forvalues j = 1/`INmatcolNUM' {
			if `"`decimals'"'!="" {
				if `decnum' > 1 {
					local pdec : word `j' of `decimals'
				}
				else local pdec = `decimals'
				local pformat "%`width'.`pdec'f"

			}
			if `"`format'"'!="" {
				local wcount = wordcount("`format'")
				if `wcount' > 1 {
					local pformat : word `j' of `format'
				}
				else local pformat "`format'"
			}
			*Add row equations and row names
			if `j'==1 {
				* Add row equations if they exist
				if `reqexist' {
					gettoken req1 INmatroweqNMS : INmatroweqNMS, parse(" ")
					local    req2 : subinstr local req1 " " "", all
					if `"`req2'"'==`"`prevreq'"' local cellreq `""'
					if `"`req2'"'!=`"`prevreq'"' local cellreq `"{`req1'}"'
					local rowcell0 `"\pard\intbl `cellreq'\cell"'
					local prevreq `"`req2'"'
				}

				*Add rowumn names next
				gettoken rnm INmatrowNMS : INmatrowNMS, parse(" ")
				local rowcell00 `"\pard\intbl {`rnm'}\cell"'
			}
			local cellnum = `INmat'[`i',`j']
			local cellnum : di `pformat' `cellnum'
			local cellnum = trim("`cellnum'")
			if `"`cellnum'"'==".z" local cellnum ""
			local rowline`i' `"`rowline`i'' \pard\intbl {`cellnum'}\cell"'
		}

		if `i'==`INmatrowNUM' {
			local celllayout `"`bborder'"'
		}
		if `i'!=`INmatrowNUM' {
			local celllayout `"`cellsize'"'
		}
		local rowline`i' `"{`celllayout' `rowcell0' `rowcell00' `rowline`i''\row}"'
		local body2 `"`body2' `rowline`i''"'
	}

	local body2 `"{`rowline0' `rowline00' `body2'"'
	local body3 `"{\pard \fs20 \`mtxalign' `note' \par} \par}"'
	local close    `"}"'
}

//  #19
//  HTML: locals to create file

if "`file_ext'" == "html" {
	*clear locals
	local cellsize  ""
	local rowline0  ""
	local rowline00 ""
	local rowcell0  ""
	local rowcell00 ""
	local bborder   ""
	local tborder   ""
	local tbborder  ""
	local body1     ""
	local body2     ""
	local body3     ""
	local reqspace  ""
	forvalues i = 1/`INmatrowNUM' {
		local rowline`i' ""
	}

	* prepare options
	if `inmatnum'==1 {
		* html uses %, so need to remove decimal plae on scale
		if "`scale'"!= "" local scale = `scale'*100

		* size options
		if "`height'"!="" & "`width'"=="" local size `"height=`height'"'
		if "`height'"=="" & "`width'"!="" local size `"width=`width'"'
		if "`height'"!="" & "`width'"!="" local size `"height=`height' width=`width'"'
		if "`scale'"!="" local size `"width=`scale'%"'

		* alignment option
		loc mtxalign `"<div style="text-align: `mtxalign';">"'

		* title & note option
		if `"`note'"' !="" local note  `"<tr><td colspan="`TOTcols'">`note'</td></tr>"'

		local open   `"<html><head><style type="text/css"> table.sample {border-collapse: collapse;} td{padding: 5px} tr.bottom td{border-bottom:thin solid black;} tr.top td{ border-top: thin solid black;} tr.note td{}</style>  <html><body>"'

		local close  "</body></html>"
	}

	* create large part of body2 macro with matrix info
	forvalues i = 1/`INmatrowNUM' {
		* Add column names and column equation names
		if `i'==1 {
			local prevceq ""
			local prevreq ""

			* Add empty column for row equation
			if `reqexist' local reqspace "<td> </td>"

			* Add column equations if they exist
			if `ceqexist' {
				while `"`INmatcoleqNMS'"'!=`""' {
					gettoken ceq1 INmatcoleqNMS : INmatcoleqNMS, parse(" ")
					local    ceq2 : subinstr local ceq1 " " "", all
					if `"`ceq2'"'==`"`prevceq'"' local cellceq `" "'
					if `"`ceq2'"'!=`"`prevceq'"' local cellceq `"`ceq1'"'
					local rowline0 `"`rowline0' <td> `cellceq' </td>"'
					local prevceq `"`ceq2'"'
				}
				local rowline0 `"<tr class="top"> `reqspace' <td> </td> `rowline0' </tr>"'
			}

			*Add column names next
			while `"`INmatcolNMS'"'!=`""' {
				gettoken cnm INmatcolNMS : INmatcolNMS, parse(" ")
				local    cnm : subinstr local cnm " " "", all
				local rowline00 `"`rowline00' <td> `cnm' </td>"'
			}
			if  `ceqexist' local tborder `"<tr class="bottom">"'
			if !`ceqexist' local tborder `"<tr class="top bottom">"'

			local rowline00 `"`tborder' `reqspace' <td> </td> `rowline00' </tr>"'
		}
		* Get rows of matrix
		forvalues j = 1/`INmatcolNUM' {
			if `"`decimals'"'!="" {
				if `decnum' > 1 {
					local pdec : word `j' of `decimals'
				}
				else local pdec = `decimals'
				local pformat "%`width'.`pdec'f"

			}
			if `"`format'"'!="" {
				local wcount = wordcount("`format'")
				if `wcount' > 1 {
					local pformat : word `j' of `format'
				}
				else local pformat "`format'"
			}
			*Add row equations and row names
			if `j'==1 {
				* Add row equations if they exist
				if `reqexist' {
					gettoken req1 INmatroweqNMS : INmatroweqNMS, parse(" ")
					local    req2 : subinstr local req1 " " "", all
					if `"`req2'"'==`"`prevreq'"' local cellreq `" "'
					if `"`req2'"'!=`"`prevreq'"' local cellreq `"`req1'"'
					local rowcell0 `"<td> `cellreq' </td>"'
					local prevreq `"`req2'"'
				}

				*Add rowumn names next
				gettoken rnm INmatrowNMS : INmatrowNMS, parse(" ")
				local    rnm : subinstr local rnm " " "", all
				local rowcell00 `"<td>`rnm'</td>"'
			}
			local cellnum = `INmat'[`i',`j']
			local cellnum : di `pformat' `cellnum'
			local cellnum = trim("`cellnum'")
			if `"`cellnum'"'==".z" local cellnum ""
			local rowline`i' `"`rowline`i'' <td>`cellnum'</td>"'
		}

		if `i'==`INmatrowNUM' {
			local bborder `"<tr class="bottom">"'
		}
		local rowline`i' `"<tr> `bborder'`rowcell0' `rowcell00' `rowline`i''</tr>"'
		local body2 `"`body2' `rowline`i''"'
	}

	local body2 `"`rowline0' `rowline00' `body2'`note'"'
	local body3 `"</table> <br> </div> "'
}

//  #20
//  CSV: locals to create file

if "`file_ext'" == "csv" {
	*clear locals
	local cellsize  ""
	local rowline0  ""
	local rowline00 ""
	local rowcell0  ""
	local rowcell00 ""
	local bborder   ""
	local tborder   ""
	local tbborder  ""
	local body1     ""
	local body2     ""
	local body3     ""
	local close     ""
	local reqspace  ""
	forvalues i = 1/`INmatrowNUM' {
		local rowline`i' ""
	}

	forvalues i = 1/`INmatrowNUM' {
		* Add column names and column equation names
		if `i'==1 {

			* Add empty column for row equation
			if  `reqexist' local reqspace `"="","'
			if !`reqexist' local reqspace ""

			* Add column equations if they exist
			if `ceqexist' {
				local colnum = 0
				while `"`INmatcoleqNMS'"'!=`""' {
					local ++colnum
					local c ","
					if `colnum'==`INmatcolNUM' local c ""
					gettoken ceq1 INmatcoleqNMS : INmatcoleqNMS, parse(" ")
					local    ceq2 : subinstr local ceq1 " " "", all
					if `"`ceq2'"'==`"`prevceq'"' local cellceq `"=""`c'"'
					if `"`ceq2'"'!=`"`prevceq'"' local cellceq `"="`ceq1'"`c'"'
					local rowline0 `"`rowline0'`cellceq'"'
					local prevceq `"`ceq2'"'
				}
				local rowline0 `"`reqspace'="",`rowline0'"'
			}

			*Add column names next
			local colnum = 0
			while `"`INmatcolNMS'"'!=`""' {
				local ++colnum
				local c ","
				if `colnum'==`INmatcolNUM' local c ""
				gettoken cnm INmatcolNMS : INmatcolNMS, parse(" ")
				local rowline00 `"`rowline00'="`cnm'"`c'"'
			}
				local rowline00 `"`reqspace'="",`rowline00'"'
		}
		* Get rows of matrix
		local colnum = 0
		forvalues j = 1/`INmatcolNUM' {
			if `"`decimals'"'!="" {
				if `decnum' > 1 {
					local pdec : word `j' of `decimals'
				}
				else local pdec = `decimals'
				local pformat "%`width'.`pdec'f"

			}
			if `"`format'"'!="" {
				local wcount = wordcount("`format'")
				if `wcount' > 1 {
					local pformat : word `j' of `format'
				}
				else local pformat "`format'"
			}

			* No comma after last column
			local ++colnum
			local c ","
			if `colnum'==`INmatcolNUM' local c ""

			*Add row equations and row names
			if `j'==1 {
				* Add row equations if they exist
				if `reqexist' {
					gettoken req1 INmatroweqNMS : INmatroweqNMS, parse(" ")
					local    req2 : subinstr local req1 " " "", all
					if `"`req2'"'==`"`prevreq'"' local cellreq `"""`c'"'
					if `"`req2'"'!=`"`prevreq'"' local cellreq `"="`req1'"`c'"'
					local rowcell0 `"`cellreq'"'
					local prevreq `"`req2'"'
				}

				*Add rowumn names next
				gettoken rnm INmatrowNMS : INmatrowNMS, parse(" ")
				local rowcell00 `"="`rnm'"`c'"'
			}
			local cellnum = `INmat'[`i',`j']
			local cellnum : di `pformat' `cellnum'
			local cellnum = trim("`cellnum'")
			if `"`cellnum'"'==".z" local cellnum ""
			local rowline`i' `"`rowline`i''="`cellnum'"`c'"'
		}
		local rowline`i' `"`rowcell0'`rowcell00'`rowline`i''"'
	}
}

//  #21
//  TEX: locals to create file

if "`file_ext'" == "tex" {
	*clear locals
	*clear locals
	local cellsize  ""
	local rowline0  ""
	local rowline00 ""
	local rowcell0  ""
	local rowcell00 ""
	local bborder   ""
	local tborder   ""
	local tbborder  ""
	local body1     ""
	local body2     ""
	local body3     ""
	local reqspace  ""
	forvalues i = 1/`INmatrowNUM' {
		local rowline`i' ""
	}

	* options
	local space  "_n"
	if `inmatnum'==1 {
		if `"`placement'"' !="" local place "[`placement']"
		if `"`placement'"' =="" local place ""
		if `"`document'"'  !="" {
			local open   `"\documentclass{article} `tex_brpkg' \begin{document}"'
			local close  `"\end{document}"'
		}
		if `"`label'"' != "" local label `"\label{`label'}"'
		if `"`note'"'  != "" {
			local note : subinstr local note "-" "\-", all
			local note : subinstr local note "_" "\_", all
			local note `"\multicolumn{`TOTcols'}{l}{\footnotesize `note'} \\"'
		}
	}

	* create large part of body2 macro with matrix info
	forvalues i = 1/`INmatrowNUM' {
		* Add column names and column equation names
		if `i'==1 {
			local prevceq ""
			local prevreq ""

			* Add empty column for row equation
			if `reqexist' local reqspace "\multicolumn{1}{c}{} &"

			* Add column equations if they exist
			if `ceqexist' {
				local colnum = 0
				while `"`INmatcoleqNMS'"'!=`""' {
					local ++colnum
					local c "&"
					if `colnum'==`INmatcolNUM' local c "\\"
					gettoken ceq1 INmatcoleqNMS : INmatcoleqNMS, parse(" ")

					*replace _ with \_ for tex file
					local    ceq1 : subinstr local ceq1 "_" "\_", all
					*remove space to compare with prev eq
					local    ceq2 : subinstr local ceq1 " " "", all

					*if equation is the same print space
					if `"`ceq2'"'==`"`prevceq'"' {
						local cellceq `"\multicolumn{1}{c}{} `c'"'
					}
					if `"`ceq2'"'!=`"`prevceq'"' {
						local cellceq `"\multicolumn{1}{c}{`ceq1'} `c'"'
					}
					local rowline0 `"`rowline0' `cellceq'"'
					local prevceq `"`ceq2'"'
				}
				local rowline0 `"`reqspace' \multicolumn{1}{c}{} & `rowline0' \hline"'
			}

			*Add column names next
			local colnum=0
			while `"`INmatcolNMS'"'!=`""' {
				local ++colnum
				local c "&"
				if `colnum'==`INmatcolNUM' local c "\\"
				gettoken cnm INmatcolNMS : INmatcolNMS, parse(" ")
				local    cnm : subinstr local cnm "_" "\_", all
				local rowline00 `"`rowline00' \multicolumn{1}{c}{`cnm'} `c'"'
			}
			local rowline00 `"`reqspace' \multicolumn{1}{c}{} & `rowline00' \hline"'
		} // end if `i'==1

		* Get rows of matrix
		local colnum = 0
		forvalues j = 1/`INmatcolNUM' {
			if `"`decimals'"'!="" {
				if `decnum' > 1 {
					local pdec : word `j' of `decimals'
				}
				else local pdec = `decimals'
				local pformat "%`width'.`pdec'f"
			}
			if `"`format'"'!="" {
				local wcount = wordcount("`format'")
				if `wcount' > 1 {
					local pformat : word `j' of `format'
				}
				else local pformat "`format'"
			}
			local ++colnum
			local c "&"
			if `colnum'==`INmatcolNUM' local c "\\"

			*Add row equations and row names
			if `j'==1 {
				* Add row equations if they exist
				if `reqexist' {
					gettoken req1 INmatroweqNMS : INmatroweqNMS, parse(" ")
					*replace _ with \_ for tex file
					local    req1 : subinstr local req1 "_" "\_", all
					local    req2 : subinstr local req1 " " "", all
					if `"`req2'"'==`"`prevreq'"' local cellreq `"&"'
					if `"`req2'"'!=`"`prevreq'"' local cellreq `" `req1' `c'"'
					local rowcell0 `"`cellreq'"'
					local prevreq `"`req2'"'
				}

				*Add rowumn names next
				gettoken rnm INmatrowNMS : INmatrowNMS, parse(" ")
				local    rnm : subinstr local rnm "_" "\_", all
				local rowcell00 `"`rnm' `c'"'
			}
			local cellnum = `INmat'[`i',`j']
			local cellnum : di `pformat' `cellnum'
			local cellnum = trim("`cellnum'")
			if `"`cellnum'"'==".z" local cellnum ""
			local rowline`i' `"`rowline`i'' `cellnum' `c'"'
		}

		if `i'==`INmatrowNUM' {
			local celllayout `"`bborder'"'
		}
		if `i'!=`INmatrowNUM' {
			local celllayout `"`cellsize'"'
		}
		local rowline`i' `"`rowcell0' `rowcell00' `rowline`i''"'
		local body1 `"`body1' `rowline`i''"'
	}

	local body2 `"`note'"'
	local body3 `"`label' \end{tabular} `label' \end{`mtxalign'} \end{table}}"'

}

//  #22
//  Append file (based on Eng 2007 and Jann 2007)

if "`append'" != "" {
	_cllct_appendFILE, ahandle(`ahandle') bhandle(`bhandle') ///
		  appfile(`appfile') savefile(`SaveFileAs') ///
		 file_ext(`file_ext') tabnum(`tabnum')
	 local tabnum `"`s(tabnum)'"'
}

//  #23
//  Replace/create file

if (`"`file_ext'"'=="rtf") | (`"`file_ext'"'=="tex") |  (`"`file_ext'"'=="html") {
	local ospace "_n _n"
	local cspace "_n"
}
if `"`file_ext'"'=="csv" {
	local ospace "_n"
	local cspace ""
}

if "`replace'" != "" {
	qui file open  `ahandle' using `repfile', replace write
	file write `ahandle' `"`open'"' `ospace'
}

//  #24
//  Add matrix to file

if "`file_ext'" != "csv" {
	if ("`nocount'" == "") local tabnum = `tabnum' + 1
	if "`file_ext'"=="rtf" 	{
		local body1 `"{\pard \fs24 \`mtxalign' `table' `tabnum'`colon' `title'`ftitle' \par}"'
	}
	if "`file_ext'"=="html" {
		local body1  "<table class="sample"> <CAPTION> `mtxalign' `table' `tabnum'`colon' `title'`ftitle' <CAPTION/>"
	}
	if "`file_ext'"=="tex" {
		if `"`title'"'  != "" {
			local title : subinstr local title "-" "\-", all
			local title : subinstr local title "_" "\_", all
			local title : subinstr local title "#" "\#", all
		}
		local body1 `"{\begin{table} \begin{`mtxalign'} \caption{`title'`ftitle'} \begin{tabular}{l*{`TOTcols'}{c}}   `place' \hline `rowline0' `rowline00' `body1' \hline"'
	}
	file write `ahandle' `"`body1'"' _n `"`body2' `body3' `body4'"' `space'
}

if "`file_ext'" == "csv" {
	if ("`nocount'" == "") {
		local tabnum = `tabnum' + 1
		local newtitle `"Table `tabnum'`colon' `title'`ftitle'"'
	}

	forvalues i = 1/`INmatrowNUM' {
		local header1 ""
		local header2 ""
		local nline1  ""
		local nline2  ""
		local nspace  ""
		local tspace  ""
		local lspace  "_n"
		if `i'==1 {
			if `"`rowline0'"'!=`""' {
				local header1 `"`rowline0'"'
				local nline1 "_n"
			}
			local header2 `"`rowline00'"'
			local nline2  "_n"
			if `"`newtitle'"'!="" local tspace "_n"
		}
		if `i'>1 {
			local newtitle ""
			if `i'==`INmatrowNUM' local lspace ""
		}

		if `i'< `INmatrowNUM' local note2 ""
		if `i'==`INmatrowNUM' {
			local note2 `"`note'"'
			if `"`note'"'!="" local nspace "_n"
		}

		file write `ahandle' `"`newtitle'"'`tspace'`"`header1'"'`nline1' `"`header2'"'`nline2'`"`rowline`i''"'`lspace'`nspace'`"`note2'"'
	}
}

file write `ahandle' `cspace' `"`close'"' `cspace'

//  #25
//  Save file

file close `ahandle'

if "`append'"   != "" {
	file close `bhandle'
	copy `"`appfile'"' `macval(SaveFileAs)', replace
}

if "`replace'"  != "" {
	copy `"`repfile'"'  `macval(SaveFileAs)', replace
}

//  #26
//  End "if" SaveFile

}

//  #27
//  End "while" loop through matrices

}

//  #28
//  Display link to file

if `inmatcount'==1 & `SaveFile' {
	di as text `"(matrix added to {browse `file_name'})"'
}
if `inmatcount' >1 & `SaveFile' {
	di as text `"(matrices added to {browse `file_name'})"'
}

//  #29
//  End statout

end

//  #0
//  Subroutines ------------------------------------------------------------


//  #1
//  Rename rows/col and return index of row/col to keep/delete

program RowColRename, sclass

syntax ,                           ///
	[							   ///
	names(string asis)             /// user defined row or column names
	rowcol(string)                 /// is row or col?
	mat(string)                    /// name of matrix
	inmat(string)                  /// names of row/col of matrix
	rename(string)                 /// renaming row/col?
	]

sreturn clear

*create locals of row/col names to edit
local INmat`rowcol'NMS `"`inmat'"'
local NEW`rowcol'NMS   `"`inmat'"'
if "`rowcol'" == "row" local rc "r"
if "`rowcol'" == "col" local rc "c"

*create local for keeping/droping columns/rows
local rsplit "\"
local csplit ","

_cllct_parseNMS, innames(`names')
local IN`rowcol'NMS  `"`s(IN_NMS)'"'
local OUT`rowcol'NMS `"`s(OUT_NMS)'"'

if !regexm(`"`names'"', "=") & "`rename'"=="yes" {
	*if names contain eq count colons
	if regexm(`"`names'"', ":") {
		local trash: subinstr local names count(local nmsnum)
	}

	*if no equations just count names
	if !regexm(`"`names'"', ":") {
		local nmsnum : list sizeof names
	}

	*error if number names != number of rows or columns
	local tmpnum = `rowcol'sof(`mat')
	if `nmsnum'!=`tmpnum' {
		di as error ///
		"Incorrect number of names specified in `rc'rename()"
		exit 198
	}
	local NEW`rowcol'NMS `"`names'"'
}

if regexm(`"`names'"', "=") | "`rename'"=="" {
	local IN`rowcol'NMS  `"`s(IN_NMS)'"'
	local OUT`rowcol'NMS `"`s(OUT_NMS)'"'
	local rcexist `"`IN`rowcol'NMS'"'
	local rcexist : subinstr local rcexist "_:" "", all

	local stop  = `rowcol'sof(`mat')
	local count = 0

	*replace incoming with outgoing names
	foreach matnm of local INmat`rowcol'NMS {
		local OUTnms `"`OUT`rowcol'NMS'"'
		local INnms  `"`IN`rowcol'NMS'"'
		local cdropnm ""
		local ckeepnm ""
		local rdropnm ""
		local rkeepnm ""
		local ++count

		while `"`OUTnms'"'!="" {
			gettoken outchk1 OUTnms : OUTnms, parse(":")
			gettoken outchk2 OUTnms : OUTnms, parse(" ")
			local    outchk2        : subinstr local outchk2 ":" ""
			if `"`outchk1'"'!="_" & `"`outchk1'"'!=`":"' {
				local out `""`outchk1'":`outchk2'"'
			}
			else local out `"`outchk2'"'

			gettoken inchk1 INnms : INnms, parse(":")
			gettoken inchk2 INnms : INnms, parse(" ")
			local    inchk2       : subinstr local inchk2 ":" ""
			if `"`inchk1'"'!="_" & `"`inchk1'"'!=`":"'{
				local in    `""`inchk1'":`inchk2'"'
				local inrep `"`inchk1':`inchk2'"'
			}
			else local in `"`inchk2'"'

			*if no eq specified replace all names
			if !regexm(`"`in'"', ":") & !regexm(`"`out'"', ":") & regexm(`"`matnm'"', ":") {
				gettoken trash matnm : matnm, parse(":")
				gettoken trash matnm : matnm, parse(":")
			}

			if `count'==`stop' {
				local csplit ""
				local rsplit ""
			}


			*v042: fix margins row error
	  		if `"`in'==`matnm'"'==`"1._at==1bn._at"' {

				local NEW`rowcol'NMS : subinstr local NEW`rowcol'NMS `"`in'"' `"1bn._at"'

				local NEW`rowcol'NMS : subinstr local NEW`rowcol'NMS `"1bn._at"' `"`out'"'

				local rcexist : subinstr local rcexist `"`in'"'    ""
				if `"`inrep'"'!="" {
					local rcexist : subinstr local rcexist `"`inrep'"' ""
				}

				*make local for cols & rows to keep or drop
				local cdropnm "0`csplit'"
				local ckeepnm "1`csplit'"
				local rdropnm "0`rsplit'"
				local rkeepnm "1`rsplit'"
			}

			if `"`in'"'==`"`matnm'"' {
				local NEW`rowcol'NMS : subinstr local NEW`rowcol'NMS `"`in'"' `"`out'"'

				local rcexist : subinstr local rcexist `"`in'"'    ""
				if `"`inrep'"'!="" {
					local rcexist : subinstr local rcexist `"`inrep'"' ""
				}

				*make local for cols & rows to keep or drop
				local cdropnm "0`csplit'"
				local ckeepnm "1`csplit'"
				local rdropnm "0`rsplit'"
				local rkeepnm "1`rsplit'"
			}

			if `"`in'"'!=`"`matnm'"' {
				if "`cdropnm'"=="" local cdropnm "1`csplit'"
				if "`ckeepnm'"=="" local ckeepnm "0`csplit'"
				if "`rdropnm'"=="" local rdropnm "1`rsplit'"
				if "`rkeepnm'"=="" local rkeepnm "0`rsplit'"
			}
		} // end while
		local rkeep  "`rkeep' `rkeepnm'"
		local rdrop  "`rdrop' `rdropnm'"
		local ckeep  "`ckeep' `ckeepnm'"
		local cdrop  "`cdrop' `cdropnm'"
	} // end foreach matnm
}


*error if row not found
if `"`rcexist'"'!="" & `"`rcexist'"'!=" " {
	local rcenum : list sizeof rcexist
	if `rcenum' != 0 {
		di as error ///
		`"`rowcol'(s) `rcexist' not found"'
		exit 198
	}
}

*change names of matrices
mat `rowcol'names `mat' = `NEW`rowcol'NMS'

sreturn local ckeep `"`ckeep'"'
sreturn local cdrop `"`cdrop'"'
sreturn local rkeep `"`rkeep'"'
sreturn local rdrop `"`rdrop'"'
sreturn local rcloc `"`rcloc'"'

end

//  #2
//  Reorder col/rows based on user specification

program RowColReorder, sclass
syntax ,                           ///
	[							   ///
	names(string asis)             /// user defined row or column names
	rowcol(string)                 /// is row or col?
	mat(string)                    /// name of matrix
	inmat(string)                  /// names of row/col of matrix
	]

sreturn clear
tempname INtemp

*create locals of row/col names to edit
local INmat`rowcol'NMS `"`inmat'"'

_cllct_parseNMS, innames(`names')
local IN`rowcol'NMS  `"`s(IN_NMS)'"'
local OUT`rowcol'NMS `"`s(OUT_NMS)'"'
local OUTnms         `"`OUT`rowcol'NMS'"'
local INnms          `"`IN`rowcol'NMS'"'

*find position of column or row in matrix
while `"`OUTnms'"'!="" {
	local count = 0
	gettoken outchk1 OUTnms : OUTnms, parse(":")
	gettoken outchk2 OUTnms : OUTnms, parse(" ")
	local    outchk2           : subinstr local outchk2 ":" ""
	if `"`outchk1'"'!="_" & `"`outchk1'"'!=`":"' {
		local out `""`outchk1'":`outchk2'"'
	}
	else local out `"`outchk2'"'

	gettoken inchk1 INnms : INnms, parse(":")
	gettoken inchk2 INnms : INnms, parse(" ")
	local    inchk2          : subinstr local inchk2 ":" ""
	if `"`inchk1'"'!="_" & `"`inchk1'"'!=`":"'{
		local in `""`inchk1'":`inchk2'"'
	}
	else local in `"`inchk2'"'

	foreach matnm of local INmat`rowcol'NMS {
		local ++count

		*if no eq specified replace all names
		if !regexm(`"`in'"', ":") & !regexm(`"`out'"', ":") & regexm(`"`matnm'"', ":") {
			gettoken trash matnm : matnm, parse(":")
			gettoken trash matnm : matnm, parse(":")
		}

		if `"`in'"'==`"`matnm'"' {
			local rcloc `"`rcloc' `count'"'
		}
	}
}

local `rowcol'num = `rowcol'sof(`mat')

if "`rowcol'" == "col" {
	forvalues rc = 1/``rowcol'num' {
		tempname tempmat`rc'
		mat `tempmat`rc'' = `mat'[1...,`rc']
	}

	local wcount = wordcount("`rcloc'")

	forvalues rc = 1/`wcount' {
		local pos : word `rc' of `rcloc'
		if `rc'==1 {
			mat `INtemp' = `tempmat`pos''
		}
		if `rc'>1 {
			mat `INtemp' = `INtemp' , `tempmat`pos''
		}
	}
}

if "`rowcol'" == "row" {
	forvalues rc = 1/``rowcol'num' {
		tempname tempmat`rc'
		mat `tempmat`rc'' = `mat'[`rc',1...]
	}

	local wcount = wordcount("`rcloc'")

	forvalues rc = 1/`wcount' {
		local pos : word `rc' of `rcloc'
		if `rc'==1 {
			mat `INtemp' = `tempmat`pos''
		}
		if `rc'>1 {
			mat `INtemp' = `INtemp' \ `tempmat`pos''
		}
	}
}

mat `mat' = `INtemp'

end

//  #3
//  Return an index of row and column names

program GetRowColIndex, sclass

syntax ,                 ///
	[                    ///
	name(string)         /// names of row or column
	matrix(string)       /// name of matrix
	rowcol(string)       /// row or column
	]

sreturn clear
local MAT`rowcol'NMS : `rowcol'names `matrix'
local MAT`rowcol'num : list posof "`name'" in MAT`rowcol'NMS
sreturn local MAT`rowcol'num `"`MAT`rowcol'num'"'

end

//  #4
//  Get file extension [taken directly from esttab (Ben Jann)]

program _getfilesuffix, sclass
    version 8
    gettoken filename rest : 0
    if `"`rest'"' != "" {
        exit 198
    }
    local hassuffix 0
    gettoken word rest : filename, parse(".")
    while `"`rest'"' != "" {
        local hassuffix 1
        gettoken word rest : rest, parse(".")
    }
    if `"`word'"'=="." {
        di as err `"incomplete filename; ends in ."'
        exit 198
    }
    if index(`"`word'"',"/") | index(`"`word'"',"\") local hassuffix 0
    if `hassuffix' sreturn local suffix `"`word'"'
    else           sreturn local suffix ""
end // _getfilesuffix

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


exit // exit program

//  version history --------------------------------------------------------

version 0.1.0 2011-02-17 initial conversion
version 0.1.1 2011-04-01 gen stub fix
version 0.1.2 2011-04-04 use _clct_ListMatrix.ado
version 0.2.0 05/24/11 :: JDW fixes
version 0.3.0 01/25/12 8:04 AM :: Update for equations
version 0.3.1 01/28/12 5:56 PM :: Write to file
version 0.3.2 03/30/12 11:32 AM :: bugs
version 0.3.3 04/03/12 4:18 PM :: more bug fixes
version 0.3.6 04/08/12 3:55 PM
version 0.3.7 04/09/12 10:36 AM
version 0.3.8 05/25/12 10:04 AM :: fix format bug; fix bug with keep()
version 0.3.9 05/27/12 10:25 AM :: fix dec() bug
version 0.4.0 05/27/12 1:21 PM :: change global name to _ststo_stored_matrices
version 0.4.1 :: 05/31/12 3:54 PM :: basecount option added
