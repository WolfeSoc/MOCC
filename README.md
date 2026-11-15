Stata code for the analyses in:

Thomeer, Mieke Beth, Joseph D. Wolfe, Rin Reczek, and Dee Ferguson. Forthcoming. "Childbearing Histories and Cognitive Functioning: Do Cohort and Gender Moderate the Association?" *Social Science & Medicine*. https://doi.org/10.1016/j.socscimed.2026.120055

The variables and analyses are described in the paper's Methods section and commented in the do files. This README explains how the files and folders fit together and how to run the code.

**The HRS data are not included in this repository.** HRS does not allow public redistribution of its data files, so each user must register with HRS and download the files (see [Data](#data)).

## Repository structure

```
MOCC/
├── README.md
├── mocc06_data.do        Step 1: data
├── mocc06_models.do      Step 2: models
├── _commands/            statsto.ado and statout.ado
├── _RawData/             Put the three HRS files here (not tracked)
└── _results/             All output is written here
```

## How the code works

1. **Data first.** `mocc06_data.do` reads the three HRS files in `_RawData/` and builds the analysis file, `_results/mocc06.dta`.
2. **Models second.** `mocc06_models.do` reads `_results/mocc06.dta` and produces the descriptive statistics, regression models, tests, and figures.
3. **All output is in `_results/`.** Both do files write their logs, intermediate data files, tables, and figures to this folder.

All file paths are relative, so **Stata's working directory must be the main MOCC folder** (the one containing the two do files) when you run them.

## Requirements

- Stata 19.5 (both do files set `version 19.5`)
- `estout` from SSC (for `esttab`): `ssc install estout`
- `statsto` and `statout` are included in `_commands/`. `mocc06_models.do` loads them with `adopath ++ "_commands"`, so nothing needs to be installed for them.

## Data

`mocc06_data.do` reads these three files from `_RawData/`. The file names must match exactly.

| File in `_RawData/` | HRS product |
|---|---|
| `randhrs1992_2022v1.dta` | RAND HRS Longitudinal File 2022 (V1) |
| `randhrsfamk1992_2022v1.dta` | RAND HRS Family Data 2022 (V1), kid-level file |
| `cogimp9222a_r.dta` | HRS Cross-Wave Imputation of Cognitive Functioning Measures 1992-2022 |

These are the file versions used to produce the results in the paper.

To get HRS data:

1. Download the Stata (`.dta`) version of each file at the HRS data portal, https://hrsdata.isr.umich.edu. 
   - RAND files: https://hrsdata.isr.umich.edu/data-products/rand (under "Longitudinal and Cross-Wave Data Products")
   - Cognition file: https://hrsdata.isr.umich.edu/node/73720
3. Put the `.dta` files in `_RawData/`.

## Running the code

1. Put the three HRS files in `_RawData/`.
2. In Stata, install estout if needed: `ssc install estout`
3. Set the working directory to the main MOCC folder.
4. Run the do files in order:

```stata
do mocc06_data.do
do mocc06_models.do
```

## Contact

Joseph D. Wolfe, Department of Sociology, University of Alabama at Birmingham, jdwolfe@uab.edu
