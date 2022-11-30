# calcal-lisp - Compute and convert dates, all in Common Lisp

## About
The primary motivation for writing _calcal-lisp_ was to take first steps in [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) and to learn how to distribute Common Lisp applications. The result of this endeavour is a command line application (`calcal`) that computes and converts dates from 11 calendars, consisting of the Gregorian, ISO, Julian, Islamic, Hebrew, Mayan (long count, haab, tzolkin), French Revolutionary, and Old Hindu (solar, lunar) calendars. 

The calendrical functions are directly taken from: 

- [Dershowitz, Nachum, and Edward Reingold. 1990. "Calendrical Calculations", Software - Practice and Experience, 20 (9), 899-928](https://citeseerx.ist.psu.edu/doc_view/pid/83b114f570002a7a8e1d4e3730cd0e4cdbcad212) and 

- [Reingold, Edward, Nachum Dershowitz, and Stewart Clamen. 1993. "Calendrical Calculations, II: Three Historical Calendars", Software - Practice & Experience, 23 (4), 383-404.](https://citeseerx.ist.psu.edu/doc_view/pid/11dbb1d457750c50c19d87b6b1254df23c17c059)

The Lisp source code is available at https://www.cs.tau.ac.il/~nachum/calendar-book/papers/calendar.l.

Please note: The generated binary is quite large (10-40 MB, depending on the Lisp implementation). This is due to the fact that the binary consists of the image of the Lisp session in which _calcal_ was compiled as well as the Common Lisp runtime. Other Lisp implementations may provide means to reduce the binary size.

## Prequisites
Building _calcal_ requires `make`, `patch`, and a Common Lisp implementation (tested with [SBCL](https://www.sbcl.org/), [Clozure CL](https://ccl.clozure.com/), and [CLISP](https://www.gnu.org/software/clisp/)). 

## Build and install
To build, clone this repository, and run `make`.

```sh
git clone github.com/staudtlex/calcal-lisp 
cd calcal-lisp
# with SBCL (default)
make
# with Clozure CL
make LISP=ccl64
# with CLISP
make LISP=clisp
```

In order for the command line (e.g. `bash`) to be able find _calcal_, move it somewhere on `$PATH`, or modify `$PATH`.

## Usage
To compute the dates corresponding to the current date (e.g. 2022-01-12), simply run `calcal` on the command line.
```
$ calcal
Please note:
- for dates before 1 January 1 (Gregorian), calcal may return incorrect
  ISO and Julian dates
- for dates before the beginning of the Islamic calendar (16 July 622
  Julian), calcal returns an invalid Islamic date
- for dates before the beginning of the French Revolutionary calendar
  calendar (22 September 1792), calcal returns an invalid French date
- Old Hindu (solar and lunar) calendar dates may be off by one day

Gregorian               12 January 2022                 
ISO                     2022-W02-3                      
Julian                  30 December 2021                
Islamic                 8 Jumada II 1443                
Hebrew                  10 Shevat 5782                  
Mayan Long Count        13.0.9.3.9                      
Mayan Haab              7 Muan                          
Mayan Tzolkin           11 Muluc                        
French Revolutionary    23 Nivôse an 230                
Old Hindu Solar         28 Dhanus 5122                  
Old Hindu Lunar         10 Pausha 5122        
```
Run `calcal --help` to show all available options (for binaries built with _CLISP_, you may need to run `calcal -- --help` instead).
```
$ calcal --help
Usage: calcal [options]
Options:
  --calendar  comma-separated list of calendars.
              Currently, calcal supports:
                all             all calendars listed below (default)
                gregorian       Gregorian calendar
                iso             ISO calendar
                julian          Julian calendar
                islamic         Islamic calendar
                hebrew          Hebrew calendar
                mayanLongCount  Mayan Long Count calendar
                mayanHaab       Mayan Haab calendar
                mayanTzolkin    Mayan Tzolkin calendar
                french          French Revolutionary calendar
                oldHinduSolar   Old Hindu Solar calendar
                oldHinduLunar   Old Hindu Lunar calendar
  --date      date (format: yyyy-mm-dd). When omitted, '--date'
              defaults to the current date (2022-01-12)
```

## Limitations
_calcal_ currently only supports converting _Gregorian calendar dates_ into ISO, Julian, Islamic, Hebrew, Mayan (long count, haab, tzolkin), French Revolutionary, and Old Hindu (solar, lunar) dates. Converting dates _from_ any of those calendars is not supported (yet).

The calendrical functions used in _calcal_ do not reliably support dates before 1 January 1 (Gregorian) and/or dates before the beginning of a calendar's epoch. Hence, 

- for dates before 1 January 1 (Gregorian), _calcal_ may return incorrect ISO and Julian dates

- for dates before the beginning of the Islamic calendar (16 July 622 Julian/19 July 622 Gregorian), _calcal_ returns an invalid Islamic date (currently displayed as "`0 NIL 0`" on stdout)

- for dates before the beginning of the French Revolutionary calendar calendar (22 September 1792), _calcal_ returns an invalid French date (currently displayed as "`NIL NIL an NIL`")

- Old Hindu (solar and lunar) calendar dates may be off by one day
