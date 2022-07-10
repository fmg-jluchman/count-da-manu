/*
Project: 	Training/PD: Count dominance  
Purpose: 	Analysis in support of count DV dominance analysis research manuscript
Author: 	Joe Luchman
Created:	11/2/2016
Machine: 	FMGL172


~~~~	Legend		~~~~

*	txt				= 	short notes
//syntax 			= 	commented out syntax - kept for posterity or convenience
/* txt */ 			= 	long notes or descriptions
* ~~ #) txt ~~ *	=	numbered headings

~~~~	~~~~~~		~~~~
*/


* ~~ 1) set-up phase ~~ *
/*version 12.1

project, doinfo
local ppath "`r(pdir)'"
local pname "`r(pname)'"

project, relies_on("`ppath'/Documentation/projectlog_`pname'.txt")	/* project notes/log - ensures they're up-to-date */
project, relies_on("`ppath'/`pname'_lists.txt")	/* include lists of ordered, crossing, and interest variables */
capture which lmoremata.mlib	/* implementation requires -moremata- from SSC */
if _rc end	/* forces failure if -moremata- not present */


* ~~ 2) data management phase ~~ *
project, do("`ppath'/dt_mgnt/`pname'_dt_mgnt.do")


* ~~ 3) confidentiality analysis phase ~~ *
project, do("`ppath'/analysis/`pname'_analysis.do")


* ~~ 4) remerge and finalize phase ~~ *
project, do("`ppath'/produce_final/`pname'_produce_final.do")

*/

use "C:\Users\jluchman\Desktop\explmntl\relimportance\count_domin\nls79_masterORM.dta", clear

generate une_flag = cond(emp_stat == 2, 1, .)

generate nlf_flag = cond(emp_stat == 3, 1, .)

generate emp_miss = cond(emp_stat < 5, 1, .)

egen une_sum = count(une_flag), by(ID)

egen nlf_sum = count(nlf_flag), by(ID)

egen valid_emps = count(emp_miss), by(ID)

by ID: drop if _n > 1

poisson une_sum  LOC i.GENDER AFQT nlf_sum, exposure(valid_emps)

domin une_sum LOC AFQT nlf_sum, reg(poisson, exposure(valid_emps)) fitstat(e(r2_p)) sets((i.GENDER))
