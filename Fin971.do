## fin 971

##ssc install ftools
##data cleaning and processing
##grab the data from website
##year range from 1965-01 to 2004-01
##generate dataset
use /Users/liuxintian/Desktop/Q1.dta

gen tdebt=dlc+dltt
gen book_leverage=tdebt/at
gen firm_size=log(at)
gen profitabilty=oibdp/at
gen me= prcc_f*csho
gen market_lev=tdebt/(tdebt+me)
gen market_to_book=(me+tdebt+pstkl-txditc)/at
gen collateral=invt+ppent/at
gen z_score=3.3*pi+sale+1.4*re+1.2*(act-lct)/at
gen tangibility=ppent/at
gen intang= intan/at
gen logsale=log(sale)
gen divpayer=0
replace divpayer=1 if dvc>0

destring sic, replace
drop if sic >= 6000 & sic <= 6999
gen sic2 = int(sic/100)

merge m:m GVKEY fyear using /Users/liuxintian/Desktop/crsp.dta
drop _merge

gen mibl = .
replace mibl = 1 if sic >= 100 & sic <= 999
replace mibl = 2 if sic >= 1000 & sic <= 1299
replace mibl = 3 if sic >= 1300 & sic <= 1399
replace mibl = 4 if sic >= 1400 & sic <= 1499
replace mibl = 5 if sic >= 1500 & sic <= 1799
replace mibl = 6 if sic >= 2000 & sic <= 2099
replace mibl = 7 if sic >= 2100 & sic <= 2199
replace mibl = 8 if sic >= 2200 & sic <= 2299
replace mibl = 9 if sic >= 2300 & sic <= 2399
replace mibl = 10 if sic >= 2400 & sic <= 2499
replace mibl = 11 if sic >= 2500 & sic <= 2599
replace mibl = 12 if sic >= 2600 & sic <= 2661
replace mibl = 13 if sic >= 2700 & sic <= 2799
replace mibl = 14 if sic >= 2800 & sic <= 2899
replace mibl = 15 if sic >= 2900 & sic <= 2999
replace mibl = 16 if sic >= 3000 & sic <= 3099
replace mibl = 17 if sic >= 3100 & sic <= 3199
replace mibl = 18 if sic >= 3200 & sic <= 3299
replace mibl = 19 if sic >= 3300 & sic <= 3399
replace mibl = 20 if sic >= 3400 & sic <= 3499
replace mibl = 21 if sic >= 3500 & sic <= 3599
replace mibl = 22 if sic >= 3600 & sic <= 3699	
replace mibl = 23 if sic >= 3700 & sic <= 3799
replace mibl = 24 if sic >= 3800 & sic <= 3879
replace mibl = 25 if sic >= 3900 & sic <= 3999
replace mibl = 26 if sic >= 4000 & sic <= 4799
replace mibl = 27 if sic >= 4800 & sic <= 4829
replace mibl = 28 if sic >= 4830 & sic <= 4899
replace mibl = 29 if sic >= 4900 & sic <= 4949
replace mibl = 30 if sic >= 4950 & sic <= 4959
replace mibl = 31 if sic >= 4960 & sic <= 4969
replace mibl = 32 if sic >= 4970 & sic <= 4979
replace mibl = 33 if sic >= 5000 & sic <= 5199
replace mibl = 34 if sic >= 5200 & sic <= 5999
replace mibl = 35 if sic >= 6000 & sic <= 6999
replace mibl = 36 if sic >= 7000 & sic <= 8999
replace mibl = 37 if sic >= 9000 & sic <= 9999
replace mibl = 38 if !missing(sic) & missing(mibl)
gen mibook_lev=mibl/100


#new cash_flow_vol_tr:

duplicates tag gvkey fyear, gen(isdup)
drop if isdup==1
tsset fyear gvkey
tsegen cash_std= rowsd(L(0/2).oibdp,3)

## winsorize the data
winsor2 book_leverage market_lev logsale market_to_book profitabilty tangibility mibook_lev divpayer intang cash_std, cut(5 95) trim label

keep if fyear >= 1965 & fyear <= 2003
bysort gvkey: gen survivor = _N >= 20
la var survivor "survivor dummy"

## table 1

estpost tabstat book_leverage_tr market_lev_tr logsale_tr market_to_book_tr profitabilty_tr tangibility_tr cash_std_tr mibook_lev_tr divpayer_tr intang_tr, stat(mean median sd) col(stat)

eststo all_firms

qui estpost tabstat book_leverage_tr market_lev_tr logsale_tr market_to_book_tr profitabilty_tr tangibility_tr cash_std_tr mibook_lev_tr divpayer_tr intang_tr if survivor==1, stat(mean median SD) col(stat)
eststo all


c,replace booktabs cells("mean(fmt(%3.2f)) sd(par(( )) fmt(%3.2f))" "p50(par([ ]) fmt(%3.2f))") nomtitles nonumbers label nonotes msign("$-$") 


mgroups("All Firms" "Survivors", pattern(1 1) 
prefix(\multicolumn{@span}{c}{) suffix(})
span erepeat("\cmidrule(lr){@span}")) 
collabels("Mean [Median]" "(\textit{SD})", 
prefix("\multicolumn{@span}{p{1.5cm}}{\centering ") suffix("}")) 
stats(N, fmt("%10.0fc") labels("Obs."))

esttab all_firms survivors using  "/Users/liuxintian/Desktop/table1.tex", replace booktabs cells("Mean(fmt(%3.2f)) SD(par(( )) fmt(%3.2f))" "p50(par([ ]) fmt(%3.2f))") nomtitles nonumbers label nonotes msign("$-$") mgroups("All Firms" "Survivors", pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat("\cmidrule(lr){@span}")) collabels("Mean [Median]" "(\textit{SD})", prefix("\multicolumn{@span}{p{1.5cm}}{\centering ") suffix("}")) stats(N, fmt("%10.0fc") labels("Obs."))



## table 2
## panel A
##step_1
## first we clean the dataset to the panel dataset
## we drop the surplus value 
duplicates tag gvkey fyear, gen(isdup)
drop if isdup==1
destring gvkey, generate(gvkey)
tsset gvkey fyear

##step_2
preserve
collapse (firstnm) initblev = book_leverage_tr, by(gvkey)
tempfile initblev
save `initblev', replace
restore

preserve
collapse (firstnm) initialmlev = market_lev_tr, by(gvkey)
tempfile initmlev
save `initmlev', replace
restore

merge m:1 gvkey using `initblev'
drop _merge

merge m:m gvkey using `initmlev'
drop _merge

foreach v in book_leverage_tr market_lev_tr logsale_tr market_to_book_tr profitabilty_tr tangibility_tr mibook_lev_tr divpayer intang_tr cash_std_tr initblev initmlev{
 	gen lag1_`v' = L.`v'
}

by GVKEY: drop if _n == 1

foreach v of varlist initblev initmlev lag1_* {
	egen `v'_std = std(`v')
}


global control_1 "lag1_logsale_tr lag1_market_to_book_tr_std lag1_profitabilty_tr_std lag1_tangibility_tr_std"
global control2 "lag1_logsale_tr lag1_market_to_book_tr_std lag1_profitabilty_tr_std lag1_tangibility_tr_std lag1_mibook_lev_tr_std lag1_cash_std_tr lag1_divpayer_std"



reghdfe book_leverage_tr initblev_std, noabsorb cl(gvkey)
reghdfe book_leverage_tr initblev_std $control_1, a(fyear) cl(gvkey)
reghdfe book_leverage_tr initblev_std $control2, a(fyear) cl(gvkey)

reghdfe market_lev_tr initmlev_std, noabsorb cl(gvkey)
reghdfe market_lev_tr initmlev_std $control_1, a(fyear) cl(gvkey)
reghdfe market_lev_tr initmlev_std $control2, a(fyear) cl(gvkey)

## panel B
keep if survivor == 1
reghdfe book_leverage_tr initblev_std, noabsorb cl(gvkey)
reghdfe book_leverage_tr initblev_std $control_1, a(fyear) cl(gvkey)
reghdfe book_leverage_tr initblev_std $control2, a(fyear) cl(gvkey)

reghdfe market_lev_tr initmlev_std, noabsorb cl(gvkey)
reghdfe market_lev_tr initmlev_std $control_1, a(fyear) cl(gvkey)
reghdfe market_lev_tr initmlev_std $control2, a(fyear) cl(gvkey)

## Question3
## panel A
reghdfe book_leverage_tr $control_1, a(fyear gvkey)
reghdfe book_leverage_tr $control2, a(fyear gvkey)

reghdfe market_lev_tr $control_1, a(fyear gvkey)
reghdfe market_lev_tr $control2, a(fyear gvkey)

# panel B
keep if survivor == 1
reghdfe book_leverage_tr $control_1, a(fyear gvkey)
reghdfe book_leverage_tr $control2, a(fyear gvkey)

reghdfe market_lev_tr $control_1, a(fyear gvkey)
reghdfe market_lev_tr $control2, a(fyear gvkey)



## Question 4

rename year fyear
keep GVKEY datadate fyear me
merge m:m GVKEY datadate using /Users/liuxintian/Desktop/compustat2.dta
unique me_crsp me
drop _merge
summarize me_crsp
summarize me_crsp


#Question 5
duplicates drop year GVKEY,force
tsset GVKEY year

sort GVKEY 
by GVKEY: gen lag2 = prccm-prccm[_n-1]

ssc install tsegen
tsset GVKEY year
tsegen inv_sd3 = rowsd(L(0/2).lag2,3)
gen sigma_a_t = (me_crsp/(me_crsp+ tdebt))*inv_sd3
reghdfe book_leverage_tr initblev_std sigma_a_t, noabsorb cl(GVKEY)
reghdfe book_leverage_tr initblev_std sigma_a_t $control_1, a(fyear) cl(GVKEY)
reghdfe book_leverage_tr initblev_std sigma_a_t $control2, a(fyear) cl(GVKEY)

reghdfe market_lev_tr initmlev_std sigma_a_t, noabsorb cl(GVKEY)
reghdfe market_lev_tr initmlev_std sigma_a_t, $control_1, a(fyear) cl(GVKEY)
reghdfe market_lev_tr initmlev_std sigma_a_t $control2, a(fyear) cl(GVKEY)
