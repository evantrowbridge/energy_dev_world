*** Working Directory: Make sure to update ***
cd "\\tsclient\C\Users\edtro\OneDrive\Documents\GitHub\energy_dev_world\assignment_one"
******

import delimited "discom_data.csv", encoding(ISO-8859-2) clear 

replace unitsconsumed = "" if unitsconsumed == "NA"
destring unitsconsumed, replace

replace assessment = "" if assessment == "NA"
destring assessment, replace

replace totalcollection = "" if totalcollection == "NA"
destring totalcollection, replace

codebook

format %20.0g con_id

sort con_id year month

save "non_collapsed.dta", replace

** Part A **
collapse (sum) unitsconsumed, by(con_id year)

histogram unitsconsumed, xtitle(Annual Electricity Consumption (kWh)) ///
	title("Figure 10: Frequency of Energy Consumption Amounts" "For Unique Customers") ///
	subtitle(Measured in kWh)

use "non_collapsed.dta", clear

collapse (mean) unitsconsumed, by(month)

gen ca_avg = 557

** Part B **
twoway ///
	(line unitsconsumed month, sort) (line ca_avg month, sort), ///
	ytitle(Avg. Consumption (kWh)) ///
	xtitle(Month (1 = January, 12 = December)) ///
	xlabel(#12) ///
	title("Figure 11: Average Monthly Energy Consumption""For Unique Customers") ///
	subtitle(Measured in kWh)

** Part C **
twoway ///
	(line unitsconsumed month, sort) (line ca_avg month, sort), ///
	ytitle(Avg. Consumption (kWh)) ///
	yline(557) ///
	ylabel(0(100)600) ///
	xtitle(Month (1 = January, 12 = December)) ///
	xlabel(#12) ///
	title("Figure 12: Average Monthly Energy Consumption""For Unique Customers") ///
	subtitle(Measured in kWh) ///
	note(	"India values are measured monthly and vary month-to-month." ///
			"California values are measured annually and are constant month-to-month.", ///
			size(medium)) ///
			legend(order(1 "India" 2 "California")) ///
			clegend(on) plegend(on)

** Part D **
use "non_collapsed.dta", clear

gen shortfall = assessment - totalcollection
label variable shortfall "Difference Between Amount Billed and Amount Payed"

save "non_collapsed.dta", replace

collapse (mean) shortfall, by(con_id year)

histogram shortfall, ///
	xtitle(Avg. Monthly Shortfall) ///
	title(Figure 13: Frequency of Average Monthly Shortfalls) ///
	subtitle(For Unique Customers)
	
use "non_collapsed.dta", clear

* Calculating 
gen pay_frac = totalcollection / assessment

replace pay_frac = . if assessment == .

by con_id year: egen avg_pay_frac = mean(pay_frac)

save "non_collapsed.dta", replace

collapse (mean) pay_frac unitsconsumed shortfall, by(con_id year)

count if pay_frac >= 0.95
* Only six households pay more than 95% of their bill on average

egen n_paying_95 = total(pay_frac >= 0.95)
gen percent_paying_95 = n_paying_95 / _N

histogram pay_frac, percent ///
	xtitle(Avg. Percent Paid) ///
	xlabel(#20) ///
	title("Figure 14: Frequency of Average Percent" "of Monthly Bill Paid") ///
	subtitle(For Unique Customers) ///


levelsof(percent_paying_95)
* Only 1.32 out of every 100 households pay at least 95% of what they owe on average

** Part E **
regress unitsconsumed shortfall

twoway (scatter unitsconsumed shortfall, sort) ///
	(lfit unitsconsumed shortfall, lwidth(thick)), ///
	ytitle(Avg. Consumption (kWh)) ///
	xtitle(Avg. Shortfall) ///
	title("Figure 15: Comparing Electricity Consumption" "With Paymen Shortfall") ////
	subtitle(Using Monthly Averages) ///
	legend(order(1 "Unique Customer" 2 "Fitted Values"))

** Part F **
use "non_collapsed.dta", clear

regress pay_frac unitsconsumed

* Making Panel Data Indexed by Year
xtset month

* Time Fixed Effects Regression
xtreg pay_frac unitsconsumed, fe

** Part H **

* Establishing quantiles based on electricity consumption
sort(con_id year)
by con_id year: egen annual_consumption = total(unitsconsumed)

xtile q_consumption = annual_consumption, nq(5)

label define quantile 1 "Lowest" 2 "Low" 3 "Middle" 4 "High" 5 "Hightest"
label values q_consumption quantile

egen total_shortfall = total(shortfall)
label variable total_shortfall "Total Difference Between Amount Billed and Amount Payed"

sort(q_consumption)
by q_consumption: egen quantile_shortfall = total(shortfall)

foreach var in total_shortfall quantile_shortfall {
	replace `var' = . if q_consumption==.
}

gen quantile_fraction = quantile_shortfall / total_shortfall
label variable quantile_fraction "Proportion of Total Shortfall by Avg. Annual Consumption Quintile"

save "non_collapsed.dta", replace

keep q_consumption total_shortfall quantile_shortfall quantile_fraction
duplicates drop

drop if q_consumption==.

graph bar (asis) ///
	quantile_fraction, ///
	over(q_consumption, sort(q_consumption)) ///
	blabel(bar, size(medium) format(%10.3g)) ///
	yscale(off) ///
	title("Figure 16: Proportion of Payment Shortfalls" ///
	"by Average Annual Consumption Quintiles")
	
** Part I **
use "non_collapsed.dta", clear

sort con_id
by con_id: egen total_consumption = total(unitsconsumed)

save "non_collapsed.dta", replace

keep con_id avg_pay_frac total_consumption q_consumption
duplicates drop

sort(q_consumption)
by q_consumption: egen quantile_total_consumption = total(total_consumption)
by q_consumption: egen quantile_avg_pay_frac = mean(avg_pay_frac)

gen profit = 2 * quantile_total_consumption * quantile_avg_pay_frac

keep q_consumption quantile_total_consumption quantile_avg_pay_frac profit
duplicates drop

twoway (connected quantile_total_consumption q_consumption, ///
	sort mcolor("27 158 119") lcolor("27 158 119") ///
	lpattern(dash)) ///
	(connected quantile_avg_pay_frac q_consumption, ///
	sort yaxis(2) mcolor("117 112 179") lcolor("117 112 179") ///
	lpattern(dash)) ///
	(connected profit q_consumption, ////
	sort mcolor("217 95 2") msize(large) lcolor("217 95 2") lwidth(thick)), ///
	ytitle(Consumption (kWh) and Profit (Rs)) ///
	ytitle(Payment Fraction, ///
	axis(2)) ylabel(#4, format(%9.0gc)) ///
	xtitle(Average Annual Consumption Quintiles) ///
	xlabel(, valuelabel) ///
	title("Figure 17: Assessing Potential Profits Based on" ///
	"Consumption and Payment Fraction") ///
	legend(order(1 "Total Consumption" 2 "Profit" 3 "Payment Fraction"))
	
** Part J **
use "non_collapsed.dta", clear

sort con_id month
by con_id: egen avg_indiv_month_consumption = mean(unitsconsumed)
by con_id: egen total_indiv_consumption = total(unitsconsumed)

keep con_id q_consumption total_indiv_consumption avg_indiv_month_consumption
duplicates drop

egen avg_monthly_consumption = mean(avg_indiv_month_consumption)

sort(q_consumption)
by q_consumption: egen quantile_avg_monthly_consumption = mean(avg_indiv_month_consumption)

keep q_consumption avg_monthly_consumption quantile_avg_monthly_consumption

duplicates drop

keep if q_consumption == 4 | q_consumption == 5

* For graphing purposes, combining avg_monthly_consumption 
* and quantile_avg_monthly_consumption
set obs 3
replace q_consumption = 4.5 in 3
egen avg_monthly_consumption_dup = max(avg_monthly_consumption)
replace quantile_avg_monthly_consumption = avg_monthly_consumption_dup in 3
keep q_consumption quantile_avg_monthly_consumption

twoway (bar quantile_avg_monthly_consumption q_consumption, ///
	sort fcolor("217 95 2") barwidth(0.4)), ///
	ytitle(Consumption (kWh)) ytitle(, size(small)) ylabel(0(20)140) ///
	xtitle("") ///
	xlabel(4 "60-80% Quintile (Most Profitable)" ///
	4.5 "Total Population" ///
	5 "80-100% Quintile (For Comparison)", ///
	labsize(small)) ///
	title("Figure 18: Comparing Avg. Monthly Electricity Consumption for" ///
	"Most Profitable Group with Total Popultion Avg.", ///
	size(medlarge)) ///
	subtitle(Also including 80-100% Quintile for Comparison) xsize(6.5)