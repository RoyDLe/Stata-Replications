/********************
MICROECONOMETRICS 
*********************/

*** Program: Repl_Lalonde_Experiments.do
*** first created: 23/02/2024
*** last updated:  07/03/2024



/*----------------------
Initial script configuration
-----------------------*/
	//Please replace your path in the command below
	cd "C:\Users\"
	
	// Preparing the necessary dataset
	clear all
	use jtrain2.dta
	
	// Installing the necessary packages.
	ssc install outreg2
	ssc install randtreat
	ssc install dmout
	ssc install ritest
	
/*----------------------
Part 1 
-----------------------*/
	* 1.a *
quietly {
	// Label Management for proper execution of dmout command
	label variable age "in years 1997"
	label variable re74 "'75 earnings $1000s '82"
	label variable re75 "'75 earnings $1000s '82"
	label variable black "1 when black"
	label variable hisp "1 when hispanic"
	label variable educ "years of education"
	label variable nodegree "1 if no high school degree"
	
	// Generating the balance table in csv file
    eststo treatment: estpost summarize age educ black hisp nodegree re74 re75 if train==1
    eststo control: estpost summarize age educ black hisp nodegree re74 re75 if train==0
    eststo groupdiff: estpost ttest age educ black hisp nodegree re74 re75, by(train)		
	esttab treatment control groupdiff using table1_main.csv ,  replace ///
    cell( ///
        mean(pattern(1 1 0) fmt(3)) & b(pattern(0 0 1) fmt(3)) ///
        sd(pattern(1 1 0) fmt(3)) & se(pattern(0 0 1) fmt(2)) ///
    ) mtitle("Training" "Control" "Difference  Means")

	// Providing an alternative way of creating the balance table
	* Note: an alternative, more compact way of performing these actions is using the command
	// dmout age educ black hisp nodegree re74 re75 using TABLE_1, by(train) replace // *
	
	// Comment about the balancing of the variables
	* Most variables are balanced at the standard levels of significance, but not all of them. Namely, there is a 1% significant difference between people with no high school degree, that receive the training treatment less than people with a high school degree. It might be reasonable to assume that this is caused by some kind of selection bias. This can be due to the fact that assignment is non random. Further, there is a 10% significant difference between the hispanics and non hispanic. Hispanics appear to receive the treatment less frequently. *
}
	* 1.b *
quietly {
	// Running the regression between earnings and training
	reg re78 train, robust
	
	// Storing coefficients as scalars
	scalar b_train=_b[train]
	scalar sd_train=_se[train]
	
	//Commenting on the coefficients
	* All things being equal, people that get assigned into the job training earned, on average, 1794$ (in real annual terms) more than someone that has not been assigned to the job training. This appears to be in line with the idea that someone who is more trained gets rewarded more money for their work, and is a first hint at the fact that the treatment might be effective. It can also be noted that the standard error is generally high and the confidence interval relatively large. This implies that while one can say that there is an effect, it is challenging to determine the precize size of it based on the previous analysis. *
}
	* 1.c *
quietly {		
	//Calculating the number of treated 
	summarize train if train == 1
	local num_treated = r(N)

	//Calculating the number of controls
	summarize train if train == 0
	local num_control = r(N)

	//Performing the three requested regressions and adding the output in a table 
	reg re78 train, robust
	outreg2 using TABLE_2.xls, replace ctitle(Spec 1) addstat(N_Treated,`num_treated', N_Control,`num_control')
	reg re78 train age educ black hisp, robust
	outreg2 using TABLE_2.xls, append ctitle(Spec 2) addstat(N_Treated,`num_treated', N_Control,`num_control')
	reg re78 train age educ black hisp re74 re75, robust
	outreg2 using TABLE_2.xls,  append ctitle(Spec 3) addstat(N_Treated,`num_treated', N_Control,`num_control')
	
	// Commenting on the results after the adding of covariates
	* The results do not appear to be distinctively sensitive to the adding of covariates, as the level of significance remains under the 5% level and the coefficient does not change substantially, along with the standard erorrs. However, it should be noted that indeed, even from the second regressions and the addition of the education, ethnicity and age variables, the p-value increases. Further, as expected, the R-squared increases significantly with the addition of the covariates. *
}
	* 1.d *
quietly {
	// Calculating the dfbetas and creating the corresponding variable
	reg re78 train age educ black hisp re74 re75
	dfbeta train
	gen influence_train = _dfbeta_1

	// Running a loop to drop influential observations and run a regression afterwards with the reduced sample
	local droppings "3 2 5" 
	foreach i of local droppings {
		gsort- influence_train
		drop in 1/`i'
		sort influence_train
		drop in 1/`i'
		reg re78 train age educ black hisp re74 re75
		if `i' == 3 {
			outreg2 using tabledf.xls, replace
    }
    else {
        outreg2 using tabledf.xls, append
	}
	}
	//Commenting on the results of the three regressions
	* The results appear to be slightly sensitive to influential observations. The removal of outliers leads to a decrease in the absolute value of the coefficients as more observations are removed, going from an initial value of 1680$ in the original regression down to 1022$ to the one with 20 observations removed. However the significance of the training in affecting annual earnings is persistent, increasing to a 1% significance level after the first dropping and going back to 5% in the subsequent ones. 
}
/*----------------------
Part 2
-----------------------*/
	* 2.a *
quietly {
	// Preparing the new dataset
	use jtrain3.dta, clear
	
	// Label Management for proper execution of dmout command
	label variable age "in years 1997"
	label variable re74 "'75 earnings $1000s '82"
	label variable re75 "'75 earnings $1000s '82"
	label variable black "1 when black"
	label variable hisp "1 when hispanic"
	label variable educ "years of education"
	
	// Calculating the number of treated and control observations
	summarize train if train ==1 
	local num_treat=r(N)
	summarize train if train ==0
	local num_control=r(N)
 
	 // Generating the balance table in csv file 
	eststo treatment: estpost summarize age educ black hisp re74 re75 if train==1
    eststo control: estpost summarize age educ black hisp re74 re75 if train==0
    eststo groupdiff: estpost ttest age educ black hisp re74 re75, by(train)
	esttab treatment control groupdiff using table1_jt3.csv ,  replace ///
    cell( ///
        mean(pattern(1 1 0) fmt(3)) & b(pattern(0 0 1) fmt(3)) ///
        sd(pattern(1 1 0) fmt(3)) & se(pattern(0 0 1) fmt(2)) ///
    ) mtitle("Training_jt3" "Control_jt3" "Difference  Means_jt3")

	// Commenting on the balancing of the variables 
	* The table seems not to be balanced in none of the variables except for the hispanic proportion. It is expected since most of the new control was not part of the original randomization process.  *

}

	* 2.b *
quietly {
	// Generating the random treatment
	set seed 12345
	gen rand=runiform()
	gsort - rand
	gen rank=_n
	gen treated=1 if rank<2675/2
	replace treated=0 if treated==.
}
	* 2.c *
quietly {
	// Generating the second random treatment 
	randtreat, gen(treated_2) misfit(strata) 
	
	// Checking statistically significant correlation
	pwcorr treated treated_2, sig 
	
	// Commenting on the significance
	* Since Pvalue=0.2090, the correlation is not significant. This allow us to perform the comparative analysis in the following questions of this sections without the risk of having a correlation that distort the results*
	
}
	* 2.d *
quietly {	 
	// Creating the balance table using the fake treatment in csv file	 
	eststo treatment: estpost summarize age educ black hisp re74 re75 if treated==1
    eststo control: estpost summarize age educ black hisp re74 re75 if treated==0
    eststo groupdiff: estpost ttest age educ black hisp re74 re75, by(treated)	
	esttab treatment control groupdiff using table1_rand3.csv ,  replace ///
    cell( ///
        mean(pattern(1 1 0) fmt(3)) & b(pattern(0 0 1) fmt(3)) ///
        sd(pattern(1 1 0) fmt(3)) & se(pattern(0 0 1) fmt(2)) ///
    ) mtitle("Training_rand3" "Control_rand3" "Difference  Means_rand3")
	 
	// Commenting on the findings of the balance table with the fake treatment
	*  The new treatment is more balanced across all variables in the sample. There is no selection bias, being the variable completely randomized by the software and not being an actual real life treatment. *
}
	* 2.e *
quietly {
	//Calculating the number of controls
	summarize train if train == 1
	local num_treated = r(N)

	//Calculating the number of controls
	summarize train if train == 0
	local num_control = r(N)
		
	// Running the first regression with the randomized treatment
	reg re78 treated, robust
	outreg2 using TABLE_2.xls, append ctitle(Fake Treatment 1) addstat(Treated, `num_treated', Control, `num_control')

	// Running the second regression with the randomized treatment
	reg re78 treated age educ black hisp, robust
	outreg2 using TABLE_2.xls, append ctitle(Fake Treatment 2) addstat(Treated, `num_treated', Control, `num_control')
	
	// Running the third regression with the randomized treatment
	reg re78 treated age educ black hisp re74 re75, robust
	outreg2 using TABLE_2.xls, append ctitle(Fake Treatment 3) addstat(Treated, `num_treated', Control, `num_control')
	
	// Commenting on the results of the regression
	* With the random treatment, one does not get any significant results in the random variable. This is to be expected since the treatment is fictitious and there is no actual real world impact of assignation of trainings. *
}
	* 2.f *
quietly {
	// Running the first regression with the actual treatment
	reg re78 train, robust
	outreg2 using TABLE_2.xls, append ctitle(Actual Treatment 1) addstat(N_Treated, `num_treated', N_Control, `num_control')
	
	// Running the second regression with the actual treatment
	reg re78 train age educ black hisp, robust
	outreg2 using TABLE_2.xls, append ctitle(Actual Treatment 2) addstat(N_Treated, `num_treated', N_Control, `num_control')
	
	// Running the third regression with the actual treatment
	reg re78 train age educ black hisp re74 re75, robust
	outreg2 using TABLE_2.xls, append ctitle(Actual Treatment 3) addstat(N_Treated, `num_treated', N_Control, `num_control')
	
	// Commenting on the difference between the results with the first three columns
	* The regression has a significative negative value that dissipates when the income of previous years are considered. The key difference probably relies on the unbalances present when using the larger sample, since we are including observations that was not part of the randomization process. The effect of the covariates is relevant in the specifications. Adding the sociodemographic variables cuts the effect in half, while adding the income variables nullifies completely the train effect. This is to be expected for 2 reasons: first, the new control was not part of the original randomization design(which could lead to potential selection bias) and also the noted earlier that there is no balance in covariates.  *
}


/*----------------------
Part 3
-----------------------*/
	* 3.a *
quietly {
/* 
Neyman proposed using the difference in average outcomes by treatment status as an estimator for the average treatment effect. The unbiasedness of Neyman's estimator under heterogeneous treatment effects is ensured by several conditions: 
	
- It accounts for the fixed potential outcomes in the population, thus keeping fixed the total number of units assigned to treatment and control. 
- Complete randomization in treatment assignment. This to ensure balance and comparability of the heterogeneity between the control and treatment groups, and making the expectation of the treatment assignment indicators equals zero. Also, by the increasing precision and reliability of estimations with larger sample sizes.
- No interference between units, also referred to as the Stable Unit Treatment Value Assumption (SUTVA). This means that the outcome observed for any individual is assumed to be a direct result of the treatment condition, unaffected by the conditions assigned to other units. 
- Finally, the variance estimation reflects the variability in treatment effects across the population and is thereby crucial for the unbiased inference on average treatment effects under heterogeneity, accounting for the variability in individual responses.
*/
}
	* 3.b *
quietly {
	quietly{
/*Fisher's inference tests the sharp null hypothesis of zero treatment effects against the alternative of treatment effects. The idea behind the procedure is to rerandomize the assignment of treatment and controls iteratively and calculate the (absolute) value of the statistic with each iteration to obtain the distribution of the statistic under the null hypothesis. The exact distribution of the statistic is obtained calculating the value of the statistic for every possible alternative reassignment, which in some cases is computationally challenging. The p-value is the fraction of reassignments that lead to a statistic at least as large as the observed value of the statistic in absolute value. Randomization inference does not assume random sampling from the population and as such does not rely on large sample theory. The prerequisite to carry out randomization inference is to have knowledge of the randomization process*/ 
}


*Homemade algorithm*

	//Initialize variables: # permutations, # treatment units, observed mean (threshold) value, counter variable for p-value
	set seed 8888
	local perm = 10000
	sca treatment_fixed = 185
	local threshold_y = 1.79
	sca p_count = 0

	//Initialize variables: reassigment variable of treatment status, random number
	gen reassigned_treatment = .
	gen random_number = runiform()

//Main Analysis Loop
forvalues i=1/`perm' {
	replace random_number = runiform()
	sort random_number //sort data by the generated random number 
	replace reassigned_treatment = .
	replace reassigned_treatment = 1 if _n <= treatment_fixed //assign to treatment the first 185 individuals sorted by random number, and the rest to control
	replace reassigned_treatment = 0 if _n > treatment_fixed 

	quietly: summ re78 if reassigned_treatment==1
	sca mean_t=r(mean) //obtain the means from the summaries
	quietly: summ re78 if reassigned_treatment==0
	sca mean_c=r(mean)
	sca abs_diff = abs(mean_t - mean_c) //take the absolute value of the statistic
	display abs_diff
	
if abs_diff >= `threshold_y' { 
		sca p_count = p_count + 1 //increment the count when statistic >= 1.79
	}
}
//Output the p-value
display p_count / `perm'


/*Note: the randomization procedure used to reassign individuals with each permutation does not replicate the randomization procedure in Lalonde. Ideally we would have to know the exact randomization process. Also, since the number of possible reassignments is very large (6.08 * 10^129) (no clustering / stratification), deriving the exact distribution of the statistic is computationally infeasible.

We arrive at a similar but not the same p-value as Athey & Imbens. Reasons for the difference may be the fact that unless you compute the exact distribution of the statistic by running all possible treatment reassignments, not every run-through results in the same reassignment vectors (it is random). It is also not clear how many permutations were run in Athey&Imbens, which may also be a reason for the differences. */


*Alternative: ritest as described in Hess to obtain p-value for simple difference in means of treated and control*
ritest train _b[train], reps(1000): regress re78 train
}
	* 3.c *
quietly {
/*
Lalonde (1986) specifies that there is certain heterogeneity within the treatment. They mention, for example, that there were specific trainings applied in different local sites. Since the procedure in Athey & Imbens (2017) does not account for the possibility of heterogeneity of effects, the randomization performed in the permutation would not be a precise measure of a different scenario, leading to a different distribution of the statistic.
	
Furthermore, according to Hess, to obtain the exact distribution of the statistic under the sharp null hypothesis, you have to compute the value of the statistic for each possible alternative assignment of the treatment. This was most likely not done by Athey & Imbens since the total number of possible assignments is in the order of magnitude of 10^129. Therefore, Athey & Imbens (as well as our own procedure) technically obtained an approximate p-value. 

The Fisherian inference method does not take into account sample attrition, which the LaLonde paper acknowledges as a potential source of bias. By ignoring attrition, the Fisherian p-values might not accurately reflect the uncertainty about treatment effects. 

The accuracy of the p-value depends on the number of simulated reassignments. If too few simulations are conducted, the p-value may not be precise. At the same time, the number of shuffling iterations used is omitted from the research output, not allowing for a completely transparent re-evaluation of the results and thus undermining replicability. 

*/
}
	* 3.d.1 *
quietly {
/*
Data Colada post evaluates the supposed superiority of randomization tests over regression analyses, as most research conclude, and conclude that it is actually a consequence of using Stata's default HC1 method rather than an inherent advantage of randomization tests themselves. In fact, when robust standard errors are calculated using HC3 instead of HC1, the performance of regression analyses in terms of type I rates become comparable to, or even better than, randomization tests.

HC1 is method for computing robust standard errors adjusts for heteroskedasticity, a condition where the variance of the error terms is not constant across observation which can invalidate the standard errors and consequently  the p-values. This method, particularly for small samples, does not provide the most accurate adjustment. 

In HC1 estimation uses the term (n/(n-k)) , which is a degrees-of-freedom correction for small sample sizes. This allows to adjust for the bias in the estimation of variance that occurs when the sample size is not significantly larger than the number of parameters being estimated.

On the other hand, the HC3 method provides a more aggressive adjustment for small samples (N < 250), where the risk of heteroskedasticity affecting the results is higher. This aims to reduce the bias in standard error estimates more effectively than HC1, maintaining type I error rate closer to the expected level (for instance 5%). The formula for HC3 is modified by a term Omega* (Omega star) with each diagonal element being the squared residual divided by 1 - h_i^2 (h of observation i squared) which adjusts the squared residuals by the leverage of the observations (leverage quantifies the potential influence of that observation on the fitted values, and on the regression model itself).

This adjustment makes HC3 more robust in small samples by giving less weight to observations with high leverage, which are those observations that have an excessive influence on the regression coefficients.
*/
}
	* 3.d.2 *
quietly {
	// Preparing the correct dataset for the analysis
	use jtrain2.dta, clear
	
	// Calculating the Treated and Control sizes for the next regressions
	summarize train if train ==1 
	local num_treat=r(N)
	summarize train if train ==0
	local num_control=r(N)
	
	// Performing the three regressions using HC3
	reg re78 train, vce(hc3)
	outreg2 using TABLE_HC3.xls, replace ctitle(HC3 Standard Errors 1) addstat(N_Treated, `num_treat', N_Control, `num_control')
		
	reg re78 train age educ black hisp, vce(hc3)
	outreg2 using TABLE_HC3.xls, append ctitle(HC3 Standard Errors 2) addstat(N_Treated, `num_treat', N_Control, `num_control')
		
	reg re78 train age educ black hisp re74 re75, vce(hc3)
	outreg2 using TABLE_HC3.xls, append ctitle(HC3 Standard Errors 3) addstat(N_Treated, `num_treat', N_Control, `num_control')
	
	// Commenting on the results of the three new regressions
	* Standard errors increase slightly (aprox. +5%). However, there are no changes in significance level from the scenario where we use HC1. Although this is a more conservative estimation of the SE, we can conclude that our results are  robust to different specifications of potential heteroskedasticity. *
	
	
	// Preparing the values for the dfbeta regressions
	reg re78 train age educ black hisp re74 re75
	dfbeta train
	gen influence_train = _dfbeta_1

	// Running a loop to drop influential observations and run a regression afterwards with the reduced sample
	local droppings "3 2 5" 
	foreach i of local droppings {
		gsort- influence_train
		drop in 1/`i'
		sort influence_train
		drop in 1/`i'
		reg re78 train age educ black hisp re74 re75, vce(hc3)
		if `i' == 3 {
			outreg2 using tabledfhc3.xls, replace
    }
    else {
        outreg2 using tabledfhc3.xls, append
	}
	
	
	// Commenting on the results 
	* The results are similar to those of the previous regression analysis with HC1. The absolute value of the coefficient decreases as influential observations are removed.. Although the standard error increases in a similar fashion than in the previous simple regression analysis made in this question (aprox +5% in each model), there are no changes in significance level of the coefficients, making our results robust to different SE specifications and influential observations. 
	}
}

	* 3.d.3 *
quietly {
	// Reloading the original file to run the default setting
	use jtrain2, clear
	
	// Running the three bootstrap regressions
	reg re78 train, vce(bootstrap)
	outreg2 using TABLE_2boot.xls, replace ctitle(Spec 1)
	reg re78 train age educ black hisp, vce(bootstrap)
	outreg2 using TABLE_2boot.xls, append ctitle(Spec 2)
	reg re78 train age educ black hisp re74 re75, vce(bootstrap)
	outreg2 using TABLE_2boot.xls,  append ctitle(Spec 3)
	
	// Commenting on the results of the bootstrap regressions
	* The bootstrap regressions give similar results as the analysis of point 1.c. As a matter of fact, the coefficient does not change substantially, the p-value remains at a significant level and the standard errors remain around the same level. The only difference in the p-value is that is still under the 1% threshold in the second regression, and p value increases over 1% only after including the previous annual earnings. *
	
	// Explaining how bootstrap standard errors are calculated 
	* Standard errors using this method are calculated by taking into account all coefficients generated by the regressions, calculating the standard error of the single coefficients, and finally calculating the mean of the standard errors. *
	
	// Preparing the values for the dfbeta regressions
	reg re78 train age educ black hisp re74 re75
	dfbeta train
	gen influence_train = _dfbeta_1

	// Running a loop to drop influential observations and run a regression afterwards with the reduced sample
	local droppings "3 2 5" 
	foreach i of local droppings {
		gsort- influence_train
		drop in 1/`i'
		sort influence_train
		drop in 1/`i'
		reg re78 train age educ black hisp re74 re75, vce(bootstrap)
		if `i' == 3 {
			outreg2 using tabledfboot.xls, replace
    }
    else {
        outreg2 using tabledfboot.xls, append
	}
	}
	
	// Commenting on the results
	* The results are similar to those of the previous analysis. The absolute value of the coefficient decreases as influential observations are removed, as in the case of the HC1 standard errors. The significance level, however, remains the same throughout the entire observation dropping process. The persistent significance of the train variable keeps confirming the effectiveness of the training program with the original sample, solidifying the robustness of the study.
	
}
	
	* 3.d.4 *
quietly {
/*
None of the conclusions change dramatically in the analysis of part 3.d: the treatment coefficients remain significant at conventional significance levels for all of the main specifications using HC3 and bootstrap standard errors. 
The reason why HC3 may lead to different results than under HC1 might be because the HC1 procedure performs worse in small samples. While in Lalonde the sample size is over the threshold of 250 observations as suggested by Long & Ervin (2000) for using HC3, it might still be a sample size sufficiently small such that pronounced differences between the two procedures are present. In the Stata blog post by Enrique Pinzon, even when N=1000, there are relatively substantial differences in the 5% rejection rates across all levels of heteroskedasticity between HC1 and HC3.  Only for very large samples, the differences dissipate. 
In fact, since we observed only very small differences in the standard errors using HC3 compared to the default in Stata, this might be in line with the suggestion by Long & Ervin. Lalonde's sample size is indeed large enough to not warrant the use of HC3 standard errors.
*/
}





/*----------------------
Appendix
-----------------------*/
	/*
	Merging of the 3 generated Table_1 tables
	*/
quietly {
	// Preparing 1.a Table for merger
	clear
	import delimited using table1_main.csv
	
	// Creating artificial indexer for merger
	gen rank=_n			
	gen rank_unfixed=rank	
	replace rank=rank-2 if rank>11	
	replace rank=20+rank if rank_unfixed==13 | rank_unfixed==12 
	
	* Note: We are altering this indexer since the tables from 2a and 2b do not have the "no degree" variable. Thus, we will be usinn "rank" as the id for the merge and "rank_unfixed" to retrieve the original order for the final table 1. *

	save table1_order, replace		

	// Preparing 2.a Table for merger
	clear
	import delimited using table1_jt3
	
	// Creating artificial indexer for merger
	gen rank=_n
		
	// Renaming variable for the final Table
	rename (v1 v2 v3 v4) (Variables Control_jt3 Treatment_jt3 Diff_Means_jt3)
	
	// Saving the table
	save table1_jt3, replace 

	// Preparing 2.b Table for merger
	clear
	import delimited using table1_rand3

	// Creating artificial indexer for merger
	gen rank=_n
		
	// Variable Rename for final Table
	rename (v1 v2 v3 v4) (Variables Control_rand3 Treatment_rand3 Diff_Means_rand3)
		
	// Saving the table
	save table1_rand3, replace 

	// Performing the final merge of the tables
	use table1_order, clear

	// Joining with previous tables
	merge 1:1 rank using table1_jt3, nogen
	merge 1:1 rank using table1_rand3, nogen
		
	// Recovering original order of rows
	sort rank_unfixed

	// Dropping irrelevant intermediate variables
	drop rank* Variables

	// Final drop of ="" 
	foreach var of varlist _all {
		replace `var'=subinstr(`var',"=","",.)
		replace `var'=subinstr(`var',char(34),"",.)
		}
	
	// Exporting the table 
	export excel using TABLE_1.xlsx, replace
}

	// Erasing auxiliary files
	foreach file in table1_order.dta TABLE_2.txt TABLE_HC3.txt table1_jt3.csv table1_jt3.dta table1_main.csv table1_rand3.csv table1_rand3.dta tabledf.txt TABLE_2boot.txt tabledfboot.txt tabledfhc3.txt{
		erase `file'
		}




