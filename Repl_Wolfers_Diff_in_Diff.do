/********************
MICROECONOMETRICS 
*********************

*********************/

*** Program: Repl_Wolfers_Diff_in_Diff.do
*** first created: 19/04/2024
*** last updated:  07/05/2024


/*----------------------
Initial script configuration
-----------------------*/

	// Please replace your path in the command below
	cd "C:"
	local direc : pwd
	
	// Importing the necessary dataset
	use pset_4.dta, clear
	
	// Installing the necessary packages
	ssc install bacondecomp
	ssc install avar
	ssc install eventstudyinteract
	*ssc install twowayfeweights
	
/*----------------------
Exercise 1 
-----------------------*/
	* 1.a *
quietly {
	// Commenting on the weight that should be used for the analysis
	* Since Wolfers (2006) utilizes state divorce rates, which are comparable to state means, the observations should be weighted with analytic weights. This is due to the fact that unexplained variation will come from state characteristics, and not from individual characteristics.
}

	* 1.b *
quietly {
	* Graph 1 *
	// Creating a dummy for states that introduced the law between 1968 and 1988
	gen law68_88 =  1
	replace law68_88 = 0 if !inrange(lfdivlaw, 1968, 1988)
	
	// Creating year means of divorce rates for the two types of states
	collapse (mean) div_rate [aweight=stpop], by(year law68_88)	
	
	// Creating a two way graph for states that did not introduce the law between the selected years
	twoway line div_rate year if 1968<=year<=1988 & law68_88 == 1  || line div_rate year if 1968<=year<=1988 & law68_88 == 0, legend(label(1 "Reform states") label(2 "Control states") position(bottom)) ytitle("Divorces per 1,000 persons per year") xtitle("Year") xline(1969 1979)
	graph export "GraphI.pdf", replace
	
	* Graph 2 *
	// Rebooting the original dataset 
	use pset_4.dta, clear
	
	// Creating a dummy for state who introduced the law in 2000
	gen law2000 =  1
	replace law2000 = 0 if lfdivlaw!=2000
	
	// Creating a dummy for states who introduced the law between 1969 and 1973
	gen law69_73 =  1
	replace law69_73 = 0 if !inrange(lfdivlaw, 1969, 1973)
	
	// Creating year means of divorce rates for the two types of states
	keep if law2000 ==  1 | law69_73 == 1
	collapse div_rate [aweight=stpop], by (year law2000)
	
	// Creating the requested two way graph
	twoway line div_rate year if year<=1978 & law2000 ==  0 || line div_rate year if year<=1978 & law2000 ==  1, xline(1968.5) legend(label(1 "1969-1973 introduction States") label(2 "2000 introduction states") position(bottom)) ytitle("Divorces per 1,000 persons per year") xtitle("Year")
	graph export "GraphII.pdf", replace
	
	// Commenting on whether the results support the assumption of parallel trends
	* In the both graphs, the parallel trends assumptions appears to be borderline satisfied overall, with some slight changes in differences at some points. In the first graph, there is a slight relative change in the relative preexisting levels of divorce rates between the two categories of states, which might imply some shortfalls in the parallel trends assumption. Specifically, it appears that reform states slightly increase in trend with respect to control states before the treatment. As Wolfers points out, not controlling for this factor would overstate the effect of the policy. In our restricted sample that we create in the second graph, the same phenomenon appears to be happening. As a matter of fact, there is a slight relative decrease in divorce rate of the control group before the treatment, which might imply a violation of the parallel trends assumption, confounding results of our analysis. 
}

	* 1.c *
quietly {	
	// Rebooting the original dataset 
	use pset_4.dta, clear	
	
	// Restricting the sample to 1968 and 1978 only
	keep if year == 1968 | year == 1978
	
	// Creating the variable UNILATERAL
	gen UNILATERAL = 1
	replace UNILATERAL = 0 if !inrange(lfdivlaw, 1969, 1973)
	
	// Creating the variable POST
	gen POST = 1 
	replace POST = 0 if year != 1978
	
	// Creating the variable POST_UNILATERAL
	gen POST_UNILATERAL = 0 
	replace POST_UNILATERAL = 1 if POST == 1 & UNILATERAL == 1 
	
	// Estimating the pooled OLS regression 
	regress div_rate POST_UNILATERAL POST [aweight=stpop], vce(robust)
	
	// Estimating the full Diff-in-Diff specification
	regress div_rate POST UNILATERAL POST_UNILATERAL [aweight=stpop], vce(robust)
	
	// Commenting on the difference in the coefficients from the two regressions
	* The first specification does not take into account the difference in divorce rate of the two regions prior to the introduction of the laws. The coefficient of POST_UNILATERAL specifies the difference between the two groups of states after the introduction of the law, while the coefficient of POST specifies the overall average difference in divorce rate from one period to the other. As a matter of fact we can see a positive and significant value of post unilateral, meaning that the divorce rates were actually higher after the introduction of the unilateral divorce laws. This however does not imply causality between the two factors. 
	* In the second specification, with a full diff-in-diff, the value of the coefficient of POST specifies the difference in divorce rates from before and after the introduction period of the policies in regions that did not introduce the law. The value of the coefficient of UNILATERAL is the weighted mean difference in the rates of divorce before the policy between states that will introduce the law and states that will not. Furthermore, the constant coefficient is the mean divorce rate of the countries that were not going to introduce the law in the pre policy period. Finally, it generates a POST_UNILATERAL coefficient that is positive but very close to zero, and therefore non-significant. Therefore, according to the analysis, there is no causal relationship between the introduction of unilateral divorce laws and divorce rates.
}		
	* 1.d *
quietly {	
	// Defining excel file for outputs
	putexcel set TABLE_1, replace
	
	// Defining Columnn and Row names
	putexcel B1 = "UNILATERAL=1"
	putexcel C1 = "UNILATERAL=0"
	putexcel D1 = "Difference 2"
	putexcel A2 = "POST=1"
	putexcel A3 = "POST=0"
	putexcel A4 = "Difference 1"
	
	// Generating the values for the matrix
	foreach var in 0 1 {
		foreach val in 0 1 {
			sum div_rate [aweight=stpop] if UNILATERAL==`var' & POST==`val'
			local uni`var'post`val'=r(mean)
		}
	}
	
	//Matrix with values generation
	matrix define TABLE_1 = ( `uni1post1',`uni0post1',`uni1post1'-`uni0post1' \ `uni1post0',`uni0post0',`uni1post0'-`uni0post0' \ `uni1post1'-`uni1post0',`uni0post1'-`uni0post0',`uni1post1'-`uni1post0'-`uni0post1'+`uni0post0')
	
	//Exporting Values to the Excel File	
	putexcel B2 = matrix(TABLE_1)	
}	
	* 1.e *
quietly {		
	// Reloading the complete dataset
	use pset_4.dta, clear
	keep if (year >1955 & year <1989)
	
	// Creating the variable IMP_UNILATERAL
	gen IMP_UNILATERAL = (year>=lfdivlaw)
	
	// Creating state dummies
	tabulate st, gen(state)
	local states state1-state51 
	
	// Creating year dummies
	tabulate year, gen(yr)
	local years yr1-yr33
	
	// Regressing divorce rate on state and year dummies and on UNILATERAL
	regress div_rate IMP_UNILATERAL `years' `states' [aw=stpop], vce(robust)
	
	// Defining new year variable for time fixed effects
	gen t=year-1955
	gen t2=t^2
	encode st, gen(sta)
	
	// Regressing I: divorce rate on state and year dummies and on UNILATERAL
	regress div_rate IMP_UNILATERAL `years' `states' [aweight=stpop], vce(robust)
	
	// Regression II: state specific linear trends
	regress div_rate IMP_UNILATERAL `years' `states' sta#c.t [aweight=stpop], vce(robust)
	
	
	// Regression III: state specific linear and quadratic trends 
	regress div_rate IMP_UNILATERAL `years' `states' sta#c.t sta#c.t2 [aweight=stpop], vce(robust)
	
	// Commenting on the results of the three specifications
	/*
	The effect of the estimate in the first specification gives us a negative non-significant result. However, we find significant positive results in the subsequent 2 specifications. The change between the results of our three regressions suggest that the having introduced the law or not is correlated to the time trends in divorce rates. However, a scenario in which state-specific time trends are absent would grant us equal results across specifications. This would imply that there are no factors varying in time that are particular to each state. 
	*/
}		
	* 1.f *
quietly {		
	//Clear Dataset
	clear all
	
	// Code given by the problem set: 
	// Creating simulated observations
	set obs 6
	gen obs = _n
	gen state = floor( 0.9 + obs / 3 )
	bysort state : gen year = _n
	gen D = state == 1 & year == 3
	replace D = 1 if state == 2 & (year == 2 | year == 3)
	
	// Creating simulated outcome 
	gen Y = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + runiform()/100
	gen Y2 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.3 * (state==2 & year==3) + runiform()/100
	gen Y3 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.4 * (state==2 & year==3) + runiform()/100
	gen Y4 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.5 * (state==2 & year==3) + runiform()/100
	
	//Regression model I for all outcomes.
	regress Y D year state, vce(robust)
	regress Y2 D year state, vce(robust)
	regress Y3 D year state, vce(robust)
	regress Y4 D year state, vce(robust)
	
	//Comment
	/*
	The possibility of estimating consistently the results relies heavily on the real nature of the Generating Process. In the generation of the first outcome variable, there is an increase in year two for every state in the outcome variable (parallel trend assumption is satisfied). In the case Y is the real outcome, therefore, it is efficient to estimate the coefficient with the type of regression proposed in point e.(i), without accounting for specific state trends. As a matter of fact, the specification reports a positive (although non significant, with varying p values depending on the run of the code, due to it including a uniformly distributed random variable). On the other hand, in the generation process of variables Y2, Y3, Y4, an increasing (throughout the specifications) trend in the outcome is added only for state 2 at year 3. In this case, our estimate is inconsistent, given the fact that we do not account for state specific trends in the specification. As a matter of fact, the coefficients progressively deteriorate as the state specific trend gets more prominent, becoming more and more negative. In this case, the regression is not able to efficiently estimate the effect of the law introduction on divorce rates due to the omitted variable bias.  
	*/
}		
	* 1.g *
quietly {
	// Running the twowayfeweights package for the simulation	
	twowayfeweights Y state year D, type(feTR) summary_measures
	twowayfeweights Y2 state year D, type(feTR) summary_measures
	twowayfeweights Y3 state year D, type(feTR) summary_measures
	twowayfeweights Y4 state year D, type(feTR) summary_measures
	
	//Comment
	/*
	The estimation results arise from the assumption of common trend, which is violated when in models 2 to 4 we include state specific trend, which breaks the differences in differences estimator assumption. The TwowayFE package with the feTR type does not account for this variation, similarly to the previous model.
	*/
}	
	* 1.h *
quietly {		
	//Generating init_stpop
	use pset_4, clear
	keep if (year >1955 & year <1989)
	bys st: egen init_stpop=first(stpop)
	
	// Creating state dummies
	tabulate st, gen(state)
	local states state1-state51 
	
	// Creating year dummies
	tabulate year, gen(yr)
	local years yr1-yr33
	
	// Creating the variable IMP_UNILATERAL
	gen IMP_UNILATERAL = (year>=lfdivlaw)
	
	encode st, gen(state)
	xtset state year
	
	bacondecomp div_rate IMP_UNILATERAL [aw=init_stpop], ddetail stub(state)
	graph export "BaconDecomposition.pdf", replace
	
	// Comment
	/*
	The Goodman-Bacon analysis decomposes the DD estimator as the weighted average of all possible 2x2 estimators that can arise from the sample. These estimators can be classified in three categories: Timing groups, comparing groups that got the treatment later to ones that received it earlier; Always groups, comparing always takers with groups that got the treatment in a time within the sample; and Never groups, comparing never takers with groups that did get the treatment within the time considered by the sample.  The goal is to identify how much of the estimator can be attributed to timing variation (in opposition to the never-always groups). In our scenario, the second and third group account for 90% of the variation, thus resulting in a low percentage of the variation resulting from timing. Finally, we can check in the graph that there are no weights in the regression below the 0 line, hinting at the absence of negative weights in our model. 
	*/
}	
	* 1.i *
quietly {		
	set scheme s1color
	use pset_4, clear
	
	// Remove comment to get the analysis utilizing Wolfers' sample
	keep if year>1955 & year<1989
	
	//Generating the dummy variables
	gen dif=year-lfdivlaw
		
	forvalues tau_min = 10(-1)2 {
		if `tau_min' == 10{ 
			gen bminstate_`tau_min'= (dif<=-`tau_min')
		}
		else{
			gen bminstate_`tau_min'= (dif==-`tau_min') 
		}
	}
		
	forvalues tau_max = 0/15 {
		if `tau_max' == 15{
			gen bmaxstate_`tau_max'= (dif>=`tau_max') 
		}
		else {
			gen bmaxstate_`tau_max'= (dif==`tau_max')
		}
	}
	*(Only for labels)
	forvalues labval= 10(-1)2 {
		label var bminstate_`labval' "-`labval'"
	}
	forvalues labval = 0/15 {
		label var bmaxstate_`labval' "+`labval'"
	}
			
	// Creating state dummies
	tabulate st, gen(state)
	encode st, gen(state)
	local states state1-state51 
	
	// Creating year dummies
	tabulate year, gen(yr)
	local years yr1 - yr43
	
	// Running the first regression
	reghdfe div_rate bminstate_* bmaxstate_* [aweight=stpop], absorb(i.state i.year) vce(cluster state)

	estimates store reg1
	
	// Running the second regression, after generating trending variables.
	encode st, gen(sta)
	gen t = year - 1955
	gen t2 = t^2
	
	reghdfe div_rate bminstate_* bmaxstate_* sta#c.t [aweight=stpop], absorb(i.state i.year) vce(cluster state)
	estimates store reg2

	// Running the third regression with state trend and quadratic trend interactions. Note: Stata automatically removes multicollinear variables.
	reghdfe div_rate bminstate_* bmaxstate_* sta#c.t sta#c.t2 [aweight=stpop], absorb(i.state i.year) vce(cluster state)
	estimates store reg3
	
	//Comment 
	/*
	For all three regressions, prior to the introduction of unilateral divorce laws, the coefficients are relatively small and statistically insignificant. From tau = 0 (time of introduction), the coefficients becomes positive and statistically significant for some periods, then revert back to zero. The coefficient from the regression without state specific trends even becomes negative (and statistically significant) while the ones with state specific trends hover around zero. Generally, the confidence intervals from the regression output are quite large, suggesting that there is some substantial estimation uncertainty in the coefficients. 
	We can interpret the results as follows: once the law becomes binding, there seems to be a positive effect on divorce rates, but this effect starts to dissipate over time. The long run effect for the regression without state-specific trends interestingly becomes negative while the long run effects for the regressions with state specific trends become slightly positive but statistically insignificant. That is, the conclusion that long-run effects are negative is fragile, and there may be a tendency to say that the long-run effect is closer to zero. 
	Compared with a single-coefficient analysis, the event-study provides some important extra information. It provides insights to whether there are anticipatory effects i.e. we are able to see whether divorce rates start rising prior to the introduction of the law. Using lagged indicators is further crucial for distinguishing the effect of the law itself from other ongoing trends in the outcome- we should not observe significant coefficients for tau <0. Furthermore, we are able to observe the dynamic response post enactment of the treatment and whether the effects persist over time. The inclusion of quadratic trends allows us to see whether the rate of change of divorces itself is changing (an acceleration or deceleration). Generally, the event study approach provides a robustness test to determine whether there are existing trends that could confound the attributed effects of the law, affecting the extent to which we can claim that the effect is causal. 
	*/
}

	* 1.j *
quietly {	
	
	//General graph and saving
	coefplot reg1 reg2 reg3, keep(bminstate_* bmaxstate_*) xtitle("Years (until / after) adoption of UDL") ytitle("Estimated Effect") title("Event Study") vertical xlabel(, angle(vertical)) yline(0) legend(order(2 "No trends" 4 "Linear Trends" 6 "Quadratic Trends") pos(6))
	graph export "EventStudy.pdf", replace
	
		// Separate plot for each specification
		coefplot reg1, keep(bminstate_* bmaxstate_*) xtitle("Years (until / after) adoption of UDL") ytitle("Estimated Effect") title("Event Study NT") vertical xlabel(, angle(vertical)) yline(0)
		graph export "EventStudyNoTrend.pdf", replace
		
		coefplot reg2 , keep(bminstate_* bmaxstate_*) xtitle("Years (until / after) adoption of UDL") ytitle("Estimated Effect") title("Event Study Linear") vertical xlabel(, angle(vertical)) yline(0) 
		graph export "EventStudyLinearTrend.pdf", replace
		
		coefplot  reg3, keep(bminstate_* bmaxstate_*) xtitle("Years (until / after) adoption of UDL") ytitle("Estimated Effect") title("Event Study Quadratic") vertical xlabel(, angle(vertical)) yline(0) 
		graph export "EventStudyQuadTrend.pdf", replace
	
}

	* 1.k *
quietly {	
	// Commenting on differences between conclusions of Friedberg (1998) and Wolfers (2006)
	* Friedberg's analysis concludes that the adoption of unilateral divorce laws significantly and permanently increased divorce rates. She attributes approximately 17% of the increase in divorce rates from 1968 to 1988 to these laws, using state-level panel data with extensive controls for state-specific trends and demographic factors. Her findings suggest a lasting effect of the laws on increasing divorce rates.
	* Wolfers agrees that divorce rates initially surged following the adoption of unilateral divorce laws but argues that this increase dissipated over time. According to his findings, the spike in divorce rates reversed within a decade, and there might even be a slight decline in divorce rates 15 years after the reforms. He proposes that the initial rise was due to a "pent-up demand" from those previously constrained by more restrictive divorce laws, and this effect diminishes as the public adjusts to the new legal landscape. In other words, divorce rates did not de facto increase due to the new law, but because those who already wanted to divorce were allowed to effectively do so, leading to a dissolution of bad matches. 
	* Friedberg's methodology employs robust controls for unobserved heterogeneity and state-specific characteristics, which she argues are essential for accurately estimating the impact of the law.
	* Wolfers critiques Friedberg's difference-in-difference methodology, which employs a single "Unilateral" dummy variable to capture the entirety of the adjustment process and suffers from issues related to the specification of state trends. He argues that this approach fails to adequately separate the effect of the law change from ongoing state-specific trends and broader social changes, potentially conflating the rise in divorces due to the new law with pre-existing trends in each state. He suggests that state-specific trends might absorb not only inherent differences between states but also the nuances of how divorce rates evolved post-reform, thus misleading the estimated impact of unilateral divorce laws. He also extends the data series back to 1956 (compared to Friedberg's start in 1968) to capture a broader range of pre-existing trends.
	* Both Friedberg and Wolfers discuss the implications of the Coase theorem, suggesting that if bargaining were costless (no transaction costs) and information complete, changes in divorce laws should not affect the divorce rate as individuals would negotiate to achieve the same outcomes regardless of legal constraints. 
}	
	* 1.l *
quietly {	
	// Generate the control cohort variable
	gen control = (lfdivlaw == 2000)

	// Eventstudyinteract - Model (1)
	eventstudyinteract div_rate bminstate_* bmaxstate_* [aweight=stpop], cohort(lfdivlaw) control_cohort(control) absorb(i.state i.year) vce(cluster state)
	matrix C1 = e(b_iw) //Retrieve coefficients
	mata st_matrix("A1", sqrt(diagonal(st_matrix("e(V_iw)")))) //Retrieve standard errors
	matrix C1 = C1 \ A1'

	// Eventstudyinteract - Model (2)
	eventstudyinteract div_rate bminstate_* bmaxstate_* [aweight=stpop], cohort(lfdivlaw) control_cohort(control) covariates(i.sta##c.t) absorb(i.state i.year) vce(cluster state)
	matrix C2 = e(b_iw)
	mata st_matrix("A2",sqrt(diagonal(st_matrix("e(V_iw)"))))
	matrix C2 = C2 \ A2'
	
	// Eventstudyinteract - Model (3)
	eventstudyinteract div_rate bminstate_* bmaxstate_* [aweight=stpop], cohort(lfdivlaw) control_cohort(control) covariates(sta##c.t sta##c.t2) absorb(i.state i.year) vce(cluster state)
	matrix C3 = e(b_iw)
	mata st_matrix("A3",sqrt(diagonal(st_matrix("e(V_iw)"))))
	matrix C3 = C3 \ A3'

	// Event-study plot
	coefplot (matrix(C1[1]), se(C1[2])) ///
         (matrix(C2[1]), se(C2[2])) ///
         (matrix(C3[1]), se(C3[2])), ///
         keep(bminstate_* bmaxstate_*) ///
         vertical yline(0) ///
         xtitle("Years (until / after) adoption of UDL") ///
         ytitle("Estimated Effect") ///
         title("Event Study (two-way FE correction Sun & Abraham)") ///
         xlabel(, angle(vertical))  ///
		 legend(order(2 "No trends" 4 "Linear Trends" 6 "Quadratic Trends") pos(6))
	graph export "EventStudyInteract.pdf", replace
	
	// Separate plot for each especification
		coefplot (matrix(C1[1]), se(C1[2])), ///
			 keep(bminstate_* bmaxstate_*) ///
			 vertical yline(0) ///
			 xtitle("Years (until / after) adoption of UDL") ///
			 ytitle("Estimated Effect") ///
			 title("Event Study NT (two-way FE correction Sun & Abraham)") ///
			 xlabel(, angle(vertical))  ///
			 
		// Graph saving
		graph export "EventStudyInteractNT.pdf", replace
		
	coefplot (matrix(C2[1]), se(C2[2])), ///
			 keep(bminstate_* bmaxstate_*) ///
			 vertical yline(0) ///
			 xtitle("Years (until / after) adoption of UDL") ///
			 ytitle("Estimated Effect") ///
			 title("Event Study Linear (two-way FE correction Sun & Abraham)") ///
			 xlabel(, angle(vertical))  ///
		
		// Graph saving
		graph export "EventStudyInteractLT.pdf", replace
			
		coefplot (matrix(C3[1]), se(C3[2])), ///
			 keep(bminstate_* bmaxstate_*) ///
			 vertical yline(0) ///
			 xtitle("Years (until / after) adoption of UDL") ///
			 ytitle("Estimated Effect") ///
			 title("Event Study (two-way FE correction Sun & Abraham)") ///
			 xlabel(, angle(vertical))  ///
			 
		// Graph saving
		graph export "EventStudyInteractQT.pdf", replace
		
	// Commenting on the consistency of our results with the original paper
	* Our most recent analysis is mostly consistent with the results of the original paper, but not entirely. The placebo effect check of non significance of the policy before it was adopted is shown on the left side of the graph. The signs of the coefficients are extremely similar: they start as positive and, interestingly, become negative after a number of years. 
	* The main difference with the results of the original paper stands in the persistence of the significance of the effect of the policy on divorce rates. Focusing on the specification with state trends only, when taken into account treatment effect heterogeneity across states, the effect becomes non significant (although still positive) from the third year from the introduction of the policy. On the other hand, in Wolfers' analysis the effects keep being significant until 8 years after introduction. As a matter of fact, while the Author finds the dissipation of the effect after a decade from introduction, our analysis concludes that this dissipation happens way earlier. Similar conclusions can be drawn focusing on the first and third specification (without state trends and with quadratic state trends respectively), that see the effect of the policy lose their significance in earlier stages. The main conclusion that can be drawn from the employment of the interaction weighted estimator is that, due to disregarding the possibility of treatment effect heterogeneity across states, the dynamic analysis proposed by Wolfers' overestimates the effects (and their significance) of unilateral divorce laws on divorce rates, especially in the middle of the decade after introduction.  
	
	// Restricting the sample to Wolfers' sample and commenting
	* When restricting the sample and the analysis to the years taken into account by Wolfers', the results are not consistent with the ones of the paper in one specific instance. While the similar results to our previous analysis are obtained for the no trend and linear trend specifications, the adding of the quadratic time trends leads to abnormally high absolute values of coefficients and related standard errors. This may be due to the fact that by restricting the sample further by taking out the observations before 1956 and after 1988, the relative size of the remaining cohorts increases due to taking out cohorts that introduced the law in years that are not anymore considered. Hence, the share of the remaining cohorts increases. The point estimate of the coefficients proposed by Sun and Abraham scale quadratically in the share of the cohort, which combined with the quadratic specification leads to abnormally high values and standard errors.
	
	// Explaining what correction our proposed algorithm performs
	* In the event study performed in point i we assume that we have an homogeneous effect in all the states. This may lead to contamination from effects from other periods in the case of treatment effects heterogeneity. We therefore utilize an algorithm that divides states into different cohorts based on the timing of the law introduction. The process also fixes interpretability of the weights allocated to each coefficient.
	* The algorithm performs as follows. Firstly, it estimates the interacted regression, in which interactions are between relative (not absolute) time indicators and cohort indicators. Then, shares from states that introduced the law in the same year (cohorts), underlying each relative timing, are estimated. Finally, it computes a weighted average of the estimates computed utilizing the shares as weights. 

}	


