/********************
MICROECONOMETRICS 
*********************

*********************/

*** Program: Repl_Meyerson_Gonzalez_RDD.do
*** first created: 02/04/2024
*** last updated:  18/04/2024


/*----------------------
Initial script configuration
-----------------------*/

	// Please replace your path in the command below
	cd "C:"
	local direc : pwd
	
	// Importing the necessary dataset
	use pset_3.dta, clear
	
	// Installing the necessary packages
	net install lpdensity, from("https://raw.githubusercontent.com/nppackages/lpdensity/master/stata") replace
	net install rddensity, from("https://raw.githubusercontent.com/rdpackages/rddensity/master/stata") replace
	net install rdrobust, from("https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata") replace

/*----------------------
Exercise 1 
-----------------------*/
	* 1.a *
quietly {
	// Constructing the main plot
	rdplot T X, graph_options(xtitle("Running variable") ytitle("Treatment variable"))
	
	// Checking for discontinuity of the outcome against the running variable and commenting
	rdplot Y X 
	*The current design is sharp RD since the treatment status is visibly a deterministic function of the running variable (treatment probability jumps from zero to 1 at the cutoff) i.e. there is perfect compliance with the outcome of the votes - the elected candidate becomes the major of the jurisdiction with certainty (and the non-elected does not).
}
	* 1.b and 1.c *
quietly {
	// Setting definition for graphs and loop
	set scheme s1color
	local covariates "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"
	putexcel set "`direc'\Table_1.xls", replace

	//Initializing the first row of the table with required column titles
	putexcel A1 = "Label" B1 = "MSE-Optimal Bandwidth" C1 = "RD Estimator" D1 = "p-value" E1 = "Effective Number of Observations"

	// Setting the counter to start from row 2 in the table
	local counter = 2
	
	// Creating a loop to iterate over covariates to create rdplots and obtain the table data
	foreach var in `covariates' {
		local lble: variable label `var'
		
		* Part C section of the loop 
		rdplot `var' X, ci(95) shade graph_options(title("`lble'") xtitle("Islamic Vote Margin in 1994"))
		graph rename `var'_X, replace 
		local graphlist "`graphlist' `var'_X"
		rdrobust `var' X, c(0) kernel(triangular) bwselect(mserd)
	
		* Part B section of the loop
		local bandwidth_upper = round(e(h_r), .001)
		local bandwidth_lower = round(e(h_l),.001)
		local opt_bandwidth = "-" + string(`bandwidth_lower')+ "," + string(`bandwidth_upper')
		local rd_estimate = round(e(tau_cl),.001) //conventional RD estimate
		local p_value = round(e(pv_cl),.001) //conventional p-value
		local eff_obs = e(N_h_l) + e(N_h_r)
		
		* Writing row entry into Excel file and increasing the counter
		putexcel A`counter' = "`lble'" B`counter' = "`opt_bandwidth'"  C`counter' = `rd_estimate' D`counter' = `p_value' E`counter' = `eff_obs'
		local counter = `counter' + 1
	}
	
	// Saving the output
	putexcel save
	
	// Exporting the graph and saving
	graph combine `graphlist', altshrink
	graph export "`direc'\Graph_1.pdf", replace
}
	* 1.d *
quietly {
	// Plotting a histogram of the running variable
	rdrobust Y X 
	
	// Obtaining the optimal bandwidth
	sca left = -e(h_l)
	sca right = e(h_r)
	twoway (histogram X if X >= left & X < 0, frequency width(2) color(red%30) lcolor(black) lwidth(vthin)) ///
		(histogram X if X>=0 & X<=right, frequency width(2) color(blue%30) lcolor(black) lwidth(vthin)), ///
		legend(off) title("Histogram of the Running Variable") ytitle("Frequency") xtitle("Islamic Vote Margin in 1994") xline(0, lcolor(black) lwidth(medium) lpattern(dash))
	graph rename Y_X_Hist, replace

	// Generating the rddensity plot of the running variable 
	local b_left = left
	local b_right = right
	rddensity X, plot plot_range(`b_left' `b_right') hist_range(`b_left' `b_right') graph_opt(title("Manipulation Test Plot") xtitle("Islamic Vote Margin in 1994") legend(off) xline(0, lcolor(black) lwidth(medium) lpattern(dash)))
	graph rename Y_X_Man_Test, replace

	// Combining and saving the final graph
	graph combine Y_X_Hist Y_X_Man_Test, altshrink
	graph export "`direc'\Graph_2.pdf", replace
}
	* 1.e *
quietly{
	// Testing for the existence of discontinuities in the running variable at cutoff
	rddensity X
	
	// Commenting on the test
	* The p-value of the manipulation test is  0.1634, so we fail to reject the null hypotheses of no manipulation at conventional levels of significance. This is no surprise as the 95% confidence intervals of the manipulation plot overlap at the cutoff. This is in favor of the validity of the RD design as it is evidence for the fact that there was at least no unidirectional selection into treatment or control based on potential outcomes e.g. individuals or families with specific educational preferences might choose to migrate to areas where the Islamic vote margin is reflective of their own views, potentially affecting both the vote margin and educational attainment, causing quasi-randomization to break down. But we believe that this is not likely at the cutoff. 
}
	* 1.f *
quietly{
	// Plotting multiple potential cutoffs
	rddensity X, c(-10)
	rddensity X, c(-5)
	rddensity X, c(5) 
	rddensity X, c(10) 
	
	// Commenting on the results
	* There is no evidence of alternative discontinuities at the proposed cutoffs since the p-values are all larger than the 5% significance level, keeping in mind we use a polynomial of order 4. Only at the cutoff -5, the p-value is only slightly higher than 5%, so it might be necessary to check the density using polynomials of different orders for robustness.
}
	* 1.g *
quietly {
	// Plotting the main regression plot
	rdplot Y X, c(0) nbins(20 20) graph_options(xtitle("Running Variable") ytitle("Outcome"))
}
	* 1.h *
quietly {
	// Estimating the requested effect with the triangular kernel
	rdrobust Y X, c(0) p(1) q(2) kernel(triangular)

	// Saving optimal bandwith for points J and K
	local opt_l_i = e(h_l)
	local opt_r_i = e(h_r)

	// Estimating the requested effect with the uniform kernel
	rdrobust Y X, c(0) p(1) q(2) kernel(uniform)
	
	// Commenting on the results
	* There is evidence of a significant effect of the election of a mayor from an Islamic party on the educational attainment of women. This is measured by a discontinuity around the cutoff of a 3.01 percentage points increase utilizing the triangular kernel.  
	* Also, our estimate is slightly sensitive to the selection of the kernel type. We can see the point estimation of our effect increases from 3.01 to 3.2. Additionally, the significance level goes from 5% to 10% when using a triangular kernel with the robust SD estimates.
}
	* 1.i *
quietly {
	// Utilizing a global approach with the requested features
	reg Y T c.X c.X#c.X c.X#c.X#c.X c.X#c.X#c.X#c.X
}
	* 1.j *
quietly {
	// Estimating the effect with a local approach and the requested features
	reg Y T c.X  if (X>=-`opt_l_i') & (X<=`opt_r_i')

	// Commenting on the results
	* We do not get the same result. In terms of the coefficient, the point estimation difference derives from the different methodologies employed. OLS captures the difference in means across the entire group, while RDD considers the probability of selection (closeness to thershold). By exploiting the intervention design, RDD improves its accuracy.
}
	* 1.k *
quietly {
	// Setting the spreadsheet to store the values
	putexcel set graph3data.xls, replace

	// Creating a loop for graph values
	foreach band in 0.5 0.75 1 1.25 1.5 {
		rdrobust Y X, c(0) p(1) q(2) kernel(triangular) h(`opt_l_i'*`band' `opt_r_i'*`band')
		local row = (`band'/0.25)-1
		local mean = e(tau_cl)
		local sd = e(se_tau_cl)
		putexcel A`row' = `band'
		putexcel B`row' = `mean'
		putexcel C`row' = `sd'
	}

	// Recoverying values
	import excel graph3data, clear

	// Defining the graph
	serrbar B C A, scale(1.96) title("Graph 3: Sensivity to Bandwith Size Analysis") xtitle("Percentage of optimal bandwith") ytitle("Estimate and CI") xtick(0.5 0.75 1 1.25 1.5) yline(0)

	// Saving the graph
	graph export "`direc'\Graph_3.pdf", replace

	// Commenting on the results
	* We can observe that when the band decreases, there is a loss of significance of the treatment effect that can be explained by a considerable decrease in the point estimate (33% of the effect) as well as a increase in the standard error due to the expected loss in precision for the smaller number of observations. Meanwhile, the increase in bandwith does not have a significant impact on the point estimate nor its significance. 
}
/*----------------------
Exercise 2 
-----------------------*/
	* 2.a *
quietly {
	// Preparing the necessary dataset
	use fraud_pcenter_final, clear
	
	// Creating a variable that allows to understand on which side of the discontinuity the center is on
	gen _temp=_dist
	replace _temp=-_dist if cov==0
	
	// Plotting the treatment variable as a function of the new running variable
	rdplot cov _temp if _temp > -20 & _temp < 20, p(1) graph_options(title(Coverage-Distance Discontinuity) legend(off))
	graph save discontinuity, replace
	scatter cov _temp, title("Scatterplot of Coverage vs Distance") xline(0)
	
	// Generating a variable for percentage of comprised votes out of total votes
	gen ratio_compr = total_ecc / totalv_pc
	
	// Running a regression for obtaining the RD estimate
	rdrobust ratio_compr _temp, fuzzy(cov)
	
	// Commenting on whether the current design is sharp or fuzzy RD 	
	* The figures generated with both methods (rdplot and scatter) uncover a fuzzy Regression Discontinuity case. As a matter of fact, it appears that the use of a longitude proxy variable for the creation of the distance variable causes the generation of a probabilistic, as opposed to deterministic, rule for assignment of telephone coverage to centers. This can be ascertained considering the fact that at the cutoff the telephone coverage does not go from present to absent precisely. Instead, when approaching the cutoff what appears to be happening is that the probability of a certain center having coverage increases (although rapidly), until it reaches a point in which is it exactly one. As a matter of fact, center observations below the cutoff appear to have a lower probability to have coverage, while centers above it have a way higher probability of coverage. 

	// Commenting on necessary assumptions for validity of estimates of Gonzalez (2021)
	/* The assumptions needed for identifying and estimate the causal relationship are:
	 o	Boundary positivity assumption: it assumes that there is a positive probability of observing units close to the cutoff from both sides. In this context it means that for every segment of the boundary that we consider, there are polling centers on both sides. Meaning, we restrict the analysis on segments that satisfy this assumption.
	 o	Continuity of potential outcomes: in absence of treatment, potential outcomes must transition smoothly across the cutoff. In other words, in the absence of 2G coverage, we should not observe discountinuous changes in the rates of election fraud at the cutoff.
	 o	No manipulation of the running variable: by observing the density of polling centers along the running variable, we should not be observing bunching of the density of polling centers on either side of the cutoff.
	 o	Identifying assumptions: other possible covariates that influence the fraud outcome, but which are not the running variable, vary smoothly at the cutoff.
	 o	No discontinuity of the ouctome variable away from the cutoff point: the assumption is tested by the author by randomly selecting 100 longitudes and latitudes to create artificial "boundaries" and then conducting the same RD analysis as if these boundaries were meaningful with respect to cell phone coverage, Gonzalez aims to test the robustness, and thus validating any issue arising from using a proxy for longitude. The results of the falsification tests suggest that such errors would not necessarily invalidate the RD design choice. Since the falsification tests found no significant RD effects at artificial boundaries (where no true treatment exists), it suggests that random misallocations of centers (due to proxy measurement errors) outside the actual coverage areas are unlikely to introduce a systematic bias that mimics the true treatment effect of cell phone coverage.
	 o	IV assumptions: exogeneity and exclusion restriction. One must note that given the fact that we must employ a fuzzy RD for our estimate given the introduction of distances calculated with proxy longitude, IV assumptions should hold. Hence, as for the first one, the error term should not be correlated with the instrument _dist, and for the second one the instrument should not influence election fraud rates in pathways different from telephone coverage. 
	 
	It can be noted that in the Gonzalez (2021) setting results may plausibly be violated due to multiple issues. To name a few examples, there is no data on possible additional mobile service providers in certain areas, some coverage towers might have shutdowns, and there may be spillovers of some non coverage polling centers getting benefits from nearby coveraged areas. However, these are thoroughly analyzed and tackled by the author throughout the paper.
	 */
}	
	* 2.b *
quietly {
	// Commenting on settings in which proxied longitude does not call for a change in research design
	* In general, one can define two dinstinct settings in which having a proxy for longitude does not require a change in RD design.
	* Firstly, as is stated by the Additional Results section of Gonzalez (2021), specifically in the paragraph Signal Strenght Fuzziness, there are frequent changes in factors that influence the actual telephone coverage in certain areas, such as, for example, weather conditions, conductivity of terrain, and temperature. This causes the signal strenght to vary at different times, meaning that the telephone coverage might not vary sharply at the cutoff as the original design intended. Consequently, the author of the paper considers the treatment variable of the telefone coverage as an intention-to-treat effect instead of the actual treatment. This relieves them from the need of changing design from sharp to fuzzy. However, if one were to consider a research setting in which the actual treatment, as opposed to the ITT, is investigated, they would be required to employ a fuzzy RDD design instead. Therefore, an employment of a distance measure generated with a proxy of longitude would not require a change in RD design, which would be fuzzy from the beginning. Still, it is important to note that while the change in distance variable would not change the research design itself, it would still be bound to increase the noise in the obtained estimates. 
	* Secondly, a case in which distance is based only on latitude would not call for a change to a fuzzy design, as the treatment would be uncorrelated with longitude, whether proxied or not, leading to absence of measurement error. However, it should be noted that this would happen only in the case in which telephone coverage were to be transmitted horizontally, which is not the case within the paper of Gonzalez and is unrealistic in any real world setting. Hence, this solution is only theoretical and should not be considered as a concrete and feasible research setting. 
}		
	* 2.c *
quietly {
	
	// Changing label of variable cov for future use in LaTeX table
	label variable cov "Inside coverage"
	
	// Running a Bandwith Selection Procedure, generating the optimal bandwith for the two regions aggregated and secondarly for both regions singularly
	foreach var in comb comb_ind {
		rdbwselect vote_`var' _temp if ind_seg50==1, vce(cluster segment50)
		scalar hopt_`var'=e(h_mserd)
		forvalues r=1/2 {
			rdbwselect vote_`var' _temp if ind_seg50==1 & region2==`r', vce(cluster segment50)
			scalar hopt_`var'_`r'=e(h_mserd)
		}
	}
	
	// Running a loop to store means of the variables when there is no coverage and distance to the closest coverage boundary is within the optimal bandwidth range
	foreach var in comb comb_ind {
		sum vote_`var' if (cov==0 & ind_seg50==1 & _dist<=hopt_`var') // Distance to be smaller than the optimal bandwidth estimator. 
		scalar mean_`var'=r(mean)
		forvalues r=1/2 {
			sum vote_`var' if (cov==0 & ind_seg50==1 & _dist<=hopt_`var'_`r' & region2==`r')
			scalar mean_`var'_`r'=r(mean)
		}
	}
	
	// Running a loop to store means  of the variables when there is no coverage, without the distance condition
	foreach var in comb comb_ind {
		sum vote_`var' if (cov==0 & ind_seg50==1)
		scalar mean_`var'_all=r(mean)
		forvalues r=1/2 {
			sum vote_`var' if (cov==0 & ind_seg50==1 & region2==`r')
			scalar mean_`var'_`r'_all=r(mean)
		}
	}
	
	// Declaring the necessary panel data
	xtset, clear
	xtset segment50 pccode
	
	// Generating necessary variables for the next regressions
	gen cov_instrum = 0
	replace cov_instrum = 1 if _temp > 0
	gen interaction = cov * _temp
	gen instr_int = cov_instrum * _temp
	
	// Running local fuzzy RDD regressions to obtain point estimates and storing them
	foreach var in comb_ind comb {	
	* All regions
	xtivreg vote_`var' (cov = cov_instrum) _temp if ind_seg50==1 & _dist<=hopt_`var', fe  vce(robust)
		est store col1_1a_`var'
	
	* Southeast 
	xtivreg vote_`var' (cov = cov_instrum) _temp if ind_seg50==1 & _dist<=hopt_`var'_1 & region2==1, fe vce(robust) 
		est store col1_1b_`var'

	* Northwest
	xtivreg vote_`var' (cov = cov_instrum) _temp if ind_seg50==1 & _dist<=hopt_`var'_2 & region2==2, fe vce(robust)
		est store col1_1c_`var'
	}
	
	// Running local fuzzy RDD regressions to obtain point estimates and storing them
	foreach var in comb_ind comb {	
	* All regions
	xtivreg vote_`var' (cov interaction = cov_instrum instr_int) _temp if ind_seg50==1 & _dist<=hopt_`var', fe  vce(robust)
		est store col1_2a_`var'
		
	* Southeast
	xtivreg vote_`var' (cov interaction = cov_instrum instr_int) _temp if ind_seg50==1 & _dist<=hopt_`var'_1 & region2==1, fe vce(robust)  
		est store col1_2b_`var'
		
	* Northwest
	xtivreg vote_`var' (cov interaction = cov_instrum instr_int) _temp if ind_seg50==1 & _dist<=hopt_`var'_2 & region2==2, fe vce(robust)
		est store col1_2c_`var'
	}
	
	// Exporting the results in a LaTex table
	estout col1_1a_comb_ind  col1_2a_comb_ind  col1_1b_comb_ind  col1_2b_comb_ind col1_1c_comb_ind  col1_2c_comb_ind  ///
	using "results.tex", replace style(tex) ///
	label cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
	keep(cov) mlabels(, none) collabels(, none) eqlabels(, none) ///
	stats(N, fmt(a3) ///
	labels("Observations")) ///
	prehead("\begin{table}[H]" "\centering" "\begin{tabular}{lcccccc}" ///
	"\noalign{\smallskip} \hline \noalign{\smallskip}" ///
	"& \multicolumn{6}{c} {RDD - Optimal Bandwidth}"   ///
	"\noalign{\smallskip} \\ " ///
	"& \multicolumn{2}{c} {All regions} & \multicolumn{2}{c} {SouthEast region} & \multicolumn{2}{c} {NorthWest region}\\" ///
	" & (1) & (2) & (3) & (4) & (5) & (6) \\" ) ///
	prefoot("\noalign{\smallskip}" "Interaction Var & No & Yes & No & Yes & No & Yes \\") ///
	posthead("\hline \noalign{\smallskip}" "\multicolumn{6}{l}{\emph{Panel A. At least one station with Category C fraud}} \\" "\noalign{\smallskip} \noalign{\smallskip}" )

	estout col1_1a_comb  col1_2a_comb  col1_1b_comb  col1_2b_comb col1_1c_comb  col1_2c_comb   ///
	using "results.tex", append style(tex) ///
	label cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
	keep(cov) mlabels(, none) collabels(, none) eqlabels(, none) ///
	stats(N, fmt(a3) ///
	labels("Observations")) ///
	posthead("\noalign{\smallskip} \noalign{\smallskip} \noalign{\smallskip}" "\multicolumn{6}{l}{\emph{Panel B.  Share of votes under Category C fraud}} \\" "\noalign{\smallskip} \noalign{\smallskip}" ) ///
	prefoot("\noalign{\smallskip}" " Interaction Var & No & Yes & No & Yes & No & Yes \\") ///
	postfoot("\noalign{\smallskip} \hline \noalign{\smallskip}" ///
	"\end{tabular} \end{table}")

	// Commenting on the results obtained
	* It's easy to notice that the use of a proxy variable for longitude and a consequent switch to a fuzzy RD design leads to estimates that are less precise compared to the ones in Gonzalez(2021). There is no specific direction in which our estimates change, meaning that it is hard to ascertain whether the change in design and distance variable leads to an overestimation or underestimation of the effect. However, it is clear that the standard error increases with respect of the design of the paper, leading up to decreased levels of significance for our estimates.
	* Further, also compared to the results of our first global analysis carried out in point 3.a the standard errors increase. This outcome is likely to be the result of the fact that by utilizing a bandwidth and a local regression, we are restricting the considered sample, which can lead to an increase in the standard errors. 
	* Finally, let us analyze the results obtained. In the table, the point estimates of the carried out regressions are positioned on 2 rows, representing the two different panels, which use different definitions of the outcome variable fraud. Within each row, the odd numbered columns represent regressions in which no interaction variable is present for the fuzzy IV regression, while the even numbered represent the ones in which the latter is included. Regressions are also divided by regions accordingly. 
	* When considering results for the whole country, the presence of the coverage decreases the number of fraudolent votes by 10.2%, while it does not affect the probability of a polling center to be fraudolent, which appears to be non significant. Further, consistently with results from Gonzalez (2021), for both definitions of the fraud outcome variable, the effect of coverage is heterogeneous by region. As a matter of fact, we witness no significance in the North-West region, while coverage appears to be significant for the presence of fraud in the SouthEast region. More specifically, in the latter region the presence of telephone coverage, according to our results, decrease the probability of a polling center to be fraudolent by 28.5%. Similarly, the 2G coverage decreases the percentage of fraudolent voters by 27.1% in the Southeast region. It is likely that the absence of significance of the effect in the North-West region affects the point estimates in the country as a whole, causing a decrease in the absolute value of point estimates and a consequent loss in significance.  
}
