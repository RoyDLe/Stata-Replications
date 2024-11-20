/********************
MICROECONOMETRICS 
*********************

*********************/

*** Program: IV_2SLS_ShiftShare.do
*** first created: 16/03/2024
*** last updated:  27/03/2024


/*----------------------
Initial script configuration
-----------------------*/

	//Please replace your path in the command below
	cd "C:"
	
	// Preparing the necessary dataset
	clear all
	use pset_2_q_1.dta
	
	// Installing the necessary packages
	ssc install outreg2
	ssc install ivreg2
	ssc install ranktest
	
/*----------------------
Exercise 1 
-----------------------*/
*** Point 1 ***
	* 1.a *
quietly {
	// Collapsing data to calculate mean of education level by birth year and quarter
	collapse Education birthdate, by(birthyear birthqtr)

	// Showing an additional way to perform the action done by the previous command
	sort birthdate
	egen mu_edu = mean(Education), by (birthyear birthqtr)
	keep mu_edu birthyear birthqtr birthdate
	duplicates drop
	rename mu_edu Education
	
	// Replicating Figure I of Angrist and Krueger (1991)
	twoway (connected Education birthdate if birthdate<1940, sort mlabel(birthqtr) mlabcolor(black) mcolor(black) lc(black)), title("Figure I")	subtitle("Years of Education and Season of Birth") ytitle("Years of Completed Education") xtitle("Year of Birth") t2title("1980 Census") t1title("Note. Quarter of birth is listed below each observation")
	graph save FigureI, replace
	
	// Replicating Figure II of Angrist and Krueger (1991)
	twoway (connected Education birthdate if 1940<=birthdate & birthdate<1950, sort mlabel(birthqtr) mlabcolor(black) mcolor(black) lc(black)), title("Figure II")	subtitle("Years of Education and Season of Birth") ytitle("Years of Completed Education") xtitle("Year of Birth") t2title("1980 Census") t1title("Note. Quarter of birth is listed below each observation")
	graph save FigureII, replace
	
	// Replicating Figure III of Angrist and Krueger (1991)
	twoway (connected Education birthdate if birthdate>=1950, sort mlabel(birthqtr) mlabcolor(black) mcolor(black) lc(black)), title("Figure III")	subtitle("Years of Education and Season of Birth") ytitle("Years of Completed Education") xtitle("Year of Birth") t2title("1980 Census") t1title("Note. Quarter of birth is listed below each observation")
	graph save FigureIII, replace
	
	// Commenting on the generated graphics
	* The graphs depict an increasing trend in years of education for men born from the 1930s to the late 1940s, followed by a decreasing one for men born after 1950. This last reversal, as signaled by the paper, is due to the individuals not having yet completed their studies at the time of the 1980 Census and due to school attenance decreasing due to military enrollment caused by the Vietnam War. 
	* Indeed, we are able to observe a 1st quarter effect of lower education relative to subsequent quarters. As a matter of fact, one can notice that in the first two figures, people born in the first quarter of their respective year consistently score lower in years of education with respect to their peers born in the subsequent quarters, with only a few exceptions. In a number of cases, people born in the first quarter of the year have even lower education than people born in the later quarters of the previous years, signaling that the effect of the quarter of birth on education might even exceed the effect of the general increasing trend in education. 
	* Judging by the generated figures, one can therefore argue that quarter of birth is a relevant instrument for level of education. 
	* It should be noted, however, that due to the decreasing trend of education after 1950, the instrument appears to lose its relevance when analyzing people born after said year. This is consistent with the choice of Angrist and Krueger to focus the analysis on men born in the 1930s and 1940s.
}	
	* 1.b *
quietly {
	// Commenting on the plausibility of randomness
	* Being the exact date of birth an inherently random event, quarter of birth is clearly as-good-as-random-ly assigned to individuals of the population, meaning that randomness appears to be satisfied. There is no non-random selection that assigns quarter of birth to people based on personal characteristics, similarly as in the setting in which wage was the outcome of interest. 
	
	// Commenting on the plausibility of exclusion restriction
	* Exclusion restriction, second subcondition for exogeneity, can also be considered plausible. The quarter of birth shouldn't consistently affect health status in any other pathway other than education, as it appears to not be related to other personal characteristics that might affect health, at least at first glance, due to it being seemingly irrelevant in a person's overall life. This appears to apply unless policies that influence health assign people to treatment based on the date of birth.  
	
	// Commenting on the plausibility of exogeneity
	* Given that we stated that both randomness and exclusion restrictions seem plausible, exogeneity is plausible for quarter of birth in a model about health status. From these considerations it appears unlikely that the variable is correlated with the error term in the model.
	
	// Thinking about potential violations for the exogeneity assumption and the exclusion restriction in Angrist and Krueger (1991) setting
	* The exclusion restriction, however, when analyzing more observantly the model, might not actually be satisfied. Firstly, income of parents might be correlated to the quarter of birth, as there might be some mechanisms through which parents with certain income levels are more likely to have children in certain periods of the year. Income level, in turn, can influence the labor market choices and abilities of offsprings, possibly leading to consistent differences in wage outcomes in people born in different periods of the year. Secondly, it might be the case that individuals born in certain quarters of the year are more likely to display some physical or mental issues due to environmental conditions characterizing specific periods of their lives. These aspects, if confirmed, are bound to influence wage in paths different from education. Such a violation of the exclusion restriction would also lead to a violation of the exogeneity assumption. 
	* We note that even in a setting with health as the outcome of interest, the previously made health considerations would definitely violate the exclusion restriction. 
}
	* 1.c *
quietly {
	// Commenting on expected estimates of 2SLS and OLS
	* We expect the OLS estimates to be higher than 2SLS ones.
	* We should expect such a bias on OLS estimates due to education being correlated with other covariates that are also relevant to determine income, due to an omitted variable bias. As a matter of fact, factors such as motivation, cognitive skill and parental socioeconomic status, to name a few, clearly influence both education and income, biasing the OLS estimates that do not include the above-mentioned variables. Then, in the OLS model, education will also likely capture the effect of those unobserved covariates, inflating the OLS estimates of the effect of education on wage returns. On the other hand, 2SLS estimates that use an instrument for education that is uncorrelated with unobserved personal characteristics other than the years of schooling, allows the estimates to factor out the effect on wage that is related to the unobserved characteristics, leading to lower estimates. 
	* However, one should also note that, as the paper from Angrist and Krueger suggests, although the omitted variables appear to be positively correlated with both the outcome and education, the OLS estimates might result in being slighly biased downward, as we will confirm in point 2.e. This happens contrary to our expectations. 
	
	// Commenting on compliers identification 
	* If the instrument is quarter of birth and the outcome is health, identification of compliers does not change, since the definition of compliers is only related to the treatment, education, and the instrument, the quarter of birth. Therefore, compliers will be people that have their education influenced by their quarter of birth, that is people born in the first quarters of the year who drop out right after attaining the legal dropout age, and people from the later quarters of the years that similarly drop out right after attaining their legal dropout age, but resulting in an overall higher education with respect to their peers who can drop out earlier. People who continue on studying are, in this instance, never takers, since their total years of education are independent from their quarter of birth.
}
*** Point 2 ***
	* 2.a *
quietly {
	// Preparing the necessary dataset
	use pset_2_q_2_and_3.dta, clear
	
	// Computing the average of outcome Healthy, saving it as a scalar
	sum Healthy
	scalar mu_y = r(mean)
	
	// Computing the average of outcome Education, saving it as a scalar
	sum Education
	scalar mu_x = r(mean)
}
	* 2.b *
quietly {
	// Running a loop to generate all 4 quarter dummies
	forvalues i=1(1)4{
		gen Quarter`i' = (birthqtr == `i')
	}
	
	// Geneerating all 9 region dummies
	tabulate region, gen(regn)
	
	// Generating dummies for each year of birth
	tabulate birthyear, gen(brthyr)
	
	// Creating a local with Central, Married and the regional dummies
	local reg_dums regn1-regn9
	local Controls Central Married `reg_dums'
	local Birth_Year_FEs brthyr1-brthyr10
}
	* 2.c and 2.d *
quietly {
	// Running the three regressions and generating the corresponding table
	reg Healthy Education, robust
	outreg2 using TABLE_Q_2.xls, replace ctitle(OLS 1) nocon addtext(Controls, NO, Year of birth FE, NO) addstat(Mean y, mu_y,Mean x, mu_x)
	
	reg Healthy Education `Controls', robust
	outreg2 using TABLE_Q_2.xls, append ctitle(OLS 2) nocon addtext(Controls, YES, Year of birth FE, NO) keep(Education) addstat(Mean y, mu_y,Mean x, mu_x)
	
	reg Healthy Education `Controls' `Birth_Year_FEs', robust
	outreg2 using TABLE_Q_2.xls, append ctitle(OLS 3) nocon addtext(Controls, YES, Year of birth FE, YES) keep(Education) addstat(Mean y, mu_y,Mean x, mu_x)
}	
	* 2.e and 2.f *
quietly {
	// Running the three IV regressions and appending the lines to the previous table	
	ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3), robust
	scalar F_IV_1 = e(widstat)
	outreg2 using TABLE_Q_2.xls, nocons append keep(Education) ctitle(2SLS 1) addstat(Mean y, mu_y,Mean x, mu_x,F statistic IVs, F_IV_1) addtext(Controls, NO, Year of birth FE, NO) nor2
	
	ivreg2 Healthy `Controls' (Education = Quarter1 Quarter2 Quarter3), robust
	scalar F_IV_2 = e(widstat)
	outreg2 using TABLE_Q_2.xls, nocons keep(Education) append ctitle(2SLS 2) addstat(Mean y, mu_y,Mean x, mu_x,F statistic IVs, F_IV_2) addtext(Controls, YES, Year of birth FE, NO) nor2

	ivreg2 Healthy `Controls' `Birth_Year_FEs' (Education = Quarter1 Quarter2 Quarter3), robust
	scalar F_IV_3= e(widstat)
	outreg2 using TABLE_Q_2.xls, nocons keep(Education) append ctitle(2SLS 3) addstat(Mean y, mu_y,Mean x, mu_x,F statistic IVs, F_IV_3) addtext(Controls, YES, Year of birth FE, YES) nor2
	
	* Note: the nor2 option is added since the R-squared has no statistical meaning in the IV context. 
}	
*** Point 3 ***
	* 3.a *
quietly {
	// Estimating the requested OLS regression and exporting it to an excel table
	reg Healthy Education `Controls' `Birth_Year_FEs', robust
	outreg2 using TABLE_Q_3.xls, replace nocon keep(Education) ctitle(OLS)
}
	* 3.b *
quietly {
	// Getting the Fstatistic
	ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', first savefirst robust
	scalar f_stat = e(widstat)
	est restore _ivreg2_Education
	
	* Note: Using outreg2 directly after these commands would not output the R-squared of the regression. Hence, the following regress command is required. 
	
	// Running the First Stage regression and outputting 
	regress Education Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs', robust
	outreg2 using TABLE_Q_3.xls, excel append addstat("F-statistic IVs", f_stat) keep(Quarter1 Quarter2 Quarter3) nocons ctitle(First stage)	
}
	* 3.c *
quietly {
	//  Running the Reduced Form Regression and exporting the results
	reg Healthy Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs', robust

	// Exporting the Reduced Form results
	outreg2 using TABLE_Q_3.xls, append keep(Quarter1 Quarter2 Quarter3) nocon ctitle(Reduced form)

	// Commenting on the coefficients obtained based on our expectations
	* We first note that from the results in point 2.e it appears that the instrument quarter of birth is efficient in predicting the education attainment, as exhibited by the F-statistic. Indeed, an F-statistic of 61.05 is well above 10, the rule-of-thumb level indicating a satisfactory strength of the instruments.
	* Based on results from point 2.e, we expect the coefficients of Quarter1 and Quarter2 to be negative, with Quarter1 having a lower coefficient than Quarter2. This is because from the IV regressions that we carried out earlier, we get positive and significant coefficients for education on health utilizing the instruments. Since we know that the instruments, and mainly the first months of the year, are negatively correlated with years of education, we can expect in a reduced stage regression to have the coefficients of the quarter dummies to be negatively correlated with health. The Quarter3 coefficient might be positive or negative, but presumably non significant, since people born in the third quarter have levels of education that are similar to people born in the fourth one, considering they are equally influenced by the educational age cutoff. 
	* As a matter of fact, from the regression carried out previously in this point, it is shown that the coefficients of Quarter 1 and Quarter 2 are negative and significant, while the coefficient of Quarter 3 is positive but not significant. These results confirm the expectations formulated upon the results of point 2.e, since individuals born in quarter 1 and quarter 2 will have a lower educational attainment than those born the subsequent quarters, and since education can be expected to positively affect health, which would result in a negative relationship between being born in the first quarters of the year with health status.
}
	* 3.d *
quietly {
	// Estimating the second stage of the regression, storing the results
	ivreg2 Healthy `Controls' `Birth_Year_FEs' (Education = Quarter1-Quarter3), robust
	outreg2 using TABLE_Q_3.xls, append keep(Education) nocons ctitle(Second stage)
}
	* 3.e *
quietly {	
	// Discussing how bias can be generated in the IV regression previously estimated
	* The potential bias in the IV regression can arise from the strength of the instruments and finite sample bias in case the instruments are not strong enough. The output includes the Kleibergen-Paap rk Wald F statistic, which is 61.046. This statistic is compared against critical values provided by Stock and Yogo to assess the strength of the instruments. Since the F-statistic is significantly above the critical values (even for a 5% maximal IV relative bias), it indicates that the instruments are strong. The concern of weak instrument bias, (i.e., weak instrument-explanatory variable correlation) is that it could result in a bias towards the OLS estimates, however, it is mitigated by the strength of the instruments. As a matter of fact, excluded instruments do not appear to be weakly correlated with the endogenous regressiors, meaning weak identification does not appear to be an issue in this regression. 
	
	// Discussing the test of joint significance of the instruments
	* One cannot reject the null hypothesis of the test of joint significance of the instruments. In fact, for such test we can check the Hansen J statistic which has a low value of 1.023, and exhibits a p-value of 0.5997 that does not allow to reject the null under any conventional significance level. 
                                                  
	// Discussing the likelihood of the finite sample bias
	* Based on the size of the F-statistic (61.046 for the Kleibergen-Paap rk Wald F statistic), finite sample bias is unlikely to be an issue in this case. This F-statistic is well above the commonly used rule-of-thumb threshold of 10, suggesting that the instruments are strong. Consequently, the IV estimator is less likely to suffer from the biases that plague weak instrument scenarios, including finite sample bias.	
}	
	* 3.f *
quietly {	
	// Generating local with State dummies
	tabulate bpl, gen(sttbrth)
	local State_FEs sttbrth1-sttbrth50
	* Note: since Wyoming is signaled by sttbrth51, it has been excluded from the local.
	
	// Generating local with year-quarter births
	tabulate birthdate, gen(yrqrtr)
	local Year_Quarter_FEs yrqrtr1-yrqrtr39
	* Note: since year 1939-4 is signaled by yrqrtr40, it has been excluded from the local. 
	
	// Generating local with state-quarter births
	egen stt_qrtr = group (bpl birthqtr)
	tabulate stt_qrtr, gen(stt_qrtr_d)
	local State_Quarter_FEs stt_qrtr_d1-stt_qrtr_d203
	* Note: since Wyoming-4 is signaled by stt_qrtr_d204, it has been excluded from the local. 
}
	* 3.g and 3.h *
quietly {
	// Estimating the first IV regression and computing the F-statistic required
	ivreg2 Healthy `Controls' `Birth_Year_FEs' (Education = `Year_Quarter_FEs'), first savefirst robust
	scalar F_IV_YQ = e(widstat)
	display F_IV_YQ
	* Note: the value is 7.9607721.

	// Estimating the second IV regression and computing the F-statistic required
	ivreg2 Healthy `Controls' `Birth_Year_FEs' `State_FEs' (Education = `State_Quarter_FEs'), first savefirst robust
	scalar F_IV_SQ = e(widstat)
	display F_IV_SQ
	* Note: the value is 3.1663298.
	
	// Commenting on likelihood of regressions to suffer from finite sample bias
	* Quantitively important finite sample biases may affect the estimate in both cases. This is due to the fact that the instruments are weakly correlated with the endogenous variable. As a matter of fact, when the F-statistic of the excluded instruments approaches 1, we are likely to have biases in the 2SLS estimates that tend to the same biases the OLS estimates incur into, even when utilizing very large samples (Bound, Jaeger, Baker 1995). Particularly, we have that both f statistics are smaller than 10, indicating the possibility of a finite sample bias issue. As one can notice, the F-statistic of the excluded instruments of the first regression is higher than the one of the second one. Hence, it might be that while increasing control for State fixed effects and leaving the interaction values between State and Quarter as the instrument might result in decreased standard errors of the estimates, it exacerbates the issue of possible finite sample bias that is encountered when State_FEs is not a control and the interaction value between Year and Quarter of birth are used as an instrument for education. 
}
/*----------------------
Exercise 2
-----------------------*/
*** Point 1 ***
	* 1.a *
quietly {
	/*
	The variable of interest ("import exposure") in Autor et al (2013) has Bartik-form and combines industry growth rates of imports from China with the industry's manufacturing employment share in a specific location (defined a commuting zone in the US), then aggregates these across industries. However, the problem with this vanilla approach is that location-specific characteristics that also affect manufacturing employment may be correlated with import exposure (e.g. US demand-side effects) and the Bartik-like variable suffers from the endogeneity problem. Therefore, Autor et al (2013) create a Bartik instrument that combines lagged industry-location shares with growth rates of Chinese imports in other high-income countries (share and shift, respectively). Industry shares are lagged to account for the fact that local industries, in anticipation of trade shocks, adjust their employment, which would mean that the growth rates themselves would not be meaningful for identification (industry shares themselves are no longer equilibrium objects but are allowed to adjust to the expected growth rate so that shares and labor market outcomes are codetermined). So, if the endogenous Bartik-like variable and the instrument measure shares in the same period, 2SLS could not solve the endogeneity problem. 
	
The result in Pinkham et al implies that Bartik instrument is equivalent to using the initial "lagged" shares as instruments for import exposure in a weighted generalized method of moments estimation, where each industry-level share can be used as an individual instrument. Thus, to argue that the proposed instrument can consistently estimate and identify the effect of interest, we need to argue strict exogeneity of the lagged industry-location shares and relevance (note, here the "shift" component of the instrument only affects the relevance). Relevance implies that the shift-share Bartik instrument has predictive power for changes in import exposure in the US for at least some industries and time periods, conditional on controls. Strict exogeneity means that the lagged initial industry-locations shares are uncorrelated with the error terms in the model conditional on observables, ensuring that the estimates are not affected by omitted factors or reverse-causality. Essentially, the effect of the Chinese import shock in the US on labor market outcomes should only be transmitted via higher exposure of a location whose industry is dependent on said imports. Under homogeneity, these two assumptions are sufficient to identify and consistently measure the effect of interest. A more implicit assumption in the framework is that locations are independent and there are no labor market spillover effects across locations.

Using the shift-share instrument, the potential nuanced mechanisms that make the clear identification of the effect of interest fuzzy are discussed in Autor et al (2013). In particular, the Bartik instrument becomes problematic if technology or product demand shocks are correlated among high-income countries. Furthermore, if US productivity shocks (not China's) drive increasing imports from third countries in high-income economies, the industry shares might in fact be endogenous. Also, if changes in manufacturing employment are linked to common trends appearing in manufacturing sectors in high-income countries (e.g., the manufacturing share of output in these economies is systematically declining) independent of developments in China, we cannot be sure that changes in labor market outcomes are solely attributable to the trade channel. We must assume these effects to be absent. 


*/
}
	* 1.b *
quietly {
	/* 
	Under heterogeneity, different instruments estimate parameters that are a different weighted combination of location-specific parameters. To still recover consistent estimates, Pinkham et al impose additional assumptions. Firstly, within the context of Autor et al (2013), we assume a linear relationship between the lagged location-industry shares and import exposure, where, for each industry, the first stage coefficient is weakly positive over each location. Secondly, the industry-location shares are uncorrelated with both the error term and any aggregate effects specific to that location, conditional on controls. This assumption ensures that the industry-location shares do not introduce bias related to location-specific characteristics inherent in the error term. 
	
By imposing these assumptions, Pinkham et al show that it is possible to interpret the estimates obtained through the Bartik instrument as a weighted composite of the effects across many locations, though it is restricted in the sense that it assumes linear effects within a location and still imposes strong homogeneity assumptions across the industry dimension. The result is generalizable to account for heterogeneity across the time dimension. In a more recent paper, Chaisemartin and Lei (2022) propose an estimator that is robust to heterogeneous effects across locations and over time, imposing a weaker assumption analogous to a parallel trend assumption on first and second stage effects. The general message is that when accounting for heterogeneity, researchers should be comfortable with the pattern of heterogeneity, and thus Pinkham et al proposed ways to probe these to see if there is a reasonable interpretation. 
	*/
}
*** Point 2 ***
	* 2.a and 2.b *
quietly {
	// Running the Modified Rotemberg file with new tables and figures ** 
	clear all
	*cd "C:" Replace Directory
	do "make_rotemberg_summary_ADH_MOD.do"
}
	* 2.c *
quietly {
	/*
	Some industries seem to have a high weight in the determination of the bartik Instrument. If we consider the rothemberg weights,
 for example, we can appreciate named the top 5 industries are significantly higher than the median industry(in terms of weight). However, this heterogeneity does not seem to imply that there is a big TE heterogeneity, since the values of most of those industries are around the value of the overall Bartik Instrument. Therefore, this is not a complete threat on the effect of the TE overall, and we can still interpret this as LATE.
	*/
}
*** Point 3 ***
	* 3.a *
quietly {
	/*
	In this new scenario, Autor et. al (2020) analyze a new relationship, this time between labor supply movement resulting from Chinese imports and political outcomes in the US.  The instrument takes a similar form than the one in Autor et. al. (2013), considering the exports from China to 8 selected non-US countries in the construction of the instrument. We can review each of the required IV identification conditions and compare them to the case of Autor et. al (2013).
	
First, the exclusion restriction requires that there is no direct channel of transmission from the instrument to political outcomes in the US. We believe that the estimation of labor market outcomes could be more prone to violations of the exclusion restriction since there is a more direct mechanism binding import exposure of other high-income countries and labor market outcomes in the US (possibly through the demand channel), while the mechanism is less vivid for political outcomes as the dependent variable. Therefore, within the context of Autor et al (2020), the assumption seems more plausible. One of the more limited possible violations of these condition can be attributed to migration: for example, migrants from the non-US high-income countries that became citizens of the US during the period of study can influences political behaviors depending on situations in their home countries. Same goes for second-generation migrants. However, this effect is more likely to be negligible. 

Second, the relevance condition demands that there is a strong correlation between exports to the US and to the other selected countries. This is most likely to be fulfilled since both US and foreign high-income countries are strongly exposed to trade policies of China: the authors quote cross-country, cross-industry correlations of Chinese imports, reporting Rho's from 0.55 to 0.96. Since the instruments used in both papers are similar, the relevance condition can be argued for in a similar manner. 

Finally, the strict exogeneity condition will require that there is no relationship between the instrument and confounding factors that may also affect political outcomes in the US, conditional on observables. In this point, there are more identification threats than in the Autor et al (2013) case. Global economic conditions and shocks have an important role on both variables, and situations like a Global Economic Crises can be driving factors independently of Chinese exports. Therefore, we consider that this particular condition is more in danger than the Autor et al (2013) counterpart. 
	*/
}
	
