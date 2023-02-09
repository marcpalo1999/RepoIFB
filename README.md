# RepoIFB
Creation of a synthetic population with given attirbutes (like age, smoking and dietary habits, genetic mutations etc.), and deriving their probability of developing NASH (Non-Alcoholic Steatotic Hepatitis) using a logistic regression model of the illness.

Each attribute of the patients comes along with a value (beta value or odds ratio) which has a given relationship with the risk of developing the illness.

Archives:

1. Final_DF.xlsx: Dataframe of the synthetic population (100.000 people) which have the given characteristics you can see inside, with distributions analogous to those of South Korea (the population emulates a South Korean sample of 100.000 people, mimicing the same men/women proportion, the same age distribution, the same negative exponential consumprion of alcohol per capita etc.). Finally, the label ill(healthy is shown in the last oclumn of the dataframe, generated sampling from the extracted probability from the logistic regression model. Only some of the characteristics given in the dataframe are determinant for the illness. Some other are dummy variables introduced to confuse.

2. Nash_model-vFinal.R: The R code used to generate the Final_DF.xlsx dataframe.

3. NASH factores and odds ratio.xlsx: the dataframe from where the odds ratio (the log increasing odds to develop NASH given an isolated unit increase of a characteristic). This dataframe is only used inside the R script for the logistic regression of the illness.

CHALLANGE: A final challange proposed is to determine the characteristics which actually influence the illness from analysing only the Final_DF.xlsx archive. Several methods could be used. Final solution can be checked inside the NASH factores and odds ratio.xlsx to see which variables are determinant and which ones are dummy to the illness.

