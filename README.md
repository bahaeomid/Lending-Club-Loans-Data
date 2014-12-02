Lending Club Loans Data
=======================

##Introduction
Lending Club is a financial institution that provides a medium for borrowers and investors to benefit from a low-cost online environment to meet their financial needs. According to Lending Club, the better rates offered to applicants are influenced by several factors such as creditworthiness, debt to income ratio, and recent credit activity.
This study was conducted to investigate the various factors affecting the interest rates offered by Lending Club. It was found that there is a significant association between interest rate offered and the applicant’s credit score, amount of loan requested, and the length of the loan. The analysis performed suggests that longer-term loans, higher loan amounts, and lower credit scores are associated with increased interest rates.

##Methods
###Data Collection
The data used for this analysis was downloaded from Coursera.org on November 8, 2013 which includes a sample of 2,500 loans from Lending Club.
###Exploratory Analysis
Non-graphical methods such as data summaries and cross-tabulation, and graphical methods such as scatter plots and boxplots were utilized to perform exploratory data analysis in order to 
1.Identify missing values
2.Identify outliers and unusual features in the data
3.Verify quality of the data
4.Explore correlations between all variables
5.Transform variables to the appropriate class for further analysis
###Statistical Modelling
To exhibit the relationship between the interest rate and the explanatory variables, a multivariate linear regression model was employed . The explanatory variables and the model were selected based on the outcome of our exploratory analysis and the coefficients in the model were estimated with ordinary least squares.
###Reproducibility
The current analyses are reproducible through accessing the R markdown file Ass1.Rmd. To obtain the exact results given in the current document, the same data source must be used. The applicants’ requirements set by Lending Club may change over time and hence one may not reach the same conclusions if the analyses are performed on a different dataset.
