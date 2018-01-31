# Credit Risk Analysis
Situation:
The grantor wants to improve the efficacy of the approval screening process of the HELOC loans’ applicants. Also, the current screening process includes assessment of loan applicants’ portfolios including credit risk management and revenue growth. Without a quick replacement / alternative plan, the grantor will be losing revenue on applicants who are unable to repay the loan as negotiated.

Vision: 
The vision of “FICO Revolving Loan and Credit Risk” project is to refine decision-making processes for the loan grantors in the Consumer Mortgage and Home Equity Line of Credit (HELOC) by developing predictive models to determine which loan applicants are most likely to default and scaling the suggested predictive models so that they can be used to run on the new test data on a regularly basis. Our suggested model should be at least as good as the model being used in the loan approval screening process for the benefit of all internal and external stakeholders

Mission Statement:
The Mission of our “FICO Revolving Loan and Credit Risk” project is to serve the Vision through a compelling, visual and reliable predictive data analytics solution which is empowering and enabling to the grantors to make beneficial loan approval decisions  

Business Problems:
The problem of applicants defaulting (unable to repay the loan amount) who have cleared the loan approval screening process affects the reputation of the financial loan granting organization, the internal and external stakeholders for the organization, trust and loyalty of the current and potential customers, the impact of which is low customer satisfaction, reduced new customer acquisition rate, and increased customer retention costs. A successful solution would be to have a better screening process before granting the loan amount to a particular borrower which would help in revenue generation and thereby incurring less losses. 

Proposed Solution
We propose GAMspline (Generalized additive model) model which uses the classification algorithm that is appropriate for the dataset provided by FICO that will help in providing optimal and feasible solution by predicting if the applicant will default or not. We also refer to the other models used that implement similar solution to find the correlation and association between the variables of the dataset. 

About the Project:
The ‘RiskData’ dataset provided by The FICO® Academic Engagement Program (AEP) contained 247,500 approved-and-then-booked applicants, out of which 242,041 are good loans (Low Risk of Default) and rest 5,459 are bad loans (High Risk of Default). FICO has performed Random Stratified Sampling process and down sampled the entire dataset to deal with imbalances between the good loan and bad loan classes.
The problem was the efficacy of the current approval screening process of the HELOC loans’ applicants with regards to the portfolio’s credit risk management and revenue growth. 
In performing data discovery and pre-processing on the dataset, we found 10,459 observations and 37 variables - 1 Response Variable and 36 Predictors. (Refer to Section 3). Further data discovery, we found more discrepancies in the dataset such as mismatch of factor and numeric data types, NA Values in “Bank_Relationship” Predictor-column, Outliers in “Debt_Ratio” Predictor-Column, Missing Values in “CB_IL_Util” Predictor-Column, and “999” Values in the “CB_Mos_Since_Dlq” Predictor-Column (Refer to Section 3.1 - 3.7)
We used full scale plots to roughly approximate roughly the proportion of missing data for the better understanding. (Refer to Section 3.8). Also, we implemented data processing and cleanup in order to minimize the inconsistency between the variables and to retain as much information as possible. (Refer to Section 4).
We planned the models to be used on the dataset based on the three Hypothesis we derived: 
		Hypothesis 1: To predict whether or not a loan applicant was going to default using all available Predictor Variables.
		Hypothesis 2: To test whether the best performing model identified in H1 would have improving performance in the absence of highly correlated Predictor Variables.
		Hypothesis 3: To test whether there was any significant improvement in the best performing model based on H1 by selecting only the first 20 most important Predictor Variables (Refer to Section 5.1).
We identified our problem being a classification problem, we worked on 13 models to identify the best model that was fitting our data and then eventually perform prediction on the new data set. (Refer Section 5.2)
We used mice package for imputation of missing values with a total of 5 imputed data sets, and performed all 13 models on the processed dataset. Upon working on 13 different models, we found GAMspline (Generalized additive model) model to be the best model for the provided FICO dataset with a sensitivity of approx. 81%. At the end, after playing around with the data and applying models; we came up with specific suggestions and implications for our proposal problem. (Refer to Section 8). According to our analysis and various model approaches, we found the GamSpline to be functioning better than other models with a sensitivity of approx. 81% for the test data.

Ideal World Scenario (No Customers Default): In the ideal world, for the 250,000 customers we calculated the average amount of loan amount requested to be approximately $40,000 per customer. Since the dataset that was provided to us by FICO was between March 2010 and March 2012, we researched about the average Fixed Rate Mortgage was found to be 4.8% during that time period. Thus, the total principal amount that was to given out to customers as loan was about $10 billion. Based on these values, the interest revenue accumulated for HELOC between 2010 and 2012 was around $960 million. Total Revenue generated in 2 years was $10.96 billion.
Calculation: 
Total principal amount: 250,000 (customers) X $40,000 (average loan amount) = $10,000,000,000 = $10 billion
Interest value generated: 250,000 X $40,000 (average loan amount) X 0.048 (average fixed rate mortgage) X 2 (years) = $960,000,000 = $960 million
Total revenue generated: $10 billion + $960 million = $10.96 billion

Real World Scenario(At least some Customers Default): Based on the data analysis, we deduced that the total number of customers who were to Default was around 2.2% of the total customers. So, based on this percentage the total amount in dollars that HELOC lost because of the defaulting customers was $220 million. At the same time, we also assume that HELOC did not recover any money from these bad applicants, which was around $2.1 million. Thus, if we consider all the above assumptions, the total amount was 9.8 billion. All these numbers were generated before we run our models. 
Calculation: 
Revenue lost due to defaulters: $10 billion (total principal amount) X 0.022 (Defaulters rate) = $220 million 
Interested accumulated for 2 years: $220 million X 0.048 X 2 = $21,120,000
Revenue generated for 2 years: $10 billion – ($220million (Revenue lost due to defaulters) – $21,120,000 (Interested accumulated for 2 years)) 
 = $10 billion – ($198,880,000) = $980,112,0000 = $9.8 billion

Results of Completed model(At least some Customers Default): Based on the dataset available to us, there were around 5,459 Defaulters. We got our best result for the available dataset with the “gamSpline” model, with a sensitivity of 81%. This meant that the total number of applicants that our model predicted correctly as defaulter was approximately 4,421 customers. 
	Thus, the model would eventually save approximately $176 million by not granting loans to such applicants from the very beginning. We had similar sensitivity results from our modeling on removing the highly correlated variables of 81.3%, which was expected as these correlated models were explaining similar information.
Calculation: 
Principal: 4,421 X $40,000 = $176,840,000
