# H1B-Prediction
Wanna know whether your case status will get approved if you apply for H1-B visa?. Don't worry!!! I have created a webpage that is built on **django** which uses classification machine learning algorithm to give you the prediction. Also, take a look at the statistical findings to get a knowledge about the important factors behind the scene:) which is presented using **R** and **Python**.

### <ins>Table of contents</ins>:
  - [ Introduction ](#intro)
  - [ Data Description ](#desc)
  - [ EDA ](#eda)
  - [ Result ](#res)
  
  
<a name='intro'></a>
### Introduction:
Every year hundreds of students come to US with the hope of a successful career. The main motivation for most of the students to come to US is the demand of high job prospects and high wages. For any student to work in US, they must acquire a H-1B visa from the U.S. Citizenship and Immigration Services (USCIS). The steps involved in this process is as follows: 
  - Finding an H1B Sponsor
  - Employer Submits Labour Conditions Approval (LCA)
  - Employer Submits Form I-129
  - Applicant Completes Application at a US Embassy or Consulate

So,the quest to solve the questions like wo are the companies that are filling Labour Conditions Approval (LCA)? , which roles or job titles are filled for LCA?, where these roles are mostly centralized?, how much wages are provided to corresponding roles?, what is the case of a “Data Analyst or Scientist”?. i have used predictive modeling and a data mining approach.

<a name='desc'></a>
### Data Description:
We considered the data from 2014-2018 which has an combined entry of about 2545660 and around 50 features. After applying Dimensionality reduction and feature selection, I have considered the top 10 features that captures the variance better which is listed [ here ](#table) 

The data pre-processing involed:
  - Data imputation
  - Truncated and Factorised the SOC code and Sector code
  - data wrangling
  - Geo coding to obtain the geo locations of the applicant's state and county
  - Multinomial to Binomial classification
  - Tackled class imbalance using methods like under sampling, over sampling, smote, etc
 
<a name='eda'></a>
### Exploratory Data Analysis: 

 
<a name="table"></a>
### Data features
| Field | Meaning |
| ----------- | ----------- |
| CASE STATUS | decision variable| 
| SOC CODE | Occupational code  | 
| SECTOR CODE | Industry code | 
| EMPLOYMENT PERIOD | Total duration of employment requested for the applicant. | 
| TOTAL WAGE | Total Wage per year for the job | 
| YEAR | Application Year | 
| SOC CODE | Occupational code  | 
| FULL TIME POSITION | whether the applicant has a full-time |
