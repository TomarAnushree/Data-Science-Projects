# Overview:

In recent years, attention has increasingly been paid to human resources (HR), since worker quality and skills represent a growth factor and a real competitive advantage for companies. After proving its mettle in sales and marketing, artificial intelligence is also becoming central to employee-related decisions within HR management. Organizational growth largely depends on staff retention. Losing employees frequently impacts the morale of the organization and hiring new employees is more expensive than retaining existing ones. 


# Business Problem:

HR Department of a large insurance company is facing sales team attrition so staffing becomes a crucial aspect.we need to analyse the attrition rate because insurance sales teams help insurance companies generate new business by contacting potential customers and selling one or more types of insurance. 


# [Dataset Source:](https://datahack.analyticsvidhya.com/contest/job-a-thon-november-2021/)

Data are provided with the monthly information for a segment of employees for 2016 and 2017 and as a Data science soulution we need to predict whether a current employee will be leaving the organization in the upcoming two quarters (01 Jan 2018 - 01 July 2018) or not.


# Data Description:

1. Demographics of the employee (city, age, gender etc.)

2. Tenure information (joining date, Last Date)

3. Historical data regarding the performance of the employee (Quarterly rating, Monthly business acquired, designation, salary)




# 1.	Train Data

|Variable |	Defination     |
|----------|--------------|
|MMMM-YY      |	Reporting Date (Monthly)|
|Emp_ID       |	Unique id for employees     |
|Age	        |	Age of the employee |
|Gender  |Gender of the employee|
|City|	City Code of the employee|
|Education_Level	|Education level : Bachelor, Master or College|
|Salary|Salary of the employee|
|Dateofjoining|	Joining date for the employee|
|LastWorkingDate|	Last date of working for the employee|
|Joining Designation|	Designation of the employee at the time of joining|
|Designation|	Designation of the employee at the time of reporting|
|Total_Business_Value|The total business value acquired by the employee in a month
|                   |(negative business indicates cancellation/refund of sold |   |                   |insurance policies)  |
|Quarterly Rating|Quarterly rating of the employee: 1,2,3,4 (higher is better)|


	



# 2. Test Data

|Variable	|Defination|
|-----------|------|
|Emp_ID   |	Unique Id for the employees|

# Sample Submission

|Variable|Defination|
|-----------|------|
|Emp_ID|Employee ID|
|Target|0: if the employee does not leave the organization,|
|      |1: if the employee leaves the organization


# Evaluation Metric

The evaluation metric for this competition is macro f1_score



