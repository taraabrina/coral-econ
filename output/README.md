The output folder is created within the working directory for model outcomes.
Currently, the script can only create one output file a day, as the file name
is dependent on the date.

The output file will be in .xlsx format and will contain the following tabs:
1. Yearly_HCC for all n=2,404 reefs under 7 scenarios, the contribution of recruits
   to yearly HCC, and annual number of settled larvae
2. Annual_PH_Recruit which summarises recruit contribution to HCC and settled larvae by year
4. Individual Intervention Gain, which summarises the net HCC for each reef centroid
   (Year 25 HCC with intervention only on that reef - Year 25 HCC without intervention)
5. Group Intervention Gain, which summarises the net HCC for each reef centroid 
   (Year 25 HCC with group intervention on preselected reef IDs - Year 25 HCC without intervention)
