# VNL: Value Next Lab code and trial code
## Instructions to use the code in this directory
* Start with source(librariesVNL.R)
* 2nd, source(getResultDate.R): Note:  dDir on line 19 of getResultDate.R must be set to your data directory for the code to work
* 3rd, source(alignThreshold.R) after changing dDir on line 24 to your data directory.
* 4th source(VNL_LinearRegression.R) to use the functions to make the data tables useful in time. Must change dDir on line 17 to your data directory. The two functions that are to be used are:
	* getTrainMatrix() which takes all of the lab data and considers it as if it was one patient with many results. 
	* getTimeTrainMatrix() which takes all of the labs for each ENC_CSN_ID and stores the lab results and day for the threshold day and the lab results and days for 10 days prior to the threshold day. The labs are normalized to the threshold (lab at threshold = 1). Missing lab values are inferred from previous lab values as constant until the next measurement. If no prior lab value exists, then the previous lab values are inferred as constant from the oldest lab.
