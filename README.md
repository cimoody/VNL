# VNL: Value Next Lab code and trial code
## Instructions to use the code in this directory
1. Start with source(librariesVNL.R)
2. source(getResultDate.R): Note:  dDir on line 19 of getResultDate.R must be set to your data directory for the code to work
3. source(alignThreshold.R) after changing dDir on line 24 to your data directory.
4. source(VNL_LinearRegression.R) to use the functions to make the data tables useful in time. Must change dDir on line 17 to your data directory. The two functions that are to be used are:
	* getTrainMatrix() which takes all of the lab data and considers it as if it was one patient with many results. 
	* getTimeTrainMatrix() which takes all of the labs for each ENC_CSN_ID and stores the lab results and day for the threshold day and the lab results and days for 10 days prior to the threshold day. The labs are normalized to the threshold (lab at threshold = 1). Missing lab values are inferred from previous lab values as constant until the next measurement. If no prior lab value exists, then the previous lab values are inferred as constant from the oldest lab.
	* There is also a lot of code for linear regression attempts. the 3rd attempt on the results from getTimeTrainMatrix() for all the available labs and selecting only the INT_FLAG=1 patients, is decent. You must hit <Return> to cycle through the plots from the regression attempts to finish the source() command.
5. source(VNL_kNN_kMeans.R). Must change dDir on line 25. Main points:
	* createMeta() cleans up the data from getTimeTrainMatrix() for classification.
	* Lines 204-395 are attempts to use classification to predict the THRESHOLD_TIME. They do not work.
	* After line 395, classification attempts are made to predict the INT_FLAG so that the 3rd generalized multiple linear regression from VNL_LinearRegression.R can be used without prior knowledge on whether the patient will cross the threshold. 
