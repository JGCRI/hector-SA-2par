# hector-SA-npar

### About 


Hector sensitivity analysis for n combined parameter sets, the objective of this project is to investiage the uncertainty of Carbon-cycle paramters. 
This read me provides a general overview of the project code, for more deatils about the projects please refer to the wiki. 

The project code is broken up into three parts. 
* `code/part_1`: standalone hector is run x times using the quasi random combinations of n C parameters. After cleaning up the hector 
results the hector runs are compared to the observational data products using the Dn method.
* `code/part_2`: select Hector parameter sets based on some criteria to pass along to use in Hector-GCAM.
* `code/visualize`: contains the code to visualize and graph results.

