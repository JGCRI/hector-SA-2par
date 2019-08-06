read me code/part_3


So it turns out that is really hard to automate code to generate the configuration, batch.xml, and other files that are required 
to find the total cost of a policy run. There are mulitple ways to do this. Regardless of method used to get the total cost the only way to extract 
this information from the database is to use the model interface to extract the results. 


DURING PART 2
If we are only interested in the target runs and do not need any reference run information we can set up the configuration file in part 2 so that it creates the total cost when doing the policy run. However this is not advisable if you also need the refernce run. 


GCAM-CORE METHOD
Before you set up GCAM to run the cost cruve you need CO2 price results from a policy target finder run (we did this in part_2)
1. Create a carbon_tax_runName.xml based on the code/part_3/carbon_tax_template.xml. Replace the values with the correct CO2 prices. This 
	xml should be saved in gcam-core/input/policy and will be read into the batch file. 
2. See code/part_3/batch_template.xml for an example of the batch file. Each file set nees to point to a hectorSA ini file and the carbon_tax.xml. 
3. See code/part_3/config-template.xml for an example of the configuration file. Make sure that the configuration file uses the batch file and
	that batch mode is turned on. Also check to make sure that the createCostCurve boolean is turned ON and that the find-path boolean is turned OFF.
	
	** you can check the GCAM results, the RF or temperautre results should match the results from the policy run from part_2. This requires a 
	lot of work before starting the run and is prone to set up error. But is quick to solve.
	
GCAM-PARALLEL METHOD
1. Change the <Value name="createCostCurve">0</Value> in the configuration files bools section from 0 to 1. 
2. Set up the parallel GCAM to run the target finder, this really only works if you are NOT running a reference run. This requires minimal set uptime but will take a long time to run. 

	
PROCESSING OUTPUT 
For some reason rgcam is not happy with the xml data base created by the cost curve configuration. Instead you will use Model Interface 
to get the undiscounted and discounted results from the data bases. Save these results in csv files called gcam_rslts_discounted and 
gcam_rslts_undiscounted. Becasue there are no units returned by model interface you will have to convert to desired dollar years and add unit information.