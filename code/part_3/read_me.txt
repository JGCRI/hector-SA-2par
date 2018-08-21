read me code/part_3


So it turns out that is really hard to automate code to generate the configuration, batch.xml, and other files that are required 
to do the find policy cost curve. There is a lot that will have to be done by hand, this read me outlines the steps. 


GCAM SET UP
Before you set up GCAM to run the cost cruve you need CO2 price results from a policy target finder run (we did this in part_2)
1. Create a carbon_tax_runName.xml based on the code/part_3/carbon_tax_template.xml. Replace the values with the correct CO2 prices. This 
	xml should be saved in gcam-core/input/policy and will be read into the batch file. 
2. See code/part_3/batch_template.xml for an example of the batch file. Each file set nees to point to a hectorSA ini file and the carbon_tax.xml. 
3. See code/part_3/config-template.xml for an example of the configuration file. Make sure that the configuration file uses the batch file and
	that batch mode is turned on. Also check to make sure that the createCostCurve boolean is turned ON and that the find-path boolean is turned OFF.
	
	** you can check the GCAM results, the RF or temperautre results should match the results from the policy run from part_2. 
	
PROCESSING OUTPUT 
For some reason rgcam is not happy with the xml data base created by the cost curve configuration. Instead you will use Model Interface 
to run batches and pull out specific query results. These results will be saved in out-3. 

After all of the queries are saved in the out-3/queries csv code/part_3/A1.format_csv.R can be sourced to format the query results into something that can be plotted.