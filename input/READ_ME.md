This dir contains the inputs required by the hector-SA-npar project. 

* **observations/** : contains the raw observational data that Hector output is compared too
* **cms_query.xml** : GCAM queries
* **hector-gcam_template.ini** : the hector-gcam ini template, because `part_2/B.hector-gcam_ini_xml_maker.R`saves the ini files at  `gcam-parallel/input/climate/sub_dir` all of the relative pathsto the emissions in the ini file MUST reflect this, or GCAM will fail!
* **selected_output_variables.csv** : contains the parameter combinations to use in the hector-gcam ini files
* **xml_pointer_template.xml** : template to use to generate the xmls that point to the hector-gcam ini files in  `gcam-parallel/input/climate/sub_dir`.
* **hector-input** : contains csv files that MUST be copied into the hector/inst/input/emissions dir
