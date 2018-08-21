# hector-SA-npar

Hector sensitivity analysis for n combined parameter sets, the objective of this project is to investigate the economic impacts of uncertainty of in the carbon cycle. This read me provides a general overview of the project  repository, for more details about the projects please refer to the wiki. 

Contents
* `code` : project code 
* `out-fig`: diagrams and figures 
* `input`: project inputs such as the observational records, ini files, and queries 
* `out-1`: intermediate output will be saved here, the only file that should be commited in this dir should be **A.par4_combinations.csv**, the parameter combinations used for this analysis 
* `out-2`: the directory containing the sub-directories of the Dn metric results and GCAM reference/target run results (these results are not committed on GitHub)
* `out-3`: the directory containing the sub-directories of the GCAM policy cost runs

Notes about this branch
* Before taking a break from CMS we were trying to figure out if we should use an omptimized or stacked Dn metric methodology. We were also trying to figure out if the full observational record could be used or if it should stop when the rcp emissions start. Should look at local/docs/meeting_notes/next_meeting.dox for a refresher as to where we left off. 


