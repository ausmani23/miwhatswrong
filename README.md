# What's Wrong with Mass Incarceration?

This repository contains the code that generates results and figures for *What's Wrong with Mass Incarceration?* by Christopher Lewis and Adaner Usmani. To do more than browse the code, you will need the source data. We do not store these on Github. Contact Adaner for more information. Replication files for the book will only be ready when the book is finished. 

## Folder and File Structure

+ 'code/' contains all replication code.

	**NECESSITY**
	+ thetradeoff_necessity - Illustrations of The Efficiency-Feasibility Paradox

    **EFFICIENCY**
    + thetradeoff_efficiency_01data.R - Loads main datasets into memory, creates some descriptive figures
    + thetradeoff_efficiency_02getConsequences.R - Calculates costs/benefits looping through all points in the 2x2, under different methods
    + thetradeoff_efficiency_03output.R - Outputs tables and figures based on calculations above.
    + thetradeoff_fwbalancecalcs.R - Estimates how many prisoner-years are added by adding a police officer
	+ calculate_homicides.R - Code for three lower-level functions on which these scripts depends. The main is 'calculate_homicides', which takes a starting point in the 2x2, an ending point in the 2x2, and estimates what is likely to happen to the # of homicides
    + calculate_costsbenefits.R - Code for function 'calculate_costsbenefits.R' which calculates all the costs and benefits for a point in this 2x2 space
	
    
    **FAIRNESS**
    + thetradeoff_fairness_01thearrested - Calculates the share ever arrested, arrested in 2019, killed by the police
    + thetradeoff_fairness_02theincarcerated - Calculates the share imprisoned
    + thetradeoff_fairness_03themurdered - Calculates the share murdered
    + thetradeoff_fairness_04output.R - Outputs figures based on the calculations above
	
## Related Work

The book builds on other work first published elsewhere. Some of the ideas in Part I and Part II can be found in an essay titled 'Abolition of What?', forthcoming in the Journal of Criminal Law and Criminology. We first sketched the ideas in Part III in a paper titled 'The Injustice of Under-Policing', published in the American Journal of Law and Equality. The results of these papers can be replicated by the material in this repository as well.

To replicate the figures and calculations in 'Abolition of What?', see (https://github.com/ausmani23/aow)[this repository]. To replicate the figures and calculations in 'The Injustice of Underpolicing', see ajle_figs.R (and to replicate the figures and calculations for our 'Reply to Karakantsis', see ajle_alec.R). To run these files, you will need to download the source data from [this link](https://www.dropbox.com/sh/dd6ml5emjb3zt95/AAD-8nV2GOlbB9sLOe9v_lska?dl=0). Where our calculations rely on data from the History of Punishment project (which will not be public for a few years) we share the intermediate but not the original data. 

Note that the code does not replicate Figures 1 and 2 from 'The Injustice of Underpolicing' exactly. A few of the non-US points have moved slightly, reflecting updates to the History of Punishment dataset between February 2022 (when the figures were finalized for publication) and August 2022 (when the replication code was drafted). The differences are minor (the code shows that correlations between the new and published series are >0.96), but anyone looking to use these data themselves should use the newer estimates. Most of the data we used in this paper come from public datasets available from the (https://www.unodc.org/)[UNODC] and the (https://www.prisonstudies.org/)[World Prison Brief], so users looking to replicate our descriptive inferences about the US-RoW contrast are encouraged to download the relevant data themselves.

Please contact Adaner with questions about how to run the code or if you encounter trouble replicating any results. 
    
    **MISCELLANEOUS**
    + ajle_figs.R - Replication code for 'The Injustice of Under-Policing'. Note that Figure 6 and associated calculations are replicated by the 'thetradeoff_fairness' files. Because these files will continue to be updated for the book, I've put archived versions of them in the repository. These have the suffix '_ajlecopy'. 
	+ ajle_alec.R - Replication code for our response to Alec Karakantsis. 
    
## Notes

To run the code, you'll have to mimic the folder structure I use, putting any replication data files in a '/data' folder and creating an '/output' folder to house figures and calculations. When files have numbers, either at the start of the file or in the middle, it indicates that they need to be run in a particular order. 



