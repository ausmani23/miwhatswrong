# What's Wrong with Mass Incarceration?

This repository contains the code that generates results and figures for *What's Wrong with Mass Incarceration?* by Christopher Lewis and Adaner Usmani. To do more than browse the code, you will need the source data. We do not store these on Github. Contact Adaner for more information. Replication files for the book will only be ready when the book is finished. 

To replicate figures and calculations for 'The Injustice of Under-Policing', see ajle_figs.R. You can download the source data from [this link]. (We cannot make the original data from the History of Punishment available until that dataset is public, but this folder includes the specific data we rely on in the AJLE paper.) Please contact Adaner with questions about how to run the code or if you encounter trouble replicating any results.  

## Folder and File Structure

+ 'code/' contains all replication code.

    **EFFICIENCY**
    + thetradeoff_efficiency_01data.R - Loads main datasets into memory, creates some descriptive figures
    + thetradeoff_efficiency_02getConsequences.R - Calculates costs/benefits looping through all points in the 2x2, under different methods
    + thetradeoff_efficiency_03output.R - Outputs tables and figures based on calculations above.
    + calculate_homicides.R - Code for three lower-level functions on which these scripts depends. The main is 'calculate_homicides', which takes a starting point in the 2x2, an ending point in the 2x2, and estimates what is likely to happen to the # of homicides
    + calculate_costsbenefits.R - Code for function 'calculate_costsbenefits.R' which calculates all the costs and benefits for a point in this 2x2 space
    
    **FAIRNESS**
    + thetradeoff_fairness_01thearrested - Calculates the share ever arrested, arrested in 2019, killed by the police
    + thetradeoff_fairness_02theincarcerated - Calculates the share imprisoned
    + thetradeoff_fairness_03themurdered - Calculates the share murdered
    + thetradeoff_fairness_04output.R - Outputs figures based on the calculations above
    
    **MISCELLANEOUS**
    + ajle_figs.R - Replication code for 'The Injustice of Under-Policing'. Note that Figure 6 and associated calculations are replicated by the 'thetradeoff_fairness' files. Because these files continue to be updated for the book, I've put archived versions of them in the repository. These have the suffix '_ajlecopy'. 
	+ ajle_alec.R - Replication code for our response to Alec Karakantsis. 
    + aow.R - Replication code for 'Abolition of What?'
    
## Notes

When files have numbers, either at the start of the file or in the middle, it indicates that they need to be run in a particular order. 



