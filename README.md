# Early_stopping_in_PET
Evaluation of Bayes Factor (BF) sequential testing/early stopping for clinical positron emission tomography (PET) studies.  

The project contains code and raw data to reproduce figures, tables and results in article: [Link to BioRxiv]. 

This article is a tutorial on how to apply sequential BF testing for testing mean differences in simple cross-sectional (or two-sample) and paired (or one-sample) designs in PET research.  

## Project structure 

The project is structured as follows: 

1. Simulate data and save as *.rds*
2. Read simulated data, slim down, and save as slimmed *.rds*
3. Read in slimmed data, create data to plot and save as *.csv*
4. Read in plot-data, create figures and save as *.png*

With the ./R/Figures/reproduce_figures.R the user should be able to reproduce the figures in the article and supplementary following the steps above. 

__NB1:__ Data from step 1 above is not saved on github, since the files are to large. Hence the folder "./DerivedData/SimulationData" is empty.   

__NB2:__ Raw data from Figure 6 have been jittered before public sharing. It will hence not exactly produce Figure 6 in the article.    

Tables can be reproduced using the *Draft.Rmd* file. To do so the user must change the path in the first code-chunk: 

`knitr::opts_knit$set(root.dir = normalizePath("/Users/pontus.sigray/Github/Early_stopping_in_PET/"))`

to their own folder. They must also make sure that the paths in the yaml header are correct:

`csl: /Users/pontus.sigray/Github/Early_stopping_in_PET/ArticleDraft/csl/ejnmmi-research.csl`

`bibliography: /Users/pontus.sigray/Github/Early_stopping_in_PET/ArticleDraft/BibTex/Early_stopping_in_clinical_PET_research.bib`

## Background

The main source of inspiration for this tutorial is _"Schönbrodt FD, Wagenmakers E-J, Zehetleitner M, Perugini M (2017): Sequential hypothesis testing with Bayes factors: Efficiently testing mean differences. Psychol Methods. 22: 322."_ [Link to pdf](https://www.researchgate.net/publication/286971067_Sequential_Hypothesis_Testing_With_Bayes_Factors_Efficiently_Testing_Mean_Differences) 

In our project we adapt the purpose to fit the field of PET: we introduce a paired design, focus on sample sizes commonly seen in PET, put greater focus on fixed "error control" using conventional thresholds when planning for studies, and relate the results to the conventional way of performing statistical inference in the field of PET today (i.e., classical fixed N NHST). 

As such, we hope this tutorial will fill two purposes: Introducing Bayesian hypothesis testing using BF to the field of PET, and encourage PET researcher to use adaptive study designs in order to reduce expense and radioactivity exposure.  



