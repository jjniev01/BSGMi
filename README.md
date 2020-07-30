[![N|Solid](http://maps.worldpop.org.uk/img/worldpop-logo.png)](http://maps.worldpop.org.uk)

# BSGM : Built-Settlement Growth Model v.1a
---


##  Introduction
  There has been a recent increase in available urban feature or [built-settlement][f1] (BS) extent dataset extracted from remotely sensed data. However, even if these data are multi-temporal and not consisting of a single time point, most of these data are cross-sectional in nature with gaps in between their observations. This leaves the question of how did the urban/BS landscape change from the observed state at time A to the observed state at time B and what did it look like at regular intervals in-between? To repeat the extraction procedures for every year could be resource intensive or areas could not have suitable remotely-sensed imagery available for the time period due to instrumentation issues or more natural phenomenon such as cloud cover. Additionally, such extraction methods cannot be used to extrapolate beyond the period for which there is imagery, limiting forecasting and historical mapping.

  These issues are particularly important when it comes to mapping population into the future and the past. [Nieves et al. (2017)][r1] showed that regardless of the country, BS extents, and data serving as proxies for BS, are the most important covariates in predicting the spatial distribution of populations. With the WorldPop Project scaling up its map production to dasymetrically map populations for all countries for every year from 2000 to 2020, it was realized that BS extents would be a key driver in determining the spatial distribution of populations for each year. However, at the beginning of this project in the summer of 2016, the selected BS datasets of the optical-based Global Human Settlement Layer ([GHSL][r2]) was only available for 1975, 1990, 2000, and 2014 and the Synthetic Aperture Radar (SAR)-based Global Urban Footprint ([GUF][r3]) was only available circa 2012. This was when it was decided that a flexible spatially-explicit modeling framework for interpolating and extrapolating the transition of areas from non-BS to BS regardless of the timeperiod or location on the globe was needed to drive the global population modeling. 
  
  This is when we developed the BSGM, interpolative (BSGMi) which utilizes the infrastructure and code-base developed for the global population modeling to rapidly predict annual BS extents across the globe with the primary objective being to improve population mapping. All input census data and covariates for 249 countries are stored on the [WorldPop](http://www.worldpop.org.uk) FTP and are directly sourced by the BSGM by using the R package [wpgpCovariates](https://github.com/wpgp/wpgpCovariates) and the support package [wpUtilities](https://github.com/wpgp/wpUtilities). This is the documentation for the above noted version and preceding version tagged models. Note that the code utilized in the production of datasets for the global project, an alpha version of this model, are within the [BSGMi_alpha](https://github.com/wpgp/BSGMi_alpha) repository.



##  Table of Contents
* [Introduction](https://bitbucket.org/jjnieves/bsgm/src/master/)
* [Repository Overview](https://bitbucket.org/jjnieves/bsgm/src/master/docs/RepoOverview.md)
* [Methods](https://bitbucket.org/jjnieves/bsgm/src/master/docs/Methods.md)
* [Dependencies and Installation](https://bitbucket.org/jjnieves/bsgm/src/master/docs/Dependencies.md)
* [Getting Started](https://bitbucket.org/jjnieves/bsgm/src/master/docs/GettingStarted.md)
* [Inputs](https://bitbucket.org/jjnieves/bsgm/src/master/docs/Inputs.md)
* [Outputs](https://bitbucket.org/jjnieves/bsgm/src/master/docs/Outputs.md)
* [HPC Runs](https://bitbucket.org/jjnieves/bsgm/src/master/docs/HPCRuns.md)
* [Footnotes](https://bitbucket.org/jjnieves/bsgm/src/master/docs/footnotes.md)
* [References](https://bitbucket.org/jjnieves/bsgm/src/master/docs/references.md)

##  Project Team
###  Jeremiah J. Nieves, MSc, PhD Candidate - University of Southampton, U.K.
![Alt text][i1]  
_jeremiah.j.nieves@outlook.com_

Primary Investigator, Code Author and Maintainer, Model Development, Research Design

###  Forrest R. Stevens, PhD - University of Louisville, USA
![Alt text][i2]  

Model and Research Design

###  Andrea E. Gaughan, PhD - University of Louisville, USA
![Alt text][i3]  

Model and Research Design

###  Alessandro Sorichetta, PhD - University of Southampton, U.K.
![Alt text][i4]  

Model and Research Design, PhD Co-Supervisor

###  Catherine Linard, PhD - Universite de Namur & Universite Libre de Brussels, Belgium
![Alt text][i5] 

Model and Research Design

###  Maksym Bondarenko, PhD - University of Southampton, U.K.
![Alt text][i6]  

Author and maintainer of [wpgpCovariates](https://github.com/wpgp/wpgpCovariates) and the support package [wpUtilities](https://github.com/wpgp/wpUtilities), Author of WPRFPMS upon which the code based its data ingestion, High-performance Computing Management and Production Manager for WorldPop Global Efforts

###  Jess Steele, PhD - University of Southampton, U.K.
![Alt text][i7]  

PhD Co-supervisor

###  Andrew J. Tatem, PhD - University of Southampton, U.K.
![Alt text][i8]

Model and Research Design, PhD Co-Supervisor

###  Heather Chamberlain - University of Southampton, U.K.
![Alt text][i10]
Data and Population Preprocessing

###  David Kerr - University of Southampton, U.K.
![Alt text][i11]

Data and Population Preprocessing

###  Nik Vesnikos - University of Southampton, U.K.
![Alt text][i12]
Data and Population Preprocessing


##  Acknowledgements
We'd like to acknowledge Thomas Esch for his expertise on GUF, providing early access to GUF-related datasets, and feedback on early versions of this model. We'd also like to thank Deborah Balk for her feedback and insights on early versions of this model, particularly with regards to population dynamics. We'd like to recognize our WorldPop global partners at the Center for International Earth Science Information Network for providing the annual subnational population data across the globe. Huge thanks to the rest of the WorldPop team for their ongoing support and feedback through all of the countless informal conversations had. 



[f1]: /docs/footnotes.md "Footnote 1"
[r1]: /docs/references.md "Journal of the Royal Society Interface, forthcoming"
[r2]: /docs/references.md ""
[r3]: /docs/references.md ""
[i1]: http://www.worldpop.org.uk/about_our_work/team/jn.jpg
[i2]: http://www.worldpop.org.uk/about_our_work/team/fs.jpg
[i3]: http://www.worldpop.org.uk/about_our_work/team/ag.jpg
[i4]: http://www.worldpop.org.uk/about_our_work/team/as.jpg
[i5]: http://www.worldpop.org.uk/about_our_work/team/lc.jpg
[i6]: http://www.worldpop.org.uk/about_our_work/team/M_Bon.jpg
[i7]: http://www.worldpop.org.uk/about_our_work/team/js.jpg
[i8]: http://www.worldpop.org.uk/about_our_work/team/at.jpg
[i10]: http://www.worldpop.org.uk/about_our_work/team/heathercham.jpg