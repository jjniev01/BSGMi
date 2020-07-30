#  BSGM Methods
##  Interpolation
###  Overview
Here we interpolated for every year between a set of observed timepoints, _T_ = \{ _t0, t<sub>1</sub>, t<sub>2</sub>,., t1_ \} where _t0_ is the initial observed timepoint, _t1_ is the final observed timepoint, and all other times observed, _t_, are points lying between _t0_ and _t1_ for which we had observed BS extents. The time between any two observed time points _t_ is referred to as a period, _p_. The model framework here can handle any regular intra-period time-step if the input data corresponds. We generalize the process to determine the timing and number of transitions for each time step independently for each subnational unit, hereafter unit, as follows:
1.	Create a population map for all _t_ in _T_.
2.	At all _t_, for each unit, _i_, extract the time-specific population count within the time specific BS extents and derive the corresponding average BS population density.
3.	For each unit _i_, interpolate the extracted BS population count between each tobserved using piecewise-fit logistic growth curves and BS population density by fitting natural cubic splines across all tobserved. 
4.	Estimate expected total BS extent area, for each _i_ and _t_, in number of pixels, and the expected number of transitions for that time-step based upon predicted unit-specific total BS population and BS population density. 
5.	For every unit _i_, adjust the expected transitions by the sum of all expected transitions across the given period, ta and tb, e.g. 2000-2005 or 2005-2010, and divide by the total observed changes, e.g. 2005 BS extents minus 2000 BS extents. Repeat for all periods _p_.
6.	For each unit _i_, use the adjusted predicted transitions from step five as relative weights within a given unit _i_ to dasymetrically redistribute observed transitions from the larger source period _p_ to smaller individual and unobserved time-steps within _p_, e.g. years, while maintaining the original number of transitions when all time-steps are summed. Repeat for all periods _p_.


We first train a Random Forest (RF) model to produce a continuous surface representing the probability of a given pixel transitioning from non-BS to BS between _t0_ and _t1_. For every time-step, processing each subnational unit independently, we utilized unit-normalized lagged lights-at-night (LAN) data to annually adjust the base RF-derived transition probabilities. The assumption behind this being that areas that underwent the largest increase in brightness, relative to the rest of the unit in a single time-step, have a higher probability of transitioning and vice versa. From the pixels that were known to have transitioned, as indicated in the input BS data, we selected pixels with the _n_ -th highest probabilities for transition, where _n_ was equal to the number of pixels predicted to transition for that time-step. We then converted those pixels to BS, recorded the new BS extents, and used those extents as the basis for the next time-step of transitions to be predicted. This resulted in a series of regularly spaced time-specific binary spatial predictions of the BS extents in raster format.


###  Random Forest (RF) Estimation of the Probability of Transition
Given that the binary dataset of transition/non-transition constitutes an intrinsic "imbalanced set", i.e. there are many more non-transitions than transitions; we adopted a stratified random over/under-sampling method as follows: (i) randomly sample 80 percent of the pixels that transitioned, up to 50,000 and, (ii) randomly sample an equal number of pixels that did not undergo transition. The choice for equal sampling of each stratum was determined by testing different relative proportions and samples sizes until finding the most consistent and best model results, balancing performance and efficiency.


To create a probability of transition surface for the complex and non-linear phenomenon of BS transition, we utilized a RF model to accurately and efficiently model across an entire country at the pixel level in an automatable and parallelizable fashion. We trained the classification RF on whether a pixel had transitioned between time _t0_ and _t1_ (1) or not transitioned (0) against the corresponding values of covariates at time _t0_. We used the constructed RF to predict for the entire given country. Rather than accepting the default output of the RF classifier, which outputs a single predicted class as indicated by the majority of the predictions of its individual constituent trees, we wanted a continuous, 0.00 to 1.00, probability of transitioning in to discriminate between high and low probabilities. Given that we trained the RF as a binary classifier, we took the mean of the individual tree predictions for each pixel. This class probability has a value between 0.00 and 1.00 represents the posterior probability of a pixel being classified by the RF as transitioning between _t0_ and _t1_.


###Population Mapping of Endpoints
To interpolate, we first needed a spatially-explicit best estimate of the subnational unit specific BS population at all observed timepoints in the modelling period. To get this we created a population surface using the available time-specific and, assumed, time-invariant covariates  using the WorldPop RF method, to dasymetrically redistribute the time-specific population totals from the subnational unit level to desired output pixel. 
For any given time point in the population modelling, we included the distance to nearest BS edge for the _t0_ timepoint as population relates to older parts of a BS agglomeration differently from newer one. For example, if we were to model the population map of 2010 we would include the distance to nearest BS edge for 2010 as one of the predictive covariates as well as the distance to nearest BS edge corresponding to the 2000 BS extents. This was done to avoid centres of agglomerations being assigned artificially low population densities relative to the preceding modelled time point. We then extracted and summed by unit the total populations that were spatially coincident with the BS extents and derived the corresponding BS population density for use in the BSGM predictive phases.


###Transition Magnitude Estimation
To estimate the number of transitions for each time-step within the study period, we used the predicted BS population changes and the predicted changes in BS population density for every unit. We first interpolated the BS population count of each unit _i_ for every year, _BSPOP(t)_, by fitting logistic growth curves, in a piecewise manner, i.e. for each time-period between observed points, using the year-specific total population, _K<sub>i</sub>(t)_, as the varying carrying capacity as shown in Equation 1.






* Distance to Nearest Edge of BS
* Proportion of BS within 1 pixel
* Proportion of BS within 5 pixels
* Proportion of BS within 10 pixels
* Proportion of BS within 15 pixels
* Elevation
* Slope
* Distance to Nearest Edge of Level 1 Protected Areas ([WDPA][r1])

The covariate choice was based upon the Africa-specific urban growth model by [Linard et al. 2013][r2].

We then take the constructed RF to predict across the entire modeling area. Rather than accepting the default output of a RF classifier, which puts out the single majority predicted class as indicated by the predictions of its individual constituent trees, we wanted a continuous, 0.00 to 1.00, probability of transitioning. Therefore, given that the RF is trained as a binary classifier, we took the mean of the individual binary predictions and output them in a spatially explicit manner as a raster. More specifically, this value between 0.00 and 1.00 represents the posterior probability of a cell being classified by the RF as having transitioned between _t0_ and _t1_.  
  
  
  

###  Lights-At-Night (LAN) Processing
Here we utilize a corrected database of annually aggregated DMSP LAN data and a collection of annually aggregated VIIRS LAN data. The DMSP data covers the time period of 2000 to 2011 and the VIIRS data covers from 2012 to 2016. The processing which occurs only happens on DMSP or on VIIRS; there is no crossover between the datasets even though they are temporally adjacent as that would cause undue difficulties in resolving method and instrumentation differences between the datasets.
  
For a given LAN dataset, covering the set of regular time points \{ _T_ \} = \{1, 2, ..., _t_\}, we begin by lagging every LAN dataset at a given time point _t_ within \{ _T_ \}, from _t_ = 2 to _t_, such that:

  
![Alt Text][i1]


Then for every _t_, we approach reweighting the lagged LAN data to values between 0.00 and 1.00 for every admin unit _j_ using the following equation for every pixel _i_ and where _x_ is a member of the set \{ _X_ \}:


![Alt Text][i2]


These year specific reweighted data are then output as rasters. The script responsible for this processing can be found in the `/accessories/` folder of the repository and is titled [LANClusterProcessing.R][/accessories/LANClusterProcessing.R].




###  Initial Population Mapping
For any given interpolation period we first need to have a best estimate of the admin. unit specific BS population at the time points bracketing the modeling period. To get this we create a "first draft" population using the available time-specific and, assumed, time-invariant covariates and the WorldPop RF method of dasymetrically redistributing the time-specific population totals from the admin. unit level to 100 meter resolution rasters, as detailed in [Stevens et al. 2015][r3]. Based upon findings from [Gaughan et al. 2016][r4] for a given time point, the distance to nearest BS edge for all proceeding observed timepoints are included. For example, if we were to model the "first draft" population map of 2012 we would include not only the distance to nearest BS edge for 2012 as one of the predictive covariates, but would also include the distance to nearest BS edge corresponding to the 2000 BS extents. Similarly for 2014, we would include the distance to nearest BS edge for 2014, 2012, and 2000. This is done to show that older parts of a BS agglomeration are different from newer ones and, from a practical modelling stance, to avoid the appearance of "doughnuts" where centers of agglomerations appear to have low population densities relative to the preceeding modeled timepoint. With the exception of the 2000 timepoint, the 2012 and the 2014, or any crossectional timepoint should another BS/urban dataset be used in the future, would be replaced by the "final" population model draft which includes the distance to nearest BS/urban edge of all timepoints prior, e.g. for 2012 the layers for 2000, 2001, ...,2011, 2012, in the predictive covariates.

Once these inital population models are created, then the total population spatially coincident with the BS extents are extracted and summed by admin. unit and the corresponding BS population density is derived as well for use in the BSGM predictive phases.




###  Transition Magnitude Estimation
In order to estimate the number of transitions for each time step within the modeling period, we examine the relative predicted BS population changes as well as the predicted relative changes in BS population density for every admin. unit. To do this we take the previously extracted BS population and corresponding BS population density and interpolate for both using an independent exponential growth/decay of the Urban Rural Ratio (URR) for each admin. unit. This exponential interpolation is based upon the [U.N. Urban Population Interpolation Method][r5]. The URR at time _t0_ is defined as:


![Alt text][i3]


Where _BSPOP\_o_ is the BS Population of the admin unit and _POP\_o_ is the total admin. population  at time _t0_. Accordingly, we interpolate URR at some time _t_ as:


![Alt text][i4]


Where _URR\_k_ is the URR at time _k_ which corresponds to the end time point of the model _t1_.


Similarly, BS population density ( _BSD_ ) at time _t0_ is given by the equation:


![Alt text][i5]


Where _BSCNT\_o_ is the number of BS cells in the admin. unit at time _t0_. We then interpolate _BSD_ for a given time _t_ using the following equation:


![Alt text][i6]


Having interpolated the BS Population and corresponding BS population density for each time _t_ we can the interpolate the number of pixels which should transition for each time step, after translating _URR_ into the "Proportion Urban" ( _PU_ ), and based upon the simple calculation of:


![Alt text][i7]


####  Handling of Negative Cases
Given that the above equations can result in "negative" predicted growth in a given year, and the input BS extent data assumes that once an area has transitioned to BS it remains BS, we are faced with a contradiction. Here we must side with the extent data and limit the model to only be able to show "stagnation," i.e. no growth, or growth. We do this in several ways based upon the origin and the implied real world origin of this negative growth or "decay." Case Is have to do with situations where the observed changes are greater than zero and some or all of the estimated changes are negative and Case IIs have to do with situations where the observed changes are zero and the estimated changes are negative.

#####  Case Ia
All predicted differences are negative, but the observed changes are greater than zero.

* Origins: Built population decreases, but built settlement increases either becasue of differences in imagery and sensitivity of original datasets (i.e. artifact) or because the relationship between population and built settlement area are inverse of what would be expected.

* Implication:  Lacking any other information, we will assume that the greatest built settlement changes occurred circa the biggest population magnitude changes

* Handling: The reweighting scheme makes all the weights positive by virtue of all indiviudal differences being negative; no further action is necessary.


#####  Case Ib
Some, but not all, predicted differences are negative and the observed changes are greater than zero.

*Origins: Comes about by the population decreasing for a year while outpacing the predicted decrease in built settlement population density

* Implication: Built settlement growth during this period is unlikely compared to other years in the total transition period.

* Handling: Set the predicted difference for that year, and therefore its weighted difference to zero.

#####  Case II
Predicted differences in at least one year are not zero, but the observed changes are zero.

* Origins: Relationships between population and built settlement counts are not straightforward and not stationary either through time and space. Compounding this, there are inaccuracies in the original built settlement data and even the population estimates. Any of these errors, in conjunction with my model assumptions, could combine to result in this.

* Implications: Best to continue with the base assumption that the input built settlement data is the best we have in knowing if any transition occurred.

* Handling:  Set all differences to zero in order to match the observed changes



###  Transition Distribution Across Time
Given that the number of predicted transitions is not inherently constrained by the observed transitions between time _t0_ and _t1_, on an admin. unit by admin. unit basis, we first reweight the transitions of each time step to match the total number of observed transitions for the given modeling period. This can be written for a given admin unit as:


![Alt text][i8]


Where _w\_t_ sums to 1 and to obtain the weighted transitions we simply multiply the weight of each year by the observed number of transitions. This allows for the model to maintain agreement between multiple time points in the time series of BS extents for which it is interpolating upon and is essentially dasymetrically redistributing the transitions through time based upon the latent temporal information contained by the time step specific changes population totals for each admin. unit.

After the negative values are handled and the estimated changes are weighted by the observed changes, sometimes there remain discrepancies, over or under estimations, between the sum of predicted transitions and the observed changes primarily due to rounding during the weighting procedure. Lacking other information, we decide to obtain agreement betweeen the predicted and observed counts by way of a stochastic process where a random timepoint within the modeling period is sampled and one transition is added or subtracted. This "salting" occurs until there is agreement between the predicted and observed counts for a given admin unit is obtained. In the case where subtraction is being performed to correct for overestimation, subtractions are not done on years which are predicted to have no transitions, i.e. the process will not create negative transitions.



###  Predicted BS Extents Map Production
In order to turn those predicted transitions into timestep specific BS extent maps, i.e. spatially allocate the transitions within each admin. unit, we have two options. We can allocate based solely upon the RF-derived probability transition surface or we can utilize the same RF-derived probability surface in conjunction with the time step specific [admin. unit weighted Lights at Night (LAN)][#markdown-header--lights-at-night-(lan)-processing] data.


####  Admin. Unit-specific RF-derived Probability Surface for Disaggregating Transitions
The procedure using only the RF-derived probability is carried out on an admin.-by-admin. unit basis. Given that the transition process is iterative in nature, we begin by taking the extents of the previous time step, or the extents of _t0_ if it is the first time step. The location(s) where transitions can be allocated in the given admin unit is limited to the locations where transitions were observed to have occurred. For those locations, assumming they haven't already been transitioned in previous steps, we retrieve the probability of transition as stated by the RF-derived surface. We make the assumption that cells with a higher probability of transition are more likely to transition before cells with lower probabilities. We select the _n_ highest probabilities from the subset of potential transition cells, change their value to represent BS in addition to the pre-existing extents, and output the union of the new transitions and preceding BS as the predicted BS extents at that time step. This output is then used as the base BS extents for the next time step's transition procedure until all time steps have been processed.


####  Time Step & Admin. Unit-specific LAN Modified RF-derived Probability Surface for Disaggregating Transitions
The procedure for using time step specific LAN adjusted probabilities of transition is also carried out on an admin. unit by admin. unit basis and largely follows the same procedure given for the [RF-only disaggregation of transitions][#markdown-header--admin-unit-specific-rf-derived-probability-surface-for-disaggregating-transitions]. Where the methods differ is that the base probability of transition for every cell _i_ is adjusted by the weighte LAN data, for time step _t_, of the admin. unit _j_ using the following:


![Alt text][i9]


The procedure after that is identical.




[Return to Overview/Table of Contents][l1]

[r1]: /docs/References.md "World Database of Protected Areas"
[r2]: /docs/References.md ""
[r3]: /docs/References.md ""
[r4]: /docs/References.md ""
[r5]: /docs/References.md ""

[i1]: /Figures/lag_equation.gif ""
[i2]: /Figures/LAN_reweighting.gif ""
[i3]: /Figures/URR_o.gif ""
[i4]: /Figures/URR_t.gif ""
[i5]: /Figures/BSD_o.gif ""
[i6]: /Figures/BSD_t.gif ""
[i7]: /Figures/BSCNT_t.gif ""
[i8]: /Figures/weight_t.gif ""
[i9]: /Figures/reweightingLANtrans.gif ""

[l1]: /README.md