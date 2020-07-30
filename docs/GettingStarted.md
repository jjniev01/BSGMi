#  First Time Set Up
##  Local Setup
You need to have R 3.4.2 or newer on your machine as well as a valid GDAL 1.10.1 install with corresponding Python wrappers for Python 2.7.5. You will need to install the necessary R packages as detailed in the [Dependencies][r1] section. Ensure that you have a system variable defined which points to the `python.exe`.

Simply download the entire BSGM repository and place it within a directory. The path of that directory, e.g. `C:\\Users\\JJNieves\\BSGM\\`, is what you will typically need to assign to the `root` variable in most of the BSGM scripts you call.

For a given model run it will also be necessary to create a folder, within the country specific data folder, entitled _"Pop"_ that you then populate with the population maps created for your interpolation 'anchor years' or the base year for your extrapolation. Additionally, if you anticipate using Lights-at-Night annual weighting in the model, you will need to create another folder in that country-specific data folder entitled _"LAN"_ and fill it with the annual lights-at-night (LAN) data processed separately with the [LAN Preprocessing Script][r2]. For more details on the population maps and LAN data see the [Methods Documentation][r3]. An example of the internal folder structure can be seen in the following two images.


![Alt Text][i1]


![Alt Text][i2]




###  First-time Config Setup
In the config.R scripts, you will need to point ot where your GDAL python wrappers are for a few key functions, much like in the example below.

```
bsgm.gdal_merge_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_merge.py\"",spr="")
bsgm.gdal_calc_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_calc.py\"",spr="")
bsgm.gdal_polygonize_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_polygonize.py\"",spr="")
```

You should only need to change the path preceding the `...\\GDAL\\...` portion of the path, taking care to maintain the back-slashes and double-quote patterns in order to prevent issues when the path is submitted as a system call.

Note: For an HPC set-up you will need to reference where GDAL is installed on the cluster *in addition to* installing the modules within your job submission script (see [HPC Runs][l1] for more details).
##  ArcGIS and GDAL Difficulties
The GDAL and Python install, with corresponding wrappers, which comes with ArcGIS 10.4 and newer has issues with finding the gdal_calculate and other necessary functions. The only solution I have found is to redownload the GDAL install in Python using _pip_ via the command line. The instructions are detailed in [GDAL Install and Difficulties][r4].


##  High Performance Computing SetUp
The setup for running the model on a High Performance Computing (HPC) setup is similar to a local setup however the modules for the HPC will need to be manually built. The [HPC Documentation][r5] gives an overview of the setup on the University of Southampton's IRIDIS computing system which is Linux based and utilizes a PBS job management system. The `.sh` job submission scripts, accessory `.txt` files and the HPC versions of the BSGM script and modules are set up for the IRIDIS environment, but can likely be modified for another Unix based HPC environment without much trouble for a user who is experienced in such systems.



#  Running the Model Locally
##  Overview
All necessary user inputs are made in the [config.R][r6] and the [input.R][r7] scripts. The user required inputs in the config.R script are unlikely to be changed between model runs where as model specific inputs will be carried out in the input.R script.


##  Config.R
This section will go through the most necessary declarations needed from the user, but by no means lists all the options available. See the comments within the scripts for descriptions of all options.
The first option which needs to be declared by the user is the character representation of the system environment variable which points to the `python.exe` file of your Python install.


```bsgm.python_path <- "python"```


You then need to specify the paths to the Python GDAL wrappers necessary for the script, i.e. where they are installed. This only needs to be done one time on a given machine. Typically, the only thing that will change in the below example paths is the directory path where GDAL was installed. Take care to keep the specific use of double quotations as these paths will be used in system calls and changing the very deliberate use of escape characters and quotation will likely mess this up.


```
bsgm.gdal_merge_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_merge.py\"",spr="")
bsgm.gdal_calc_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_calc.py\"",spr="")
bsgm.gdal_polygonize_path <- paste("\"C:\\Program Files (x86)\\GDAL\\gdal_polygonize.py\"",spr="")
```

The user will also need to specify a numeric factor which is used in the calculation of how many blocks to break a raster up into. The value that is appropriate to use is dependent upon the amount of RAM in the machine being used, the number of cores being used, and the size of the country being modeled. This necessitates a bit of experimentation on the part of the user. A decent default to start at is `50` and if any freezing or RAM overflow is encountered than the value should be increased. If no freezing or crashing occurs, but the process seems to be progressing very slowly, i.e. too much over head is being spent on writing and communicating between nodes, than decreasing the value can be experimented with. For a full breakdown of how the number of blocks is calculated, see the [wpUtilities Documentation][r8].


`nmb <- 35`


By default, the code will set the number of cores used to the total number of cores detected on your processor. However, if you are mutlitasking while the model is running you may not want to use all the cores. Simply put, the more cores used, the quicker the parallel computation portions of the script will progress.


```
bsgm.cores = parallel:::detectCores()
bsgm.cluster_workers <- bsgm.cores
```


##  Input.R
This section will go through the most necessary declarations needed from the user, but by no means lists all the options available. See the comments within the scripts for descriptions of all options.

First user defined parameter will be the three-letter ISO code representing the country being modeled.

`bsgm.input.countries <- c("PER")`

The next user defined parameter is a character vector of the names of the covariates stored in the WorldPop FTP which will be used in the calculation of the transition probability surface. Take care to use the time specific covariates corresponding to time timpoint _t0_.


```
bsgm.input.cvr <- list("slope",
                      "topo",
                      "tt50k_2000",
                      "urbpx_prp_1_2012",
                      "urbpx_prp_5_2012",
                      "urbpx_prp_10_2012",
                      "urbpx_prp_15_2012",
                      "ghsl_guf_dst_2012",
                      "wdpa_cat1_dst_2014"
                      )   
```


You will also need to explicitly state the name of the covariate which represents the BS extents at time t0.

`bsgm.t0.extents <- "ghsl_guf_2012"`

You will also need to expressly declare the beginning year of the modeling period, _t0_, and the end year of the modeling period, _t1_.

```
t0 <- 2012
t1 <- 2014
```

You will also need to declare whether or not to use the Lights-at-Night (LAN) weighting for transition prediction.

`bsgm.LAN.weighting <- TRUE`


You will need to declare if you are using a fixed set in your model by declaring TRUE or FALSE. If TRUE, you will need to make sure you place all RF model objects to be used as fixed sets in the apprpriate folder prior to running the model.
`bsgm.fixed.set <- FALSE`
  







[Return to Overview/Table of Contents][l1]

[r1]: /docs/Dependencies.md "Dependencies"
[r2]: /accessories/LANClusterProcessing.R "LAN PRocessing Script"
[r3]: /docs/Methods.md "Methods Documentation"
[r4]: /docs/GDALDocumentation.md "GDAL Documentation"
[r5]: /docs/HPCRuns.md "HPC Documentation"
[r6]: config.R "Configuration Script"
[r7]: input.R "User Input Script"
[r8]: https://github.com/wpgp/wpUtilities/blob/master/R/wpGetBlocksNeed.R "wpGetBlocksNeed.R"


[i1]: /Figures/primaryfolderstructure.gif "Root Folder Structure"
[i2]: /Figures/countryfolderstructure.gif "Country Specific Folder Structure"

[l1]: /README.md ""
[l2]: /docs/HPCRuns.md ""