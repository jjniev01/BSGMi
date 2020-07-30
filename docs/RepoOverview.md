#  Repository Overview

The source repository is divided into four folders:

* _accesories_ - this is where scripts used for preprocessing of data and post modeling activities (e.g. validation) are located

* _docs_ - location of the markdown scripts which help document the code base

* _hpc_ - this is where high-performance computing versions of the modeling and accessory scripts, and their supporting files, are located. Bash files for submission to a PBS-based Unix computing cluster are included. Some scripts included versions for submitting hpc jobs as job arrays where command line arguments are passed through a text based argument file.

* _modules_ - this is where a series of scripts which contain the primary functions of the BSGM are stored and sourced by the main script.

Alongside these folders are the primary interpolation and extrapolation BSGM scripts and their corresponding input.R and config.R scripts. Additionally is the primary README.md file.


[Return to Overview/Table of Contents][l1]

[l1]: /README.md