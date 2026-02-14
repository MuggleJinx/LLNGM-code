# Dataset overview for precipitation data

The data used in the analysis is included in the file precip.data.Rdata. This file contains a modified (see the manuscript for details) subset of the original NARCCAP data, which is published under Creative Commons Attribution 4.0 International license (https://creativecommons.org/licenses/by/4.0/legalcode). The citation for the original dataset is 

Mearns, L.O., et al., 2007, updated 2012. The North American Regional Climate Change Assessment Program dataset, National Center for Atmospheric Research Earth System Grid data portal, Boulder, CO. Data downloaded 2019-08-20. [doi:10.5065/D6RN35ST]

For completeness, we include the file read.precip.data.R which can be used to create precip.data.Rdata from the original data source. This script first downloads the following files:
   
     pr_ECP2_ncep_1979010103.nc
     pr_ECP2_ncep_1981010103.nc
     pr_ECP2_ncep_1986010103.nc
     pr_ECP2_ncep_1991010103.nc
     pr_ECP2_ncep_1996010103.nc
     pr_ECP2_ncep_2001010103.nc

from the homepage

     https://www.earthsystemgrid.org/dataset/narccap.ecp2.ncep.table2.html

As well as the file 

     http://www.narccap.ucar.edu/data/table4/orog_ECP2.nc

The script then extracts the needed data from the downloaded files and saves it into the Rdata file. 
