# Seasonal Monitoring Report

This repository holds all the code associated with the Interagency Ecological Program Seasonal Monitoring reports. Most of the data is acccessable online and harvested regularly via the SQLlite database hosted on the DWR Sharepoint site (get link). 

# smonitr R package:

The [smonitr R package](https://github.com/InteragencyEcologicalProgram/smonitr) is a group of R functions used to standardize the formatting and elements of the plots in the Status and Trends Reports. Files for this package are stored within its own `smonitr` GitHub repository, and installation instructions are provided in its README file.

# Guide to files:

Below is an inventory of all the files in the GitHub repository for the IEP Status and Trends Reports. 

## Main folder
*	[IEP_Status&Trends_DatasetsBySeason_Table.pdf](IEP_Status&Trends_DatasetsBySeason_Table.pdf): Draft Table of data sets to plot in IEP Status and Trends Reports by season
* [basemap.png](basemap.png) – figure showing all the regions of the estuary
- [IEP_Plot_Theme.R](IEP_Plot_Theme.R) – ggplot theme to standardize the look of all the figures
- [IEP_Status&Trends_util.R](IEP_Status&Trends_util.R) – another place with the theme. I don’t know why there are two.

## fall_report
*	IEP_Status&Trends_2017_Fall_Metadata.pdf: Metadata associated with data sets depicted in the IEP Status and Trends Fall 2017 Report
*	IEP_Status&Trends_2017_Fall_Report.pdf: IEP Status and Trends Fall 2017 Report
*	IEP_Status&Trends_2017_Fall_Metadata.pdf: Metadata associated with data sets depicted in the IEP Status and Trends Fall 2017 Report
*	IEP_2017FallStatusTrends_ADA.pdf: ADA=compliant version of IEP Status and Trends Fall 2017 Report
*	IEP_Status&Trends_FallDatasets_2019-03-21.docx: Basic metadata about the web-based location and file types of the fall season data sets
*	IEP_Status&Trends_Fishes_Fall.R: R code for generating plots of Delta smelt, longfin smelt, striped bass, American shad, white sturgeon, and fall-run chinook salmon for the IEP Status and Trends Fall 2017 Report
*	IEP_Status&Trends_Flow.R: R code for generating plots of Delta Outflow Index for the IEP Status and Trends Fall 2017 Report
*	IEP_Status&Trends_WaterQuality.R: R code for generating plots of Secchi depth, dissolved nitrogen, and chlorophyll-a for the IEP Status and Trends Fall 2017 Report
*	IEP_Status&Trends_Zooplankton.R R code for generating plots of zooplankton biomass per unit effort for the IEP Status and Trends Fall 2017 Report
*	run_report.r : Code for generating all the graphs to go into the fall report
*	figures – folder full of the figures that went into the fall report

## winter_report
*	BayStudyFishPlots.R
*	BayStudyPlots_Winter2017.Rmd
*	IEP_Status&Trends_Fishes_DJFMP.R
*	IEP_Status&Trends_Fishes_SKT.R
*	IEP_Status&Trends_Flow.R
*	IEP_Status&Trends_Redbluff.R
*	IEP_Status&Trends_util.R
*	IEP_Status&Trends_WaterQuality.R
*	IEP_Status&Trends_Zooplankton.R
*	IEP_Status&Trends_WinterDatasets_2018-12-06.docx: Basic metadata about the web-based location and file types of the winter season data sets
*	latex – figures and code to knit the report together using LaTeX


## data folder: Source data files
*	1972-2018CBMatrix.xlsx – CDFW Zooplankton Study mesozooplankton data.
*	1972-2018MysidMatrix.xlsx – CDFW Zooplankton Study macrozooplankton data.
*	1972-2018Pump Matrix.xlsx – CDFW Zooplankton Study microzooplankton data.
*	WQ_Discrete_1975-2017.csv: California Department of Water Resources (CDWR), Environmental Monitoring Program (EMP), monthly discrete water quality data, 1975-2017
*	chla_all_seasons.csv: CDWR, EMP, monthly chlorophyll-a data, 1975-2017
*	flow_1929-10-01_2018-09-30.csv: CDWR, Environmental Planning and Information Branch Net, Delta Outflow Index, 1929-2018
*	fmwt.csv: California Department of Fish and Wildlife (CDFW), Fall Midwater Trawl, Indices for Delta Smelt, Striped Bass, Longfin Smelt, and American Shad, 1967-2017
*	frch.csv: CDFW, Fisheries Branch Anadromous Resource Assessment Unit, count estimates for fall-run chinook salmon adult escapement, 1975-2017
*	wq_stations.csv: CDWR, EMP, monthly discrete water quality data station locations
*	wst.csv: CDFW, Sturgeon Study, Trammel Net Survey data for white sturgeon, 1967-2017
*	zoop_cb.csv: CDFW, Zooplankton Study, monthly mesozooplankton catch per unit effort data, 1974-2017
*	zoop_individual_mass.csv: CDFW Zooplankton Study, micro- and mesozooplankton masses for calculation of biomass per unit effort 
*	zoop_mysid.csv: CDFW Zooplankton Study, monthly macrozooplankton catch per unit effort data, 1974-2017
*	zoop_mysid_mass.csv: CDFW Zooplankton Study, monthly macrozooplankton biomass per unit effort data, 1974-2017
*	zoop_pump.csv: CDFW Zooplankton Study, monthly microzooplankton pump catch per unit effort data, 1974-2017
*	zoop_stations.csv: CDFW Zooplankton Study, monthly zooplankton sampling station locations
*	yci_bs.csv – San Francisco Bay Study white sturgeon Year Class Index
*	redbluff_all.csv – passage estimates for juvenile chinook salmon from the Red Bluff Diversion Dam
*	Bay Study_MWT_1980-2018_FishMatrix.xlsx – San Francisco Bay Study fish catch.
