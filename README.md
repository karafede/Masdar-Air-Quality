# Masdar-Air-Quality
## An ensemble of R scripts to process air quality data organized in a database format. The proposed scripts are intented to carry out summary statistics, daily averages and, 8-hour averages according to the Air Quality directives. In this repository I also show how to setup interactive time-series to be embedded into an air quality website. This work is ideal to prepare air quality reports at institutional level.

See below for more information about the files contained in this repository:

Information about the location and number of monitoring stations are contained in these .csv files Stations_EAD_info_2.csv, Stations_DM_info_2.csv, Stations_NCMS_info_2.csv and excel files Stations_EAD_2.xlsx, Stations_DM_2.xlsx, Stations_NCMS_2.xlsx that also contains pollutant concentration units.
Use the <strong> RunTheGui_1.R </strong> User Interface (UI) script to convert all data from the .xlsx spreadsheets into .csv (Data Base format (DB)). The RunTheGui_1.R script use the <strong> .First.R </strong> script to process all data (conversion from xlsx to .cvs files – DB format).

To build interactive time-series choose among the 3 scripts  <strong> get_data_EAD.R  </strong>,  <strong> get_data_DM.R  </strong>,  <strong> get_data_NCMS.R </strong> to generate time series from the .csv files (DB format) obtained with the  <strong> .First.R  </strong> GUI.
  
R Markdown scripts, such as  <strong> EAD_SO2.RMD  </strong>, are created for each variable. These scripts can be opened in R but they need to run under an R project (read something about it on the web). Once one (any) of the .RMD is open in the R project environment, then you can click on the button “Build Website”. This will start generating .html files containing time-series of each variables (pollutant or met parameter) for all the monitoring sites.

!!! Before building the website with R Markdown, be sure to delete all the cache files (and the site_libs) in the folder. 
 <strong> The files site.yml and navbar.yml must be checked and update (if necessary) before building the website  </strong>

Other scripts reported in this repository include:
1)	Data capture for PM2.5:  <strong> Data_capture_PM25.R </strong>
2)	Conversion of hourly data into daily (24h averaged) data:  <strong> hourly2daily_script.R </strong>
3)	Ozone and Carbon monoxide 8h averages:  <strong> ozone_daily_8hour.R </strong>,  <strong> CO_daily_8hour.R  </strong>
4)	Outliers are identified and time-series are rebuilt without outliers using these two scripts:  <strong> box_plot_all.R  </strong> (for hourly data) and  <strong> box_plot_all_daily.R </strong> for 24h data. Time-series without outliers are also part of the input data of for the R Markdown files (i.e. get_data_DM.R) needed to generate 2 layers of time-series (with and without outliers).
5)	Finally  <strong> stats_FK.R </strong> generates different kind of boxplot to generate statistical figures for all pollutants.

Please hit me up should you need further clarifications

