##########################################################################################
##########################################################################################
###                                                                                    ###
### FOLLOW THESE STEPS                                                                 ###
### Just Another Service Provided by                                                   ###
### Your Friendly Neighborhood MV                                                      ###
###                                                                                    ###
##########################################################################################
##########################################################################################

0. Watch out for instructions within the scripts

1. Transform hourly Data files from '.xlsx' format to '.csv' database format
--> Run "RunTheGui_1.RData"

2. Filter outliers from hourly database files
--> Run "box_plot_all.R"

3. Transform hourly database files to daily 
--> Run "hourly2daily_script.R"

4. Filter outliers from daily database files
--> Run "box_plot_all_daily.R"

5. Extrac ozone data from hourly database and transfrom it to 8h and daily
--> Run "ozone_daily_8hour.R"

6. Extrac carbon monoxide data from hourly database and transfrom it to 8h and daily
--> Run "CO_daily_8hour.R"

7. Generate Interactive time series either hourly or daily
--> Open respective folder
	--> Run "Interactive_plots_R.Rproj"
	--> In the upper right window select "Build" tab
	--> Select "Build website"

8. Generate Air Quality Index Statistics
--> Run "AQI_UAE_new.R"

9. Generate Summary plots for AQI
--> Run "AQI_short_UAE_new.R"