# Constructing a Model to Identify Markets for Rooftop Solar on Multifamily Housing

Reproducible code for master's group project by students in the Bren School at the University of California Santa Barbara. This project included seven criteria to determine favorable markets for rooftop solar PV on multifamily housing: landlord policy favorability, real estate market potential, CO2 abatement potential, electricity generation potential, solar installation internal rate of return, climate risk avoidance, and health costs associated with primary air pollutants. 

A total investment favorability score is calculated based on criteria weights assigned by the user.  Investment favorability scores were investigated for different preferences to demonstrate the robustness and generalizability of the framework. Future studies can utilize the reproducible code to inform decisions on where to invest in solar PV on multifamily housing anywhere in the United States by changing weights within the model depending on preferences.


The files are named in order of which they should be used. 

### 00_demographic_data_wrangling.Rmd

This code used the raw demographic data collected from the Census, HUDuser, and Bureau of Labor Statistics and compiled it into a single file. If studies use this methodology, this can provide structural requirements of data for following Rmd. In addition, an initial list of areas can be output for further analysis.

Once demographic data is collected, city information can be put into NREL's REopt tool to obtain the remaining criteria data. Instructions of the REopt inputs are included to follow. The city, residential utility rate, and net energy metering capacity are the site specific inputs required for REopt.

### 01_metric_normalization.Rmd
This code calculated the real estate and landlord metrics, then used a normalization function to normalize all the metric data. The original code used for the analysis is included in this file, which retrieved data files from google sheets. 

### 02_weight_scores.Rmd
First, the real estate and landlord metrics are weighted, as these criteria consist of multiple metrics. The weights for these criteria can be changed as needed. In all cases, weights sum to 1 to ensure consistent methodology. 

### 03_input_weights.Rmd
This code was created to input weights for all criteria, not dependent on the project's data.

