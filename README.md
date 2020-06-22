**Core Method and Species Inventory Calibration Code**

Calibration ensures the defensibility and integrity of data by providing evidence that the data are consistent and trustworthy. This R script evaluates the calibration exercise success of Vegetation Height, Line-point Intercept, Canopy Gap Intercept, & Species Inventory. 

The script requires the following inputs:
1)A csv file with crew and member Core Method annd Species Richness summary information
2)A file location for the csv file
3)An output location for csv and 
  In this format: 
 <img src="https://github.com/cedillon/training_calibration_code/blob/master/input_screenshot.PNG" alt="File Dir." width="500"/>

The script outputs include 3 csv files:
- qualitiative_report, Includes variables as crews and calibration indicators with PASS,FAIL, & NA observations.
<img src="https://github.com/cedillon/training_calibration_code/blob/master/qualitative_report_example.PNG" alt=  width="500"/>
- quantitative_report, Includes variables as crews, indicator, absolute difference, indicator status with numeric and character observations.
![image](https://github.com/cedillon/training_calibration_code/blob/master/quantitative_report_example.PNG)
- calibration_status, Includes variables as crews and observations as the number of indicators passed, failed, and the final status of the calibration status of the exercise.
![image](https://github.com/cedillon/training_calibration_code/blob/master/calibration_status_example.PNG)


More information on calibration can be found in the [Monitoring Manual for Grassland, Shrubland, and Savanna Ecosystems: Volume 1 2nd Edition (Herrick et. al., 2017)](https://www.landscapetoolbox.org/manuals/monitoring-manual/). 

Authors: Catherine Dillon (cdillon@nmsu.edu), Sarah McCord (sarah.mccord@usda.gov), Nelson Stauffer (nelson.stauffer@usda.gov)

