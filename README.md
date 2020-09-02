**Core Method and Species Inventory Calibration Code**

Calibration ensures the defensibility and integrity of data by providing evidence that the data are consistent and trustworthy. This R script evaluates the calibration exercise success of Vegetation Height, Line-point Intercept, Canopy Gap Intercept, & Species Inventory. 

The script requires the following inputs:
1) A csv file with crew and member Core Method annd Species Richness summary information
2) A file location for the csv file on line 26. Example: input_path <- "C:/calibration_data/data/my_data.csv" 
3) An output location for csv and figure outputs on line 29. Example: output_path <- "C:/calibration_data/outputs"

The script outputs include 2 csv files:
- qualitiative_report, Includes variables as crews and calibration indicators with PASS,FAIL, & NA observations.
<img src="https://github.com/cedillon/training_calibration_code/blob/master/qualitative_report_example.PNG" alt= "Qual." width="500"/>

- quantitative_report, Includes variables as crews, indicator, absolute difference, indicator status with numeric and character observations.
<img src="https://github.com/cedillon/training_calibration_code/blob/master/quantitative_report_example.PNG" alt= "Quant." width="500"/>

More information on calibration can be found in the [Monitoring Manual for Grassland, Shrubland, and Savanna Ecosystems: Volume 1 2nd Edition (Herrick et. al., 2017)](https://www.landscapetoolbox.org/manuals/monitoring-manual/). 

Authors: Catherine Dillon (cdillon@nmsu.edu), Sarah McCord (sarah.mccord@usda.gov), Nelson Stauffer (nelson.stauffer@usda.gov)

