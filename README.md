**Core Method and Species Inventory Calibration Code**

Calibration ensures the defensibility and integrity of data by providing evidence that the data are consistent and trustworthy. This R script evaluates the calibration exercise success of Vegetation Height, Line-point Intercept, Canopy Gap Intercept, & Species Inventory. 

The script requires the following inputs:
1) A csv file with crew and member Core Method annd Species Richness summary information
2) A file location for the csv file on line 26. Example: input_path <- "C:/calibration_data/data/my_data.csv" 
3) An output location for csv and figure outputs on line 29. Example: output_path <- "C:/calibration_data/outputs"

The script outputs include 2 csv files and 12 figures:
- qualitiative_report, Includes variables as crews and calibration indicators with PASS,FAIL, & NA observations. 
- The final observation row includes the calibration exercise status with 'Failed Calibration' or 'Passes Calibration' entries.
<img src="https://github.com/cedillon/training_calibration_code/blob/master/qualitative_report_example.PNG" alt= "Qual." width="500"/>

- quantitative_report, Includes variables as crews, indicator, absolute difference, indicator status with numeric and character observations.
<img src="https://github.com/cedillon/training_calibration_code/blob/master/quantitative_report_example.PNG" alt= "Quant." width="500"/>

-figures, three per each of the method will include:
  1) One histogram with indicator success per method, a sum of the number of calibrated indicators across crews
  2) One histogram with the number of crews who passed each method versus failed
  3) One figure displaying the calibration absolute difference criteria per method indicator, e.g. 10%, and indicators as above (failed)
     or below (passed) that horizontal line
<img src="https://github.com/cedillon/training_calibration_code/blob/master/quantitative_report_example.PNG" alt= "Quant." width="500"/>
  
  

More information on calibration can be found in the [Monitoring Manual for Grassland, Shrubland, and Savanna Ecosystems: Volume 1 2nd Edition (Herrick et. al., 2017)](https://www.landscapetoolbox.org/manuals/monitoring-manual/). 

Authors: Catherine Dillon (cdillon@nmsu.edu), Sarah McCord (sarah.mccord@usda.gov), Nelson Stauffer (nelson.stauffer@usda.gov)

