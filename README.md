Principal Component and Clustering Analysis of Tumor Characteristics
Otajon Yuldashev
2025-01-09
The Breast Cancer Wisconsin (Diagnostic) Dataset is a well-known dataset used for binary classification tasks related to breast cancer diagnosis. The dataset contains features computed from breast tissue samples collected via Fine Needle Aspiration (FNA). The goal is to distinguish between benign (non-cancerous) and malignant (cancerous) tumors based on various measurements of cell nuclei.

General Information
Dataset Name: Breast Cancer Wisconsin (Diagnostic)
Source: UCI Machine Learning Repository
Number of Instances (Rows): 569 samples
Number of Features (Columns): 32 total columns:
1 target variable (diagnosis – Benign or Malignant)
30 numerical features related to tumor characteristics.
1 identifier column (patient ID)
Load Necessary Libraries
library(tidyverse)
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(factoextra)
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
library(GGally)
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
library(corrplot)
## corrplot 0.95 loaded
Load the Dataset
The dataset is loaded from the specified file path:

data <- read.csv("C:\\Users\\user\\OneDrive\\Desktop\\pca project final\\data.csv")
View Structure, Summary, and Missing Values
str(data)
## 'data.frame':    569 obs. of  33 variables:
##  $ id                     : int  842302 842517 84300903 84348301 84358402 843786 844359 84458202 844981 84501001 ...
##  $ diagnosis              : chr  "M" "M" "M" "M" ...
##  $ radius_mean            : num  18 20.6 19.7 11.4 20.3 ...
##  $ texture_mean           : num  10.4 17.8 21.2 20.4 14.3 ...
##  $ perimeter_mean         : num  122.8 132.9 130 77.6 135.1 ...
##  $ area_mean              : num  1001 1326 1203 386 1297 ...
##  $ smoothness_mean        : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
##  $ compactness_mean       : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
##  $ concavity_mean         : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
##  $ concave.points_mean    : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
##  $ symmetry_mean          : num  0.242 0.181 0.207 0.26 0.181 ...
##  $ fractal_dimension_mean : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
##  $ radius_se              : num  1.095 0.543 0.746 0.496 0.757 ...
##  $ texture_se             : num  0.905 0.734 0.787 1.156 0.781 ...
##  $ perimeter_se           : num  8.59 3.4 4.58 3.44 5.44 ...
##  $ area_se                : num  153.4 74.1 94 27.2 94.4 ...
##  $ smoothness_se          : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
##  $ compactness_se         : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
##  $ concavity_se           : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
##  $ concave.points_se      : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
##  $ symmetry_se            : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
##  $ fractal_dimension_se   : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
##  $ radius_worst           : num  25.4 25 23.6 14.9 22.5 ...
##  $ texture_worst          : num  17.3 23.4 25.5 26.5 16.7 ...
##  $ perimeter_worst        : num  184.6 158.8 152.5 98.9 152.2 ...
##  $ area_worst             : num  2019 1956 1709 568 1575 ...
##  $ smoothness_worst       : num  0.162 0.124 0.144 0.21 0.137 ...
##  $ compactness_worst      : num  0.666 0.187 0.424 0.866 0.205 ...
##  $ concavity_worst        : num  0.712 0.242 0.45 0.687 0.4 ...
##  $ concave.points_worst   : num  0.265 0.186 0.243 0.258 0.163 ...
##  $ symmetry_worst         : num  0.46 0.275 0.361 0.664 0.236 ...
##  $ fractal_dimension_worst: num  0.1189 0.089 0.0876 0.173 0.0768 ...
##  $ X                      : logi  NA NA NA NA NA NA ...
summary(data)
##        id             diagnosis          radius_mean      texture_mean  
##  Min.   :     8670   Length:569         Min.   : 6.981   Min.   : 9.71  
##  1st Qu.:   869218   Class :character   1st Qu.:11.700   1st Qu.:16.17  
##  Median :   906024   Mode  :character   Median :13.370   Median :18.84  
##  Mean   : 30371831                      Mean   :14.127   Mean   :19.29  
##  3rd Qu.:  8813129                      3rd Qu.:15.780   3rd Qu.:21.80  
##  Max.   :911320502                      Max.   :28.110   Max.   :39.28  
##  perimeter_mean     area_mean      smoothness_mean   compactness_mean 
##  Min.   : 43.79   Min.   : 143.5   Min.   :0.05263   Min.   :0.01938  
##  1st Qu.: 75.17   1st Qu.: 420.3   1st Qu.:0.08637   1st Qu.:0.06492  
##  Median : 86.24   Median : 551.1   Median :0.09587   Median :0.09263  
##  Mean   : 91.97   Mean   : 654.9   Mean   :0.09636   Mean   :0.10434  
##  3rd Qu.:104.10   3rd Qu.: 782.7   3rd Qu.:0.10530   3rd Qu.:0.13040  
##  Max.   :188.50   Max.   :2501.0   Max.   :0.16340   Max.   :0.34540  
##  concavity_mean    concave.points_mean symmetry_mean    fractal_dimension_mean
##  Min.   :0.00000   Min.   :0.00000     Min.   :0.1060   Min.   :0.04996       
##  1st Qu.:0.02956   1st Qu.:0.02031     1st Qu.:0.1619   1st Qu.:0.05770       
##  Median :0.06154   Median :0.03350     Median :0.1792   Median :0.06154       
##  Mean   :0.08880   Mean   :0.04892     Mean   :0.1812   Mean   :0.06280       
##  3rd Qu.:0.13070   3rd Qu.:0.07400     3rd Qu.:0.1957   3rd Qu.:0.06612       
##  Max.   :0.42680   Max.   :0.20120     Max.   :0.3040   Max.   :0.09744       
##    radius_se        texture_se      perimeter_se       area_se       
##  Min.   :0.1115   Min.   :0.3602   Min.   : 0.757   Min.   :  6.802  
##  1st Qu.:0.2324   1st Qu.:0.8339   1st Qu.: 1.606   1st Qu.: 17.850  
##  Median :0.3242   Median :1.1080   Median : 2.287   Median : 24.530  
##  Mean   :0.4052   Mean   :1.2169   Mean   : 2.866   Mean   : 40.337  
##  3rd Qu.:0.4789   3rd Qu.:1.4740   3rd Qu.: 3.357   3rd Qu.: 45.190  
##  Max.   :2.8730   Max.   :4.8850   Max.   :21.980   Max.   :542.200  
##  smoothness_se      compactness_se      concavity_se     concave.points_se 
##  Min.   :0.001713   Min.   :0.002252   Min.   :0.00000   Min.   :0.000000  
##  1st Qu.:0.005169   1st Qu.:0.013080   1st Qu.:0.01509   1st Qu.:0.007638  
##  Median :0.006380   Median :0.020450   Median :0.02589   Median :0.010930  
##  Mean   :0.007041   Mean   :0.025478   Mean   :0.03189   Mean   :0.011796  
##  3rd Qu.:0.008146   3rd Qu.:0.032450   3rd Qu.:0.04205   3rd Qu.:0.014710  
##  Max.   :0.031130   Max.   :0.135400   Max.   :0.39600   Max.   :0.052790  
##   symmetry_se       fractal_dimension_se  radius_worst   texture_worst  
##  Min.   :0.007882   Min.   :0.0008948    Min.   : 7.93   Min.   :12.02  
##  1st Qu.:0.015160   1st Qu.:0.0022480    1st Qu.:13.01   1st Qu.:21.08  
##  Median :0.018730   Median :0.0031870    Median :14.97   Median :25.41  
##  Mean   :0.020542   Mean   :0.0037949    Mean   :16.27   Mean   :25.68  
##  3rd Qu.:0.023480   3rd Qu.:0.0045580    3rd Qu.:18.79   3rd Qu.:29.72  
##  Max.   :0.078950   Max.   :0.0298400    Max.   :36.04   Max.   :49.54  
##  perimeter_worst    area_worst     smoothness_worst  compactness_worst
##  Min.   : 50.41   Min.   : 185.2   Min.   :0.07117   Min.   :0.02729  
##  1st Qu.: 84.11   1st Qu.: 515.3   1st Qu.:0.11660   1st Qu.:0.14720  
##  Median : 97.66   Median : 686.5   Median :0.13130   Median :0.21190  
##  Mean   :107.26   Mean   : 880.6   Mean   :0.13237   Mean   :0.25427  
##  3rd Qu.:125.40   3rd Qu.:1084.0   3rd Qu.:0.14600   3rd Qu.:0.33910  
##  Max.   :251.20   Max.   :4254.0   Max.   :0.22260   Max.   :1.05800  
##  concavity_worst  concave.points_worst symmetry_worst   fractal_dimension_worst
##  Min.   :0.0000   Min.   :0.00000      Min.   :0.1565   Min.   :0.05504        
##  1st Qu.:0.1145   1st Qu.:0.06493      1st Qu.:0.2504   1st Qu.:0.07146        
##  Median :0.2267   Median :0.09993      Median :0.2822   Median :0.08004        
##  Mean   :0.2722   Mean   :0.11461      Mean   :0.2901   Mean   :0.08395        
##  3rd Qu.:0.3829   3rd Qu.:0.16140      3rd Qu.:0.3179   3rd Qu.:0.09208        
##  Max.   :1.2520   Max.   :0.29100      Max.   :0.6638   Max.   :0.20750        
##     X          
##  Mode:logical  
##  NA's:569      
##                
##                
##                
## 
sum(is.na(data))  
## [1] 569
Observations: The dataset contains irrelevant columns such as patient IDs (first column) and a column full of missing values (last column).

Data Cleaning
Step 1: Remove Irrelevant Columns
The first column (patient IDs) and the last column (containing only NA values) are removed:

data <- data[, -c(1, ncol(data))]
Verify the structure:

str(data)
## 'data.frame':    569 obs. of  31 variables:
##  $ diagnosis              : chr  "M" "M" "M" "M" ...
##  $ radius_mean            : num  18 20.6 19.7 11.4 20.3 ...
##  $ texture_mean           : num  10.4 17.8 21.2 20.4 14.3 ...
##  $ perimeter_mean         : num  122.8 132.9 130 77.6 135.1 ...
##  $ area_mean              : num  1001 1326 1203 386 1297 ...
##  $ smoothness_mean        : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
##  $ compactness_mean       : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
##  $ concavity_mean         : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
##  $ concave.points_mean    : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
##  $ symmetry_mean          : num  0.242 0.181 0.207 0.26 0.181 ...
##  $ fractal_dimension_mean : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
##  $ radius_se              : num  1.095 0.543 0.746 0.496 0.757 ...
##  $ texture_se             : num  0.905 0.734 0.787 1.156 0.781 ...
##  $ perimeter_se           : num  8.59 3.4 4.58 3.44 5.44 ...
##  $ area_se                : num  153.4 74.1 94 27.2 94.4 ...
##  $ smoothness_se          : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
##  $ compactness_se         : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
##  $ concavity_se           : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
##  $ concave.points_se      : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
##  $ symmetry_se            : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
##  $ fractal_dimension_se   : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
##  $ radius_worst           : num  25.4 25 23.6 14.9 22.5 ...
##  $ texture_worst          : num  17.3 23.4 25.5 26.5 16.7 ...
##  $ perimeter_worst        : num  184.6 158.8 152.5 98.9 152.2 ...
##  $ area_worst             : num  2019 1956 1709 568 1575 ...
##  $ smoothness_worst       : num  0.162 0.124 0.144 0.21 0.137 ...
##  $ compactness_worst      : num  0.666 0.187 0.424 0.866 0.205 ...
##  $ concavity_worst        : num  0.712 0.242 0.45 0.687 0.4 ...
##  $ concave.points_worst   : num  0.265 0.186 0.243 0.258 0.163 ...
##  $ symmetry_worst         : num  0.46 0.275 0.361 0.664 0.236 ...
##  $ fractal_dimension_worst: num  0.1189 0.089 0.0876 0.173 0.0768 ...
head(data)
##   diagnosis radius_mean texture_mean perimeter_mean area_mean smoothness_mean
## 1         M       17.99        10.38         122.80    1001.0         0.11840
## 2         M       20.57        17.77         132.90    1326.0         0.08474
## 3         M       19.69        21.25         130.00    1203.0         0.10960
## 4         M       11.42        20.38          77.58     386.1         0.14250
## 5         M       20.29        14.34         135.10    1297.0         0.10030
## 6         M       12.45        15.70          82.57     477.1         0.12780
##   compactness_mean concavity_mean concave.points_mean symmetry_mean
## 1          0.27760         0.3001             0.14710        0.2419
## 2          0.07864         0.0869             0.07017        0.1812
## 3          0.15990         0.1974             0.12790        0.2069
## 4          0.28390         0.2414             0.10520        0.2597
## 5          0.13280         0.1980             0.10430        0.1809
## 6          0.17000         0.1578             0.08089        0.2087
##   fractal_dimension_mean radius_se texture_se perimeter_se area_se
## 1                0.07871    1.0950     0.9053        8.589  153.40
## 2                0.05667    0.5435     0.7339        3.398   74.08
## 3                0.05999    0.7456     0.7869        4.585   94.03
## 4                0.09744    0.4956     1.1560        3.445   27.23
## 5                0.05883    0.7572     0.7813        5.438   94.44
## 6                0.07613    0.3345     0.8902        2.217   27.19
##   smoothness_se compactness_se concavity_se concave.points_se symmetry_se
## 1      0.006399        0.04904      0.05373           0.01587     0.03003
## 2      0.005225        0.01308      0.01860           0.01340     0.01389
## 3      0.006150        0.04006      0.03832           0.02058     0.02250
## 4      0.009110        0.07458      0.05661           0.01867     0.05963
## 5      0.011490        0.02461      0.05688           0.01885     0.01756
## 6      0.007510        0.03345      0.03672           0.01137     0.02165
##   fractal_dimension_se radius_worst texture_worst perimeter_worst area_worst
## 1             0.006193        25.38         17.33          184.60     2019.0
## 2             0.003532        24.99         23.41          158.80     1956.0
## 3             0.004571        23.57         25.53          152.50     1709.0
## 4             0.009208        14.91         26.50           98.87      567.7
## 5             0.005115        22.54         16.67          152.20     1575.0
## 6             0.005082        15.47         23.75          103.40      741.6
##   smoothness_worst compactness_worst concavity_worst concave.points_worst
## 1           0.1622            0.6656          0.7119               0.2654
## 2           0.1238            0.1866          0.2416               0.1860
## 3           0.1444            0.4245          0.4504               0.2430
## 4           0.2098            0.8663          0.6869               0.2575
## 5           0.1374            0.2050          0.4000               0.1625
## 6           0.1791            0.5249          0.5355               0.1741
##   symmetry_worst fractal_dimension_worst
## 1         0.4601                 0.11890
## 2         0.2750                 0.08902
## 3         0.3613                 0.08758
## 4         0.6638                 0.17300
## 5         0.2364                 0.07678
## 6         0.3985                 0.12440
Handling Zero Values
While observing the middle and bottom sides of the dataset I came across some rows with 0 values. So in order to make our results more reliable , I’m gonna remove all rows with 0 values.So lets first check how many rows we have in total , and then remove the rows with 0 values. Check how many rows contain zeros:

# Step 1: Remove rows with zero values across all columns
filtered_data <- data %>% filter(if_all(everything(), ~ . != 0))

# Step 2: Select only the numerical columns after filtering
numeric_data <- filtered_data %>% select(where(is.numeric))

# Verify the structure of the cleaned numeric data
str(numeric_data)
## 'data.frame':    556 obs. of  30 variables:
##  $ radius_mean            : num  18 20.6 19.7 11.4 20.3 ...
##  $ texture_mean           : num  10.4 17.8 21.2 20.4 14.3 ...
##  $ perimeter_mean         : num  122.8 132.9 130 77.6 135.1 ...
##  $ area_mean              : num  1001 1326 1203 386 1297 ...
##  $ smoothness_mean        : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
##  $ compactness_mean       : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
##  $ concavity_mean         : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
##  $ concave.points_mean    : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
##  $ symmetry_mean          : num  0.242 0.181 0.207 0.26 0.181 ...
##  $ fractal_dimension_mean : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
##  $ radius_se              : num  1.095 0.543 0.746 0.496 0.757 ...
##  $ texture_se             : num  0.905 0.734 0.787 1.156 0.781 ...
##  $ perimeter_se           : num  8.59 3.4 4.58 3.44 5.44 ...
##  $ area_se                : num  153.4 74.1 94 27.2 94.4 ...
##  $ smoothness_se          : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
##  $ compactness_se         : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
##  $ concavity_se           : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
##  $ concave.points_se      : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
##  $ symmetry_se            : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
##  $ fractal_dimension_se   : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
##  $ radius_worst           : num  25.4 25 23.6 14.9 22.5 ...
##  $ texture_worst          : num  17.3 23.4 25.5 26.5 16.7 ...
##  $ perimeter_worst        : num  184.6 158.8 152.5 98.9 152.2 ...
##  $ area_worst             : num  2019 1956 1709 568 1575 ...
##  $ smoothness_worst       : num  0.162 0.124 0.144 0.21 0.137 ...
##  $ compactness_worst      : num  0.666 0.187 0.424 0.866 0.205 ...
##  $ concavity_worst        : num  0.712 0.242 0.45 0.687 0.4 ...
##  $ concave.points_worst   : num  0.265 0.186 0.243 0.258 0.163 ...
##  $ symmetry_worst         : num  0.46 0.275 0.361 0.664 0.236 ...
##  $ fractal_dimension_worst: num  0.1189 0.089 0.0876 0.173 0.0768 ...
print(paste("Number of rows after removing rows with 0:", nrow(numeric_data)))
## [1] "Number of rows after removing rows with 0: 556"
Descriptive Statistics
Summary of Numeric Data
summary(numeric_data)
##   radius_mean      texture_mean   perimeter_mean     area_mean     
##  Min.   : 7.691   Min.   : 9.71   Min.   : 48.34   Min.   : 170.4  
##  1st Qu.:11.760   1st Qu.:16.18   1st Qu.: 75.84   1st Qu.: 427.8  
##  Median :13.455   Median :18.86   Median : 87.09   Median : 557.6  
##  Mean   :14.238   Mean   :19.26   Mean   : 92.74   Mean   : 663.7  
##  3rd Qu.:16.040   3rd Qu.:21.73   3rd Qu.:105.25   3rd Qu.: 798.0  
##  Max.   :28.110   Max.   :39.28   Max.   :188.50   Max.   :2501.0  
##  smoothness_mean   compactness_mean  concavity_mean     concave.points_mean
##  Min.   :0.06251   Min.   :0.01938   Min.   :0.000692   Min.   :0.001852   
##  1st Qu.:0.08667   1st Qu.:0.06661   1st Qu.:0.030880   1st Qu.:0.020895   
##  Median :0.09603   Median :0.09509   Median :0.064905   Median :0.034840   
##  Mean   :0.09662   Mean   :0.10568   Mean   :0.090876   Mean   :0.050063   
##  3rd Qu.:0.10540   3rd Qu.:0.13060   3rd Qu.:0.132325   3rd Qu.:0.074843   
##  Max.   :0.16340   Max.   :0.34540   Max.   :0.426800   Max.   :0.201200   
##  symmetry_mean    fractal_dimension_mean   radius_se        texture_se    
##  Min.   :0.1167   Min.   :0.04996        Min.   :0.1115   Min.   :0.3602  
##  1st Qu.:0.1619   1st Qu.:0.05767        1st Qu.:0.2324   1st Qu.:0.8307  
##  Median :0.1792   Median :0.06152        Median :0.3217   Median :1.0880  
##  Mean   :0.1813   Mean   :0.06275        Mean   :0.4064   Mean   :1.1929  
##  3rd Qu.:0.1958   3rd Qu.:0.06609        3rd Qu.:0.4827   3rd Qu.:1.4652  
##  Max.   :0.3040   Max.   :0.09744        Max.   :2.8730   Max.   :3.5680  
##   perimeter_se       area_se        smoothness_se      compactness_se    
##  Min.   : 0.757   Min.   :  6.802   Min.   :0.002667   Min.   :0.002252  
##  1st Qu.: 1.605   1st Qu.: 17.858   1st Qu.:0.005124   1st Qu.:0.013688  
##  Median : 2.296   Median : 24.700   Median :0.006302   Median :0.020740  
##  Mean   : 2.880   Mean   : 40.795   Mean   :0.006975   Mean   :0.025842  
##  3rd Qu.: 3.388   3rd Qu.: 45.440   3rd Qu.:0.008076   3rd Qu.:0.032587  
##  Max.   :21.980   Max.   :542.200   Max.   :0.031130   Max.   :0.135400  
##   concavity_se      concave.points_se   symmetry_se       fractal_dimension_se
##  Min.   :0.000692   Min.   :0.001852   Min.   :0.007882   Min.   :0.0008948   
##  1st Qu.:0.015620   1st Qu.:0.007997   1st Qu.:0.015008   1st Qu.:0.0022495   
##  Median :0.026245   Median :0.011100   Median :0.018685   Median :0.0031590   
##  Mean   :0.032639   Mean   :0.012072   Mean   :0.020314   Mean   :0.0037990   
##  3rd Qu.:0.042563   3rd Qu.:0.014932   3rd Qu.:0.022933   3rd Qu.:0.0045585   
##  Max.   :0.396000   Max.   :0.052790   Max.   :0.078950   Max.   :0.0298400   
##   radius_worst    texture_worst   perimeter_worst    area_worst    
##  Min.   : 8.678   Min.   :12.02   Min.   : 54.49   Min.   : 223.6  
##  1st Qu.:13.085   1st Qu.:21.16   1st Qu.: 84.57   1st Qu.: 521.5  
##  Median :15.040   Median :25.45   Median : 98.32   Median : 696.0  
##  Mean   :16.408   Mean   :25.68   Mean   :108.24   Mean   : 893.4  
##  3rd Qu.:19.098   3rd Qu.:29.55   3rd Qu.:126.75   3rd Qu.:1106.8  
##  Max.   :36.040   Max.   :49.54   Max.   :251.20   Max.   :4254.0  
##  smoothness_worst  compactness_worst concavity_worst    concave.points_worst
##  Min.   :0.08125   Min.   :0.03432   Min.   :0.001845   Min.   :0.008772    
##  1st Qu.:0.11718   1st Qu.:0.15118   1st Qu.:0.121800   1st Qu.:0.065712    
##  Median :0.13155   Median :0.21700   Median :0.231400   Median :0.101700    
##  Mean   :0.13282   Mean   :0.25847   Mean   :0.278553   Mean   :0.117286    
##  3rd Qu.:0.14633   3rd Qu.:0.34160   3rd Qu.:0.386200   3rd Qu.:0.163150    
##  Max.   :0.22260   Max.   :1.05800   Max.   :1.252000   Max.   :0.291000    
##  symmetry_worst   fractal_dimension_worst
##  Min.   :0.1565   Min.   :0.05504        
##  1st Qu.:0.2509   1st Qu.:0.07187        
##  Median :0.2824   Median :0.08007        
##  Mean   :0.2908   Mean   :0.08414        
##  3rd Qu.:0.3189   3rd Qu.:0.09209        
##  Max.   :0.6638   Max.   :0.20750
Summary of Numeric Data
The dataset contains different numerical features that describe tumor characteristics. Below is an overview of what the data tells us:

1. Size-Related Features:
radius_mean (Average size of the tumor’s radius):
Values range from 7.69 to 28.11, with an average of 14.24.
The median is 13.45, which is close to the mean, showing the distribution isn’t heavily skewed, but there are some bigger tumors.
perimeter_mean and area_mean (Perimeter and area of tumors):
Perimeter ranges from 48.34 to 188.50.
Area ranges from 170.4 to 2501.0—this is a huge range, meaning some tumors are much larger than the others, which could indicate outliers.
2. Shape-Related Features:
Features like smoothness_mean, compactness_mean, concavity_mean, and concave.points_mean describe the tumor’s shape:
smoothness_mean (how smooth the tumor surface is) has small values ranging from 0.05 to 0.30, meaning there isn’t a lot of difference between tumors in this aspect.
concavity_mean (how concave the tumor’s border is) shows more variability, meaning some tumors have more irregular shapes.
3. Texture-Related Features:
texture_mean (Average texture of the tumor):
Values range from 9.71 to 39.28 with an average of 19.26.
The values seem fairly balanced since the mean and median are similar.
4. Complexity of Tumor Border:
fractal_dimension_mean (a measure of how complex the tumor’s border is):
Values are very small (between 0.04 and 0.30), meaning the complexity is generally consistent across tumors.
5. Overall Spread of the Data:
Most features have a large range (difference between the minimum and maximum values), especially for size-related features like radius_mean, perimeter_mean, and area_mean.
This shows that tumors can differ a lot in size and shape.
6. Outliers:
Some variables, like area_mean, have very large maximum values, which could mean there are outliers—these could be unusually large tumors.
Conclusion:
We observe that tumors can vary a lot in size, shape, and texture. The variables related to size, like area_mean and perimeter_mean, have big ranges, while shape-related variables like smoothness_mean don’t vary as much. There may also be some outliers that we should keep in mind for further analysis. This makes it a good dataset for techniques like PCA and clustering to understand patterns in the tumor characteristics.

Visualize Distributions with Boxplots
This boxplot shows the distribution of values for each variable before scaling:

numeric_data %>%
  gather(variable, value) %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Boxplot of Variables Before Scaling")


Description of the Boxplot:
Variable Range Differences:
Some variables, such as area_mean, area_se, and area_worst, have significantly higher values (ranging up to 4000).
Other variables, such as fractal_dimension_mean, concavity_mean, and symmetry_se, have values closer to 0 or below 1.
Outliers:
There are noticeable outliers, particularly in area_mean and related variables, representing samples with extreme values for the respective feature.
Scale Imbalance:
The wide range of scales across different variables indicates that certain features (e.g., tumor size-related metrics) will dominate the analysis unless the data is standardized.
Overview for PCA:
PCA requires that all variables be on a similar scale to ensure fair comparison and contribution to the components.
Variables with larger scales can heavily influence the direction of the principal components, leading to skewed results.
After scaling, the variables will have a mean of 0 and a standard deviation of 1, equalizing their contributions to the PCA.
Data Standardization
Standardize the data:

scaled_data <- scale(numeric_data)
scaled_data <- as.data.frame(scaled_data)
Correlation Heatmap
The heatmap shows the correlation matrix of the variables:

corr_matrix <- cor(numeric_data)
corrplot::corrplot(corr_matrix, method = "color", tl.cex = 0.7)


Correlation Heatmap Interpretation
The heatmap above represents the correlation matrix of the variables in the dataset. Each square in the heatmap shows the correlation between two variables, with the color indicating the strength and direction of the correlation.

Clustered Correlations:
Groups of variables with strong positive correlations (dark blue clusters) suggest that these variables are closely related and may contribute similarly to the overall variance.
For example, in the PCA context, highly correlated variables tend to have similar contributions to the principal components.
Overview for PCA:
Variables that show strong correlations can be combined into fewer components in PCA to reduce redundancy while still retaining most of the information.
If variables are uncorrelated, they may contribute unique information, and their individual influence will be more spread across different principal components.
Conclusion:
The heatmap shows that many variables are highly correlated, suggesting that PCA is a suitable technique for dimensionality reduction as it captures correlated variables within fewer principal components.

Principal Component Analysis (PCA)
Perform PCA:

pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
summary(pca_result)
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
## Standard deviation     3.6392 2.4064 1.6891 1.39549 1.27052 1.10804 0.81250
## Proportion of Variance 0.4415 0.1930 0.0951 0.06491 0.05381 0.04093 0.02201
## Cumulative Proportion  0.4415 0.6345 0.7296 0.79450 0.84830 0.88923 0.91124
##                            PC8     PC9    PC10   PC11    PC12   PC13    PC14
## Standard deviation     0.68665 0.63658 0.58181 0.5421 0.51739 0.4898 0.39225
## Proportion of Variance 0.01572 0.01351 0.01128 0.0098 0.00892 0.0080 0.00513
## Cumulative Proportion  0.92695 0.94046 0.95174 0.9615 0.97046 0.9785 0.98359
##                           PC15    PC16    PC17    PC18    PC19    PC20    PC21
## Standard deviation     0.30823 0.28123 0.24127 0.22932 0.22199 0.17649 0.16507
## Proportion of Variance 0.00317 0.00264 0.00194 0.00175 0.00164 0.00104 0.00091
## Cumulative Proportion  0.98676 0.98939 0.99133 0.99309 0.99473 0.99577 0.99668
##                           PC22    PC23   PC24    PC25    PC26    PC27    PC28
## Standard deviation     0.16331 0.14929 0.1336 0.12474 0.08971 0.08237 0.04036
## Proportion of Variance 0.00089 0.00074 0.0006 0.00052 0.00027 0.00023 0.00005
## Cumulative Proportion  0.99756 0.99831 0.9989 0.99942 0.99969 0.99992 0.99997
##                           PC29    PC30
## Standard deviation     0.02757 0.01173
## Proportion of Variance 0.00003 0.00000
## Cumulative Proportion  1.00000 1.00000
PCA Summary Table Interpretation
The table above shows the importance of components from the Principal Component Analysis (PCA). It provides details for each principal component (PC), including the standard deviation, proportion of variance explained, and cumulative proportion of variance explained.

Key Sections:
Standard Deviation:
This represents the square root of the eigenvalues associated with each principal component.
A higher standard deviation means that the principal component explains more variance.
For example, PC1 has a standard deviation of 3.6392, indicating that it captures much more information compared to later PCs such as PC30 (with a standard deviation of 0.01173).
Proportion of Variance:
This shows the proportion of the total variance explained by each principal component.
PC1 explains 44.1% of the total variance, PC2 explains 19.3%, and PC3 explains 9.5%.
Subsequent PCs explain less variance, with very small contributions from PCs beyond PC10.
Cumulative Proportion:
This represents the cumulative sum of the variance explained by the principal components.
Key cumulative proportions:
The first two components (PC1 and PC2) explain 63.4% of the variance combined.
The first four components (PC1 to PC4) explain 79.4% of the total variance.
By including up to PC7, you capture over 91% of the variance.
Overview for Dimensionality Reduction:
The first few components (PC1 to PC4) explain most of the variance in the data and are therefore the most informative.
Components with very low contributions (e.g., PC20 and beyond) add minimal information and can likely be discarded.
A common threshold for deciding how many PCs to retain is to aim for 80-90% cumulative variance. In this case, retaining 4 to 7 components would capture the majority of the variance while significantly reducing the dimensionality.
Conclusion:
For this dataset, I could retain approximately 4 to 7 components to capture a significant amount of variance while discarding the rest to simplify the data.

Scree Plot
The scree plot shows the percentage of variance explained by each component:

  fviz_eig(pca_result, addlabels = TRUE) +
  labs(title = "Scree Plot: Variance Explained by PCs", x = "Principal Components", y = "Percentage of Variance Explained")


Scree Plot Interpretation
This scree plot shows the percentage of variance explained by each principal component (PC) in the Principal Component Analysis (PCA).

Key Points:
X-Axis (Principal Components):
The X-axis represents the different principal components (PC1, PC2, PC3, etc.). Each principal component is an independent linear combination of the original variables.

Y-Axis (Percentage of Variance Explained):
The Y-axis shows how much variance each principal component explains as a percentage of the total variance in the dataset.

Interpretation:
Explained Variance by PC1 and PC2:

PC1 explains 44.1% of the total variance.
PC2 explains 19.3% of the total variance.
Together, the first two PCs explain 63.4% of the total variance, which is a substantial amount.
Elbow Point (Dimensionality Reduction Insight):
The “elbow” point appears to be around PC2 to PC3. After this point, the explained variance gradually decreases and becomes smaller for the subsequent components.Suggesting that using the first few components (e.g., PC1 and PC3) may capture most of the essential information in the data, making them a good choice for dimensionality reduction.

Remaining Components:
The later components (e.g., PC7 onward) explain very little variance individually (close to 1-2%). These components may not add significant value to the overall understanding of the dataset.

Conclusion:
The first few principal components (e.g., PC1 to PC3) capture most of the dataset’s variability.
Discarding the later components with minimal variance (e.g., PC7 onward) can simplify the dataset without significant information loss.
Contributions of Variables to Principal Components
Top 10 Contributing Variables to PC1, PC2, and PC3
contribution_table <- as.data.frame(pca_result$rotation[, 1:3]) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(abs(PC1))) %>%
  slice(1:10)
contribution_table
##                Variable        PC1         PC2           PC3
## 1   concave.points_mean -0.2605856  0.03774287  0.0248863052
## 2        concavity_mean -0.2585127 -0.05883707 -0.0006030504
## 3  concave.points_worst -0.2502418  0.01024983  0.1692359862
## 4      compactness_mean -0.2390425 -0.14940317  0.0798322755
## 5       perimeter_worst -0.2346535  0.20427597  0.0475700922
## 6       concavity_worst -0.2270423 -0.09758419  0.1781302716
## 7          radius_worst -0.2256629  0.22425580  0.0459387022
## 8        perimeter_mean -0.2253281  0.21925242  0.0025295971
## 9            area_worst -0.2229940  0.22332649  0.0132567233
## 10            area_mean -0.2188273  0.23456207 -0.0329591478
1. Top 10 Variable Contributions to PC1
This section describes the contributions of variables to the first principal component (PC1).

# Top 10 variables contributing to PC1
fviz_contrib(pca_result, choice = "var", axes = 1, top = 10) +
  ggtitle("Top 10 Variable Contributions to PC1")


Description:
Main Contributors:
concave.points_mean, concavity_mean, and concave.points_worst have the highest contributions, each contributing over 6%.
compactness_mean, perimeter_worst, and concavity_worst also contribute significantly.
Overview:
PC1 captures variability related to tumor shape and size, focusing on concavity and perimeter-related measurements.
The dominance of concave.points_mean and related metrics suggests that PC1 reflects variations in tumor protrusions and point distribution characteristics.
2. Top 10 Variable Contributions to PC2
This section describes the contributions of variables to the second principal component (PC2).

# Top 10 variables contributing to PC2
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10) +
  ggtitle("Top 10 Variable Contributions to PC2")


Description:
Main Contributors:
fractal_dimension_mean is the highest contributor, with over 10% contribution, followed by fractal_dimension_se and fractal_dimension_worst.
Other important contributors include radius_mean, compactness_se, and area_mean.
Overview:
PC2 primarily captures border complexity and irregularity.
The dominance of fractal_dimension variables suggests that PC2 represents variations in tumor border fractality, emphasizing how rough or complex the tumor’s boundary is.
3. Top 10 Variable Contributions to PC3
This section describes the contributions of variables to the third principal component (PC3).

# Top 10 variables contributing to PC3
fviz_contrib(pca_result, choice = "var", axes = 3, top = 10) +
  ggtitle("Top 10 Variable Contributions to PC3")


Description:
Main Contributors:
texture_se contributes the most, with over 12% of the total contribution.
Other significant contributors include smoothness_se, symmetry_worst, and smoothness_worst.
Additional contributors include concave.points_se, radius_se, and compactness_worst.
Overview:
PC3 captures variability related to surface texture and smoothness.
The strong contributions of texture_se and smoothness_se indicate that PC3 reflects variations in the uniformity and surface granularity of tumor regions.
The presence of symmetry_worst suggests that this component may also capture asymmetry in extreme cases.
General Insights:
PC1: Focuses on tumor shape and size, emphasizing concavity and perimeter features.
PC2: Represents border complexity, particularly variations in fractal dimensions.
PC3: Captures surface texture and measurement variations, highlighting standard errors in texture and smoothness.
PCA Biplot of Variables
The biplot shows the contributions of variables in the PCA space:

  fviz_pca_var(pca_result, col.var = "darkred",labelsize=2) +
  labs(title = "Variables - PCA",
       x = paste0("Dim1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
       y = paste0("Dim2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)"))


Overview:
Axes (Dim1 and Dim2):
The X-axis (Dim1) represents the first principal component (PC1), which explains 44.1% of the variance.
The Y-axis (Dim2) represents the second principal component (PC2), explaining 19.3% of the variance.
Variable Arrows:
Each arrow represents a variable in the dataset.
The length of the arrow indicates the strength of the contribution of the variable to the components.
Variables with longer arrows contribute more to the variance explained by the PCs.
Direction of Arrows:
Arrows pointing in similar directions indicate that the variables are positively correlated.
Arrows pointing in opposite directions indicate a negative correlation.
Variables that form a 90-degree angle (perpendicular) are uncorrelated.
Relationships:
Variables such as concavity_mean, concave.points_mean, and compactness_mean have long arrows along PC1, indicating that PC1 is strongly influenced by tumor shape-related features.
In contrast, fractal_dimension_mean has a strong contribution along PC2, suggesting that this principal component distinguishes samples based on complexity-related features.
Circle of Correlations:
The circle helps interpret the relationships between variables. Variables close to the edge of the circle have a stronger contribution, while variables closer to the origin (center) contribute less to the components.
Conclusion:
The biplot shows that tumor shape metrics dominate the variance along PC1, while border complexity metrics such as fractal_dimension contribute heavily to PC2.
This visual reinforces the insights from the scree plot by showing which features drive the separation along the principal components.
Prepare PCA results for clustering (first two principal components)
pca_2d <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2])

# Perform K-means clustering on PCA results
set.seed(123)  # For reproducibility
kmeans_pca <- kmeans(pca_2d, centers = 2, nstart = 25)  # Choose 2 clusters (e.g., benign vs malignant)

# Add cluster labels to PCA data
pca_2d$Cluster <- as.factor(kmeans_pca$cluster)

# Visualize K-means clustering on PCA results
ggplot(pca_2d, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering on PCA (2D)", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "blue", "2" = "red"))  # Cluster 1 = blue, Cluster 2 = red


Description of PCA Clustering Result
The following describes the results of K-means clustering applied to the first two principal components (PC1 and PC2) obtained from Principal Component Analysis (PCA).

Overview
The separation between the clusters in the plot shows how the data points differ in terms of their principal components.
The blue and red points are relatively well-separated, indicating that the K-means algorithm identified two distinct clusters based on the transformed PCA space.
This analysis helps understand how well the original features, reduced to two dimensions, contribute to the differences in diagnosis types.
Clustering the PCA results in terms of categorical variable(Diagnosis)
# Add diagnosis labels to PCA data
pca_2d$Diagnosis <- filtered_data$diagnosis

# Visualize clusters with diagnosis labels
ggplot(pca_2d, aes(x = PC1, y = PC2, color = Diagnosis, shape = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering on PCA (2D) with Diagnosis Comparison", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_manual(values = c("B" = "green", "M" = "red"))  # Diagnosis: B = green, M = red


Description of PCA Clustering Result with Diagnosis Comparison
The following describes the K-means clustering results with an overlay of diagnosis labels for comparison, applied to the first two principal components (PC1 and PC2) obtained from Principal Component Analysis (PCA).

Overview
The clustering results (shapes) were compared with the actual diagnosis (colors):
Cluster 1 (circles) mostly corresponds to benign observations.
Cluster 2 (triangles) corresponds to malignant observations.
Some overlap exists, but the clustering generally aligns well with the actual diagnosis.
This comparison highlights how well K-means clustering, applied to the PCA-reduced dimensions, captures the distinction between benign and malignant diagnoses.
Computing t-SNE
What is t-SNE?
T-Distributed Stochastic Neighbor Embedding (t-SNE) is a non-linear dimensionality reduction technique primarily used for visualizing high-dimensional data in a low-dimensional space (typically 2D or 3D). Developed by Laurens van der Maaten and Geoffrey Hinton, t-SNE preserves the local structure of data by mapping similar points in high-dimensional space to nearby points in the low-dimensional representation.

How t-SNE Works:
t-SNE models the similarity between pairs of points in the original high-dimensional space using a conditional probability distribution.
In the low-dimensional space, it attempts to recreate those similarities by minimizing a loss function called Kullback-Leibler (KL) divergence.
t-SNE gives more weight to preserving local neighborhoods, making it particularly good for visualizing clusters.
Why t-SNE Was Used in This Project
t-SNE was applied in addition to PCA for the following reasons:

Capture Non-linear Relationships:
PCA assumes that variance in the data can be explained using linear combinations of features. However, the tumor features may exhibit complex, non-linear patterns that PCA may not fully capture. t-SNE can capture these non-linear patterns and provide more meaningful clusters.

Visualize High-Dimensional Data in 2D:
t-SNE is designed for visualizing high-dimensional data by mapping it to a 2D space while preserving local neighborhoods. This makes it easier to observe and interpret clusters related to different diagnoses.

Complementary to PCA:
PCA provides a global summary of variance in the data, whereas t-SNE focuses on preserving local relationships between data points. The use of both methods allows for a more comprehensive understanding of the data structure.

Improved Cluster Separation:
t-SNE is known for creating distinct visual clusters, which is helpful for confirming whether benign and malignant tumors form separable groups. This helps validate the clustering results observed in PCA.

# Install Rtsne if not installed
if (!requireNamespace("Rtsne", quietly = TRUE)) {
  install.packages("Rtsne")
}

# Load the necessary library
library(Rtsne)

# Perform t-SNE
tsne_result <- Rtsne(scaled_data, dims = 2, perplexity = 30, max_iter = 3000)
tsne_2d <- data.frame(tsne_result$Y)
colnames(tsne_2d) <- c("tSNE1", "tSNE2")

# Add diagnosis labels to the t-SNE result
tsne_2d$Diagnosis <- filtered_data$diagnosis

# Plot the t-SNE with diagnosis labels (color-coded)
ggplot(tsne_2d, aes(x = tSNE1, y = tSNE2, color = Diagnosis)) +
  geom_point(size = 3) +
  labs(title = "t-SNE with Diagnosis Labels", x = "tSNE1", y = "tSNE2") +
  scale_color_manual(values = c("B" = "green", "M" = "red")) +
  theme_minimal()


Description of t-SNE with Diagnosis Labels
Data Preparation
The t-SNE algorithm was applied to the scaled dataset with the following parameters:
Dimensions (dims): 2 (two-dimensional projection)
Perplexity: 30 (controls the balance between local and global aspects)
Maximum iterations (max_iter): 3000
The resulting t-SNE components were stored as tSNE1 and tSNE2.
Diagnosis labels (B for benign, M for malignant) were added to the t-SNE results.
Overview
The t-SNE plot shows a clear separation between the benign (green) and malignant (red) diagnoses.
The clusters formed in the t-SNE space suggest that the t-SNE method effectively captured the differences in the data based on diagnosis.
Some degree of overlap still present, indicating complexity in the structure of the dataset, for example .
This plot demonstrates that the dimensionality reduction using t-SNE preserves the separability of diagnoses when visualized in two dimensions.
Overall Conclusion
This research utilized Principal Component Analysis (PCA), K-means clustering, and t-Distributed Stochastic Neighbor Embedding (t-SNE) to analyze tumor characteristics and differentiate between benign and malignant tumors. The primary goal was to reduce the dataset’s dimensionality while retaining the key patterns and insights necessary for accurate classification.

The study demonstrated that: - PCA effectively reduced the dataset’s dimensions by capturing the majority of the variance within a few principal components. - Clustering using the PCA-reduced data identified meaningful groups corresponding to the tumor diagnosis labels. - t-SNE provided a more visually distinct separation between benign and malignant tumors, reinforcing the findings from PCA and clustering.

This analysis confirmed the importance of specific tumor features, such as concavity, perimeter, and fractal dimension, in differentiating between benign and malignant tumors. Although PCA provided a linear reduction of dimensions, t-SNE’s non-linear approach was able to better capture complex patterns in the data.

Conclusion and Future Directions
The analysis highlighted the effectiveness of dimensionality reduction techniques like PCA and t-SNE in understanding complex datasets. K-means clustering, when applied to PCA-reduced data, provided meaningful insights into tumor classification. However, future work could explore: - Experimentation with different t-SNE perplexity values to further validate the robustness of the results.

This research provides a strong foundation for applying dimensionality reduction and clustering in biomedical datasets, showcasing the potential for identifying critical features that contribute to medical diagnoses.

Additional Resources
Serrano.Academy about PCA
Statistics Globe about PCA
StatQuest YouTube Video on PCA
StatQuest YouTube Video on t-SNE
Distill Guide on t-SNE https://youtu.be/NEaUSP4YerM?feature=shared
