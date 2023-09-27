# JLspec Documentation
---

JLspec is a web-based application created with shiny R. For any questions, please contact X via email or visit the GitHub repository [here](https://github.com/anitamnd/jlspec_2_0).

![App dashboard](image.png)

## Input

### Data file
* Comma-separated values file with samples in columns and features in rows.

### Sequence file
* After uploading the datafile, your dashboard will update to the sequence panel, and you will be able to upload a CSV file for the sequence.


| Sample | Label         | Batch | Group | Time | Paired |
| -----  |:-------------:| -----:|-----: |-----: |-----: |
| Names  | Name |  | | | |
| QC01     | QC      |   | | | |
| m.z    | Mass      |    | | | |



## Data transformation (side panel)

Imagem

### Blank filtration
 remove uninformative features based on the ratio of the sample mean versus 
the blank mean, removing the features which are not abundant in the samples when compared 
to the blank samples

### Missing value filtration
 removing features with more than a user-defined percentage of missing 
values

### Internal standards normalization
 to account for technical variations and enable meaningful 
comparisons

### Imputation
which is another way of removing missing values and it consists of imputation of the 
missing values with half of the minimum detected value from the data.


### Drift correction
by including quality control (QC) samples from a pool of all the samples and 
correct each metabolite. To find the drift pattern the app uses locally estimated scatterplot 
smoothing (LOESS)

### Merge datasets
positive and negative ion modes merge to ensure the best coverage of detected 
metabolites

### QC normalization

## Statistical Analysis
Paired
Unpaired
Time series

## Output

### Sequence panel
* Download sequence file

### Export panel

