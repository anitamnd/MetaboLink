# JLspec Documentation
---

JLspec is a web-based application created with shiny R and it is available at http://computproteomics.bmb.sdu.dk/Metabolomics/. For any questions, please contact X via email or visit the GitHub repository [here](https://github.com/anitamnd/jlspec_2_0).

![App dashboard](image.png)

## 1. Input

### Data file
* Comma-separated values (CSV) file with samples in columns and features in rows.

### Sequence file (metafile)
* After uploading the datafile, your dashboard will update to the sequence panel, and you will be able to upload a CSV file for the sequence.
* The sequence file works as an identify 

| Sample | Label | Batch | Group | Time | Paired |
| ----   | ----  | ----  | ----  | ---- | ----   |
| Names  | Name  |       |       |      |        |
| QC01   | QC    | 1     |       |      |        |
| m.z    | Mass  | 1     |       |      |        |

* **Sample:** column names of the datafile

* **Label:** Name/QC/

* **Batch:** batch number

* **Group:** group name/number

* **Time:** time point (e.g., 15min/day 1/... )

* **Paired:** paired samples should have the same value in this column.


> Important noting:
* When uploading a datafile, the app will automatically detect the labels for each column.
* Your sequence file does not need to have all the columns above to be valid, but it should not have more than those.
* When manually updating the metafile, the user must press the 'Update' button to save the changed values.
* It is only possible to change the label in the metafile by changing it within the shiny application.


## 2. Data Pre-processing

### 2.1 Blank filtration
Remove uninformative features based on the ratio of the sample mean versus the blank mean, removing the features which are not abundant in the samples when compared to the blank samples.

### 2.2 Missing value filtration
Removing features with more than a user-defined percentage of missing values.

* in class
* entire data
* QC

### 2.3 Internal standards normalization
To account for technical variations and enable meaningful comparisons. This function requires a data file including a column of retention times labeled "RT" and a column of annotations labeled "Name". At least one feature should be an internal standard and include "(is)" within its annotation.

### 2.4 Imputation

* KNN: impute missing values with KNN algorithm.
* Median: impute missing values with median value from class.
* min/X: impute missing values with class minimum divided by X (user-defined).

### 2.5 Drift correction
By including quality control (QC) samples from a pool of all the samples and correct each metabolite. To find the drift pattern the app uses locally estimated scatterplot smoothing (LOESS).

### 2.6 Merge datasets
Positive and negative ion modes merge to ensure the best coverage of detected metabolites.

### 2.7 QC normalization
- 
## 3. Statistical Analysis

Unpaired

Paired

Time series

## 4. Output

### Sequence panel
In the sequence panel one can download the sequence (.csv file) on the bottom left.

### Export panel
In the export panel one can download:

* .csv data file
* .csv data file for metaboanalyst
* .xlsx file
* Statistics results

---

## 5. Instalation

Dockerfile...