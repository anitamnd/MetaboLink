# JLspec
---

JLspec is a web-based application created with shiny R and it is available at http://computproteomics.bmb.sdu.dk/Metabolomics/.


## 1. Input

### Data file
* Comma-separated values (CSV) file with samples in columns and features in rows.

### Sequence file (metafile)
* After uploading the datafile, your dashboard will update and open the sequence panel. Here you will be able to upload a CSV file which works and an ID for the main table: 

| Sample | Label | Batch | Group | Time | Paired |
| ----   | ----  | ----  | ----  | ---- | ----   |
| Names  | Name  |       |       |      |        |
| QC01   | QC    | 1     |       |      |        |
| m.z    | Mass  | 1     |       |      |        |

* **Sample:** column names of the datafile

* **Label:** Name/MASS/RT/Blank/Sample/QC/Adduct_pos/Adduct_neg

* **Batch:** batch number

* **Group:** group name/number

* **Time:** time point (e.g., 15min/day 1/... )

* **Paired:** paired samples should have the same value in this column.


> Important noting:
* When uploading a datafile, the app will automatically detect the labels for each column
* Your sequence file does not need to have all the columns above to be valid, but it should not have more than those
* When manually updating the metafile, the user must press the 'Update' button to save the changed values
* It is only possible to change the label in the metafile by changing it within the shiny application

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

### 2.4 Normalization
Normalize the data using:

* Probabilistic Quocient Normalization (PQN) using the QC samples as reference
* Sum
* Median

### 2.5 Imputation

* KNN: impute missing values with KNN algorithm.
* Median: impute missing values with median value from class.
* min/X: impute missing values with class minimum divided by X (user-defined).

### 2.6 Drift correction
By including quality control (QC) samples from a pool of all the samples and correct each metabolite. To find the drift pattern the app uses locally estimated scatterplot smoothing (LOESS).

### 2.7 Merge datasets
Positive and negative ion modes merge to ensure the best coverage of detected metabolites.


## 3. Statistical Analysis
JLspec uses the sequence file to detect the different groups/conditions and time points and allows the user to select which they would like to compare.

The app allows for unpaired, paired and time series tests using the Limma package.

## 4. Output

### Sequence panel
In the sequence panel one can download the sequence (.csv file) on the bottom left.

### Export panel
In the export panel one can download:

* .csv data file
* .csv data file for metaboanalyst
* .csv data file for polystest
* .xlsx file
* .csv statistics results file

---

## Getting help
If you have any suggestions for improvement, contact anitamnd@outlook.com. If you encounter bugs or have further questions or requests, you can raise an issue at the issue page.
