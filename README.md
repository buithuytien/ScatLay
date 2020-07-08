<h1><p align="center"> ScatLay </p></h1>

ScatLay identifies differential genes from gene expression data by using the overlap of 2 scatter plots. Plots are generated in log10 scale. The source code can be interacted via command-line interface.

## Contents 
- [Getting started](#getting-started)</br>
  - [Prerequisites](#prerequisites)</br>
  - [Launching ScatLay](#launching-scatlay)</br>
  - [Gene expression data format](#gene-expression-data-format)</br>
- [User guide](#user-guide)</br>
  - [Reading the data](#reading-the-data)</br>
  - [Preprocessing](#preprocessing)</br>
  - [ScatLay](#scatlay)</br>
- [About](#about)</br>


## Getting started

### Prerequisites
The code runs in Python 3 and RStudio environment. You will need to install Python, R and RStudio. Please follow the installation steps closely

#### R and RStudio
1. Install R from https://cran.r-project.org/
2. Install Rstudio from https://www.rstudio.com/ </br>
This ScatLay version was developed and tested on R version 4.0.0 and RStudio version 1.3.959, Windows OS

#### Python
1. Install Python from https://www.python.org/downloads/
2. Take note of your installed Python folder. For example, in my case, Python was installed in `C:\Users\BUITT\AppData\Local\Programs\Python\Python38`. In some other cases, Python can be installed in `C:\Program Files\Python3`
![alt_text](https://github.com/buithuytien/ScatLay/blob/master/www/screenshots/00_python38_IDLE.PNG)

#### Python dependencies
1. Launch Windows Command Line:
* Press Windows Key + X.
* Click Run.
* Type in cmd.exe and hit enter.
2. Go to the folder named `Scripts` inside your installed Python folder in the command line. For example, in my case: </br>
```
cd C:\Users\BUITT\AppData\Local\Programs\Python\Python38\Scripts
```
3. Install deppendencies packages: ```numpy```, ```pandas``` and ```matplotlib```, which can be installed via ```pip``` in the command line: </br>
```
pip install numpy pandas matplotlib
```

#### Download Scatlay
Clone Scatlay, or download the zipped file from Github and then unzip it

### Launching Scatlay
Open the `ScatLay.R` file using RStudio and click `RunApp` button on the topright. </br>
 
![alt_text](https://github.com/buithuytien/ScatLay/blob/master/www/screenshots/01_launch.PNG)

You're good to go! A demo file ```ecoli_expression_tpm.csv``` along with its meta data file ```ecoli_meta.csv``` has been provided containing Ecoli gene expression data (TPM normalized with no cut-off). 

### Gene expression data format
The input gene expression data MUST BE in comma-separated values ```csv``` format, where rows are *genes* and columns are *samples*. The first column MUST contain gene names. Replicates of the same column should be placed together. For example: </br>

![alt text](https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_normalised.png)

The meta data file should match column names of data file to experimental conditions should be given in two-column .csv formate. For example: </br>
![alt text](https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_metadata.png)

The demo.csv file is an excerpt of the gene expression data taken from [Gene Expression Omnibus database](https://www.ncbi.nlm.nih.gov/geo/) . Full data can be accessed via accession number [GSE71562](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE71562) . The demo.csv file include replicate a and b for the 2 conditions 0 minute and 10 minute (namely a1 and a6 for replicate a, b1 and b6 for replicate b)

<details>
<summary> Example </summary>

|       | a1 | b1 | a6 | b6 |
| ----- | -- | -- | -- | -- |
| G1    | 2  | 7  | 3  | 2  |
| G2    | 4  | 6  | 2  | 0  |
| G3    | 0  | 5  | 0  | 0  |
| ..... | 3  | 2  | 1  | 2  |

</details>

## User guide
Under this section, you will learn how to read in your own data and apply customisations to the scatter plots. 

### Reading the data
* Choose an RNA-Seq data file in comma-separated value (.csv) format.
* If you input a normalised data file, it should have gene names in rows and genotypes in columns, following the usual format of files deposited in the GEO database.
* If you input raw data (read counts), please make sure that the first column contains gene names, and the read counts of each genotype (conditions: wildtype, mutants, replicates, etc.) are in the following columns. Each genotype column should have a column name.
  - Along with raw read counts, you can provide gene length (base pair) information in two-column .csv file, with the first column specifying gene names, which must match the gene names in raw data file, and the second column specifying gene length in base pair. Gene length file is required for normalization methods for sequencing depth and gene length: RPKM, FPKM, TPM
  - List of negative control genes (spike-in or stably expressed genes accross all samples), if available, should be contained in one-column .csv file. Negative control genes are required for Remove Unwated Variation (RUV) normalziation.
* Finally, a metadata table matching column names of data file to experimental conditions should be given in two-column .csv formate. Metadata table is required for differential expression analysis
* Hit `SUBMIT` button. The software automatically move on to the preprocessing and analysis tabs once the datafile is loaded.

![alt_text](https://github.com/buithuytien/ScatLay/blob/master/www/screenshots/02_load_in.PNG)

### Preprocessing
* Specify the cut-off expression values (same unit to your input data file - either raw read counts or normalized expression), and the minimum number of columns (samples) whose expression is above threshold value. 
* Normalization methods are available depending on your input of supporting data files (gene length and negative control genes). 
* Relative Log Expression (RLE) plot of raw and normalized data are displayed to compare the effects of normalziation

![alt_text](https://github.com/buithuytien/ScatLay/blob/master/www/screenshots/03_preprocessing.PNG)

### ScatLay
* ScatLay finds differentially expressed genes using 2 criteria:
  - When overlaying scatter between 2 conditions onto scatter between 2 replicates, the genes that are non-overlapping are candidates for differentially expressed. This is controlled by the size of scatter dot, defaulted at 0.01. Varying the scatter dot size directly affects number of overlapping/non-overlapping points
  - p-value associating with each gene, estimated by integrating 2D kernel density from the scatter between 2 replicates, from -Infinity to coordinates of that gene. This is controlled by p-value threshold, defaulted at 0.1

* You will need to specify the 2 condtions for analyzing differentially expressed genes. You will also need to specify 2 replicates used in this analysis

* 4 scatter plots will be generated:
  - Top panels: Scatter plot betwen 2 replicates. 
  - Bottom left panel: Scatter plot between 2 condition
  - Bottom right panel: Overlaid between-condition scatter ontop of between-replicate scatter. Differentially expressed genes are highlighted in GREEN

![alt_text](https://github.com/buithuytien/ScatLay/blob/master/www/screenshots/04_scatters.PNG)
 
* Table of Differentially Expressed genes:
  - Genes that are non-overlapping in the overlaid scatters, abd satisfy the p-value cut-off condition are listed in the `DE Gene Table`
  - You can retrieve (.csv format) this list of DE genes by the `doanload` button on the side bar

![alt_text](https://github.com/buithuytien/ScatLay/blob/master/www/screenshots/05_DEtable.PNG)

## About
ScatLay identify differentially expressed genes by overlaying gene expression scatter plot of 2 different conditions on top of that of 2 replicates between the same condition. The non-overlapping genes are differentially expressed genes.
