<h1><p align="center"> ScatterOverlay </p></h1>

ScatterOverlay identifies differential genes from gene expression data by using the overlap of 2 scatter plots. Plots are generated in log10 scale. The source code can be interacted via command-line interface.

## Contents 
- [Getting started](#getting-started)</br>
  - [Prerequisites](#prerequisites)</br>
  - [Gene expression data format](#gene-expression-data-format)</br>
- [User guide](#user-guide)</br>
  - [Reading the data](#reading-the-data)</br>
  - [Marker size](#marker-size)</br>
  - [Saving the output](#saving-the-output)</br>
- [About](#about)</br>


## Getting started

### Prerequisites
The code runs in Python 3 environment. Dependencies used are ```numpy```, ```pandas``` and ```matplotlib```, which can be installed via ```pip``` in the command line: </br>

```
pip install numpy pandas matplotlib
```

Download the file, unzip it, and move the folder to your preferred location. Then, open up command line and ```cd``` to the location where you have placed the folder. For example: </br>

```
cd D:\ScatLay-master
```

You're good to go! A demo file ```demo.csv``` has been provided containing Ecoli gene expression data (TPM with no cut-off). You can try it out by running: </br>

```
python ScatLay.py -filename demo.csv
```

You should see these plots being displayed below. Overlapping regions are denoted in orange while non-overlapping points are identified as differential genes.

![alt text](https://github.com/buithuytien/ScatLay/blob/master/scatter.png)


### Gene expression data format
The input gene expression data is expected in comma-separated values ```csv``` format, where rows are *genes* and columns are *samples*. The demo file ```demo.csv``` shows the expected format.

<details>
<summary> Example </summary>
  
|       | S1  | S2  | S3  | ... |
|-------|-----|-----|-----|-----|   
| G1    | 2   | 7   | 3   | 2   |
| G2    | 4   | 6   | 2   | 0   |
| G3    | 0   | 5   | 0   | 0   |
| ..... | 3   | 2   | 1   | 2   |

</details>


## User guide
Under this section, you will learn how to read in your own data and apply customisations to the scatter plots. If at any time you need a quick reference, run the ```help``` command: </br>

```
python ScatLay.py --help
```

### Reading the data
Name your input gene expression data as ```data.csv``` and place it in the ```ScatterOverlay-master``` folder. By default, the first 4 samples in the data are used for the plots. If you would like to read in different ```samples``` from the file (e.g. columns 1,2,3,4), run the following: </br>

```
python ScatLay.py -columns 1,2,3,4 -filename demo.csv
```

### Marker size
The default marker size used for the plots is 0.0125. Varying the marker size directly affects number of overlapping/non-overlapping points. To modify the ```size``` used (e.g. 0.03), run the following:

```
python ScatLay.py -size 0.03 -filename demo.csv
```

Finally, you can combine the various customisations applied above to be as such:
```
python ScatLay.py -columns 2,4,6,8 -size 0.03 -filename demo.csv
```

### Saving the output
The csv file containing names of differentially expressed genes will be automatically generated after each run. It is saved in the same directory as ```ScatLay.py```. To save individual scatter plots, simply click on the save icon located below each plot.

## About
ScatLay identify differentially expressed genes by overlaying gene expression scatter plot of 2 different conditions on top of that of 2 replicates between the same condition. The non-overlapping genes are differentially expressed genes.

