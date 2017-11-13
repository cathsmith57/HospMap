# HospMapper User Guide

HospMapper is an [R Shiny](https://shiny.rstudio.com/) app. 

To use, you need to have [R](https://www.r-project.org/) installed on your machine. 

You then need to install the Shiny package: ```install.pacakges("shiny")```, and load it: ```library(shiny)```.

## Runnnig the app
1. Create a new folder to act as your working directory. Set your working directory in R using: ```setwd("~path/name-of-folder")```
2. Download two files that make up the app:[ui.R](https://github.com/cathsmith57/HospMap/blob/master/ui.R) and [server.R](https://github.com/cathsmith57/HospMap/tree/master/server.R) folder and save in your working directory.
3. Run the app: ```runApp()```

## Example data
When the app opens, a data input screen will load. To run the app with example data, select the 'Load dummy data' option. The example data sets and variable names will automatically be selected. Click 'Go' to view visualisations.  

## Loading data

HospMapper uses three data files, examples of which can be seen by running the application with the 'Dummy data' option selected.
For each data set, click the 'Browse' button and navigate to the saved .csv file with your data. The file that you have selected will appear in the 'Preview data' section.
The names of the columns in your dataset containing the key variables then need to be identified using the drop-downs in the 'Identify variables' section. 

These variables are:

### Core data
This is the minimum data set required to run the app. It is formatted as one row per individual with an infection and must contain:

- Unique patient identifier.
- Admission date - date on which patient was admitted to hospital.
- Sample date - date on which sample that tested positive for infection was taken from patient.
- Ward identifier - name of ward on which patient was staying when their positive sample was taken.

Optionally, it can also include categorical variables that describe the patient. For example, age group, sex, reason for admission to hospital, infection sub-type.

### Ward transfers
The dates that patients moved into and out of different wards during their hospital admission. This allows the timeline and hospital map visualisation to be produced. It is formatted as one row per ward per patient with an infection and must contain:

- Patient identifier (must be the same as the identifiers used in core data).
- Ward identifier - name of ward in which patient stayed.
- Date into ward - date that patient was admitted to the ward.
- Day out of ward - date that patient was discharged from the ward.

Optionally, if known, the floor within the hospital that the ward is located can be specified. This will allow wards to be laid out on the map according to the floor that they are located on.

### Genetic distances
The genetic distance between each patient's infection. This allows genetic links between patients to be visualised on the map. It is formatted as distances between each pair of patients in the data. For all pairs of patients it must have:

- Identifier of patient one (must be the same as the identifiers used in core data).
- Identifier of patient two (must be the same as the identifiers used in core data).
- Genetic distance between infection of patients one and two.

## Viewing visualisations
After you have clicked 'Go', the Bar Chart visualisation will load. If you have included ward transfers data, the timeline and map tabs will also be active. 



