# pest-interceptions
Collection of data request of pest interceptions
## Overview
This repository contains data on pest interceptions collected from various sources. The data is organized in CSV files and includes information such as the type of pest, location of interception, date, and other relevant details.


## How to
### Scope the request
1. Identify the pest of interest: common and taxonomic names
2. Determine the origin countries of interest.
3. Specify the time frame for the data.
4. Confirm the commodity or host plant associated with the pest: common and taxonomic names

10/15/25 we are working on a standardized form to scope the request. Ideally, this will address nuances besides these broader questions

### Access the blank form
The blank form is found as a readme file to prevent accidental overwriting. Save the file as a .Rmd, fill the parameters, and run the code chunks to generate a filled form and data request.

### Include add-ons
If there are any specific analyses, visualizations, or additional data processing steps identified in the scoping document, write them flexibly and save as a script in add-ons folder. The script and function should be saved to reflect what it does.
- include #' nomenclature to describe the function and its dependencies
The script can be sourced using souce() and help(function_name) or ?function_name will show the documentation made using the #' nomenclature

### Access the data
The data will be saved in a folder within the `reports` folder in the root directory of this repository. An individual folder will have both a CSV and report file. The naming structure reflects the pest, origin countries, time frame, and commodity/host plant.

### Share the data
Once the new file is saved, create a pull/push request to this repository
Share the files over sharepoint or point to the github repository with the data available