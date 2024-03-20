# Flocos
The following repo contains the code to replicate the studies made in our paper entitled *Is it fair to be accurate? 
Studying moral reactions to the use of artificial intelligence.*

## Gettings started
### Setting up the environment
The script are written in the R programming language. 
R needs to be installed on your machine (as well as RStudio, depending on how you want to run R). 
R can be installed here : `https://cran.r-project.org/`
The required packages are automatically download in the scripts.

### Step 1 : Downloading the data
1. Download the data located on the dataverse : 
`https://dataverse.uclouvain.be/dataset.xhtml?persistentId=doi:10.14428/DVN/ZGD5DR`.
To download the data, select "Access Dataset" and download the Original Format ZIP. 
2. Unzip the data in any location. E.g., `~/Documents/`. 
Create a `data` folder, and inside the folder, create a `raw` folder.
The data should be stored in a `~/Documents/data/raw/`.

## Step 2 : Running the scripts
1. Clone the present repo (`flocos`) and create a file `.env` containing the following line:
`PATH='~/Documents/data/'`
2. Create a folder called `results` in `~/Documents/data/`. 
The results of the studies will be exported in `.csv` in the folder `~/Documents/data/results/`
3. Run the pretest script `Pretests.R` and/or the different studies `StudyX.R`