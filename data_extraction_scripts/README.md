## Scripts to extract anonymized data from CLARA 

- The file `StudentSuccessViews.txt` contains the procedure to create an anonymized mirror of the CLARA tables

- The file `dbo.LoadData.sql` contains the script to write out the anonymized tables into `txt` files, with the headers stored in `fmt` files.

- the `R` folder holds a script that can take the `txt` files and `fmt` files, ideally all stored in a common folder ( and identified in the script as the path to that directory on local workstation ), and buld the R environment workspace automatically. It is this workspace that is needed to run any of the other scripts in the different R folders.