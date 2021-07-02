These scripts are to be used with the Coprus Interlangue (CIL) data located on nakala.fr

Each script has a specific function detailed below. 

Create_data.R: 
1. Scans the CIL collection. 
2. Retrieves all .eaf files and meta csv files. 
3. Merges them into one csv.

Cil_query.R : 
Imput: files created by create_data.R. 
1. Linguistic processing with UDpipe. 
2. Adds metadata per token. 
3. Output: linguistic dataset including, texts, metadata and linguistic annotation.

Visualize.R: 
Input: files created with CIL_query.R. 
Creates cooccurence visualisations. 

update.R: 
Upload or/and update CIL collection. 
Checks the difference between local corpus directory and collection. Ignores existing data and uploads new data. 

upload_data.R: Upload all files to new collection. 
Important NOTE: if  the data is already on Nakala, new data will still be created in Nakala even if it corresponds to the same ID-learner.Do not use unless it is for a new corpus. 
