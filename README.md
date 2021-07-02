2/07/2021

CIL_query is a suite of R scripts to query the Corpus Interlangue (CIL) data located on nakala.fr

Each script has a specific function detailed below. 

Create_data.R: 
1. Enter working directory
2. Enter CIL collection ID (found on nakala.fr) 11280/4caeaf9c
3. Enter the file type containing learner texts: eaf for the conversation and .txt for the learner writings. 
4. Scans the CIL collection. 
5. Retrieves all .eaf files and meta csv files. 
6. Merges them into one csv.

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
