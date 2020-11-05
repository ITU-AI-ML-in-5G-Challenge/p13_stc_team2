# ITU 2020: Problem 13

## Requirements

This project can be run using docker. 

Download the following datasets:

- input_node_files

- input_node_files_test

- output_simulator

- output_simulator_test

This could not be uploaded to Github limitation. Data can be downloaded from [here](https://zenodo.org/record/4059189#.X6ODSdtRVZM)

## Start

- Clone this project project 

- Add the downloaded datasets to the data folder so that there will be four folders inside the data folder 

- In the terminal run`docker-compose up -d` to start the docker container

- Open localhost:8787 in browser

- User: rstuido

- Password: itu

## Notes

The file`main.R`  has the high level functions. By default, if you used docker, you won't run the cleaning nor the training and will use the output of these two functions. Change the value of `clean_data` and `train_and_score` functions to `TRUE` to rerun them. 

In this project, we split the training data into  training and testing and we ran the training function on the whole training set. Further, we used **100** combinations with 10 folds cross valdiation 





