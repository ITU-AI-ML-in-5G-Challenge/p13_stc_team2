# ITU 2020: Problem 13 - STC Team 2

## Requirements

This project can be run using [**docker**](https://www.docker.com/products/docker-desktop). 

Download the following datasets from [here](https://zenodo.org/record/4059189#.X6ODSdtRVZM):

- input_node_files

- input_node_files_test

- output_simulator

- output_simulator_test

This could not be uploaded to Github due to size limitation. 

## Start

- Clone this project project 

- Add the downloaded datasets to the data folder so that there will be an additional four folders inside the data folder 

- In the terminal run`docker-compose up -d` to start the docker container

- Open `localhost:8787` in browser

- User: **rstudio**

- Password: **itu**

## Notes

The file`main.R`  has the high level functions. Change the value of `clean_data` and `train_and_score` functions to `FALSE` if you do not want to redo the cleaning and training after first run. 

In this project, we splited the training data into training and testing and we ran the training function on the whole training set usiing 10-folds cross-valdiation. Further, we used **100** and **300** combinations with ten folds cross valdiation. 





