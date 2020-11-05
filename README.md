# ITU 2020: Problem 13

## Requirements

This project can be run using docker


## Start

- In the terminal while inside this project after it is cloned, run`docker-compose up -d` to start the docker container

- Open localhost:8787 in browser

- User: rstuido

- Password: itu

## Notes

The file`main.R`  has the high level functions. By default, if you used docker, you won't run the cleaning nor the training and will use the output of these two functions. Change the value of `clean_data` and `train_and_score` functions to `TRUE` to rerun them. 

In this project, we split the training data into  training and testing and we ran the training function on the whole training set. Further, we used **100** combinations with 10 folds cross valdiation 





