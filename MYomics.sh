#!/bin/bash

echo "shiny::runApp('.', launch.browser = T)" > runapp.R
Rscript runapp.R
rm runapp.R

