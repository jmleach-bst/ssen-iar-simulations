library(drake)
library(tidyverse)
library(knitr)
library(sim2Dpredictr)
library(glmnet)
library(rstan)
rstan_options(auto_write = TRUE) # avoid stan re-compiling stan files
library(BhGLM)
library(ssnet)
library(testthat)

