% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enstack.R
\name{enstack}
\alias{enstack}
\title{Stacking Ensemble Learning Function}
\usage{
enstack(iris, split, response)
}
\arguments{
\item{dataset, Split}{ratio, Response variable}
}
\value{
Confusion Matrix
}
\description{
This function takes 1) dataset, 2) Split ratio, 3) Response variable as arguments
It uses SVM, Decision tree and Logistic Regression as base learners for stacking
For meta model training it uses Random Forest
This function returns confusion matrix
please not that response variable should have factor levels or else SVM will not work
}
