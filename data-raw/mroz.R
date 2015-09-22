#Load and save the mroz data.
mroz = read.csv("data-raw/mroz.csv")

devtools::use_data(mroz, compress = "bzip2", overwrite=TRUE)
