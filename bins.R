# bin app
# © Denis Rasulev 2016

# load package
library(data.table)

# read data in
raw <- data.table(read.csv2("raw.csv")) # raw data
bin <- data.table(read.csv2("bin.csv")) # values to bins conversion table
cff <- data.table(read.csv2("cff.csv")) # bins to coefficients table

# PART 1 - ASSIGN BINS BASED ON VARIABLES VALUES
------------------------------------------------

# create new data table where we will replace actual values with bins
binned <- raw

# assign bins according to variable values
for(i in 1:nrow(raw)) {
    # age
    binned$age[i] <- bin[var == "AGE" & raw$age[i] >= min & raw$age[i] <= max]$bin
    # profession; NA values are replaced with bin 6
    ifelse(is.na(raw$prf[i]),
           binned$prf[i] <- 6,
           binned$prf[i] <- bin[var == "PROFESSION" & raw$prf[i] == min]$bin
    )
    # region
    binned$reg[i] <- bin[var == "REGION" & raw$reg[i] == min]$bin
}

# PART 2 - SCORE BINS USING COEFFICIENTS
----------------------------------------

# create new data table where we will replace bin values with bin * coefficient values
scored <- binned

# find coefficients according to the bin value of the variable
# then multiply bin value to found coefficient and save it
# since there are no coefficients for profession, do not change it
for(i in 1:nrow(binned)) {
    # age
    scored$age[i] <- cff[var == "AGE" & bin == binned$age[i]]$cff * binned$age[i]
    # region
    scored$reg[i] <- cff[var == "REGION" & bin == binned$reg[i]]$cff * binned$reg[i]
}

# PART 3 - CALCULATE FINAL SCORE PER ID
---------------------------------------

# create final data table with id from raw data and score column filled with 0 values
final <- data.table(id = scored[, id], score = 0)

# calculate final scores and fill them in
for (i in 1:nrow(scored)) {
    final$score[i] <- scored$age[i] + scored$prf[i] + scored$reg[i]
}

# eof
