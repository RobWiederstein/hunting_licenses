convert_to_constant_dollars <-function(price, from_date, to_date){
        #convert to constant dollars
        #USCB uses BLS's (CPI-U-RS)
        #more info here: www.bls.gov/cpi/cpiurs.htm
        path  <- "./data_pure/bls/CPI_U_RS.xlsx"
        df.00 <- readxl::read_xlsx(path = path, range = "A3:B76")
        colnames(df.00) <- c("year", "cpi")
        myArray <- array(dim = length(price))
        for(i in 1:length(from_date)){
                conv_to_date <- price[i] * df.00[which(df.00$year == to_date), 2]
                myArray[i] <- conv_to_date / df.00[which(df.00$year == from_date[i]), 2]
                }
        unlist(myArray)
}
convert_to_constant_dollars(price = 1000, from_date = 1947, to_date = 2000)
convert_to_constant_dollars(price = 101608879, from_date = 1970, to_date = 2019)
