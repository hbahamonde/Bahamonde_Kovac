cat("\014")
rm(list=ls())
graphics.off()


##############################
# Yearly weight data for weight matrix
##############################
# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

p_load(foreign)
bilateral.d = read.csv("https://github.com/hbahamonde/Bahamonde_Kovac/raw/master/Dyadic_COW_4.0.csv")

# Codebook
## importer1: Name of country A
## importer2: Name of country B
## flow1: Imports of Country A from Country B [in US millions of current dollars]
## flow2: Imports of Country B from Country A [in US millions of current dollars]

# keeping columns i'll need
bilateral.d <- bilateral.d[c("year", "importer1", "importer2", "flow1", "flow2")]

trade = bilateral.d

# Drop NAs
trade$flow1[trade$flow1 == -9] <- 0
trade$flow2[trade$flow2 ==  -9] <- 0

# Loop to Generate Independent XLSX Files (one per year) contanining the square country matrices.
p_load(dplyr)
p_load(igraph)

setwd("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix")

sequence <- seq.int(min(trade$year),max(trade$year),1)

for ( i in sequence){
        filtracion <- filter(trade,year==i)
        country_from <- c(as.character(filtracion$importer1),as.character(filtracion$importer2))
        country_to <- c(as.character(filtracion$importer2),as.character(filtracion$importer1))
        trade_weight <- c(filtracion$flow1,filtracion$flow2)
        d <- data.frame(from=country_from, to=country_to, weight=trade_weight)
        g <- graph_from_data_frame(d, directed = TRUE, vertices = NULL)
        ad_matrix <- get.adjacency(g, attr="weight", sparse = F)
        file_name <- paste(c("trade_year_",i,".csv"), collapse = "")
        print(i)
        write.csv(ad_matrix,file_name)
}

########################################################
# Merge Trade-COW y National Material Capabilities-COW
########################################################

cat("\014")
rm(list=ls())
graphics.off()

## ---- data:loadings ----
# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# load National Material Capabilities-COW
p_load(foreign)
mnc.d <- read.dta("/Users/hectorbahamonde/research/Bahamonde_Kovac/NMC_5_0.dta") 


# load Trade-COW
p_load(foreign)
trade.d <- read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/National_COW_4.0.csv") 

# merge
cow.d = merge(mnc.d, trade.d, by=c("ccode","year"))

# subset variables
cow.d <- cow.d[c("ccode", "year", "milex", "milper", "irst", "pec")]

# Format Country Name
country = data.frame(
        statename = trade.d$statename,
        ccode = trade.d$ccode,
        year = trade.d$year
)
cow.d = merge(cow.d, country, by = c("ccode", "year"))
cow.d$statename <- as.character(cow.d$statename)


# rename time-ID and panel-ID vars.
colnames(cow.d)[which(names(cow.d) == "statename")] <- "ID"
colnames(cow.d)[which(names(cow.d) == "year")] <- "Time"


# reorder time-ID and panel-ID vars.
cow.d = subset(cow.d,  select=c("ID", "Time","milper", "irst")) # complete var names: "statename", "year", "milex", "milper", "irst", "pec"
cow.d$ID <- gsub(' ', '.', cow.d$ID) # replace blank space with dot "" just like in full.info.countries.first.time.span
cow.d$ID <- gsub('-', '.', cow.d$ID) # replace blank space with dot "" just like in full.info.countries.first.time.span

################################################################################################################
## First Period: 1871 to 1913
################################################################################################################

# Filter complete obs by year; here it's where I split the datasets
cow.d.1 <- subset(cow.d, Time >= 1871 & Time <= 1913)

# Drop NAs
cow.d.1$milper[cow.d.1$milper == -9] <- 0
cow.d.1$irst[cow.d.1$irst ==  -9] <- 0

# Filter countries for which we have complete data
p_load(dplyr)
cow.d.1 = cow.d.1 %>% dplyr::group_by(ID) %>% filter(milper != 0 & irst != 0)

# Drop countries for which we don't have the complete series
full.info.countries.first.time.span = unique(cow.d.1[cow.d.1$ID %in% names(which(table(cow.d.1$ID) == as.numeric(length(1871:1913)))), ]$ID) # Countries for which we have complete rows

# Filtering complete obs by country name
cow.d.1 = data.frame(cow.d.1[cow.d.1$ID %in% full.info.countries.first.time.span,])

# Reformat time variable
cow.d.1$Time = as.character(cow.d.1$Time)
cow.d.1$Time = as.Date(cow.d.1$Time,"%Y")
cow.d.1$Time <- as.POSIXct(cow.d.1$Time, origin="1871", tz = "GMT",  tryFormats ="%Y", optional = T)

# Sort df by country name and time
cow.d.1 = cow.d.1[with(cow.d.1, order(ID, Time)),]
rownames(cow.d.1) <- NULL

## Checking if panels are balanced
# p_load(plm)
# plm::is.pbalanced(cow.d.1)    


# Import CVSs
y.1871 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1871.csv"))
y.1872 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1872.csv"))
y.1873 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1873.csv"))
y.1874 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1874.csv"))
y.1875 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1875.csv"))
y.1876 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1876.csv"))
y.1877 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1877.csv"))
y.1878 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1878.csv"))
y.1879 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1879.csv"))
y.1880 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1880.csv"))
y.1881 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1881.csv"))
y.1882 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1882.csv"))
y.1883 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1883.csv"))
y.1884 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1884.csv"))
y.1885 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1885.csv"))
y.1886 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1886.csv"))
y.1887 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1887.csv"))
y.1888 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1888.csv"))
y.1889 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1889.csv"))
y.1890 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1890.csv"))
y.1891 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1891.csv"))
y.1892 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1892.csv"))
y.1893 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1893.csv"))
y.1894 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1894.csv"))
y.1895 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1895.csv"))
y.1896 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1896.csv"))
y.1897 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1897.csv"))
y.1898 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1898.csv"))
y.1899 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1899.csv"))
y.1900 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1900.csv"))
y.1901 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1901.csv"))
y.1902 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1902.csv"))
y.1903 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1903.csv"))
y.1904 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1904.csv"))
y.1905 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1905.csv"))
y.1906 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1906.csv"))
y.1907 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1907.csv"))
y.1908 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1908.csv"))
y.1909 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1909.csv"))
y.1910 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1910.csv"))
y.1911 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1911.csv"))
y.1912 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1912.csv"))
y.1913 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1913.csv"))


# Standarize country names, select columns, then rows, and then delete X column
p_load(dplyr)
# test = y.1871
# test$X <- gsub(' ', '.', test$X);test$X <- gsub('-', '.', test$X);test = test %>% select(c(X, full.info.countries.first.time.span));test = test[test$X %in% full.info.countries.first.time.span,];test$X <- NULL;rownames(test) <- NULL 


y.1871$X <- gsub(' ', '.', y.1871$X);y.1871$X <- gsub('-', '.', y.1871$X);y.1871 = y.1871 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1871 = y.1871[y.1871$X %in% full.info.countries.first.time.span,];y.1871$X <- NULL;rownames(y.1871) <- NULL 
y.1872$X <- gsub(' ', '.', y.1872$X);y.1872$X <- gsub('-', '.', y.1872$X);y.1872 = y.1872 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1872 = y.1872[y.1872$X %in% full.info.countries.first.time.span,];y.1872$X <- NULL;rownames(y.1872) <- NULL 
y.1873$X <- gsub(' ', '.', y.1873$X);y.1873$X <- gsub('-', '.', y.1873$X);y.1873 = y.1873 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1873 = y.1873[y.1873$X %in% full.info.countries.first.time.span,];y.1873$X <- NULL;rownames(y.1873) <- NULL 
y.1874$X <- gsub(' ', '.', y.1874$X);y.1874$X <- gsub('-', '.', y.1874$X);y.1874 = y.1874 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1874 = y.1874[y.1874$X %in% full.info.countries.first.time.span,];y.1874$X <- NULL;rownames(y.1874) <- NULL 
y.1875$X <- gsub(' ', '.', y.1875$X);y.1875$X <- gsub('-', '.', y.1875$X);y.1875 = y.1875 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1875 = y.1875[y.1875$X %in% full.info.countries.first.time.span,];y.1875$X <- NULL;rownames(y.1875) <- NULL 
y.1876$X <- gsub(' ', '.', y.1876$X);y.1876$X <- gsub('-', '.', y.1876$X);y.1876 = y.1876 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1876 = y.1876[y.1876$X %in% full.info.countries.first.time.span,];y.1876$X <- NULL;rownames(y.1876) <- NULL 
y.1877$X <- gsub(' ', '.', y.1877$X);y.1877$X <- gsub('-', '.', y.1877$X);y.1877 = y.1877 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1877 = y.1877[y.1877$X %in% full.info.countries.first.time.span,];y.1877$X <- NULL;rownames(y.1877) <- NULL 
y.1878$X <- gsub(' ', '.', y.1878$X);y.1878$X <- gsub('-', '.', y.1878$X);y.1878 = y.1878 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1878 = y.1878[y.1878$X %in% full.info.countries.first.time.span,];y.1878$X <- NULL;rownames(y.1878) <- NULL 
y.1879$X <- gsub(' ', '.', y.1879$X);y.1879$X <- gsub('-', '.', y.1879$X);y.1879 = y.1879 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1879 = y.1879[y.1879$X %in% full.info.countries.first.time.span,];y.1879$X <- NULL;rownames(y.1879) <- NULL 
y.1880$X <- gsub(' ', '.', y.1880$X);y.1880$X <- gsub('-', '.', y.1880$X);y.1880 = y.1880 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1880 = y.1880[y.1880$X %in% full.info.countries.first.time.span,];y.1880$X <- NULL;rownames(y.1880) <- NULL 
y.1881$X <- gsub(' ', '.', y.1881$X);y.1881$X <- gsub('-', '.', y.1881$X);y.1881 = y.1881 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1881 = y.1881[y.1881$X %in% full.info.countries.first.time.span,];y.1881$X <- NULL;rownames(y.1881) <- NULL 
y.1882$X <- gsub(' ', '.', y.1882$X);y.1882$X <- gsub('-', '.', y.1882$X);y.1882 = y.1882 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1882 = y.1882[y.1882$X %in% full.info.countries.first.time.span,];y.1882$X <- NULL;rownames(y.1882) <- NULL 
y.1883$X <- gsub(' ', '.', y.1883$X);y.1883$X <- gsub('-', '.', y.1883$X);y.1883 = y.1883 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1883 = y.1883[y.1883$X %in% full.info.countries.first.time.span,];y.1883$X <- NULL;rownames(y.1883) <- NULL 
y.1884$X <- gsub(' ', '.', y.1884$X);y.1884$X <- gsub('-', '.', y.1884$X);y.1884 = y.1884 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1884 = y.1884[y.1884$X %in% full.info.countries.first.time.span,];y.1884$X <- NULL;rownames(y.1884) <- NULL 
y.1885$X <- gsub(' ', '.', y.1885$X);y.1885$X <- gsub('-', '.', y.1885$X);y.1885 = y.1885 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1885 = y.1885[y.1885$X %in% full.info.countries.first.time.span,];y.1885$X <- NULL;rownames(y.1885) <- NULL 
y.1886$X <- gsub(' ', '.', y.1886$X);y.1886$X <- gsub('-', '.', y.1886$X);y.1886 = y.1886 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1886 = y.1886[y.1886$X %in% full.info.countries.first.time.span,];y.1886$X <- NULL;rownames(y.1886) <- NULL 
y.1887$X <- gsub(' ', '.', y.1887$X);y.1887$X <- gsub('-', '.', y.1887$X);y.1887 = y.1887 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1887 = y.1887[y.1887$X %in% full.info.countries.first.time.span,];y.1887$X <- NULL;rownames(y.1887) <- NULL 
y.1888$X <- gsub(' ', '.', y.1888$X);y.1888$X <- gsub('-', '.', y.1888$X);y.1888 = y.1888 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1888 = y.1888[y.1888$X %in% full.info.countries.first.time.span,];y.1888$X <- NULL;rownames(y.1888) <- NULL 
y.1889$X <- gsub(' ', '.', y.1889$X);y.1889$X <- gsub('-', '.', y.1889$X);y.1889 = y.1889 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1889 = y.1889[y.1889$X %in% full.info.countries.first.time.span,];y.1889$X <- NULL;rownames(y.1889) <- NULL 
y.1890$X <- gsub(' ', '.', y.1890$X);y.1890$X <- gsub('-', '.', y.1890$X);y.1890 = y.1890 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1890 = y.1890[y.1890$X %in% full.info.countries.first.time.span,];y.1890$X <- NULL;rownames(y.1890) <- NULL 
y.1891$X <- gsub(' ', '.', y.1891$X);y.1891$X <- gsub('-', '.', y.1891$X);y.1891 = y.1891 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1891 = y.1891[y.1891$X %in% full.info.countries.first.time.span,];y.1891$X <- NULL;rownames(y.1891) <- NULL 
y.1892$X <- gsub(' ', '.', y.1892$X);y.1892$X <- gsub('-', '.', y.1892$X);y.1892 = y.1892 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1892 = y.1892[y.1892$X %in% full.info.countries.first.time.span,];y.1892$X <- NULL;rownames(y.1892) <- NULL 
y.1893$X <- gsub(' ', '.', y.1893$X);y.1893$X <- gsub('-', '.', y.1893$X);y.1893 = y.1893 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1893 = y.1893[y.1893$X %in% full.info.countries.first.time.span,];y.1893$X <- NULL;rownames(y.1893) <- NULL 
y.1894$X <- gsub(' ', '.', y.1894$X);y.1894$X <- gsub('-', '.', y.1894$X);y.1894 = y.1894 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1894 = y.1894[y.1894$X %in% full.info.countries.first.time.span,];y.1894$X <- NULL;rownames(y.1894) <- NULL 
y.1895$X <- gsub(' ', '.', y.1895$X);y.1895$X <- gsub('-', '.', y.1895$X);y.1895 = y.1895 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1895 = y.1895[y.1895$X %in% full.info.countries.first.time.span,];y.1895$X <- NULL;rownames(y.1895) <- NULL 
y.1896$X <- gsub(' ', '.', y.1896$X);y.1896$X <- gsub('-', '.', y.1896$X);y.1896 = y.1896 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1896 = y.1896[y.1896$X %in% full.info.countries.first.time.span,];y.1896$X <- NULL;rownames(y.1896) <- NULL 
y.1897$X <- gsub(' ', '.', y.1897$X);y.1897$X <- gsub('-', '.', y.1897$X);y.1897 = y.1897 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1897 = y.1897[y.1897$X %in% full.info.countries.first.time.span,];y.1897$X <- NULL;rownames(y.1897) <- NULL 
y.1898$X <- gsub(' ', '.', y.1898$X);y.1898$X <- gsub('-', '.', y.1898$X);y.1898 = y.1898 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1898 = y.1898[y.1898$X %in% full.info.countries.first.time.span,];y.1898$X <- NULL;rownames(y.1898) <- NULL 
y.1899$X <- gsub(' ', '.', y.1899$X);y.1899$X <- gsub('-', '.', y.1899$X);y.1899 = y.1899 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1899 = y.1899[y.1899$X %in% full.info.countries.first.time.span,];y.1899$X <- NULL;rownames(y.1899) <- NULL 
y.1900$X <- gsub(' ', '.', y.1900$X);y.1900$X <- gsub('-', '.', y.1900$X);y.1900 = y.1900 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1900 = y.1900[y.1900$X %in% full.info.countries.first.time.span,];y.1900$X <- NULL;rownames(y.1900) <- NULL 
y.1901$X <- gsub(' ', '.', y.1901$X);y.1901$X <- gsub('-', '.', y.1901$X);y.1901 = y.1901 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1901 = y.1901[y.1901$X %in% full.info.countries.first.time.span,];y.1901$X <- NULL;rownames(y.1901) <- NULL 
y.1902$X <- gsub(' ', '.', y.1902$X);y.1902$X <- gsub('-', '.', y.1902$X);y.1902 = y.1902 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1902 = y.1902[y.1902$X %in% full.info.countries.first.time.span,];y.1902$X <- NULL;rownames(y.1902) <- NULL 
y.1903$X <- gsub(' ', '.', y.1903$X);y.1903$X <- gsub('-', '.', y.1903$X);y.1903 = y.1903 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1903 = y.1903[y.1903$X %in% full.info.countries.first.time.span,];y.1903$X <- NULL;rownames(y.1903) <- NULL 
y.1904$X <- gsub(' ', '.', y.1904$X);y.1904$X <- gsub('-', '.', y.1904$X);y.1904 = y.1904 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1904 = y.1904[y.1904$X %in% full.info.countries.first.time.span,];y.1904$X <- NULL;rownames(y.1904) <- NULL 
y.1905$X <- gsub(' ', '.', y.1905$X);y.1905$X <- gsub('-', '.', y.1905$X);y.1905 = y.1905 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1905 = y.1905[y.1905$X %in% full.info.countries.first.time.span,];y.1905$X <- NULL;rownames(y.1905) <- NULL 
y.1906$X <- gsub(' ', '.', y.1906$X);y.1906$X <- gsub('-', '.', y.1906$X);y.1906 = y.1906 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1906 = y.1906[y.1906$X %in% full.info.countries.first.time.span,];y.1906$X <- NULL;rownames(y.1906) <- NULL 
y.1907$X <- gsub(' ', '.', y.1907$X);y.1907$X <- gsub('-', '.', y.1907$X);y.1907 = y.1907 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1907 = y.1907[y.1907$X %in% full.info.countries.first.time.span,];y.1907$X <- NULL;rownames(y.1907) <- NULL 
y.1908$X <- gsub(' ', '.', y.1908$X);y.1908$X <- gsub('-', '.', y.1908$X);y.1908 = y.1908 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1908 = y.1908[y.1908$X %in% full.info.countries.first.time.span,];y.1908$X <- NULL;rownames(y.1908) <- NULL 
y.1909$X <- gsub(' ', '.', y.1909$X);y.1909$X <- gsub('-', '.', y.1909$X);y.1909 = y.1909 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1909 = y.1909[y.1909$X %in% full.info.countries.first.time.span,];y.1909$X <- NULL;rownames(y.1909) <- NULL 
y.1910$X <- gsub(' ', '.', y.1910$X);y.1910$X <- gsub('-', '.', y.1910$X);y.1910 = y.1910 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1910 = y.1910[y.1910$X %in% full.info.countries.first.time.span,];y.1910$X <- NULL;rownames(y.1910) <- NULL 
y.1911$X <- gsub(' ', '.', y.1911$X);y.1911$X <- gsub('-', '.', y.1911$X);y.1911 = y.1911 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1911 = y.1911[y.1911$X %in% full.info.countries.first.time.span,];y.1911$X <- NULL;rownames(y.1911) <- NULL 
y.1912$X <- gsub(' ', '.', y.1912$X);y.1912$X <- gsub('-', '.', y.1912$X);y.1912 = y.1912 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1912 = y.1912[y.1912$X %in% full.info.countries.first.time.span,];y.1912$X <- NULL;rownames(y.1912) <- NULL 
y.1913$X <- gsub(' ', '.', y.1913$X);y.1913$X <- gsub('-', '.', y.1913$X);y.1913 = y.1913 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1913 = y.1913[y.1913$X %in% full.info.countries.first.time.span,];y.1913$X <- NULL;rownames(y.1913) <- NULL 



# Building WM for the first period
wm.1 = list(as.matrix(y.1871), as.matrix(y.1872), as.matrix(y.1873), as.matrix(y.1874), as.matrix(y.1875), as.matrix(y.1876), as.matrix(y.1877), as.matrix(y.1878), as.matrix(y.1879), as.matrix(y.1880), as.matrix(y.1881), as.matrix(y.1882), as.matrix(y.1883), as.matrix(y.1884), as.matrix(y.1885), as.matrix(y.1886), as.matrix(y.1887), as.matrix(y.1888), as.matrix(y.1889), as.matrix(y.1890), as.matrix(y.1891), as.matrix(y.1892), as.matrix(y.1893), as.matrix(y.1894), as.matrix(y.1895), as.matrix(y.1896), as.matrix(y.1897), as.matrix(y.1898), as.matrix(y.1899), as.matrix(y.1900), as.matrix(y.1901), as.matrix(y.1902), as.matrix(y.1903), as.matrix(y.1904), as.matrix(y.1905), as.matrix(y.1906), as.matrix(y.1907), as.matrix(y.1908), as.matrix(y.1909), as.matrix(y.1910), as.matrix(y.1911), as.matrix(y.1912), as.matrix(y.1913))


# stores years
year.min.t1 = as.character(format(as.Date(min(cow.d.1$Time), "%Y", tz = "GMT"),"%Y"))
year.max.t1 = as.character(format(as.Date(max(cow.d.1$Time), "%Y", tz = "GMT"),"%Y"))


################################################################################################################
## Second Period: 1955 - 2014 (Minor Countries)
################################################################################################################

# Filter complete obs by year; here it's where I split the datasets
cow.d.2 <- subset(cow.d, Time >= 1955 & Time <= 2014) # 2014

# Drop NAs
cow.d.2$milper[cow.d.2$milper == -9] <- NA
cow.d.2$irst[cow.d.2$irst ==  -9] <- NA

# Filter countries for which we have complete data
cow.d.2 = cow.d.2 %>% group_by(ID) %>% filter(milper != 0 & irst != 0)

# Drop countries for which we don't have the complete series
full.info.countries.second.time.span = unique(cow.d.2[cow.d.2$ID %in% names(which(table(cow.d.2$ID) == max(table(cow.d.2$ID)))), ]$ID) # Countries for which we have complete rows

# Exclude Luxembourg (it's got too many similar values year after year)
#full.info.countries.second.time.span <- full.info.countries.second.time.span[full.info.countries.second.time.span != "Luxembourg"]
#full.info.countries.second.time.span <- full.info.countries.second.time.span[full.info.countries.second.time.span != "Taiwan"]
#full.info.countries.second.time.span <- full.info.countries.second.time.span[full.info.countries.second.time.span != "North.Korea"]

full.info.countries.second.time.span = full.info.countries.second.time.span[c(-5, -21, -29)] # ALL COUNTRIES EXCEPT: 5. US, 21. Russia, 29. China # WORKING!

# test = max(table(cow.d.2$ID)) ; View(test) # Germanies appear 36 times. Consider do GVAR for both of them.

# Filtering complete obs by country name
cow.d.2 = data.frame(cow.d.2[cow.d.2$ID %in% full.info.countries.second.time.span,])

# TEST: introduce stochastic noise ## Truncated normal with mean 0
p_load(truncnorm)
set.seed(2020); cow.d.2$milper = round(cow.d.2$milper + rtruncnorm(n=nrow(cow.d.2), a=-3, b=3, mean=0, sd=1), 2) # Mean 0
set.seed(2019); cow.d.2$irst = round(cow.d.2$irst + rtruncnorm(n=nrow(cow.d.2), a=-3, b=3, mean=0, sd=1), 2) # Mean 0

# Reformat time variable
cow.d.2$Time = as.character(cow.d.2$Time)
cow.d.2$Time = as.Date(cow.d.2$Time,"%Y")
cow.d.2$Time <- as.POSIXct(cow.d.2$Time, origin=min(cow.d.2$Time), tz = "GMT",  tryFormats ="%Y", optional = T)

# Sort df by country name and time
cow.d.2 = cow.d.2[with(cow.d.2, order(ID, Time)),]
rownames(cow.d.2) <- NULL


# stores years
year.min.t2 = as.character(format(as.Date(min(cow.d.2$Time), "%Y", tz = "GMT"),"%Y"))
year.max.t2 = as.character(format(as.Date(max(cow.d.2$Time), "%Y", tz = "GMT"),"%Y"))


## Checking if panels are balanced
# p_load(plm)
# plm::is.pbalanced(cow.d.2)    


# Import CVSs
y.1955 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1955.csv"))
y.1956 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1956.csv"))
y.1957 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1957.csv"))
y.1958 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1958.csv"))
y.1959 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1959.csv"))
y.1960 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1960.csv"))
y.1961 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1961.csv"))
y.1962 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1962.csv"))
y.1963 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1963.csv"))
y.1964 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1964.csv"))
y.1965 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1965.csv"))
y.1966 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1966.csv"))
y.1967 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1967.csv"))
y.1968 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1968.csv"))
y.1969 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1969.csv"))
y.1970 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1970.csv"))
y.1971 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1971.csv"))
y.1972 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1972.csv"))
y.1973 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1973.csv"))
y.1974 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1974.csv"))
y.1975 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1975.csv"))
y.1976 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1976.csv"))
y.1977 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1977.csv"))
y.1978 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1978.csv"))
y.1979 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1979.csv"))
y.1980 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1980.csv"))
y.1981 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1981.csv"))
y.1982 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1982.csv"))
y.1983 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1983.csv"))
y.1984 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1984.csv"))
y.1985 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1985.csv"))
y.1986 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1986.csv"))
y.1987 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1987.csv"))
y.1988 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1988.csv"))
y.1989 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1989.csv"))
y.1990 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1990.csv"))
y.1991 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1991.csv"))
y.1992 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1992.csv"))
y.1993 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1993.csv"))
y.1994 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1994.csv"))
y.1995 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1995.csv"))
y.1996 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1996.csv"))
y.1997 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1997.csv"))
y.1998 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1998.csv"))
y.1999 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1999.csv"))
y.2000 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2000.csv"))
y.2001 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2001.csv"))
y.2002 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2002.csv"))
y.2003 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2003.csv"))
y.2004 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2004.csv"))
y.2005 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2005.csv"))
y.2006 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2006.csv"))
y.2007 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2007.csv"))
y.2008 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2008.csv"))
y.2009 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2009.csv"))
y.2010 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2010.csv"))
y.2011 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2011.csv"))
y.2012 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2012.csv"))
y.2013 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2013.csv"))
y.2014 = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2014.csv"))

# Standarize country names, select columns, then rows, and then delete X column
p_load(dplyr,truncnorm);set.seed(2020)

y.1955$X <- gsub(' ', '.', y.1955$X);y.1955$X <- gsub('-', '.', y.1955$X);y.1955 = y.1955[y.1955$X %in% full.info.countries.second.time.span,];y.1955 = y.1955 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1955 <- y.1955[order(y.1955$X),];y.1955$X <- NULL;rownames(y.1955) <- NULL; y.1955 <- y.1955+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1956$X <- gsub(' ', '.', y.1956$X);y.1956$X <- gsub('-', '.', y.1956$X);y.1956 = y.1956[y.1956$X %in% full.info.countries.second.time.span,];y.1956 = y.1956 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1956 <- y.1956[order(y.1956$X),];y.1956$X <- NULL;rownames(y.1956) <- NULL; y.1956 <- y.1956+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1957$X <- gsub(' ', '.', y.1957$X);y.1957$X <- gsub('-', '.', y.1957$X);y.1957 = y.1957[y.1957$X %in% full.info.countries.second.time.span,];y.1957 = y.1957 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1957 <- y.1957[order(y.1957$X),];y.1957$X <- NULL;rownames(y.1957) <- NULL; y.1957 <- y.1957+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1958$X <- gsub(' ', '.', y.1958$X);y.1958$X <- gsub('-', '.', y.1958$X);y.1958 = y.1958[y.1958$X %in% full.info.countries.second.time.span,];y.1958 = y.1958 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1958 <- y.1958[order(y.1958$X),];y.1958$X <- NULL;rownames(y.1958) <- NULL; y.1958 <- y.1958+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1959$X <- gsub(' ', '.', y.1959$X);y.1959$X <- gsub('-', '.', y.1959$X);y.1959 = y.1959[y.1959$X %in% full.info.countries.second.time.span,];y.1959 = y.1959 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1959 <- y.1959[order(y.1959$X),];y.1959$X <- NULL;rownames(y.1959) <- NULL; y.1959 <- y.1959+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1960$X <- gsub(' ', '.', y.1960$X);y.1960$X <- gsub('-', '.', y.1960$X);y.1960 = y.1960[y.1960$X %in% full.info.countries.second.time.span,];y.1960 = y.1960 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1960 <- y.1960[order(y.1960$X),];y.1960$X <- NULL;rownames(y.1960) <- NULL; y.1960 <- y.1960+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1961$X <- gsub(' ', '.', y.1961$X);y.1961$X <- gsub('-', '.', y.1961$X);y.1961 = y.1961[y.1961$X %in% full.info.countries.second.time.span,];y.1961 = y.1961 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1961 <- y.1961[order(y.1961$X),];y.1961$X <- NULL;rownames(y.1961) <- NULL; y.1961 <- y.1961+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1962$X <- gsub(' ', '.', y.1962$X);y.1962$X <- gsub('-', '.', y.1962$X);y.1962 = y.1962[y.1962$X %in% full.info.countries.second.time.span,];y.1962 = y.1962 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1962 <- y.1962[order(y.1962$X),];y.1962$X <- NULL;rownames(y.1962) <- NULL; y.1962 <- y.1962+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1963$X <- gsub(' ', '.', y.1963$X);y.1963$X <- gsub('-', '.', y.1963$X);y.1963 = y.1963[y.1963$X %in% full.info.countries.second.time.span,];y.1963 = y.1963 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1963 <- y.1963[order(y.1963$X),];y.1963$X <- NULL;rownames(y.1963) <- NULL; y.1963 <- y.1963+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1964$X <- gsub(' ', '.', y.1964$X);y.1964$X <- gsub('-', '.', y.1964$X);y.1964 = y.1964[y.1964$X %in% full.info.countries.second.time.span,];y.1964 = y.1964 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1964 <- y.1964[order(y.1964$X),];y.1964$X <- NULL;rownames(y.1964) <- NULL; y.1964 <- y.1964+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1965$X <- gsub(' ', '.', y.1965$X);y.1965$X <- gsub('-', '.', y.1965$X);y.1965 = y.1965[y.1965$X %in% full.info.countries.second.time.span,];y.1965 = y.1965 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1965 <- y.1965[order(y.1965$X),];y.1965$X <- NULL;rownames(y.1965) <- NULL; y.1965 <- y.1965+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1966$X <- gsub(' ', '.', y.1966$X);y.1966$X <- gsub('-', '.', y.1966$X);y.1966 = y.1966[y.1966$X %in% full.info.countries.second.time.span,];y.1966 = y.1966 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1966 <- y.1966[order(y.1966$X),];y.1966$X <- NULL;rownames(y.1966) <- NULL; y.1966 <- y.1966+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1967$X <- gsub(' ', '.', y.1967$X);y.1967$X <- gsub('-', '.', y.1967$X);y.1967 = y.1967[y.1967$X %in% full.info.countries.second.time.span,];y.1967 = y.1967 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1967 <- y.1967[order(y.1967$X),];y.1967$X <- NULL;rownames(y.1967) <- NULL; y.1967 <- y.1967+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1968$X <- gsub(' ', '.', y.1968$X);y.1968$X <- gsub('-', '.', y.1968$X);y.1968 = y.1968[y.1968$X %in% full.info.countries.second.time.span,];y.1968 = y.1968 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1968 <- y.1968[order(y.1968$X),];y.1968$X <- NULL;rownames(y.1968) <- NULL; y.1968 <- y.1968+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1969$X <- gsub(' ', '.', y.1969$X);y.1969$X <- gsub('-', '.', y.1969$X);y.1969 = y.1969[y.1969$X %in% full.info.countries.second.time.span,];y.1969 = y.1969 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1969 <- y.1969[order(y.1969$X),];y.1969$X <- NULL;rownames(y.1969) <- NULL; y.1969 <- y.1969+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1970$X <- gsub(' ', '.', y.1970$X);y.1970$X <- gsub('-', '.', y.1970$X);y.1970 = y.1970[y.1970$X %in% full.info.countries.second.time.span,];y.1970 = y.1970 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1970 <- y.1970[order(y.1970$X),];y.1970$X <- NULL;rownames(y.1970) <- NULL; y.1970 <- y.1970+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1971$X <- gsub(' ', '.', y.1971$X);y.1971$X <- gsub('-', '.', y.1971$X);y.1971 = y.1971[y.1971$X %in% full.info.countries.second.time.span,];y.1971 = y.1971 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1971 <- y.1971[order(y.1971$X),];y.1971$X <- NULL;rownames(y.1971) <- NULL; y.1971 <- y.1971+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1972$X <- gsub(' ', '.', y.1972$X);y.1972$X <- gsub('-', '.', y.1972$X);y.1972 = y.1972[y.1972$X %in% full.info.countries.second.time.span,];y.1972 = y.1972 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1972 <- y.1972[order(y.1972$X),];y.1972$X <- NULL;rownames(y.1972) <- NULL; y.1972 <- y.1972+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1973$X <- gsub(' ', '.', y.1973$X);y.1973$X <- gsub('-', '.', y.1973$X);y.1973 = y.1973[y.1973$X %in% full.info.countries.second.time.span,];y.1973 = y.1973 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1973 <- y.1973[order(y.1973$X),];y.1973$X <- NULL;rownames(y.1973) <- NULL; y.1973 <- y.1973+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1974$X <- gsub(' ', '.', y.1974$X);y.1974$X <- gsub('-', '.', y.1974$X);y.1974 = y.1974[y.1974$X %in% full.info.countries.second.time.span,];y.1974 = y.1974 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1974 <- y.1974[order(y.1974$X),];y.1974$X <- NULL;rownames(y.1974) <- NULL; y.1974 <- y.1974+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1975$X <- gsub(' ', '.', y.1975$X);y.1975$X <- gsub('-', '.', y.1975$X);y.1975 = y.1975[y.1975$X %in% full.info.countries.second.time.span,];y.1975 = y.1975 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1975 <- y.1975[order(y.1975$X),];y.1975$X <- NULL;rownames(y.1975) <- NULL; y.1975 <- y.1975+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1976$X <- gsub(' ', '.', y.1976$X);y.1976$X <- gsub('-', '.', y.1976$X);y.1976 = y.1976[y.1976$X %in% full.info.countries.second.time.span,];y.1976 = y.1976 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1976 <- y.1976[order(y.1976$X),];y.1976$X <- NULL;rownames(y.1976) <- NULL; y.1976 <- y.1976+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1977$X <- gsub(' ', '.', y.1977$X);y.1977$X <- gsub('-', '.', y.1977$X);y.1977 = y.1977[y.1977$X %in% full.info.countries.second.time.span,];y.1977 = y.1977 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1977 <- y.1977[order(y.1977$X),];y.1977$X <- NULL;rownames(y.1977) <- NULL; y.1977 <- y.1977+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1978$X <- gsub(' ', '.', y.1978$X);y.1978$X <- gsub('-', '.', y.1978$X);y.1978 = y.1978[y.1978$X %in% full.info.countries.second.time.span,];y.1978 = y.1978 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1978 <- y.1978[order(y.1978$X),];y.1978$X <- NULL;rownames(y.1978) <- NULL; y.1978 <- y.1978+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1979$X <- gsub(' ', '.', y.1979$X);y.1979$X <- gsub('-', '.', y.1979$X);y.1979 = y.1979[y.1979$X %in% full.info.countries.second.time.span,];y.1979 = y.1979 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1979 <- y.1979[order(y.1979$X),];y.1979$X <- NULL;rownames(y.1979) <- NULL; y.1979 <- y.1979+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1980$X <- gsub(' ', '.', y.1980$X);y.1980$X <- gsub('-', '.', y.1980$X);y.1980 = y.1980[y.1980$X %in% full.info.countries.second.time.span,];y.1980 = y.1980 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1980 <- y.1980[order(y.1980$X),];y.1980$X <- NULL;rownames(y.1980) <- NULL; y.1980 <- y.1980+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1981$X <- gsub(' ', '.', y.1981$X);y.1981$X <- gsub('-', '.', y.1981$X);y.1981 = y.1981[y.1981$X %in% full.info.countries.second.time.span,];y.1981 = y.1981 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1981 <- y.1981[order(y.1981$X),];y.1981$X <- NULL;rownames(y.1981) <- NULL; y.1981 <- y.1981+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1982$X <- gsub(' ', '.', y.1982$X);y.1982$X <- gsub('-', '.', y.1982$X);y.1982 = y.1982[y.1982$X %in% full.info.countries.second.time.span,];y.1982 = y.1982 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1982 <- y.1982[order(y.1982$X),];y.1982$X <- NULL;rownames(y.1982) <- NULL; y.1982 <- y.1982+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1983$X <- gsub(' ', '.', y.1983$X);y.1983$X <- gsub('-', '.', y.1983$X);y.1983 = y.1983[y.1983$X %in% full.info.countries.second.time.span,];y.1983 = y.1983 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1983 <- y.1983[order(y.1983$X),];y.1983$X <- NULL;rownames(y.1983) <- NULL; y.1983 <- y.1983+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1984$X <- gsub(' ', '.', y.1984$X);y.1984$X <- gsub('-', '.', y.1984$X);y.1984 = y.1984[y.1984$X %in% full.info.countries.second.time.span,];y.1984 = y.1984 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1984 <- y.1984[order(y.1984$X),];y.1984$X <- NULL;rownames(y.1984) <- NULL; y.1984 <- y.1984+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1985$X <- gsub(' ', '.', y.1985$X);y.1985$X <- gsub('-', '.', y.1985$X);y.1985 = y.1985[y.1985$X %in% full.info.countries.second.time.span,];y.1985 = y.1985 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1985 <- y.1985[order(y.1985$X),];y.1985$X <- NULL;rownames(y.1985) <- NULL; y.1985 <- y.1985+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1986$X <- gsub(' ', '.', y.1986$X);y.1986$X <- gsub('-', '.', y.1986$X);y.1986 = y.1986[y.1986$X %in% full.info.countries.second.time.span,];y.1986 = y.1986 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1986 <- y.1986[order(y.1986$X),];y.1986$X <- NULL;rownames(y.1986) <- NULL; y.1986 <- y.1986+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1987$X <- gsub(' ', '.', y.1987$X);y.1987$X <- gsub('-', '.', y.1987$X);y.1987 = y.1987[y.1987$X %in% full.info.countries.second.time.span,];y.1987 = y.1987 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1987 <- y.1987[order(y.1987$X),];y.1987$X <- NULL;rownames(y.1987) <- NULL; y.1987 <- y.1987+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1988$X <- gsub(' ', '.', y.1988$X);y.1988$X <- gsub('-', '.', y.1988$X);y.1988 = y.1988[y.1988$X %in% full.info.countries.second.time.span,];y.1988 = y.1988 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1988 <- y.1988[order(y.1988$X),];y.1988$X <- NULL;rownames(y.1988) <- NULL; y.1988 <- y.1988+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1989$X <- gsub(' ', '.', y.1989$X);y.1989$X <- gsub('-', '.', y.1989$X);y.1989 = y.1989[y.1989$X %in% full.info.countries.second.time.span,];y.1989 = y.1989 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1989 <- y.1989[order(y.1989$X),];y.1989$X <- NULL;rownames(y.1989) <- NULL; y.1989 <- y.1989+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1990$X <- gsub(' ', '.', y.1990$X);y.1990$X <- gsub('-', '.', y.1990$X);y.1990 = y.1990[y.1990$X %in% full.info.countries.second.time.span,];y.1990 = y.1990 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1990 <- y.1990[order(y.1990$X),];y.1990$X <- NULL;rownames(y.1990) <- NULL; y.1990 <- y.1990+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1991$X <- gsub(' ', '.', y.1991$X);y.1991$X <- gsub('-', '.', y.1991$X);y.1991 = y.1991[y.1991$X %in% full.info.countries.second.time.span,];y.1991 = y.1991 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1991 <- y.1991[order(y.1991$X),];y.1991$X <- NULL;rownames(y.1991) <- NULL; y.1991 <- y.1991+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1992$X <- gsub(' ', '.', y.1992$X);y.1992$X <- gsub('-', '.', y.1992$X);y.1992 = y.1992[y.1992$X %in% full.info.countries.second.time.span,];y.1992 = y.1992 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1992 <- y.1992[order(y.1992$X),];y.1992$X <- NULL;rownames(y.1992) <- NULL; y.1992 <- y.1992+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1993$X <- gsub(' ', '.', y.1993$X);y.1993$X <- gsub('-', '.', y.1993$X);y.1993 = y.1993[y.1993$X %in% full.info.countries.second.time.span,];y.1993 = y.1993 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1993 <- y.1993[order(y.1993$X),];y.1993$X <- NULL;rownames(y.1993) <- NULL; y.1993 <- y.1993+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1994$X <- gsub(' ', '.', y.1994$X);y.1994$X <- gsub('-', '.', y.1994$X);y.1994 = y.1994[y.1994$X %in% full.info.countries.second.time.span,];y.1994 = y.1994 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1994 <- y.1994[order(y.1994$X),];y.1994$X <- NULL;rownames(y.1994) <- NULL; y.1994 <- y.1994+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1995$X <- gsub(' ', '.', y.1995$X);y.1995$X <- gsub('-', '.', y.1995$X);y.1995 = y.1995[y.1995$X %in% full.info.countries.second.time.span,];y.1995 = y.1995 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1995 <- y.1995[order(y.1995$X),];y.1995$X <- NULL;rownames(y.1995) <- NULL; y.1995 <- y.1995+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1996$X <- gsub(' ', '.', y.1996$X);y.1996$X <- gsub('-', '.', y.1996$X);y.1996 = y.1996[y.1996$X %in% full.info.countries.second.time.span,];y.1996 = y.1996 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1996 <- y.1996[order(y.1996$X),];y.1996$X <- NULL;rownames(y.1996) <- NULL; y.1996 <- y.1996+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1997$X <- gsub(' ', '.', y.1997$X);y.1997$X <- gsub('-', '.', y.1997$X);y.1997 = y.1997[y.1997$X %in% full.info.countries.second.time.span,];y.1997 = y.1997 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1997 <- y.1997[order(y.1997$X),];y.1997$X <- NULL;rownames(y.1997) <- NULL; y.1997 <- y.1997+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1998$X <- gsub(' ', '.', y.1998$X);y.1998$X <- gsub('-', '.', y.1998$X);y.1998 = y.1998[y.1998$X %in% full.info.countries.second.time.span,];y.1998 = y.1998 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1998 <- y.1998[order(y.1998$X),];y.1998$X <- NULL;rownames(y.1998) <- NULL; y.1998 <- y.1998+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.1999$X <- gsub(' ', '.', y.1999$X);y.1999$X <- gsub('-', '.', y.1999$X);y.1999 = y.1999[y.1999$X %in% full.info.countries.second.time.span,];y.1999 = y.1999 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1999 <- y.1999[order(y.1999$X),];y.1999$X <- NULL;rownames(y.1999) <- NULL; y.1999 <- y.1999+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2000$X <- gsub(' ', '.', y.2000$X);y.2000$X <- gsub('-', '.', y.2000$X);y.2000 = y.2000[y.2000$X %in% full.info.countries.second.time.span,];y.2000 = y.2000 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2000 <- y.2000[order(y.2000$X),];y.2000$X <- NULL;rownames(y.2000) <- NULL; y.2000 <- y.2000+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2001$X <- gsub(' ', '.', y.2001$X);y.2001$X <- gsub('-', '.', y.2001$X);y.2001 = y.2001[y.2001$X %in% full.info.countries.second.time.span,];y.2001 = y.2001 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2001 <- y.2001[order(y.2001$X),];y.2001$X <- NULL;rownames(y.2001) <- NULL; y.2001 <- y.2001+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2002$X <- gsub(' ', '.', y.2002$X);y.2002$X <- gsub('-', '.', y.2002$X);y.2002 = y.2002[y.2002$X %in% full.info.countries.second.time.span,];y.2002 = y.2002 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2002 <- y.2002[order(y.2002$X),];y.2002$X <- NULL;rownames(y.2002) <- NULL; y.2002 <- y.2002+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2003$X <- gsub(' ', '.', y.2003$X);y.2003$X <- gsub('-', '.', y.2003$X);y.2003 = y.2003[y.2003$X %in% full.info.countries.second.time.span,];y.2003 = y.2003 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2003 <- y.2003[order(y.2003$X),];y.2003$X <- NULL;rownames(y.2003) <- NULL; y.2003 <- y.2003+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2004$X <- gsub(' ', '.', y.2004$X);y.2004$X <- gsub('-', '.', y.2004$X);y.2004 = y.2004[y.2004$X %in% full.info.countries.second.time.span,];y.2004 = y.2004 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2004 <- y.2004[order(y.2004$X),];y.2004$X <- NULL;rownames(y.2004) <- NULL; y.2004 <- y.2004+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2005$X <- gsub(' ', '.', y.2005$X);y.2005$X <- gsub('-', '.', y.2005$X);y.2005 = y.2005[y.2005$X %in% full.info.countries.second.time.span,];y.2005 = y.2005 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2005 <- y.2005[order(y.2005$X),];y.2005$X <- NULL;rownames(y.2005) <- NULL; y.2005 <- y.2005+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2006$X <- gsub(' ', '.', y.2006$X);y.2006$X <- gsub('-', '.', y.2006$X);y.2006 = y.2006[y.2006$X %in% full.info.countries.second.time.span,];y.2006 = y.2006 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2006 <- y.2006[order(y.2006$X),];y.2006$X <- NULL;rownames(y.2006) <- NULL; y.2006 <- y.2006+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2007$X <- gsub(' ', '.', y.2007$X);y.2007$X <- gsub('-', '.', y.2007$X);y.2007 = y.2007[y.2007$X %in% full.info.countries.second.time.span,];y.2007 = y.2007 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2007 <- y.2007[order(y.2007$X),];y.2007$X <- NULL;rownames(y.2007) <- NULL; y.2007 <- y.2007+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2008$X <- gsub(' ', '.', y.2008$X);y.2008$X <- gsub('-', '.', y.2008$X);y.2008 = y.2008[y.2008$X %in% full.info.countries.second.time.span,];y.2008 = y.2008 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2008 <- y.2008[order(y.2008$X),];y.2008$X <- NULL;rownames(y.2008) <- NULL; y.2008 <- y.2008+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2009$X <- gsub(' ', '.', y.2009$X);y.2009$X <- gsub('-', '.', y.2009$X);y.2009 = y.2009[y.2009$X %in% full.info.countries.second.time.span,];y.2009 = y.2009 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2009 <- y.2009[order(y.2009$X),];y.2009$X <- NULL;rownames(y.2009) <- NULL; y.2009 <- y.2009+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2010$X <- gsub(' ', '.', y.2010$X);y.2010$X <- gsub('-', '.', y.2010$X);y.2010 = y.2010[y.2010$X %in% full.info.countries.second.time.span,];y.2010 = y.2010 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2010 <- y.2010[order(y.2010$X),];y.2010$X <- NULL;rownames(y.2010) <- NULL; y.2010 <- y.2010+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2011$X <- gsub(' ', '.', y.2011$X);y.2011$X <- gsub('-', '.', y.2011$X);y.2011 = y.2011[y.2011$X %in% full.info.countries.second.time.span,];y.2011 = y.2011 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2011 <- y.2011[order(y.2011$X),];y.2011$X <- NULL;rownames(y.2011) <- NULL; y.2011 <- y.2011+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2012$X <- gsub(' ', '.', y.2012$X);y.2012$X <- gsub('-', '.', y.2012$X);y.2012 = y.2012[y.2012$X %in% full.info.countries.second.time.span,];y.2012 = y.2012 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2012 <- y.2012[order(y.2012$X),];y.2012$X <- NULL;rownames(y.2012) <- NULL; y.2012 <- y.2012+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2013$X <- gsub(' ', '.', y.2013$X);y.2013$X <- gsub('-', '.', y.2013$X);y.2013 = y.2013[y.2013$X %in% full.info.countries.second.time.span,];y.2013 = y.2013 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2013 <- y.2013[order(y.2013$X),];y.2013$X <- NULL;rownames(y.2013) <- NULL; y.2013 <- y.2013+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
y.2014$X <- gsub(' ', '.', y.2014$X);y.2014$X <- gsub('-', '.', y.2014$X);y.2014 = y.2014[y.2014$X %in% full.info.countries.second.time.span,];y.2014 = y.2014 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2014 <- y.2014[order(y.2014$X),];y.2014$X <- NULL;rownames(y.2014) <- NULL; y.2014 <- y.2014+round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)


# Building WM for the second period
wm.2 = list(as.matrix(y.1955), as.matrix(y.1956), as.matrix(y.1957), as.matrix(y.1958), as.matrix(y.1959), as.matrix(y.1960), as.matrix(y.1961), as.matrix(y.1962), as.matrix(y.1963), as.matrix(y.1964), as.matrix(y.1965), as.matrix(y.1966), as.matrix(y.1967), as.matrix(y.1968), as.matrix(y.1969), as.matrix(y.1970), as.matrix(y.1971), as.matrix(y.1972), as.matrix(y.1973), as.matrix(y.1974), as.matrix(y.1975), as.matrix(y.1976), as.matrix(y.1977), as.matrix(y.1978), as.matrix(y.1979), as.matrix(y.1980), as.matrix(y.1981), as.matrix(y.1982), as.matrix(y.1983), as.matrix(y.1984), as.matrix(y.1985), as.matrix(y.1986), as.matrix(y.1987), as.matrix(y.1988), as.matrix(y.1989), as.matrix(y.1990), as.matrix(y.1991), as.matrix(y.1992), as.matrix(y.1993), as.matrix(y.1994), as.matrix(y.1995), as.matrix(y.1996), as.matrix(y.1997), as.matrix(y.1998), as.matrix(y.1999), as.matrix(y.2000), as.matrix(y.2001), as.matrix(y.2002), as.matrix(y.2003), as.matrix(y.2004), as.matrix(y.2005), as.matrix(y.2006), as.matrix(y.2007), as.matrix(y.2008), as.matrix(y.2009), as.matrix(y.2010), as.matrix(y.2011), as.matrix(y.2012), as.matrix(y.2013), as.matrix(y.2014)
)

#round(rtruncnorm(length(year.min.t2:year.max.t2), a=-3, b=3, mean=0, sd=1),3)
################################################################################################################
## Second Period: 1955 - 2014 (Big Countries)
################################################################################################################

# Filter complete obs by year; here it's where I split the datasets
cow.d.2.B <- subset(cow.d, Time >= 1955 & Time <= 2014) # 2014

# Drop NAs
cow.d.2.B$milper[cow.d.2.B$milper == -9] <- NA
cow.d.2.B$irst[cow.d.2.B$irst ==  -9] <- NA

# Filter countries for which we have complete data
cow.d.2.B = cow.d.2.B %>% group_by(ID) %>% filter(milper != 0 & irst != 0)

# Drop countries for which we don't have the complete series
full.info.countries.second.time.span.B = unique(cow.d.2.B[cow.d.2.B$ID %in% names(which(table(cow.d.2.B$ID) == max(table(cow.d.2.B$ID)))), ]$ID) # Countries for which we have complete rows

# Exclude Luxembourg (it's got too many similar values year after year)
#full.info.countries.second.time.span.B <- full.info.countries.second.time.span.B[full.info.countries.second.time.span.B != "Luxembourg"]
#full.info.countries.second.time.span.B <- full.info.countries.second.time.span.B[full.info.countries.second.time.span.B != "Taiwan"]
#full.info.countries.second.time.span.B <- full.info.countries.second.time.span.B[full.info.countries.second.time.span.B != "North.Korea"]

full.info.countries.second.time.span.B = full.info.countries.second.time.span.B[c(5, 21, 29)] # ALL COUNTRIES EXCEPT: 5. US, 21. Russia, 29. China # WORKING!

# test = max(table(cow.d.2.B$ID)) ; View(test) # Germanies appear 36 times. Consider do GVAR for both of them.

# Filtering complete obs by country name
cow.d.2.B = data.frame(cow.d.2.B[cow.d.2.B$ID %in% full.info.countries.second.time.span.B,])

# Introduce stochastic noise ## Truncated normal with mean 0
p_load(truncnorm)
set.seed(2020); cow.d.2.B$milper = round(cow.d.2.B$milper + rtruncnorm(n=nrow(cow.d.2.B), a=-3, b=3, mean=0, sd=1), 2) # Mean 0
set.seed(2019); cow.d.2.B$irst = round(cow.d.2.B$irst + rtruncnorm(n=nrow(cow.d.2.B), a=-3, b=3, mean=0, sd=1), 2) # Mean 0

# Reformat time variable
cow.d.2.B$Time = as.character(cow.d.2.B$Time)
cow.d.2.B$Time = as.Date(cow.d.2.B$Time,"%Y")
cow.d.2.B$Time <- as.POSIXct(cow.d.2.B$Time, origin=min(cow.d.2.B$Time), tz = "GMT",  tryFormats ="%Y", optional = T)

# Sort df by country name and time
cow.d.2.B = cow.d.2.B[with(cow.d.2.B, order(ID, Time)),]
rownames(cow.d.2.B) <- NULL

## Checking if panels are balanced
# p_load(plm)
# plm::is.pbalanced(cow.d.2.B)    


# Plot the data
# Pending


# Import CVSs
y.1955.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1955.csv"))
y.1956.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1956.csv"))
y.1957.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1957.csv"))
y.1958.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1958.csv"))
y.1959.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1959.csv"))
y.1960.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1960.csv"))
y.1961.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1961.csv"))
y.1962.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1962.csv"))
y.1963.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1963.csv"))
y.1964.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1964.csv"))
y.1965.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1965.csv"))
y.1966.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1966.csv"))
y.1967.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1967.csv"))
y.1968.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1968.csv"))
y.1969.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1969.csv"))
y.1970.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1970.csv"))
y.1971.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1971.csv"))
y.1972.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1972.csv"))
y.1973.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1973.csv"))
y.1974.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1974.csv"))
y.1975.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1975.csv"))
y.1976.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1976.csv"))
y.1977.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1977.csv"))
y.1978.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1978.csv"))
y.1979.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1979.csv"))
y.1980.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1980.csv"))
y.1981.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1981.csv"))
y.1982.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1982.csv"))
y.1983.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1983.csv"))
y.1984.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1984.csv"))
y.1985.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1985.csv"))
y.1986.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1986.csv"))
y.1987.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1987.csv"))
y.1988.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1988.csv"))
y.1989.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1989.csv"))
y.1990.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1990.csv"))
y.1991.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1991.csv"))
y.1992.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1992.csv"))
y.1993.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1993.csv"))
y.1994.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1994.csv"))
y.1995.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1995.csv"))
y.1996.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1996.csv"))
y.1997.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1997.csv"))
y.1998.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1998.csv"))
y.1999.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_1999.csv"))
y.2000.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2000.csv"))
y.2001.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2001.csv"))
y.2002.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2002.csv"))
y.2003.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2003.csv"))
y.2004.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2004.csv"))
y.2005.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2005.csv"))
y.2006.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2006.csv"))
y.2007.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2007.csv"))
y.2008.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2008.csv"))
y.2009.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2009.csv"))
y.2010.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2010.csv"))
y.2011.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2011.csv"))
y.2012.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2012.csv"))
y.2013.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2013.csv"))
y.2014.b = data.frame(read.csv("/Users/hectorbahamonde/research/Bahamonde_Kovac/matrix/trade_year_2014.csv"))


# Standarize country names, select columns, then rows, and then delete X column
p_load(dplyr)

y.1955.b$X <- gsub(' ', '.', y.1955.b$X);y.1955.b$X <- gsub('-', '.', y.1955.b$X);y.1955.b = y.1955.b[y.1955.b$X %in% full.info.countries.second.time.span.B,];y.1955.b = y.1955.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1955.b <- y.1955.b[order(y.1955.b$X),];y.1955.b$X <- NULL;rownames(y.1955.b) <- NULL 
y.1956.b$X <- gsub(' ', '.', y.1956.b$X);y.1956.b$X <- gsub('-', '.', y.1956.b$X);y.1956.b = y.1956.b[y.1956.b$X %in% full.info.countries.second.time.span.B,];y.1956.b = y.1956.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1956.b <- y.1956.b[order(y.1956.b$X),];y.1956.b$X <- NULL;rownames(y.1956.b) <- NULL 
y.1957.b$X <- gsub(' ', '.', y.1957.b$X);y.1957.b$X <- gsub('-', '.', y.1957.b$X);y.1957.b = y.1957.b[y.1957.b$X %in% full.info.countries.second.time.span.B,];y.1957.b = y.1957.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1957.b <- y.1957.b[order(y.1957.b$X),];y.1957.b$X <- NULL;rownames(y.1957.b) <- NULL 
y.1958.b$X <- gsub(' ', '.', y.1958.b$X);y.1958.b$X <- gsub('-', '.', y.1958.b$X);y.1958.b = y.1958.b[y.1958.b$X %in% full.info.countries.second.time.span.B,];y.1958.b = y.1958.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1958.b <- y.1958.b[order(y.1958.b$X),];y.1958.b$X <- NULL;rownames(y.1958.b) <- NULL 
y.1959.b$X <- gsub(' ', '.', y.1959.b$X);y.1959.b$X <- gsub('-', '.', y.1959.b$X);y.1959.b = y.1959.b[y.1959.b$X %in% full.info.countries.second.time.span.B,];y.1959.b = y.1959.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1959.b <- y.1959.b[order(y.1959.b$X),];y.1959.b$X <- NULL;rownames(y.1959.b) <- NULL 
y.1960.b$X <- gsub(' ', '.', y.1960.b$X);y.1960.b$X <- gsub('-', '.', y.1960.b$X);y.1960.b = y.1960.b[y.1960.b$X %in% full.info.countries.second.time.span.B,];y.1960.b = y.1960.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1960.b <- y.1960.b[order(y.1960.b$X),];y.1960.b$X <- NULL;rownames(y.1960.b) <- NULL 
y.1961.b$X <- gsub(' ', '.', y.1961.b$X);y.1961.b$X <- gsub('-', '.', y.1961.b$X);y.1961.b = y.1961.b[y.1961.b$X %in% full.info.countries.second.time.span.B,];y.1961.b = y.1961.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1961.b <- y.1961.b[order(y.1961.b$X),];y.1961.b$X <- NULL;rownames(y.1961.b) <- NULL 
y.1962.b$X <- gsub(' ', '.', y.1962.b$X);y.1962.b$X <- gsub('-', '.', y.1962.b$X);y.1962.b = y.1962.b[y.1962.b$X %in% full.info.countries.second.time.span.B,];y.1962.b = y.1962.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1962.b <- y.1962.b[order(y.1962.b$X),];y.1962.b$X <- NULL;rownames(y.1962.b) <- NULL 
y.1963.b$X <- gsub(' ', '.', y.1963.b$X);y.1963.b$X <- gsub('-', '.', y.1963.b$X);y.1963.b = y.1963.b[y.1963.b$X %in% full.info.countries.second.time.span.B,];y.1963.b = y.1963.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1963.b <- y.1963.b[order(y.1963.b$X),];y.1963.b$X <- NULL;rownames(y.1963.b) <- NULL 
y.1964.b$X <- gsub(' ', '.', y.1964.b$X);y.1964.b$X <- gsub('-', '.', y.1964.b$X);y.1964.b = y.1964.b[y.1964.b$X %in% full.info.countries.second.time.span.B,];y.1964.b = y.1964.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1964.b <- y.1964.b[order(y.1964.b$X),];y.1964.b$X <- NULL;rownames(y.1964.b) <- NULL 
y.1965.b$X <- gsub(' ', '.', y.1965.b$X);y.1965.b$X <- gsub('-', '.', y.1965.b$X);y.1965.b = y.1965.b[y.1965.b$X %in% full.info.countries.second.time.span.B,];y.1965.b = y.1965.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1965.b <- y.1965.b[order(y.1965.b$X),];y.1965.b$X <- NULL;rownames(y.1965.b) <- NULL 
y.1966.b$X <- gsub(' ', '.', y.1966.b$X);y.1966.b$X <- gsub('-', '.', y.1966.b$X);y.1966.b = y.1966.b[y.1966.b$X %in% full.info.countries.second.time.span.B,];y.1966.b = y.1966.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1966.b <- y.1966.b[order(y.1966.b$X),];y.1966.b$X <- NULL;rownames(y.1966.b) <- NULL 
y.1967.b$X <- gsub(' ', '.', y.1967.b$X);y.1967.b$X <- gsub('-', '.', y.1967.b$X);y.1967.b = y.1967.b[y.1967.b$X %in% full.info.countries.second.time.span.B,];y.1967.b = y.1967.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1967.b <- y.1967.b[order(y.1967.b$X),];y.1967.b$X <- NULL;rownames(y.1967.b) <- NULL 
y.1968.b$X <- gsub(' ', '.', y.1968.b$X);y.1968.b$X <- gsub('-', '.', y.1968.b$X);y.1968.b = y.1968.b[y.1968.b$X %in% full.info.countries.second.time.span.B,];y.1968.b = y.1968.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1968.b <- y.1968.b[order(y.1968.b$X),];y.1968.b$X <- NULL;rownames(y.1968.b) <- NULL 
y.1969.b$X <- gsub(' ', '.', y.1969.b$X);y.1969.b$X <- gsub('-', '.', y.1969.b$X);y.1969.b = y.1969.b[y.1969.b$X %in% full.info.countries.second.time.span.B,];y.1969.b = y.1969.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1969.b <- y.1969.b[order(y.1969.b$X),];y.1969.b$X <- NULL;rownames(y.1969.b) <- NULL 
y.1970.b$X <- gsub(' ', '.', y.1970.b$X);y.1970.b$X <- gsub('-', '.', y.1970.b$X);y.1970.b = y.1970.b[y.1970.b$X %in% full.info.countries.second.time.span.B,];y.1970.b = y.1970.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1970.b <- y.1970.b[order(y.1970.b$X),];y.1970.b$X <- NULL;rownames(y.1970.b) <- NULL 
y.1971.b$X <- gsub(' ', '.', y.1971.b$X);y.1971.b$X <- gsub('-', '.', y.1971.b$X);y.1971.b = y.1971.b[y.1971.b$X %in% full.info.countries.second.time.span.B,];y.1971.b = y.1971.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1971.b <- y.1971.b[order(y.1971.b$X),];y.1971.b$X <- NULL;rownames(y.1971.b) <- NULL 
y.1972.b$X <- gsub(' ', '.', y.1972.b$X);y.1972.b$X <- gsub('-', '.', y.1972.b$X);y.1972.b = y.1972.b[y.1972.b$X %in% full.info.countries.second.time.span.B,];y.1972.b = y.1972.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1972.b <- y.1972.b[order(y.1972.b$X),];y.1972.b$X <- NULL;rownames(y.1972.b) <- NULL 
y.1973.b$X <- gsub(' ', '.', y.1973.b$X);y.1973.b$X <- gsub('-', '.', y.1973.b$X);y.1973.b = y.1973.b[y.1973.b$X %in% full.info.countries.second.time.span.B,];y.1973.b = y.1973.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1973.b <- y.1973.b[order(y.1973.b$X),];y.1973.b$X <- NULL;rownames(y.1973.b) <- NULL 
y.1974.b$X <- gsub(' ', '.', y.1974.b$X);y.1974.b$X <- gsub('-', '.', y.1974.b$X);y.1974.b = y.1974.b[y.1974.b$X %in% full.info.countries.second.time.span.B,];y.1974.b = y.1974.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1974.b <- y.1974.b[order(y.1974.b$X),];y.1974.b$X <- NULL;rownames(y.1974.b) <- NULL 
y.1975.b$X <- gsub(' ', '.', y.1975.b$X);y.1975.b$X <- gsub('-', '.', y.1975.b$X);y.1975.b = y.1975.b[y.1975.b$X %in% full.info.countries.second.time.span.B,];y.1975.b = y.1975.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1975.b <- y.1975.b[order(y.1975.b$X),];y.1975.b$X <- NULL;rownames(y.1975.b) <- NULL 
y.1976.b$X <- gsub(' ', '.', y.1976.b$X);y.1976.b$X <- gsub('-', '.', y.1976.b$X);y.1976.b = y.1976.b[y.1976.b$X %in% full.info.countries.second.time.span.B,];y.1976.b = y.1976.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1976.b <- y.1976.b[order(y.1976.b$X),];y.1976.b$X <- NULL;rownames(y.1976.b) <- NULL 
y.1977.b$X <- gsub(' ', '.', y.1977.b$X);y.1977.b$X <- gsub('-', '.', y.1977.b$X);y.1977.b = y.1977.b[y.1977.b$X %in% full.info.countries.second.time.span.B,];y.1977.b = y.1977.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1977.b <- y.1977.b[order(y.1977.b$X),];y.1977.b$X <- NULL;rownames(y.1977.b) <- NULL 
y.1978.b$X <- gsub(' ', '.', y.1978.b$X);y.1978.b$X <- gsub('-', '.', y.1978.b$X);y.1978.b = y.1978.b[y.1978.b$X %in% full.info.countries.second.time.span.B,];y.1978.b = y.1978.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1978.b <- y.1978.b[order(y.1978.b$X),];y.1978.b$X <- NULL;rownames(y.1978.b) <- NULL 
y.1979.b$X <- gsub(' ', '.', y.1979.b$X);y.1979.b$X <- gsub('-', '.', y.1979.b$X);y.1979.b = y.1979.b[y.1979.b$X %in% full.info.countries.second.time.span.B,];y.1979.b = y.1979.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1979.b <- y.1979.b[order(y.1979.b$X),];y.1979.b$X <- NULL;rownames(y.1979.b) <- NULL 
y.1980.b$X <- gsub(' ', '.', y.1980.b$X);y.1980.b$X <- gsub('-', '.', y.1980.b$X);y.1980.b = y.1980.b[y.1980.b$X %in% full.info.countries.second.time.span.B,];y.1980.b = y.1980.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1980.b <- y.1980.b[order(y.1980.b$X),];y.1980.b$X <- NULL;rownames(y.1980.b) <- NULL 
y.1981.b$X <- gsub(' ', '.', y.1981.b$X);y.1981.b$X <- gsub('-', '.', y.1981.b$X);y.1981.b = y.1981.b[y.1981.b$X %in% full.info.countries.second.time.span.B,];y.1981.b = y.1981.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1981.b <- y.1981.b[order(y.1981.b$X),];y.1981.b$X <- NULL;rownames(y.1981.b) <- NULL 
y.1982.b$X <- gsub(' ', '.', y.1982.b$X);y.1982.b$X <- gsub('-', '.', y.1982.b$X);y.1982.b = y.1982.b[y.1982.b$X %in% full.info.countries.second.time.span.B,];y.1982.b = y.1982.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1982.b <- y.1982.b[order(y.1982.b$X),];y.1982.b$X <- NULL;rownames(y.1982.b) <- NULL 
y.1983.b$X <- gsub(' ', '.', y.1983.b$X);y.1983.b$X <- gsub('-', '.', y.1983.b$X);y.1983.b = y.1983.b[y.1983.b$X %in% full.info.countries.second.time.span.B,];y.1983.b = y.1983.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1983.b <- y.1983.b[order(y.1983.b$X),];y.1983.b$X <- NULL;rownames(y.1983.b) <- NULL 
y.1984.b$X <- gsub(' ', '.', y.1984.b$X);y.1984.b$X <- gsub('-', '.', y.1984.b$X);y.1984.b = y.1984.b[y.1984.b$X %in% full.info.countries.second.time.span.B,];y.1984.b = y.1984.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1984.b <- y.1984.b[order(y.1984.b$X),];y.1984.b$X <- NULL;rownames(y.1984.b) <- NULL 
y.1985.b$X <- gsub(' ', '.', y.1985.b$X);y.1985.b$X <- gsub('-', '.', y.1985.b$X);y.1985.b = y.1985.b[y.1985.b$X %in% full.info.countries.second.time.span.B,];y.1985.b = y.1985.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1985.b <- y.1985.b[order(y.1985.b$X),];y.1985.b$X <- NULL;rownames(y.1985.b) <- NULL 
y.1986.b$X <- gsub(' ', '.', y.1986.b$X);y.1986.b$X <- gsub('-', '.', y.1986.b$X);y.1986.b = y.1986.b[y.1986.b$X %in% full.info.countries.second.time.span.B,];y.1986.b = y.1986.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1986.b <- y.1986.b[order(y.1986.b$X),];y.1986.b$X <- NULL;rownames(y.1986.b) <- NULL 
y.1987.b$X <- gsub(' ', '.', y.1987.b$X);y.1987.b$X <- gsub('-', '.', y.1987.b$X);y.1987.b = y.1987.b[y.1987.b$X %in% full.info.countries.second.time.span.B,];y.1987.b = y.1987.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1987.b <- y.1987.b[order(y.1987.b$X),];y.1987.b$X <- NULL;rownames(y.1987.b) <- NULL 
y.1988.b$X <- gsub(' ', '.', y.1988.b$X);y.1988.b$X <- gsub('-', '.', y.1988.b$X);y.1988.b = y.1988.b[y.1988.b$X %in% full.info.countries.second.time.span.B,];y.1988.b = y.1988.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1988.b <- y.1988.b[order(y.1988.b$X),];y.1988.b$X <- NULL;rownames(y.1988.b) <- NULL 
y.1989.b$X <- gsub(' ', '.', y.1989.b$X);y.1989.b$X <- gsub('-', '.', y.1989.b$X);y.1989.b = y.1989.b[y.1989.b$X %in% full.info.countries.second.time.span.B,];y.1989.b = y.1989.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1989.b <- y.1989.b[order(y.1989.b$X),];y.1989.b$X <- NULL;rownames(y.1989.b) <- NULL 
y.1990.b$X <- gsub(' ', '.', y.1990.b$X);y.1990.b$X <- gsub('-', '.', y.1990.b$X);y.1990.b = y.1990.b[y.1990.b$X %in% full.info.countries.second.time.span.B,];y.1990.b = y.1990.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1990.b <- y.1990.b[order(y.1990.b$X),];y.1990.b$X <- NULL;rownames(y.1990.b) <- NULL 
y.1991.b$X <- gsub(' ', '.', y.1991.b$X);y.1991.b$X <- gsub('-', '.', y.1991.b$X);y.1991.b = y.1991.b[y.1991.b$X %in% full.info.countries.second.time.span.B,];y.1991.b = y.1991.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1991.b <- y.1991.b[order(y.1991.b$X),];y.1991.b$X <- NULL;rownames(y.1991.b) <- NULL 
y.1992.b$X <- gsub(' ', '.', y.1992.b$X);y.1992.b$X <- gsub('-', '.', y.1992.b$X);y.1992.b = y.1992.b[y.1992.b$X %in% full.info.countries.second.time.span.B,];y.1992.b = y.1992.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1992.b <- y.1992.b[order(y.1992.b$X),];y.1992.b$X <- NULL;rownames(y.1992.b) <- NULL 
y.1993.b$X <- gsub(' ', '.', y.1993.b$X);y.1993.b$X <- gsub('-', '.', y.1993.b$X);y.1993.b = y.1993.b[y.1993.b$X %in% full.info.countries.second.time.span.B,];y.1993.b = y.1993.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1993.b <- y.1993.b[order(y.1993.b$X),];y.1993.b$X <- NULL;rownames(y.1993.b) <- NULL 
y.1994.b$X <- gsub(' ', '.', y.1994.b$X);y.1994.b$X <- gsub('-', '.', y.1994.b$X);y.1994.b = y.1994.b[y.1994.b$X %in% full.info.countries.second.time.span.B,];y.1994.b = y.1994.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1994.b <- y.1994.b[order(y.1994.b$X),];y.1994.b$X <- NULL;rownames(y.1994.b) <- NULL 
y.1995.b$X <- gsub(' ', '.', y.1995.b$X);y.1995.b$X <- gsub('-', '.', y.1995.b$X);y.1995.b = y.1995.b[y.1995.b$X %in% full.info.countries.second.time.span.B,];y.1995.b = y.1995.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1995.b <- y.1995.b[order(y.1995.b$X),];y.1995.b$X <- NULL;rownames(y.1995.b) <- NULL 
y.1996.b$X <- gsub(' ', '.', y.1996.b$X);y.1996.b$X <- gsub('-', '.', y.1996.b$X);y.1996.b = y.1996.b[y.1996.b$X %in% full.info.countries.second.time.span.B,];y.1996.b = y.1996.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1996.b <- y.1996.b[order(y.1996.b$X),];y.1996.b$X <- NULL;rownames(y.1996.b) <- NULL 
y.1997.b$X <- gsub(' ', '.', y.1997.b$X);y.1997.b$X <- gsub('-', '.', y.1997.b$X);y.1997.b = y.1997.b[y.1997.b$X %in% full.info.countries.second.time.span.B,];y.1997.b = y.1997.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1997.b <- y.1997.b[order(y.1997.b$X),];y.1997.b$X <- NULL;rownames(y.1997.b) <- NULL 
y.1998.b$X <- gsub(' ', '.', y.1998.b$X);y.1998.b$X <- gsub('-', '.', y.1998.b$X);y.1998.b = y.1998.b[y.1998.b$X %in% full.info.countries.second.time.span.B,];y.1998.b = y.1998.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1998.b <- y.1998.b[order(y.1998.b$X),];y.1998.b$X <- NULL;rownames(y.1998.b) <- NULL 
y.1999.b$X <- gsub(' ', '.', y.1999.b$X);y.1999.b$X <- gsub('-', '.', y.1999.b$X);y.1999.b = y.1999.b[y.1999.b$X %in% full.info.countries.second.time.span.B,];y.1999.b = y.1999.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1999.b <- y.1999.b[order(y.1999.b$X),];y.1999.b$X <- NULL;rownames(y.1999.b) <- NULL 
y.2000.b$X <- gsub(' ', '.', y.2000.b$X);y.2000.b$X <- gsub('-', '.', y.2000.b$X);y.2000.b = y.2000.b[y.2000.b$X %in% full.info.countries.second.time.span.B,];y.2000.b = y.2000.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2000.b <- y.2000.b[order(y.2000.b$X),];y.2000.b$X <- NULL;rownames(y.2000.b) <- NULL 
y.2001.b$X <- gsub(' ', '.', y.2001.b$X);y.2001.b$X <- gsub('-', '.', y.2001.b$X);y.2001.b = y.2001.b[y.2001.b$X %in% full.info.countries.second.time.span.B,];y.2001.b = y.2001.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2001.b <- y.2001.b[order(y.2001.b$X),];y.2001.b$X <- NULL;rownames(y.2001.b) <- NULL 
y.2002.b$X <- gsub(' ', '.', y.2002.b$X);y.2002.b$X <- gsub('-', '.', y.2002.b$X);y.2002.b = y.2002.b[y.2002.b$X %in% full.info.countries.second.time.span.B,];y.2002.b = y.2002.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2002.b <- y.2002.b[order(y.2002.b$X),];y.2002.b$X <- NULL;rownames(y.2002.b) <- NULL 
y.2003.b$X <- gsub(' ', '.', y.2003.b$X);y.2003.b$X <- gsub('-', '.', y.2003.b$X);y.2003.b = y.2003.b[y.2003.b$X %in% full.info.countries.second.time.span.B,];y.2003.b = y.2003.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2003.b <- y.2003.b[order(y.2003.b$X),];y.2003.b$X <- NULL;rownames(y.2003.b) <- NULL 
y.2004.b$X <- gsub(' ', '.', y.2004.b$X);y.2004.b$X <- gsub('-', '.', y.2004.b$X);y.2004.b = y.2004.b[y.2004.b$X %in% full.info.countries.second.time.span.B,];y.2004.b = y.2004.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2004.b <- y.2004.b[order(y.2004.b$X),];y.2004.b$X <- NULL;rownames(y.2004.b) <- NULL 
y.2005.b$X <- gsub(' ', '.', y.2005.b$X);y.2005.b$X <- gsub('-', '.', y.2005.b$X);y.2005.b = y.2005.b[y.2005.b$X %in% full.info.countries.second.time.span.B,];y.2005.b = y.2005.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2005.b <- y.2005.b[order(y.2005.b$X),];y.2005.b$X <- NULL;rownames(y.2005.b) <- NULL 
y.2006.b$X <- gsub(' ', '.', y.2006.b$X);y.2006.b$X <- gsub('-', '.', y.2006.b$X);y.2006.b = y.2006.b[y.2006.b$X %in% full.info.countries.second.time.span.B,];y.2006.b = y.2006.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2006.b <- y.2006.b[order(y.2006.b$X),];y.2006.b$X <- NULL;rownames(y.2006.b) <- NULL 
y.2007.b$X <- gsub(' ', '.', y.2007.b$X);y.2007.b$X <- gsub('-', '.', y.2007.b$X);y.2007.b = y.2007.b[y.2007.b$X %in% full.info.countries.second.time.span.B,];y.2007.b = y.2007.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2007.b <- y.2007.b[order(y.2007.b$X),];y.2007.b$X <- NULL;rownames(y.2007.b) <- NULL 
y.2008.b$X <- gsub(' ', '.', y.2008.b$X);y.2008.b$X <- gsub('-', '.', y.2008.b$X);y.2008.b = y.2008.b[y.2008.b$X %in% full.info.countries.second.time.span.B,];y.2008.b = y.2008.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2008.b <- y.2008.b[order(y.2008.b$X),];y.2008.b$X <- NULL;rownames(y.2008.b) <- NULL 
y.2009.b$X <- gsub(' ', '.', y.2009.b$X);y.2009.b$X <- gsub('-', '.', y.2009.b$X);y.2009.b = y.2009.b[y.2009.b$X %in% full.info.countries.second.time.span.B,];y.2009.b = y.2009.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2009.b <- y.2009.b[order(y.2009.b$X),];y.2009.b$X <- NULL;rownames(y.2009.b) <- NULL 
y.2010.b$X <- gsub(' ', '.', y.2010.b$X);y.2010.b$X <- gsub('-', '.', y.2010.b$X);y.2010.b = y.2010.b[y.2010.b$X %in% full.info.countries.second.time.span.B,];y.2010.b = y.2010.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2010.b <- y.2010.b[order(y.2010.b$X),];y.2010.b$X <- NULL;rownames(y.2010.b) <- NULL 
y.2011.b$X <- gsub(' ', '.', y.2011.b$X);y.2011.b$X <- gsub('-', '.', y.2011.b$X);y.2011.b = y.2011.b[y.2011.b$X %in% full.info.countries.second.time.span.B,];y.2011.b = y.2011.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2011.b <- y.2011.b[order(y.2011.b$X),];y.2011.b$X <- NULL;rownames(y.2011.b) <- NULL 
y.2012.b$X <- gsub(' ', '.', y.2012.b$X);y.2012.b$X <- gsub('-', '.', y.2012.b$X);y.2012.b = y.2012.b[y.2012.b$X %in% full.info.countries.second.time.span.B,];y.2012.b = y.2012.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2012.b <- y.2012.b[order(y.2012.b$X),];y.2012.b$X <- NULL;rownames(y.2012.b) <- NULL 
y.2013.b$X <- gsub(' ', '.', y.2013.b$X);y.2013.b$X <- gsub('-', '.', y.2013.b$X);y.2013.b = y.2013.b[y.2013.b$X %in% full.info.countries.second.time.span.B,];y.2013.b = y.2013.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2013.b <- y.2013.b[order(y.2013.b$X),];y.2013.b$X <- NULL;rownames(y.2013.b) <- NULL 
y.2014.b$X <- gsub(' ', '.', y.2014.b$X);y.2014.b$X <- gsub('-', '.', y.2014.b$X);y.2014.b = y.2014.b[y.2014.b$X %in% full.info.countries.second.time.span.B,];y.2014.b = y.2014.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2014.b <- y.2014.b[order(y.2014.b$X),];y.2014.b$X <- NULL;rownames(y.2014.b) <- NULL 


# Building WM for the second period
wm.2.b = list(as.matrix(y.1955.b), as.matrix(y.1956.b), as.matrix(y.1957.b), as.matrix(y.1958.b), as.matrix(y.1959.b), as.matrix(y.1960.b), as.matrix(y.1961.b), as.matrix(y.1962.b), as.matrix(y.1963.b), as.matrix(y.1964.b), as.matrix(y.1965.b), as.matrix(y.1966.b), as.matrix(y.1967.b), as.matrix(y.1968.b), as.matrix(y.1969.b), as.matrix(y.1970.b), as.matrix(y.1971.b), as.matrix(y.1972.b), as.matrix(y.1973.b), as.matrix(y.1974.b), as.matrix(y.1975.b), as.matrix(y.1976.b), as.matrix(y.1977.b), as.matrix(y.1978.b), as.matrix(y.1979.b), as.matrix(y.1980.b), as.matrix(y.1981.b), as.matrix(y.1982.b), as.matrix(y.1983.b), as.matrix(y.1984.b), as.matrix(y.1985.b), as.matrix(y.1986.b), as.matrix(y.1987.b), as.matrix(y.1988.b), as.matrix(y.1989.b), as.matrix(y.1990.b), as.matrix(y.1991.b), as.matrix(y.1992.b), as.matrix(y.1993.b), as.matrix(y.1994.b), as.matrix(y.1995.b), as.matrix(y.1996.b), as.matrix(y.1997.b), as.matrix(y.1998.b), as.matrix(y.1999.b), as.matrix(y.2000.b), as.matrix(y.2001.b), as.matrix(y.2002.b), as.matrix(y.2003.b), as.matrix(y.2004.b), as.matrix(y.2005.b), as.matrix(y.2006.b), as.matrix(y.2007.b), as.matrix(y.2008.b), as.matrix(y.2009.b), as.matrix(y.2010.b), as.matrix(y.2011.b), as.matrix(y.2012.b), as.matrix(y.2013.b), as.matrix(y.2014.b)
)
## ----


########################################################
# GVARX
########################################################

# Present results like Table 4.5 in Box-Steffensmeier2014a, 121.

# GVARX
# https://www.rdocumentation.org/packages/GVARX/versions/1.3
# http://web.ntnu.edu.tw/~tsungwu/R_DevOps/GVAR/R_DevOps_GVAR_English.html
# data("tradeweightx")

# GVAR is developed by Pesaran2004

# Be adviced, this function only computes Granger causality tests for 
# BIVARIATE specifications. Will compute different models for both var types.

## Look at the pr(F-statistic)
# "the F test has the greatest power to determine the joint statistical significance of the coefficients on the lags of the variable hypothesized to Granger cause another variable. The null of no Granger causality is equivalent to the hypothesis that all these coefficients are jointly zero." Box-Steffensmeier2014a, p. 112
# F test tests if pi parameters ar jointly zero (Freeman1983, 333).
# "tests for the joint significance of the estimated regression parameters, the ir,j's, were based on F statistic" (Freeman1983, 346)
# https://stats.stackexchange.com/questions/131261/granger-causality-interpretation-using-r/132527

########################################################
# Saving Data
########################################################

write.csv(cow.d.1,"/Users/hectorbahamonde/research/Bahamonde_Kovac/cow_1.csv", row.names = FALSE)
write.csv(cow.d.2,"/Users/hectorbahamonde/research/Bahamonde_Kovac/cow_2.csv", row.names = FALSE)
write.csv(cow.d.2.B,"/Users/hectorbahamonde/research/Bahamonde_Kovac/cow_2_B.csv", row.names = FALSE)

save(wm.1, file = "/Users/hectorbahamonde/research/Bahamonde_Kovac/wm_1.RData")
save(wm.2, file = "/Users/hectorbahamonde/research/Bahamonde_Kovac/wm_2.RData")
save(wm.2.b, file = "/Users/hectorbahamonde/research/Bahamonde_Kovac/wm_2_B.RData")


########################################################
# First Period
########################################################

## ---- gvar:model:first:period ----

p_load(GVARX)

p.1=2 # The number of lag for Xt matrix
FLag.1=2 # The number of lag for foreign variables in country-specific VAR
lag.max.1=5 # The maximal number of lag for estimating country-specific VAR
type.1="both" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.1="AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

# determinsitic component for the paper
type.1.paper = as.character(ifelse(type.1=="both", as.character("Trend and constant"), ifelse(type.1 == "trend", as.character("Trend"), ifelse(type.1 == "constant", "Constant", NA ))))

options(scipen=9999999)
mainOUTPUT.1 = GVECMest(
        data = cow.d.1,
        p = p.1,
        FLag = FLag.1,
        lag.max = lag.max.1,
        type = type.1,
        ic = ic.1,
        weight.matrix=wm.1)

# Store values

# Austria.Hungary
p.1.Austria.Hungary.f.1=summary(mainOUTPUT.1$gvecm[[1]])$varresult$Austria.Hungary.milper$fstatistic
p.1.Austria.Hungary.pvalue.1=round(pf(p.1.Austria.Hungary.f.1[1], df1=p.1.Austria.Hungary.f.1[2],df2=p.1.Austria.Hungary.f.1[3], lower.tail=FALSE), 2)

p.1.Austria.Hungary.f.2=summary(mainOUTPUT.1$gvecm[[1]])$varresult$Austria.Hungary.irst$fstatistic
p.1.Austria.Hungary.pvalue.2=round(pf(p.1.Austria.Hungary.f.2[1], df1=p.1.Austria.Hungary.f.2[2],df2=p.1.Austria.Hungary.f.2[3], lower.tail=FALSE), 2)

# Belgium
p.1.Belgium.f.1=summary(mainOUTPUT.1$gvecm[[2]])$varresult$Belgium.milper$fstatistic
p.1.Belgium.pvalue.1=round(pf(p.1.Belgium.f.1[1], df1=p.1.Belgium.f.1[2],df2=p.1.Belgium.f.1[3], lower.tail=FALSE), 2)

p.1.Belgium.f.2=summary(mainOUTPUT.1$gvecm[[2]])$varresult$Belgium.irst$fstatistic
p.1.Belgium.pvalue.2=round(pf(p.1.Belgium.f.2[1], df1=p.1.Belgium.f.2[2],df2=p.1.Belgium.f.2[3], lower.tail=FALSE), 2)

# France
p.1.France.f.1=summary(mainOUTPUT.1$gvecm[[3]])$varresult$France.milper$fstatistic
p.1.France.pvalue.1=round(pf(p.1.France.f.1[1], df1=p.1.France.f.1[2],df2=p.1.France.f.1[3], lower.tail=FALSE), 2)

p.1.France.f.2=summary(mainOUTPUT.1$gvecm[[3]])$varresult$France.irst$fstatistic
p.1.France.pvalue.2=round(pf(p.1.France.f.2[1], df1=p.1.France.f.2[2],df2=p.1.France.f.2[3], lower.tail=FALSE), 2)

# Germany
p.1.Germany.f.1=summary(mainOUTPUT.1$gvecm[[4]])$varresult$Germany.milper$fstatistic
p.1.Germany.pvalue.1=round(pf(p.1.Germany.f.1[1], df1=p.1.Germany.f.1[2],df2=p.1.Germany.f.1[3], lower.tail=FALSE), 2)

p.1.Germany.f.2=summary(mainOUTPUT.1$gvecm[[4]])$varresult$Germany.irst$fstatistic
p.1.Germany.pvalue.2=round(pf(p.1.Germany.f.2[1], df1=p.1.Germany.f.2[2],df2=p.1.Germany.f.2[3], lower.tail=FALSE), 2)

# Italy
p.1.Italy.f.1=summary(mainOUTPUT.1$gvecm[[5]])$varresult$Italy.milper$fstatistic
p.1.Italy.pvalue.1=round(pf(p.1.Italy.f.1[1], df1=p.1.Italy.f.1[2],df2=p.1.Italy.f.1[3], lower.tail=FALSE), 2)

p.1.Italy.f.2=summary(mainOUTPUT.1$gvecm[[5]])$varresult$Italy.irst$fstatistic
p.1.Italy.pvalue.2=round(pf(p.1.Italy.f.2[1], df1=p.1.Italy.f.2[2],df2=p.1.Italy.f.2[3], lower.tail=FALSE), 2)

# Russia
p.1.Russia.f.1=summary(mainOUTPUT.1$gvecm[[6]])$varresult$Russia.milper$fstatistic
p.1.Russia.pvalue.1=round(pf(p.1.Russia.f.1[1], df1=p.1.Russia.f.1[2],df2=p.1.Russia.f.1[3], lower.tail=FALSE), 2)

p.1.Russia.f.2=summary(mainOUTPUT.1$gvecm[[6]])$varresult$Russia.irst$fstatistic
p.1.Russia.pvalue.2=round(pf(p.1.Russia.f.2[1], df1=p.1.Russia.f.2[2],df2=p.1.Russia.f.2[3], lower.tail=FALSE), 2)

# Spain
p.1.Spain.f.1=summary(mainOUTPUT.1$gvecm[[7]])$varresult$Spain.milper$fstatistic
p.1.Spain.pvalue.1=round(pf(p.1.Spain.f.1[1], df1=p.1.Spain.f.1[2],df2=p.1.Spain.f.1[3], lower.tail=FALSE), 2)

p.1.Spain.f.2=summary(mainOUTPUT.1$gvecm[[7]])$varresult$Spain.irst$fstatistic
p.1.Spain.pvalue.2=round(pf(p.1.Spain.f.2[1], df1=p.1.Spain.f.2[2],df2=p.1.Spain.f.2[3], lower.tail=FALSE), 2)

# United.Kingdom
p.1.United.Kingdom.f.1=summary(mainOUTPUT.1$gvecm[[8]])$varresult$United.Kingdom.milper$fstatistic
p.1.United.Kingdom.pvalue.1=round(pf(p.1.United.Kingdom.f.1[1], df1=p.1.United.Kingdom.f.1[2],df2=p.1.United.Kingdom.f.1[3], lower.tail=FALSE), 2)

p.1.United.Kingdom.f.2=summary(mainOUTPUT.1$gvecm[[8]])$varresult$United.Kingdom.irst$fstatistic
p.1.United.Kingdom.pvalue.2=round(pf(p.1.United.Kingdom.f.2[1], df1=p.1.United.Kingdom.f.2[2],df2=p.1.United.Kingdom.f.2[3], lower.tail=FALSE), 2)

# United.States.of.America
p.1.United.States.f.1=summary(mainOUTPUT.1$gvecm[[9]])$varresult$United.States.of.America.milper$fstatistic
p.1.United.States.pvalue.1=round(pf(p.1.United.States.f.1[1], df1=p.1.United.States.f.1[2],df2=p.1.United.States.f.1[3], lower.tail=FALSE), 2)

p.1.United.States.f.2=summary(mainOUTPUT.1$gvecm[[9]])$varresult$United.States.of.America.irst$fstatistic
p.1.United.States.pvalue.2=round(pf(p.1.United.States.f.2[1], df1=p.1.United.States.f.2[2],df2=p.1.United.States.f.2[3], lower.tail=FALSE), 2)


### Austria.Hungary
## Ftests
p.1.Austria.Hungary.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.fstatistic.value"]), 3)
p.1.Austria.Hungary.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Austria.Hungary.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Austria.Hungary.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Austria.Hungary.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Austria.Hungary.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.adj.r.squared"]), 3) 
# Lags
p.1.Austria.Hungary.lags = mainOUTPUT.1$lagmatrix$lags[1] # Lag for the first country 

### Belgium
## Ftests
p.1.Belgium.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.milper.fstatistic.value"]), 3)
p.1.Belgium.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Belgium.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Belgium.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Belgium.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Belgium.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.irst.adj.r.squared"]), 3) 
# Lags
p.1.Belgium.lags = mainOUTPUT.1$lagmatrix$lags[2] # Lag for the first country 

### France
## Ftests
p.1.France.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.milper.fstatistic.value"]), 3)
p.1.France.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.France.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.France.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.France.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.France.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.irst.adj.r.squared"]), 3) 
# Lags
p.1.France.lags = mainOUTPUT.1$lagmatrix$lags[3] # Lag for the first country 

### Germany
## Ftests
p.1.Germany.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.milper.fstatistic.value"]), 3)
p.1.Germany.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Germany.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Germany.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Germany.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Germany.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.irst.adj.r.squared"]), 3) 
# Lags
p.1.Germany.lags = mainOUTPUT.1$lagmatrix$lags[4] # Lag for the first country 

### Italy
## Ftests
p.1.Italy.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.milper.fstatistic.value"]), 3)
p.1.Italy.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Italy.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Italy.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Italy.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Italy.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.irst.adj.r.squared"]), 3) 
# Lags
p.1.Italy.lags = mainOUTPUT.1$lagmatrix$lags[5] # Lag for the first country 

### Russia
## Ftests
p.1.Russia.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.milper.fstatistic.value"]), 3)
p.1.Russia.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Russia.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Russia.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Russia.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Russia.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.irst.adj.r.squared"]), 3) 
# Lags
p.1.Russia.lags = mainOUTPUT.1$lagmatrix$lags[6] # Lag for the first country 

### Spain
## Ftests
p.1.Spain.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.milper.fstatistic.value"]), 3)
p.1.Spain.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Spain.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Spain.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Spain.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Spain.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.irst.adj.r.squared"]), 3) 
# Lags
p.1.Spain.lags = mainOUTPUT.1$lagmatrix$lags[7] # Lag for the first country 

### United.Kingdom
## Ftests
p.1.United.Kingdom.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.milper.fstatistic.value"]), 3)
p.1.United.Kingdom.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.United.Kingdom.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.United.Kingdom.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.United.Kingdom.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.United.Kingdom.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.irst.adj.r.squared"]), 3) 
# Lags
p.1.United.Kingdom.lags = mainOUTPUT.1$lagmatrix$lags[8] # Lag for the first country 

### United.States
## Ftests
p.1.United.States.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.milper.fstatistic.value"]), 3)
p.1.United.States.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.United.States.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.United.States.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.United.States.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.United.States.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.irst.adj.r.squared"]), 3) 
# Lags
p.1.United.States.lags = mainOUTPUT.1$lagmatrix$lags[9] # Lag for the first country 

## ----


summary(mainOUTPUT.1$gvecm[[1]]) # Austria.Hungary 
summary(mainOUTPUT.1$gvecm[[2]]) # Belgium 
summary(mainOUTPUT.1$gvecm[[3]]) # France 
summary(mainOUTPUT.1$gvecm[[4]]) # Germany 
summary(mainOUTPUT.1$gvecm[[5]]) # Italy 
summary(mainOUTPUT.1$gvecm[[6]]) # Russia 
summary(mainOUTPUT.1$gvecm[[7]]) # Spain 
summary(mainOUTPUT.1$gvecm[[8]]) # United Kingdom  
summary(mainOUTPUT.1$gvecm[[9]]) # United States


### Others
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))[1] # VD (1)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))[2] # VD (2)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.aliased.const"] # Constant (1)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.aliased.trend"] # Trend (1)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.aliased.const"] # Constant (2)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.aliased.trend"] # Trend (2) 
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["obs"] # Obs (N)








## List of Possible Pendings

### 1. Compare the dependency of residuals in VAR and GVAR.

# Compare the dependency of residuals in VAR and GVAR.
cor2_avg = averageCORgvecm(out=mainOUTPUT.1)
as.matrix((cor2_avg$vecmRSDcor)[[1]])
as.matrix((cor2_avg$vecmRSDcor)[[2]])


## https://www.rdocumentation.org/packages/GVARX/versions/1.3/topics/averageCORgvecm
data.frame(cor2_avg$vecmRSDcor) # Average residual correlations of country-specific VECM
data.frame(cor2_avg$gvecmRSDcor) # Average residual correlations of country-specific VECM augmented by foreign variables(GVECM)

### 2. Estimate Country-Specific Johansen Test Results In A Global VECM Setting
p.1=2
FLag.1=2
type="const"
ic="SC"
weight.matrix=tradeweight1

mainOUT.JO.1 = GVECM.jo(
        data = cow.d.1,
        p = p.1,
        FLag = FLag.1, 
        weight.matrix = wm.1
)

mainOUT.JO.1$JO.test

########################################################
# Second Period (Small Countries)
########################################################

## ---- gvar:model:second:period ----
p_load(GVARX)

p.2=3 # The number of lag for Xt matrix
FLag.2=3 # The number of lag for foreign variables in country-specific VAR
lag.max.2=5 # The maximal number of lag for estimating country-specific VAR
type.2="trend" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.2="AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

options(scipen=9999999)
mainOUTPUT.2 = GVECMest(
        data = cow.d.2,
        p = p.2,
        FLag = FLag.2,
        lag.max = lag.max.2,
        type = type.2,
        ic = ic.2,
        weight.matrix=wm.2)

# determinsitic component for the paper
type.2.paper = as.character(ifelse(type.2=="both", as.character("Trend and constant"), ifelse(type.2 == "trend", as.character("Trend"), ifelse(type.2 == "constant", "Constant", NA ))))

# Storing Values
## P-values


# Argentina
p.2.Argentina.f.1=summary(mainOUTPUT.2$gvecm[[1]])$varresult$Argentina.milper$fstatistic
p.2.Argentina.pvalue.1=round(pf(p.2.Argentina.f.1[1], df1=p.2.Argentina.f.1[2],df2=p.2.Argentina.f.1[3], lower.tail=FALSE), 2)

p.2.Argentina.f.2=summary(mainOUTPUT.2$gvecm[[1]])$varresult$Argentina.irst$fstatistic
p.2.Argentina.pvalue.2=round(pf(p.2.Argentina.f.2[1], df1=p.2.Argentina.f.2[2],df2=p.2.Argentina.f.2[3], lower.tail=FALSE), 2)

# Australia
p.2.Australia.f.1=summary(mainOUTPUT.2$gvecm[[2]])$varresult$Australia.milper$fstatistic
p.2.Australia.pvalue.1=round(pf(p.2.Australia.f.1[1], df1=p.2.Australia.f.1[2],df2=p.2.Australia.f.1[3], lower.tail=FALSE), 2)

p.2.Australia.f.2=summary(mainOUTPUT.2$gvecm[[2]])$varresult$Australia.irst$fstatistic
p.2.Australia.pvalue.2=round(pf(p.2.Australia.f.2[1], df1=p.2.Australia.f.2[2],df2=p.2.Australia.f.2[3], lower.tail=FALSE), 2)

# Austria
p.2.Austria.f.1=summary(mainOUTPUT.2$gvecm[[3]])$varresult$Austria.milper$fstatistic
p.2.Austria.pvalue.1=round(pf(p.2.Austria.f.1[1], df1=p.2.Austria.f.1[2],df2=p.2.Austria.f.1[3], lower.tail=FALSE), 2)

p.2.Austria.f.2=summary(mainOUTPUT.2$gvecm[[3]])$varresult$Austria.irst$fstatistic
p.2.Austria.pvalue.2=round(pf(p.2.Austria.f.2[1], df1=p.2.Austria.f.2[2],df2=p.2.Austria.f.2[3], lower.tail=FALSE), 2)

# Belgium
p.2.Belgium.f.1=summary(mainOUTPUT.2$gvecm[[4]])$varresult$Belgium.milper$fstatistic
p.2.Belgium.pvalue.1=round(pf(p.2.Belgium.f.1[1], df1=p.2.Belgium.f.1[2],df2=p.2.Belgium.f.1[3], lower.tail=FALSE), 2)

p.2.Belgium.f.2=summary(mainOUTPUT.2$gvecm[[4]])$varresult$Belgium.irst$fstatistic
p.2.Belgium.pvalue.2=round(pf(p.2.Belgium.f.2[1], df1=p.2.Belgium.f.2[2],df2=p.2.Belgium.f.2[3], lower.tail=FALSE), 2)


# Brazil
p.2.Brazil.f.1=summary(mainOUTPUT.2$gvecm[[5]])$varresult$Brazil.milper$fstatistic
p.2.Brazil.pvalue.1=round(pf(p.2.Brazil.f.1[1], df1=p.2.Brazil.f.1[2],df2=p.2.Brazil.f.1[3], lower.tail=FALSE), 2)

p.2.Brazil.f.2=summary(mainOUTPUT.2$gvecm[[5]])$varresult$Brazil.irst$fstatistic
p.2.Brazil.pvalue.2=round(pf(p.2.Brazil.f.2[1], df1=p.2.Brazil.f.2[2],df2=p.2.Brazil.f.2[3], lower.tail=FALSE), 2)

# Bulgaria
p.2.Bulgaria.f.1=summary(mainOUTPUT.2$gvecm[[6]])$varresult$Bulgaria.milper$fstatistic
p.2.Bulgaria.pvalue.1=round(pf(p.2.Bulgaria.f.1[1], df1=p.2.Bulgaria.f.1[2],df2=p.2.Bulgaria.f.1[3], lower.tail=FALSE), 2)

p.2.Bulgaria.f.2=summary(mainOUTPUT.2$gvecm[[6]])$varresult$Bulgaria.irst$fstatistic
p.2.Bulgaria.pvalue.2=round(pf(p.2.Bulgaria.f.2[1], df1=p.2.Bulgaria.f.2[2],df2=p.2.Bulgaria.f.2[3], lower.tail=FALSE), 2)

# Canada
p.2.Canada.f.1=summary(mainOUTPUT.2$gvecm[[7]])$varresult$Canada.milper$fstatistic
p.2.Canada.pvalue.1=round(pf(p.2.Canada.f.1[1], df1=p.2.Canada.f.1[2],df2=p.2.Canada.f.1[3], lower.tail=FALSE), 2)

p.2.Canada.f.2=summary(mainOUTPUT.2$gvecm[[7]])$varresult$Canada.irst$fstatistic
p.2.Canada.pvalue.2=round(pf(p.2.Canada.f.2[1], df1=p.2.Canada.f.2[2],df2=p.2.Canada.f.2[3], lower.tail=FALSE), 2)

# Chile
p.2.Chile.f.1=summary(mainOUTPUT.2$gvecm[[8]])$varresult$Chile.milper$fstatistic
p.2.Chile.pvalue.1=round(pf(p.2.Chile.f.1[1], df1=p.2.Chile.f.1[2],df2=p.2.Chile.f.1[3], lower.tail=FALSE), 2)

p.2.Chile.f.2=summary(mainOUTPUT.2$gvecm[[8]])$varresult$Chile.irst$fstatistic
p.2.Chile.pvalue.2=round(pf(p.2.Chile.f.2[1], df1=p.2.Chile.f.2[2],df2=p.2.Chile.f.2[3], lower.tail=FALSE), 2)

# Colombia
p.2.Colombia.f.1=summary(mainOUTPUT.2$gvecm[[9]])$varresult$Colombia.milper$fstatistic
p.2.Colombia.pvalue.1=round(pf(p.2.Colombia.f.1[1], df1=p.2.Colombia.f.1[2],df2=p.2.Colombia.f.1[3], lower.tail=FALSE), 2)

p.2.Colombia.f.2=summary(mainOUTPUT.2$gvecm[[9]])$varresult$Colombia.irst$fstatistic
p.2.Colombia.pvalue.2=round(pf(p.2.Colombia.f.2[1], df1=p.2.Colombia.f.2[2],df2=p.2.Colombia.f.2[3], lower.tail=FALSE), 2)

# Egypt
p.2.Egypt.f.1=summary(mainOUTPUT.2$gvecm[[10]])$varresult$Egypt.milper$fstatistic
p.2.Egypt.pvalue.1=round(pf(p.2.Egypt.f.1[1], df1=p.2.Egypt.f.1[2],df2=p.2.Egypt.f.1[3], lower.tail=FALSE), 2)

p.2.Egypt.f.2=summary(mainOUTPUT.2$gvecm[[10]])$varresult$Egypt.irst$fstatistic
p.2.Egypt.pvalue.2=round(pf(p.2.Egypt.f.2[1], df1=p.2.Egypt.f.2[2],df2=p.2.Egypt.f.2[3], lower.tail=FALSE), 2)

# Finland
p.2.Finland.f.1=summary(mainOUTPUT.2$gvecm[[11]])$varresult$Finland.milper$fstatistic
p.2.Finland.pvalue.1=round(pf(p.2.Finland.f.1[1], df1=p.2.Finland.f.1[2],df2=p.2.Finland.f.1[3], lower.tail=FALSE), 2)

p.2.Finland.f.2=summary(mainOUTPUT.2$gvecm[[11]])$varresult$Finland.irst$fstatistic
p.2.Finland.pvalue.2=round(pf(p.2.Finland.f.2[1], df1=p.2.Finland.f.2[2],df2=p.2.Finland.f.2[3], lower.tail=FALSE), 2)

# France
p.2.France.f.1=summary(mainOUTPUT.2$gvecm[[12]])$varresult$France.milper$fstatistic
p.2.France.pvalue.1=round(pf(p.2.France.f.1[1], df1=p.2.France.f.1[2],df2=p.2.France.f.1[3], lower.tail=FALSE), 2)

p.2.France.f.2=summary(mainOUTPUT.2$gvecm[[12]])$varresult$France.irst$fstatistic
p.2.France.pvalue.2=round(pf(p.2.France.f.2[1], df1=p.2.France.f.2[2],df2=p.2.France.f.2[3], lower.tail=FALSE), 2)

# Greece
p.2.Greece.f.1=summary(mainOUTPUT.2$gvecm[[13]])$varresult$Greece.milper$fstatistic
p.2.Greece.pvalue.1=round(pf(p.2.Greece.f.1[1], df1=p.2.Greece.f.1[2],df2=p.2.Greece.f.1[3], lower.tail=FALSE), 2)

p.2.Greece.f.2=summary(mainOUTPUT.2$gvecm[[13]])$varresult$Greece.irst$fstatistic
p.2.Greece.pvalue.2=round(pf(p.2.Greece.f.2[1], df1=p.2.Greece.f.2[2],df2=p.2.Greece.f.2[3], lower.tail=FALSE), 2)

# Hungary
p.2.Hungary.f.1=summary(mainOUTPUT.2$gvecm[[14]])$varresult$Hungary.milper$fstatistic
p.2.Hungary.pvalue.1=round(pf(p.2.Hungary.f.1[1], df1=p.2.Hungary.f.1[2],df2=p.2.Hungary.f.1[3], lower.tail=FALSE), 2)

p.2.Hungary.f.2=summary(mainOUTPUT.2$gvecm[[14]])$varresult$Hungary.irst$fstatistic
p.2.Hungary.pvalue.2=round(pf(p.2.Hungary.f.2[1], df1=p.2.Hungary.f.2[2],df2=p.2.Hungary.f.2[3], lower.tail=FALSE), 2)

# India
p.2.India.f.1=summary(mainOUTPUT.2$gvecm[[15]])$varresult$India.milper$fstatistic
p.2.India.pvalue.1=round(pf(p.2.India.f.1[1], df1=p.2.India.f.1[2],df2=p.2.India.f.1[3], lower.tail=FALSE), 2)

p.2.India.f.2=summary(mainOUTPUT.2$gvecm[[15]])$varresult$India.irst$fstatistic
p.2.India.pvalue.2=round(pf(p.2.India.f.2[1], df1=p.2.India.f.2[2],df2=p.2.India.f.2[3], lower.tail=FALSE), 2)

# Israel
p.2.Israel.f.1=summary(mainOUTPUT.2$gvecm[[16]])$varresult$Israel.milper$fstatistic
p.2.Israel.pvalue.1=round(pf(p.2.Israel.f.1[1], df1=p.2.Israel.f.1[2],df2=p.2.Israel.f.1[3], lower.tail=FALSE), 2)

p.2.Israel.f.2=summary(mainOUTPUT.2$gvecm[[16]])$varresult$Israel.irst$fstatistic
p.2.Israel.pvalue.2=round(pf(p.2.Israel.f.2[1], df1=p.2.Israel.f.2[2],df2=p.2.Israel.f.2[3], lower.tail=FALSE), 2)

# Italy
p.2.Italy.f.1=summary(mainOUTPUT.2$gvecm[[17]])$varresult$Italy.milper$fstatistic
p.2.Italy.pvalue.1=round(pf(p.2.Italy.f.1[1], df1=p.2.Italy.f.1[2],df2=p.2.Italy.f.1[3], lower.tail=FALSE), 2)

p.2.Italy.f.2=summary(mainOUTPUT.2$gvecm[[17]])$varresult$Italy.irst$fstatistic
p.2.Italy.pvalue.2=round(pf(p.2.Italy.f.2[1], df1=p.2.Italy.f.2[2],df2=p.2.Italy.f.2[3], lower.tail=FALSE), 2)

# Japan
p.2.Japan.f.1=summary(mainOUTPUT.2$gvecm[[18]])$varresult$Japan.milper$fstatistic
p.2.Japan.pvalue.1=round(pf(p.2.Japan.f.1[1], df1=p.2.Japan.f.1[2],df2=p.2.Japan.f.1[3], lower.tail=FALSE), 2)

p.2.Japan.f.2=summary(mainOUTPUT.2$gvecm[[18]])$varresult$Japan.irst$fstatistic
p.2.Japan.pvalue.2=round(pf(p.2.Japan.f.2[1], df1=p.2.Japan.f.2[2],df2=p.2.Japan.f.2[3], lower.tail=FALSE), 2)

# Luxembourg
p.2.Luxembourg.f.1=summary(mainOUTPUT.2$gvecm[[19]])$varresult$Luxembourg.milper$fstatistic
p.2.Luxembourg.pvalue.1=round(pf(p.2.Luxembourg.f.1[1], df1=p.2.Luxembourg.f.1[2],df2=p.2.Luxembourg.f.1[3], lower.tail=FALSE), 2)

p.2.Luxembourg.f.2=summary(mainOUTPUT.2$gvecm[[19]])$varresult$Luxembourg.irst$fstatistic
p.2.Luxembourg.pvalue.2=round(pf(p.2.Luxembourg.f.2[1], df1=p.2.Luxembourg.f.2[2],df2=p.2.Luxembourg.f.2[3], lower.tail=FALSE), 2)

# Mexico
p.2.Mexico.f.1=summary(mainOUTPUT.2$gvecm[[20]])$varresult$Mexico.milper$fstatistic
p.2.Mexico.pvalue.1=round(pf(p.2.Mexico.f.1[1], df1=p.2.Mexico.f.1[2],df2=p.2.Mexico.f.1[3], lower.tail=FALSE), 2)

p.2.Mexico.f.2=summary(mainOUTPUT.2$gvecm[[20]])$varresult$Mexico.irst$fstatistic
p.2.Mexico.pvalue.2=round(pf(p.2.Mexico.f.2[1], df1=p.2.Mexico.f.2[2],df2=p.2.Mexico.f.2[3], lower.tail=FALSE), 2)

# Netherlands
p.2.Netherlands.f.1=summary(mainOUTPUT.2$gvecm[[21]])$varresult$Netherlands.milper$fstatistic
p.2.Netherlands.pvalue.1=round(pf(p.2.Netherlands.f.1[1], df1=p.2.Netherlands.f.1[2],df2=p.2.Netherlands.f.1[3], lower.tail=FALSE), 2)

p.2.Netherlands.f.2=summary(mainOUTPUT.2$gvecm[[21]])$varresult$Netherlands.irst$fstatistic
p.2.Netherlands.pvalue.2=round(pf(p.2.Netherlands.f.2[1], df1=p.2.Netherlands.f.2[2],df2=p.2.Netherlands.f.2[3], lower.tail=FALSE), 2)

# North.Korea
p.2.North.Korea.f.1=summary(mainOUTPUT.2$gvecm[[22]])$varresult$North.Korea.milper$fstatistic
p.2.North.Korea.pvalue.1=round(pf(p.2.North.Korea.f.1[1], df1=p.2.North.Korea.f.1[2],df2=p.2.North.Korea.f.1[3], lower.tail=FALSE), 2)

p.2.North.Korea.f.2=summary(mainOUTPUT.2$gvecm[[22]])$varresult$North.Korea.irst$fstatistic
p.2.North.Korea.pvalue.2=round(pf(p.2.North.Korea.f.2[1], df1=p.2.North.Korea.f.2[2],df2=p.2.North.Korea.f.2[3], lower.tail=FALSE), 2)

# Norway
p.2.Norway.f.1=summary(mainOUTPUT.2$gvecm[[23]])$varresult$Norway.milper$fstatistic
p.2.Norway.pvalue.1=round(pf(p.2.Norway.f.1[1], df1=p.2.Norway.f.1[2],df2=p.2.Norway.f.1[3], lower.tail=FALSE), 2)

p.2.Norway.f.2=summary(mainOUTPUT.2$gvecm[[23]])$varresult$Norway.irst$fstatistic
p.2.Norway.pvalue.2=round(pf(p.2.Norway.f.2[1], df1=p.2.Norway.f.2[2],df2=p.2.Norway.f.2[3], lower.tail=FALSE), 2)

# Poland
p.2.Poland.f.1=summary(mainOUTPUT.2$gvecm[[24]])$varresult$Poland.milper$fstatistic
p.2.Poland.pvalue.1=round(pf(p.2.Poland.f.1[1], df1=p.2.Poland.f.1[2],df2=p.2.Poland.f.1[3], lower.tail=FALSE), 2)

p.2.Poland.f.2=summary(mainOUTPUT.2$gvecm[[24]])$varresult$Poland.irst$fstatistic
p.2.Poland.pvalue.2=round(pf(p.2.Poland.f.2[1], df1=p.2.Poland.f.2[2],df2=p.2.Poland.f.2[3], lower.tail=FALSE), 2)

# Portugal
p.2.Portugal.f.1=summary(mainOUTPUT.2$gvecm[[25]])$varresult$Portugal.milper$fstatistic
p.2.Portugal.pvalue.1=round(pf(p.2.Portugal.f.1[1], df1=p.2.Portugal.f.1[2],df2=p.2.Portugal.f.1[3], lower.tail=FALSE), 2)

p.2.Portugal.f.2=summary(mainOUTPUT.2$gvecm[[25]])$varresult$Portugal.irst$fstatistic
p.2.Portugal.pvalue.2=round(pf(p.2.Portugal.f.2[1], df1=p.2.Portugal.f.2[2],df2=p.2.Portugal.f.2[3], lower.tail=FALSE), 2)

# Romania
p.2.Romania.f.1=summary(mainOUTPUT.2$gvecm[[26]])$varresult$Romania.milper$fstatistic
p.2.Romania.pvalue.1=round(pf(p.2.Romania.f.1[1], df1=p.2.Romania.f.1[2],df2=p.2.Romania.f.1[3], lower.tail=FALSE), 2)

p.2.Romania.f.2=summary(mainOUTPUT.2$gvecm[[26]])$varresult$Romania.irst$fstatistic
p.2.Romania.pvalue.2=round(pf(p.2.Romania.f.2[1], df1=p.2.Romania.f.2[2],df2=p.2.Romania.f.2[3], lower.tail=FALSE), 2)


# South.Africa
p.2.South.Africa.f.1=summary(mainOUTPUT.2$gvecm[[27]])$varresult$South.Africa.milper$fstatistic
p.2.South.Africa.pvalue.1=round(pf(p.2.South.Africa.f.1[1], df1=p.2.South.Africa.f.1[2],df2=p.2.South.Africa.f.1[3], lower.tail=FALSE), 2)

p.2.South.Africa.f.2=summary(mainOUTPUT.2$gvecm[[27]])$varresult$South.Africa.irst$fstatistic
p.2.South.Africa.pvalue.2=round(pf(p.2.South.Africa.f.2[1], df1=p.2.South.Africa.f.2[2],df2=p.2.South.Africa.f.2[3], lower.tail=FALSE), 2)

# South.Korea
p.2.South.Korea.f.1=summary(mainOUTPUT.2$gvecm[[28]])$varresult$South.Korea.milper$fstatistic
p.2.South.Korea.pvalue.1=round(pf(p.2.South.Korea.f.1[1], df1=p.2.South.Korea.f.1[2],df2=p.2.South.Korea.f.1[3], lower.tail=FALSE), 2)

p.2.South.Korea.f.2=summary(mainOUTPUT.2$gvecm[[28]])$varresult$South.Korea.irst$fstatistic
p.2.South.Korea.pvalue.2=round(pf(p.2.South.Korea.f.2[1], df1=p.2.South.Korea.f.2[2],df2=p.2.South.Korea.f.2[3], lower.tail=FALSE), 2)

# Spain
p.2.Spain.f.1=summary(mainOUTPUT.2$gvecm[[29]])$varresult$Spain.milper$fstatistic
p.2.Spain.pvalue.1=round(pf(p.2.Spain.f.1[1], df1=p.2.Spain.f.1[2],df2=p.2.Spain.f.1[3], lower.tail=FALSE), 2)

p.2.Spain.f.2=summary(mainOUTPUT.2$gvecm[[29]])$varresult$Spain.irst$fstatistic
p.2.Spain.pvalue.2=round(pf(p.2.Spain.f.2[1], df1=p.2.Spain.f.2[2],df2=p.2.Spain.f.2[3], lower.tail=FALSE), 2)

# Taiwan
p.2.Taiwan.f.1=summary(mainOUTPUT.2$gvecm[[30]])$varresult$Taiwan.milper$fstatistic
p.2.Taiwan.pvalue.1=round(pf(p.2.Taiwan.f.1[1], df1=p.2.Taiwan.f.1[2],df2=p.2.Taiwan.f.1[3], lower.tail=FALSE), 2)

p.2.Taiwan.f.2=summary(mainOUTPUT.2$gvecm[[30]])$varresult$Taiwan.irst$fstatistic
p.2.Taiwan.pvalue.2=round(pf(p.2.Taiwan.f.2[1], df1=p.2.Taiwan.f.2[2],df2=p.2.Taiwan.f.2[3], lower.tail=FALSE), 2)

# Turkey
p.2.Turkey.f.1=summary(mainOUTPUT.2$gvecm[[31]])$varresult$Turkey.milper$fstatistic
p.2.Turkey.pvalue.1=round(pf(p.2.Turkey.f.1[1], df1=p.2.Turkey.f.1[2],df2=p.2.Turkey.f.1[3], lower.tail=FALSE), 2)

p.2.Turkey.f.2=summary(mainOUTPUT.2$gvecm[[31]])$varresult$Turkey.irst$fstatistic
p.2.Turkey.pvalue.2=round(pf(p.2.Turkey.f.2[1], df1=p.2.Turkey.f.2[2],df2=p.2.Turkey.f.2[3], lower.tail=FALSE), 2)

# United.Kingdom
p.2.United.Kingdom.f.1=summary(mainOUTPUT.2$gvecm[[32]])$varresult$United.Kingdom.milper$fstatistic
p.2.United.Kingdom.pvalue.1=round(pf(p.2.United.Kingdom.f.1[1], df1=p.2.United.Kingdom.f.1[2],df2=p.2.United.Kingdom.f.1[3], lower.tail=FALSE), 2)

p.2.United.Kingdom.f.2=summary(mainOUTPUT.2$gvecm[[32]])$varresult$United.Kingdom.irst$fstatistic
p.2.United.Kingdom.pvalue.2=round(pf(p.2.United.Kingdom.f.2[1], df1=p.2.United.Kingdom.f.2[2],df2=p.2.United.Kingdom.f.2[3], lower.tail=FALSE), 2)



### Argentina
## Ftests
p.2.Argentina.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.milper.fstatistic.value"]), 3)
p.2.Argentina.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Argentina.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Argentina.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Argentina.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Argentina.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.irst.adj.r.squared"]), 3) 
# Lags
p.2.Argentina.lags = mainOUTPUT.2$lagmatrix$lags[1] # Lag for the first country 


### Australia
## Ftests
p.2.Australia.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.milper.fstatistic.value"]), 3)
p.2.Australia.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Australia.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Australia.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Australia.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Australia.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.irst.adj.r.squared"]), 3) 
# Lags
p.2.Australia.lags = mainOUTPUT.2$lagmatrix$lags[2] # Lag for the first country 


### Austria
## Ftests
p.2.Austria.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.milper.fstatistic.value"]), 3)
p.2.Austria.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Austria.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Austria.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Austria.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Austria.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.irst.adj.r.squared"]), 3) 
# Lags
p.2.Austria.lags = mainOUTPUT.2$lagmatrix$lags[3] # Lag for the first country 

### Belgium
## Ftests
p.2.Belgium.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.milper.fstatistic.value"]), 3)
p.2.Belgium.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Belgium.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Belgium.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Belgium.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Belgium.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.irst.adj.r.squared"]), 3) 
# Lags
p.2.Belgium.lags = mainOUTPUT.2$lagmatrix$lags[4] # Lag for the first country 


### Brazil
## Ftests
p.2.Brazil.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.milper.fstatistic.value"]), 3)
p.2.Brazil.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Brazil.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Brazil.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Brazil.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Brazil.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.irst.adj.r.squared"]), 3) 
# Lags
p.2.Brazil.lags = mainOUTPUT.2$lagmatrix$lags[5] # Lag for the first country 

### Bulgaria
## Ftests
p.2.Bulgaria.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.milper.fstatistic.value"]), 3)
p.2.Bulgaria.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Bulgaria.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Bulgaria.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Bulgaria.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Bulgaria.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.irst.adj.r.squared"]), 3) 
# Lags
p.2.Bulgaria.lags = mainOUTPUT.2$lagmatrix$lags[6] # Lag for the first country 

### Canada
## Ftests
p.2.Canada.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.milper.fstatistic.value"]), 3)
p.2.Canada.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Canada.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Canada.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Canada.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Canada.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.irst.adj.r.squared"]), 3) 
# Lags
p.2.Canada.lags = mainOUTPUT.2$lagmatrix$lags[7] # Lag for the first country 

### Chile
## Ftests
p.2.Chile.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.milper.fstatistic.value"]), 3)
p.2.Chile.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Chile.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Chile.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Chile.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Chile.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.irst.adj.r.squared"]), 3) 
# Lags
p.2.Chile.lags = mainOUTPUT.2$lagmatrix$lags[8] # Lag for the first country 


### Colombia
## Ftests
p.2.Colombia.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.milper.fstatistic.value"]), 3)
p.2.Colombia.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Colombia.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Colombia.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Colombia.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Colombia.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.irst.adj.r.squared"]), 3) 
# Lags
p.2.Colombia.lags = mainOUTPUT.2$lagmatrix$lags[9] # Lag for the first country 

### Egypt
## Ftests
p.2.Egypt.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.milper.fstatistic.value"]), 3)
p.2.Egypt.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Egypt.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Egypt.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Egypt.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Egypt.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.irst.adj.r.squared"]), 3) 
# Lags
p.2.Egypt.lags = mainOUTPUT.2$lagmatrix$lags[10] # Lag for the first country 

### Finland
## Ftests
p.2.Finland.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.milper.fstatistic.value"]), 3)
p.2.Finland.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Finland.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Finland.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Finland.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Finland.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.irst.adj.r.squared"]), 3) 
# Lags
p.2.Finland.lags = mainOUTPUT.2$lagmatrix$lags[11] # Lag for the first country 

### France
## Ftests
p.2.France.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.milper.fstatistic.value"]), 3)
p.2.France.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.France.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.France.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.France.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.France.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.irst.adj.r.squared"]), 3) 
# Lags
p.2.France.lags = mainOUTPUT.2$lagmatrix$lags[12] # Lag for the first country 

### Greece
## Ftests
p.2.Greece.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.milper.fstatistic.value"]), 3)
p.2.Greece.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Greece.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Greece.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Greece.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Greece.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.irst.adj.r.squared"]), 3) 
# Lags
p.2.Greece.lags = mainOUTPUT.2$lagmatrix$lags[13] # Lag for the first country 


### Hungary
## Ftests
p.2.Hungary.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.milper.fstatistic.value"]), 3)
p.2.Hungary.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Hungary.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Hungary.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Hungary.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Hungary.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.irst.adj.r.squared"]), 3) 
# Lags
p.2.Hungary.lags = mainOUTPUT.2$lagmatrix$lags[14] # Lag for the first country 


### India
## Ftests
p.2.India.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.milper.fstatistic.value"]), 3)
p.2.India.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.India.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.India.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.India.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.India.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.irst.adj.r.squared"]), 3) 
# Lags
p.2.India.lags = mainOUTPUT.2$lagmatrix$lags[15] # Lag for the first country 


### Israel
## Ftests
p.2.Israel.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.milper.fstatistic.value"]), 3)
p.2.Israel.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Israel.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Israel.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Israel.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Israel.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.irst.adj.r.squared"]), 3) 
# Lags
p.2.Israel.lags = mainOUTPUT.2$lagmatrix$lags[16] # Lag for the first country 


### Italy
## Ftests
p.2.Italy.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.milper.fstatistic.value"]), 3)
p.2.Italy.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Italy.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Italy.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Italy.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Italy.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.irst.adj.r.squared"]), 3) 
# Lags
p.2.Italy.lags = mainOUTPUT.2$lagmatrix$lags[17] # Lag for the first country 

### Japan
## Ftests
p.2.Japan.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.milper.fstatistic.value"]), 3)
p.2.Japan.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Japan.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Japan.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Japan.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Japan.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.irst.adj.r.squared"]), 3) 
# Lags
p.2.Japan.lags = mainOUTPUT.2$lagmatrix$lags[18] # Lag for the first country 

### Luxembourg
## Ftests
p.2.Luxembourg.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.milper.fstatistic.value"]), 3)
p.2.Luxembourg.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Luxembourg.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Luxembourg.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Luxembourg.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Luxembourg.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.irst.adj.r.squared"]), 3) 
# Lags
p.2.Luxembourg.lags = mainOUTPUT.2$lagmatrix$lags[19] # Lag for the first country 


### Mexico
## Ftests
p.2.Mexico.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.milper.fstatistic.value"]), 3)
p.2.Mexico.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Mexico.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Mexico.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Mexico.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Mexico.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.irst.adj.r.squared"]), 3) 
# Lags
p.2.Mexico.lags = mainOUTPUT.2$lagmatrix$lags[20] # Lag for the first country 

### Netherlands
## Ftests
p.2.Netherlands.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.milper.fstatistic.value"]), 3)
p.2.Netherlands.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Netherlands.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Netherlands.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Netherlands.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Netherlands.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.irst.adj.r.squared"]), 3) 
# Lags
p.2.Netherlands.lags = mainOUTPUT.2$lagmatrix$lags[21] # Lag for the first country 


### North.Korea
## Ftests
p.2.North.Korea.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.milper.fstatistic.value"]), 3)
p.2.North.Korea.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.North.Korea.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.North.Korea.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.North.Korea.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.North.Korea.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.irst.adj.r.squared"]), 3) 
# Lags
p.2.North.Korea.lags = mainOUTPUT.2$lagmatrix$lags[22] # Lag for the first country 

### Norway
## Ftests
p.2.Norway.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.milper.fstatistic.value"]), 3)
p.2.Norway.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Norway.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Norway.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Norway.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Norway.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.irst.adj.r.squared"]), 3) 
# Lags
p.2.Norway.lags = mainOUTPUT.2$lagmatrix$lags[23] # Lag for the first country 

### Poland
## Ftests
p.2.Poland.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.milper.fstatistic.value"]), 3)
p.2.Poland.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Poland.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Poland.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Poland.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Poland.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.irst.adj.r.squared"]), 3) 
# Lags
p.2.Poland.lags = mainOUTPUT.2$lagmatrix$lags[24] # Lag for the first country 

### Portugal
## Ftests
p.2.Portugal.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.milper.fstatistic.value"]), 3)
p.2.Portugal.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Portugal.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Portugal.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Portugal.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Portugal.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.irst.adj.r.squared"]), 3) 
# Lags
p.2.Portugal.lags = mainOUTPUT.2$lagmatrix$lags[25] # Lag for the first country 

### Romania
## Ftests
p.2.Romania.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.milper.fstatistic.value"]), 3)
p.2.Romania.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Romania.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Romania.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Romania.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Romania.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.irst.adj.r.squared"]), 3) 
# Lags
p.2.Romania.lags = mainOUTPUT.2$lagmatrix$lags[26] # Lag for the first country 


### South.Africa
## Ftests
p.2.South.Africa.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.milper.fstatistic.value"]), 3)
p.2.South.Africa.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.South.Africa.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.South.Africa.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.South.Africa.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.South.Africa.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.irst.adj.r.squared"]), 3) 
# Lags
p.2.South.Africa.lags = mainOUTPUT.2$lagmatrix$lags[27] # Lag for the first country 



### South.Korea
## Ftests
p.2.South.Korea.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.milper.fstatistic.value"]), 3)
p.2.South.Korea.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.South.Korea.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.South.Korea.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.South.Korea.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.South.Korea.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.irst.adj.r.squared"]), 3) 
# Lags
p.2.South.Korea.lags = mainOUTPUT.2$lagmatrix$lags[28] # Lag for the first country 


### Spain
## Ftests
p.2.Spain.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.milper.fstatistic.value"]), 3)
p.2.Spain.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Spain.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Spain.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Spain.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Spain.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.irst.adj.r.squared"]), 3) 
# Lags
p.2.Spain.lags = mainOUTPUT.2$lagmatrix$lags[29] # Lag for the first country 


### Taiwan
## Ftests
p.2.Taiwan.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.milper.fstatistic.value"]), 3)
p.2.Taiwan.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Taiwan.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Taiwan.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Taiwan.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Taiwan.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.irst.adj.r.squared"]), 3) 
# Lags
p.2.Taiwan.lags = mainOUTPUT.2$lagmatrix$lags[30] # Lag for the first country 

### Turkey
## Ftests
p.2.Turkey.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.milper.fstatistic.value"]), 3)
p.2.Turkey.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Turkey.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Turkey.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Turkey.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Turkey.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.irst.adj.r.squared"]), 3) 
# Lags
p.2.Turkey.lags = mainOUTPUT.2$lagmatrix$lags[31] # Lag for the first country 

### United.Kingdom
## Ftests
p.2.United.Kingdom.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.milper.fstatistic.value"]), 3)
p.2.United.Kingdom.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.United.Kingdom.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.United.Kingdom.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.United.Kingdom.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.United.Kingdom.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.irst.adj.r.squared"]), 3) 
# Lags
p.2.United.Kingdom.lags = mainOUTPUT.2$lagmatrix$lags[32] # Lag for the first country 
## ----

summary(mainOUTPUT.2$gvecm[[1]]) # Argentina 
summary(mainOUTPUT.2$gvecm[[2]]) # Australia 
summary(mainOUTPUT.2$gvecm[[3]]) # Austria 
summary(mainOUTPUT.2$gvecm[[4]]) # Belgium 
summary(mainOUTPUT.2$gvecm[[5]]) # Brazil 
summary(mainOUTPUT.2$gvecm[[6]]) # Bulgaria 
summary(mainOUTPUT.2$gvecm[[7]]) # Canada 
summary(mainOUTPUT.2$gvecm[[8]]) # Chile 
summary(mainOUTPUT.2$gvecm[[9]]) # Colombia 
summary(mainOUTPUT.2$gvecm[[10]]) # Egypt 
summary(mainOUTPUT.2$gvecm[[11]]) # Finland 
summary(mainOUTPUT.2$gvecm[[12]]) # France 
summary(mainOUTPUT.2$gvecm[[13]]) # Greece 
summary(mainOUTPUT.2$gvecm[[14]]) # Hungary 
summary(mainOUTPUT.2$gvecm[[15]]) # India 
summary(mainOUTPUT.2$gvecm[[16]]) # Israel 
summary(mainOUTPUT.2$gvecm[[17]]) # Italy 
summary(mainOUTPUT.2$gvecm[[18]]) # Japan 
summary(mainOUTPUT.2$gvecm[[19]]) # Luxembourg 
summary(mainOUTPUT.2$gvecm[[20]]) # Mexico 
summary(mainOUTPUT.2$gvecm[[21]]) # Netherlands 
summary(mainOUTPUT.2$gvecm[[22]]) # North.Korea 
summary(mainOUTPUT.2$gvecm[[23]]) # Norway 
summary(mainOUTPUT.2$gvecm[[24]]) # Poland 
summary(mainOUTPUT.2$gvecm[[25]]) # Portugal 
summary(mainOUTPUT.2$gvecm[[26]]) # Romania 
summary(mainOUTPUT.2$gvecm[[27]]) # South.Africa 
summary(mainOUTPUT.2$gvecm[[28]]) # South.Korea 
summary(mainOUTPUT.2$gvecm[[29]]) # Spain 
summary(mainOUTPUT.2$gvecm[[30]]) # Taiwan 
summary(mainOUTPUT.2$gvecm[[31]]) # Turkey 
summary(mainOUTPUT.2$gvecm[[32]]) # United.Kingdom 


########################################################
# Second Period (Big Countries)
########################################################

## ---- gvar:model:second:period:b ----

p_load(GVARX)

p.2.b=2 # The number of lag for Xt matrix
FLag.2.b=2 # The number of lag for foreign variables in country-specific VAR
lag.max.2.b=5 # The maximal number of lag for estimating country-specific VAR
type.2.b="none" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.2.b="AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

options(scipen=9999999)
mainOUTPUT.2.b = GVECMest(
        data = cow.d.2.B,
        p = p.2.b,
        FLag = FLag.2.b,
        lag.max = lag.max.2.b,
        type = type.2.b,
        ic = ic.2.b,
        weight.matrix=wm.2.b)

# determinsitic component for the paper
type.2.b.paper = as.character(ifelse(type.2.b=="both", as.character("Trend and constant"), ifelse(type.2.b == "trend", as.character("Trend"), ifelse(type.2.b == "constant", "Constant", NA ))))

# Storing Values

# China
p.2.China.f.1=summary(mainOUTPUT.2.b$gvecm[[1]])$varresult$China.milper$fstatistic
p.2.China.pvalue.1=round(pf(p.2.China.f.1[1], df1=p.2.China.f.1[2],df2=p.2.China.f.1[3], lower.tail=FALSE), 2)

p.2.China.f.2=summary(mainOUTPUT.2.b$gvecm[[1]])$varresult$China.irst$fstatistic
p.2.China.pvalue.2=round(pf(p.2.China.f.2[1], df1=p.2.China.f.2[2],df2=p.2.China.f.2[3], lower.tail=FALSE), 2)

# Russia
p.2.Russia.f.1=summary(mainOUTPUT.2.b$gvecm[[2]])$varresult$Russia.milper$fstatistic
p.2.Russia.pvalue.1=round(pf(p.2.Russia.f.1[1], df1=p.2.Russia.f.1[2],df2=p.2.Russia.f.1[3], lower.tail=FALSE), 2)

p.2.Russia.f.2=summary(mainOUTPUT.2.b$gvecm[[2]])$varresult$Russia.irst$fstatistic
p.2.Russia.pvalue.2=round(pf(p.2.Russia.f.2[1], df1=p.2.Russia.f.2[2],df2=p.2.Russia.f.2[3], lower.tail=FALSE), 2)


# United.States.of.America
p.2.United.States.f.1=summary(mainOUTPUT.2.b$gvecm[[3]])$varresult$United.States.of.America.milper$fstatistic
p.2.United.States.pvalue.1=round(pf(p.2.United.States.f.1[1], df1=p.2.United.States.f.1[2],df2=p.2.United.States.f.1[3], lower.tail=FALSE), 2)

p.2.United.States.f.2=summary(mainOUTPUT.2.b$gvecm[[3]])$varresult$United.States.of.America.irst$fstatistic
p.2.United.States.pvalue.2=round(pf(p.2.United.States.f.2[1], df1=p.2.United.States.f.2[2],df2=p.2.United.States.f.2[3], lower.tail=FALSE), 2)


### China
## Ftests
p.2.China.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.milper.fstatistic.value"]), 3)
p.2.China.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.China.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.China.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.China.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.China.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.irst.adj.r.squared"]), 3) 
# Lags
p.2.China.lags = mainOUTPUT.2.b$lagmatrix$lags[1] # Lag for the first country 



### Russia
## Ftests
p.2.Russia.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.milper.fstatistic.value"]), 3)
p.2.Russia.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Russia.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Russia.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Russia.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Russia.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.irst.adj.r.squared"]), 3) 
# Lags
p.2.Russia.lags = mainOUTPUT.2.b$lagmatrix$lags[2] # Lag for the first country 


### United.States.of.America
## Ftests
p.2.United.States.of.America.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.milper.fstatistic.value"]), 3)
p.2.United.States.of.America.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.United.States.of.America.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.United.States.of.America.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.United.States.of.America.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.United.States.of.America.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.irst.adj.r.squared"]), 3) 
# Lags
p.2.United.States.of.America.lags = mainOUTPUT.2.b$lagmatrix$lags[3] # Lag for the first country 
## ----


## ---- pvalues.plot:d ----
# first period, all
d.1 = data.frame(
        PValue = as.vector(rbind(
                # 1
                pf(p.1.Austria.Hungary.f.1[1], df1=p.1.Austria.Hungary.f.1[2],df2=p.1.Austria.Hungary.f.1[3], lower.tail=FALSE), 
                pf(p.1.Belgium.f.1[1], df1=p.1.Belgium.f.1[2],df2=p.1.Belgium.f.1[3], lower.tail=FALSE), 
                pf(p.1.France.f.1[1], df1=p.1.France.f.1[2],df2=p.1.France.f.1[3], lower.tail=FALSE), 
                pf(p.1.Germany.f.1[1], df1=p.1.Germany.f.1[2],df2=p.1.Germany.f.1[3], lower.tail=FALSE), 
                pf(p.1.Italy.f.1[1], df1=p.1.Italy.f.1[2],df2=p.1.Italy.f.1[3], lower.tail=FALSE), 
                pf(p.1.Russia.f.1[1], df1=p.1.Russia.f.1[2],df2=p.1.Russia.f.1[3], lower.tail=FALSE), 
                pf(p.1.Spain.f.1[1], df1=p.1.Spain.f.1[2],df2=p.1.Spain.f.1[3], lower.tail=FALSE), 
                pf(p.1.United.Kingdom.f.1[1], df1=p.1.United.Kingdom.f.1[2],df2=p.1.United.Kingdom.f.1[3], lower.tail=FALSE), 
                pf(p.1.United.States.f.1[1], df1=p.1.United.States.f.1[2],df2=p.1.United.States.f.1[3], lower.tail=FALSE), 
                # 2
                pf(p.1.Austria.Hungary.f.2[1], df1=p.1.Austria.Hungary.f.2[2],df2=p.1.Austria.Hungary.f.2[3], lower.tail=FALSE),
                pf(p.1.Belgium.f.2[1], df1=p.1.Belgium.f.2[2],df2=p.1.Belgium.f.2[3], lower.tail=FALSE),
                pf(p.1.France.f.2[1], df1=p.1.France.f.2[2],df2=p.1.France.f.2[3], lower.tail=FALSE),
                pf(p.1.Germany.f.2[1], df1=p.1.Germany.f.2[2],df2=p.1.Germany.f.2[3], lower.tail=FALSE),
                pf(p.1.Italy.f.2[1], df1=p.1.Italy.f.2[2],df2=p.1.Italy.f.2[3], lower.tail=FALSE),
                pf(p.1.Russia.f.2[1], df1=p.1.Russia.f.2[2],df2=p.1.Russia.f.2[3], lower.tail=FALSE),
                pf(p.1.Spain.f.2[1], df1=p.1.Spain.f.2[2],df2=p.1.Spain.f.2[3], lower.tail=FALSE),
                pf(p.1.United.Kingdom.f.2[1], df1=p.1.United.Kingdom.f.2[2],df2=p.1.United.Kingdom.f.2[3], lower.tail=FALSE),
                pf(p.1.United.States.f.2[1], df1=p.1.United.States.f.2[2],df2=p.1.United.States.f.2[3], lower.tail=FALSE)
        )
        ),
        Country = rep(c(unique(cow.d.1$ID)), 2),
        Years = rep(paste(as.character(year.min.t1), as.character(year.max.t1), sep = "-"), length(rep(c(unique(cow.d.1$ID)), 2))),
        Relationship = as.vector(c(rep("Steel -> Guns", length(unique(cow.d.1$ID))), rep("Guns -> Steel", length(unique(cow.d.1$ID)))))
)



# Second Period, Small countries
d.2 = data.frame(
        PValue = as.vector(rbind(
                # 1
                pf(p.2.Argentina.f.1[1], df1=p.2.Argentina.f.1[2],df2=p.2.Argentina.f.1[3], lower.tail=FALSE),
                pf(p.2.Australia.f.1[1], df1=p.2.Australia.f.1[2],df2=p.2.Australia.f.1[3], lower.tail=FALSE),
                pf(p.2.Austria.f.1[1], df1=p.2.Austria.f.1[2],df2=p.2.Austria.f.1[3], lower.tail=FALSE),
                pf(p.2.Belgium.f.1[1], df1=p.2.Belgium.f.1[2],df2=p.2.Belgium.f.1[3], lower.tail=FALSE),
                pf(p.2.Brazil.f.1[1], df1=p.2.Brazil.f.1[2],df2=p.2.Brazil.f.1[3], lower.tail=FALSE),
                pf(p.2.Bulgaria.f.1[1], df1=p.2.Bulgaria.f.1[2],df2=p.2.Bulgaria.f.1[3], lower.tail=FALSE),
                pf(p.2.Canada.f.1[1], df1=p.2.Canada.f.1[2],df2=p.2.Canada.f.1[3], lower.tail=FALSE),
                pf(p.2.Chile.f.1[1], df1=p.2.Chile.f.1[2],df2=p.2.Chile.f.1[3], lower.tail=FALSE),
                pf(p.2.Colombia.f.1[1], df1=p.2.Colombia.f.1[2],df2=p.2.Colombia.f.1[3], lower.tail=FALSE),
                pf(p.2.Egypt.f.1[1], df1=p.2.Egypt.f.1[2],df2=p.2.Egypt.f.1[3], lower.tail=FALSE),
                pf(p.2.Finland.f.1[1], df1=p.2.Finland.f.1[2],df2=p.2.Finland.f.1[3], lower.tail=FALSE),
                pf(p.2.France.f.1[1], df1=p.2.France.f.1[2],df2=p.2.France.f.1[3], lower.tail=FALSE),
                pf(p.2.Greece.f.1[1], df1=p.2.Greece.f.1[2],df2=p.2.Greece.f.1[3], lower.tail=FALSE),
                pf(p.2.Hungary.f.1[1], df1=p.2.Hungary.f.1[2],df2=p.2.Hungary.f.1[3], lower.tail=FALSE),
                pf(p.2.India.f.1[1], df1=p.2.India.f.1[2],df2=p.2.India.f.1[3], lower.tail=FALSE),
                pf(p.2.Israel.f.1[1], df1=p.2.Israel.f.1[2],df2=p.2.Israel.f.1[3], lower.tail=FALSE),
                pf(p.2.Italy.f.1[1], df1=p.2.Italy.f.1[2],df2=p.2.Italy.f.1[3], lower.tail=FALSE),
                pf(p.2.Japan.f.1[1], df1=p.2.Japan.f.1[2],df2=p.2.Japan.f.1[3], lower.tail=FALSE),
                pf(p.2.Luxembourg.f.1[1], df1=p.2.Luxembourg.f.1[2],df2=p.2.Luxembourg.f.1[3], lower.tail=FALSE),
                pf(p.2.Mexico.f.1[1], df1=p.2.Mexico.f.1[2],df2=p.2.Mexico.f.1[3], lower.tail=FALSE),
                pf(p.2.Netherlands.f.1[1], df1=p.2.Netherlands.f.1[2],df2=p.2.Netherlands.f.1[3], lower.tail=FALSE),
                pf(p.2.North.Korea.f.1[1], df1=p.2.North.Korea.f.1[2],df2=p.2.North.Korea.f.1[3], lower.tail=FALSE),
                pf(p.2.Norway.f.1[1], df1=p.2.Norway.f.1[2],df2=p.2.Norway.f.1[3], lower.tail=FALSE),
                pf(p.2.Poland.f.1[1], df1=p.2.Poland.f.1[2],df2=p.2.Poland.f.1[3], lower.tail=FALSE),
                pf(p.2.Portugal.f.1[1], df1=p.2.Portugal.f.1[2],df2=p.2.Portugal.f.1[3], lower.tail=FALSE),
                pf(p.2.Romania.f.1[1], df1=p.2.Romania.f.1[2],df2=p.2.Romania.f.1[3], lower.tail=FALSE),
                pf(p.2.South.Africa.f.1[1], df1=p.2.South.Africa.f.1[2],df2=p.2.South.Africa.f.1[3], lower.tail=FALSE),
                pf(p.2.South.Korea.f.1[1], df1=p.2.South.Korea.f.1[2],df2=p.2.South.Korea.f.1[3], lower.tail=FALSE),
                pf(p.2.Spain.f.1[1], df1=p.2.Spain.f.1[2],df2=p.2.Spain.f.1[3], lower.tail=FALSE),
                pf(p.2.Taiwan.f.1[1], df1=p.2.Taiwan.f.1[2],df2=p.2.Taiwan.f.1[3], lower.tail=FALSE),
                pf(p.2.Turkey.f.1[1], df1=p.2.Turkey.f.1[2],df2=p.2.Turkey.f.1[3], lower.tail=FALSE),
                pf(p.2.United.Kingdom.f.1[1], df1=p.2.United.Kingdom.f.1[2],df2=p.2.United.Kingdom.f.1[3], lower.tail=FALSE),
                # 2
                pf(p.2.Argentina.f.2[1], df1=p.2.Argentina.f.2[2],df2=p.2.Argentina.f.2[3], lower.tail=FALSE), 
                pf(p.2.Australia.f.2[1], df1=p.2.Australia.f.2[2],df2=p.2.Australia.f.2[3], lower.tail=FALSE), 
                pf(p.2.Austria.f.2[1], df1=p.2.Austria.f.2[2],df2=p.2.Austria.f.2[3], lower.tail=FALSE), 
                pf(p.2.Belgium.f.2[1], df1=p.2.Belgium.f.2[2],df2=p.2.Belgium.f.2[3], lower.tail=FALSE), 
                pf(p.2.Brazil.f.2[1], df1=p.2.Brazil.f.2[2],df2=p.2.Brazil.f.2[3], lower.tail=FALSE), 
                pf(p.2.Bulgaria.f.2[1], df1=p.2.Bulgaria.f.2[2],df2=p.2.Bulgaria.f.2[3], lower.tail=FALSE), 
                pf(p.2.Canada.f.2[1], df1=p.2.Canada.f.2[2],df2=p.2.Canada.f.2[3], lower.tail=FALSE), 
                pf(p.2.Chile.f.2[1], df1=p.2.Chile.f.2[2],df2=p.2.Chile.f.2[3], lower.tail=FALSE), 
                pf(p.2.Colombia.f.2[1], df1=p.2.Colombia.f.2[2],df2=p.2.Colombia.f.2[3], lower.tail=FALSE), 
                pf(p.2.Egypt.f.2[1], df1=p.2.Egypt.f.2[2],df2=p.2.Egypt.f.2[3], lower.tail=FALSE), 
                pf(p.2.Finland.f.2[1], df1=p.2.Finland.f.2[2],df2=p.2.Finland.f.2[3], lower.tail=FALSE), 
                pf(p.2.France.f.2[1], df1=p.2.France.f.2[2],df2=p.2.France.f.2[3], lower.tail=FALSE), 
                pf(p.2.Greece.f.2[1], df1=p.2.Greece.f.2[2],df2=p.2.Greece.f.2[3], lower.tail=FALSE), 
                pf(p.2.Hungary.f.2[1], df1=p.2.Hungary.f.2[2],df2=p.2.Hungary.f.2[3], lower.tail=FALSE), 
                pf(p.2.India.f.2[1], df1=p.2.India.f.2[2],df2=p.2.India.f.2[3], lower.tail=FALSE), 
                pf(p.2.Israel.f.2[1], df1=p.2.Israel.f.2[2],df2=p.2.Israel.f.2[3], lower.tail=FALSE), 
                pf(p.2.Italy.f.2[1], df1=p.2.Italy.f.2[2],df2=p.2.Italy.f.2[3], lower.tail=FALSE), 
                pf(p.2.Japan.f.2[1], df1=p.2.Japan.f.2[2],df2=p.2.Japan.f.2[3], lower.tail=FALSE), 
                pf(p.2.Luxembourg.f.2[1], df1=p.2.Luxembourg.f.2[2],df2=p.2.Luxembourg.f.2[3], lower.tail=FALSE), 
                pf(p.2.Mexico.f.2[1], df1=p.2.Mexico.f.2[2],df2=p.2.Mexico.f.2[3], lower.tail=FALSE), 
                pf(p.2.Netherlands.f.2[1], df1=p.2.Netherlands.f.2[2],df2=p.2.Netherlands.f.2[3], lower.tail=FALSE), 
                pf(p.2.North.Korea.f.2[1], df1=p.2.North.Korea.f.2[2],df2=p.2.North.Korea.f.2[3], lower.tail=FALSE), 
                pf(p.2.Norway.f.2[1], df1=p.2.Norway.f.2[2],df2=p.2.Norway.f.2[3], lower.tail=FALSE), 
                pf(p.2.Poland.f.2[1], df1=p.2.Poland.f.2[2],df2=p.2.Poland.f.2[3], lower.tail=FALSE), 
                pf(p.2.Portugal.f.2[1], df1=p.2.Portugal.f.2[2],df2=p.2.Portugal.f.2[3], lower.tail=FALSE), 
                pf(p.2.Romania.f.2[1], df1=p.2.Romania.f.2[2],df2=p.2.Romania.f.2[3], lower.tail=FALSE), 
                pf(p.2.South.Africa.f.2[1], df1=p.2.South.Africa.f.2[2],df2=p.2.South.Africa.f.2[3], lower.tail=FALSE), 
                pf(p.2.South.Korea.f.2[1], df1=p.2.South.Korea.f.2[2],df2=p.2.South.Korea.f.2[3], lower.tail=FALSE), 
                pf(p.2.Spain.f.2[1], df1=p.2.Spain.f.2[2],df2=p.2.Spain.f.2[3], lower.tail=FALSE), 
                pf(p.2.Taiwan.f.2[1], df1=p.2.Taiwan.f.2[2],df2=p.2.Taiwan.f.2[3], lower.tail=FALSE), 
                pf(p.2.Turkey.f.2[1], df1=p.2.Turkey.f.2[2],df2=p.2.Turkey.f.2[3], lower.tail=FALSE), 
                pf(p.2.United.Kingdom.f.2[1], df1=p.2.United.Kingdom.f.2[2],df2=p.2.United.Kingdom.f.2[3], lower.tail=FALSE))),
        Country = rep(c(unique(cow.d.2$ID)), 2),
        Years = rep(paste(as.character(year.min.t2), as.character(year.max.t2), sep = "-"), length(rep(c(unique(cow.d.2$ID)), 2))),
        Relationship = as.vector(c(rep("Steel -> Guns", length(unique(cow.d.2$ID))), rep("Guns -> Steel", length(unique(cow.d.2$ID)))))
)

# Second Period, Big countries
d.3 = data.frame(
        PValue = as.vector(rbind(
                pf(p.2.China.f.1[1], df1=p.2.China.f.1[2],df2=p.2.China.f.1[3], lower.tail=FALSE), # p.2.China.pvalue.1
                pf(p.2.Russia.f.1[1], df1=p.2.Russia.f.1[2],df2=p.2.Russia.f.1[3], lower.tail=FALSE), # p.2.Russia.pvalue.1
                pf(p.2.United.States.f.1[1], df1=p.2.United.States.f.1[2],df2=p.2.United.States.f.1[3], lower.tail=FALSE), # p.2.United.States.pvalue.1
                pf(p.2.China.f.2[1], df1=p.2.China.f.2[2],df2=p.2.China.f.2[3], lower.tail=FALSE), # p.2.China.pvalue.2
                pf(p.2.Russia.f.2[1], df1=p.2.Russia.f.2[2],df2=p.2.Russia.f.2[3], lower.tail=FALSE), # p.2.Russia.pvalue.2
                pf(p.2.United.States.f.2[1], df1=p.2.United.States.f.2[2],df2=p.2.United.States.f.2[3], lower.tail=FALSE) # p.2.United.States.pvalue.2
        )),
        Country = rep(c(unique(cow.d.2.B$ID)), 2),
        Years = rep(paste(as.character(year.min.t2), as.character(year.max.t2), sep = "-"), length(rep(c(unique(cow.d.2.B$ID)), 2))),
        Relationship = as.vector(c(rep("Steel -> Guns", length(unique(cow.d.2.B$ID))), rep("Guns -> Steel", length(unique(cow.d.2.B$ID)))))
)

d = rbind(d.1, d.2, d.3)

p_load(ggplot2,hrbrthemes, viridis)
p.value.plot = ggplot(d, aes(x = Country, y = PValue)) +
        geom_point(aes(colour = PValue), size = 5,  alpha = 0.6) + #shape = Relationship # size = PValue
        #geom_text(hjust = 1, size = 2) +
        scale_size(range = c(5,10)) +
        theme_bw() + # theme_ipsum
        scale_color_gradient(low = "green", high = "red",limits=c(0, 0.1)) +
        facet_grid(Relationship ~ Years) +
        theme(legend.position="bottom", legend.direction="horizontal")  +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7,angle = 90), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=9), 
              legend.title=element_text(size=9),
              plot.title = element_text(size=3),
              #legend.position="bottom",
              legend.key.size = unit(0.9,"cm"),
              legend.spacing.x = unit(0.7, 'cm'),
              strip.text.x = element_text(size = 7))

# percentages
percent.steel.causes.guns.t1 = round(as.numeric(table(d.1$PValue[d.1$Relationship == "Steel -> Guns"] <= 0.1)[2]*100/length(unique(d.1$Country))),0)
percent.steel.causes.guns.t2 = round(as.numeric(table(d.2$PValue[d.2$Relationship == "Steel -> Guns"] <= 0.1)[2]*100/length(unique(d.2$Country))),0)
percent.steel.causes.guns.t2.b = round(as.numeric(table(d.3$PValue[d.3$Relationship == "Steel -> Guns"] <= 0.1)[2]*100/length(unique(d.3$Country))),0)

## ----

## ---- pvalues.plot ----
p.value.plot
p.value.plot.note <- paste(
        paste("{\\bf P-Values of the Country-specific Granger-causality F-Tests,", paste(paste(year.min.t1, "-",paste(year.max.t2, ".", sep=""), sep=""), "}", sep = ""), sep=" "),
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: Plot shows country-specific p-values of the Granger-causality f-tests obtained when fitting \\autoref{eq:gvar:granger} (detailed results shown in \\autoref{t:1}, \\autoref{t:2} and \\autoref{t:2b}). The plot shows that during the", paste(paste(year.min.t1, "-",paste(year.max.t1, sep=""), sep=""), sep = ""), "period, in" ,
              paste(percent.steel.causes.guns.t1, "\\%", sep = ""), "of the countries, steel Granger-caused guns. This porcentage changes to", paste(percent.steel.causes.guns.t2, "\\%", sep = ""), "and to", paste(percent.steel.causes.guns.t2.b, "\\%", sep = ""), "for the hegemonic countries during the", paste(paste(year.min.t2, "-",paste(year.max.t2, sep=""), sep=""), sep = ""), "period."),
        "\n")
## ----




############################################
# Checking Panel stationarity First Period
############################################

# "the GVAR methodology can be applied to stationary and/or integrated variables" Dees2007, p. 10
# Granger models can be estimated with non-stationary data (Freeman1983, 334).

## tests in "plm" package assume "the series under scrutiny are cross-sectionally independent"
## tests in "punitroots" assume "cross-dependence across the panel units", which is what we believe
## Kleiber2011, p. 2


### Package
## Dependencies
# install.packages("fBasics")
# install.packages("fUnitRoots")
# install.packages("CADFtest")
## Install package of interest
# install.packages("punitroots")
library(punitroots)

# Need to melt data first to make them look like these data("OECDunemp")
p_load(dplyr)

cow.d.1.irst = select(cow.d.1, "ID", "irst", "Time")
cow.d.1.irst = data.frame(t(reshape(cow.d.1.irst, idvar = "ID", timevar = "Time", direction = "wide")))
rownames(cow.d.1.irst) <- NULL
colnames(cow.d.1.irst) <- c(as.character(lapply(cow.d.1.irst[1,] , as.character)))
cow.d.1.irst = data.frame(cow.d.1.irst[-c(1), ])
cow.d.1.irst <- mutate_all(cow.d.1.irst, function(x) as.numeric(as.character(x)))

cow.d.1.milper = select(cow.d.1, "ID", "milper", "Time")
cow.d.1.milper = data.frame(t(reshape(cow.d.1.milper, idvar = "ID", timevar = "Time", direction = "wide")))
rownames(cow.d.1.milper) <- NULL
colnames(cow.d.1.milper) <- c(as.character(lapply(cow.d.1.milper[1,] , as.character)))
cow.d.1.milper = data.frame(cow.d.1.milper[-c(1), ])
cow.d.1.milper <- mutate_all(cow.d.1.milper, function(x) as.numeric(as.character(x)))

## ?pCADFtest
# This function implements the panel Covariate Augmented Dickey-Fuller (pCADF) test developed in Costantini and Lupi (2012). 
# The panel unit root tests proposed in Choi (2001) and in Demetrescu et al. (2006) can also be performed using this function.
cow.d.1.irst.station = pCADFtest(Y=cow.d.1.irst, type = "drift", criterion = "AIC")
cow.d.1.milper.station = pCADFtest(Y = cow.d.1.milper, type = "drift", criterion = "AIC")

summary(cow.d.1.irst.station)
# 1. The line Correction for cross-correlation: TRUE states that cross-dependence has been detected and Hartung’s correction has been used in the combination of the p values as sug- gested in Demetrescu et al. (2006). Kleiber2011, 10
# 2. unit root = NULL. Do we have enough to reject the null? p-value of 1.00000 indicates that we have non-stationarity.


summary(cow.d.1.milper.station)
# 1. The line Correction for cross-correlation: TRUE states that cross-dependence has been detected and Hartung’s correction has been used in the combination of the p values as sug- gested in Demetrescu et al. (2006). Kleiber2011, 10
# 2. unit root = NULL. Do we have enough to reject the null? p-value of 0.07191166 indicates that we have non-stationarity.


#############################
# First Differencing the series
#############################

cow.d.1.irst.diff = data.frame(diff(as.matrix(cow.d.1.irst)))
cow.d.1.milper.diff = data.frame(diff(as.matrix(cow.d.1.milper)))

#############################
# Re-testing for stationarity
#############################

options(scipen=9999999)

cow.d.1.irst.diff.station = pCADFtest(Y=cow.d.1.irst.diff, type = "drift", criterion = "AIC") ; summary(cow.d.1.irst.diff.station)
cow.d.1.milper.diff.station = pCADFtest(Y=cow.d.1.milper.diff, type = "drift", criterion = "AIC") ; summary(cow.d.1.milper.diff.station)

# Really small p-values: series are stationary once they're first differenced


#############################
# Test Lag Structure
#############################

# Pending
## Test for stability
### This is neccesary to set "p" below.

# Include in the paper a brief but detailed discussion, like FN # 31 in Box-Steffensmeier2014a, p. 120.


# check lags per country; build df's, one per country
country.1.t1 = data.frame(milper = cow.d.1[cow.d.1$ID == full.info.countries.first.time.span[1], ]$milper, irst = cow.d.1[cow.d.1$ID == full.info.countries.first.time.span[1], ]$irst, row.names = NULL)

# package to test lag lenght
p_load(vars)

lag.max.lag.lenght = 5
test.restrictions.lag.lenght = "both" # both = test for both constant and trend


test = VARselect(country.1.t1, lag.max = lag.max.lag.lenght, type=test.restrictions.lag.lenght) 
test$selection # "selection" = Vector with the optimal lag number according to each criterium




################################################################################################################
#### Plots
################################################################################################################

## ---- plot:d ----

# Plot t1
p_load(reshape)
cow.d.1.log = cow.d.1
names(cow.d.1.log)[3] <- "Guns"
names(cow.d.1.log)[4] <- "Steel"
cow.d.1.log$Guns = log(cow.d.1.log$Guns)
cow.d.1.log$Steel = log(cow.d.1.log$Steel)


cow.d.1.log <- melt(cow.d.1.log, id.vars = c("ID", "Time"))

p_load(ggplot2)
cow.d.1.plot = ggplot(cow.d.1.log,
                      aes(x = Time, y = value, colour = variable)) +
        geom_line() +
        facet_wrap(~ ID, ncol = 3, scales="free_y") +
        xlab("Year") + 
        ylab("National Steel and Guns Production (log)") +
        theme_bw() +
        theme(legend.position="bottom", legend.direction="horizontal")  +
        theme(axis.text.y = element_text(size=3), 
              axis.text.x = element_text(size=3), 
              axis.title.y = element_text(size=6), 
              axis.title.x = element_text(size=6), 
              legend.text=element_text(size=6), 
              legend.title=element_text(size=0),
              plot.title = element_text(size=3),
              legend.position="bottom",
              legend.key.size = unit(0.5,"cm"),
              legend.spacing.x = unit(0.3, 'cm'),
              strip.text.x = element_text(size = 3))

# Plot t2
p_load(reshape)
cow.d.2.log = cow.d.2
names(cow.d.2.log)[3] <- "Guns"
names(cow.d.2.log)[4] <- "Steel"


# Luxembourg, Guns
cow.d.2.log$Guns[cow.d.2.log$ID=="Luxembourg"] <- cow.d.2.log$Guns[cow.d.2.log$ID=="Luxembourg"]+2# There were some zeroes which were causing issues when taking the log for the plots. Monotonic transformation (log of 1 is 0, so change is really small)

# And now we take the logs and there are no issues
cow.d.2.log$Guns = log(cow.d.2.log$Guns)
cow.d.2.log$Steel = log(cow.d.2.log$Steel)

cow.d.2.log <- melt(cow.d.2.log, id.vars = c("ID", "Time"))

p_load(ggplot2)
cow.d.2.plot = ggplot(cow.d.2.log,
                      aes(x = Time, y = value, colour = variable)) +
        geom_line() +
        facet_wrap(~ ID, ncol = 4, scales="free_y") +
        xlab("Year") + 
        ylab("National Steel and Guns Production (log)") +
        theme_bw() +
        theme(legend.position="bottom", legend.direction="horizontal")  +
        theme(axis.text.y = element_text(size=3), 
              axis.text.x = element_text(size=3), 
              axis.title.y = element_text(size=6), 
              axis.title.x = element_text(size=6), 
              legend.text=element_text(size=6), 
              legend.title=element_text(size=0),
              plot.title = element_text(size=3),
              legend.position="bottom",
              legend.key.size = unit(0.5,"cm"),
              legend.spacing.x = unit(0.3, 'cm'),
              strip.text.x = element_text(size = 4))
## ----


## ---- plot:t:1 ----
cow.d.1.plot
cow.d.1.plot.note <- paste(
        paste("{\\bf National Steel and Guns Production (log),", paste(paste(year.min.t1, "-",paste(year.max.t1, ".", sep=""), sep=""), "}", sep = ""), sep=" "),
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Variables are ``milper'' and ``irst.'' Both  were obtained from \\textcite{Singer:1972aa}. ",
        "\n")
## ----





## ---- plot:t:2 ----
cow.d.2.plot
cow.d.2.plot.note <- paste(
        paste("{\\bf National Steel and Guns Production (log),", paste(paste(year.min.t2, "-",paste(year.max.t2, ".", sep=""), sep=""), "}", sep = ""), sep=" "),
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Variables are ``milper'' and ``irst.'' Both  were obtained from \\textcite{Singer:1972aa}. ",
        "\n")
## ----



################
# Simulation: 120 countries, 200 years
################

# https://www.rdocumentation.org/packages/tsDyn/versions/0.8-1/topics/VAR.sim
cat("\014")
rm(list=ls())
graphics.off()

if (!require("pacman")) install.packages("pacman"); library(pacman) 


## ---- var:sim:d ----
p_load(tsDyn)

# Simulation presented in Enders2014 p. 289
parameters.1 = 0.7
parameters.2 = 0.2
parameters = rep(c(parameters.1,parameters.2), 2)
years.sim = 200
parameters.m <- matrix(parameters, 2)

# var sims
set.seed(1);country.1 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(2);country.2 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(3);country.3 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(4);country.4 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(5);country.5 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(6);country.6 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(7);country.7 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(8);country.8 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(9);country.9 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(10);country.10 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(11);country.11 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(12);country.12 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(13);country.13 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(14);country.14 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(15);country.15 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(16);country.16 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(17);country.17 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(18);country.18 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(19);country.19 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(20);country.20 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(21);country.21 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(22);country.22 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(23);country.23 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(24);country.24 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(25);country.25 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(26);country.26 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(27);country.27 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(28);country.28 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(29);country.29 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(30);country.30 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(31);country.31 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(32);country.32 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(33);country.33 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(34);country.34 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(35);country.35 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(36);country.36 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(37);country.37 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(38);country.38 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(39);country.39 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(40);country.40 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(41);country.41 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(42);country.42 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(43);country.43 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(44);country.44 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(45);country.45 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(46);country.46 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(47);country.47 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(48);country.48 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(49);country.49 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(50);country.50 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(51);country.51 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(52);country.52 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(53);country.53 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(54);country.54 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(55);country.55 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(56);country.56 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(57);country.57 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(58);country.58 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(59);country.59 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(60);country.60 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(61);country.61 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(62);country.62 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(63);country.63 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(64);country.64 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(65);country.65 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(66);country.66 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(67);country.67 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(68);country.68 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(69);country.69 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(70);country.70 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(71);country.71 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(72);country.72 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(73);country.73 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(74);country.74 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(75);country.75 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(76);country.76 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(77);country.77 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(78);country.78 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(79);country.79 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(80);country.80 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(81);country.81 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(82);country.82 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(83);country.83 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(84);country.84 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(85);country.85 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(86);country.86 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(87);country.87 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(88);country.88 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(89);country.89 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(90);country.90 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(91);country.91 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(92);country.92 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(93);country.93 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(94);country.94 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(95);country.95 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(96);country.96 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(97);country.97 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(98);country.98 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(99);country.99 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(100);country.100 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(101);country.101 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(102);country.102 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(103);country.103 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(104);country.104 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(105);country.105 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(106);country.106 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(107);country.107 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(108);country.108 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(109);country.109 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(110);country.110 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(111);country.111 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(112);country.112 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(113);country.113 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(114);country.114 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(115);country.115 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(116);country.116 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(117);country.117 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(118);country.118 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(119);country.119 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(120);country.120 <- VAR.sim(B=parameters.m,n=years.sim,include="none")

# df's
country.1.d = data.frame(x = country.1[,1], y = country.1[,2],Country = "1",Time = 1:years.sim)
country.2.d = data.frame(x = country.2[,1], y = country.2[,2],Country = "2",Time = 1:years.sim)
country.3.d = data.frame(x = country.3[,1], y = country.3[,2],Country = "3",Time = 1:years.sim)
country.4.d = data.frame(x = country.4[,1], y = country.4[,2],Country = "4",Time = 1:years.sim)
country.5.d = data.frame(x = country.5[,1], y = country.5[,2],Country = "5",Time = 1:years.sim)
country.6.d = data.frame(x = country.6[,1], y = country.6[,2],Country = "6",Time = 1:years.sim)
country.7.d = data.frame(x = country.7[,1], y = country.7[,2],Country = "7",Time = 1:years.sim)
country.8.d = data.frame(x = country.8[,1], y = country.8[,2],Country = "8",Time = 1:years.sim)
country.9.d = data.frame(x = country.9[,1], y = country.9[,2],Country = "9",Time = 1:years.sim)
country.10.d = data.frame(x = country.10[,1], y = country.10[,2],Country = "10",Time = 1:years.sim)
country.11.d = data.frame(x = country.11[,1], y = country.11[,2],Country = "11",Time = 1:years.sim)
country.12.d = data.frame(x = country.12[,1], y = country.12[,2],Country = "12",Time = 1:years.sim)
country.13.d = data.frame(x = country.13[,1], y = country.13[,2],Country = "13",Time = 1:years.sim)
country.14.d = data.frame(x = country.14[,1], y = country.14[,2],Country = "14",Time = 1:years.sim)
country.15.d = data.frame(x = country.15[,1], y = country.15[,2],Country = "15",Time = 1:years.sim)
country.16.d = data.frame(x = country.16[,1], y = country.16[,2],Country = "16",Time = 1:years.sim)
country.17.d = data.frame(x = country.17[,1], y = country.17[,2],Country = "17",Time = 1:years.sim)
country.18.d = data.frame(x = country.18[,1], y = country.18[,2],Country = "18",Time = 1:years.sim)
country.19.d = data.frame(x = country.19[,1], y = country.19[,2],Country = "19",Time = 1:years.sim)
country.20.d = data.frame(x = country.20[,1], y = country.20[,2],Country = "20",Time = 1:years.sim)
country.21.d = data.frame(x = country.21[,1], y = country.21[,2],Country = "21",Time = 1:years.sim)
country.22.d = data.frame(x = country.22[,1], y = country.22[,2],Country = "22",Time = 1:years.sim)
country.23.d = data.frame(x = country.23[,1], y = country.23[,2],Country = "23",Time = 1:years.sim)
country.24.d = data.frame(x = country.24[,1], y = country.24[,2],Country = "24",Time = 1:years.sim)
country.25.d = data.frame(x = country.25[,1], y = country.25[,2],Country = "25",Time = 1:years.sim)
country.26.d = data.frame(x = country.26[,1], y = country.26[,2],Country = "26",Time = 1:years.sim)
country.27.d = data.frame(x = country.27[,1], y = country.27[,2],Country = "27",Time = 1:years.sim)
country.28.d = data.frame(x = country.28[,1], y = country.28[,2],Country = "28",Time = 1:years.sim)
country.29.d = data.frame(x = country.29[,1], y = country.29[,2],Country = "29",Time = 1:years.sim)
country.30.d = data.frame(x = country.30[,1], y = country.30[,2],Country = "30",Time = 1:years.sim)
country.31.d = data.frame(x = country.31[,1], y = country.31[,2],Country = "31",Time = 1:years.sim)
country.32.d = data.frame(x = country.32[,1], y = country.32[,2],Country = "32",Time = 1:years.sim)
country.33.d = data.frame(x = country.33[,1], y = country.33[,2],Country = "33",Time = 1:years.sim)
country.34.d = data.frame(x = country.34[,1], y = country.34[,2],Country = "34",Time = 1:years.sim)
country.35.d = data.frame(x = country.35[,1], y = country.35[,2],Country = "35",Time = 1:years.sim)
country.36.d = data.frame(x = country.36[,1], y = country.36[,2],Country = "36",Time = 1:years.sim)
country.37.d = data.frame(x = country.37[,1], y = country.37[,2],Country = "37",Time = 1:years.sim)
country.38.d = data.frame(x = country.38[,1], y = country.38[,2],Country = "38",Time = 1:years.sim)
country.39.d = data.frame(x = country.39[,1], y = country.39[,2],Country = "39",Time = 1:years.sim)
country.40.d = data.frame(x = country.40[,1], y = country.40[,2],Country = "40",Time = 1:years.sim)
country.41.d = data.frame(x = country.41[,1], y = country.41[,2],Country = "41",Time = 1:years.sim)
country.42.d = data.frame(x = country.42[,1], y = country.42[,2],Country = "42",Time = 1:years.sim)
country.43.d = data.frame(x = country.43[,1], y = country.43[,2],Country = "43",Time = 1:years.sim)
country.44.d = data.frame(x = country.44[,1], y = country.44[,2],Country = "44",Time = 1:years.sim)
country.45.d = data.frame(x = country.45[,1], y = country.45[,2],Country = "45",Time = 1:years.sim)
country.46.d = data.frame(x = country.46[,1], y = country.46[,2],Country = "46",Time = 1:years.sim)
country.47.d = data.frame(x = country.47[,1], y = country.47[,2],Country = "47",Time = 1:years.sim)
country.48.d = data.frame(x = country.48[,1], y = country.48[,2],Country = "48",Time = 1:years.sim)
country.49.d = data.frame(x = country.49[,1], y = country.49[,2],Country = "49",Time = 1:years.sim)
country.50.d = data.frame(x = country.50[,1], y = country.50[,2],Country = "50",Time = 1:years.sim)
country.51.d = data.frame(x = country.51[,1], y = country.51[,2],Country = "51",Time = 1:years.sim)
country.52.d = data.frame(x = country.52[,1], y = country.52[,2],Country = "52",Time = 1:years.sim)
country.53.d = data.frame(x = country.53[,1], y = country.53[,2],Country = "53",Time = 1:years.sim)
country.54.d = data.frame(x = country.54[,1], y = country.54[,2],Country = "54",Time = 1:years.sim)
country.55.d = data.frame(x = country.55[,1], y = country.55[,2],Country = "55",Time = 1:years.sim)
country.56.d = data.frame(x = country.56[,1], y = country.56[,2],Country = "56",Time = 1:years.sim)
country.57.d = data.frame(x = country.57[,1], y = country.57[,2],Country = "57",Time = 1:years.sim)
country.58.d = data.frame(x = country.58[,1], y = country.58[,2],Country = "58",Time = 1:years.sim)
country.59.d = data.frame(x = country.59[,1], y = country.59[,2],Country = "59",Time = 1:years.sim)
country.60.d = data.frame(x = country.60[,1], y = country.60[,2],Country = "60",Time = 1:years.sim)
country.61.d = data.frame(x = country.61[,1], y = country.61[,2],Country = "61",Time = 1:years.sim)
country.62.d = data.frame(x = country.62[,1], y = country.62[,2],Country = "62",Time = 1:years.sim)
country.63.d = data.frame(x = country.63[,1], y = country.63[,2],Country = "63",Time = 1:years.sim)
country.64.d = data.frame(x = country.64[,1], y = country.64[,2],Country = "64",Time = 1:years.sim)
country.65.d = data.frame(x = country.65[,1], y = country.65[,2],Country = "65",Time = 1:years.sim)
country.66.d = data.frame(x = country.66[,1], y = country.66[,2],Country = "66",Time = 1:years.sim)
country.67.d = data.frame(x = country.67[,1], y = country.67[,2],Country = "67",Time = 1:years.sim)
country.68.d = data.frame(x = country.68[,1], y = country.68[,2],Country = "68",Time = 1:years.sim)
country.69.d = data.frame(x = country.69[,1], y = country.69[,2],Country = "69",Time = 1:years.sim)
country.70.d = data.frame(x = country.70[,1], y = country.70[,2],Country = "70",Time = 1:years.sim)
country.71.d = data.frame(x = country.71[,1], y = country.71[,2],Country = "71",Time = 1:years.sim)
country.72.d = data.frame(x = country.72[,1], y = country.72[,2],Country = "72",Time = 1:years.sim)
country.73.d = data.frame(x = country.73[,1], y = country.73[,2],Country = "73",Time = 1:years.sim)
country.74.d = data.frame(x = country.74[,1], y = country.74[,2],Country = "74",Time = 1:years.sim)
country.75.d = data.frame(x = country.75[,1], y = country.75[,2],Country = "75",Time = 1:years.sim)
country.76.d = data.frame(x = country.76[,1], y = country.76[,2],Country = "76",Time = 1:years.sim)
country.77.d = data.frame(x = country.77[,1], y = country.77[,2],Country = "77",Time = 1:years.sim)
country.78.d = data.frame(x = country.78[,1], y = country.78[,2],Country = "78",Time = 1:years.sim)
country.79.d = data.frame(x = country.79[,1], y = country.79[,2],Country = "79",Time = 1:years.sim)
country.80.d = data.frame(x = country.80[,1], y = country.80[,2],Country = "80",Time = 1:years.sim)
country.81.d = data.frame(x = country.81[,1], y = country.81[,2],Country = "81",Time = 1:years.sim)
country.82.d = data.frame(x = country.82[,1], y = country.82[,2],Country = "82",Time = 1:years.sim)
country.83.d = data.frame(x = country.83[,1], y = country.83[,2],Country = "83",Time = 1:years.sim)
country.84.d = data.frame(x = country.84[,1], y = country.84[,2],Country = "84",Time = 1:years.sim)
country.85.d = data.frame(x = country.85[,1], y = country.85[,2],Country = "85",Time = 1:years.sim)
country.86.d = data.frame(x = country.86[,1], y = country.86[,2],Country = "86",Time = 1:years.sim)
country.87.d = data.frame(x = country.87[,1], y = country.87[,2],Country = "87",Time = 1:years.sim)
country.88.d = data.frame(x = country.88[,1], y = country.88[,2],Country = "88",Time = 1:years.sim)
country.89.d = data.frame(x = country.89[,1], y = country.89[,2],Country = "89",Time = 1:years.sim)
country.90.d = data.frame(x = country.90[,1], y = country.90[,2],Country = "90",Time = 1:years.sim)
country.91.d = data.frame(x = country.91[,1], y = country.91[,2],Country = "91",Time = 1:years.sim)
country.92.d = data.frame(x = country.92[,1], y = country.92[,2],Country = "92",Time = 1:years.sim)
country.93.d = data.frame(x = country.93[,1], y = country.93[,2],Country = "93",Time = 1:years.sim)
country.94.d = data.frame(x = country.94[,1], y = country.94[,2],Country = "94",Time = 1:years.sim)
country.95.d = data.frame(x = country.95[,1], y = country.95[,2],Country = "95",Time = 1:years.sim)
country.96.d = data.frame(x = country.96[,1], y = country.96[,2],Country = "96",Time = 1:years.sim)
country.97.d = data.frame(x = country.97[,1], y = country.97[,2],Country = "97",Time = 1:years.sim)
country.98.d = data.frame(x = country.98[,1], y = country.98[,2],Country = "98",Time = 1:years.sim)
country.99.d = data.frame(x = country.99[,1], y = country.99[,2],Country = "99",Time = 1:years.sim)
country.100.d = data.frame(x = country.100[,1], y = country.100[,2],Country = "100",Time = 1:years.sim)
country.101.d = data.frame(x = country.101[,1], y = country.101[,2],Country = "101",Time = 1:years.sim)
country.102.d = data.frame(x = country.102[,1], y = country.102[,2],Country = "102",Time = 1:years.sim)
country.103.d = data.frame(x = country.103[,1], y = country.103[,2],Country = "103",Time = 1:years.sim)
country.104.d = data.frame(x = country.104[,1], y = country.104[,2],Country = "104",Time = 1:years.sim)
country.105.d = data.frame(x = country.105[,1], y = country.105[,2],Country = "105",Time = 1:years.sim)
country.106.d = data.frame(x = country.106[,1], y = country.106[,2],Country = "106",Time = 1:years.sim)
country.107.d = data.frame(x = country.107[,1], y = country.107[,2],Country = "107",Time = 1:years.sim)
country.108.d = data.frame(x = country.108[,1], y = country.108[,2],Country = "108",Time = 1:years.sim)
country.109.d = data.frame(x = country.109[,1], y = country.109[,2],Country = "109",Time = 1:years.sim)
country.110.d = data.frame(x = country.110[,1], y = country.110[,2],Country = "110",Time = 1:years.sim)
country.111.d = data.frame(x = country.111[,1], y = country.111[,2],Country = "111",Time = 1:years.sim)
country.112.d = data.frame(x = country.112[,1], y = country.112[,2],Country = "112",Time = 1:years.sim)
country.113.d = data.frame(x = country.113[,1], y = country.113[,2],Country = "113",Time = 1:years.sim)
country.114.d = data.frame(x = country.114[,1], y = country.114[,2],Country = "114",Time = 1:years.sim)
country.115.d = data.frame(x = country.115[,1], y = country.115[,2],Country = "115",Time = 1:years.sim)
country.116.d = data.frame(x = country.116[,1], y = country.116[,2],Country = "116",Time = 1:years.sim)
country.117.d = data.frame(x = country.117[,1], y = country.117[,2],Country = "117",Time = 1:years.sim)
country.118.d = data.frame(x = country.118[,1], y = country.118[,2],Country = "118",Time = 1:years.sim)
country.119.d = data.frame(x = country.119[,1], y = country.119[,2],Country = "119",Time = 1:years.sim)
country.120.d = data.frame(x = country.120[,1], y = country.120[,2],Country = "120",Time = 1:years.sim)

country.var.d = rbind(country.1.d, country.2.d, country.3.d, country.4.d, country.5.d, country.6.d, country.7.d, country.8.d, country.9.d, country.10.d, country.11.d, country.12.d, country.13.d, country.14.d, country.15.d, country.16.d, country.17.d, country.18.d, country.19.d, country.20.d, country.21.d, country.22.d, country.23.d, country.24.d, country.25.d, country.26.d, country.27.d, country.28.d, country.29.d, country.30.d, country.31.d, country.32.d, country.33.d, country.34.d, country.35.d, country.36.d, country.37.d, country.38.d, country.39.d, country.40.d, country.41.d, country.42.d, country.43.d, country.44.d, country.45.d, country.46.d, country.47.d, country.48.d, country.49.d, country.50.d, country.51.d, country.52.d, country.53.d, country.54.d, country.55.d, country.56.d, country.57.d, country.58.d, country.59.d, country.60.d, country.61.d, country.62.d, country.63.d, country.64.d, country.65.d, country.66.d, country.67.d, country.68.d, country.69.d, country.70.d, country.71.d, country.72.d, country.73.d, country.74.d, country.75.d, country.76.d, country.77.d, country.78.d, country.79.d, country.80.d, country.81.d, country.82.d, country.83.d, country.84.d, country.85.d, country.86.d, country.87.d, country.88.d, country.89.d, country.90.d, country.91.d, country.92.d, country.93.d, country.94.d, country.95.d, country.96.d, country.97.d, country.98.d, country.99.d, country.100.d, country.101.d, country.102.d, country.103.d, country.104.d, country.105.d, country.106.d, country.107.d, country.108.d, country.109.d, country.110.d, country.111.d, country.112.d, country.113.d, country.114.d, country.115.d, country.116.d, country.117.d, country.118.d, country.119.d, country.120.d)

# rename colnames
colnames(country.var.d) <- c("x", "y", "ID", "Time")

# reorder colnames
country.var.d = country.var.d[, c("ID", "Time", "x", "y")]

# Reformat time variable
country.var.d$Time = as.character(country.var.d$Time)
country.var.d$Time = as.Date(country.var.d$Time,"%Y")
country.var.d$Time <- as.POSIXct(country.var.d$Time, origin=min(country.var.d$Time), tz = "GMT",  tryFormats ="%Y", optional = T)

# plot
p_load(ggplot2,reshape)
country.var.d.plot = melt(country.var.d, id.vars = c("ID", "Time"))

country.var.d.plot$ID = factor(country.var.d.plot$ID, levels = c( "1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",  "10",  "11",  "12",  "13",  "14",  "15",  "16",  "17",  "18",  "19",  "20",  "21",  "22",  "23",  "24",  "25",  "26",  "27",  "28",  "29",  "30",  "31",  "32",  "33",  "34",  "35",  "36",  "37",  "38",  "39",  "40",  "41",  "42",  "43",  "44",  "45",  "46",  "47",  "48",  "49",  "50",  "51",  "52",  "53",  "54",  "55",  "56",  "57",  "58",  "59",  "60",  "61",  "62",  "63",  "64",  "65",  "66",  "67",  "68",  "69",  "70",  "71",  "72",  "73",  "74",  "75",  "76",  "77",  "78",  "79",  "80",  "81",  "82",  "83",  "84",  "85",  "86",  "87",  "88",  "89",  "90",  "91",  "92",  "93",  "94",  "95",  "96",  "97",  "98",  "99",  "100",  "101",  "102",  "103",  "104",  "105",  "106",  "107",  "108",  "109",  "110",  "111",  "112",  "113",  "114",  "115",  "116",  "117",  "118",  "119",  "120"))


sim.var.plot = ggplot(country.var.d.plot,
                      aes(x = Time, y = value, colour = variable)) +
        geom_line(alpha=0.5) +
        facet_wrap(~ ID, ncol = 6, scales="free_y") +
        xlab("Year") + 
        ylab("Simulated VAR Processes") +
        theme_bw() +
        theme(legend.position="bottom", legend.direction="horizontal")  +
        theme(axis.text.y = element_text(size=3), 
              axis.text.x = element_text(size=3), 
              axis.title.y = element_text(size=6), 
              axis.title.x = element_text(size=6), 
              legend.text=element_text(size=6), 
              legend.title=element_text(size=0),
              plot.title = element_text(size=3),
              legend.position="bottom",
              legend.key.size = unit(0.5,"cm"),
              legend.spacing.x = unit(0.3, 'cm'),
              strip.text.x = element_text(size = 4))
## ---- 


# plot
## ---- plot:var:sim ----
sim.var.plot
sim.var.plot.note <- paste(
        paste(paste("Simulated VAR Processes:", paste(years.sim, paste("years", ",", sep = ""),  sep = " "), paste(length(unique(country.var.d$Country)), "countries.", sep = " "))),
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}:", paste("The figure shows", length(unique(country.var.d$Country)), "simulated VAR processes (countries). Each process lasts for", years.sim, "years. Following \\textcite[286]{Enders2014}, we set in \\autoref{eq:var:enders} $\\alpha_{10}=\\alpha_{20} = 0, \\beta_{11}=\\beta_{22}=$", parameters.1, "and $\\beta_{12}=\\beta_{21} =$", paste(parameters.2, ".", sep = ""))),
        "\n")
## ---- 


## ---- w:sim ----
# Sim W: 1-point-increasing average of dyadic trade
m1 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m1[, ] <- c(rnorm(120,1,1))}; diag(m1) <- 0
m2 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m2[i,] <- c(rnorm(120,2,1))}; diag(m2) <- 0
m3 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m3[i,] <- c(rnorm(120,3,1))}; diag(m3) <- 0
m4 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m4[i,] <- c(rnorm(120,4,1))}; diag(m4) <- 0
m5 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m5[i,] <- c(rnorm(120,5,1))}; diag(m5) <- 0
m6 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m6[i,] <- c(rnorm(120,6,1))}; diag(m6) <- 0
m7 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m7[i,] <- c(rnorm(120,7,1))}; diag(m7) <- 0
m8 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m8[i,] <- c(rnorm(120,8,1))}; diag(m8) <- 0
m9 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m9[i,] <- c(rnorm(120,9,1))}; diag(m9) <- 0
m10 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m10[i,] <- c(rnorm(120,10,1))}; diag(m10) <- 0
m11 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m11[i,] <- c(rnorm(120,11,1))}; diag(m11) <- 0
m12 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m12[i,] <- c(rnorm(120,12,1))}; diag(m12) <- 0
m13 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m13[i,] <- c(rnorm(120,13,1))}; diag(m13) <- 0
m14 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m14[i,] <- c(rnorm(120,14,1))}; diag(m14) <- 0
m15 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m15[i,] <- c(rnorm(120,15,1))}; diag(m15) <- 0
m16 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m16[i,] <- c(rnorm(120,16,1))}; diag(m16) <- 0
m17 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m17[i,] <- c(rnorm(120,17,1))}; diag(m17) <- 0
m18 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m18[i,] <- c(rnorm(120,18,1))}; diag(m18) <- 0
m19 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m19[i,] <- c(rnorm(120,19,1))}; diag(m19) <- 0
m20 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m20[i,] <- c(rnorm(120,20,1))}; diag(m20) <- 0
m21 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m21[i,] <- c(rnorm(120,21,1))}; diag(m21) <- 0
m22 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m22[i,] <- c(rnorm(120,22,1))}; diag(m22) <- 0
m23 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m23[i,] <- c(rnorm(120,23,1))}; diag(m23) <- 0
m24 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m24[i,] <- c(rnorm(120,24,1))}; diag(m24) <- 0
m25 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m25[i,] <- c(rnorm(120,25,1))}; diag(m25) <- 0
m26 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m26[i,] <- c(rnorm(120,26,1))}; diag(m26) <- 0
m27 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m27[i,] <- c(rnorm(120,27,1))}; diag(m27) <- 0
m28 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m28[i,] <- c(rnorm(120,28,1))}; diag(m28) <- 0
m29 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m29[i,] <- c(rnorm(120,29,1))}; diag(m29) <- 0
m30 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m30[i,] <- c(rnorm(120,30,1))}; diag(m30) <- 0
m31 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m31[i,] <- c(rnorm(120,31,1))}; diag(m31) <- 0
m32 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m32[i,] <- c(rnorm(120,32,1))}; diag(m32) <- 0
m33 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m33[i,] <- c(rnorm(120,33,1))}; diag(m33) <- 0
m34 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m34[i,] <- c(rnorm(120,34,1))}; diag(m34) <- 0
m35 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m35[i,] <- c(rnorm(120,35,1))}; diag(m35) <- 0
m36 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m36[i,] <- c(rnorm(120,36,1))}; diag(m36) <- 0
m37 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m37[i,] <- c(rnorm(120,37,1))}; diag(m37) <- 0
m38 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m38[i,] <- c(rnorm(120,38,1))}; diag(m38) <- 0
m39 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m39[i,] <- c(rnorm(120,39,1))}; diag(m39) <- 0
m40 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m40[i,] <- c(rnorm(120,40,1))}; diag(m40) <- 0
m41 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m41[i,] <- c(rnorm(120,41,1))}; diag(m41) <- 0
m42 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m42[i,] <- c(rnorm(120,42,1))}; diag(m42) <- 0
m43 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m43[i,] <- c(rnorm(120,43,1))}; diag(m43) <- 0
m44 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m44[i,] <- c(rnorm(120,44,1))}; diag(m44) <- 0
m45 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m45[i,] <- c(rnorm(120,45,1))}; diag(m45) <- 0
m46 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m46[i,] <- c(rnorm(120,46,1))}; diag(m46) <- 0
m47 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m47[i,] <- c(rnorm(120,47,1))}; diag(m47) <- 0
m48 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m48[i,] <- c(rnorm(120,48,1))}; diag(m48) <- 0
m49 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m49[i,] <- c(rnorm(120,49,1))}; diag(m49) <- 0
m50 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m50[i,] <- c(rnorm(120,50,1))}; diag(m50) <- 0
m51 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m51[i,] <- c(rnorm(120,51,1))}; diag(m51) <- 0
m52 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m52[i,] <- c(rnorm(120,52,1))}; diag(m52) <- 0
m53 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m53[i,] <- c(rnorm(120,53,1))}; diag(m53) <- 0
m54 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m54[i,] <- c(rnorm(120,54,1))}; diag(m54) <- 0
m55 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m55[i,] <- c(rnorm(120,55,1))}; diag(m55) <- 0
m56 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m56[i,] <- c(rnorm(120,56,1))}; diag(m56) <- 0
m57 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m57[i,] <- c(rnorm(120,57,1))}; diag(m57) <- 0
m58 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m58[i,] <- c(rnorm(120,58,1))}; diag(m58) <- 0
m59 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m59[i,] <- c(rnorm(120,59,1))}; diag(m59) <- 0
m60 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m60[i,] <- c(rnorm(120,60,1))}; diag(m60) <- 0
m61 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m61[i,] <- c(rnorm(120,61,1))}; diag(m61) <- 0
m62 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m62[i,] <- c(rnorm(120,62,1))}; diag(m62) <- 0
m63 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m63[i,] <- c(rnorm(120,63,1))}; diag(m63) <- 0
m64 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m64[i,] <- c(rnorm(120,64,1))}; diag(m64) <- 0
m65 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m65[i,] <- c(rnorm(120,65,1))}; diag(m65) <- 0
m66 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m66[i,] <- c(rnorm(120,66,1))}; diag(m66) <- 0
m67 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m67[i,] <- c(rnorm(120,67,1))}; diag(m67) <- 0
m68 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m68[i,] <- c(rnorm(120,68,1))}; diag(m68) <- 0
m69 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m69[i,] <- c(rnorm(120,69,1))}; diag(m69) <- 0
m70 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m70[i,] <- c(rnorm(120,70,1))}; diag(m70) <- 0
m71 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m71[i,] <- c(rnorm(120,71,1))}; diag(m71) <- 0
m72 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m72[i,] <- c(rnorm(120,72,1))}; diag(m72) <- 0
m73 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m73[i,] <- c(rnorm(120,73,1))}; diag(m73) <- 0
m74 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m74[i,] <- c(rnorm(120,74,1))}; diag(m74) <- 0
m75 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m75[i,] <- c(rnorm(120,75,1))}; diag(m75) <- 0
m76 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m76[i,] <- c(rnorm(120,76,1))}; diag(m76) <- 0
m77 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m77[i,] <- c(rnorm(120,77,1))}; diag(m77) <- 0
m78 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m78[i,] <- c(rnorm(120,78,1))}; diag(m78) <- 0
m79 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m79[i,] <- c(rnorm(120,79,1))}; diag(m79) <- 0
m80 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m80[i,] <- c(rnorm(120,80,1))}; diag(m80) <- 0
m81 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m81[i,] <- c(rnorm(120,81,1))}; diag(m81) <- 0
m82 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m82[i,] <- c(rnorm(120,82,1))}; diag(m82) <- 0
m83 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m83[i,] <- c(rnorm(120,83,1))}; diag(m83) <- 0
m84 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m84[i,] <- c(rnorm(120,84,1))}; diag(m84) <- 0
m85 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m85[i,] <- c(rnorm(120,85,1))}; diag(m85) <- 0
m86 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m86[i,] <- c(rnorm(120,86,1))}; diag(m86) <- 0
m87 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m87[i,] <- c(rnorm(120,87,1))}; diag(m87) <- 0
m88 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m88[i,] <- c(rnorm(120,88,1))}; diag(m88) <- 0
m89 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m89[i,] <- c(rnorm(120,89,1))}; diag(m89) <- 0
m90 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m90[i,] <- c(rnorm(120,90,1))}; diag(m90) <- 0
m91 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m91[i,] <- c(rnorm(120,91,1))}; diag(m91) <- 0
m92 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m92[i,] <- c(rnorm(120,92,1))}; diag(m92) <- 0
m93 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m93[i,] <- c(rnorm(120,93,1))}; diag(m93) <- 0
m94 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m94[i,] <- c(rnorm(120,94,1))}; diag(m94) <- 0
m95 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m95[i,] <- c(rnorm(120,95,1))}; diag(m95) <- 0
m96 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m96[i,] <- c(rnorm(120,96,1))}; diag(m96) <- 0
m97 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m97[i,] <- c(rnorm(120,97,1))}; diag(m97) <- 0
m98 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m98[i,] <- c(rnorm(120,98,1))}; diag(m98) <- 0
m99 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m99[i,] <- c(rnorm(120,99,1))}; diag(m99) <- 0
m100 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m100[i,] <- c(rnorm(120,100,1))}; diag(m100) <- 0
m101 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m101[i,] <- c(rnorm(120,101,1))}; diag(m101) <- 0
m102 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m102[i,] <- c(rnorm(120,102,1))}; diag(m102) <- 0
m103 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m103[i,] <- c(rnorm(120,103,1))}; diag(m103) <- 0
m104 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m104[i,] <- c(rnorm(120,104,1))}; diag(m104) <- 0
m105 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m105[i,] <- c(rnorm(120,105,1))}; diag(m105) <- 0
m106 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m106[i,] <- c(rnorm(120,106,1))}; diag(m106) <- 0
m107 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m107[i,] <- c(rnorm(120,107,1))}; diag(m107) <- 0
m108 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m108[i,] <- c(rnorm(120,108,1))}; diag(m108) <- 0
m109 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m109[i,] <- c(rnorm(120,109,1))}; diag(m109) <- 0
m110 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m110[i,] <- c(rnorm(120,110,1))}; diag(m110) <- 0
m111 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m111[i,] <- c(rnorm(120,111,1))}; diag(m111) <- 0
m112 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m112[i,] <- c(rnorm(120,112,1))}; diag(m112) <- 0
m113 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m113[i,] <- c(rnorm(120,113,1))}; diag(m113) <- 0
m114 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m114[i,] <- c(rnorm(120,114,1))}; diag(m114) <- 0
m115 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m115[i,] <- c(rnorm(120,115,1))}; diag(m115) <- 0
m116 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m116[i,] <- c(rnorm(120,116,1))}; diag(m116) <- 0
m117 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m117[i,] <- c(rnorm(120,117,1))}; diag(m117) <- 0
m118 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m118[i,] <- c(rnorm(120,118,1))}; diag(m118) <- 0
m119 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m119[i,] <- c(rnorm(120,119,1))}; diag(m119) <- 0
m120 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m120[i,] <- c(rnorm(120,120,1))}; diag(m120) <- 0
m121 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m121[i,] <- c(rnorm(120,121,1))}; diag(m121) <- 0
m122 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m122[i,] <- c(rnorm(120,122,1))}; diag(m122) <- 0
m123 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m123[i,] <- c(rnorm(120,123,1))}; diag(m123) <- 0
m124 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m124[i,] <- c(rnorm(120,124,1))}; diag(m124) <- 0
m125 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m125[i,] <- c(rnorm(120,125,1))}; diag(m125) <- 0
m126 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m126[i,] <- c(rnorm(120,126,1))}; diag(m126) <- 0
m127 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m127[i,] <- c(rnorm(120,127,1))}; diag(m127) <- 0
m128 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m128[i,] <- c(rnorm(120,128,1))}; diag(m128) <- 0
m129 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m129[i,] <- c(rnorm(120,129,1))}; diag(m129) <- 0
m130 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m130[i,] <- c(rnorm(120,130,1))}; diag(m130) <- 0
m131 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m131[i,] <- c(rnorm(120,131,1))}; diag(m131) <- 0
m132 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m132[i,] <- c(rnorm(120,132,1))}; diag(m132) <- 0
m133 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m133[i,] <- c(rnorm(120,133,1))}; diag(m133) <- 0
m134 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m134[i,] <- c(rnorm(120,134,1))}; diag(m134) <- 0
m135 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m135[i,] <- c(rnorm(120,135,1))}; diag(m135) <- 0
m136 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m136[i,] <- c(rnorm(120,136,1))}; diag(m136) <- 0
m137 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m137[i,] <- c(rnorm(120,137,1))}; diag(m137) <- 0
m138 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m138[i,] <- c(rnorm(120,138,1))}; diag(m138) <- 0
m139 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m139[i,] <- c(rnorm(120,139,1))}; diag(m139) <- 0
m140 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m140[i,] <- c(rnorm(120,140,1))}; diag(m140) <- 0
m141 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m141[i,] <- c(rnorm(120,141,1))}; diag(m141) <- 0
m142 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m142[i,] <- c(rnorm(120,142,1))}; diag(m142) <- 0
m143 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m143[i,] <- c(rnorm(120,143,1))}; diag(m143) <- 0
m144 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m144[i,] <- c(rnorm(120,144,1))}; diag(m144) <- 0
m145 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m145[i,] <- c(rnorm(120,145,1))}; diag(m145) <- 0
m146 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m146[i,] <- c(rnorm(120,146,1))}; diag(m146) <- 0
m147 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m147[i,] <- c(rnorm(120,147,1))}; diag(m147) <- 0
m148 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m148[i,] <- c(rnorm(120,148,1))}; diag(m148) <- 0
m149 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m149[i,] <- c(rnorm(120,149,1))}; diag(m149) <- 0
m150 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m150[i,] <- c(rnorm(120,150,1))}; diag(m150) <- 0
m151 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m151[i,] <- c(rnorm(120,151,1))}; diag(m151) <- 0
m152 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m152[i,] <- c(rnorm(120,152,1))}; diag(m152) <- 0
m153 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m153[i,] <- c(rnorm(120,153,1))}; diag(m153) <- 0
m154 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m154[i,] <- c(rnorm(120,154,1))}; diag(m154) <- 0
m155 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m155[i,] <- c(rnorm(120,155,1))}; diag(m155) <- 0
m156 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m156[i,] <- c(rnorm(120,156,1))}; diag(m156) <- 0
m157 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m157[i,] <- c(rnorm(120,157,1))}; diag(m157) <- 0
m158 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m158[i,] <- c(rnorm(120,158,1))}; diag(m158) <- 0
m159 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m159[i,] <- c(rnorm(120,159,1))}; diag(m159) <- 0
m160 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m160[i,] <- c(rnorm(120,160,1))}; diag(m160) <- 0
m161 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m161[i,] <- c(rnorm(120,161,1))}; diag(m161) <- 0
m162 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m162[i,] <- c(rnorm(120,162,1))}; diag(m162) <- 0
m163 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m163[i,] <- c(rnorm(120,163,1))}; diag(m163) <- 0
m164 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m164[i,] <- c(rnorm(120,164,1))}; diag(m164) <- 0
m165 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m165[i,] <- c(rnorm(120,165,1))}; diag(m165) <- 0
m166 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m166[i,] <- c(rnorm(120,166,1))}; diag(m166) <- 0
m167 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m167[i,] <- c(rnorm(120,167,1))}; diag(m167) <- 0
m168 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m168[i,] <- c(rnorm(120,168,1))}; diag(m168) <- 0
m169 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m169[i,] <- c(rnorm(120,169,1))}; diag(m169) <- 0
m170 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m170[i,] <- c(rnorm(120,170,1))}; diag(m170) <- 0
m171 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m171[i,] <- c(rnorm(120,171,1))}; diag(m171) <- 0
m172 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m172[i,] <- c(rnorm(120,172,1))}; diag(m172) <- 0
m173 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m173[i,] <- c(rnorm(120,173,1))}; diag(m173) <- 0
m174 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m174[i,] <- c(rnorm(120,174,1))}; diag(m174) <- 0
m175 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m175[i,] <- c(rnorm(120,175,1))}; diag(m175) <- 0
m176 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m176[i,] <- c(rnorm(120,176,1))}; diag(m176) <- 0
m177 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m177[i,] <- c(rnorm(120,177,1))}; diag(m177) <- 0
m178 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m178[i,] <- c(rnorm(120,178,1))}; diag(m178) <- 0
m179 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m179[i,] <- c(rnorm(120,179,1))}; diag(m179) <- 0
m180 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m180[i,] <- c(rnorm(120,180,1))}; diag(m180) <- 0
m181 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m181[i,] <- c(rnorm(120,181,1))}; diag(m181) <- 0
m182 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m182[i,] <- c(rnorm(120,182,1))}; diag(m182) <- 0
m183 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m183[i,] <- c(rnorm(120,183,1))}; diag(m183) <- 0
m184 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m184[i,] <- c(rnorm(120,184,1))}; diag(m184) <- 0
m185 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m185[i,] <- c(rnorm(120,185,1))}; diag(m185) <- 0
m186 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m186[i,] <- c(rnorm(120,186,1))}; diag(m186) <- 0
m187 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m187[i,] <- c(rnorm(120,187,1))}; diag(m187) <- 0
m188 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m188[i,] <- c(rnorm(120,188,1))}; diag(m188) <- 0
m189 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m189[i,] <- c(rnorm(120,189,1))}; diag(m189) <- 0
m190 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m190[i,] <- c(rnorm(120,190,1))}; diag(m190) <- 0
m191 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m191[i,] <- c(rnorm(120,191,1))}; diag(m191) <- 0
m192 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m192[i,] <- c(rnorm(120,192,1))}; diag(m192) <- 0
m193 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m193[i,] <- c(rnorm(120,193,1))}; diag(m193) <- 0
m194 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m194[i,] <- c(rnorm(120,194,1))}; diag(m194) <- 0
m195 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m195[i,] <- c(rnorm(120,195,1))}; diag(m195) <- 0
m196 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m196[i,] <- c(rnorm(120,196,1))}; diag(m196) <- 0
m197 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m197[i,] <- c(rnorm(120,197,1))}; diag(m197) <- 0
m198 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m198[i,] <- c(rnorm(120,198,1))}; diag(m198) <- 0
m199 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m199[i,] <- c(rnorm(120,199,1))}; diag(m199) <- 0
m200 <- matrix(NA, nrow=120, ncol=120); for(i in 1:120){m200[i,] <- c(rnorm(120,200,1))}; diag(m200) <- 0



# W
sim.w.matrix = list(as.matrix(m1), as.matrix(m2), as.matrix(m3), as.matrix(m4), as.matrix(m5), as.matrix(m6), as.matrix(m7), as.matrix(m8), as.matrix(m9), as.matrix(m10), as.matrix(m11), as.matrix(m12), as.matrix(m13), as.matrix(m14), as.matrix(m15), as.matrix(m16), as.matrix(m17), as.matrix(m18), as.matrix(m19), as.matrix(m20), as.matrix(m21), as.matrix(m22), as.matrix(m23), as.matrix(m24), as.matrix(m25), as.matrix(m26), as.matrix(m27), as.matrix(m28), as.matrix(m29), as.matrix(m30), as.matrix(m31), as.matrix(m32), as.matrix(m33), as.matrix(m34), as.matrix(m35), as.matrix(m36), as.matrix(m37), as.matrix(m38), as.matrix(m39), as.matrix(m40), as.matrix(m41), as.matrix(m42), as.matrix(m43), as.matrix(m44), as.matrix(m45), as.matrix(m46), as.matrix(m47), as.matrix(m48), as.matrix(m49), as.matrix(m50), as.matrix(m51), as.matrix(m52), as.matrix(m53), as.matrix(m54), as.matrix(m55), as.matrix(m56), as.matrix(m57), as.matrix(m58), as.matrix(m59), as.matrix(m60), as.matrix(m61), as.matrix(m62), as.matrix(m63), as.matrix(m64), as.matrix(m65), as.matrix(m66), as.matrix(m67), as.matrix(m68), as.matrix(m69), as.matrix(m70), as.matrix(m71), as.matrix(m72), as.matrix(m73), as.matrix(m74), as.matrix(m75), as.matrix(m76), as.matrix(m77), as.matrix(m78), as.matrix(m79), as.matrix(m80), as.matrix(m81), as.matrix(m82), as.matrix(m83), as.matrix(m84), as.matrix(m85), as.matrix(m86), as.matrix(m87), as.matrix(m88), as.matrix(m89), as.matrix(m90), as.matrix(m91), as.matrix(m92), as.matrix(m93), as.matrix(m94), as.matrix(m95), as.matrix(m96), as.matrix(m97), as.matrix(m98), as.matrix(m99), as.matrix(m100), as.matrix(m101), as.matrix(m102), as.matrix(m103), as.matrix(m104), as.matrix(m105), as.matrix(m106), as.matrix(m107), as.matrix(m108), as.matrix(m109), as.matrix(m110), as.matrix(m111), as.matrix(m112), as.matrix(m113), as.matrix(m114), as.matrix(m115), as.matrix(m116), as.matrix(m117), as.matrix(m118), as.matrix(m119), as.matrix(m120), as.matrix(m121), as.matrix(m122), as.matrix(m123), as.matrix(m124), as.matrix(m125), as.matrix(m126), as.matrix(m127), as.matrix(m128), as.matrix(m129), as.matrix(m130), as.matrix(m131), as.matrix(m132), as.matrix(m133), as.matrix(m134), as.matrix(m135), as.matrix(m136), as.matrix(m137), as.matrix(m138), as.matrix(m139), as.matrix(m140), as.matrix(m141), as.matrix(m142), as.matrix(m143), as.matrix(m144), as.matrix(m145), as.matrix(m146), as.matrix(m147), as.matrix(m148), as.matrix(m149), as.matrix(m150), as.matrix(m151), as.matrix(m152), as.matrix(m153), as.matrix(m154), as.matrix(m155), as.matrix(m156), as.matrix(m157), as.matrix(m158), as.matrix(m159), as.matrix(m160), as.matrix(m161), as.matrix(m162), as.matrix(m163), as.matrix(m164), as.matrix(m165), as.matrix(m166), as.matrix(m167), as.matrix(m168), as.matrix(m169), as.matrix(m170), as.matrix(m171), as.matrix(m172), as.matrix(m173), as.matrix(m174), as.matrix(m175), as.matrix(m176), as.matrix(m177), as.matrix(m178), as.matrix(m179), as.matrix(m180), as.matrix(m181), as.matrix(m182), as.matrix(m183), as.matrix(m184), as.matrix(m185), as.matrix(m186), as.matrix(m187), as.matrix(m188), as.matrix(m189), as.matrix(m190), as.matrix(m191), as.matrix(m192), as.matrix(m193), as.matrix(m194), as.matrix(m195), as.matrix(m196), as.matrix(m197), as.matrix(m198), as.matrix(m199), as.matrix(m200))
## ----




# Sims: different packages


## GVAR
p_load(GVARX)


p.1.sim=2 # The number of lag for Xt matrix
FLag.1.sim=2 # The number of lag for foreign variables in country-specific VAR
lag.max.sim=5 # The maximal number of lag for estimating country-specific VAR
type.sim="none" # Model specification for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.sim="AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

# determinsitic component for the paper
type.sim.paper = as.character(ifelse(type.sim=="both", as.character("Trend and constant"), ifelse(type.sim == "trend", as.character("Trend"), ifelse(type.sim == "constant", "Constant", NA ))))


options(scipen=9999999)
mainOUTPUT.sim = GVECMest(
        data = country.var.d,
        p = p.1.sim,
        FLag = FLag.1.sim,
        lag.max = lag.max.sim,
        type = type.sim,
        ic = ic.sim,
        weight.matrix=sim.w.matrix)


summary(mainOUTPUT.sim$gvecm[[2]]) 



################
# ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
writeLines(paste("Power remains a crucial concept in international relations. In recent decades, the prevailing notion in the literature explains that economic power is a prerequisite for all other forms of power (military, political and cultural). Yet, such an assumption has never been properly tested. To test this assumption, the paper introduces a new time-series method to political science---Global Vector Auto-regression (GVAR). While the method is widely used in economics, it has not been employed in political science. The method should be appealing to scholars in political science since it enables big-N and big-T hypotheses tests. We also present Granger-causality tests within the context of GVAR and test if economic power is a prerequisite for military power. Our results suggest that the role of the economy has changed throughout history. Namely, in 19th century it was the military power that drove (Granger-caused) the economy; yet, since ", as.character(format(min(min(cow.d.2$Time), min(cow.d.2.B$Time)), format="%Y")), "the roles are reversed."), fileConn)
close(fileConn)
## ----
