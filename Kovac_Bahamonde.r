cat("\014")
rm(list=ls())
graphics.off()


##############################
# Yearly weight data for weight matrix
##############################
# cat("\014")
# rm(list=ls())

# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

p_load(foreign)
bilateral.d <- read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/Dyadic_COW_4.0.csv") 

# Generating another column with the absolute market trade distance between the two countries
options(scipen=1000000) 
bilateral.d$trade = as.numeric(round(abs(bilateral.d$flow1-bilateral.d$flow2), 4))

# keeping columns i'll need
bilateral.d <- bilateral.d[c("year", "importer1", "importer2", "trade")]


# Others adjustments PENDING

# 1. Rename "West Germany" to "Germany" only for 1946-1989 period.
# bilateral.d$importer1[bilateral.d$importer1 == "German Federal Republic"] <- "Germany"
# bilateral.d$importer2[bilateral.d$importer2 == "German Federal Republic"] <- "Germany"

trade = bilateral.d


# Loop to Generate Independent XLSX Files (one per year) contanining the square country matrices.
p_load(dplyr)
p_load(igraph)

setwd("~/RU/research/Bahamonde_Kovac/matrix")

sequence <- seq.int(min(trade$year),max(trade$year),1)

for ( i in sequence){
        filtracion <- filter(trade,year==i)
        d <- data.frame(filtracion$importer1,filtracion$importer2)
        g <- graph_from_data_frame(d, directed = FALSE, vertices = NULL)
        E(g)$weight <- filtracion$trade
        ad_matrix <- get.adjacency(g, attr="weight", sparse = F)
        file_name <- paste(c("trade_year_",i,".csv"), collapse = "")
        print(i)
        write.csv(ad_matrix,file_name)
        }

##############################
## First Period: 1871 to 1913
##############################

# Import CVSs
y.1871 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1871.csv"))
y.1872 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1872.csv"))
y.1873 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1873.csv"))
y.1874 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1874.csv"))
y.1875 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1875.csv"))
y.1876 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1876.csv"))
y.1877 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1877.csv"))
y.1878 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1878.csv"))
y.1879 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1879.csv"))
y.1880 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1880.csv"))
y.1881 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1881.csv"))
y.1882 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1882.csv"))
y.1883 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1883.csv"))
y.1884 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1884.csv"))
y.1885 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1885.csv"))
y.1886 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1886.csv"))
y.1887 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1887.csv"))
y.1888 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1888.csv"))
y.1889 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1889.csv"))
y.1890 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1890.csv"))
y.1891 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1891.csv"))
y.1892 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1892.csv"))
y.1893 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1893.csv"))
y.1894 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1894.csv"))
y.1895 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1895.csv"))
y.1896 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1896.csv"))
y.1897 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1897.csv"))
y.1898 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1898.csv"))
y.1899 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1899.csv"))
y.1900 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1900.csv"))
y.1901 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1901.csv"))
y.1902 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1902.csv"))
y.1903 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1903.csv"))
y.1904 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1904.csv"))
y.1905 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1905.csv"))
y.1906 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1906.csv"))
y.1907 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1907.csv"))
y.1908 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1908.csv"))
y.1909 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1909.csv"))
y.1910 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1910.csv"))
y.1911 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1911.csv"))
y.1912 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1912.csv"))
y.1913 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1913.csv"))


# 1. Sort column X (with countries) and Delete column X
y.1871 = y.1871[order(y.1871$X), ] ; y.1871$X <- NULL
y.1872 = y.1872[order(y.1872$X), ] ; y.1872$X <- NULL
y.1873 = y.1873[order(y.1873$X), ] ; y.1873$X <- NULL
y.1874 = y.1874[order(y.1874$X), ] ; y.1874$X <- NULL
y.1875 = y.1875[order(y.1875$X), ] ; y.1875$X <- NULL
y.1876 = y.1876[order(y.1876$X), ] ; y.1876$X <- NULL
y.1877 = y.1877[order(y.1877$X), ] ; y.1877$X <- NULL
y.1878 = y.1878[order(y.1878$X), ] ; y.1878$X <- NULL
y.1879 = y.1879[order(y.1879$X), ] ; y.1879$X <- NULL
y.1880 = y.1880[order(y.1880$X), ] ; y.1880$X <- NULL
y.1881 = y.1881[order(y.1881$X), ] ; y.1881$X <- NULL
y.1882 = y.1882[order(y.1882$X), ] ; y.1882$X <- NULL
y.1883 = y.1883[order(y.1883$X), ] ; y.1883$X <- NULL
y.1884 = y.1884[order(y.1884$X), ] ; y.1884$X <- NULL
y.1885 = y.1885[order(y.1885$X), ] ; y.1885$X <- NULL
y.1886 = y.1886[order(y.1886$X), ] ; y.1886$X <- NULL
y.1887 = y.1887[order(y.1887$X), ] ; y.1887$X <- NULL
y.1888 = y.1888[order(y.1888$X), ] ; y.1888$X <- NULL
y.1889 = y.1889[order(y.1889$X), ] ; y.1889$X <- NULL
y.1890 = y.1890[order(y.1890$X), ] ; y.1890$X <- NULL
y.1891 = y.1891[order(y.1891$X), ] ; y.1891$X <- NULL
y.1892 = y.1892[order(y.1892$X), ] ; y.1892$X <- NULL
y.1893 = y.1893[order(y.1893$X), ] ; y.1893$X <- NULL
y.1894 = y.1894[order(y.1894$X), ] ; y.1894$X <- NULL
y.1895 = y.1895[order(y.1895$X), ] ; y.1895$X <- NULL
y.1896 = y.1896[order(y.1896$X), ] ; y.1896$X <- NULL
y.1897 = y.1897[order(y.1897$X), ] ; y.1897$X <- NULL
y.1898 = y.1898[order(y.1898$X), ] ; y.1898$X <- NULL
y.1899 = y.1899[order(y.1899$X), ] ; y.1899$X <- NULL
y.1900 = y.1900[order(y.1900$X), ] ; y.1900$X <- NULL
y.1901 = y.1901[order(y.1901$X), ] ; y.1901$X <- NULL
y.1902 = y.1902[order(y.1902$X), ] ; y.1902$X <- NULL
y.1903 = y.1903[order(y.1903$X), ] ; y.1903$X <- NULL
y.1904 = y.1904[order(y.1904$X), ] ; y.1904$X <- NULL
y.1905 = y.1905[order(y.1905$X), ] ; y.1905$X <- NULL
y.1906 = y.1906[order(y.1906$X), ] ; y.1906$X <- NULL
y.1907 = y.1907[order(y.1907$X), ] ; y.1907$X <- NULL
y.1908 = y.1908[order(y.1908$X), ] ; y.1908$X <- NULL
y.1909 = y.1909[order(y.1909$X), ] ; y.1909$X <- NULL
y.1910 = y.1910[order(y.1910$X), ] ; y.1910$X <- NULL
y.1911 = y.1911[order(y.1911$X), ] ; y.1911$X <- NULL
y.1912 = y.1912[order(y.1912$X), ] ; y.1912$X <- NULL
y.1913 = y.1913[order(y.1913$X), ] ; y.1913$X <- NULL

# 2. Sort column names
y.1871 = y.1871[ , order(names(y.1871))]
y.1872 = y.1872[ , order(names(y.1872))]
y.1873 = y.1873[ , order(names(y.1873))]
y.1874 = y.1874[ , order(names(y.1874))]
y.1875 = y.1875[ , order(names(y.1875))]
y.1876 = y.1876[ , order(names(y.1876))]
y.1877 = y.1877[ , order(names(y.1877))]
y.1878 = y.1878[ , order(names(y.1878))]
y.1879 = y.1879[ , order(names(y.1879))]
y.1880 = y.1880[ , order(names(y.1880))]
y.1881 = y.1881[ , order(names(y.1881))]
y.1882 = y.1882[ , order(names(y.1882))]
y.1883 = y.1883[ , order(names(y.1883))]
y.1884 = y.1884[ , order(names(y.1884))]
y.1885 = y.1885[ , order(names(y.1885))]
y.1886 = y.1886[ , order(names(y.1886))]
y.1887 = y.1887[ , order(names(y.1887))]
y.1888 = y.1888[ , order(names(y.1888))]
y.1889 = y.1889[ , order(names(y.1889))]
y.1890 = y.1890[ , order(names(y.1890))]
y.1891 = y.1891[ , order(names(y.1891))]
y.1892 = y.1892[ , order(names(y.1892))]
y.1893 = y.1893[ , order(names(y.1893))]
y.1894 = y.1894[ , order(names(y.1894))]
y.1895 = y.1895[ , order(names(y.1895))]
y.1896 = y.1896[ , order(names(y.1896))]
y.1897 = y.1897[ , order(names(y.1897))]
y.1898 = y.1898[ , order(names(y.1898))]
y.1899 = y.1899[ , order(names(y.1899))]
y.1900 = y.1900[ , order(names(y.1900))]
y.1901 = y.1901[ , order(names(y.1901))]
y.1902 = y.1902[ , order(names(y.1902))]
y.1903 = y.1903[ , order(names(y.1903))]
y.1904 = y.1904[ , order(names(y.1904))]
y.1905 = y.1905[ , order(names(y.1905))]
y.1906 = y.1906[ , order(names(y.1906))]
y.1907 = y.1907[ , order(names(y.1907))]
y.1908 = y.1908[ , order(names(y.1908))]
y.1909 = y.1909[ , order(names(y.1909))]
y.1910 = y.1910[ , order(names(y.1910))]
y.1911 = y.1911[ , order(names(y.1911))]
y.1912 = y.1912[ , order(names(y.1912))]
y.1913 = y.1913[ , order(names(y.1913))]

## Countries for which we have complete information
full.info.countryes.first.time.span = c(Reduce(intersect, lapply(list(y.1871, y.1872, y.1873, y.1874, y.1875, y.1876, y.1877, y.1878, y.1879, y.1880, y.1881, y.1882, y.1883, y.1884, y.1885, y.1886, y.1887, y.1888, y.1889, y.1890, y.1891, y.1892, y.1893, y.1894, y.1895, y.1896, y.1897, y.1898, y.1899, y.1900, y.1901, y.1902, y.1903, y.1904, y.1905, y.1906, y.1907, y.1908, y.1909, y.1910, y.1911, y.1912, y.1913), names)))

## Keeping Columns for which we have complete information
p_load(dplyr)
y.1871 = dplyr::select(y.1871, full.info.countryes.first.time.span)
y.1872 = dplyr::select(y.1872, full.info.countryes.first.time.span)
y.1873 = dplyr::select(y.1873, full.info.countryes.first.time.span)
y.1874 = dplyr::select(y.1874, full.info.countryes.first.time.span)
y.1875 = dplyr::select(y.1875, full.info.countryes.first.time.span)
y.1876 = dplyr::select(y.1876, full.info.countryes.first.time.span)
y.1877 = dplyr::select(y.1877, full.info.countryes.first.time.span)
y.1878 = dplyr::select(y.1878, full.info.countryes.first.time.span)
y.1879 = dplyr::select(y.1879, full.info.countryes.first.time.span)
y.1880 = dplyr::select(y.1880, full.info.countryes.first.time.span)
y.1881 = dplyr::select(y.1881, full.info.countryes.first.time.span)
y.1882 = dplyr::select(y.1882, full.info.countryes.first.time.span)
y.1883 = dplyr::select(y.1883, full.info.countryes.first.time.span)
y.1884 = dplyr::select(y.1884, full.info.countryes.first.time.span)
y.1885 = dplyr::select(y.1885, full.info.countryes.first.time.span)
y.1886 = dplyr::select(y.1886, full.info.countryes.first.time.span)
y.1887 = dplyr::select(y.1887, full.info.countryes.first.time.span)
y.1888 = dplyr::select(y.1888, full.info.countryes.first.time.span)
y.1889 = dplyr::select(y.1889, full.info.countryes.first.time.span)
y.1890 = dplyr::select(y.1890, full.info.countryes.first.time.span)
y.1891 = dplyr::select(y.1891, full.info.countryes.first.time.span)
y.1892 = dplyr::select(y.1892, full.info.countryes.first.time.span)
y.1893 = dplyr::select(y.1893, full.info.countryes.first.time.span)
y.1894 = dplyr::select(y.1894, full.info.countryes.first.time.span)
y.1895 = dplyr::select(y.1895, full.info.countryes.first.time.span)
y.1896 = dplyr::select(y.1896, full.info.countryes.first.time.span)
y.1897 = dplyr::select(y.1897, full.info.countryes.first.time.span)
y.1898 = dplyr::select(y.1898, full.info.countryes.first.time.span)
y.1899 = dplyr::select(y.1899, full.info.countryes.first.time.span)
y.1900 = dplyr::select(y.1900, full.info.countryes.first.time.span)
y.1901 = dplyr::select(y.1901, full.info.countryes.first.time.span)
y.1902 = dplyr::select(y.1902, full.info.countryes.first.time.span)
y.1903 = dplyr::select(y.1903, full.info.countryes.first.time.span)
y.1904 = dplyr::select(y.1904, full.info.countryes.first.time.span)
y.1905 = dplyr::select(y.1905, full.info.countryes.first.time.span)
y.1906 = dplyr::select(y.1906, full.info.countryes.first.time.span)
y.1907 = dplyr::select(y.1907, full.info.countryes.first.time.span)
y.1908 = dplyr::select(y.1908, full.info.countryes.first.time.span)
y.1909 = dplyr::select(y.1909, full.info.countryes.first.time.span)
y.1910 = dplyr::select(y.1910, full.info.countryes.first.time.span)
y.1911 = dplyr::select(y.1911, full.info.countryes.first.time.span)
y.1912 = dplyr::select(y.1912, full.info.countryes.first.time.span)
y.1913 = dplyr::select(y.1913, full.info.countryes.first.time.span)

## Keeping Rows for which we have complete information
y.1871 = y.1871[1:length(full.info.countryes.first.time.span), ]
y.1872 = y.1872[1:length(full.info.countryes.first.time.span), ]
y.1873 = y.1873[1:length(full.info.countryes.first.time.span), ]
y.1874 = y.1874[1:length(full.info.countryes.first.time.span), ]
y.1875 = y.1875[1:length(full.info.countryes.first.time.span), ]
y.1876 = y.1876[1:length(full.info.countryes.first.time.span), ]
y.1877 = y.1877[1:length(full.info.countryes.first.time.span), ]
y.1878 = y.1878[1:length(full.info.countryes.first.time.span), ]
y.1879 = y.1879[1:length(full.info.countryes.first.time.span), ]
y.1880 = y.1880[1:length(full.info.countryes.first.time.span), ]
y.1881 = y.1881[1:length(full.info.countryes.first.time.span), ]
y.1882 = y.1882[1:length(full.info.countryes.first.time.span), ]
y.1883 = y.1883[1:length(full.info.countryes.first.time.span), ]
y.1884 = y.1884[1:length(full.info.countryes.first.time.span), ]
y.1885 = y.1885[1:length(full.info.countryes.first.time.span), ]
y.1886 = y.1886[1:length(full.info.countryes.first.time.span), ]
y.1887 = y.1887[1:length(full.info.countryes.first.time.span), ]
y.1888 = y.1888[1:length(full.info.countryes.first.time.span), ]
y.1889 = y.1889[1:length(full.info.countryes.first.time.span), ]
y.1890 = y.1890[1:length(full.info.countryes.first.time.span), ]
y.1891 = y.1891[1:length(full.info.countryes.first.time.span), ]
y.1892 = y.1892[1:length(full.info.countryes.first.time.span), ]
y.1893 = y.1893[1:length(full.info.countryes.first.time.span), ]
y.1894 = y.1894[1:length(full.info.countryes.first.time.span), ]
y.1895 = y.1895[1:length(full.info.countryes.first.time.span), ]
y.1896 = y.1896[1:length(full.info.countryes.first.time.span), ]
y.1897 = y.1897[1:length(full.info.countryes.first.time.span), ]
y.1898 = y.1898[1:length(full.info.countryes.first.time.span), ]
y.1899 = y.1899[1:length(full.info.countryes.first.time.span), ]
y.1900 = y.1900[1:length(full.info.countryes.first.time.span), ]
y.1901 = y.1901[1:length(full.info.countryes.first.time.span), ]
y.1902 = y.1902[1:length(full.info.countryes.first.time.span), ]
y.1903 = y.1903[1:length(full.info.countryes.first.time.span), ]
y.1904 = y.1904[1:length(full.info.countryes.first.time.span), ]
y.1905 = y.1905[1:length(full.info.countryes.first.time.span), ]
y.1906 = y.1906[1:length(full.info.countryes.first.time.span), ]
y.1907 = y.1907[1:length(full.info.countryes.first.time.span), ]
y.1908 = y.1908[1:length(full.info.countryes.first.time.span), ]
y.1909 = y.1909[1:length(full.info.countryes.first.time.span), ]
y.1910 = y.1910[1:length(full.info.countryes.first.time.span), ]
y.1911 = y.1911[1:length(full.info.countryes.first.time.span), ]
y.1912 = y.1912[1:length(full.info.countryes.first.time.span), ]
y.1913 = y.1913[1:length(full.info.countryes.first.time.span), ]


# Resetting Rownames
rownames(y.1871) <- NULL
rownames(y.1872) <- NULL
rownames(y.1873) <- NULL
rownames(y.1874) <- NULL
rownames(y.1875) <- NULL
rownames(y.1876) <- NULL
rownames(y.1877) <- NULL
rownames(y.1878) <- NULL
rownames(y.1879) <- NULL
rownames(y.1880) <- NULL
rownames(y.1881) <- NULL
rownames(y.1882) <- NULL
rownames(y.1883) <- NULL
rownames(y.1884) <- NULL
rownames(y.1885) <- NULL
rownames(y.1886) <- NULL
rownames(y.1887) <- NULL
rownames(y.1888) <- NULL
rownames(y.1889) <- NULL
rownames(y.1890) <- NULL
rownames(y.1891) <- NULL
rownames(y.1892) <- NULL
rownames(y.1893) <- NULL
rownames(y.1894) <- NULL
rownames(y.1895) <- NULL
rownames(y.1896) <- NULL
rownames(y.1897) <- NULL
rownames(y.1898) <- NULL
rownames(y.1899) <- NULL
rownames(y.1900) <- NULL
rownames(y.1901) <- NULL
rownames(y.1902) <- NULL
rownames(y.1903) <- NULL
rownames(y.1904) <- NULL
rownames(y.1905) <- NULL
rownames(y.1906) <- NULL
rownames(y.1907) <- NULL
rownames(y.1908) <- NULL
rownames(y.1909) <- NULL
rownames(y.1910) <- NULL
rownames(y.1911) <- NULL
rownames(y.1912) <- NULL
rownames(y.1913) <- NULL

# Building WM for the first period
wm.1 = list(as.matrix(y.1871), as.matrix(y.1872), as.matrix(y.1873), as.matrix(y.1874), as.matrix(y.1875), as.matrix(y.1876), as.matrix(y.1877), as.matrix(y.1878), as.matrix(y.1879), as.matrix(y.1880), as.matrix(y.1881), as.matrix(y.1882), as.matrix(y.1883), as.matrix(y.1884), as.matrix(y.1885), as.matrix(y.1886), as.matrix(y.1887), as.matrix(y.1888), as.matrix(y.1889), as.matrix(y.1890), as.matrix(y.1891), as.matrix(y.1892), as.matrix(y.1893), as.matrix(y.1894), as.matrix(y.1895), as.matrix(y.1896), as.matrix(y.1897), as.matrix(y.1898), as.matrix(y.1899), as.matrix(y.1900), as.matrix(y.1901), as.matrix(y.1902), as.matrix(y.1903), as.matrix(y.1904), as.matrix(y.1905), as.matrix(y.1906), as.matrix(y.1907), as.matrix(y.1908), as.matrix(y.1909), as.matrix(y.1910), as.matrix(y.1911), as.matrix(y.1912), as.matrix(y.1913))

########################################################
# Merge Trade-COW y National Material Capabilities-COW
########################################################
# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# load National Material Capabilities-COW
p_load(foreign)
mnc.d <- read.dta("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/NMC_5_0.dta") 


# load Trade-COW
p_load(foreign)
trade.d <- read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/National_COW_4.0.csv") 

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
cow.d.1 = subset(cow.d,  select=c("ID", "Time","milper", "irst")) # complete var names: "statename", "year", "milex", "milper", "irst", "pec"
cow.d.1$ID <- gsub(' ', '', cow.d.1$ID) # replace blank space with dot "" just like in full.info.countryes.first.time.span
cow.d.1$ID <- gsub('-', '', cow.d.1$ID) # replace blank space with dot "" just like in full.info.countryes.first.time.span

# Filter complete obs by country name
cow.d.1 <- subset(cow.d.1, ID == "Argentina" | ID == "AustriaHungary" | ID == "Belgium" | ID ==  "Bolivia" | ID == "Brazil" | ID == "Chile" | ID == "China" | ID == "Colombia" | ID == "Denmark" | ID == "Ecuador" | ID == "France" | ID == "Germany" | ID == "Greece" | ID == "Guatemala" | ID == "Haiti" | ID == "Iran" | ID == "Italy" | ID == "Japan" | ID == "Mexico" | ID == "Netherlands" | ID == "Peru" | ID == "Portugal" | ID == "Russia" | ID == "Spain" | ID == "Sweden" | ID == "Switzerland" | ID == "Turkey" | ID == "UnitedKingdom" | ID == "UnitedStatesofAmerica" | ID == "Venezuela")

# Filter complete obs by year
cow.d.1 <- subset(cow.d.1, Time >= 1871 & Time <= 1913)

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

########################################################
# GVARX First Period
########################################################

# Install/load GVARX
p_load(GVARX)
# https://www.rdocumentation.org/packages/GVARX/versions/1.3
# data("tradeweightx")

# Be adviced, this function only computes Granger causality tests for 
# BIVARIATE specifications. Will compute different models for both var types.

p=2 # The number of lag for Xt matrix
FLag=2 # The number of lag for foreign variables in country-specific VAR
lag.max=15 # The maximal number of lag for estimating country-specific VAR
type="const" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic="SC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".


mainOUTPUT = GVECMest(
        data = cow.d.1,
        p = p,
        FLag = FLag,
        lag.max = lag.max,
        type = type,
        ic = ic,
        weight.matrix=wm.1)





########################################################
# Others
########################################################

# Other package
# install.packages("GVAR", repos="http://R-Forge.R-project.org")
# library(GVAR)


