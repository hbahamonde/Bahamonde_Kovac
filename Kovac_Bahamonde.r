cat("\014")
rm(list=ls())
graphics.off()


##############################
# weight matrix
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

# Generate List 

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
y.1871 = select(y.1871, full.info.countryes.first.time.span)
y.1872 = select(y.1872, full.info.countryes.first.time.span)
y.1873 = select(y.1873, full.info.countryes.first.time.span)
y.1874 = select(y.1874, full.info.countryes.first.time.span)
y.1875 = select(y.1875, full.info.countryes.first.time.span)
y.1876 = select(y.1876, full.info.countryes.first.time.span)
y.1877 = select(y.1877, full.info.countryes.first.time.span)
y.1878 = select(y.1878, full.info.countryes.first.time.span)
y.1879 = select(y.1879, full.info.countryes.first.time.span)
y.1880 = select(y.1880, full.info.countryes.first.time.span)
y.1881 = select(y.1881, full.info.countryes.first.time.span)
y.1882 = select(y.1882, full.info.countryes.first.time.span)
y.1883 = select(y.1883, full.info.countryes.first.time.span)
y.1884 = select(y.1884, full.info.countryes.first.time.span)
y.1885 = select(y.1885, full.info.countryes.first.time.span)
y.1886 = select(y.1886, full.info.countryes.first.time.span)
y.1887 = select(y.1887, full.info.countryes.first.time.span)
y.1888 = select(y.1888, full.info.countryes.first.time.span)
y.1889 = select(y.1889, full.info.countryes.first.time.span)
y.1890 = select(y.1890, full.info.countryes.first.time.span)
y.1891 = select(y.1891, full.info.countryes.first.time.span)
y.1892 = select(y.1892, full.info.countryes.first.time.span)
y.1893 = select(y.1893, full.info.countryes.first.time.span)
y.1894 = select(y.1894, full.info.countryes.first.time.span)
y.1895 = select(y.1895, full.info.countryes.first.time.span)
y.1896 = select(y.1896, full.info.countryes.first.time.span)
y.1897 = select(y.1897, full.info.countryes.first.time.span)
y.1898 = select(y.1898, full.info.countryes.first.time.span)
y.1899 = select(y.1899, full.info.countryes.first.time.span)
y.1900 = select(y.1900, full.info.countryes.first.time.span)
y.1901 = select(y.1901, full.info.countryes.first.time.span)
y.1902 = select(y.1902, full.info.countryes.first.time.span)
y.1903 = select(y.1903, full.info.countryes.first.time.span)
y.1904 = select(y.1904, full.info.countryes.first.time.span)
y.1905 = select(y.1905, full.info.countryes.first.time.span)
y.1906 = select(y.1906, full.info.countryes.first.time.span)
y.1907 = select(y.1907, full.info.countryes.first.time.span)
y.1908 = select(y.1908, full.info.countryes.first.time.span)
y.1909 = select(y.1909, full.info.countryes.first.time.span)
y.1910 = select(y.1910, full.info.countryes.first.time.span)
y.1911 = select(y.1911, full.info.countryes.first.time.span)
y.1912 = select(y.1912, full.info.countryes.first.time.span)
y.1913 = select(y.1913, full.info.countryes.first.time.span)

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
wm.1 = list(y.1871, y.1872, y.1873, y.1874, y.1875, y.1876, y.1877, y.1878, y.1879, y.1880, y.1881, y.1882, y.1883, y.1884, y.1885, y.1886, y.1887, y.1888, y.1889, y.1890, y.1891, y.1892, y.1893, y.1894, y.1895, y.1896, y.1897, y.1898, y.1899, y.1900, y.1901, y.1902, y.1903, y.1904, y.1905, y.1906, y.1907, y.1908, y.1909, y.1910, y.1911, y.1912, y.1913)


##############################
## Second Period: 1955 to 2012
##############################





########################################################
# Merge Trade-COW y National Material Capabilities-COW
########################################################

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
cow.d.1$ID <- gsub(' ', '.', cow.d.1$ID) # replace blank space with dot "." just like in full.info.countryes.first.time.span
cow.d.1$ID <- gsub('-', '.', cow.d.1$ID) # replace blank space with dot "." just like in full.info.countryes.first.time.span

# Filter complete obs by country name
cow.d.1 <- subset(cow.d.1, ID == "Argentina" | ID == "Austria.Hungary" | ID == "Belgium" | ID ==  "Bolivia" | ID == "Brazil" | ID == "Chile" | ID == "China" | ID == "Colombia" | ID == "Denmark" | ID == "Ecuador" | ID == "France" | ID == "Germany" | ID == "Greece" | ID == "Guatemala" | ID == "Haiti" | ID == "Iran" | ID == "Italy" | ID == "Japan" | ID == "Mexico" | ID == "Netherlands" | ID == "Peru" | ID == "Portugal" | ID == "Russia" | ID == "Spain" | ID == "Sweden" | ID == "Switzerland" | ID == "Turkey" | ID == "United.Kingdom" | ID == "United.States.of.America" | ID == "Venezuela")

# Filter complete obs by year
cow.d.1 <- subset(cow.d.1, Time >= 1871 & Time <= 1913)

# check everything is alright
# table(sort(full.info.countryes.first.time.span) == sort(unique(cow.d.1$ID)))

########################################################
# GVARX First Period
########################################################


# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# Install/load GVARX
p_load(GVARX)
# https://www.rdocumentation.org/packages/GVARX/versions/1.1/topics/grangerGVAR
# data("tradeweightx")




# (1)
# Be adviced, this function only computes Granger causality tests for 
# BIVARIATE specifications. Will compute different models for both var types.

# (2)
# Weight matrix will be the COW Trade DF.


# Rename "West Germany" to "Germany" only for 1946-1989 period.
# cow.d.1$ID[cow.d.1$ID == "German Federal Republic"] <- "Germany"

# Partion the DF into two smaller DF's:  (1) 1871-1913, and (2) 1946-today
cow.d.1871.1913 = cow.d.1[cow.d.1$Time >= 1871 & cow.d.1$Time <= 1913,]
cow.d.1946.today = cow.d.1[cow.d.1$Time >= 1955 & cow.d.1$Time <= max(cow.d.1$Time),]
# trim 1945-1955 years for all: we want to have one Germany across time (and West Germany pops un in the database only since 1955). That's why

## countries that entered into "cow.d.1871.1913" and "cow.d.1946.today"
### unique(cow.d.1871.1913$ID)
### unique(cow.d.1946.today$ID)


# append both DF's
cow.d.time.trimmed <- rbind(cow.d.1871.1913,cow.d.1946.today)

# we want to have one Germany across time (and West Germany pops un in the database only since 1955). 
cow.d.time.trimmed$ID[cow.d.time.trimmed$ID == "German Federal Republic" & cow.d.time.trimmed$Time >= 1955 & cow.d.time.trimmed$Time <=1989] <- "Germany"

# drop if ID == "German Federal Republic" | ID == "German Federal Republic" // OTHERWISE, we would have duplicates (two Germanies in 1955 onwards).
cow.d.time.trimmed <- cow.d.time.trimmed[!(cow.d.time.trimmed$ID == "German Federal Republic" | cow.d.time.trimmed$ID == "German Democratic Republic"),]



# check if panels are "strictly balanced?
p_load(plm)
is.pbalanced(cow.d.time.trimmed) # False


dat = cow.d.time.trimmed
dat$Time = as.Date(as.character(dat$Time), "%Y")


# pre dataset
# dat.pre = dat#[dat$Time<=2019,]


# balancing pre
dat.test <- pdata.frame(dat, index=c("ID","Time"))
dat.test = make.pbalanced(dat.test, balance.type = "shared.individuals", index = c('ID', 'Time') ) # shared.individuals / fill
is.pbalanced(dat.test)
# N
unique(dat.test$ID)











########################################################
# GVAR
########################################################
# formatting df as "timeseries data as list (each entry is a matrix of a subsystem of variables)"
cow.d <- split(cow.d, cow.d$ccode)


# Transform DF into TS object
cow = as.ts(cow, 
              start = min(cow$year),
              end = max(cow$year),
              frequency = 1 # 1 year
              )

names(cow) <- unique(country$statename)
c.names <- names(cow)[-length(cow)]


install.packages("GV")

# install.packages("GVAR", repos="http://R-Forge.R-project.org")
library(GVAR)


data(pesaran26)
c.names <- names(Data)[-length(Data)]

p <- c(2,2,2,1,2,2,1,2,2,2,2,1,2,1,1,2,2,2,2,2,2,1,2,2,2,2)
q <- c(2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
lex <- q

endo <- ord <- we <- d <- vector("list",length=length(c.names))
names(endo) <- names(ord) <- names(we) <- names(d) <- c.names
# base country: usa
endo[[1]] <- c(1:3,5:7)
ord[[1]] <- c(1:3,5:7)
we[[1]] <- c(1:2,4)
d[[1]] <- NULL
# countries with 6 endogenous variables:
for (j in c("EuroArea", "Japan", "UK", "Sweden", "Switzerland", "Norway", "Australia", "Canada", "NewZealand", "Korea", "Safrica")) {i <- which(c.names==j); endo[[i]] <- ord[[i]] <- 1:6}
# countries with 5 endogenous variables:
for (j in c("Argentina", "Chile", "Malaysia", "Philippines", "Singapore", "Thailand", "India")) {i <- which(c.names==j); endo[[i]] <- ord[[i]] <- 1:5}
# countries with 4 endogenous variables:
for (j in c("China", "Brazil", "Mexico", "Peru", "Indonesia", "Turkey")) {i <- which(c.names==j); endo[[i]] <- ord[[i]] <- c(1:2,4:5)}
# Saudi Arabia
endo[[21]] <- ord[[21]] <- c(1:2,4)

# all countries but us
for (i in 2:length(we))
{
        we[[i]] <- c(1:3,5,6)
        d[[i]] <- 1
}

Case <- "IV"
r <- c(2,1,1,4,3,3,3,2,2,1,2,3,3,4,4,3,3,4,1,2,3,3,2,1,1,1)

res.GVAR <- GVAR(Data=Data,r=r,p=p,q=q,weight=weight,Case=Case,exo.var=TRUE,d=d,lex=lex,ord=ord,we=we,endo=endo,method="max.eigen")

# view vecm models
res.GVAR$we.vecms
