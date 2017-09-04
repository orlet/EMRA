#In Excel, pivot the table and delete irrelevant headers, leave col. 2

dat <- read.csv("NanoDrop.csv", header=FALSE)
head(dat)
names(dat)[names(dat) == c("V1", "V2", "V3")] <- c("Wavelength", "Blank", "Absorbance") 
head(dat)

plot <- ggplot(dat, aes(x = Wavelength,
                y = Absorbance)) + 
         geom_line() +
         theme_few()
       
