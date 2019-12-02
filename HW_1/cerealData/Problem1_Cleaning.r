data = read.csv("cereal.csv");
summary(data);

mean(data$calories);

# drop rows with negative values
data <- data[data$carbo >= 0, ]
data <- data[data$potass >= 0, ]
data <- data[data$sugars >= 0, ]

# save data to 
saveRDS(data, file = "clean_cereal.rds")
