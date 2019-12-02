library(ggplot2);
library(ggcorrplot);

data = readRDS(file = "clean_cereal.rds");
summary(data);

# [1] "name"     "mfr"      "type"     "calories" "protein"  "fat"     
# [7] "sodium"   "fiber"    "carbo"    "sugars"   "potass"   "vitamins"#
# [13] "shelf"    "weight"   "cups"     "rating"

require(corrgram)
corrgram(data, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, 
        text.panel=panel.txt, main="Cereal data")

x11();
ggplot(data = data, aes(x = mfr, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = type, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = calories, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = protein, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = fat, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = sodium, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = fiber, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = carbo, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = sugars, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = potass, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = vitamins, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = shelf, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = weight, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

x11();
ggplot(data = data, aes(x = cups, y = rating)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)


