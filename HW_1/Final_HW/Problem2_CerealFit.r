library(ggplot2);
library(ggcorrplot);

data = readRDS(file = "clean_cereal.rds");
summary(data);

fit = lm(rating ~ mfr + type + calories + protein + fat + sodium + fiber + carbo + sugars + 
    potass + vitamins + shelf + weight + cups, 
    data = data)

summary(fit);

fit2 = lm(rating ~ mfr + type + calories + protein + fat + sodium + fiber + carbo + sugars + 
    potass + vitamins + shelf + weight + cups + sugars:calories,
    data = data)

summary(fit2);

fit3 = lm(rating ~ mfr + type + calories + protein + fat + sodium + fiber + carbo + sugars + 
    potass + vitamins + shelf + weight + cups + calories:fat,
    data = data)

summary(fit3);


fit4 = lm(rating ~ mfr + type + + protein + sodium + fiber + carbo + sugars + 
    potass + vitamins + shelf + weight + cups + calories*fat,
    data = data)

summary(fit4);