filepath = 'C:\\Users\\sachi\\Downloads\\Geese.txt'

geese <- read.table(filepath,          # TXT data file indicated as string or full path to the file
                    header = TRUE,    # Whether to display the header (TRUE) or not (FALSE)
                    sep = "",          # Separator of the columns of the file
                    dec = ".")         # Character used to separate decimals of the numbers in the file

# PREDICTOR (temprature)
temp<-geese$temp

# RESPONCE (time)
time<-geese$time

# Apply the lm() function.
model = lm(temp ~ time, data = data.frame(geese))

print(model)

print(summary(model))




# VISUALIZATION

# Plot the chart.
plot(time,temp,col = "blue",main = "Effect of Air Temperature on Lesser Snow Geese Leaving Roost",
     abline(lm(temp~time)), cex = 1.3, pch = 16,
     xlab = "Air Temperature",
     ylab = "Lesser Snow Geese Response Time")

par(mfrow = c(2, 2))
plot(model)

# Inspect the data

# We will mainly use .fitted, .resid, .hat, .std.resid, .cooksd

# LINEARITY
# Now lets check the linearity assumption
dev.off()
plot(model,1)


model



lm(formula = time ~ temp, data = geese)

x = rnorm(1000, mean = mean(temp), sd = sd(temp))

reg <- B0 + B1*x

# Check attributes of the model
attributes(summary(relation))

summary(relation)$r.squared

predict(model, ,interval="confidence")


confint(model, 'time', level=0.95)


cor(time,temp)



