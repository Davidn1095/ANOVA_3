# ANOVA_3

# Creating a Data Frame
data <- data.frame(
  'c1' = c(5.65,5.75,5.64,5.73,5.69,5.71),
  'c2' = c(5.83,5.78,5.79,5.92,5.95,5.82),
  'c3' = c(5.75,5.63,5.68,5.64,5.72,5.66)
)

# Calculating Sum of Squares
sum_squares <- sum(sapply(data, function(x) sum(x^2)))

# Calculating Total Sum and SSTotal
total_sum <- sum(sapply(data, sum))^2
N <- sum(sapply(data, length))
SSTotal <- sum_squares - (total_sum / N)

# Printing SSTotal
print(SSTotal)

# Calculating Sum of Squares for Treatments (SSTreatments)
sum_squares_treatments <- sum(sapply(data, function(x) sum(x)^2 / length(x)))
total_sum <- sum(sapply(data, sum))
total_sum_squared <- total_sum^2 / N
SSTratamientos <- sum_squares_treatments - total_sum_squared

# Printing SSTratamientos
print(SSTratamientos)

# Calculating Sum of Squares for Error (SSError)
SSError <- SSTotal - SSTratamientos

# Printing SSError
print(SSError)

# Creating ANOVA Table
anova_table <- data.frame(
  'Fuente de variación' = c('Tratamientos (Entre)', 'Errores (Dentro)', 'Total'),
  'S.S.' = c(SSTratamientos, SSError, SSTotal),
  'g.l.' = c(2, 15, 17),
  'C.M.' = c(SSTratamientos/2, SSError/15, NA),
  'F' = c((SSTratamientos/2) /(SSError/15), NA, NA)
)

# Printing the ANOVA Table
print(anova_table)
