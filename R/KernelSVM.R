require(ggplot2)

# Init data
x1 = c(-1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5)
x2 = c(-2.0, 0.0, 1.0, 2.0, 1.0, 2.0, 3.75,-2.0, -1.0, -3.0, -0.5, -2.0, 1.5)
y = c(1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1)
  
df = data.frame(x1, x2, y)
str(df)
ggplot(data=df[,c('x1', 'x2')], aes(x1,x2)) + geom_point() + geom_point(aes(colour = y))


# Test of Kernel1 = (x1^2, x2)
kernel1 = data.frame(df['x1']^2, df['x2'])
ggplot(kernel1, aes(x1,x2)) + geom_point() + geom_point(aes(colour = y))


# Test of Kernel1 = (x1^3 - 2x1x, x2)
kernel2 = data.frame((df['x1']^2-2*x1), df['x2'])
ggplot(kernel2, aes(x1,x2)) + geom_point() + geom_point(aes(colour = y))


# Test of Kernel1 = (x1^3, x2)
kernel3 = data.frame(df['x1']^3, df['x2'])
ggplot(kernel3, aes(x1,x2)) + geom_point()  + geom_point(aes(colour = y))


# Kernel three allows the sepereation of the data.



