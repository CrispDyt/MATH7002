library(CircNNTSR)
data(Turtles)
par(mfrow = c(1, 2))

# Left: 30
breaks_left <- seq(0, 360, by = 30)  
direction_table_left <- table(cut(Turtles, breaks = breaks_left, include.lowest = TRUE, right = FALSE))
heights_left <- as.vector(direction_table_left)
widths_left <- diff(breaks_left)
bp_left <- barplot(heights_left, width = widths_left, main = "Turtle Movement Directions (30-degree)",
                   xlab = "Direction (degrees)", ylab = "Frequency", col = "lightblue", border = "white")

axis(1, at = bp_left, labels = breaks_left[-length(breaks_left)], las = 1)

breaks_right <- seq(0, 360, by = 50)  
direction_table_right <- table(cut(Turtles, breaks = breaks_right, include.lowest = TRUE, right = FALSE))
heights_right <- as.vector(direction_table_right)
widths_right <- diff(breaks_right)

bp_right <- barplot(heights_right, width = widths_right, main = "Turtle Movement Directions (40-degree)",
                    xlab = "Direction (degrees)", ylab = "Frequency", col = "lightgreen", border = "white")

axis(1, at = bp_right, labels = breaks_right[-length(breaks_right)], las = 1)

par(mfrow = c(1, 1))

