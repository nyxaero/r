is_valid_triangle <- function(a, b, c) {
  return((a + b > c) & (b + c > a) & (a + c > b))
}

# Function to determine type of triangle
triangle_type <- function(a, b, c) {
  if (a == b && b == c) {
    return("Equilateral")
  } else if (a == b || b == c || a == c) {
    return("Isosceles")
  } else {
    return("Scalene")
  }
}

# Function to calculate area of triangle
triangle_area <- function(a, b, c) {
  s <- (a + b + c) / 2
  area <- sqrt(s * (s - a) * (s - b) * (s - c))
  return(area)
}

# Function to validate input
validate_input <- function(x) {
  if (!is.numeric(x) || x <= 0) {
    stop("Error: Input must be a positive number.")
  }
  return(TRUE)
}

# Prompt for triangle sides
cat("Enter the lengths of the sides of the triangle:\n")

# Assign side lengths
a <- as.numeric ( readline ( prompt = " Side a: ") )
b <- as.numeric ( readline ( prompt = " Side b: ") )
c <- as.numeric ( readline ( prompt = " Side c: ") )

# Try block for validation, type, and area
tryCatch({
  validate_input(a)
  validate_input(b)
  validate_input(c)
  
  if (!is_valid_triangle(a, b, c)) {
    stop("Error: The given sides do not form a valid triangle.")
  }
  
  type_of_triangle <- triangle_type(a, b, c)
  cat("The triangle is:", type_of_triangle, "\n")
  
  area_of_triangle <- triangle_area(a, b, c)
  cat("The area of the triangle is:", area_of_triangle, "\n")
  
}, error = function(e) {
  cat(e$message, "\n")
})