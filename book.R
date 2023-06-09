## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(magrittr, quietly = TRUE))
suppressPackageStartupMessages(library(pryr, quietly = TRUE))
suppressPackageStartupMessages(library(microbenchmark, quietly = TRUE))
suppressPackageStartupMessages(library(purrr, quietly = TRUE))
suppressPackageStartupMessages(library(microbenchmark, quietly = TRUE))

assert <- function(expr, expected) {
	if (!expr) stop(paste0("ERROR: ", expr))
}

options(width = 50)

Sys.setenv(LANG = "en")



## -----------------------------------------------------------------------------
square <- function(x) x**2


## -----------------------------------------------------------------------------
square <- \(x) x**2


## -----------------------------------------------------------------------------
square(1:5)


## -----------------------------------------------------------------------------
square <- function(x) {
	x**2
}


## -----------------------------------------------------------------------------
rescale <- function(x) {
    m <- mean(x)
    s <- sd(x)
    (x - m) / s
}


## -----------------------------------------------------------------------------
(x <- 1:5)


## -----------------------------------------------------------------------------
x**2


## -----------------------------------------------------------------------------
invisible(x**2)


## ---- echo=FALSE--------------------------------------------------------------
rm(x)


## -----------------------------------------------------------------------------
if (2 + 2 == 4) "Brave New World" else "1984"


## -----------------------------------------------------------------------------
x <- for (i in 1:10) i
x


## -----------------------------------------------------------------------------
rescale <- function(x, only_translate) {
    m <- mean(x)
    translated <- x - m
    if (only_translate) return(translated)
    s <- sd(x)
    translated / s
}
rescale(1:4, TRUE)
rescale(1:4, FALSE)


## ---- results="hide"----------------------------------------------------------
rescale(x = 1:4, only_translate = TRUE)
rescale(x = 1:4, only_translate = FALSE)


## ---- results="hide"----------------------------------------------------------
rescale(only_translate = TRUE, x = 1:4)
rescale(only_translate = FALSE, x = 1:4)


## ---- results="hide"----------------------------------------------------------
rescale(1:4, only_translate = TRUE)
rescale(only_translate = TRUE, 1:4)
rescale(x = 1:4, TRUE)
rescale(TRUE, x = 1:4)


## ---- results="hide"----------------------------------------------------------
rescale(1:4, o = TRUE)
rescale(o = TRUE, 1:4)


## -----------------------------------------------------------------------------
rescale <- function(x, only_translate = FALSE) {
    m <- mean(x)
    translated <- x - m
    if (only_translate) return(translated)
    s <- sd(x)
    translated / s
}


## ---- result="hide"-----------------------------------------------------------
rescale(1:4)


## -----------------------------------------------------------------------------
rescale <- function(x, ...) {
    m <- mean(x, ...)
    s <- sd(x, ...)
    (x - m) / s
}


## -----------------------------------------------------------------------------
x <- c(NA, 1:3)
rescale(x)


## -----------------------------------------------------------------------------
rescale(x, na.rm = TRUE)


## -----------------------------------------------------------------------------
f <- function(x) x
g <- function(x, ...) x
f(1:4, foo = "bar")
g(1:4, foo = "bar")


## -----------------------------------------------------------------------------
f <- function(...) list(...)
g <- function(x, y, ...) f(...)
g(x = 1, y = 2, z = 3, w = 4)


## -----------------------------------------------------------------------------
f <- function(w) w
g <- function(x, y, ...) f(...)
g(x = 1, y = 2, z = 3, w = 4)


## -----------------------------------------------------------------------------
f <- function(a, b) a
f(2, stop("error if evaluated"))
f(stop("error if evaluated"), 2)


## -----------------------------------------------------------------------------
f <- function(a, b = a) a + b
f(a = 2)


## -----------------------------------------------------------------------------
a <- 4
f <- function(x) {
    a <- 2
    x
}
f(1 + a)


## ---- echo=FALSE--------------------------------------------------------------
rm(a)


## -----------------------------------------------------------------------------
f <- function(a) function(b) a + b


## -----------------------------------------------------------------------------
f(2)(2)


## -----------------------------------------------------------------------------
ff <- vector("list", 4)
for (i in 1:4) {
  ff[[i]] <- f(i)
}
ff


## -----------------------------------------------------------------------------
ff[[1]](1)


## -----------------------------------------------------------------------------
i <- 1
ff[[2]](1)


## -----------------------------------------------------------------------------
i <- 2
ff[[2]](1)


## -----------------------------------------------------------------------------
results <- vector("numeric", 4)
for (i in 1:4) {
  results[i] <- ff[[i]](1)
}
results


## -----------------------------------------------------------------------------
f <- function(a) {
  force(a)
  function(b) a + b
}

ff <- vector("list", 4)
for (i in 1:4) {
  ff[[i]] <- f(i)
}

ff[[1]](1)
i <- 1
ff[[2]](1)


## -----------------------------------------------------------------------------
(function(x) x**2)(2)


## -----------------------------------------------------------------------------
x <- 1:5
y <- 6:10
x - y


## -----------------------------------------------------------------------------
2 * x


## -----------------------------------------------------------------------------
x <- 1:6
y <- 1:3
x - y


## -----------------------------------------------------------------------------
log(1:3) - sqrt(1:3)


## -----------------------------------------------------------------------------
f <- function(a, b) log(a) - sqrt(b)
f(1:3, 1:3)


## -----------------------------------------------------------------------------
compare <- function(x, y) {
    if (x < y) {
        -1
    } else if (y < x) {
        1
    } else {
        0
    }
}


## -----------------------------------------------------------------------------
compare <- function(x, y) {
    ifelse(x < y, -1, ifelse(y < x, 1, 0))
}
compare(1:6, 1:3)


## -----------------------------------------------------------------------------
compare <- function(x, y) {
    if (x < y) {
        -1
    } else if (y < x) {
        1
    } else {
        0
    }
}
compare <- Vectorize(compare)
compare(1:6, 1:3)


## -----------------------------------------------------------------------------
scale_with <- function(x, y) {
    (x - mean(y)) / sd(y)
}


## -----------------------------------------------------------------------------
scale_with(1:6, 1:3)
scale_with <- Vectorize(scale_with)
scale_with(1:6, 1:3)


## -----------------------------------------------------------------------------
scale_with <- function(x, y) {
    (x - mean(y)) / sd(y)
}
scale_with <- Vectorize(scale_with, vectorize.args="x")
scale_with(1:6, 1:3)


## -----------------------------------------------------------------------------
make_node <- function(name, left = NULL, right = NULL) 
  list(name = name, left = left, right = right)

tree <- make_node("root", 
                  make_node("C", make_node("A"), 
                                 make_node("B")),
                  make_node("D"))


## -----------------------------------------------------------------------------
node_depth <- function(tree, name, depth = 0) {
    if (is.null(tree))     return(NA)
    if (tree$name == name) return(depth)

    left <- node_depth(tree$left, name, depth + 1)
    if (!is.na(left)) return(left)
    right <- node_depth(tree$right, name, depth + 1)
    return(right)
}


## -----------------------------------------------------------------------------
node_depth(tree, "D")
node_depth(tree, "A")


## -----------------------------------------------------------------------------
node_depth <- Vectorize(node_depth, vectorize.args = "name", 
                        USE.NAMES = FALSE)
node_depth(tree, c("A", "B", "C", "D"))


## -----------------------------------------------------------------------------
`+`(2, 2)


## -----------------------------------------------------------------------------
`if`(2 > 3, "true", "false")


## -----------------------------------------------------------------------------
`%x%` <- `*`
3 %x% 2


## -----------------------------------------------------------------------------
`%x%` <- function(expr, num) replicate(num, expr)
3 %x% 5
cat("This is ", "very " %x% 3, "much fun")


## -----------------------------------------------------------------------------
rnorm(1) %x% 4


## -----------------------------------------------------------------------------
`%x%` <- function(expr, num) {
  m <- match.call()
  replicate(num, eval.parent(m$expr))
}
rnorm(1) %x% 4


## -----------------------------------------------------------------------------
x <- y <- 1:5
x
y
x[1] <- 6
x
y


## -----------------------------------------------------------------------------
rm(x) ; rm(y)
mem_change(x <- 1:10000000)
address(x)
mem_change(x[1] <- 6)
address(x)


## -----------------------------------------------------------------------------
class(6)
class(6L)


## -----------------------------------------------------------------------------
z <- 1:5
class(z)
z[1] <- 6
class(z)

## ---- echo=FALSE--------------------------------------------------------------
rm(z)


## -----------------------------------------------------------------------------
mem_change(x[3] <- 8)
address(x)


## -----------------------------------------------------------------------------
mem_change(y <- x)
address(x)
address(y)


## -----------------------------------------------------------------------------
mem_change(x[3] <- 8)
address(x)
address(y)


## -----------------------------------------------------------------------------
mem_change(x[4] <- 9)
address(x)

## ---- echo=FALSE--------------------------------------------------------------
rm(x) ; rm(y)


## -----------------------------------------------------------------------------
x <- 1:4
x
names(x) <- letters[1:4]
x
names(x)


## -----------------------------------------------------------------------------
names(x) <- letters[1:4]


## -----------------------------------------------------------------------------
x <- `names<-`(x, letters[1:4])


## -----------------------------------------------------------------------------
x <- 1:4
attributes(x)
attributes(x) <- list(foo = "bar")
attributes(x)
attr(x, "baz") <- "qux"
attributes(x)


## -----------------------------------------------------------------------------
tree <- make_node("root", 
                  make_node("C", make_node("A"), 
                                 make_node("B")),
                  make_node("D"))


## -----------------------------------------------------------------------------
`left<-` <- function(node, value) {
    node$left = value
    node
}
`right<-` <- function(node, value) {
    node$right = value
    node
}


## -----------------------------------------------------------------------------
A <- make_node("A")
B <- make_node("B")
C <- make_node("C")
D <- make_node("D")
root <- make_node("root")
left(C) <- A
right(C) <- B
left(root) <- C
right(root) <- D
tree <- root


## -----------------------------------------------------------------------------
print_tree <- function(tree) {
  build_string <- function(node) {
    if (is.null(node$left) && is.null(node$right)) {
        node$name
    } else {
        left <- build_string(node$left)
        right <- build_string(node$right)
        paste0("(", left, ",", right, ")")
    }
  }
  build_string(tree)
}
print_tree(tree)


## -----------------------------------------------------------------------------
A <- make_node("A")
B <- make_node("B")
C <- make_node("C")
D <- make_node("D")
root <- make_node("root")
left(root) <- C
right(root) <- D
left(C) <- A
right(C) <- B
tree <- root
print_tree(tree)


## ---- echo=FALSE--------------------------------------------------------------
is_empty <- function(x) length(x) == 0
first <- function(x) x[1]
rest  <- function(x) x[-1]

## -----------------------------------------------------------------------------
lin_search <- function(element, sequence) {
    if (is_empty(sequence))              FALSE
    else if (first(sequence) == element) TRUE
    else lin_search(element, rest(sequence))
}

x <- 1:5
lin_search(0, x)
lin_search(1, x)
lin_search(5, x)
lin_search(6, x)


## -----------------------------------------------------------------------------
is_empty <- function(x) length(x) == 0
first <- function(x) x[1]
rest  <- function(x) x[-1]


## -----------------------------------------------------------------------------
next_list <- function(element, rest = NULL)
    list(element = element, rest = rest)


## -----------------------------------------------------------------------------
x <- next_list(1, next_list(2, next_list(3, next_list(4))))



## -----------------------------------------------------------------------------
nl_is_empty <- function(nl) is.null(nl)
nl_first <- function(nl) nl$element
nl_rest <- function(nl) nl$rest


## -----------------------------------------------------------------------------
nl_lin_search <- function(element, sequence) {
    if (nl_is_empty(sequence))              FALSE
    else if (nl_first(sequence) == element) TRUE
    else nl_lin_search(element, nl_rest(sequence))
}


## -----------------------------------------------------------------------------
vector_to_next_list <- function(x) {
    if (is_empty(x)) NULL
    else next_list(first(x), vector_to_next_list(rest(x)))
}


## -----------------------------------------------------------------------------
i_is_empty <- function(x, i) i > length(x)
i_first <- function(x, i) x[i]


## -----------------------------------------------------------------------------
i_vector_to_next_list <- function(x, i = 1) {
    if (i_is_empty(x, i)) NULL
    else next_list(i_first(x, i), i_vector_to_next_list(x, i + 1))
}


## -----------------------------------------------------------------------------
i_lin_search <- function(element, sequence, i = 1) {
    if (i_is_empty(sequence, i))              FALSE
    else if (i_first(sequence, i) == element) TRUE
    else i_lin_search(element, sequence, i + 1)
}


## -----------------------------------------------------------------------------
lin_search <- function(element, sequence, i = 1) {
    if (i > length(sequence)) FALSE
    else if (sequence[i] == element) TRUE
    else lin_search(element, sequence, i + 1)
}

## ---- echo=FALSE--------------------------------------------------------------
assert(lin_search(0, 1:5) == FALSE)
assert(lin_search(1, 1:5) == TRUE)


## -----------------------------------------------------------------------------
binary_search <- function(element, x, 
                          first = 1, last = length(x)) {

    if (last < first) return(FALSE) # empty sequence
  
    middle <- (last - first) %/% 2 + first
    if (element == x[middle]) {
        TRUE
    } else if (element < x[middle]) {
        binary_search(element, x, first, middle)
    } else {
        binary_search(element, x, middle, last)
    }
}


## -----------------------------------------------------------------------------
binary_search <- function(element, x, 
                          first = 1, last = length(x)) {

    if (last < first) return(FALSE) # empty sequence
  
    middle <- (last - first) %/% 2 + first
    if (element == x[middle]) {
        TRUE
    } else if (element < x[middle]) {
        binary_search(element, x, first, middle - 1)
    } else {
        binary_search(element, x, middle + 1, last)
    }
}

## ---- echo=FALSE--------------------------------------------------------------
assert(binary_search(0, 1:5) == FALSE)
assert(binary_search(1, 1:5) == TRUE)
assert(binary_search(2, 1:5) == TRUE)
assert(binary_search(3, 1:5) == TRUE)
assert(binary_search(4, 1:5) == TRUE)
assert(binary_search(5, 1:5) == TRUE)
assert(binary_search(6, 1:5) == FALSE)


## -----------------------------------------------------------------------------
node_depth <- function(tree, name, depth = 0) {
    if (is.null(tree))     return(NA)
    if (tree$name == name) return(depth)

    left <- node_depth(tree$left, name, depth + 1)
    if (!is.na(left)) return(left)
    right <- node_depth(tree$right, name, depth + 1)
    return(right)
}


## -----------------------------------------------------------------------------
factorial <- function(n) {
    if (n == 1) 1
    else n * factorial(n - 1)
}


## -----------------------------------------------------------------------------
rm_duplicates <- function(x) {
    # Base cases
    if (is.null(x)) return(NULL)
    if (is.null(x$rest)) return(x)

    # Recursive case
    if (x$element == x$rest$element) rm_duplicates(x$rest)
    else next_list(x$element, rm_duplicates(x$rest))
}

x <- next_list(1, next_list(1, next_list(2, next_list(2))))
rm_duplicates(x)


## ---- echo=FALSE--------------------------------------------------------------
find_duplicates <- which %.% duplicated

## -----------------------------------------------------------------------------
vector_rm_duplicates <- function(x) {
    dup <- find_duplicates(x)
    x[-dup]
}
vector_rm_duplicates(c(1, 1, 2, 2))


## ---- echo=FALSE--------------------------------------------------------------
builtin_find_duplicates <- which %.% duplicated

## -----------------------------------------------------------------------------
find_duplicates <- function(x, i = 1) {
    if (i >= length(x)) return(c())

    if (x[i] == x[i + 1]) c(i, find_duplicates(x, i + 1))
    else find_duplicates(x, i + 1)
}

## ---- echo=FALSE--------------------------------------------------------------
x <- c(1,1,2,3,4,4)
assert(all(builtin_find_duplicates(x)-1 == find_duplicates(x)))


## -----------------------------------------------------------------------------
size_of_tree <- function(node) {
  if (is.null(node)) 0
  else size_of_tree(node$left) + size_of_tree(node$right) + 1
}


## -----------------------------------------------------------------------------
make_node <- function(name, left = NULL, right = NULL) 
  list(name = name, left = left, right = right)

tree <- make_node("root", 
                  make_node("C", make_node("A"), 
                                 make_node("B")),
                  make_node("D"))

size_of_tree(tree)


## -----------------------------------------------------------------------------
set_size_of_subtrees <- function(node) {
  if (is.null(node)) return(0); # We can't save anything in NULL
  
  # Compute the size if we don't have it
  if (is.null(node$size)) {
    left_size <- set_size_of_subtrees(node$left)
    right_size <- set_size_of_subtrees(node$right)
    node$size <- left_size + right_size + 1
  }
  
  return(node$size)
}


## -----------------------------------------------------------------------------
set_size_of_subtrees(tree)
tree$size


## -----------------------------------------------------------------------------
# Handles both NULL and nodes that know their size
get_size <- function(node) if (is.null(node)) 0 else node$size

set_size_of_subtrees <- function(node) {
  if (is.null(node)) return(NULL)
  
  # Update children
  node$left <- set_size_of_subtrees(node$left)
  node$right <- set_size_of_subtrees(node$right)
  
  # Set size
  node$size <- get_size(node$left) + get_size(node$right) + 1

  return(node)
}

tree <- set_size_of_subtrees(tree)
tree$size


## -----------------------------------------------------------------------------
factorial <- function(n) {
    if (n == 1) 1
    else n * factorial(n - 1)
}


## -----------------------------------------------------------------------------
factorial <- function(n, acc = 1) {
    if (n == 1) acc
    else factorial(n - 1, acc * n)
}

## ---- echo=FALSE--------------------------------------------------------------
assert(factorial(3) == 3*2)


## -----------------------------------------------------------------------------
find_duplicates <- function(x, i = 1) { 
    if (i >= length(x)) return(c())
    rest <- find_duplicates(x, i + 1) 
    if (x[i] == x[i + 1]) c(i, rest) else rest
}


## -----------------------------------------------------------------------------
find_duplicates <- function(x, i = 1, acc = c()) { 
    if (i >= length(x)) return(acc)
    if (x[i] == x[i + 1]) find_duplicates(x, i + 1, c(acc, i))
    else find_duplicates(x, i + 1, acc)
}

## ---- echo=FALSE--------------------------------------------------------------
assert(all(find_duplicates(c(1,1,2,2)) == c(1,3)))


## -----------------------------------------------------------------------------
r_lin_search <- function(element, sequence, i = 1) {
  if (i > length(sequence)) FALSE
  else if (sequence[i] == element) TRUE
  else r_lin_search(element, sequence, i + 1)
}


## -----------------------------------------------------------------------------
l_lin_search <- function(element, sequence) {
  for (e in sequence) {
    if (e == element) return(TRUE)
  }
  return(FALSE)
}


## ----lin_search_comparison, cache=TRUE----------------------------------------
x <- 1:1000
microbenchmark::microbenchmark(
  r_lin_search(-1, x),
  l_lin_search(-1, x)
)


## -----------------------------------------------------------------------------
r_binary_search <- function(element, x, 
                            first = 1, last = length(x)) {
  if (last < first) return(FALSE) # empty sequence
  
  middle <- (last - first) %/% 2 + first
  if (element == x[middle]) TRUE
  else if (element < x[middle]) {
    r_binary_search(element, x, first, middle - 1)
  } else {
    r_binary_search(element, x, middle + 1, last)
  }
}


## -----------------------------------------------------------------------------
l_binary_search <- function(element, x, first = 1, last = length(x)) {
  repeat {
    # Base case with no match
    if (last < first) return(FALSE) # empty sequence  
    
    middle <- (last - first) %/% 2 + first
    
    # Base case with a match
    if (element == x[middle]) return(TRUE)
    
    if (element < x[middle]) {
      last <- middle - 1    # "recursive" call with last = middle - 1
    } else {
      first <- middle + 1   # "recursive" call with first = middle + 1
    }
  }
}


## -----------------------------------------------------------------------------
l_binary_search <- function(element, x, first = 1, last = length(x)) {
  while (last >= first) {
    middle <- (last - first) %/% 2 + first
    
    # Base case with a match
    if (element == x[middle]) return(TRUE)
    
    if (element < x[middle]) {
      last <- middle - 1    # "recursive" call with last = middle - 1
    } else {
      first <- middle + 1   # "recursive" call with first = middle + 1
    }
  }
  return(FALSE)
}


## -----------------------------------------------------------------------------
x <- 2
y <- 3
x + y


## -----------------------------------------------------------------------------
genv <- globalenv()


## -----------------------------------------------------------------------------
genv$x
genv$y


## -----------------------------------------------------------------------------
expr <- quote(x + y)


## -----------------------------------------------------------------------------
eval(expr)


## -----------------------------------------------------------------------------
eval(expr, genv)


## -----------------------------------------------------------------------------
env <- new.env()
env$x <- 4
env$y <- 5
eval(expr, env)


## -----------------------------------------------------------------------------
eval(expr, list2env(list(x = -1, y = -2)))


## -----------------------------------------------------------------------------
gx <- 1:10
f <- function(px) as.list(environment())
f(gx**2)


## -----------------------------------------------------------------------------
f <- function(x) {
  res <- sqrt(sum(x))
  as.list(environment())
}
x <- 1:10

f(x**2)


## -----------------------------------------------------------------------------
f <- function(x) {
  sqrt(sum(x + y))
}
x <- 1:10
y <- 1:10

f(x**2)


## -----------------------------------------------------------------------------
f <- function(x, y = 2 * x) x + y


## -----------------------------------------------------------------------------
a <- 2
f(x = a)


## -----------------------------------------------------------------------------
f <- function(expr, ...) {
  expr <- substitute(expr)    # Turn argument into a plain expression
  env <- list2env(list(...))  # Make an env from '...'
  parent.env(env) <- parent.frame() # Get remaining args from caller's env
  eval(expr, env)             # Now evaluate the expression in the new env
}

a <- 22
f(a)
f(a + b, b = 13)


## -----------------------------------------------------------------------------
f <- function() environment()


## -----------------------------------------------------------------------------
environment(f)


## -----------------------------------------------------------------------------
f()


## -----------------------------------------------------------------------------
f <- function(x) {
  print(environment())  # Print the local environment
  g <- function(y) x + y
  g
}


## -----------------------------------------------------------------------------
environment(f)
g <- f(2)
environment(g)


## -----------------------------------------------------------------------------
g(3)


## -----------------------------------------------------------------------------
h1 <- f(1)
h2 <- f(2)
h3 <- f(3)


## -----------------------------------------------------------------------------
sapply(1:5, h1)
sapply(1:5, h2)
sapply(1:5, h3)


## -----------------------------------------------------------------------------
# Increment all elements in x by inc
inc_all <- function(x, inc) {
  sapply(x, \(a) a + inc)
}
inc_all(1:5, 3)


## -----------------------------------------------------------------------------
1:5 + 3


## ---- echo=FALSE--------------------------------------------------------------
database <- c("foo", "bar", "baz")
sapply(c("foo", "qux", "bar"), \(key) key %in% database)


## -----------------------------------------------------------------------------
make_counter <- function() {
    x <- 0
    count <- function() {
        x <- x + 1
        x
    }
}
counter <- make_counter()


## -----------------------------------------------------------------------------
counter()
counter()
counter()


## -----------------------------------------------------------------------------
make_counter <- function() {
    x <- 0
    count <- function() {
        x <<- x + 1
        x
    }
}
counter <- make_counter()
counter()
counter()
counter()


## -----------------------------------------------------------------------------
depth_first_numbers <- function(tree) {
  table <- c()
  counter <- make_counter()

  traverse_tree <- function(node) {
    if (is.null(node$left) && is.null(node$right)) {
      dfn <- counter()
      node$range <- c(dfn, dfn)
      table[node$name] <<- dfn
      node
    
    } else {
      left <- traverse_tree(node$left)
      right <- traverse_tree(node$right)
      new_node <- make_node(node$name, left, right)
      new_node$range <- c(left$range[1], right$range[2])
      new_node
    }
  }

  new_tree <- traverse_tree(tree)
  list(tree = new_tree, table = table)
}

## ---- echo=FALSE--------------------------------------------------------------
make_node <- function(name, left = NULL, right = NULL) 
  list(name = name, left = left, right = right)

print_tree <- function(tree) {
  build_string <- function(node) {
    if (is.null(node$left) && is.null(node$right)) {
        node$name
    } else {
        left <- build_string(node$left)
        right <- build_string(node$right)
        paste0("(", left, ",", right, ")")
    }
  }
  build_string(tree)
}

tree <- make_node("root", 
                  make_node("C", make_node("A"), 
                                 make_node("B")),
                  make_node("D"))

## -----------------------------------------------------------------------------
result <- depth_first_numbers(tree)
print_tree(result$tree)
result$table


## -----------------------------------------------------------------------------
df_eval <- function(expr, df = list()) {
  x <- substitute(expr) # strip expr of env; now just quoted expr
  env <- list2env(df, parent = parent.frame())
  eval(x, env)
}


## -----------------------------------------------------------------------------
x <- 1:3
y <- "foo"
df <- data.frame(y = 2:4, z = 3:5)


## -----------------------------------------------------------------------------
df_eval(x)


## -----------------------------------------------------------------------------
df_eval(y+z, df)


## -----------------------------------------------------------------------------
sapply(1:4, sqrt)


## -----------------------------------------------------------------------------
myapply <- function(x, f) {
  result <- x
  for (i in seq_along(x))
    result[i] <- f(x[i])
  result
}

myapply(1:4, sqrt)


## -----------------------------------------------------------------------------
rescale <- function(x) {
  m <- mean(x)
  s <- sd(x)
  (x - m) / s
}
rescale(1:4)


## -----------------------------------------------------------------------------
rescale <- function(x) {
  m <- mean(x)
  s <- sd(x)
  f <- function(y) (y - m) / s
  myapply(x, f)
}
rescale(1:4)


## -----------------------------------------------------------------------------
rescale <- function(x) {
  m <- mean(x)
  s <- sd(x)
  myapply(x, \(y) (y - m) / s)
}
rescale(1:4)


## -----------------------------------------------------------------------------
f <- function(x, y) x + y


## -----------------------------------------------------------------------------
g <- function(y) f(2, y)
myapply(1:4, g)


## -----------------------------------------------------------------------------
myapply(1:4, \(y) f(2, y))


## -----------------------------------------------------------------------------
h <- function(x) \(y) f(x, y)
myapply(1:4, h(2))


## -----------------------------------------------------------------------------
f(2, 2)
h(2)(2)


## -----------------------------------------------------------------------------
curry2 <- function(f) \(x) \(y) f(x, y)


## -----------------------------------------------------------------------------
h <- curry2(f)
f(2, 3)
h(2)(3)


## -----------------------------------------------------------------------------
h <- curry2(`+`)
h(2)(3)


## -----------------------------------------------------------------------------
myapply(1:4, curry2(`+`)(2))


## -----------------------------------------------------------------------------
myapply(1:4, \(y) 2 + y)


## -----------------------------------------------------------------------------
curry <- function(f) {
  n <- length(formals(f))
  if (n == 1) return(f) # no currying needed

  arguments <- vector("list", length = n)
  last <- function(x) {
    arguments[n] <<- x
    do.call(f, arguments)
  }
  make_i <- function(i, continuation) {
    force(i) ; force(continuation)
    function(x) {
      arguments[i] <<- x
      continuation
    }
  }

  continuation <- last
  for (i in seq(n-1, 1)) {
    continuation <- make_i(i, continuation)
  }
  continuation
}


## -----------------------------------------------------------------------------
f <- function(x, y, z) x + 2*y + 3*z
f(1, 2, 3)
curry(f)(1)(2)(3)


## -----------------------------------------------------------------------------
bind_parameters <- function(f, ...) {
  remembered <- list(...)
  function(...) {
    new <- list(...)
    do.call(f, c(remembered, new))
  }
}

f <- function(x, y, z, w = 4) x + 2*y + 3*z + 4*w

f(1, 2, 3, 4)
g <- bind_parameters(f, y = 2)
g(x = 1, z = 3)

h <- bind_parameters(f, y = 1, w = 1)
f(2, 1, 3, 1)
h(x = 2, z = 3)


## -----------------------------------------------------------------------------
my_sum_direct <- function(lst) {
  if (is_empty(lst)) 0
  else first(lst) + my_sum_direct(rest(lst))
}
my_sum_acc <- function(lst, acc = 0) {
  if (is_empty(lst)) acc
  else my_sum_acc(rest(lst), first(lst) + acc)
}
my_sum_cont <- function(lst, cont = identity) {
  if (is_empty(lst)) cont(0)
  else my_sum_cont(rest(lst), 
                   function(acc) cont(first(lst) + acc))
}


## ---- echo=FALSE--------------------------------------------------------------
make_node <- function(name, left = NULL, right = NULL)
  list(name = name, left = left, right = right)


## -----------------------------------------------------------------------------
size_of_tree <- function(node, continuation = identity) {
  if (is.null(node$left) && is.null(node$right)) {
    # A leaf, of size 1.
    # Tell the continuation to compute the rest from that value.
    continuation(1)
  } else {
    # Make a continuation that will be called with the size of the left tree
    left_continuation <- function(left_result) {
      # When we have the size of the left tree, create a continuation that
      # combines left and right tree (and give it to the continuation)
      right_continuation <- function(right_result) {
        continuation(left_result + right_result + 1)
      }
      
      # Compute the size of the right tree
      size_of_tree(node$right, right_continuation)
    }
    
    # Compute the size of the left tree
    size_of_tree(node$left, left_continuation)
  }
}

tree <- make_node("root",
                  make_node("C", make_node("A"),
                                 make_node("B")),
                  make_node("D"))

size_of_tree(tree)


## -----------------------------------------------------------------------------
size_of_tree <- function(node, continuation = identity) {
  repeat {
    if (is.null(node$left) && is.null(node$right)) {
      return(continuation(1))
    }
    new_continuation <- function(continuation) {
      force(continuation)
      function(left_result) {
        size_of_tree(
          node$right, 
          \(right_result) continuation(left_result + right_result + 1)
        )
      }
    }
    # simulated recursive call
    node <- node$left
    continuation <- new_continuation(continuation)
  }
}

size_of_tree(tree)


## -----------------------------------------------------------------------------
# A function we can call directly as e.g. f(1, 2)
f <- function(x, y) x + y

# thunk will call f(1, 2)
thunk <- \() f(1, 2)

f(1, 2)
thunk()


## -----------------------------------------------------------------------------
make_thunk <- function(f, ...) { 
  # Make sure the variables don't change after we return
  force(f)
  params <- list(...)
  # Then return the thunk
  \() do.call(f, params)
}

g <- make_thunk(f, 1, 2)
g()


## -----------------------------------------------------------------------------
trampoline <- function(thunk) {
  while (is.function(thunk)) thunk <- thunk()
  thunk
}


## -----------------------------------------------------------------------------
factorial <- function(n, acc = 1) {
  if (n == 1) acc
  else factorial(n - 1, acc * n)
}


## ----cp_factorial, cache=TRUE-------------------------------------------------
cp_factorial <- function(n, continuation = identity) {
  if (n == 1) {
    continuation(1)
  } else {
    cp_factorial(n - 1, \(result) continuation(result * n))
  } 
}

factorial(10)
cp_factorial(10)


## -----------------------------------------------------------------------------
thunk_factorial <- function(n, continuation = identity) {
  if (n == 1) {
    continuation(1)
  } else {
    # The new continuation is a function that returns a thunk.
    # The thunk will run the next step of the computation by
    # calling the continuation
    new_continuation <- \(result) \() continuation(n * result)
    
    # We don't call the factorial recursion yet, we just return a
    # thunk that is ready to take the first step of the computation
    \() thunk_factorial(n - 1, new_continuation)
  }
}


## -----------------------------------------------------------------------------
thunk_factorial(1)


## -----------------------------------------------------------------------------
# Takes us one step down the recursion, but not up again
thunk_factorial(2)()
# The next call will take us up to a result
thunk_factorial(2)()()


## ----thunk_factorial_explicit, cache=TRUE-------------------------------------
thunk_factorial(3)()()()()
thunk_factorial(4)()()()()()()
thunk_factorial(5)()()()()()()()()


## ----trampoline_thunk, cache=TRUE---------------------------------------------
trampoline(thunk_factorial(100))


## ----trampoline_thunk_function, cache=TRUE------------------------------------
make_trampoline <- function(f) \(...) trampoline(f(...))
factorial <- make_trampoline(thunk_factorial)
factorial(100)


## -----------------------------------------------------------------------------
thunk_size <- function(node, continuation = identity) {
  if (is.null(node$left) && is.null(node$right)) {
    continuation(1)
  } else {
    left_continuation <- function(left_result) {
      # Right continuation gives us a thunk for returning with
      # the size of the current tree (left + right + 1).
      right_continuation <- function(right_result)
        \() continuation(left_result + right_result + 1)
      
      # Return thunk for recursing right
      \() thunk_size(node$right, right_continuation)
    }
    
    # Return thunk for recursing left
    \() thunk_size(node$left, left_continuation)
  }
}

size_of_tree <- make_trampoline(thunk_size)
size_of_tree(tree)


## -----------------------------------------------------------------------------
list(1, 2, 3, 4)


## -----------------------------------------------------------------------------
1:4


## -----------------------------------------------------------------------------
as.list(1:4)


## -----------------------------------------------------------------------------
list(1:4)


## -----------------------------------------------------------------------------
is_even <- \(x) x %% 2 == 0
unlist(Filter(is_even, 1:10))


## -----------------------------------------------------------------------------
larger_than <- \(x) \(y) y > x
unlist(Filter(larger_than(5), 1:10))


## -----------------------------------------------------------------------------
unlist(Filter(\(y) y > 5, 1:10))


## -----------------------------------------------------------------------------
s <- list(a = 1:10,                # A vector
          b = list(1,2,3,4,5,6),   # A list
          c = y ~ x1 + x2 + x3,    # A formula
          d = vector("numeric"))   # An empty vector
Filter(function(x) length(x) > 5, s)


## -----------------------------------------------------------------------------
unlist(Map(is_even, 1:5))


## -----------------------------------------------------------------------------
add <- function(x) function(y) x + y
unlist(Map(add(2), 1:5))
unlist(Map(add(3), 1:5))


## -----------------------------------------------------------------------------
s <- list(a = 1:10, b = list(1,2,3,4,5,6), 
          c = y ~ x1 + x2 + x3, d = vector("numeric"))
unlist(Map(length, s))


## -----------------------------------------------------------------------------
unlist(Map(`+`, 1:5, 1:5))


## -----------------------------------------------------------------------------
x <- 1:10
y <- c(NA, x)
s <- list(x = x, y = y)
unlist(Map(mean, s))
unlist(Map(mean, s, na.rm = TRUE))


## -----------------------------------------------------------------------------
unlist(Map(mean, s, MoreArgs = list(na.rm = TRUE)))


## -----------------------------------------------------------------------------
scale <- function(x, y) (x - mean(y))/sd(y)


## -----------------------------------------------------------------------------
unlist(Map(scale, 1:10, 1:5))


## -----------------------------------------------------------------------------
unlist(Map(scale, 1:10, y = 1:5))


## -----------------------------------------------------------------------------
unlist(Map(scale, 1:10, MoreArgs = list(y = 1:5)))


## -----------------------------------------------------------------------------
s <- list(a = 1:10, b = list(1,2,3,4,5,6), 
          c = y ~ x1 + x2 + x3, d = vector("numeric"))
unlist(Map(length, s))


## -----------------------------------------------------------------------------
Reduce(`+`, 1:5)


## -----------------------------------------------------------------------------
Reduce(`+`, 1:5, accumulate = TRUE)


## -----------------------------------------------------------------------------
Reduce(`+`, 1:5, right = TRUE, accumulate = TRUE)


## -----------------------------------------------------------------------------
Reduce(`+`, 1:5, init = 10, accumulate = TRUE)


## -----------------------------------------------------------------------------
Reduce(`*`, 1:5)
Reduce(`*`, 1:5, accumulate = TRUE)
Reduce(`*`, 1:5, right = TRUE, accumulate = TRUE)


## -----------------------------------------------------------------------------
samples <- replicate(3, sample(1:10, replace = TRUE), 
                                     simplify = FALSE)
str(samples)
Reduce(intersect, samples)


## ---- echo=FALSE--------------------------------------------------------------
make_node <- function(name, left = NULL, right = NULL) 
  list(name = name, left = left, right = right)

print_tree <- function(tree) {
  build_string <- function(node) {
    if (is.null(node$left) && is.null(node$right)) {
        node$name
    } else {
        left <- build_string(node$left)
        right <- build_string(node$right)
        paste0("(", left, ",", right, ")")
    }
  }
  build_string(tree)
}

size_of_tree <- function(node) {
  if (is.null(node$left) && is.null(node$right)) {
    size <- 1
  } else {
    left_size <- size_of_tree(node$left)
    right_size <- size_of_tree(node$right)
    size <- left_size + right_size + 1
  }
  size
}


## -----------------------------------------------------------------------------
A <- make_node("A")
C <- make_node("C", make_node("A"), 
                    make_node("B"))
E <- make_node("E", 
               make_node("C", make_node("A"), make_node("B")),
               make_node("D"))

trees <- list(A = A, C = C, E = E)


## -----------------------------------------------------------------------------
trees[[2]]
unlist(trees[[2]])
print_tree(trees[[2]])


## -----------------------------------------------------------------------------
Map(print_tree, trees)
unlist(Map(print_tree, trees))


## -----------------------------------------------------------------------------
unlist(Map(print_tree, 
           Filter(function(tree) size_of_tree(tree) > 1, trees)))


## -----------------------------------------------------------------------------
unlist(Map(size_of_tree, trees))
Reduce(`+`, Map(size_of_tree, trees), 0)


## ---- echo=FALSE--------------------------------------------------------------
node_depth <- function(tree, name, depth = 0) {
  if (is.null(tree))     return(NA)
  if (tree$name == name) return(depth)
  
  left <- node_depth(tree$left, name, depth + 1)
  if (!is.na(left)) return(left)
  right <- node_depth(tree$right, name, depth + 1)
  return(right)
}


## -----------------------------------------------------------------------------
node_depth_B <- function(tree) node_depth(tree, "B")
unlist(Map(node_depth_B, trees))


## -----------------------------------------------------------------------------
unlist(Map(node_depth_B, trees), use.names = FALSE)


## -----------------------------------------------------------------------------
Filter(function(x) !is.na(x), 
       unlist(Map(node_depth_B, trees), use.names = FALSE))


## -----------------------------------------------------------------------------
has_B <- function(node) {
  if (node$name == "B") return(TRUE)
  if (is.null(node$left) && is.null(node$right)) return(FALSE)
  has_B(node$left) || has_B(node$right)
}
unlist(Map(node_depth_B, Filter(has_B, trees)), use.names = FALSE)


## -----------------------------------------------------------------------------
sapply(trees, size_of_tree)
sapply(trees, identity)


## -----------------------------------------------------------------------------
vapply(trees, size_of_tree, 1)


## -----------------------------------------------------------------------------
lapply(trees, size_of_tree)


## -----------------------------------------------------------------------------
(m <- matrix(1:6, nrow=2, byrow=TRUE))


## -----------------------------------------------------------------------------
collaps_input <- function(x) paste(x, collapse = ":")


## -----------------------------------------------------------------------------
apply(m, 1, collaps_input)


## -----------------------------------------------------------------------------
apply(m, 2, collaps_input)


## -----------------------------------------------------------------------------
apply(m, c(1, 2), collaps_input)


## -----------------------------------------------------------------------------
(x <- rnorm(10))
(categories <- sample(c("A", "B", "C"), size = 10, replace = TRUE))
tapply(x, categories, mean)


## -----------------------------------------------------------------------------
(categories2 <- sample(c("X", "Y"), size = 10, replace = TRUE))
tapply(x, list(categories, categories2), mean)


## -----------------------------------------------------------------------------
library(purrr)


## -----------------------------------------------------------------------------
keep(1:5, \(x) x > 3)
discard(1:5, \(x) x > 3)


## -----------------------------------------------------------------------------
keep(as.list(1:5), \(x) x > 3)


## -----------------------------------------------------------------------------
every(1:5, \(x) x > 0)
every(1:5, \(x) x > 3)
some(1:5, \(x) x > 3)
some(1:5, \(x) x > 6)


## -----------------------------------------------------------------------------
keep(1:5, ~ .x > 3)
discard(1:5, ~ .x > 3)


## -----------------------------------------------------------------------------
map(1:5, ~ .x + 2)
map_dbl(1:5, ~ .x + 2)


## -----------------------------------------------------------------------------
map2(1:5, 6:10, ~ 2 * .x + .y)
map2_dbl(1:5, 6:10, ~ 2 * .x + .y)


## -----------------------------------------------------------------------------
pmap(list(1:5, 6:10, 11:15), \(x, y, z) x + y + z)
pmap_dbl(list(1:5, 6:10, 11:15), \(x, y, z) x + y + z)


## -----------------------------------------------------------------------------
unlist(map_if(1:5, ~ .x %% 2 == 1, ~ 2*.x))


## -----------------------------------------------------------------------------
map_chr(map(keep(trees, ~ size_of_tree(.x) > 1), "left"),
        print_tree)


## -----------------------------------------------------------------------------
reduce(1:5, `+`)
reduce(1:5, `*`, .dir = "backward") # Reduce from the right


## -----------------------------------------------------------------------------
compose <- function(g, f) function(...) g(f(...))


## -----------------------------------------------------------------------------
xs <- 1:4
unlist(Map(\(x) x + 2, xs))  # explicit Map + unlist call


## -----------------------------------------------------------------------------
umap <- compose(unlist, Map) # composing Map and unlist
umap(\(x) x + 2, xs)         # using the new function


## -----------------------------------------------------------------------------
library(pryr)
umap <- unlist %.% Map
umap(\(x) x + 2, xs)


## -----------------------------------------------------------------------------
rmse <- sqrt %.% mean %.% \(x, y) (x - y)**2
rmse(1:4, 2:5)


## -----------------------------------------------------------------------------
`%;%` <- function(f, g) function(...) g(f(...))
rmse <- (\(x, y) (x - y)**2) %;% mean %;% sqrt
rmse(1:4, 2:5)


## -----------------------------------------------------------------------------
library(magrittr)
1:4 %>% mean %>% sqrt


## -----------------------------------------------------------------------------
1:4 |> mean() |> sqrt()


## -----------------------------------------------------------------------------
rnorm(4) %>% data.frame(x = ., y = cos(.))


## -----------------------------------------------------------------------------
rnorm(4) %>% data.frame(x = sin(.), y = cos(.))


## -----------------------------------------------------------------------------
mean_sqrt <- 1:4 %>% mean %>% sqrt
mean_sqrt


## -----------------------------------------------------------------------------
mean_sqrt <- . %>% mean %>% sqrt
mean_sqrt(1:4)


## -----------------------------------------------------------------------------
1:4 %>% mean_sqrt


## -----------------------------------------------------------------------------
rmse <- . %>% { (.$x - .$y)**2 } %>% mean %>% sqrt
data.frame(x = 1:4, y = 2:5) %>% rmse


## -----------------------------------------------------------------------------
rnorm(4) %>% { data.frame(x = sin(.), y = cos(.)) }

