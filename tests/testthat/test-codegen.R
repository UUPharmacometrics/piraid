context("Code generator")
empty <- code_generator()

test_that("Constructor", {
    code <- get_code(empty)
    expect_equal(code, "\n")
})

test_that("Indent line", {
    line <- indent_line("X = 1", indent_level=2)  
    expect_equal(line, "        X = 1")
    line <- indent_line("X = 1", indent_level=0)
    expect_equal(line, "X = 1")
})

test_that("Add line", {
    cg <- add_line(empty, "CONTINUE")
    code <- get_code(cg)
    expect_equal(code, "CONTINUE\n")
})