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

test_that("join_with_max_length", {
    expect_equal(join_with_max_length(c("a", "b")), "a b")
    expect_equal(join_with_max_length(c("a", "b"), max=2), c("a", "b"))
    expect_equal(join_with_max_length(c("ar", "taj", "er", "trava", "stend", "bleck"), max=6), c("ar taj", "er", "trava", "stend", "bleck"))
    expect_equal(join_with_max_length(c("ar", "taj", "er", "trava", "stend", "bleck"), max=11), c("ar taj er", "trava stend", "bleck"))
})