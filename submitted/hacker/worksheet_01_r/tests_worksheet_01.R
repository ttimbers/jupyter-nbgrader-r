library(testthat)
library(digest)

test_2.1 <- function(){
    test_that('Solution is incorrect, the dplyr package needs to be loaded. Try doing this with the library function.', {
        expect_true("package:dplyr" %in% search())
        })
    test_that('Solution is incorrect, the ggplot2 package needs to be loaded. Try doing this with the library function.', {
        expect_true("package:ggplot2" %in% search())
        })
    test_that('Solution is incorrect, the readr package needs to be loaded. Try doing this with the library function.', {
        expect_true("package:readr" %in% search())
        })
    print("Success!")
}

test_2.2 <- function(){
    if (digest(toupper(answer2.2)) == "75f1160e72554f4270c809f041c7a776") {
        print("Not quite! We are trying to visualize the relationship between two quantitative variables, bar charts are not the most effective visualization for this.")
    } else if (digest(toupper(answer2.2)) == "3a5505c06543876fe45598b5e5e5195d") {
        print("Not quite! We are trying to visualize the relationship between two quantitative variables, pie charts are not the most effective visualization for this.")
    } else if (digest(toupper(answer2.2)) == "c1f86f7430df7ddb256980ea6a3b57a4") {
        print("Not quite! We are trying to visualize the relationship between two quantitative variables, box plots are not the most effective visualization for this.")
    }
    test_that('Solution is incorrect', {
        expect_equal(digest(toupper(answer2.2)), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    print("Success!")
}

test_2.3 <- function(){
    test_that('Did not create an object named race_times', {
        expect_true(exists("race_times"))
        })
    test_that('race_times should be a data frame', {
        expect_true('data.frame' %in% class(race_times))
        })
    test_that('race_times does not contain the correct data.', {
        expect_equal(dim(race_times), c(1833, 5))
        expect_equal(sum(race_times$age), 66455.5)
        expect_equal(colnames(race_times), c("age", "bmi", "km5_time_seconds", "km10_time_seconds", "sex"))
        })
    print("Success!")
    }

test_2.4 <- function(){
    test_that('race_times_women has the incorrect number of rows, did you remember to filter for females?', {
        expect_equal(digest(nrow(race_times_women)), '22c7b9e96a1f1a8c4a13dc8b6586dc80') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('race_times_women has the incorrect number of columns', {
        expect_equal(digest(ncol(race_times_women)), 'dd4ad37ee474732a009111e3456e7ed7') # we hid the answer to the test here so you can't see it, but we can still run the test   
        })
    test_that('race_times_women has the wrong column names', {
        expect_equal(digest(paste0(sort(colnames(race_times_women)), collapse = "")), 'e40c810969d18f6fff2c8a290c0ffb37') # we hid the answer to the test here so you can't see it, but we can still run the test   
        })
    test_that('race_times_women bmi column contains the incorrect values, did you remember to filter for females?', {
        expect_equal(digest(as.numeric(sum(race_times_women$bmi))), '7cc4baefd16add414fe6a9e051a2f5f5') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    print("Success!")
}

test_2.5 <- function(){
    test_that('Did not create a plot named race_times_plot', {
        expect_true(exists("race_times_plot")) 
        })
    test_that('bmi should be on the x-axis.', {
        expect_true("bmi" %in% c(rlang::get_expr(race_times_plot$mapping$x), rlang::get_expr(race_times_plot$layers[[1]]$mapping$x)))
        })
    test_that('km5_time_minutes should be on the y-axis.', {
        expect_true("km5_time_seconds" %in% c(rlang::get_expr(race_times_plot$mapping$y), rlang::get_expr(race_times_plot$layers[[1]]$mapping$y)))
        })
    test_that('race_times_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(race_times_plot$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(race_times_plot$labels$x == 'bmi')
        expect_false(race_times_plot$labels$y == 'km5_time_seconds')
        })
    print("Success!")
}

test_2.6 <- function(){
    if (digest(toupper(answer2.6)) == "75f1160e72554f4270c809f041c7a776") {
        print("Not quite! Look at how as the values for BMI increase, so do the values for race time.")
    } else if (digest(toupper(answer2.6)) == "475bf9280aab63a82af60791302736f6") {
        print("Close, but not quite! You are right that there is a relationship, but check the direction.")
    }
    test_that('Solution is incorrect', {
        expect_match(digest(toupper(answer2.6)), '3a5505c06543876fe45598b5e5e5195d')
        })
    print("Success!")
}
