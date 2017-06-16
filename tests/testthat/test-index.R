library(eodplotter)

test_that("get peaks", {
    p=peakFinder('file.tdms')
    e=read.csv('peaks.csv',stringsAsFactors=F)
    expect_equal(p,e)
})

