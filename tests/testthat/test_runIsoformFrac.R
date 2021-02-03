context("DGEobj.utils - tests for isoformFrac.R functions")


test_that("isoformFrac.R: isoformFrac()", {
    skip_if(is.null(t_obj1))
    dgeObj <- t_obj1
    expect_s3_class(dgeObj, "DGEobj")
    expect_error(isoformFrac(dgeObj$DGEList),
                 regexp = "dgeObj must be of class 'DGEobj.")
    expect_error(isoformFrac(dgeObj),
                 regexp = "The levels attribute of dgeObj must be 'isoform'.")

    dgeObj <- addItem(dgeObj   = dgeObj,
                              item     = dgeObj$geneData,
                              itemName = "isoformData",
                              itemType = "meta")

    attr(dgeObj, "level") <- "isoform"
    iso_data <- isoformFrac(dgeObj)
    expect_equal(dim(iso_data), c(946, 48))
})
