context("DGEobj.utils - tests for convertCounts.R functions")


test_that("convertCounts.R: convertCounts()", {
    # CPM
    count_matrix <- convertCounts(counts      = t_obj1$counts_orig,
                                  unit        = "CPM",
                                  log         = FALSE,
                                  normalize   = "none",
                                  prior.count = NULL)

    expect_true("matrix" %in% class(count_matrix))
    expect_identical(dim(t_obj1$counts_orig), dim(count_matrix))
    expect_error(convertCounts(unit = "CPM"),
                 msg = "argument \"counts\" is missing, with no default")
    expect_error(convertCounts(counts = t_obj1$counts_orig),
                 msg = "argument \"unit\" is missing, with no default")
    expect_equal(dim(convertCounts(counts = t_obj1$counts_orig[1:100,], unit = "CPM")),
                   c(100, 48))

    # TPM
    gene_data <- getItem(t_obj1, "geneData")
    genelength <- c(gene_data$end - gene_data$start)
    count_matrix <- convertCounts(counts      = t_obj1$counts,
                                  unit        = "TPM",
                                  geneLength  = genelength ,
                                  log         = FALSE,
                                  normalize   = "none",
                                  prior.count = NULL)

    expect_true("matrix" %in% class(count_matrix))
    expect_identical(dim(t_obj1$counts), dim(count_matrix))
    expect_error(convertCounts(counts = t_obj1$counts, unit = "TPM"), regexp = "geneLength is required for unit = FPK|FPKM|TPM")
    expect_warning(convertCounts(counts = t_obj1$counts, unit = "TPM", geneLength = genelength, normalize = "TMM"),
                   regexp = "TPM normalization overides TMM normalization!")
    expect_warning(convertCounts(counts = t_obj1$counts, unit = "TPM", geneLength = genelength, prior.count = 1,log = TRUE),
                   regexp = "Using a prior.count for logTPM calculations is not recommended and may produce unpredictable results!")

    expect_equal(dim(convertCounts(counts = t_obj1$counts_orig[1:100,], unit = "TPM", geneLength  = genelength[1:100])),
                 c(100, 48))
    }
)

test_that("convertCounts.R: tpm.on.subset()", {
    geneLength <- NULL
    if (attr(t_obj1, "source") == "Omicsoft") {
        gene_data <- getItem(t_obj1, "geneData")
        geneLength <- c(gene_data$end - gene_data$start)
    } else if ("effectiveLength_orig" %in% names(t_obj1)) {
        geneLength <- rowMeans(getItem(t_obj1, "effectiveLength_orig"), na.rm = TRUE)
    }
    skip_if(is.null(geneLength))

    # testing level isoform
    isoform_dgeObj <- t_obj1
    isoform_dgeObj <- addItem(dgeObj   = isoform_dgeObj,
                              item     = isoform_dgeObj$geneData_orig,
                              itemName = "isoformData_orig",
                              itemType = "meta")
    attr(isoform_dgeObj, "level") <- "isoform"
    tpmObj <- tpm.on.subset(isoform_dgeObj)
    expect_true("matrix" %in% class(tpmObj))

    expect_error(tpm.on.subset("XYZ"), regexp = "dgeObj should be of class 'DGEobj'.")
    # testing level exon
    exon_dgeObj <- t_obj1
    attr(exon_dgeObj, "level") <- "exon"
    expect_error(tpm.on.subset(exon_dgeObj),
                 regexp = "The level of dgeObj should be of type 'isoform' or type 'gene'.")

    # testing for bad source attribute
    attr(isoform_dgeObj, "source") <- "XYZ"
    expect_error(tpm.on.subset(isoform_dgeObj),
                 regexp = "object 'geneLength' not found")
})

test_that("convertCounts.R: tpm.direct()", {
    skip_if(is.null(getItem(t_obj1, "geneData")$ExonLength))

    genelength <- getItem(t_obj1, "geneData")$ExonLength
    tpmObj <- tpm.direct(t_obj1$counts, geneLength = genelength)
    expect_true("matrix" %in% class(tpmObj))

    # testing count as vector type
    tpmObj <- tpm.direct(counts = genelength, geneLength = genelength)
    expect_true("matrix" %in% class(tpmObj))

    # testing bad genelength parameter
    expect_error(tpm.direct(t_obj1$counts, geneLength = as.data.frame(genelength)),
                 regexp = "The dimensions of counts and geneLength should match.")

    # testing collapse parameter
    tpmObj <- tpm.direct(t_obj1$counts, geneLength = as.matrix(genelength), collapse = TRUE)
    expect_true("matrix" %in% class(tpmObj))
})
