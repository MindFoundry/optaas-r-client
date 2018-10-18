context("Version Check")

mock_client_version <- '3.1.5'

equal_server_version_with_post <- '3.1.5.postX'

newer_server_versions <- c('3.1.6', '3.2.0', '3.10.0', '4.0.0')
older_server_versions <- c('3.1.4', '3.0.99', '2.99.99')

expected_new_version_warning <- paste(
    "A new version of the OPTaaS client is available",
    ".+",
    "\\(\\'MindFoundry/optaas-r-client@VERSION\\'\\)"
, sep="")

expected_old_version_warning <- paste(
    "Your OPTaaS client version is not in sync with your OPTaaS server",
    ".+",
    "\\(\\'MindFoundry/optaas-r-client@VERSION\\'\\)"
, sep="")

test_that("No message if server version is missing", {
    expect_silent(check_version(NULL, mock_client_version))
})

test_that("No message if server version has same prefix", {
    expect_silent(check_version(equal_server_version_with_post, mock_client_version))
    expect_silent(check_version(mock_client_version, mock_client_version))
})

test_that("Warning appears if new version is available ", {
    for (version in newer_server_versions) {
        expected_warning <- gsub("VERSION", version, expected_new_version_warning)
        expect_warning(check_version(version, mock_client_version), expected_warning)
    }
})

test_that("Warning appears if server version is older ", {
    for (version in older_server_versions) {
        expected_warning <- gsub("VERSION", version, expected_old_version_warning)
        expect_warning(check_version(version, mock_client_version), expected_warning)
    }
})

test_that("Warning appears if new version with suffix is available ", {
    expected_warning <- gsub("VERSION", '3.1.6', expected_new_version_warning)
    expect_warning(check_version('3.1.6.postX', mock_client_version), expected_warning)
})

test_that("Warning appears if old version with suffix is available ", {
    expected_warning <- gsub("VERSION", '3.1.4', expected_old_version_warning)
    expect_warning(check_version('3.1.4.postX', mock_client_version), expected_warning)
})
