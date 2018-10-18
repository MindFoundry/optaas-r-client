context("API Keys")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

expect_key_is_present = function(all_keys, id, role, expired=FALSE) {
    found_key <- FALSE
    for (key in all_keys) {
        if (key$id == id) {
            expect_equal(key$role, role)
            expect_equal(key$expired, expired)
            expect_equal(key$json$id, id)
            expect_equal(key$json$role, role)
            expect_equal(key$json$expired, expired)
            found_key <- TRUE
            break
        }
    }
    expect_true(found_key)
}

test_that("Can get API keys", {
    api_keys <- client$get_api_keys()
    expect_key_is_present(api_keys, id=OPTAAS_API_KEY, role="admin")
})

verify_key = function(api_key, role) {
    expect_equal(api_key$role, role)
    expect_equal(api_key$json$role, role)
    
    expect_equal(api_key$expired, FALSE)
    expect_equal(api_key$json$expired, FALSE)
    
    all_keys <- client$get_api_keys()
    expect_key_is_present(all_keys, id=api_key$id, role=role)
    
    client_for_key <- OPTaaSClient$new(OPTAAS_URL, api_key$id)
    expect_silent(client_for_key$get_all_tasks())
    
    if (role == "admin") {
        expect_equal(length(all_keys), length(client_for_key$get_api_keys()))
    } else {
        expect_error(client_for_key$get_api_keys(), "Status: 401   Message: Invalid or missing API Key")
    }

    if (role == "read_only") {
        expect_error(client_for_key$create_task(title="dummy task", parameters=list(BoolParameter("dummy_bool"))), 
                     "Status: 401   Message: Invalid or missing API Key")
    } else {
        expect_silent(client_for_key$create_task(title="dummy task", parameters=list(BoolParameter("dummy_bool"))))
    }
}

test_that("Can generate new key", {
    new_key <- client$generate_api_key()
    verify_key(new_key, role="standard")
})

test_that("Can generate new admin key", {
    new_key <- client$generate_api_key(role="admin")
    verify_key(new_key, role="admin")
})

test_that("Can generate new read-only key", {
    new_key <- client$generate_api_key(role="read_only")
    verify_key(new_key, role="read_only")
})

test_that("Can change role", {
    new_key <- client$generate_api_key(role="read_only")
    new_key$set_role("admin")
    verify_key(new_key, role="admin")
    new_key$set_role("standard")
    verify_key(new_key, role="standard")
    new_key$set_role("read_only")
    verify_key(new_key, role="read_only")
})

test_that("Can expire and un-expire key", {
    new_key <- client$generate_api_key()
    new_key$set_expired(TRUE)
    
    expect_key_is_present(client$get_api_keys(), new_key$id, "standard", TRUE)
    
    client_for_key <- OPTaaSClient$new(OPTAAS_URL, new_key$id)
    expect_error(client_for_key$get_all_tasks(), "Status: 401   Message: Invalid or missing API Key")
    
    new_key$set_expired(FALSE)
    verify_key(new_key, role="standard")
})

