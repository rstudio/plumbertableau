# This test uses a snapshot test.
# (https://testthat.r-lib.org/articles/snapshotting.html)
# If it fails, but the user guide is displaying correctly, update it with 
# > snapshot_accept("user_guide")
test_that("user_guide", {
  pr <- plumber::plumb(pr_path())
  expect_snapshot(cat(render_user_guide("", pr)))
})
