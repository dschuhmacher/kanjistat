test_that("initialize kanjistat options", {
  expect_true( exists("kanjistat_env") )
  expect_false( exists("kanjistat_env$kanjistatOptions") )
  expect_type( kanjistat_options(), "list") 
    # more things hard to test. of course the .Rkanjistat-profile is not meant to
    # replace "kanjistat_env$kanjistatOptions" by the emptylist, but it can do so (AFAICS)
    # because it gets called before the namespace is sealed
})

test_that("codepointToKanji and kanjiToCodepoint", {
  expect_equal( codepointToKanji(c("51b7", "6696", "71b1")), c("冷","暖","熱") )
  expect_equal( codepointToKanji(c("51b7", "6696", "71b1"), concat = TRUE), c("冷暖熱") )
  expect_equal( codepointToKanji(as.hexmode(c("51b7", "6696", "71b1"))), c("冷","暖","熱") )
  expect_equal( kanjiToCodepoint(c("冷","暖","熱")), as.hexmode(c("51b7", "6696", "71b1")) )
  expect_equal( kanjiToCodepoint(c("冷","暖","熱"), character = TRUE), c("51b7", "6696", "71b1") )
  expect_equal( kanjiToCodepoint(c("冷暖熱")), as.hexmode(c("51b7", "6696", "71b1")) )
})

# these following also tests well-behavedness of kbase to some extent, the latter should be 
# more comprehensively tested elsewhere
test_that("samplekan", {  
  expect_equal( sort(samplekan("kyouiku", size = 1026)), sort(kbase$kanji[kbase$class=="kyouiku"]))
  expect_error( samplekan("kyouiku", size = 1027))
  expect_equal( sort(samplekan("jouyou", size = 2136)), sort(kbase$kanji[kbase$grade <= 8]))
  expect_error( samplekan("jouyou", size = 2137))
  
})
