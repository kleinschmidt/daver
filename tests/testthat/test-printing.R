context('Printing helpers')

test_that('paste_with_last', {
  paste_with_last(1:3, ' ', ' and ') == '1 2 and 3'
})

test_that('paste_and', {
  paste_and(1:3) == '1, 2, and 3'
  paste_and(1:3, ' ') == '1 2 and 3'
})

