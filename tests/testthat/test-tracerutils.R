context("Check the 3D plot of neurons and associated volumes")

conn=try(catmaid::catmaid_login(Force = TRUE), silent = TRUE)

test_that("can plot neuron by name", {

  if(inherits(conn, 'try-error')) skip('No catmaid connection')

  test_neuronlist <- plot_catmaid(skid = 'WTPN2017_uPN_right')

  #poke the list to see if it is neuronlist
  expect_is(test_neuronlist, 'neuronlist')
  #just poke an individual neuron and see if it is actually a catmaidneuron
  expect_is(test_neuronlist[[1]], 'catmaidneuron')

})

test_that("can plot neuron by number(skid)", {

  if(inherits(conn, 'try-error')) skip('No catmaid connection')

  test_neuronlist <- plot_catmaid(skid = 21999)

  #poke the list to see if it is neuronlist
  expect_is(test_neuronlist, 'neuronlist')
  #just poke an individual neuron and see if it is actually a catmaidneuron
  expect_is(test_neuronlist[[1]], 'catmaidneuron')

})

test_that("can plot neuron along with its volume", {

  if(inherits(conn, 'try-error')) skip('No catmaid connection')

  test_neuronlist <- plot_catmaid(skid = 'WTPN2017_uPN_right', volumes = 'v14.neuropil')

  #poke the list to see if it is neuronlist
  expect_is(test_neuronlist, 'neuronlist')
  #just poke an individual neuron and see if it is actually a catmaidneuron
  expect_is(test_neuronlist[[1]], 'catmaidneuron')

})
