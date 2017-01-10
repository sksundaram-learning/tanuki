# TODO

## General Items

1. Allow browsing by year, the month
1. Add an "/admin" scope for various functions
    - Renaming a tag across all documents
    - Renaming a topic across all documents
    - Renaming a location across all documents
    - Finding and merging duplicate records
    - Changing a tag to a location across all documents
    - Changing a tag to a topic across all documents
    - After all that is done, remove all of the corresponding apps
1. Might be nice to reduce the console output when running `mix ct`
1. Get the overlays working in the release (see `rel/config.exs`)
1. See if there is an easy way to get the `Version` file working again
    - Maybe just generate it using some code in `rel/config.exs`
1. Read about deployment in Phoenix:
    - http://www.phoenixframework.org/docs/advanced-deployment
    - https://medium.com/@brucepomeroy/create-an-elixir-umbrella-project-containing-a-phoenix-app-and-build-a-release-with-distillery-46371f2617df#.oinfggn3b
1. Get the `COOKIE` envar thing working for `:prod` in `rel/config.exs`
1. Look for bootstrap css guide to select a good style
1. Fix image references in error view
1. Find out where the Elixir apps will be writing their logs
1. Eventually rewrite everything in Elixir
    - Running CommonTest with `ct` plugin is ugly as sin
    - Seems like `mix ct` doesn't run the tests in test env mode, despite code being in place
1. Write a test for `fetch_document` in `tanuki_backend`
1. Ensure QuickTime videos play in the browser
1. Produce thumbnails for the videos and cache them
    * https://github.com/emedia-project/erlffmpeg
1. Can we produce thumbnails for AVI files?
1. Show list of known locations
1. Show list of known topics

## Documentation

1. Write an architecture document that explains the components:
    * CouchDB
        - couchbeam
    * Incoming
    * Backend
    * Frontend
        - Phoenix
            + Cowboy
            + Plug
        - Elixir
    * Describe how pages are rendered (Phoenix endpoints, router, controllers, views)
