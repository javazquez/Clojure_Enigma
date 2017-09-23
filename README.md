# Clojure_Enigma
Enigma machine written in clojure


## How to run the tests

I had to add `[lein-midje "3.1.3"]` to my ~/.lein/profiles.clj

```clojure
{:user {:plugins [[cider/cider-nrepl "0.8.1"]
                  [lein-midje "3.1.3"]]}}
```

`lein midje` will run all tests.

`lein midje namespace.*` will run only tests beginning with "namespace.".

`lein midje :autotest` will run all the tests indefinitely. It sets up a
watcher on the code files. If they change, only the relevant tests will be
run again.
