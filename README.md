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

## List of resources used consulted

- http://enigma.louisedade.co.uk/enigma.html
- Enigma Machine Mechanism (feat. a 'Double Step')  https://www.youtube.com/watch?v=hcVhQeZ5gI4
- http://users.telenet.be/d.rijmenants/en/enigmatech.htm#reflector
- https://www.scienceabc.com/innovation/the-imitation-game-how-did-the-enigma-machine-work.html
- https://en.wikipedia.org/wiki/Enigma_machine
- https://en.wikipedia.org/wiki/Cryptanalysis_of_the_Enigma
- https://en.wikipedia.org/wiki/Enigma_rotor_details
- http://practicalcryptography.com/ciphers/enigma-cipher/