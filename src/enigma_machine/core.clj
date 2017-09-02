(ns enigma-machine.core)

;;https://en.wikipedia.org/wiki/Enigma_rotor_details
;http://users.telenet.be/d.rijmenants/en/enigmatech.htm#reflector

(def alphabet (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def rotor1   {:wiring (seq "EKMFLGDQVZNTOWYHXUSPAIBRCJ")
               :offset 0
               :current-char \E
               :notch \Q
               })

(def rotor2   {:wiring (seq "AJDKSIRUXBLHWTMCQGZNPYFVOE")
               :offset 0
               :current-char \A
               :notch \E
               })

(def rotor3   {:wiring (seq "BDFHJLCPRTXVZNYEIWGAKMUSQO")
               :offset 0
               :current-char \B ;use this to check for stepping
               :notch \V})

(def reflector-a (seq "EJMZALYXVBWFCRQUONTSPIKHGD"))
(def reflector-b (seq "YRUHQSLDPXNGOKMIEBFZCWVJAT"))
(def reflector-c (seq "FVPJIAOYEDRZXWGCTKUQSBNMHL"))


(defn reflect
  "reflect incoming letter using given reflector"
  [{:keys [reflector letter]}]
  (->> (.indexOf alphabet letter )
       (nth reflector )
       (hash-map :letter)))

(defn step-position?
  "If the notch postion is 0 in the off-mapping, it means that it is in the 'A' 
  position"
  [rotor]
  (= (:notch rotor) 
     (:current-char rotor)))

(defn setup-rotor
  "step a rotor to start position(a char) used in initialization"
  [{:keys [rotor start-pos]}]
  (let [pos (+ (:offset rotor) (.indexOf alphabet start-pos))]
    (merge rotor {:offset pos
                  :current-char (nth alphabet pos)})))

(defn step-rotor
  "Step a single rotor "
  [ rotor]
  (let [offset-pos (-> :offset rotor inc (mod 26))  ]
    (merge rotor {:offset offset-pos
                  :current-char (nth (:wiring rotor) offset-pos )})))

(defn step-machine
  "Step the right-rotor and all other rotors accordingly"
  [{:keys [left-rotor middle-rotor right-rotor] :as rotors}]
  (let [lr-notch? (step-position? left-rotor) 
        mr-notch? (step-position? middle-rotor)
        rr-notch? (step-position? right-rotor)
        rr (step-rotor right-rotor)
        mr (if (or rr-notch? 
                   mr-notch?
                   lr-notch?) ;double step 
             (step-rotor middle-rotor) 
             middle-rotor)
        lr (if mr-notch? 
             (step-rotor left-rotor)
             left-rotor)]
    {:left-rotor lr
     :middle-rotor mr
     :right-rotor rr}))

(defn- encode-helper
  "function that handles the translation between rotor inputs and the internal
  offeset due to stepping. translation-func will be provided by both inverse-encode 
  and encode functions"
  [{:keys [rotor letter  translation-func]}]
  (let [in-letter-pos  (.indexOf alphabet letter) ;j
        offset-pos (mod (+ in-letter-pos (:offset rotor)) 26)  
        out-letter (->> offset-pos ;translated incoming position to cur rotor
                        translation-func
                        (#(- % (:offset rotor))) ;translate it back 
                        (+ 26) ; incase it is negative 
                        (#(mod % 26)) ; simulate rotation
                        (nth alphabet ))]
    {:letter  out-letter}))

(defn inverse-encode
  "Inverse-encode letter with given rotor. Translation function will handle the 
  offset.. revese of how encode handles it"
  [{:keys [letter rotor] :as pass-thru}]
  (encode-helper {:letter letter 
                  :rotor rotor
                  :translation-func (fn [pos]
                                      (->>  (nth alphabet pos )
                                            (.indexOf (:wiring rotor))))}))

(defn encode
  "Encode a letter using the given rotor."
  [{:keys [rotor letter]}]
  (encode-helper {:letter letter 
                  :rotor rotor
                  :translation-func (fn [pos] ;translate offset to 
                                      (->> (nth (:wiring rotor) pos)
                                           (.indexOf alphabet )))}))

(defn- thread-encoder
  "Helper function for pipeline encode/inverse-encode because a thread macro for 
  hash-maps is not available and (#(encode {:letter % rotor: rotorx})) looks ugly"
  [f & maps]
  (f (apply merge maps)))

(defn pipeline
  "End to end encoding of a letter through all rotors and reflector"
  [{:keys [reflector left-rotor middle-rotor right-rotor letter]}]
  (->> (encode {:rotor  right-rotor, :letter  letter }) ;rr-out
       (thread-encoder encode {:rotor middle-rotor })   ;mr-out
       (thread-encoder encode {:rotor left-rotor })     ;lr-out
       (thread-encoder reflect { :reflector reflector });reflect-val 
       (thread-encoder inverse-encode {:rotor left-rotor })
       (thread-encoder inverse-encode {:rotor middle-rotor })
       (thread-encoder inverse-encode {:rotor right-rotor })))

(defn push-key
  "Simulate a key press. Right rotor will always be stepped."
  [{:keys [left-rotor middle-rotor right-rotor reflector letter settings]}]
  (let [[i,ii,iii] settings 
        lr (setup-rotor {:rotor  left-rotor, :start-pos i})
        mr (setup-rotor {:rotor middle-rotor, :start-pos ii})
        rr (setup-rotor {:rotor  right-rotor, :start-pos iii})
        stepped-rotors (step-machine {:left-rotor lr
                                      :middle-rotor mr
                                      :right-rotor rr})]
    (pipeline (merge 
               stepped-rotors 
               {:reflector reflector
                :letter letter} ))))







