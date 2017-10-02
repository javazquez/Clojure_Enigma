(ns enigma-machine.core
  (:require [clojure.spec.alpha :as s]))

;valid enigma has at least 3 rotors and a reflector

(s/def ::valid-alpha-string? (s/and 
                       string? 
                       #(not (empty? %))
                       #(re-seq #"^[A-Za-z]*$" %)))

(s/def ::twenty26-chars? #(= (count %) 26))

(s/def ::wiring  (s/and #(every? char? %)
                       ::twenty26-chars?))
(s/def ::offset int?)
(s/def ::notch char?)
(s/def ::reflector ::twenty26-chars?)
(s/def ::settings (s/and string?
                         #(= (count % ) 3)))
(s/def ::rotor (s/keys :req-un [::wiring ::offset ::notch]) )
(s/def ::plugboard (s/keys))
(s/def ::left-rotor ::rotor)
(s/def ::middle-rotor ::rotor)
(s/def ::right-rotor ::rotor)
(s/def ::unique-rotors #(apply distinct? %))

(s/def ::enigma-machine? (s/keys :req-un [::right-rotor ::middle-rotor ::left-rotor ::reflector ::settings]
                                 :opt-un [::plugboard]))

;;https://en.wikipedia.org/wiki/Enigma_rotor_details
;http://users.telenet.be/d.rijmenants/en/enigmatech.htm#reflector
(def alphabet (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def rotor1   {:wiring (seq "EKMFLGDQVZNTOWYHXUSPAIBRCJ")
               :offset 0
               :notch \Q })

(def rotor2   {:wiring (seq "AJDKSIRUXBLHWTMCQGZNPYFVOE")
               :offset 0
               :notch \E })

(def rotor3   {:wiring (seq "BDFHJLCPRTXVZNYEIWGAKMUSQO")
               :offset 0
               :notch \V })

(def reflector-a (seq "EJMZALYXVBWFCRQUONTSPIKHGD"))
(def reflector-b (seq "YRUHQSLDPXNGOKMIEBFZCWVJAT"))
(def reflector-c (seq "FVPJIAOYEDRZXWGCTKUQSBNMHL"))

(defn- reflect
  "reflect incoming letter using given reflector"
  [{:keys [reflector letter]}]
  (->> (.indexOf alphabet letter )
       (nth reflector )
       (hash-map :letter)))

(defn- step-position?
  "If the notch postion is 0 in the off-mapping, it means that it is in the 'A' 
  position"
  [rotor]
  (= (.indexOf alphabet (:notch rotor)) 
     (:offset rotor)))

(defn- setup-rotor
  "step a rotor to start position(a char) used in initialization"
  [{:keys [rotor start-pos]}]
  (let [pos (.indexOf alphabet start-pos)]
    (merge rotor {:offset pos })))

(defn- step-rotor
  "Step a single rotor "
  [rotor]
  (let [offset-pos (-> :offset rotor inc (mod 26))  ]
    (merge rotor {:offset offset-pos })))

(defn- step-machine
  "Step the right-rotor and all other rotors accordingly"
  [{:keys [left-rotor middle-rotor right-rotor] }]
  (let [mr-notch? (step-position? middle-rotor)
        mr (if (or (step-position? right-rotor) 
                   mr-notch?
                   (step-position? left-rotor)) ;double step 
             (step-rotor middle-rotor) 
             middle-rotor)
        lr (if mr-notch? 
             (step-rotor left-rotor)
             left-rotor)]
    {:left-rotor lr
     :middle-rotor mr
     :right-rotor (step-rotor right-rotor)}))

(defn- encode-helper
  "function that handles the translation between rotor inputs and the internal
  offeset due to stepping. translation-func will be provided by both inverse-encode 
  and encode functions"
  [{:keys [rotor letter translation-func]}]
  (let [in-letter-pos (.indexOf alphabet letter)
        offset-pos (mod (+ in-letter-pos (:offset rotor)) 26)  
        out-letter (->> offset-pos ;translated incoming position to cur rotor
                        (translation-func rotor)
                        (#(- % (:offset rotor))) ;translate it back 
                        (+ 26) ; incase it is negative 
                        (#(mod % 26)) ; simulate rotation
                        (nth alphabet ))]
    {:letter out-letter}))

(def inverse-map {:translation-func (fn [rotor pos]
                                      (->> (nth alphabet pos )
                                           (.indexOf (:wiring rotor))))})

(defn- inverse-encode
  "Inverse-encode letter with given rotor. Translation function will handle the 
  offset.. reverse of how encode handles it"
  [{:keys [letter rotor] :as params }]
  (encode-helper (conj  params inverse-map)))

(def encode-map {:translation-func (fn [rotor pos]
                                      (->> (nth (:wiring rotor) pos)
                                           (.indexOf alphabet )))})

(defn- encode
  "Encode a letter using the given rotor."
  [{:keys [rotor letter ] :as params}]
  (encode-helper (conj params encode-map) ))

(defn- thread-encoder
  "Helper function for pipeline encode/inverse-encode because a thread macro for 
  hash-maps is not available and (#(encode {:letter % rotor: rotorx})) looks ugly"
  [f & maps]
  (f (apply merge maps)))

(defn- string->char 
  "convert a string plugboard mapping to char in order to work with rotors/reflect which are chars"
  [str] 
  (map #(.charAt % 0) str ))

(defn create-mappings 
  [coll-1 coll-2]
  (into {}  
        (vec (map vec 
                  (partition 2 (interleave coll-1 coll-2))))))

(defn- setup-plugboard
  "take a map of wiring pairs. Each map pair implies the reverse 
  ie. {:A :Z} would also bind {:Z :A}. converting string keys into char keys  "
  [key-bindings]
  (let [ks (string->char (keys key-bindings))
        vs (string->char (vals key-bindings))]
    (merge (create-mappings ks vs)
           (create-mappings vs ks))))

(defn- plug-transpose
  "Take a plugboard and letter and return transposed letter. returns original letter if there is no match in plugboard"
  [{:keys [plugboard letter]}]
  {:letter (if-let [ch (get plugboard letter)]
              ch
              letter)})

(defn- get-rotor-settings
  [{:keys [left-rotor middle-rotor right-rotor]}]
  (apply str (map #(nth alphabet (:offset % )) 
                   [left-rotor middle-rotor right-rotor] )))

(defn- pipeline
  "End to end encoding of a letter through all rotors and reflector
returns back the entire enigma machine"
  [{:keys [reflector left-rotor middle-rotor right-rotor letter plugboard]:as e-machine}]
  (->> (plug-transpose {:plugboard plugboard :letter letter})
       (thread-encoder encode {:rotor  right-rotor }) ;rr-out
       (thread-encoder encode {:rotor middle-rotor })   ;mr-out
       (thread-encoder encode {:rotor left-rotor })     ;lr-out
       (thread-encoder reflect { :reflector reflector });reflect-val 
       (thread-encoder inverse-encode {:rotor left-rotor })
       (thread-encoder inverse-encode {:rotor middle-rotor })
       (thread-encoder inverse-encode {:rotor right-rotor })
       (thread-encoder plug-transpose {:plugboard plugboard})
       (merge e-machine ,,,)))

(defn- push-key
  "Simulate a key press. Right rotor will always be stepped. updated settings is needed in case a rotor is setup outside of initialization"
  [{:keys [reflector letter  plugboard] :as e-machine }]
  (let [stepped-rotors (step-machine e-machine)]
    (merge
     (pipeline (merge 
                stepped-rotors 
                {:reflector reflector
                 :plugboard plugboard
                 :letter letter} ))
     {:settings (get-rotor-settings stepped-rotors) })))

(defn enigma-machine
  "create an enigma machine"
  [{:keys [left-rotor middle-rotor right-rotor reflector settings plugboard] :as e-machine}]
  (when-not (s/valid? ::enigma-machine? e-machine)
    (throw (ex-info (str "invalid input: invalid enigma machine "
                         (s/explain-str ::enigma-machine? e-machine))
                    {})))
  (when-not (s/valid? ::unique-rotors [(:wiring left-rotor)
                                       (:wiring middle-rotor) 
                                       (:wiring right-rotor)])
    (throw (ex-info (str "invalid input: need unique rotors " 
                         (s/explain-str ::unique-rotors [left-rotor middle-rotor right-rotor]))
                    {})))
  (let [[i,ii,iii] settings ]
    {:left-rotor (setup-rotor {:rotor left-rotor, :start-pos i})
     :middle-rotor (setup-rotor {:rotor middle-rotor, :start-pos ii})
     :right-rotor (setup-rotor {:rotor right-rotor, :start-pos iii})
     :reflector reflector
     :plugboard (setup-plugboard  plugboard)} ))

(defn encode-string
  "encode a string. Each encoded character returns a full enigma machine along with an output {:letter \"<some char>\"}
  Machines are functions that are mapped to each character in the message. Reductions is initialized with a full machine and letter and produces a machine. This machine represents the updated settings that is fed into the next machine (->m1 m2) which was already mapped to an input character from the original message."
  [e-machine msg]
  (when-not (s/valid? ::valid-alpha-string? msg)
    (throw (ex-info (str "invalid input " 
                         (s/explain-str ::valid-alpha-string? msg))
                    {})))
  (let  [[first-char & chrs] (seq msg)
         machines (map (fn [ch]                         
                         #(push-key (merge 
                                     % 
                                     {:letter ch})))
                       chrs)
         reducts  (reductions (fn [machine-1 machine-2]
                              (-> machine-1 
                                  machine-2)) 
                            (push-key (merge 
                                       e-machine
                                       {:letter first-char}));initial machine 
                            machines)] 
    {:result  (apply str 
                     (map :letter 
                          reducts))
     :enigma-machine (first (reverse reducts))}))





