(ns enigma-machine.core-test
  (:use midje.sweet)
  (:require [enigma-machine.core :refer :all]))

(facts "test rotor 1 in A with no offset"
  (fact "A encoded as B"
    (#'enigma-machine.core/encode {:letter \A, :rotor rotor1}) => {:letter \E})

  (fact "B encoded as K"
    (#'enigma-machine.core/encode {:letter \B, :rotor rotor1}) => {:letter \K})

  (fact "K encoded as N"
    (#'enigma-machine.core/encode {:letter \K, :rotor  rotor1}) => {:letter \N})

  (fact "stepping rotor1, A encoded as J"
    (#'enigma-machine.core/encode {:letter \A, :rotor (#'enigma-machine.core/step-rotor rotor1)}) => {:letter \J}))

(facts "test individual rotors assumming 'AAA' with simulated pushkey "
  (fact "rotor 3: A -> K => J "
    (#'enigma-machine.core/encode {:letter \A, :rotor (#'enigma-machine.core/step-rotor rotor3)}) => {:letter \C})

  (fact "rotor 2: B -> J"
    (#'enigma-machine.core/encode {:letter \C, :rotor rotor2}) => {:letter \D})

  (fact "rotor 1: J -> Z"
    (#'enigma-machine.core/encode {:letter \D, :rotor rotor1}) => {:letter \F} )

  (fact "reflector: Z -> T"
    (#'enigma-machine.core/reflect {:letter \F, :reflector reflector-b}) => {:letter  \S})

  (fact "rotor 1 reverse: T -> L "
    (#'enigma-machine.core/inverse-encode {:letter \S, :rotor rotor1}) => {:letter \S} )

  (fact "rotor 2 reverse: L -> K "
    (#'enigma-machine.core/inverse-encode {:letter \S, :rotor rotor2}) => {:letter \E} )

  (fact "rotor 3 reverse: K -> U "
    (#'enigma-machine.core/inverse-encode {:letter \E, :rotor (#'enigma-machine.core/step-rotor rotor3)}) => {:letter \B}))


(fact "verify rotor setup"
  (fact "setup rotor 1 in B position "
    (->> (:current-char (#'enigma-machine.core/setup-rotor {:rotor rotor1 
                                      :start-pos \B})) 
         (.indexOf alphabet)) => 1 )

  (fact "setup rotor in C position "
    (->> (:current-char (#'enigma-machine.core/setup-rotor {:rotor rotor1
                                      :start-pos \C}))
         (.indexOf alphabet)) => 2 )

  (fact "setup rotor 1 in Z position "
    (->> (:current-char (#'enigma-machine.core/setup-rotor {:rotor rotor1 
                                      :start-pos \Z}))
         (.indexOf alphabet)) => 25 ))


(facts "test notch positions"
  (fact "rotor one in the Q postion is true"
    (#'enigma-machine.core/step-position? (#'enigma-machine.core/setup-rotor 
                     {:rotor  rotor1, :start-pos \Q})) => true)
  (fact "rotor one in the Q postion is false"
    (#'enigma-machine.core/step-position? (#'enigma-machine.core/setup-rotor 
                     {:rotor  rotor1, :start-pos \H})) => false)

  (fact "rotor one in the Q postion after stepping is true"
    (#'enigma-machine.core/step-position?
     (#'enigma-machine.core/step-rotor  (#'enigma-machine.core/setup-rotor 
                                         {:rotor  rotor1
                                          :start-pos \P }))) => true)
  
  
  (fact "rotor 3 is in the V position is true"
    (#'enigma-machine.core/step-position? (#'enigma-machine.core/setup-rotor
                     {:rotor rotor3, :start-pos \V})) => true))

(facts "step machine"
  (fact "stepping AAA R-b III, II,I"
    (#'enigma-machine.core/step-machine 
     (#'enigma-machine.core/step-machine {:left-rotor rotor3
                    :middle-rotor rotor2
                    :right-rotor rotor1
                    })) => {:left-rotor rotor3
                            :middle-rotor rotor2
                            :right-rotor  {:wiring (seq "EKMFLGDQVZNTOWYHXUSPAIBRCJ")
                                           :offset 2
                                           :current-char \M
                                           :notch \Q
                                           }}))

;With the rotors I, II and III (from left to right), wide B-reflector, all ring settings in A-position, and start position AAA, typing AAAAA will produce the encoded sequence BDZGO 


(facts "test AAA config for R-B, III, II I,  BDZGO"
  (fact "reflector-b, III, II, I in AAA settings produces A->B"
    (encode-string (enigma-machine {:reflector reflector-b
                         :left-rotor rotor1
                         :middle-rotor rotor2
                         :right-rotor rotor3
                         :settings "AAA"
                         }) "A") => "B")

  (fact "reflector-b, III, II, I in AAB settings produces A->D"
    (encode-string (enigma-machine {:reflector reflector-b
                        :left-rotor rotor1
                        :middle-rotor rotor2
                        :right-rotor rotor3
                        :settings "AAB"
                        }) "A") =>  "D")

  (fact "reflector-b, III, II, I in AAC settings produces A->Z"
    (encode-string (enigma-machine {:reflector reflector-b
                  :left-rotor rotor1
                  :middle-rotor rotor2
                  :right-rotor rotor3
                  :settings "AAC"
                  }) "A") => "Z")

  (fact "reflector-b, III, II, I in AAC settings produces A->Z"
    (encode-string (enigma-machine {:reflector reflector-b
                        :left-rotor rotor1
                        :middle-rotor rotor2
                        :right-rotor rotor3
                        :settings "AAD"
                        }) "A") => "G")

(fact "reflector-b, III, II, I in AAC settings produces A->Z"
  (encode-string (enigma-machine {:reflector reflector-b
                      :left-rotor rotor1
                      :middle-rotor rotor2
                      :right-rotor rotor3
                      :settings "AAE"
                      }) "A") => "O")

(fact "reflector-b, III, II, I in AAC settings produces A->Z"
  (encode-string (enigma-machine {:reflector reflector-b
                      :left-rotor rotor1
                      :middle-rotor rotor2
                      :right-rotor rotor3
                      :settings "AAF"
                      }) "A") =>  "W")

(fact "reflector-b, III, II, I in AAC settings produces A->Z"
  (encode-string (enigma-machine {:reflector reflector-b
                      :left-rotor rotor1
                      :middle-rotor rotor2
                      :right-rotor rotor3
                      :settings "AAG"
                      }) "A") =>  "C"))  
;In a three-rotor machine, double-stepping affected rotor two only. If in moving forward the ratchet of rotor three was engaged, rotor two would move again on the subsequent keystroke, resulting in two consecutive steps. Rotor two also pushes rotor one forward after 26 steps, but since rotor one moves forward with every keystroke anyway, there is no double-stepping.[10] This double-stepping caused the rotors to deviate from odometer-style regular motion.


(facts "Normal sequence:"
  (fact "AAV — right rotor (III) goes in V—notch position "
    (encode-string (enigma-machine {:reflector reflector-b
                        :left-rotor rotor1
                        :middle-rotor rotor2
                        :right-rotor rotor3
                        :settings "AAV"
                        }) "A") =>  "U")

  (fact "ADV — right rotor (III) goes in V—notch position"
    (encode-string (enigma-machine {:reflector reflector-b
                        :left-rotor rotor1
                        :middle-rotor rotor2
                        :right-rotor rotor3
                        :settings "ADV"
                        }) "A") =>  "Q")

  (fact "AEW — right rotor steps, takes middle rotor (II) one step further, which is now in its own E—notch position"
    (encode-string (enigma-machine {:reflector reflector-b
                 :left-rotor rotor1
                 :middle-rotor rotor2
                 :right-rotor rotor3
                 :settings "AEW"
                 }) "A") =>  "I")

  (fact "BFX — normal step of right rotor, double step of middle rotor, normal step of left rotor" 
    (encode-string  (enigma-machine {:reflector reflector-b
                         :left-rotor rotor1
                         :middle-rotor rotor2
                         :right-rotor rotor3                        
                         :settings "BFX"
                         }) "A") => "B")

  (fact "BFY — normal step of right rotor"
    (encode-string (enigma-machine {:reflector reflector-b
                                    :left-rotor rotor1
                                    :middle-rotor rotor2
                                    :right-rotor rotor3
                                    :settings "BFY"
                                    }) "A") =>  "M"))

(fact "Encode a String 'AAAAA' =>  BDZGO"
  (encode-string (enigma-machine {:reflector reflector-b
                                  :left-rotor rotor1
                                  :middle-rotor rotor2
                                  :right-rotor rotor3                 
                                  :settings "AAA"
                                  }) "AAAAA")  => "BDZGO")


(fact "Encode a String long string "
  (encode-string (enigma-machine {:reflector reflector-b
                                  :left-rotor rotor1
                                  :middle-rotor rotor2
                                  :right-rotor rotor3                 
                                  :settings "AAA"
                                  }) "AAAAAAAAAAAAAAAAAAAAAAAA")  => "BDZGOWCXLTKSBTMCDLPBMUQO")
(fact "Encode a String 'AAAAA' =>  FTZMG"
  (encode-string (enigma-machine {:reflector reflector-b
                                  :left-rotor rotor3
                                  :middle-rotor rotor2
                                  :right-rotor rotor1                 
                                  :settings "AAA"
                                  }) "AAAAA")  => "FTZMG")

 (fact "PlugBoard Encode a String 'AAAAA' =>  JXPNB"
  (encode-string (enigma-machine {:reflector reflector-b
                                  :left-rotor rotor1
                                  :middle-rotor rotor2
                                  :right-rotor rotor3                 
                                  :settings "AAA"
                                  :plugboard {"A" "S"}
                                  }) "AAAAA")  => "JXPNB")

(fact "Reveal encrypted text =>  THISISATEST"
  (encode-string (enigma-machine {:reflector reflector-b
                                  :left-rotor rotor1
                                  :middle-rotor rotor2
                                  :right-rotor rotor3                 
                                  :settings "AAA"
                                  :plugboard {}
                                  }) "OPGNDXCRWOM" )  => "THISISATEST")





